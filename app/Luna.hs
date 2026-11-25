{-# LANGUAGE OverloadedStrings #-}

-- | Backups widget: what Luna (https://github.com/prietus/luna), a client for
-- Borg repositories, has to say about them. Luna keeps that on disk, so the
-- card is right whether its window is open or not.
module Luna
    ( Backups
    , lunaLoop
    , lunaSize
    , renderLuna
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (forM_, forever)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GI.Cairo.Render
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

import Draw
import Theme

data Backups = Backups
    { bRepos :: Int
    , bArchives :: Int
    , bTotal :: Double
    -- ^ Bytes in every repository; zero until Luna has measured them.
    , bLargest :: [Repo]
    , bSchedules :: [Schedule]
    }

data Repo = Repo
    { rName :: Text
    , rProvider :: Text
    -- ^ Where it is kept: "borgBase", "borgBox" or "local".
    , rBytes :: Double
    }

data Schedule = Schedule
    { sLabel :: Text
    , sRunning :: Bool
    , sProgress :: Maybe Double
    -- ^ From 0 to 1, when the size of the previous run is known.
    , sBytes :: Maybe Double
    -- ^ Read so far in this run.
    , sError :: Maybe Text
    , sNext :: Maybe UTCTime
    }

-- Data ----------------------------------------------------------------------

{- | Asks Luna every half a minute, and every two seconds while a backup is
running. It hands @publish@ the overview or why there is none.
-}
lunaLoop :: (Either String Backups -> IO ()) -> IO ()
lunaLoop publish = forever $ do
    report <- overview
    publish report
    threadDelay . (* 1000000) $ case report of
        Right b | any sRunning (bSchedules b) -> 2
        Right _ -> 30
        Left _ -> 60

overview :: IO (Either String Backups)
overview = do
    -- An older Luna does not know the option and would open its window
    -- instead, so its version is asked first (every Luna answers that).
    installed <- run ["--version"]
    case installed of
        Left _ -> pure (Left "Luna is not installed")
        Right out
            | supported out -> do
                answer <- run ["--overview-json"]
                pure (answer >>= \json -> eitherDecodeStrict (TE.encodeUtf8 (T.pack json)) >>= parseEither backups)
            | otherwise -> pure (Left "Luna 0.1.3 or newer is needed")
  where
    run args = do
        result <- try (readProcessWithExitCode "luna" args "")
        pure $ case result of
            Left e -> Left (show (e :: IOException))
            Right (ExitSuccess, out, _) -> Right out
            Right (ExitFailure _, _, err) -> Left (takeWhile (/= '\n') err)
    -- "Luna 0.1.3"
    supported out = case words out of
        [_, number] | Just parts <- mapM readMaybe (splitOn '.' number) -> parts >= [0, 1, 3 :: Int]
        _ -> False
    splitOn c s = case break (== c) s of
        (a, _ : rest) -> a : splitOn c rest
        (a, []) -> [a]

backups :: Value -> Parser Backups
backups = withObject "overview" $ \o ->
    Backups
        <$> o .: "repoCount"
        <*> o .: "archiveCount"
        <*> o .: "totalUsageBytes"
        <*> (o .: "topRepos" >>= mapM repo)
        <*> (o .: "schedules" >>= mapM schedule)
  where
    repo = withObject "repository" $ \r -> Repo <$> r .: "name" <*> r .: "providerKey" <*> r .: "bytes"
    schedule = withObject "schedule" $ \s -> do
        repository <- s .: "repoName"
        name <- s .: "scheduleName"
        Schedule (repository <> " · " <> name)
            <$> s .: "running"
            <*> s .:? "progress"
            <*> s .:? "progressBytes"
            <*> s .:? "lastError"
            <*> s .:? "nextFire"

-- Drawing -------------------------------------------------------------------

-- | A square, whatever the space between cards.
lunaSize :: (Double, Double)
lunaSize = (cardWidth, cardWidth)

-- | Rows of repositories the card has room for.
maxRepos :: Int
maxRepos = 5

-- | @now@ is in seconds since the epoch. Nothing yet is 'Nothing'.
renderLuna :: Theme -> Double -> Maybe (Either String Backups) -> Render ()
renderLuna th now report = do
    let (w, h) = lunaSize
        pad = 18
        note s = do
            font th FontWeightNormal 12
            dim th
            centered (w / 2) (h / 2 + 10) s
    card th (Rect 0.5 0.5 (w - 1) (h - 1))
    font th FontWeightNormal 12
    dim th
    moveTo pad 30
    showText ("Backups" :: String)
    case report of
        Nothing -> note "Loading…"
        Just (Left problem) -> note problem
        Just (Right b) -> do
            let schedules = bSchedules b
                running = filter sRunning schedules
                failed = [s | s <- schedules, not (sRunning s), Just _ <- [sError s]]
                -- How things are, in a word.
                (color, word)
                    | not (null failed) = (critColor th, show (length failed) <> " failed")
                    | not (null running) = (accentColor th, "backing up")
                    | otherwise = (okColor th, "up to date")
            font th FontWeightNormal 11
            rgba color 1
            rightAligned (w - pad) 30 word
            ext <- textExtents word
            newPath
            arc (w - pad - textExtentsXadvance ext - 9) 26 3.2 0 (2 * pi)
            fill

            -- Everything that is kept, in all the repositories.
            if bTotal b > 0
                then do
                    font th FontWeightBold 26
                    ink th
                    moveTo pad 66
                    showText (humanBytes (bTotal b))
                else do
                    font th FontWeightNormal 12
                    dim th
                    moveTo pad 62
                    showText ("Open Luna to measure the repositories" :: String)
            font th FontWeightNormal 11
            dim th
            moveTo pad 84
            showText (count (bRepos b) "repository" "repositories" <> " · " <> count (bArchives b) "archive" "archives")

            -- The largest repositories, each as long as its share of the largest one.
            let repos = take maxRepos (bLargest b)
                largest = maximum (1 : map rBytes repos)
                barW = w - 2 * pad
            forM_ (zip [0 :: Int ..] repos) $ \(i, r) -> do
                let y = 96 + 29 * fromIntegral i
                    barY = y + 22
                ink th
                pangoLine th pad y (barW - 80) 12.5 False (rName r)
                font th FontWeightNormal 11
                dim th
                rightAligned (w - pad) (y + 12) (humanBytes (rBytes r))
                setLineWidth 4
                setLineCap LineCapRound
                track th
                newPath >> moveTo (pad + 2) barY >> lineTo (pad + barW - 2) barY >> stroke
                rgba (providerColor th (rProvider r)) 1
                newPath >> moveTo (pad + 2) barY >> lineTo (pad + 2 + max 1 ((barW - 4) * rBytes r / largest)) barY >> stroke

            -- At the bottom, the news: what is running, what failed, or what comes next.
            divider th (pad, h - 50) (w - pad, h - 50)
            case (running, failed) of
                (s : _, _) -> do
                    let amount = case (sProgress s, sBytes s) of
                            (Just p, _) -> show (round (p * 100) :: Int) <> " %"
                            (Nothing, Just bytes) | bytes > 0 -> humanBytes bytes
                            _ -> ""
                    font th FontWeightBold 11
                    rgba (accentColor th) 1
                    rightAligned (w - pad) (h - 30) amount
                    ink th
                    pangoLine th pad (h - 42) (barW - 70) 12 False (sLabel s)
                    setLineWidth 4
                    setLineCap LineCapRound
                    track th
                    newPath >> moveTo (pad + 2) (h - 16) >> lineTo (w - pad - 2) (h - 16) >> stroke
                    -- Without a size to compare with there is no bar to fill.
                    forM_ (sProgress s) $ \p -> do
                        rgba (accentColor th) 1
                        newPath >> moveTo (pad + 2) (h - 16) >> lineTo (pad + 2 + (barW - 4) * max 0.01 (min 1 p)) (h - 16) >> stroke
                ([], s : _) -> do
                    rgba (critColor th) 1
                    pangoLine th pad (h - 42) barW 12 True (sLabel s)
                    forM_ (sError s) $ \problem ->
                        pangoLine th pad (h - 25) barW 11 False (T.takeWhile (/= '\n') problem)
                ([], []) -> case listToMaybe (sortOn snd [(s, at) | s <- schedules, Just at <- [sNext s]]) of
                    Just (s, at) -> do
                        font th FontWeightNormal 11
                        dim th
                        moveTo pad (h - 24)
                        showText ("Next" :: String)
                        rightAligned (w - pad) (h - 24) (wait (realToFrac (utcTimeToPOSIXSeconds at) - now))
                        ink th
                        pangoLine th (pad + 34) (h - 36) (barW - 34 - 70) 12 False (sLabel s)
                    Nothing -> do
                        font th FontWeightNormal 11
                        dim th
                        moveTo pad (h - 24)
                        showText ("No scheduled backups" :: String)
  where
    count :: Int -> String -> String -> String
    count 1 one _ = "1 " <> one
    count n _ many = show n <> " " <> many

-- | Each place a repository is kept in has its color.
providerColor :: Theme -> Text -> RGB
providerColor th provider = case provider of
    "borgBase" -> accentColor th
    "borgBox" -> rainColor th
    "local" -> sunColor th
    _ -> okColor th

-- | Seconds until something as short text.
wait :: Double -> String
wait s
    | s < 60 = "now"
    | s < 3600 = "in " <> show (floor (s / 60) :: Int) <> " min"
    | s < 86400 = "in " <> show (floor (s / 3600) :: Int) <> " h"
    | otherwise = "in " <> show (floor (s / 86400) :: Int) <> " d"
