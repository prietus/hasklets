{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Widget with the processes using the most CPU.
module Processes
    ( Proc
    , processLoop
    , processesSize
    , processesRows
    , renderProcesses
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, evaluate, try)
import Control.Monad (forM_)
import Data.Char (isDigit)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Foreign.C.Types (CInt (..))
import GI.Cairo.Render
import System.Directory (listDirectory)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Draw
import Theme
import Metrics (readCpuTicks)

data Proc = Proc
    { pName :: Text
    , pCpu :: Double
    -- ^ Percentage of one core, as in top: it can exceed 100.
    , pMem :: Double
    -- ^ Resident memory in bytes.
    }

-- Data ----------------------------------------------------------------------

-- | A snapshot of /proc: total CPU ticks, cores and, per pid, name, ticks
-- used and resident pages.
data Snapshot = Snapshot Double Int (M.Map Int (Text, Double, Double))

-- | Every 2 seconds hands @publish@ the @rows@ processes using the most CPU.
processLoop :: Int -> ([Proc] -> IO ()) -> IO ()
processLoop rows publish = do
    pageSize <- fromIntegral <$> c_getpagesize
    let go old = do
            threadDelay 2000000
            new <- snapshot
            publish (top rows pageSize old new)
            go new
    snapshot >>= go

foreign import ccall unsafe "getpagesize" c_getpagesize :: IO CInt

snapshot :: IO Snapshot
snapshot = do
    (total, cores) <- readCpuTicks
    pids <- filter (all isDigit) <$> listDirectory "/proc"
    procs <- catMaybes <$> mapM readProc pids
    pure (Snapshot total cores (M.fromList procs))

-- Processes can vanish while being read: read failures are ignored.
readProc :: String -> IO (Maybe (Int, (Text, Double, Double)))
readProc pid = do
    r <- try (TIO.readFile ("/proc/" <> pid <> "/stat") >>= evaluate . parseStat)
    pure $ case r of
        Right (Just p) -> (,p) <$> readMaybe pid
        Right Nothing -> Nothing
        Left (_ :: IOException) -> Nothing

-- "pid (name) state ...": the name may contain spaces and parentheses, so
-- split at the last ')'. utime, stime and rss are fields 14, 15 and
-- 24.
parseStat :: Text -> Maybe (Text, Double, Double)
parseStat txt = case T.breakOnEnd ")" txt of
    (before, after)
        | not (T.null before)
        , fields <- T.words after
        , length fields > 21
        , Just utime <- num (fields !! 11)
        , Just stime <- num (fields !! 12)
        , Just rss <- num (fields !! 21) ->
            Just (T.drop 1 (T.dropWhile (/= '(') (T.dropEnd 1 before)), utime + stime, rss)
    _ -> Nothing
  where
    num = fmap fromInteger . readMaybe . T.unpack

top :: Int -> Double -> Snapshot -> Snapshot -> [Proc]
top rows pageSize (Snapshot total0 _ old) (Snapshot total1 cores new) =
    take rows . sortOn (\p -> (Down (pCpu p), Down (pMem p))) $
        [ Proc name (cpu ticks (M.lookup pid old)) (pages * pageSize)
        | (pid, (name, ticks, pages)) <- M.toList new
        ]
  where
    dTotal = total1 - total0
    -- A new process has no previous sample: it counts from zero.
    cpu ticks before
        | dTotal <= 0 = 0
        | otherwise = max 0 (ticks - maybe ticks (\(_, t, _) -> t) before) / dTotal * fromIntegral cores * 100

-- Drawing -------------------------------------------------------------------

rowH :: Double
rowH = 24

-- | The height is the one on the grid that holds the rows requested.
processesSize :: Double -> Int -> (Double, Double)
processesSize gap rows = (cardWidth, fitHeight gap (56 + rowH * fromIntegral rows))

-- | The rows that fit in that height: the ones requested, or a few more.
processesRows :: Double -> Int -> Int
processesRows gap rows = max rows (floor ((snd (processesSize gap rows) - 56) / rowH + 0.001))

-- | @rows@ is how many were requested; as many as fit are drawn.
renderProcesses :: Theme -> Double -> Int -> [Proc] -> Render ()
renderProcesses th gap rows procs = do
    let (w, h) = processesSize gap rows
        -- The rows share out the height there is.
        pitch = (h - 56) / fromIntegral (processesRows gap rows)
        pad = 18
        memX = w - pad
        cpuX = w - pad - 74
    card th (Rect 0.5 0.5 (w - 1) (h - 1))
    font th FontWeightNormal 12
    dim th
    moveTo pad 30
    showText ("Processes" :: String)
    font th FontWeightNormal 11
    rightAligned cpuX 30 "CPU"
    rightAligned memX 30 "Memory"
    forM_ (zip [0 :: Int ..] procs) $ \(i, p) -> do
        let y = 44 + (pitch - rowH) / 2 + pitch * fromIntegral i
        ink th
        pangoLine th pad y (cpuX - 52 - pad) 13 False (pName p)
        font th FontWeightBold 12
        -- The color follows the load on one core.
        rgba (levelColor th (pCpu p / 100)) 1
        rightAligned cpuX (y + 13) (printf "%.1f %%" (pCpu p))
        font th FontWeightNormal 12
        dim th
        rightAligned memX (y + 13) (humanBytes (pMem p))
