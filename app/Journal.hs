{-# LANGUAGE OverloadedStrings #-}

{- | Journal widget: what is new in the systemd journal.

Lines are grouped by where they come from and by the shape of their message
(the text without its numbers). A shape that has been showing up for days is
known noise and only counts in the total; one seen for the first time in the
last day gets a row, and so does anything logged as an error.
-}
module Journal
    ( Journal
    , emptyJournal
    , journalLoop
    , journalLines
    , journalSize
    , renderJournal
    ) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (IOException, catch, finally)
import Control.Monad (forM_, forever, unless, when)
import Data.Aeson (Value (..), decodeStrict)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit)
import Data.Foldable (toList)
import Data.IORef
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Sequence (Seq, ViewL (..), (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GI.Cairo.Render
import System.IO (hIsEOF)
import System.Process
import Text.Printf (printf)
import Text.Read (readMaybe)

import Draw
import Theme

-- | What the card shows.
data Journal = Journal
    { jMinutes :: [(Int, Int)]
    -- ^ The last hour, oldest minute first: lines logged, and the priority
    -- of the worst one that deserves a look ('quiet' if none does).
    , jRows :: [Row]
    -- ^ The messages worth a row, most interesting first.
    , jTotal :: Int
    -- ^ Warnings and errors of the last day, rows included.
    , jFailed :: [Text]
    -- ^ Units systemd has as failed.
    }

data Row = Row
    { rSource :: Text
    , rText :: Text
    -- ^ The message as it was last logged.
    , rPrio :: Int
    , rNew :: Bool
    -- ^ First seen within the last day.
    , rCount :: Int
    -- ^ Times in the last day.
    , rLast :: Double
    }

emptyJournal :: Journal
emptyJournal = Journal (replicate 60 (0, quiet)) [] 0 []

-- Data ----------------------------------------------------------------------

-- | A line of the journal: seconds since the epoch, syslog priority (0 is
-- the worst, 3 an error, 4 a warning), who logged it and what.
data Entry = Entry Double Int Text Text

-- | A message without what changes from one time to the next.
type Key = (Text, Text)

data Group = Group
    { gPrio :: !Int
    , gFirst :: !Double
    , gLast :: !Double
    , gText :: !Text
    }

data St = St
    { stGroups :: !(M.Map Key Group)
    -- ^ Every shape of warning or error of the last week.
    , stCounts :: !(M.Map Key Int)
    -- ^ How many times each one was logged in the last day.
    , stRecent :: !(Seq (Double, Key))
    -- ^ The warnings and errors of the last day, oldest first.
    , stMinutes :: !(M.Map Int (Int, Int))
    -- ^ Per minute since the epoch, as in 'jMinutes'.
    }

hour, day, week :: Double
hour = 3600
day = 24 * hour
week = 7 * day

-- | Priority of a minute in which nothing deserves a look.
quiet :: Int
quiet = 8

-- | At most this many rows are kept for the card.
maxRows :: Int
maxRows = 8

{- | Follows the journal and hands @publish@ what there is to show: two
seconds after something is logged at the latest, and every half a minute
anyway, because ages and the chart move with the clock.

The last week of warnings is read first (a quarter of a second), which is
what tells the known messages from the new ones; nothing is kept on disk.
-}
journalLoop :: (Journal -> IO ()) -> IO ()
journalLoop publish = do
    state <- newIORef emptySt
    dirty <- newIORef False
    ready <- newIORef False
    failed <- newIORef []
    let clock = realToFrac <$> getPOSIXTime :: IO Double
        feed line = forM_ (parseEntry line) $ \e -> do
            atomicModifyIORef' state (\st -> (ingest e st, ()))
            writeIORef dirty True
        tick n = do
            when (n `mod` 30 == 0) (failedUnits >>= writeIORef failed)
            loaded <- readIORef ready
            changed <- atomicModifyIORef' dirty (False,)
            when (loaded && (changed || n `mod` 15 == 0)) $ do
                now <- clock
                st <- atomicModifyIORef' state (\st -> let st' = expire now st in (st', st'))
                publish . view now st =<< readIORef failed
            threadDelay 2000000
            tick (n + 1 :: Int)
        follow = do
            start <- clock
            let at t = '@' : show (floor t :: Integer)
            writeIORef state emptySt
            stream ["-p", "0..4", "--since", at (start - week), "--until", at (start - hour)] feed
            writeIORef ready True
            -- Every priority from here on: the chart counts all the lines.
            stream ["-f", "--since", at (start - hour)] feed
    ticker <- forkIO (tick 0)
    -- journalctl only ends if it is missing or was killed: try again later.
    forever ((follow `catch` \(_ :: IOException) -> pure ()) >> threadDelay 30000000)
        `finally` killThread ticker

-- | Runs journalctl and hands over its output line by line, as JSON. The
-- process is stopped when the thread reading it is.
stream :: [String] -> (BS.ByteString -> IO ()) -> IO ()
stream args onLine =
    withCreateProcess journalctl{std_in = NoStream, std_out = CreatePipe, std_err = NoStream} $ \_ out _ _ ->
        forM_ out $ \h ->
            let go = do
                    eof <- hIsEOF h
                    unless eof (BC.hGetLine h >>= onLine >> go)
             in go
  where
    journalctl = proc "journalctl" (["-q", "--no-pager", "-o", "json", "--output-fields=" <> fields] <> args)
    fields = "MESSAGE,PRIORITY,SYSLOG_IDENTIFIER,_COMM,COREDUMP_COMM,COREDUMP_EXE"

parseEntry :: BS.ByteString -> Maybe Entry
parseEntry line = do
    Object o <- decodeStrict line
    let str k = case KM.lookup k o of
            Just (String t) -> Just t
            _ -> Nothing
        -- Text that is not UTF-8 comes as a list of bytes, and a message
        -- too long to print as null.
        message = case KM.lookup "MESSAGE" o of
            Just (String t) -> t
            Just (Array bytes) -> decodeUtf8Lenient (BS.pack [truncate n | Number n <- toList bytes])
            _ -> ""
        name = T.dropWhileEnd (not . isAlphaNum) . T.takeWhileEnd (/= '/')
        source = fromMaybe "?" (str "SYSLOG_IDENTIFIER" <|> str "_COMM")
        prio = fromMaybe 6 (str "PRIORITY" >>= readMaybe . T.unpack)
    micros <- str "__REALTIME_TIMESTAMP" >>= readMaybe . T.unpack
    pure $ case str "COREDUMP_EXE" <|> str "COREDUMP_COMM" of
        -- A crash: its message is a whole stack trace.
        Just exe -> Entry (micros / 1e6) prio "coredump" (name exe <> " dumped core")
        Nothing -> Entry (micros / 1e6) prio `uncurry` unwrap (name source) (T.strip (T.takeWhile (/= '\n') message))

{- | A display manager logs under its own name whatever the session writes
to its standard output: the compositor and everything it starts. Who really
wrote a line is only known when the message begins with it, as in
@waymobar: compositor closed the connection@; a level (@Warning:@) is not a
name.
-}
unwrap :: Text -> Text -> (Text, Text)
unwrap source text
    | source `elem` ["gdm-wayland-session", "gdm-x-session"]
    , (prefix, rest) <- T.breakOn ": " text
    , not (T.null rest)
    , T.any isAlpha prefix
    , T.all (\c -> isAlphaNum c || c `elem` ("-_./" :: String)) prefix
    , T.toLower prefix `notElem` levels
    , who <- T.takeWhileEnd (/= '/') prefix
    , not (T.null who) =
        -- Some say their name twice.
        let said = T.drop 2 rest
         in (who, T.strip (fromMaybe said (T.stripPrefix (who <> ": ") said)))
    | otherwise = (source, text)
  where
    levels = ["trace", "debug", "info", "note", "notice", "warn", "warning", "err", "error", "critical", "fatal"]

-- | Numbers, addresses and identifiers change from one time to the next:
-- every run of digits (hexadecimal too) becomes a @#@.
shape :: Text -> Text
shape = T.take 120 . T.concat . map squash . T.groupBy (\a b -> isHexDigit a == isHexDigit b)
  where
    squash run
        | T.any isDigit run = "#"
        | otherwise = run

emptySt :: St
emptySt = St M.empty M.empty Seq.empty M.empty

isNew :: Double -> Group -> Bool
isNew now g = gFirst g > now - day

ingest :: Entry -> St -> St
ingest (Entry at prio source text) st
    | prio > 4 = st{stMinutes = minute quiet}
    | otherwise =
        st
            { stGroups = M.insert key group (stGroups st)
            , stCounts = M.insertWith (+) key 1 (stCounts st)
            , stRecent = stRecent st |> (at, key)
            , stMinutes = minute (if prio <= 3 || isNew at group then prio else quiet)
            }
  where
    key = (source, shape text)
    group = case M.lookup key (stGroups st) of
        Nothing -> Group prio at at text
        Just g -> g{gPrio = min prio (gPrio g), gLast = at, gText = text}
    minute worst = M.insertWith (\_ (n, p) -> (n + 1, min p worst)) (floor (at / 60)) (1, worst) (stMinutes st)

-- | Forgets what has become too old.
expire :: Double -> St -> St
expire now st =
    st
        { stGroups = M.filter ((> now - week) . gLast) (stGroups st)
        , stCounts = counts
        , stRecent = recent
        , stMinutes = snd (M.split (firstMinute now - 1) (stMinutes st))
        }
  where
    (counts, recent) = go (stCounts st) (stRecent st)
    go cs s = case Seq.viewl s of
        (at, key) :< rest | at < now - day -> go (M.update (\n -> if n > 1 then Just (n - 1) else Nothing) key cs) rest
        _ -> (cs, s)

-- | The oldest minute of the chart.
firstMinute :: Double -> Int
firstMinute now = floor (now / 60) - 59

view :: Double -> St -> [Text] -> Journal
view now st failed =
    Journal
        { jMinutes = [M.findWithDefault (0, quiet) m (stMinutes st) | m <- take 60 [firstMinute now ..]]
        , jRows = take maxRows (sortOn rank rows)
        , jTotal = Seq.length (stRecent st)
        , jFailed = failed
        }
  where
    rows =
        [ Row source (gText g) (gPrio g) new n (gLast g)
        | (key@(source, _), n) <- M.toList (stCounts st)
        , Just g <- [M.lookup key (stGroups st)]
        , let new = isNew now g
        , new || gPrio g <= 3
        ]
    -- What is new first; then the worst, then the latest.
    rank r = (not (rNew r), rPrio r, Down (rLast r))

-- | The units that failed, of the system and of the user.
failedUnits :: IO [Text]
failedUnits = (<>) <$> list [] <*> list ["--user"]
  where
    list scope =
        ( do
            (_, out, _) <- readCreateProcessWithExitCode (proc "systemctl" (scope <> ["--failed", "--no-legend", "--plain"])) ""
            pure [T.pack unit | (unit : _) <- map words (lines out)]
        )
            `catch` \(_ :: IOException) -> pure []

-- Drawing -------------------------------------------------------------------

rowH :: Double
rowH = 38

-- | How many rows the card has when up to @rows@ are wanted.
journalLines :: Int -> Journal -> Int
journalLines rows j = min rows (length (jRows j))

-- | The height depends on how many rows there are.
journalSize :: Int -> (Double, Double)
journalSize n = (300, 88 + rowH * fromIntegral n + 34)

-- | @now@ is in seconds since the epoch.
renderJournal :: Theme -> Int -> Double -> Journal -> Render ()
renderJournal th rows now j = do
    let shown = take rows (jRows j)
        (w, h) = journalSize (length shown)
        pad = 18
        level p = if p <= 3 then critColor th else warnColor th
    card th (Rect 0.5 0.5 (w - 1) (h - 1))
    font th FontWeightNormal 12
    dim th
    moveTo pad 30
    showText ("Journal" :: String)
    font th FontWeightNormal 11
    rightAligned (w - pad) 30 "last hour"

    -- A bar per minute: as tall as the lines logged (on a logarithmic
    -- scale, a burst is hundreds of times a quiet minute), and coloured
    -- when one of them is in the rows below.
    let top = 42
        bottom = 76
        slot = (w - 2 * pad) / 60
        peak = maximum (30 : map fst (jMinutes j))
        scaled n = log (1 + fromIntegral n) / log (1 + fromIntegral peak) :: Double
    forM_ (zip [0 :: Int ..] (jMinutes j)) $ \(i, (n, worst)) -> do
        let barH = if n == 0 then 2 else max 3 ((bottom - top) * scaled n)
        if n == 0
            then track th
            else if worst < quiet then rgba (level worst) 1 else rgba (accentColor th) 0.5
        rectangle (pad + slot * fromIntegral i + 0.7) (bottom - barH) (slot - 1.4) barH
        fill

    forM_ (zip [0 :: Int ..] shown) $ \(i, r) -> do
        let y = 88 + rowH * fromIntegral i
            times = if rCount r > 1 then "×" <> compact (rCount r) <> " · " else ""
        -- A full dot for what is new, a hollow one for what keeps coming back.
        rgba (level (rPrio r)) 1
        newPath
        arc (pad + 4) (y + 8) 3.2 0 (2 * pi)
        if rNew r then fill else setLineWidth 1.4 >> stroke
        ink th
        pangoLine th (pad + 15) y (w - 2 * pad - 15 - 84) 13 (rNew r) (rSource r)
        font th FontWeightNormal 11
        dim th
        rightAligned (w - pad) (y + 12) (times <> age (now - rLast r))
        pangoLine th (pad + 15) (y + 17) (w - 2 * pad - 15) 11.5 False (rText r)

    let y = 88 + rowH * fromIntegral (length shown)
        rest = jTotal j - sum (map rCount shown)
        failed = length (jFailed j)
    divider th (pad, y + 2) (w - pad, y + 2)
    font th FontWeightNormal 11
    dim th
    moveTo pad (y + 21)
    showText $
        if null shown
            then "Nothing new · " <> compact rest <> " known in 24 h"
            else compact rest <> " more in 24 h"
    when (failed > 0) $ rgba (critColor th) 1
    rightAligned (w - pad) (y + 21) $ case failed of
        0 -> "no failed units"
        1 -> "1 failed unit"
        n -> show n <> " failed units"

-- | "874", "4.3k".
compact :: Int -> String
compact n
    | n < 1000 = show n
    | otherwise = printf "%.1fk" (fromIntegral n / 1000 :: Double)

-- | Seconds ago as short text.
age :: Double -> String
age s
    | s < 60 = "now"
    | s < hour = show (floor (s / 60) :: Int) <> " m"
    | otherwise = show (floor (s / hour) :: Int) <> " h"
