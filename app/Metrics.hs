-- | System metrics read from /proc.
module Metrics
    ( Raw
    , Rates (..)
    , readRaw
    , rates
    , readMemFraction
    , readCpuTicks
    ) where

import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Clock (getMonotonicTime)
import Text.Read (readMaybe)

-- | Cumulative counters at one instant; rates come from subtracting two.
data Raw = Raw
    { rawTime :: !Double
    , rawCpuBusy :: !Double
    , rawCpuTotal :: !Double
    , rawRx :: !Double
    , rawTx :: !Double
    }

data Rates = Rates
    { cpuFraction :: !Double
    -- ^ 0..1
    , upBytesPerSec :: !Double
    , downBytesPerSec :: !Double
    }

readRaw :: IO Raw
readRaw = do
    t <- getMonotonicTime
    (busy, total) <- parseCpu <$> TIO.readFile "/proc/stat"
    (rx, tx) <- parseNet <$> TIO.readFile "/proc/net/dev"
    pure (Raw t busy total rx tx)

rates :: Raw -> Raw -> Rates
rates old new =
    Rates
        { cpuFraction = clamp01 (safeDiv (rawCpuBusy new - rawCpuBusy old) dTotal)
        , upBytesPerSec = max 0 (safeDiv (rawTx new - rawTx old) dt)
        , downBytesPerSec = max 0 (safeDiv (rawRx new - rawRx old) dt)
        }
  where
    dt = rawTime new - rawTime old
    dTotal = rawCpuTotal new - rawCpuTotal old

-- | Fraction of memory in use (0..1), based on MemAvailable.
readMemFraction :: IO Double
readMemFraction = do
    ls <- T.lines <$> TIO.readFile "/proc/meminfo"
    let field name = case [v | l <- ls, (k : v : _) <- [T.words l], k == name] of
            (v : _) -> number v
            [] -> 0
        total = field (T.pack "MemTotal:")
        available = field (T.pack "MemAvailable:")
    pure (clamp01 (safeDiv (total - available) total))

-- | Cumulative CPU ticks across all cores, and how many cores there are.
readCpuTicks :: IO (Double, Int)
readCpuTicks = do
    txt <- TIO.readFile "/proc/stat"
    let cores = length [() | l <- T.lines txt, Just r <- [T.stripPrefix (T.pack "cpu") l], Just (c, _) <- [T.uncons r], isDigit c]
    pure (snd (parseCpu txt), max 1 cores)

-- First line of /proc/stat: cpu user nice system idle iowait irq softirq steal ...
parseCpu :: Text -> (Double, Double)
parseCpu txt = case T.lines txt of
    (l : _) | (_ : fields) <- T.words l ->
        let vs = map number (take 8 fields)
            idle = sum (take 2 (drop 3 vs))
            total = sum vs
         in (total - idle, total)
    _ -> (0, 0)

-- Sums rx/tx over every interface except loopback.
parseNet :: Text -> (Double, Double)
parseNet txt = (sum (map fst rows), sum (map snd rows))
  where
    rows = mapMaybe row (drop 2 (T.lines txt))
    row l = case T.breakOn (T.pack ":") l of
        (name, rest)
            | not (T.null rest)
            , T.strip name /= T.pack "lo"
            , fields <- map number (T.words (T.drop 1 rest))
            , length fields >= 9 ->
                Just (fields !! 0, fields !! 8)
        _ -> Nothing

number :: Text -> Double
number = maybe 0 fromInteger . readMaybe . T.unpack

safeDiv :: Double -> Double -> Double
safeDiv _ 0 = 0
safeDiv a b = a / b

clamp01 :: Double -> Double
clamp01 = max 0 . min 1
