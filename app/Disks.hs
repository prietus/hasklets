{-# LANGUAGE OverloadedStrings #-}

{- | Disk usage widget: a ring for the disk that holds the home directory,
split into the things that take the most space in it, and a bar for every
other mount point.
-}
module Disks
    ( Disk
    , disksLoop
    , disksSize
    , renderDisks
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch)
import Control.Monad (forM_, unless, when)
import Data.GI.Base.GError (GError)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GI.Cairo.Render
import GI.Gio qualified as Gio
import System.Directory (getHomeDirectory)
import System.FilePath (takeFileName)
import System.Process (proc, readCreateProcessWithExitCode)
import Text.Read (readMaybe)

import Draw
import Theme

data Disk = Disk
    { dMount :: Text
    , dSize :: Double
    -- ^ Bytes.
    , dUsed :: Double
    , dHome :: Bool
    -- ^ The disk the home directory is on: drawn as the ring.
    , dLargest :: [(Text, Double)]
    -- ^ On the home disk: what takes the most space in the home directory
    -- (name and bytes), biggest first.
    }

-- Data ----------------------------------------------------------------------

-- | At most this many are shown.
maxDisks :: Int
maxDisks = 4

-- | Filesystems that count as a disk (not tmpfs, proc, fuse...).
realFilesystems :: [Text]
realFilesystems =
    ["ext2", "ext3", "ext4", "btrfs", "xfs", "f2fs", "bcachefs", "zfs", "vfat", "exfat", "ntfs", "ntfs3", "fuseblk"]

-- | How many of the largest folders and files of the home directory are shown.
maxLargest :: Int
maxLargest = 4

-- | The home directory is measured again every this many usage checks
-- (every ten minutes): it takes a few seconds of disk reads.
scanEvery :: Int
scanEvery = 20

{- | Checks usage every 30 seconds and hands it to @publish@. The first
result comes at once, without the largest folders; they follow when the
home directory has been measured.
-}
disksLoop :: ([Disk] -> IO ()) -> IO ()
disksLoop publish = do
    home <- getHomeDirectory
    largestRef <- newIORef []
    let sample = do
            disks <- catMaybes <$> (mountPoints >>= mapM usage)
            largest <- readIORef largestRef
            publish (withHome (T.pack home) largest disks)
    forM_ [0 :: Int ..] $ \n -> do
        sample
        when (n `mod` scanEvery == 0) $ do
            largestIn home >>= writeIORef largestRef
            sample
        threadDelay (30 * 1000000)

-- Marks the disk the home directory is on (the longest mount point that
-- contains it) and gives it the largest folders.
withHome :: Text -> [(Text, Double)] -> [Disk] -> [Disk]
withHome home largest disks = map mark disks
  where
    holders = sortOn (Down . T.length) [dMount d | d <- disks, dMount d `holds` home]
    mark d = case holders of
        (m : _) | m == dMount d -> d{dHome = True, dLargest = largest}
        _ -> d
    mount `holds` path = mount == "/" || mount == path || (mount <> "/") `T.isPrefixOf` path

{- | The folders and files directly in a directory (hidden ones too) that
take the most space, measured by @du@ at the lowest priority and without
leaving the filesystem. Empty if @du@ cannot be run.
-}
largestIn :: FilePath -> IO [(Text, Double)]
largestIn dir =
    ( do
        -- du exits with an error when it cannot read some folder: what it
        -- did measure is still good.
        (_, out, _) <- readCreateProcessWithExitCode (proc "nice" ["-n", "19", "du", "-x", "-a", "-d1", "-B1", "--", dir]) ""
        let entries =
                [ (T.pack (takeFileName path), bytes)
                | l <- lines out
                , (size, '\t' : path) <- [break (== '\t') l]
                , path /= dir
                , Just bytes <- [readMaybe size]
                ]
        pure (take maxLargest (sortOn (Down . snd) entries))
    )
        `catch` \(_ :: IOException) -> pure []

-- One mount point per device (the shortest: with btrfs subvolumes the same
-- device shows up several times).
mountPoints :: IO [Text]
mountPoints = do
    ls <- T.lines <$> TIO.readFile "/proc/mounts"
    let byDevice =
            M.fromListWith
                shorter
                [(dev, mnt) | l <- ls, (dev : mnt : fs : _) <- [T.words l], fs `elem` realFilesystems]
    pure (take maxDisks (sortOn (\m -> (T.length m, m)) (M.elems byDevice)))
  where
    shorter a b = if T.length a <= T.length b then a else b

usage :: Text -> IO (Maybe Disk)
usage mount =
    ( do
        file <- Gio.fileNewForPath (T.unpack mount)
        info <- Gio.fileQueryFilesystemInfo file "filesystem::size,filesystem::used" (Nothing :: Maybe Gio.Cancellable)
        size <- fromIntegral <$> Gio.fileInfoGetAttributeUint64 info "filesystem::size"
        used <- fromIntegral <$> Gio.fileInfoGetAttributeUint64 info "filesystem::used"
        pure (if size > 0 then Just (Disk mount size used False []) else Nothing)
    )
        `catch` \(_ :: GError) -> pure Nothing

-- Drawing -------------------------------------------------------------------

rowH :: Double
rowH = 38

-- Height of the block with the ring and of a line of its legend.
ringH, legendH :: Double
ringH = 112
legendH = 18

-- | The height depends on which disks there are.
disksSize :: Double -> [Disk] -> (Double, Double)
disksSize gap disks = (cardWidth, fitHeight gap (disksNatural disks))

-- | What the disks take, before the height is rounded up to the grid.
disksNatural :: [Disk] -> Double
disksNatural disks = 46 + (if null homes then 0 else ringH) + rowH * fromIntegral (length others) + padding
  where
    (homes, others) = (filter dHome disks, filter (not . dHome) disks)
    padding
        | null disks = rowH
        | null others = 6
        | otherwise = 0

renderDisks :: Theme -> Double -> [Disk] -> Render ()
renderDisks th gap disks = do
    let (w, h) = disksSize gap disks
        pad = 18
        (homes, others) = (filter dHome disks, filter (not . dHome) disks)
        -- The height left over goes between the blocks, in equal parts.
        blocks = length (take 1 homes) + length others
        spare = (h - disksNatural disks) / fromIntegral (blocks + 1)
    card th (Rect 0.5 0.5 (w - 1) (h - 1))
    font th FontWeightNormal 12
    dim th
    moveTo pad 30
    showText ("Disks" :: String)
    forM_ (take 1 homes) (renderHome th w pad (42 + spare))
    let top = 42 + (if null homes then 0 else ringH + spare)
    forM_ (zip [0 :: Int ..] others) $ \(i, d) -> do
        let y = top + spare + (rowH + spare) * fromIntegral i
            frac = dUsed d / dSize d
            barY = y + 22
            barW = w - 2 * pad
        ink th
        pangoLine th pad y (barW * 0.45) 13 False (dMount d)
        font th FontWeightNormal 11
        dim th
        rightAligned (w - pad) (y + 12) (humanBytes (dUsed d) <> " of " <> humanBytes (dSize d))

        setLineWidth 6
        setLineCap LineCapRound
        track th
        newPath >> moveTo (pad + 3) barY >> lineTo (pad + barW - 3) barY >> stroke
        rgba (levelColor th frac) 1
        newPath >> moveTo (pad + 3) barY >> lineTo (pad + 3 + (barW - 6) * frac) barY >> stroke

{- The home disk: a ring where every one of the largest folders has its own
colour, the rest of what is used is grey and the free space is the track;
next to it, the legend.
-}
renderHome :: Theme -> Double -> Double -> Double -> Disk -> Render ()
renderHome th w pad y d = do
    let radius = 40
        cx = pad + radius + 5
        cy = y + ringH / 2 - 4
        frac = dUsed d / dSize d
        largest = [(name, min bytes (dUsed d)) | (name, bytes) <- dLargest d]
        rest = max 0 (dUsed d - sum (map snd largest))
        colors = [accentColor th, rainColor th, sunColor th, okColor th]
        restColor = fst (dimColor th)
        -- Before the home directory has been measured the ring is a plain gauge.
        segments
            | null largest = [(levelColor th frac, 1, dUsed d)]
            | otherwise = zip3 colors (repeat 1) (map snd largest) <> [(restColor, 0.5, rest)]
        (a0, sweep, cap) = case gaugeStyle th of
            Arc -> (0.75 * pi, 1.5 * pi, LineCapRound)
            Ring -> (-0.5 * pi, 2 * pi, LineCapButt)
        gap = 0.045

    -- The ring.
    setLineWidth 10
    setLineCap cap
    track th
    newPath >> arc cx cy radius a0 (a0 + sweep) >> stroke
    setLineCap LineCapButt
    let draw _ [] = pure ()
        draw from ((color, alpha, bytes) : more) = do
            let to = from + sweep * bytes / dSize d
            -- A segment thinner than the gap between two still shows as a sliver.
            unless (bytes <= 0) $ do
                rgba color alpha
                newPath >> arc cx cy radius from (max (from + 0.012) (to - gap)) >> stroke
            draw to more
    draw a0 segments

    -- In the middle: how full it is.
    font th FontWeightBold 22
    if frac < 0.85 then ink th else rgba (critColor th) 1
    centered cx (cy + 5) (show (round (frac * 100) :: Int))
    font th FontWeightNormal 10
    dim th
    centered cx (cy + 19) "% used"

    -- Next to it: the mount point, the totals and the legend.
    let lx = cx + radius + 22
        lw = w - pad - lx
    ink th
    pangoLine th lx y (lw * 0.4) 13 True (dMount d)
    font th FontWeightNormal 11
    dim th
    rightAligned (w - pad) (y + 12) (humanBytes (dUsed d) <> " of " <> humanBytes (dSize d))
    forM_ (zip3 [0 :: Int ..] colors largest) $ \(i, color, (name, bytes)) -> do
        let ly = y + 26 + legendH * fromIntegral i
        rgba color 1
        newPath >> arc (lx + 4) (ly + 8) 4 0 (2 * pi) >> fill
        ink th
        pangoLine th (lx + 15) ly (lw - 15 - 62) 12 False ("~/" <> name)
        font th FontWeightNormal 11
        dim th
        rightAligned (w - pad) (ly + 12) (humanBytes bytes)
    when (null largest) $ do
        font th FontWeightNormal 11
        dim th
        moveTo lx (y + 44)
        showText (humanBytes (dSize d - dUsed d) <> " free")
