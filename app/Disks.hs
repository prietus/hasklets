{-# LANGUAGE OverloadedStrings #-}

-- | Disk usage widget, one bar per mount point.
module Disks
    ( Disk
    , disksLoop
    , disksSize
    , renderDisks
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Monad (forM_, forever)
import Data.GI.Base.GError (GError)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GI.Cairo.Render
import GI.Gio qualified as Gio

import Draw

data Disk = Disk
    { dMount :: Text
    , dSize :: Double
    -- ^ Bytes.
    , dUsed :: Double
    }

-- Data ----------------------------------------------------------------------

-- | At most this many are shown.
maxDisks :: Int
maxDisks = 4

-- | Filesystems that count as a disk (not tmpfs, proc, fuse...).
realFilesystems :: [Text]
realFilesystems =
    ["ext2", "ext3", "ext4", "btrfs", "xfs", "f2fs", "bcachefs", "zfs", "vfat", "exfat", "ntfs", "ntfs3", "fuseblk"]

-- | Checks usage every 30 seconds and hands it to @publish@.
disksLoop :: ([Disk] -> IO ()) -> IO ()
disksLoop publish = forever $ do
    mounts <- mountPoints
    catMaybes <$> mapM usage mounts >>= publish
    threadDelay (30 * 1000000)

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
        pure (if size > 0 then Just (Disk mount size used) else Nothing)
    )
        `catch` \(_ :: GError) -> pure Nothing

-- Drawing -------------------------------------------------------------------

rowH :: Double
rowH = 38

-- | The height depends on how many disks there are.
disksSize :: Int -> (Double, Double)
disksSize n = (300, 46 + rowH * fromIntegral (max 1 n))

renderDisks :: [Disk] -> Render ()
renderDisks disks = do
    let (w, h) = disksSize (length disks)
        pad = 18
    card (Rect 0.5 0.5 (w - 1) (h - 1))
    font FontWeightNormal 12
    setSourceRGBA 1 1 1 0.65
    moveTo pad 30
    showText ("Disks" :: String)
    forM_ (zip [0 :: Int ..] disks) $ \(i, d) -> do
        let y = 42 + rowH * fromIntegral i
            frac = dUsed d / dSize d
            barY = y + 22
            barW = w - 2 * pad
        setSourceRGBA 1 1 1 0.95
        pangoLine pad y (barW * 0.45) 13 False (dMount d)
        font FontWeightNormal 11
        setSourceRGBA 1 1 1 0.65
        rightAligned (w - pad) (y + 12) (humanBytes (dUsed d) <> " of " <> humanBytes (dSize d))

        setLineWidth 6
        setLineCap LineCapRound
        setSourceRGBA 1 1 1 0.13
        newPath >> moveTo (pad + 3) barY >> lineTo (pad + barW - 3) barY >> stroke
        rgba (levelColor frac) 1
        newPath >> moveTo (pad + 3) barY >> lineTo (pad + 3 + (barW - 6) * frac) barY >> stroke
