-- | Cairo drawing primitives: card, ring gauge and sparkline.
module Draw
    ( Rect (..)
    , card
    , divider
    , gauge
    , sparkTile
    , roundRect
    , font
    , centered
    , rightAligned
    , pangoLine
    , levelColor
    , rgba
    , humanBytes
    ) where

import Control.Monad (forM_, unless, when)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import GI.Cairo.Render
import GI.Cairo.Render.Connector (toRender)
import GI.Pango qualified as Pango
import GI.PangoCairo qualified as PangoCairo
import Text.Printf (printf)

data Rect = Rect {rx, ry, rw, rh :: Double}

type RGB = (Double, Double, Double)

green, orange, red :: RGB
green = (0.39, 0.82, 0.38)
orange = (1.00, 0.62, 0.26)
red = (1.00, 0.33, 0.30)

levelColor :: Double -> RGB
levelColor f
    | f < 0.60 = green
    | f < 0.85 = orange
    | otherwise = red

rgba :: RGB -> Double -> Render ()
rgba (r, g, b) = setSourceRGBA r g b

font :: FontWeight -> Double -> Render ()
font weight size = do
    selectFontFace "sans-serif" FontSlantNormal weight
    setFontSize size

roundRect :: Rect -> Double -> Render ()
roundRect (Rect x y w h) r = do
    newPath
    arc (x + w - r) (y + r) r (-pi / 2) 0
    arc (x + w - r) (y + h - r) r 0 (pi / 2)
    arc (x + r) (y + h - r) r (pi / 2) pi
    arc (x + r) (y + r) r pi (3 * pi / 2)
    closePath

-- | Card background: dark, translucent, with a thin border.
card :: Rect -> Render ()
card r = do
    roundRect r 26
    setSourceRGBA 0.09 0.09 0.13 0.88
    fillPreserve
    setSourceRGBA 1 1 1 0.08
    setLineWidth 1
    stroke

divider :: (Double, Double) -> (Double, Double) -> Render ()
divider (x0, y0) (x1, y1) = do
    setSourceRGBA 1 1 1 0.10
    setLineWidth 1
    newPath
    moveTo x0 y0
    lineTo x1 y1
    stroke

-- | Text centered horizontally on @cx@, with its baseline at @y@.
centered :: Double -> Double -> String -> Render ()
centered cx y s = do
    ext <- textExtents s
    moveTo (cx - textExtentsWidth ext / 2 - textExtentsXbearing ext) y
    showText s

-- | Text right-aligned at @xr@, with its baseline at @y@.
rightAligned :: Double -> Double -> String -> Render ()
rightAligned xr y s = do
    ext <- textExtents s
    moveTo (xr - textExtentsWidth ext - textExtentsXbearing ext) y
    showText s

{- | One line of text with Pango, for arbitrary text (song titles...): it
falls back to other fonts for missing glyphs and ellipsizes to the given
width. @(x, y)@ is the top-left corner; uses the current color.
-}
pangoLine :: Double -> Double -> Double -> Double -> Bool -> Text -> Render ()
pangoLine x y width size bold txt = do
    moveTo x y
    toRender $ \ctx -> do
        layout <- PangoCairo.createLayout ctx
        desc <- Pango.fontDescriptionNew
        Pango.fontDescriptionSetFamily desc (T.pack "sans-serif")
        Pango.fontDescriptionSetAbsoluteSize desc (size * pangoScale)
        when bold $ Pango.fontDescriptionSetWeight desc Pango.WeightBold
        Pango.layoutSetFontDescription layout (Just desc)
        Pango.layoutSetWidth layout (round (width * pangoScale))
        Pango.layoutSetEllipsize layout Pango.EllipsizeModeEnd
        Pango.layoutSetText layout txt (-1)
        PangoCairo.showLayout ctx layout
  where
    pangoScale = fromIntegral Pango.SCALE

-- | 270° ring with the value in the middle. @frac@ goes from 0 to 1.
gauge :: Rect -> String -> Double -> Render ()
gauge (Rect x y w h) title frac = do
    font FontWeightNormal 12
    setSourceRGBA 1 1 1 0.65
    moveTo x (y + 12)
    showText title

    let cx = x + w / 2
        cy = y + 22 + (h - 22) / 2
        radius = min w (h - 22) / 2 - 10
        a0 = 0.75 * pi
        sweep = 1.5 * pi
        color = levelColor frac
    setLineCap LineCapRound
    setLineWidth (radius * 0.24)
    setSourceRGBA 1 1 1 0.13
    newPath
    arc cx cy radius a0 (a0 + sweep)
    stroke
    rgba color 1
    newPath
    arc cx cy radius a0 (a0 + sweep * max 0.004 frac)
    stroke

    font FontWeightBold (radius * 0.62)
    centered cx (cy + radius * 0.14) (show (round (frac * 100) :: Int))
    font FontWeightNormal (radius * 0.28)
    setSourceRGBA 1 1 1 0.65
    centered cx (cy + radius * 0.52) "%"

-- | Tile with a label, the current value and the series as a line chart.
sparkTile :: Rect -> String -> Seq Double -> Render ()
sparkTile r@(Rect x y w h) title series = do
    roundRect r 10
    setSourceRGBA 0.20 0.13 0.07 0.75
    fill

    font FontWeightNormal 12
    setSourceRGBA 1 1 1 0.65
    moveTo (x + 8) (y + 18)
    showText title

    let values = toList series
        (num, unit) = humanRate (if null values then 0 else last values)
    font FontWeightBold 17
    rgba orange 1
    moveTo (x + 8) (y + 40)
    showText num
    numExt <- textExtents num
    font FontWeightNormal 11
    moveTo (x + 8 + textExtentsXadvance numExt + 3) (y + 40)
    showText unit

    let top = y + 52
        bottom = y + h - 6
        left = x + 6
        right = x + w - 6
        n = length values
        peak = maximum (1024 : values)
        px i = left + (right - left) * fromIntegral i / fromIntegral (max 1 (n - 1))
        py v = bottom - (bottom - top) * v / peak
        pts = zip (map px [0 :: Int ..]) (map py values)
    unless (n < 2) $ do
        save
        roundRect r 10
        clip
        -- Gradient fill under the curve.
        newPath
        moveTo left bottom
        forM_ pts (uncurry lineTo)
        lineTo right bottom
        closePath
        withLinearPattern 0 top 0 bottom $ \pat -> do
            let (cr, cg, cb) = orange
            patternAddColorStopRGBA pat 0 cr cg cb 0.30
            patternAddColorStopRGBA pat 1 cr cg cb 0.02
            setSource pat
            fill
        -- The line.
        newPath
        forM_ (zip [0 :: Int ..] pts) $ \(i, (a, b)) ->
            if i == 0 then moveTo a b else lineTo a b
        rgba orange 1
        setLineWidth 1.6
        setLineJoin LineJoinRound
        setLineCap LineCapRound
        stroke
        restore

-- | Bytes per second to (number, unit).
humanRate :: Double -> (String, String)
humanRate bps
    | kb < 1000 = (printf "%.1f" kb, "kB/s")
    | otherwise = (printf "%.1f" (kb / 1024), "MB/s")
  where
    kb = bps / 1024

-- | Bytes as short text: "512 MB", "3.4 GB".
humanBytes :: Double -> String
humanBytes b
    | gb >= 100 = printf "%.0f GB" gb
    | gb >= 1 = printf "%.1f GB" gb
    | otherwise = printf "%.0f MB" (b / 1024 ^ (2 :: Int))
  where
    gb = b / 1024 ^ (3 :: Int)
