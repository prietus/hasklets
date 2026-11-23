{-# LANGUAGE OverloadedStrings #-}

-- | Themes: colors, shapes and font shared by every widget.
module Theme
    ( Theme (..)
    , GaugeStyle (..)
    , SparkStyle (..)
    , RGB
    , RGBA
    , themes
    , defaultTheme
    , themeNamed
    ) where

import Data.List (find)
import Data.Text (Text)
import Numeric (readHex)

type RGB = (Double, Double, Double)

-- | A color and its opacity.
type RGBA = (RGB, Double)

data GaugeStyle
    = -- | 270° arc, open at the bottom.
      Arc
    | -- | Thin full circle, filled clockwise from the top.
      Ring

data SparkStyle
    = -- | A line with a gradient under it.
      Area
    | -- | One bar per sample.
      Bars

data Theme = Theme
    { themeId :: Text
    -- ^ Name in the JSON file.
    , themeLabel :: Text
    -- ^ Name in the settings window.
    , cardFill :: RGBA
    , cardBorder :: RGBA
    , cardBorderWidth :: Double
    , cardRadius :: Double
    , tileFill :: RGBA
    -- ^ Background of the tiles inside a card (the network charts).
    , tileRadius :: Double
    , inkColor :: RGBA
    -- ^ Main text.
    , dimColor :: RGBA
    -- ^ Labels and secondary text.
    , trackColor :: RGBA
    -- ^ Unfilled part of gauges and bars.
    , ruleColor :: RGBA
    -- ^ Divider lines.
    , okColor :: RGB
    , warnColor :: RGB
    , critColor :: RGB
    -- ^ Levels of gauges and bars: below 60%, below 85%, above.
    , accentColor :: RGB
    -- ^ Charts and their values.
    , sunColor :: RGB
    , cloudColor :: RGB
    , rainColor :: RGB
    , fontFamily :: String
    -- ^ Where it is not installed, the system falls back to its default font.
    , gaugeStyle :: GaugeStyle
    , sparkStyle :: SparkStyle
    }

-- | The first one is the default.
themes :: [Theme]
themes = [mocha, latte, nord, gruvbox, glass]

defaultTheme :: Theme
defaultTheme = mocha

themeNamed :: Text -> Maybe Theme
themeNamed name = find ((== name) . themeId) themes

-- | @"#rrggbb"@ to a color.
hex :: String -> RGB
hex ['#', r1, r2, g1, g2, b1, b2] = (channel [r1, r2], channel [g1, g2], channel [b1, b2])
  where
    channel s = case readHex s of
        [(n, "")] -> fromIntegral (n :: Int) / 255
        _ -> 0
hex _ = (0, 0, 0)

-- | Catppuccin Mocha: dark, flat, with thin rings.
mocha :: Theme
mocha =
    Theme
        { themeId = "mocha"
        , themeLabel = "Mocha (dark)"
        , cardFill = (hex "#1e1e2e", 0.92)
        , cardBorder = (hex "#45475a", 1)
        , cardBorderWidth = 1.5
        , cardRadius = 16
        , tileFill = (hex "#313244", 0.55)
        , tileRadius = 10
        , inkColor = (hex "#cdd6f4", 1)
        , dimColor = (hex "#a6adc8", 1)
        , trackColor = (hex "#45475a", 1)
        , ruleColor = (hex "#45475a", 0.7)
        , okColor = hex "#a6e3a1"
        , warnColor = hex "#fab387"
        , critColor = hex "#f38ba8"
        , accentColor = hex "#cba6f7"
        , sunColor = hex "#f9e2af"
        , cloudColor = hex "#bac2de"
        , rainColor = hex "#89b4fa"
        , fontFamily = "Inter"
        , gaugeStyle = Ring
        , sparkStyle = Area
        }

-- | Catppuccin Latte: the light counterpart of 'mocha'.
latte :: Theme
latte =
    mocha
        { themeId = "latte"
        , themeLabel = "Latte (light)"
        , cardFill = (hex "#eff1f5", 0.94)
        , cardBorder = (hex "#ccd0da", 1)
        , tileFill = (hex "#dce0e8", 0.8)
        , inkColor = (hex "#4c4f69", 1)
        , dimColor = (hex "#6c6f85", 1)
        , trackColor = (hex "#ccd0da", 1)
        , ruleColor = (hex "#ccd0da", 1)
        , okColor = hex "#40a02b"
        , warnColor = hex "#fe640b"
        , critColor = hex "#d20f39"
        , accentColor = hex "#8839ef"
        , sunColor = hex "#df8e1d"
        , cloudColor = hex "#9ca0b0"
        , rainColor = hex "#1e66f5"
        }

-- | Nord: cool colors, squarer cards, bar charts.
nord :: Theme
nord =
    Theme
        { themeId = "nord"
        , themeLabel = "Nord"
        , cardFill = (hex "#2e3440", 0.93)
        , cardBorder = (hex "#4c566a", 1)
        , cardBorderWidth = 1
        , cardRadius = 8
        , tileFill = (hex "#3b4252", 0.85)
        , tileRadius = 5
        , inkColor = (hex "#eceff4", 1)
        , dimColor = (hex "#d8dee9", 0.7)
        , trackColor = (hex "#434c5e", 1)
        , ruleColor = (hex "#4c566a", 0.8)
        , okColor = hex "#a3be8c"
        , warnColor = hex "#ebcb8b"
        , critColor = hex "#bf616a"
        , accentColor = hex "#88c0d0"
        , sunColor = hex "#ebcb8b"
        , cloudColor = hex "#d8dee9"
        , rainColor = hex "#81a1c1"
        , fontFamily = "Inter"
        , gaugeStyle = Arc
        , sparkStyle = Bars
        }

-- | Gruvbox: warm colors, nearly square corners, monospace text.
gruvbox :: Theme
gruvbox =
    Theme
        { themeId = "gruvbox"
        , themeLabel = "Gruvbox"
        , cardFill = (hex "#282828", 0.95)
        , cardBorder = (hex "#504945", 1)
        , cardBorderWidth = 2
        , cardRadius = 4
        , tileFill = (hex "#3c3836", 1)
        , tileRadius = 2
        , inkColor = (hex "#ebdbb2", 1)
        , dimColor = (hex "#a89984", 1)
        , trackColor = (hex "#504945", 1)
        , ruleColor = (hex "#504945", 1)
        , okColor = hex "#b8bb26"
        , warnColor = hex "#fabd2f"
        , critColor = hex "#fb4934"
        , accentColor = hex "#fe8019"
        , sunColor = hex "#fabd2f"
        , cloudColor = hex "#d5c4a1"
        , rainColor = hex "#83a598"
        , fontFamily = "monospace"
        , gaugeStyle = Ring
        , sparkStyle = Bars
        }

-- | Translucent dark glass with large corners and arc gauges.
glass :: Theme
glass =
    Theme
        { themeId = "glass"
        , themeLabel = "Glass"
        , cardFill = ((0.09, 0.09, 0.13), 0.88)
        , cardBorder = ((1, 1, 1), 0.08)
        , cardBorderWidth = 1
        , cardRadius = 26
        , tileFill = ((0.20, 0.13, 0.07), 0.75)
        , tileRadius = 10
        , inkColor = ((1, 1, 1), 0.95)
        , dimColor = ((1, 1, 1), 0.65)
        , trackColor = ((1, 1, 1), 0.13)
        , ruleColor = ((1, 1, 1), 0.10)
        , okColor = (0.39, 0.82, 0.38)
        , warnColor = (1.00, 0.62, 0.26)
        , critColor = (1.00, 0.33, 0.30)
        , accentColor = (1.00, 0.62, 0.26)
        , sunColor = (1.00, 0.78, 0.25)
        , cloudColor = (0.88, 0.90, 0.94)
        , rainColor = (0.40, 0.68, 1.00)
        , fontFamily = "Inter"
        , gaugeStyle = Arc
        , sparkStyle = Area
        }
