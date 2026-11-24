{-# LANGUAGE OverloadedStrings #-}

-- | Configuration in @~/.config/hasklets/config.json@.
module Config
    ( Config (..)
    , Corner (..)
    , WidgetKind (..)
    , defaultConfig
    , configPath
    , loadConfig
    , saveConfig
    , widgetLabel
    , cornerLabel
    ) where

import Control.Exception (IOException, try)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (find, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory
import System.FilePath (takeDirectory, (</>))

import Theme
import Weather (Place (..), Source (..))

data WidgetKind = WeatherW | ForecastW | SystemW | MprisW | DisksW | ProcessesW | CalendarW | JournalW
    deriving (Eq, Enum, Bounded)

-- | Name in the JSON file.
widgetId :: WidgetKind -> Text
widgetId k = case k of
    WeatherW -> "weather"
    ForecastW -> "forecast"
    SystemW -> "system"
    MprisW -> "mpris"
    DisksW -> "disks"
    ProcessesW -> "processes"
    CalendarW -> "calendar"
    JournalW -> "journal"

-- | Name in the settings window.
widgetLabel :: WidgetKind -> Text
widgetLabel k = case k of
    WeatherW -> "Weather, compact"
    ForecastW -> "Weather, with forecast"
    SystemW -> "CPU, memory and network"
    MprisW -> "Media player"
    DisksW -> "Disks"
    ProcessesW -> "Processes"
    CalendarW -> "Calendar"
    JournalW -> "Journal"

-- | Screen corner the widgets are placed in.
data Corner = TopLeft | TopRight | BottomLeft | BottomRight
    deriving (Eq, Enum, Bounded)

cornerId :: Corner -> Text
cornerId c = case c of
    TopLeft -> "top-left"
    TopRight -> "top-right"
    BottomLeft -> "bottom-left"
    BottomRight -> "bottom-right"

cornerLabel :: Corner -> Text
cornerLabel c = case c of
    TopLeft -> "Top left"
    TopRight -> "Top right"
    BottomLeft -> "Bottom left"
    BottomRight -> "Bottom right"

data Config = Config
    { cfgTheme :: Theme
    , cfgScale :: Double
    -- ^ Size of the widgets: 1 is the normal size, 1.25 a quarter bigger.
    , cfgCorner :: Corner
    , cfgMargin :: Int
    -- ^ Distance from the screen edges.
    , cfgGap :: Int
    -- ^ Space between widgets.
    , cfgColumns :: [[WidgetKind]]
    -- ^ Columns from left to right; widgets not listed are hidden.
    , cfgPositions :: [(WidgetKind, (Int, Int))]
    -- ^ Widgets dragged out of their column: how far each one is from the
    -- two edges of the corner, horizontally and vertically.
    , cfgWeatherSource :: Source
    , cfgRain :: Bool
    -- ^ Whether rain falls over the weather cards while it rains.
    , cfgPlace :: Place
    -- ^ Forecast location. Its coordinates are sent to api.open-meteo.com.
    , cfgProcessRows :: Int
    , cfgJournalRows :: Int
    -- ^ Messages the journal card has room for.
    }

-- Themes are compared by name.
instance Eq Config where
    a == b = key a == key b
      where
        -- In whatever order they were dragged.
        positions c = Map.fromList [(widgetId k, at) | (k, at) <- cfgPositions c]
        key c = (themeId (cfgTheme c), cfgScale c, cfgCorner c, cfgMargin c, cfgGap c, cfgColumns c, positions c, cfgWeatherSource c, cfgRain c, cfgPlace c, cfgProcessRows c, cfgJournalRows c)

defaultConfig :: Config
defaultConfig =
    Config
        { cfgTheme = defaultTheme
        , cfgScale = 1
        , cfgCorner = TopLeft
        , cfgMargin = 24
        , cfgGap = 14
        , cfgColumns = [[WeatherW, SystemW, MprisW], [DisksW, ProcessesW]]
        , cfgPositions = []
        , cfgWeatherSource = OpenMeteo
        , cfgRain = True
        , cfgPlace = Place{placeName = "Madrid", placeLat = 40.42, placeLon = -3.70}
        , cfgProcessRows = 5
        , cfgJournalRows = 4
        }

-- Missing fields take their default value.
instance FromJSON Config where
    parseJSON = withObject "config" $ \o -> do
        theme <- o .:? "theme" >>= maybe (pure (cfgTheme d)) parseTheme
        size <- o .:? "scale" .!= cfgScale d
        corner <- o .:? "corner" >>= maybe (pure (cfgCorner d)) parseCorner
        margin <- o .:? "margin" .!= cfgMargin d
        gap <- o .:? "gap" .!= cfgGap d
        columns <- maybe (cfgColumns d) columnsFromIds <$> o .:? "columns"
        positions <- o .:? "positions" .!= mempty
        weather <- o .:? "weather" .!= mempty
        name <- weather .:? "name" .!= placeName (cfgPlace d)
        lat <- weather .:? "latitude" .!= placeLat (cfgPlace d)
        lon <- weather .:? "longitude" .!= placeLon (cfgPlace d)
        provider <- weather .:? "provider" .!= "open-meteo"
        address <- weather .:? "url" .!= ""
        rain <- weather .:? "rain" .!= cfgRain d
        source <- case provider :: Text of
            "open-meteo" -> pure OpenMeteo
            "wtr" -> pure (Wtr address)
            other -> fail ("unknown weather provider: " <> T.unpack other <> " (available: open-meteo, wtr)")
        processes <- o .:? "processes" .!= mempty
        rows <- processes .:? "rows" .!= cfgProcessRows d
        journal <- o .:? "journal" .!= mempty
        messages <- journal .:? "rows" .!= cfgJournalRows d
        pure
            Config
                { cfgTheme = theme
                , cfgScale = clamp 0.5 3 size
                , cfgCorner = corner
                , cfgMargin = clamp 0 500 margin
                , cfgGap = clamp 0 200 gap
                , cfgColumns = columns
                , cfgPositions =
                    [ (k, (clamp 0 20000 x, clamp 0 20000 y))
                    | (name', (x, y)) <- Map.toList (positions :: Map.Map Text (Int, Int))
                    , Just k <- [find ((== name') . widgetId) [minBound .. maxBound]]
                    ]
                , cfgWeatherSource = source
                , cfgRain = rain
                , cfgPlace = Place name (clamp (-90) 90 lat) (clamp (-180) 180 lon)
                , cfgProcessRows = clamp 1 10 rows
                , cfgJournalRows = clamp 1 8 messages
                }
      where
        d = defaultConfig
        clamp lo hi = max lo . min hi

parseTheme :: Text -> Parser Theme
parseTheme t =
    maybe (fail ("unknown theme: " <> T.unpack t <> " (available: " <> names <> ")")) pure (themeNamed t)
  where
    names = T.unpack (T.intercalate ", " (map themeId themes))

parseCorner :: Text -> Parser Corner
parseCorner t =
    maybe (fail ("unknown corner: " <> T.unpack t)) pure $
        find ((== t) . cornerId) [minBound .. maxBound]

-- Drops unknown names, repeated widgets and empty columns.
columnsFromIds :: [[Text]] -> [[WidgetKind]]
columnsFromIds = filter (not . null) . dedupe [] . map (nub . mapMaybe kind)
  where
    kind t = find ((== t) . widgetId) [minBound .. maxBound]
    dedupe _ [] = []
    dedupe seen (c : cs) = let c' = filter (`notElem` seen) c in c' : dedupe (seen <> c') cs

-- One field per line, so the file stays easy to edit by hand.
encodeConfig :: Config -> BL.ByteString
encodeConfig cfg = "{\n" <> BL.intercalate ",\n" (map field fields) <> "\n}\n"
  where
    field (k, v) = "  " <> encode (k :: Text) <> ": " <> encode v
    place = cfgPlace cfg
    fields =
        [ ("theme", toJSON (themeId (cfgTheme cfg)))
        , ("scale", toJSON (cfgScale cfg))
        , ("corner", toJSON (cornerId (cfgCorner cfg)))
        , ("margin", toJSON (cfgMargin cfg))
        , ("gap", toJSON (cfgGap cfg))
        , ("columns", toJSON (map (map widgetId) (cfgColumns cfg)))
        , ("positions", toJSON (Map.fromList [(widgetId k, [x, y]) | (k, (x, y)) <- cfgPositions cfg]))
        ,
            ( "weather"
            , object $
                ["name" .= placeName place, "latitude" .= placeLat place, "longitude" .= placeLon place]
                    <> ["rain" .= cfgRain cfg]
                    <> case cfgWeatherSource cfg of
                        OpenMeteo -> ["provider" .= ("open-meteo" :: Text)]
                        Wtr address -> ["provider" .= ("wtr" :: Text), "url" .= address]
            )
        , ("processes", object ["rows" .= cfgProcessRows cfg])
        , ("journal", object ["rows" .= cfgJournalRows cfg])
        ]

configPath :: IO FilePath
configPath = (</> "config.json") <$> getXdgDirectory XdgConfig "hasklets"

-- | Reads the configuration; if the file does not exist, returns the defaults.
loadConfig :: IO (Either String Config)
loadConfig = do
    path <- configPath
    exists <- doesFileExist path
    if not exists
        then pure (Right defaultConfig)
        else do
            contents <- try (BS.readFile path)
            pure $ case contents of
                Left e -> Left (show (e :: IOException))
                Right bytes -> eitherDecodeStrict bytes

-- | Writes the file in one step (temp file + rename), so whoever watches it
-- never reads it half-written.
saveConfig :: Config -> IO ()
saveConfig cfg = do
    path <- configPath
    createDirectoryIfMissing True (takeDirectory path)
    let tmp = path <> ".tmp"
    BL.writeFile tmp (encodeConfig cfg)
    renameFile tmp path
