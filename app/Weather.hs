{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Weather widget, with data from Open-Meteo (https://open-meteo.com, no API
-- key) or from a wtr server (Apple's WeatherKit).
module Weather
    ( Place (..)
    , Source (..)
    , Weather
    , weatherLoop
    , geocode
    , weatherSize
    , renderWeather
    , forecastSize
    , renderForecast
    , fullSize
    , renderFull
    , isRaining
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (forM_, forever, unless, when)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.List (zip4)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (Day, UTCTime, defaultTimeLocale, formatTime, getTimeZone, localTimeOfDay, parseTimeM, todHour, utcToLocalTime)
import GI.Cairo.Render
import System.Directory (getHomeDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Draw
import Theme

data Place = Place
    { placeName :: String
    , placeLat :: Double
    , placeLon :: Double
    }
    deriving (Eq)

-- | Where the forecast comes from.
data Source
    = OpenMeteo
    | -- | A wtr server at this address. Its token is read from
      -- @~/.config/wtr/api_token@, the file the wtr client uses.
      Wtr Text
    deriving (Eq)

data Weather = Weather
    { wTemp :: Double
    , wCode :: Int
    , wIsDay :: Bool
    , wMax :: Double
    , wMin :: Double
    , wHours :: [Hour]
    -- ^ The strip at the bottom: the next hours, or the next days when the
    -- source has no hourly forecast.
    , wDays :: [DayAhead]
    -- ^ Today and the days after it.
    , wCurve :: [(Int, Double)]
    -- ^ The next 24 hours: hour of the day and temperature.
    , wAlerts :: [Alert]
    -- ^ Severe weather warnings in force (only from wtr).
    , wDetail :: Maybe Detail
    -- ^ What the full card adds (only from wtr, and only when asked for).
    }

-- | The detail view of a wtr server: the air, the sky and more days.
data Detail = Detail
    { dtFeels :: Double
    , dtClouds :: Double
    , dtCondition :: String
    , dtHumidity :: Double
    , dtDewPoint :: Double
    , dtWind :: Double
    , dtGust :: Double
    , dtWindFrom :: String
    , dtRainToday :: Double
    -- ^ Millimetres.
    , dtRainChance :: Double
    -- ^ From 0 to 1.
    , dtUv :: Double
    , dtUvBand :: String
    , dtUvMax :: Double
    , dtPressure :: Double
    , dtPressureTrend :: String
    , dtVisibility :: Double
    , dtVisibilityLabel :: String
    , dtSunrise :: Maybe String
    , dtSunset :: Maybe String
    -- ^ In the local time of the place; there is none on a polar day or night.
    , dtDaylight :: Maybe Int
    -- ^ Minutes.
    , dtMoon :: Double
    -- ^ How much of it is lit, in percent.
    , dtMoonPhase :: String
    , dtDays :: [DayAhead]
    }

-- | A severe weather warning, on Meteoalarm's color scale.
data Alert = Alert
    { aLevel :: Maybe Level
    , aDescription :: String
    }

data Level = Green | Yellow | Orange | Red
    deriving (Eq, Ord)

data DayAhead = DayAhead
    { dLabel :: String
    , dCode :: Int
    , dMax :: Double
    , dMin :: Double
    , dRain :: Maybe Double
    -- ^ The chance of rain, from 0 to 1, where the source tells.
    }

data Hour = Hour
    { hLabel :: String
    , hCode :: Int
    , hIsDay :: Bool
    , hTemp :: Double
    }

-- Data ----------------------------------------------------------------------

-- | Fetches the forecast every 15 minutes, or every 5 from wtr to keep its
-- warnings fresh (retrying after a minute on failure), and hands it to
-- @publish@. Meant to run on its own thread. The detail the full card shows
-- is only asked for when @full@.
weatherLoop :: Bool -> Source -> Place -> (Weather -> IO ()) -> IO ()
weatherLoop full source place publish = forever $ do
    result <- try (fetch full source place)
    case result of
        Right (Right w) -> publish w >> threadDelay (every source)
        Right (Left err) -> failed err
        Left e -> failed (show (e :: IOException))
  where
    minute = 60 * 1000000
    every (Wtr _) = 5 * minute
    every OpenMeteo = 15 * minute
    failed err = do
        hPutStrLn stderr ("hasklets: weather: " <> err)
        threadDelay minute

fetch :: Bool -> Source -> Place -> IO (Either String Weather)
fetch full (Wtr base) place = do
    home <- getHomeDirectory
    let tokenFile = home </> ".config" </> "wtr" </> "api_token"
    token <- try (readFile tokenFile)
    case token of
        Left e -> pure (Left ("no wtr token: " <> show (e :: IOException)))
        Right contents -> do
            answer <- get contents "" wtrForecast
            case answer of
                Left problem -> pure (Left problem)
                Right (weather, days) -> do
                    -- The hours come from a second request; without them only the curve is missing.
                    hourly <- get contents "&view=hourly" wtrHours
                    hours <- case hourly of
                        Right hs -> pure hs
                        Left problem -> [] <$ hPutStrLn stderr ("hasklets: weather: hourly: " <> problem)
                    -- So do the warnings; without them, none.
                    warnings <- get contents "&view=alerts" wtrAlerts
                    alerts <- case warnings of
                        Right as -> pure as
                        Left problem -> [] <$ hPutStrLn stderr ("hasklets: weather: alerts: " <> problem)
                    -- And the detail, which only the full card shows.
                    detail <-
                        if full
                            then
                                get contents "&view=detail" wtrDetail >>= \case
                                    Right d -> pure (Just d)
                                    Left problem -> Nothing <$ hPutStrLn stderr ("hasklets: weather: detail: " <> problem)
                            else pure Nothing
                    -- Days and hours are labelled here, in local time.
                    slots <- mapM daySlot days
                    curve <- mapM hourSlot (take 24 hours)
                    pure . Right $
                        weather
                            { wHours = [Hour (dLabel d) (dCode d) True (dMax d) | d <- take 6 (drop 1 slots)]
                            , wDays = slots
                            , wCurve = curve
                            , wAlerts = alerts
                            , wDetail = detail
                            }
  where
    get :: String -> String -> (Value -> Parser a) -> IO (Either String a)
    get token query parser = do
        -- The token goes to curl through its standard input, not its command line.
        let url = T.unpack (T.dropWhileEnd (== '/') base) <> printf "/%.4f,%.4f?format=json" (placeLat place) (placeLon place) <> query
            config = "url = \"" <> url <> "\"\nheader = \"Authorization: Bearer " <> takeWhile (/= '\n') token <> "\"\n"
        (code, out, err) <- readProcessWithExitCode "curl" ["-fsS", "--max-time", "15", "-K", "-"] config
        pure $ case code of
            ExitFailure _ -> Left (takeWhile (/= '\n') err)
            ExitSuccess -> eitherDecodeStrict (TE.encodeUtf8 (T.pack out)) >>= parseEither parser
    daySlot (at, sky', top, low) = do
        zone <- getTimeZone at
        pure (DayAhead (formatTime defaultTimeLocale "%a" (utcToLocalTime zone at)) sky' top low Nothing)
    hourSlot (at, temp) = do
        zone <- getTimeZone at
        pure (todHour (localTimeOfDay (utcToLocalTime zone at)), temp)
fetch _ OpenMeteo place = do
    (code, out, err) <- readProcessWithExitCode "curl" ["-fsS", "--max-time", "15", url] ""
    pure $ case code of
        ExitSuccess -> eitherDecodeStrict (TE.encodeUtf8 (T.pack out)) >>= parseEither forecast
        ExitFailure _ -> Left (takeWhile (/= '\n') err)
  where
    url =
        printf
            "https://api.open-meteo.com/v1/forecast?latitude=%.4f&longitude=%.4f\
            \&current=temperature_2m,weather_code,is_day\
            \&hourly=temperature_2m,weather_code,is_day\
            \&daily=weather_code,temperature_2m_max,temperature_2m_min\
            \&timezone=auto&forecast_days=7"
            (placeLat place)
            (placeLon place)

{- | Looks a place up by name in the Open-Meteo geocoding service and returns
its description ("Madrid, Community of Madrid, Spain") and its coordinates.
-}
geocode :: String -> IO (Either String (String, Double, Double))
geocode name = do
    result <-
        try $
            readProcessWithExitCode
                "curl"
                [ "-fsS", "--max-time", "10", "-G", "https://geocoding-api.open-meteo.com/v1/search"
                , "--data-urlencode", "name=" <> name
                , "-d", "count=1"
                , "-d", "language=en"
                ]
                ""
    pure $ case result of
        Left e -> Left (show (e :: IOException))
        Right (ExitFailure _, _, err) -> Left (takeWhile (/= '\n') err)
        Right (ExitSuccess, out, _) -> eitherDecodeStrict (TE.encodeUtf8 (T.pack out)) >>= parseEither found
  where
    found = withObject "geocoding" $ \o -> do
        results <- o .:? "results" .!= []
        case results of
            [] -> fail "No such place was found"
            (r : _) -> do
                place <- r .: "name"
                region <- r .:? "admin1"
                country <- r .:? "country"
                lat <- r .: "latitude"
                lon <- r .: "longitude"
                let description = T.intercalate ", " (place : [x | Just x <- [region, country]])
                pure (T.unpack description, lat, lon)

forecast :: Value -> Parser Weather
forecast = withObject "forecast" $ \o -> do
    current <- o .: "current"
    now <- current .: "time" :: Parser Text
    temp <- current .: "temperature_2m"
    code <- current .: "weather_code"
    isDay <- current .: "is_day" :: Parser Int
    hourly <- o .: "hourly"
    times <- hourly .: "time" :: Parser [Text]
    temps <- hourly .: "temperature_2m"
    codes <- hourly .: "weather_code"
    days <- hourly .: "is_day" :: Parser [Int]
    daily <- o .: "daily"
    dates <- daily .: "time" :: Parser [Text]
    dayCodes <- daily .: "weather_code"
    maxs <- daily .: "temperature_2m_max"
    mins <- daily .: "temperature_2m_min"
    -- ISO times ("2026-11-14T13:00") compare correctly as text.
    let upcoming = [h | h@(t, _, _, _) <- zip4 times temps codes days, t > now]
        hourOf t = maybe 0 (`mod` 24) (readMaybe (T.unpack (T.take 2 (T.drop 11 t))))
        weekday t = case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t) :: Maybe Day of
            Just d -> formatTime defaultTimeLocale "%a" d
            Nothing -> T.unpack t
    case (maxs, mins) of
        (hi : _, lo : _) ->
            pure
                Weather
                    { wTemp = temp
                    , wCode = code
                    , wIsDay = isDay /= 0
                    , wMax = hi
                    , wMin = lo
                    , wHours = [Hour (T.unpack (T.take 2 (T.drop 11 t))) c (d /= 0) tp | (t, tp, c, d) <- take 6 upcoming]
                    , wDays = [DayAhead (weekday d) c top low Nothing | (d, c, top, low) <- zip4 dates dayCodes maxs mins]
                    , wCurve = [(hourOf t, tp) | (t, tp, _, _) <- take 24 upcoming]
                    , wAlerts = []
                    , wDetail = Nothing
                    }
        _ -> fail "no daily data"

-- | The answer of a wtr server: the weather now and today, and every day
-- from today on (when it starts, its sky, its high and its low). It has no
-- hourly forecast.
wtrForecast :: Value -> Parser (Weather, [(UTCTime, Int, Double, Double)])
wtrForecast = withObject "forecast" $ \o -> do
    current <- o .: "current"
    temp <- current .: "temp_c"
    condition <- current .: "condition_code"
    daylight <- current .: "daylight"
    daily <- o .: "daily"
    days <- mapM day daily
    case days of
        ((_, _, hi, lo) : _) -> pure (Weather temp (weatherKitCode condition) daylight hi lo [] [] [] [] Nothing, days)
        [] -> fail "no daily data"
  where
    day = withObject "day" $ \d ->
        (,,,) <$> d .: "date" <*> (weatherKitCode <$> d .: "condition_code") <*> d .: "temp_max_c" <*> d .: "temp_min_c"

-- | The hourly view of a wtr server: when each hour starts and its temperature.
wtrHours :: Value -> Parser [(UTCTime, Double)]
wtrHours = withObject "hourly" $ \o ->
    o .: "hours" >>= mapM (withObject "hour" (\h -> (,) <$> h .: "time" <*> h .: "temp_c"))

-- | The detail view of a wtr server.
wtrDetail :: Value -> Parser Detail
wtrDetail = withObject "detail" $ \o -> do
    current <- o .: "current"
    humidity <- o .: "humidity"
    wind <- o .: "wind"
    rainfall <- o .: "precipitation"
    uv <- o .: "uv"
    pressure <- o .: "pressure"
    visibility <- o .: "visibility"
    sun <- o .: "sun"
    moon <- o .: "moon"
    daily <- o .: "daily"
    Detail
        <$> current .: "feels_like_c"
        <*> current .: "cloud_cover_pct"
        <*> current .: "condition"
        <*> humidity .: "pct"
        <*> humidity .: "dew_point_c"
        <*> wind .: "speed_kmh"
        <*> wind .: "gust_kmh"
        <*> wind .: "direction"
        <*> rainfall .: "today_mm"
        <*> rainfall .: "chance_today"
        <*> uv .: "index"
        <*> uv .: "band"
        <*> uv .: "max_today"
        <*> pressure .: "hpa"
        <*> pressure .: "trend"
        <*> visibility .: "km"
        <*> visibility .: "label"
        <*> sun .:? "rise_local"
        <*> sun .:? "set_local"
        <*> sun .:? "daylight_minutes"
        <*> moon .: "illumination_pct"
        <*> moon .: "phase"
        <*> mapM day daily
  where
    day = withObject "day" $ \d ->
        DayAhead
            <$> d .: "weekday"
            <*> (weatherKitCode <$> d .: "condition_code")
            <*> d .: "temp_max_c"
            <*> d .: "temp_min_c"
            <*> d .:? "precipitation_chance"

-- | The warnings of a wtr server: @{"active": [{"level": "yellow", "description": "..."}]}@.
wtrAlerts :: Value -> Parser [Alert]
wtrAlerts = withObject "alerts" $ \o ->
    o .: "active" >>= mapM (withObject "alert" (\a -> Alert <$> (level <$> a .:? "level") <*> a .: "description"))
  where
    level :: Maybe Text -> Maybe Level
    level l = lookup (maybe "" id l) [("green", Green), ("yellow", Yellow), ("orange", Orange), ("red", Red)]

-- | The worst warning, if any.
worstAlert :: [Alert] -> Maybe Alert
worstAlert [] = Nothing
worstAlert as = Just (foldr1 (\a b -> if aLevel a >= aLevel b then a else b) as)

-- | The color of a warning: yellow, orange (the theme's warning color) or red.
alertColor :: Theme -> Alert -> RGB
alertColor th a = case aLevel a of
    Just Green -> okColor th
    Just Yellow -> sunColor th
    Just Orange -> warnColor th
    Just Red -> critColor th
    Nothing -> warnColor th

-- | What a warning says: "⚠ Yellow · Thunderstorms", and how many more there are.
alertText :: [Alert] -> Alert -> String
alertText as a =
    "⚠ " <> maybe "" ((<> " · ") . levelName) (aLevel a) <> aDescription a
        <> (if length as > 1 then printf " (+%d)" (length as - 1) else "")
  where
    levelName l = case l of
        Green -> "Green"
        Yellow -> "Yellow"
        Orange -> "Orange"
        Red -> "Red"

-- | The card's border again, thicker and in the color of the worst warning.
alertBorder :: Theme -> (Double, Double) -> Maybe Weather -> Render ()
alertBorder th (w, h) mw = forM_ (mw >>= worstAlert . wAlerts) $ \a -> do
    roundRect (Rect 1 1 (w - 2) (h - 2)) (cardRadius th)
    rgba (alertColor th a) 0.9
    setLineWidth 2
    stroke

-- | A WeatherKit condition as the nearest WMO code.
weatherKitCode :: Text -> Int
weatherKitCode c
    | c `elem` ["Clear", "Hot", "Frigid"] = 0
    | c == "MostlyClear" = 1
    | c == "PartlyCloudy" = 2
    | c `elem` ["Foggy", "Haze", "Smoky", "BlowingDust"] = 45
    | c `elem` ["Drizzle", "FreezingDrizzle"] = 53
    | c `elem` ["Rain", "HeavyRain", "FreezingRain", "Sleet", "WintryMix"] = 63
    | c == "SunShowers" = 80
    | c `elem` ["Flurries", "SunFlurries", "Snow", "BlowingSnow", "HeavySnow", "Blizzard"] = 73
    | c `elem` ["Hail", "IsolatedThunderstorms", "ScatteredThunderstorms", "Thunderstorms", "StrongStorms", "TropicalStorm", "Hurricane"] = 95
    -- MostlyCloudy, Cloudy, Breezy, Windy, and anything unknown.
    | otherwise = 3

-- WMO codes -----------------------------------------------------------------

data Sky = Clear | Partly | Cloudy | Fog | Rain | Snow | Storm

sky :: Int -> Sky
sky c
    | c <= 1 = Clear
    | c == 2 = Partly
    | c == 3 = Cloudy
    | c <= 48 = Fog
    | c <= 67 = Rain
    | c <= 77 = Snow
    | c <= 82 = Rain
    | c <= 86 = Snow
    | otherwise = Storm

describe :: Int -> String
describe c
    | c == 0 = "Clear"
    | c == 1 = "Mostly clear"
    | c == 2 = "Partly cloudy"
    | c == 3 = "Overcast"
    | c <= 48 = "Fog"
    | c <= 57 = "Drizzle"
    | c <= 67 = "Rain"
    | c <= 77 = "Snow"
    | c <= 82 = "Showers"
    | c <= 86 = "Snow showers"
    | otherwise = "Thunderstorm"

-- Drawing -------------------------------------------------------------------

weatherSize :: Double -> (Double, Double)
weatherSize gap = (cardWidth, gridHeight gap 2)

-- | Whether it rains right now, so the cards are worth animating.
isRaining :: Maybe Weather -> Bool
isRaining mw = case sky . wCode <$> mw of
    Just Rain -> True
    Just Storm -> True
    _ -> False

{- | Rain falling over a card of the given size, at a time in seconds: slanted
lines in the rain color. The drops that look near are longer, thicker, more
opaque and faster than the ones that look far. Each drop always falls along
the same line; only how far down it is depends on the time.
-}
rain :: Theme -> (Double, Double) -> Double -> Render ()
rain th (w, h) t = do
    save
    roundRect (Rect 0.5 0.5 (w - 1) (h - 1)) (cardRadius th)
    clip
    setLineCap LineCapRound
    forM_ [1 .. round (w * h / 1350) :: Int] $ \i -> do
        let depth = noise i 1
            len = 7 + 9 * depth
            fallen = part (noise i 2 + t * (0.9 + 0.7 * depth)) * (h + 2 * len) - len
            x = noise i 3 * (w + 0.18 * h) - slant * fallen
        rgba (rainColor th) (0.10 + 0.30 * depth)
        setLineWidth (0.8 + 0.7 * depth)
        newPath
        moveTo x fallen
        lineTo (x - slant * len) (fallen + len)
        stroke
    restore
  where
    slant = 0.18
    part v = v - fromIntegral (floor v :: Integer)
    -- A fixed number between 0 and 1 for each drop and each of its traits.
    noise :: Int -> Double -> Double
    noise i k = part (sin (fromIntegral i * 12.9898 + k * 78.233) * 43758.5453)

-- | The cards take the time (in seconds) to draw the rain at, when it rains.
renderWeather :: Theme -> Double -> Place -> Maybe Double -> Maybe Weather -> Render ()
renderWeather th gap place raining mw = do
    let (w, h) = weatherSize gap
        pad = 16
    card th (Rect 0.5 0.5 (w - 1) (h - 1))
    forM_ raining (rain th (w, h))
    alertBorder th (w, h) mw
    now th place mw w pad
    case mw of
        Nothing -> do
            font th FontWeightNormal 12
            dim th
            centered (w / 2) (h / 2 + 16) "Loading…"
        Just wx -> do
            let hours = wHours wx
                colW = (w - 2 * pad) / fromIntegral (max 1 (length hours))
            forM_ (zip [0 :: Int ..] hours) $ \(i, hr) -> do
                let cx = pad + colW * (fromIntegral i + 0.5)
                font th FontWeightNormal 11
                dim th
                -- The strip stands on the bottom of the card.
                centered cx (h - 56) (hLabel hr)
                icon th (sky (hCode hr)) (hIsDay hr) cx (h - 40) 22
                font th FontWeightBold 12
                ink th
                centered cx (h - 13) (degrees (hTemp hr))

-- | The top of a weather card, in a card @w@ wide: the place and, once
-- known, the weather now with today's high and low.
now :: Theme -> Place -> Maybe Weather -> Double -> Double -> Render ()
now th place mw w pad = do
    ink th
    font th FontWeightBold 14
    moveTo pad 27
    showText (placeName place)
    forM_ (mw >>= worstAlert . wAlerts) $ \a -> do
        rgba (alertColor th a) 1
        showText (" ⚠" :: String)
    forM_ mw $ \wx -> do
        font th FontWeightNormal 38
        ink th
        moveTo (pad - 2) 66
        showText (degrees (wTemp wx))

        icon th (sky (wCode wx)) (wIsDay wx) (w - pad - 11) 21 24
        font th FontWeightNormal 12
        ink th
        rightAligned (w - pad) 50 (describe (wCode wx))
        rightAligned (w - pad) 66 (printf "H: %s  L: %s" (degrees (wMax wx)) (degrees (wMin wx)))

-- | The weather now, the days ahead and the temperature of the next 24 hours.
forecastSize :: (Double, Double)
forecastSize = (cardWidth, cardWidth)

renderForecast :: Theme -> Place -> Maybe Double -> Maybe Weather -> Render ()
renderForecast th place raining mw = do
    let (w, h) = forecastSize
        pad = 16
    card th (Rect 0.5 0.5 (w - 1) (h - 1))
    forM_ raining (rain th forecastSize)
    alertBorder th forecastSize mw
    now th place mw w pad
    font th FontWeightNormal 12
    dim th
    case mw of
        Nothing -> centered (w / 2) (h / 2 + 16) "Loading…"
        Just wx -> do
            divider th (pad, 78) (w - pad, 78)
            -- The rest is laid out from below the weather now.
            translate 0 70
            font th FontWeightNormal 12
            dim th
            moveTo pad 26
            let days = wDays wx
                colW = (w - 2 * pad) / fromIntegral (max 1 (length days))
            case worstAlert (wAlerts wx) of
                Nothing -> showText (printf "%d-day forecast" (length days) :: String)
                Just a -> do
                    rgba (alertColor th a) 1
                    pangoLine th pad 13 (w - 2 * pad) 12 True (T.pack (alertText (wAlerts wx) a))
            forM_ (zip [0 :: Int ..] days) $ \(i, d) -> do
                let cx = pad + colW * (fromIntegral i + 0.5)
                font th FontWeightNormal 11
                dim th
                centered cx 47 (dLabel d)
                icon th (sky (dCode d)) True cx 63 22
                font th FontWeightBold 12
                ink th
                centered cx 90 (degrees (dMax d))
                font th FontWeightNormal 11
                dim th
                centered cx 103 (degrees (dMin d))
            divider th (pad, 114) (w - pad, 114)
            font th FontWeightNormal 12
            dim th
            moveTo pad 133
            showText ("Next 24 hours" :: String)
            temperatureCurve th (Rect pad 155 (w - 2 * pad) 44) (wCurve wx)

{- | Everything there is to tell: the weather now, the temperature of the next
24 hours, every day ahead with its chance of rain and its range of
temperatures, and the air and the sky in tiles.
-}
fullSize :: Double -> (Double, Double)
fullSize gap = (cardWidth, fitHeight gap fullNatural)

-- | How many days and rows of tiles the full card has room for, and how
-- tall it is with each at its smallest.
fullDays, fullTileRows :: Int
fullDays = 10
fullTileRows = 5

fullNatural :: Double
fullNatural = 216 + fromIntegral fullDays * 20 + 12 + fromIntegral fullTileRows * 43 + fromIntegral (fullTileRows - 1) * tileGap + 16

tileGap :: Double
tileGap = 6

renderFull :: Theme -> Double -> Place -> Maybe Double -> Maybe Weather -> Render ()
renderFull th gap place raining mw = do
    let (w, h) = fullSize gap
        pad = 16
        -- What the grid leaves over makes the days and the tiles taller.
        slack = h - fullNatural
        rowH = 20 + slack / 2 / fromIntegral fullDays
        tileH = 43 + slack / 2 / fromIntegral fullTileRows
        daysTop = 216
        tilesTop = daysTop + fromIntegral fullDays * rowH + 12
    card th (Rect 0.5 0.5 (w - 1) (h - 1))
    forM_ raining (rain th (w, h))
    alertBorder th (w, h) mw
    now th place mw w pad
    font th FontWeightNormal 12
    dim th
    case mw of
        Nothing -> centered (w / 2) (h / 2) "Loading…"
        Just wx -> do
            divider th (pad, 78) (w - pad, 78)
            font th FontWeightNormal 12
            dim th
            moveTo pad 97
            showText ("Next 24 hours" :: String)
            temperatureCurve th (Rect pad 114 (w - 2 * pad) 44) (wCurve wx)
            divider th (pad, 184) (w - pad, 184)

            let days = take fullDays (maybe (wDays wx) dtDays (wDetail wx))
            font th FontWeightNormal 12
            dim th
            moveTo pad 203
            case worstAlert (wAlerts wx) of
                Nothing -> showText (printf "%d-day forecast" (length days) :: String)
                Just a -> do
                    rgba (alertColor th a) 1
                    pangoLine th pad 190 (w - 2 * pad) 12 True (T.pack (alertText (wAlerts wx) a))
            let coldest = minimum (map dMin days)
                warmest = maximum (map dMax days)
            forM_ (zip [0 :: Int ..] days) $ \(i, d) ->
                dayRow th (Rect pad (daysTop + fromIntegral i * rowH) (w - 2 * pad) rowH) (coldest, warmest) (i == 0) d

            let tileW = (w - 2 * pad - tileGap) / 2
                place' i = Rect (pad + fromIntegral (i `mod` 2) * (tileW + tileGap)) (tilesTop + fromIntegral (i `div` 2) * (tileH + tileGap)) tileW tileH
            case wDetail wx of
                Nothing -> do
                    font th FontWeightNormal 12
                    dim th
                    centered (w / 2) (tilesTop + (h - 16 - tilesTop) / 2 + 4) "The details come from a wtr server"
                Just d -> forM_ (zip [0 :: Int ..] (take (2 * fullTileRows) (tiles d))) $ \(i, (label, value, note)) ->
                    tile th (place' i) label value note

-- | The tiles of the full card: what each is, its value and a note on it.
tiles :: Detail -> [(String, String, String)]
tiles d =
    [ ("Feels like", degrees (dtFeels d), "")
    , ("Humidity", percent (dtHumidity d), "dew point " <> degrees (dtDewPoint d))
    , ("Wind · " <> dtWindFrom d, printf "%.0f km/h" (dtWind d), printf "gusts %.0f" (dtGust d))
    , ("Rain today", printf "%.1f mm" (dtRainToday d), percent (100 * dtRainChance d) <> " chance")
    , ("UV index", whole (dtUv d), printf "%s · max %s" (dtUvBand d) (whole (dtUvMax d)))
    , ("Pressure", printf "%.0f hPa" (dtPressure d), dtPressureTrend d)
    , ("Visibility", printf "%.0f km" (dtVisibility d), dtVisibilityLabel d)
    , ("Clouds", percent (dtClouds d), dtCondition d)
    , (maybe "Sun" (("Sun · " <>) . daylight) (dtDaylight d), maybe "—" id (dtSunrise d) <> " – " <> maybe "—" id (dtSunset d), "")
    , ("Moon", percent (dtMoon d), dtMoonPhase d)
    ]
  where
    whole v = show (round v :: Int)
    percent v = whole v <> "%"
    daylight :: Int -> String
    daylight m = printf "%d h %02d min" (m `div` 60) (m `mod` 60)

-- | A tile: its label, its value and, in what is left of the line, a note,
-- in smaller letters when it would not fit otherwise.
tile :: Theme -> Rect -> String -> String -> String -> Render ()
tile th r@(Rect x y w h) label value note = do
    roundRect r (tileRadius th)
    setColor (tileFill th)
    fill
    font th FontWeightNormal 10
    dim th
    moveTo (x + 8) (y + 16)
    showText label
    font th FontWeightBold 14
    ink th
    moveTo (x + 8) (y + h - 10)
    showText value
    ext <- textExtents value
    let used = textExtentsXadvance ext + 6
        room = w - 16 - used
    unless (null note) $ do
        font th FontWeightNormal 10
        wide <- textExtentsXadvance <$> textExtents note
        let size = max 8 (min 10 (10 * room / (wide + 3)))
        dim th
        pangoLine th (x + 8 + used) (y + h - 11 - size) room size False (T.pack note)

-- | A day in a row: its name, its sky, its chance of rain when it is worth
-- telling, and where its low and its high fall between the coldest and the
-- warmest of all the days.
dayRow :: Theme -> Rect -> (Double, Double) -> Bool -> DayAhead -> Render ()
dayRow th (Rect x y w h) (coldest, warmest) today d = do
    let cy = y + h / 2
        base = cy + 4
        barL = x + 132
        barR = x + w - 36
        at t = barL + (barR - barL) * (t - coldest) / max 1 (warmest - coldest)
    font th (if today then FontWeightBold else FontWeightNormal) 12
    ink th
    moveTo x base
    showText (if today then "Today" else dLabel d)
    icon th (sky (dCode d)) True (x + 54) cy 18
    forM_ (dRain d) $ \chance -> when (chance >= 0.1) $ do
        font th FontWeightNormal 10
        rgba (rainColor th) 1
        moveTo (x + 70) base
        showText (show (round (100 * chance) :: Int) <> "%")
    font th FontWeightNormal 12
    dim th
    rightAligned (x + 124) base (degrees (dMin d))
    setLineCap LineCapRound
    setLineWidth 4
    track th
    newPath
    moveTo barL cy
    lineTo barR cy
    stroke
    rgba (accentColor th) 1
    newPath
    moveTo (at (dMin d)) cy
    lineTo (at (dMax d)) cy
    stroke
    font th FontWeightBold 12
    ink th
    rightAligned (x + w) base (degrees (dMax d))

-- | The temperatures as an area chart, with the highest one labelled and
-- the hour marked every six.
temperatureCurve :: Theme -> Rect -> [(Int, Double)] -> Render ()
temperatureCurve th (Rect x y w h) points = unless (length points < 2) $ do
    let temps = map snd points
        hi = maximum temps
        lo = minimum temps
        bottom = y + h
        px i = x + w * fromIntegral i / fromIntegral (length points - 1)
        -- The lowest temperature still stands a bit above the base line.
        py t = bottom - h * (0.15 + 0.85 * (t - lo) / max 1 (hi - lo))
        pts = zip (map px [0 :: Int ..]) (map py temps)
        accent@(cr, cg, cb) = accentColor th
        within a = max (x + a) . min (x + w - a)
    newPath
    moveTo x bottom
    forM_ pts (uncurry lineTo)
    lineTo (x + w) bottom
    closePath
    withLinearPattern 0 y 0 bottom $ \pat -> do
        patternAddColorStopRGBA pat 0 cr cg cb 0.35
        patternAddColorStopRGBA pat 1 cr cg cb 0.04
        setSource pat
        fill
    newPath
    forM_ (zip [0 :: Int ..] pts) $ \(i, (a, b)) ->
        if i == 0 then moveTo a b else lineTo a b
    rgba accent 1
    setLineWidth 1.6
    setLineJoin LineJoinRound
    setLineCap LineCapRound
    stroke
    divider th (x, bottom + 0.5) (x + w, bottom + 0.5)

    forM_ (take 1 [p | (p, t) <- zip pts temps, t == hi]) $ \(a, b) -> do
        rgba accent 1
        newPath
        arc a b 2.6 0 (2 * pi)
        fill
        font th FontWeightBold 11
        centered (within 10 a) (b - 7) (degrees hi)
    font th FontWeightNormal 10
    dim th
    forM_ (zip [0 :: Int ..] points) $ \(i, (hour, _)) ->
        when (hour `mod` 6 == 0) $ centered (within 7 (px i)) (bottom + 14) (printf "%02d" hour)

degrees :: Double -> String
degrees t = show (round t :: Int) <> "°"

-- | Icon centered on @(cx, cy)@ inside a square of side @s@.
icon :: Theme -> Sky -> Bool -> Double -> Double -> Double -> Render ()
icon th kind isDay cx cy s = case kind of
    Clear -> orb cx cy 0.24
    Partly -> orb (cx + 0.16 * s) (cy - 0.14 * s) 0.17 >> cloud (cx - 0.04 * s) (cy + 0.06 * s) 0.85
    Cloudy -> cloud cx cy 1
    Fog -> do
        rgba (cloudColor th) 0.9
        setLineWidth (0.09 * s)
        setLineCap LineCapRound
        forM_ [-0.22, 0, 0.22] $ \dy -> do
            newPath
            moveTo (cx - 0.32 * s) (cy + dy * s)
            lineTo (cx + 0.32 * s) (cy + dy * s)
            stroke
    Rain -> do
        cloud cx (cy - 0.10 * s) 0.9
        rgba (rainColor th) 1
        setLineWidth (0.07 * s)
        setLineCap LineCapRound
        forM_ [-0.20, 0, 0.20] $ \dx -> do
            newPath
            moveTo (cx + dx * s + 0.04 * s) (cy + 0.22 * s)
            lineTo (cx + dx * s - 0.04 * s) (cy + 0.40 * s)
            stroke
    Snow -> do
        cloud cx (cy - 0.10 * s) 0.9
        rgba (cloudColor th) 1
        forM_ [-0.20, 0, 0.20] $ \dx -> do
            newPath
            arc (cx + dx * s) (cy + 0.32 * s) (0.05 * s) 0 (2 * pi)
            fill
    Storm -> do
        cloud cx (cy - 0.10 * s) 0.9
        rgba (sunColor th) 1
        newPath
        moveTo (cx + 0.04 * s) (cy + 0.10 * s)
        lineTo (cx - 0.10 * s) (cy + 0.30 * s)
        lineTo (cx + 0.00 * s) (cy + 0.30 * s)
        lineTo (cx - 0.06 * s) (cy + 0.48 * s)
        lineTo (cx + 0.12 * s) (cy + 0.24 * s)
        lineTo (cx + 0.02 * s) (cy + 0.24 * s)
        closePath
        fill
  where
    -- Sun by day, moon by night.
    orb x y r
        | isDay = do
            rgba (sunColor th) 1
            newPath
            arc x y (r * s) 0 (2 * pi)
            fill
            setLineWidth (0.07 * s)
            setLineCap LineCapRound
            forM_ [0 .. 7 :: Int] $ \k -> do
                let a = fromIntegral k * pi / 4
                    r0 = (r + 0.10) * s
                    r1 = (r + 0.20) * s
                newPath
                moveTo (x + r0 * cos a) (y + r0 * sin a)
                lineTo (x + r1 * cos a) (y + r1 * sin a)
                stroke
        | otherwise = do
            let rm = (r + 0.08) * s
            pushGroup
            rgba (cloudColor th) 1
            newPath
            arc x y rm 0 (2 * pi)
            fill
            setOperator OperatorClear
            newPath
            arc (x + 0.45 * rm) (y - 0.30 * rm) (0.85 * rm) 0 (2 * pi)
            fill
            popGroupToSource
            paint
    cloud x y k = do
        let u = k * s
        rgba (cloudColor th) 1
        forM_ [(-0.20, 0.08, 0.15), (0.00, -0.04, 0.21), (0.20, 0.08, 0.15)] $ \(dx, dy, r) -> do
            newPath
            arc (x + dx * u) (y + dy * u) (r * u) 0 (2 * pi)
            fill
        newPath
        rectangle (x - 0.20 * u) (y + 0.08 * u) (0.40 * u) (0.15 * u)
        fill
