{-# LANGUAGE OverloadedStrings #-}

-- | Weather widget with data from Open-Meteo (https://open-meteo.com, no API key).
module Weather
    ( Place (..)
    , Weather
    , weatherLoop
    , geocode
    , weatherSize
    , renderWeather
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (forM_, forever)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.List (zip4)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GI.Cairo.Render
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

import Draw
import Theme

data Place = Place
    { placeName :: String
    , placeLat :: Double
    , placeLon :: Double
    }
    deriving (Eq)

data Weather = Weather
    { wTemp :: Double
    , wCode :: Int
    , wIsDay :: Bool
    , wMax :: Double
    , wMin :: Double
    , wHours :: [Hour]
    }

data Hour = Hour
    { hLabel :: String
    , hCode :: Int
    , hIsDay :: Bool
    , hTemp :: Double
    }

-- Data ----------------------------------------------------------------------

-- | Fetches the forecast every 15 minutes (retrying after a minute on failure)
-- and hands it to @publish@. Meant to run on its own thread.
weatherLoop :: Place -> (Weather -> IO ()) -> IO ()
weatherLoop place publish = forever $ do
    result <- try (fetch place)
    case result of
        Right (Right w) -> publish w >> threadDelay (15 * minute)
        Right (Left err) -> failed err
        Left e -> failed (show (e :: IOException))
  where
    minute = 60 * 1000000
    failed err = do
        hPutStrLn stderr ("hasklets: weather: " <> err)
        threadDelay minute

fetch :: Place -> IO (Either String Weather)
fetch place = do
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
            \&daily=temperature_2m_max,temperature_2m_min\
            \&timezone=auto&forecast_days=2"
            (placeLat place)
            (placeLon place)

{- | Looks a place up by name in the Open-Meteo geocoding service and returns
its description ("Tomares, Andalusia, Spain") and its coordinates.
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
    maxs <- daily .: "temperature_2m_max"
    mins <- daily .: "temperature_2m_min"
    case (maxs, mins) of
        (hi : _, lo : _) ->
            pure
                Weather
                    { wTemp = temp
                    , wCode = code
                    , wIsDay = isDay /= 0
                    , wMax = hi
                    , wMin = lo
                    , -- ISO times ("2026-11-14T13:00") compare correctly as text.
                      wHours =
                        take 6
                            [ Hour (T.unpack (T.take 2 (T.drop 11 t))) c (d /= 0) tp
                            | (t, tp, c, d) <- zip4 times temps codes days
                            , t > now
                            ]
                    }
        _ -> fail "no daily data"

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

weatherSize :: (Double, Double)
weatherSize = (300, 150)

renderWeather :: Theme -> Place -> Maybe Weather -> Render ()
renderWeather th place mw = do
    let (w, h) = weatherSize
        pad = 16
    card th (Rect 0.5 0.5 (w - 1) (h - 1))
    ink th
    font th FontWeightBold 14
    moveTo pad 27
    showText (placeName place)
    case mw of
        Nothing -> do
            font th FontWeightNormal 12
            dim th
            centered (w / 2) (h / 2 + 16) "Loading…"
        Just wx -> do
            font th FontWeightNormal 38
            ink th
            moveTo (pad - 2) 66
            showText (degrees (wTemp wx))

            icon th (sky (wCode wx)) (wIsDay wx) (w - pad - 11) 21 24
            font th FontWeightNormal 12
            ink th
            rightAligned (w - pad) 50 (describe (wCode wx))
            rightAligned (w - pad) 66 (printf "H: %s  L: %s" (degrees (wMax wx)) (degrees (wMin wx)))

            let hours = wHours wx
                colW = (w - 2 * pad) / fromIntegral (max 1 (length hours))
            forM_ (zip [0 :: Int ..] hours) $ \(i, hr) -> do
                let cx = pad + colW * (fromIntegral i + 0.5)
                font th FontWeightNormal 11
                dim th
                centered cx 91 (hLabel hr)
                icon th (sky (hCode hr)) (hIsDay hr) cx 108 22
                font th FontWeightBold 12
                ink th
                centered cx 136 (degrees (hTemp hr))

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
