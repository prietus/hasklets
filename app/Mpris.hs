{-# LANGUAGE OverloadedStrings #-}

-- | "Now playing" widget: reads and controls MPRIS players over D-Bus.
module Mpris
    ( Now
    , Action (..)
    , connect
    , mprisLoop
    , send
    , mprisSize
    , renderMpris
    , actionAt
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Monad (forM_, void, when)
import Data.GI.Base.BasicTypes (GVariant)
import Data.GI.Base.GError (GError)
import Data.GI.Base.GVariant
import Data.Int (Int32, Int64)
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GI.Cairo.Render
import GI.Cairo.Render.Connector (toRender)
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.GdkPixbuf qualified as GdkPixbuf
import GI.Gio qualified as Gio
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

import Draw
import Theme

data Track = Track
    { trBus :: Text
    -- ^ D-Bus name of the player.
    , trTitle :: Text
    , trArtist :: Text
    , trPlaying :: Bool
    , trArtUrl :: Maybe Text
    , trLength :: Int64
    -- ^ Microseconds; 0 if unknown.
    , trPosition :: Int64
    }

-- | What gets drawn: the track and its cover art, already loaded.
data Now = Now Track (Maybe GdkPixbuf.Pixbuf)

data Action = Previous | PlayPause | Next

-- D-Bus ---------------------------------------------------------------------

connect :: IO (Maybe Gio.DBusConnection)
connect =
    (Just <$> Gio.busGetSync Gio.BusTypeSession (Nothing :: Maybe Gio.Cancellable))
        `catch` \(_ :: GError) -> pure Nothing

call :: Gio.DBusConnection -> Text -> Text -> Text -> Text -> Maybe GVariant -> IO (Maybe GVariant)
call conn bus path iface method params =
    ( Just
        <$> Gio.dBusConnectionCallSync
            conn
            (Just bus)
            path
            iface
            method
            params
            Nothing
            [Gio.DBusCallFlagsNoAutoStart]
            500
            (Nothing :: Maybe Gio.Cancellable)
    )
        `catch` \(_ :: GError) -> pure Nothing

playerPath, playerIface :: Text
playerPath = "/org/mpris/MediaPlayer2"
playerIface = "org.mpris.MediaPlayer2.Player"

players :: Gio.DBusConnection -> IO [Text]
players conn = do
    reply <- call conn "org.freedesktop.DBus" "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames" Nothing
    names <- maybe (pure Nothing) fromGVariant reply
    pure
        [ n
        | Just (GVariantSinglet ns) <- [names]
        , n <- ns
        , "org.mpris.MediaPlayer2." `T.isPrefixOf` n
        ]

trackOf :: Gio.DBusConnection -> Text -> IO (Maybe Track)
trackOf conn bus = do
    params <- toGVariant (GVariantSinglet playerIface)
    reply <- call conn bus playerPath "org.freedesktop.DBus.Properties" "GetAll" (Just params)
    props <- maybe (pure Nothing) fromGVariant reply
    case props of
        Nothing -> pure Nothing
        Just (GVariantSinglet p) -> do
            playback <- prop p "PlaybackStatus"
            position <- prop p "Position"
            meta <- fromMaybe M.empty <$> prop p "Metadata"
            title <- prop meta "xesam:title"
            artists <- prop meta "xesam:artist"
            art <- prop meta "mpris:artUrl"
            len <- micros meta "mpris:length"
            pure . Just $
                Track
                    { trBus = bus
                    , trTitle = fromMaybe "" title
                    , trArtist = T.intercalate ", " (fromMaybe [] artists)
                    , trPlaying = playback == Just ("Playing" :: Text)
                    , trArtUrl = find (not . T.null) art
                    , trLength = len
                    , trPosition = fromMaybe 0 position
                    }
  where
    prop :: (IsGVariant a) => M.Map Text GVariant -> Text -> IO (Maybe a)
    prop m k = maybe (pure Nothing) fromGVariant (M.lookup k m)
    -- The spec says int64, but some players send other integer types.
    micros m k = do
        a <- prop m k :: IO (Maybe Int64)
        b <- prop m k :: IO (Maybe Word64)
        c <- prop m k :: IO (Maybe Int32)
        pure (fromMaybe 0 (listToMaybe (catMaybes [a, fromIntegral <$> b, fromIntegral <$> c])))

-- | The player that is playing; if none is, the first one with a title.
currentTrack :: Gio.DBusConnection -> IO (Maybe Track)
currentTrack conn = do
    tracks <- catMaybes <$> (players conn >>= mapM (trackOf conn))
    pure $ case filter trPlaying tracks of
        (t : _) -> Just t
        [] -> find (not . T.null . trTitle) tracks

send :: Gio.DBusConnection -> Now -> Action -> IO ()
send conn (Now tr _) action = void (call conn (trBus tr) playerPath playerIface method Nothing)
  where
    method = case action of
        Previous -> "Previous"
        PlayPause -> "PlayPause"
        Next -> "Next"

-- | Polls the player every second and hands the state to @publish@.
-- Meant to run on its own thread.
mprisLoop :: Gio.DBusConnection -> (Maybe Now -> IO ()) -> IO ()
mprisLoop conn publish = go Nothing
  where
    -- cache: last cover art URL and its image, so it isn't reloaded every second.
    go cache = do
        current <- currentTrack conn
        cache' <- case current >>= trArtUrl of
            Nothing -> pure Nothing
            Just url
                | Just (old, _) <- cache, old == url -> pure cache
                | otherwise -> Just . (url,) <$> loadArt url
        publish (fmap (\t -> Now t (cache' >>= snd)) current)
        threadDelay 1000000
        go cache'

loadArt :: Text -> IO (Maybe GdkPixbuf.Pixbuf)
loadArt url
    | "file://" `T.isPrefixOf` url =
        (GLib.filenameFromUri url >>= load . fst) `catch` \(_ :: GError) -> pure Nothing
    | "http://" `T.isPrefixOf` url || "https://" `T.isPrefixOf` url = do
        dir <- getXdgDirectory XdgCache "hasklets"
        createDirectoryIfMissing True dir
        let file = dir </> "art"
        (code, _, _) <-
            readProcessWithExitCode "curl" ["-fsSL", "--max-time", "10", "-o", file, T.unpack url] ""
        case code of
            ExitSuccess -> load file
            ExitFailure _ -> pure Nothing
    | otherwise = pure Nothing
  where
    load path =
        GdkPixbuf.pixbufNewFromFileAtScale path 256 256 True
            `catch` \(_ :: GError) -> pure Nothing

-- Drawing -------------------------------------------------------------------

-- | The space between cards decides how tall the card is, and with it how
-- big the cover is and where everything next to it goes.
mprisSize :: Double -> (Double, Double)
mprisSize gap = (cardWidth, gridHeight gap 2)

-- The cover is a square as tall as the card allows.
artRect :: Double -> Rect
artRect gap = Rect 16 16 side side
  where
    side = snd (mprisSize gap) - 32

textLeft, controlsY, progressY :: Double -> Double
textLeft gap = 16 + rw (artRect gap) + 14
controlsY gap = snd (mprisSize gap) - 52
progressY gap = snd (mprisSize gap) - 23

buttons :: Double -> [(Action, Double)]
buttons gap = [(Previous, textLeft gap + 10), (PlayPause, textLeft gap + 46), (Next, textLeft gap + 82)]

-- | Which button is under the pointer, if any.
actionAt :: Double -> Double -> Double -> Maybe Action
actionAt gap x y = fst <$> find hit (buttons gap)
  where
    hit (_, bx) = abs (x - bx) <= 16 && abs (y - controlsY gap) <= 16

renderMpris :: Theme -> Double -> Maybe Now -> Render ()
renderMpris th gap now = do
    let (w, h) = mprisSize gap
        left = textLeft gap
        textW = w - left - 16
        cy = controlsY gap
        triangle x dir size = do
            newPath
            moveTo x (cy - size)
            lineTo (x + dir * size * 1.5) cy
            lineTo x (cy + size)
            closePath
            fill
        -- Next/previous: triangle and bar.
        skip bx dir = do
            triangle (bx - dir * 5) dir 6
            newPath
            rectangle (bx + dir * 5 - 1.5) (cy - 6) 3 12
            fill
    card th (Rect 0.5 0.5 (w - 1) (h - 1))
    case now of
        Nothing -> do
            artwork th gap Nothing
            dim th
            pangoLine th left (h / 2 - 9) textW 13 False "Nothing playing"
        Just (Now tr art) -> do
            artwork th gap art
            ink th
            pangoLine th left 18 textW 14 True (trTitle tr)
            dim th
            pangoLine th left 38 textW 12 False (trArtist tr)

            ink th
            forM_ (buttons gap) $ \(action, bx) -> case action of
                Previous -> skip bx (-1)
                Next -> skip bx 1
                PlayPause
                    | trPlaying tr -> do
                        newPath
                        rectangle (bx - 6) (cy - 7) 4 14
                        rectangle (bx + 2) (cy - 7) 4 14
                        fill
                    | otherwise -> triangle (bx - 5) 1 7

            when (trLength tr > 0) $ do
                -- The progress has a line of its own, under the buttons.
                let x0 = left + 2
                    x1 = w - 18
                    py = progressY gap
                    frac = max 0 (min 1 (fromIntegral (trPosition tr) / fromIntegral (trLength tr)))
                setLineWidth 3
                setLineCap LineCapRound
                track th
                newPath >> moveTo x0 py >> lineTo x1 py >> stroke
                ink th
                newPath >> moveTo x0 py >> lineTo (x0 + (x1 - x0) * frac) py >> stroke

-- | Cover art clipped to rounded corners, or a placeholder with a note.
artwork :: Theme -> Double -> Maybe GdkPixbuf.Pixbuf -> Render ()
artwork th gap art = do
    let Rect x y w h = artRect gap
    save
    roundRect (artRect gap) (min 10 (cardRadius th))
    clip
    track th
    paint
    case art of
        Just pixbuf -> do
            pw <- fromIntegral <$> GdkPixbuf.pixbufGetWidth pixbuf
            ph <- fromIntegral <$> GdkPixbuf.pixbufGetHeight pixbuf
            -- Scaled to cover the whole square, centered.
            let k = max (w / pw) (h / ph)
            translate (x + (w - pw * k) / 2) (y + (h - ph * k) / 2)
            scale k k
            toRender $ \ctx -> Gdk.cairoSetSourcePixbuf ctx pixbuf 0 0
            paint
        Nothing -> do
            let cx = x + w / 2
                cy = y + h / 2
            dim th
            newPath
            arc (cx - 5) (cy + 9) 6 0 (2 * pi)
            fill
            setLineWidth 3
            newPath
            moveTo (cx - 0.5) (cy + 9)
            lineTo (cx - 0.5) (cy - 14)
            lineTo (cx + 10) (cy - 10)
            stroke
    restore
