{-# LANGUAGE OverloadedStrings #-}

-- | hasklets: widgets drawn on the desktop with gtk4-layer-shell.
--
-- Which widgets are shown, in what order and with which options is read from
-- @~/.config/hasklets/config.json@ (see "Config"); @hasklets --config@ opens
-- a window to edit it.
module Main (main) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Monad (forM_, join, unless, void, when)
import Data.GI.Base (touchManagedPtr)
import Data.IORef
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import GI.Cairo.Render (Render)
import GI.Cairo.Render.Connector (renderWithContext)
import GI.GLib qualified as GLib
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import GI.Gtk4LayerShell qualified as LS
import System.Directory (doesFileExist)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Data.Time (getZonedTime, zonedTimeToLocalTime)
import System.Process (spawnProcess, waitForProcess)

import Accounts (accountsPath)
import Calendar
import Config
import Disks
import Draw
import Metrics
import Mpris
import Processes
import Settings
import Theme
import Weather

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runWidgets
        [arg] | arg `elem` ["--config", "--settings"] -> runSettings
        _ -> do
            hPutStrLn stderr "usage: hasklets            show the widgets\n       hasklets --config   open the settings window"
            exitFailure

runWidgets :: IO ()
runWidgets = do
    app <- Gtk.applicationNew (Just "dev.hasklets.Widgets") []
    -- If launched again, the running instance just gets another "activate".
    started <- newIORef False
    void $ Gio.onApplicationActivate app $ do
        already <- atomicModifyIORef' started (True,)
        unless already (activate app)
    void (Gio.applicationRun app Nothing)

-- System --------------------------------------------------------------------

cardSize :: Double
cardSize = 300

-- | Milliseconds between CPU, memory and network samples.
intervalMs :: Int
intervalMs = 1000

-- | Samples kept by each sparkline.
historyLen :: Int
historyLen = 60

data State = State
    { stRaw :: Raw
    , stCpu :: Double
    , stMem :: Double
    , stUp :: Seq Double
    , stDown :: Seq Double
    }

sample :: State -> IO State
sample st = do
    raw <- readRaw
    mem <- readMemFraction
    let r = rates (stRaw st) raw
    pure
        State
            { stRaw = raw
            , stCpu = cpuFraction r
            , stMem = mem
            , stUp = push (upBytesPerSec r) (stUp st)
            , stDown = push (downBytesPerSec r) (stDown st)
            }
  where
    push v s = Seq.drop (Seq.length s + 1 - historyLen) (s |> v)

renderSystem :: Theme -> State -> Render ()
renderSystem th st = do
    let s = cardSize
        pad = 18
        mid = s / 2
        cellW = mid - pad - 8
        cellH = mid - pad - 8
    card th (Rect 0.5 0.5 (s - 1) (s - 1))
    gauge th (Rect pad pad cellW cellH) "CPU" (stCpu st)
    gauge th (Rect (mid + 8) pad cellW cellH) "Memory" (stMem st)
    divider th (mid, pad) (mid, mid - 8)
    divider th (pad, mid) (s - pad, mid)
    divider th (mid, mid + 8) (mid, s - pad)
    sparkTile th (Rect pad (mid + 8) cellW cellH) "Network · up" (stUp st)
    sparkTile th (Rect (mid + 8) (mid + 8) cellW cellH) "Network · down" (stDown st)

-- Shared state --------------------------------------------------------------

-- | The latest data of every widget. It outlives configuration changes, so
-- rearranging the widgets neither loses the history nor leaves anything
-- blank.
data Shared = Shared
    { shWeather :: IORef (Maybe Weather)
    , shWeatherPlace :: IORef (Maybe Place)
    -- ^ Location 'shWeather' belongs to.
    , shSystem :: IORef State
    , shSystemRedraw :: IORef (IO ())
    -- ^ Redraws the system card, if it is visible.
    , shBus :: Maybe Gio.DBusConnection
    , shNow :: IORef (Maybe Now)
    , shDisks :: IORef [Disk]
    , shProcs :: IORef [Proc]
    , shAgenda :: IORef Agenda
    }

newShared :: IO Shared
newShared = do
    raw0 <- readRaw
    mem0 <- readMemFraction
    bus <- connect
    when (null bus) $
        hPutStrLn stderr "hasklets: no D-Bus session bus; the media player widget is disabled"
    Shared
        <$> newIORef Nothing
        <*> newIORef Nothing
        <*> newIORef (State raw0 0 mem0 Seq.empty Seq.empty)
        <*> newIORef (pure ())
        <*> pure bus
        <*> newIORef Nothing
        <*> newIORef []
        <*> newIORef []
        <*> newIORef emptyAgenda

-- Window --------------------------------------------------------------------

activate :: Gtk.Application -> IO ()
activate app = do
    win <- Gtk.applicationWindowNew app

    layered <- LS.isSupported
    unless layered $
        hPutStrLn stderr "hasklets: the compositor does not support layer-shell; opening as a regular window"
    when layered $ do
        LS.initForWindow win
        LS.setNamespace win (Just "hasklets")
        -- Above the wallpaper, below the windows.
        LS.setLayer win LS.LayerBottom
        LS.setKeyboardMode win LS.KeyboardModeNone

    -- No window background: only what cairo draws is visible.
    css <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromString css "window.hasklets { background: none; }"
    Gtk.widgetAddCssClass win "hasklets"
    display <- Gtk.widgetGetDisplay win
    Gtk.styleContextAddProviderForDisplay
        display
        css
        (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)

    shared <- newShared

    -- Initial configuration; if there is no file, it is created with the defaults.
    path <- configPath
    exists <- doesFileExist path
    unless exists (saveConfig defaultConfig)
    loaded <- loadConfig
    initial <- case loaded of
        Right cfg -> pure cfg
        Left err -> do
            hPutStrLn stderr ("hasklets: " <> path <> ": " <> err <> "; using the default configuration")
            pure defaultConfig
    current <- newIORef initial
    teardown <- newIORef =<< build win layered shared initial

    -- When the file changes, everything is torn down and built again.
    let reload force = do
            result <- loadConfig
            case result of
                Left err -> hPutStrLn stderr ("hasklets: " <> path <> ": " <> err <> "; keeping the previous configuration")
                Right cfg -> do
                    old <- readIORef current
                    when (force || cfg /= old) $ do
                        join (readIORef teardown)
                        writeIORef current cfg
                        build win layered shared cfg >>= writeIORef teardown
    file <- Gio.fileNewForPath path
    monitor <- Gio.fileMonitorFile file [] (Nothing :: Maybe Gio.Cancellable)
    -- One save triggers several events in a row: wait until they stop.
    pending <- newIORef (0 :: Int)
    void $ Gio.onFileMonitorChanged monitor $ \_ _ _ -> do
        mine <- atomicModifyIORef' pending (\n -> (n + 1, n + 1))
        void . GLib.timeoutAdd GLib.PRIORITY_DEFAULT 200 $ do
            latest <- readIORef pending
            when (mine == latest) (reload False)
            pure False

    -- The calendar accounts live in their own file; a change there restarts
    -- the widgets so the calendar is fetched again.
    accountsFile <- Gio.fileNewForPath =<< accountsPath
    accountsMonitor <- Gio.fileMonitorFile accountsFile [] (Nothing :: Maybe Gio.Cancellable)
    void $ Gio.onFileMonitorChanged accountsMonitor $ \_ _ _ -> do
        mine <- atomicModifyIORef' pending (\n -> (n + 1, n + 1))
        void . GLib.timeoutAdd GLib.PRIORITY_DEFAULT 200 $ do
            latest <- readIORef pending
            when (mine == latest) (reload True)
            pure False

    -- CPU, memory and network are always sampled, to keep the history.
    void . GLib.timeoutAdd GLib.PRIORITY_DEFAULT (fromIntegral intervalMs) $ do
        readIORef (shSystem shared) >>= sample >>= writeIORef (shSystem shared)
        join (readIORef (shSystemRedraw shared))
        -- Keeps the file monitors alive for as long as the program runs.
        touchManagedPtr monitor
        touchManagedPtr accountsMonitor
        pure True

-- | Builds the widgets for a configuration. Returns the action that tears
-- them down and stops their threads.
build :: Gtk.ApplicationWindow -> Bool -> Shared -> Config -> IO (IO ())
build win layered sh cfg = do
    let (vertical, horizontal) = case cfgCorner cfg of
            TopLeft -> (LS.EdgeTop, LS.EdgeLeft)
            TopRight -> (LS.EdgeTop, LS.EdgeRight)
            BottomLeft -> (LS.EdgeBottom, LS.EdgeLeft)
            BottomRight -> (LS.EdgeBottom, LS.EdgeRight)
        valign = if vertical == LS.EdgeTop then Gtk.AlignStart else Gtk.AlignEnd
        gap = fromIntegral (cfgGap cfg)
    when layered $
        forM_ [LS.EdgeTop, LS.EdgeBottom, LS.EdgeLeft, LS.EdgeRight] $ \edge -> do
            let anchored = edge == vertical || edge == horizontal
            LS.setAnchor win edge anchored
            LS.setMargin win edge (if anchored then fromIntegral (cfgMargin cfg) else 0)

    columns <- Gtk.boxNew Gtk.OrientationHorizontal gap
    Gtk.windowSetChild win (Just columns)
    threads <- newIORef []
    forM_ (filter (not . null) (cfgColumns cfg)) $ \kinds -> do
        col <- Gtk.boxNew Gtk.OrientationVertical gap
        Gtk.widgetSetValign col valign
        Gtk.boxAppend columns col
        forM_ kinds (addWidget sh cfg threads col)

    -- With no widgets there is nothing to show.
    if any (not . null) (cfgColumns cfg)
        then Gtk.windowPresent win
        else Gtk.widgetSetVisible win False

    pure $ do
        -- killThread waits for the thread to get the signal; don't block GTK on it.
        readIORef threads >>= mapM_ (forkIO . killThread)
        writeIORef (shSystemRedraw sh) (pure ())
        Gtk.windowSetChild win (Nothing :: Maybe Gtk.Widget)

addWidget :: Shared -> Config -> IORef [ThreadId] -> Gtk.Box -> WidgetKind -> IO ()
addWidget sh cfg threads box kind = case kind of
    WeatherW -> do
        let place = cfgPlace cfg
        previous <- readIORef (shWeatherPlace sh)
        when (previous /= Just place) $ writeIORef (shWeather sh) Nothing
        writeIORef (shWeatherPlace sh) (Just place)
        area <- canvas box weatherSize (renderWeather th place <$> readIORef (shWeather sh))
        background . weatherLoop place $ \w -> do
            writeIORef (shWeather sh) (Just w)
            redrawLater area
    SystemW -> do
        area <- canvas box (cardSize, cardSize) (renderSystem th <$> readIORef (shSystem sh))
        writeIORef (shSystemRedraw sh) (Gtk.widgetQueueDraw area)
    MprisW -> forM_ (shBus sh) $ \conn -> do
        area <- canvas box mprisSize (renderMpris th <$> readIORef (shNow sh))
        background . mprisLoop conn $ \n -> do
            writeIORef (shNow sh) n
            redrawLater area
        click <- Gtk.gestureClickNew
        void $ Gtk.onGestureClickPressed click $ \_ x y -> do
            playing <- readIORef (shNow sh)
            case (playing, actionAt x y) of
                (Just n, Just action) -> void (forkIO (send conn n action))
                _ -> pure ()
        Gtk.widgetAddController area click
    DisksW -> do
        -- The card height follows the number of disks.
        known <- length <$> readIORef (shDisks sh)
        area <- canvas box (disksSize known) (renderDisks th <$> readIORef (shDisks sh))
        background . disksLoop $ \ds -> do
            writeIORef (shDisks sh) ds
            void . GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                Gtk.drawingAreaSetContentHeight area (round (snd (disksSize (length ds))))
                Gtk.widgetQueueDraw area
                pure False
    ProcessesW -> do
        let rows = cfgProcessRows cfg
        area <- canvas box (processesSize rows) (renderProcesses th rows . take rows <$> readIORef (shProcs sh))
        background . processLoop rows $ \ps -> do
            writeIORef (shProcs sh) ps
            redrawLater area
    CalendarW -> do
        let draw = do
                now <- zonedTimeToLocalTime <$> getZonedTime
                renderCalendar th now <$> readIORef (shAgenda sh)
        area <- canvas box calendarSize draw
        background . calendarLoop $ \agenda -> do
            writeIORef (shAgenda sh) agenda
            redrawLater area
  where
    th = cfgTheme cfg
    background io = forkIO io >>= \t -> modifyIORef threads (t :)

-- | Adds to the column a canvas of the given size, painted with @draw@.
-- Right-clicking any widget opens the settings window.
canvas :: Gtk.Box -> (Double, Double) -> IO (Render ()) -> IO Gtk.DrawingArea
canvas box (w, h) draw = do
    area <- Gtk.drawingAreaNew
    Gtk.drawingAreaSetContentWidth area (round w)
    Gtk.drawingAreaSetContentHeight area (round h)
    Gtk.drawingAreaSetDrawFunc area . Just $ \_ ctx _ _ -> do
        r <- draw
        renderWithContext r ctx
    secondary <- Gtk.gestureClickNew
    Gtk.gestureSingleSetButton secondary 3
    void $ Gtk.onGestureClickPressed secondary $ \_ _ _ -> openSettings
    Gtk.widgetAddController area secondary
    Gtk.boxAppend box area
    pure area

openSettings :: IO ()
openSettings = do
    exe <- getExecutablePath
    process <- spawnProcess exe ["--config"]
    -- Reaped when it exits, so no zombie process is left behind.
    void (forkIO (void (waitForProcess process)))

-- | Requests a redraw from any thread (GTK is only touched from the main one).
redrawLater :: Gtk.DrawingArea -> IO ()
redrawLater area =
    void . GLib.idleAdd GLib.PRIORITY_DEFAULT $ Gtk.widgetQueueDraw area >> pure False
