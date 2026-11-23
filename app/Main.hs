{-# LANGUAGE OverloadedStrings #-}

-- | hasklets: widgets drawn on the desktop with gtk4-layer-shell.
--
-- Which widgets are shown, in what order and with which options is read from
-- @~/.config/hasklets/config.json@ (see "Config"); @hasklets --config@ opens
-- a window to edit it.
module Main (main) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Monad (forM, forM_, join, unless, void, when)
import Data.List (sortOn)
import Data.Maybe (isJust, isNothing)
import Data.GI.Base (touchManagedPtr)
import Data.IORef
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import GI.Cairo qualified as GICairo
import GI.Cairo.Render (Render)
import GI.Cairo.Render qualified as Cairo
import GI.Cairo.Render.Connector (renderWithContext)
import GI.GLib qualified as GLib
import GI.Gio qualified as Gio
import GI.Graphene qualified as Graphene
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
    , shWeatherPlace :: IORef (Maybe (Source, Place))
    -- ^ Source and location 'shWeather' belongs to.
    , shWeatherRedraw :: IORef (IO ())
    -- ^ Redraws the cards that show the weather.
    , shWeatherRunning :: IORef Bool
    -- ^ Whether the forecast is already being fetched for the cards there are.
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
        <*> newIORef (pure ())
        <*> newIORef False
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
    teardown <- newIORef =<< build app win layered shared current initial

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
                        build app win layered shared current cfg >>= writeIORef teardown
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
build :: Gtk.Application -> Gtk.ApplicationWindow -> Bool -> Shared -> IORef Config -> Config -> IO (IO ())
build app win layered sh current cfg = do
    let (vertical, horizontal) = cornerEdges (cfgCorner cfg)
        -- Widgets dragged out of their column get a window of their own.
        -- That needs layer-shell: without it, they all stay in the columns.
        free = [(kind, at) | layered, kind <- concat (cfgColumns cfg), Just at <- [lookup kind (cfgPositions cfg)]]
        inColumns = filter (not . null) (map (filter (`notElem` map fst free)) (cfgColumns cfg))
        valign = if vertical == LS.EdgeTop then Gtk.AlignStart else Gtk.AlignEnd
        -- The space between widgets grows with them.
        gap = round (fromIntegral (cfgGap cfg) * cfgScale cfg)
    when layered $
        forM_ [LS.EdgeTop, LS.EdgeBottom, LS.EdgeLeft, LS.EdgeRight] $ \edge -> do
            let anchored = edge == vertical || edge == horizontal
            LS.setAnchor win edge anchored
            LS.setMargin win edge (if anchored then fromIntegral (cfgMargin cfg) else 0)

    columns <- Gtk.boxNew Gtk.OrientationHorizontal gap
    Gtk.windowSetChild win (Just columns)
    threads <- newIORef []
    desk <- Desk app layered <$> newIORef [] <*> newIORef [] <*> newIORef (const (pure ())) <*> newIORef True
    let margin = fromIntegral (cfgMargin cfg)
    cols <- forM inColumns $ \kinds -> do
        col <- Gtk.boxNew Gtk.OrientationVertical gap
        Gtk.widgetSetValign col valign
        Gtk.boxAppend columns col
        forM_ kinds (addWidget sh cfg desk (Host win (margin, margin) False) threads col)
        pure col
    -- When a card is dragged out of its column: the configuration in use
    -- now has it placed, and a column or a window left empty is hidden.
    writeIORef (dkLeft desk) $ \positions -> do
        modifyIORef current (\c -> c{cfgPositions = positions})
        empty <- forM cols (fmap isNothing . Gtk.widgetGetFirstChild)
        forM_ (zip cols empty) $ \(col, none) -> when none (Gtk.widgetSetVisible col False)
        when (and empty) (Gtk.widgetSetVisible win False)

    -- With no widgets there is nothing to show.
    if null inColumns
        then Gtk.widgetSetVisible win False
        else Gtk.windowPresent win

    forM_ free $ \(kind, at@(x, y)) -> do
        own <- floating app (cfgCorner cfg) LS.LayerBottom at
        box <- Gtk.boxNew Gtk.OrientationVertical 0
        Gtk.windowSetChild own (Just box)
        addWidget sh cfg desk (Host own (fromIntegral x, fromIntegral y) True) threads box kind
        modifyIORef (dkWindows desk) (own :)
        -- A widget that cannot be shown (no media player bus) adds nothing.
        filled <- isJust <$> Gtk.widgetGetFirstChild box
        when filled (Gtk.windowPresent own)

    pure $ do
        writeIORef (dkAlive desk) False
        readIORef (dkWindows desk) >>= mapM_ Gtk.windowDestroy
        -- killThread waits for the thread to get the signal; don't block GTK on it.
        readIORef threads >>= mapM_ (forkIO . killThread)
        writeIORef (shSystemRedraw sh) (pure ())
        writeIORef (shWeatherRedraw sh) (pure ())
        writeIORef (shWeatherRunning sh) False
        Gtk.windowSetChild win (Nothing :: Maybe Gtk.Widget)

addWidget :: Shared -> Config -> Desk -> Host -> IORef [ThreadId] -> Gtk.Box -> WidgetKind -> IO ()
addWidget sh cfg desk host threads box kind = case kind of
    WeatherW -> weatherCard weatherSize (renderWeather th (cfgPlace cfg))
    ForecastW -> weatherCard forecastSize (renderForecast th (cfgPlace cfg))
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
            case (playing, actionAt (x / k) (y / k)) of
                (Just n, Just action) -> void (forkIO (send conn n action))
                _ -> pure ()
        Gtk.widgetAddController area click
    DisksW -> do
        -- The card height follows the disks there are.
        known <- readIORef (shDisks sh)
        area <- canvas box (disksSize known) (renderDisks th <$> readIORef (shDisks sh))
        background . disksLoop $ \ds -> do
            writeIORef (shDisks sh) ds
            void . GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                Gtk.drawingAreaSetContentHeight area (round (k * snd (disksSize ds)))
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
            localNow = zonedTimeToLocalTime <$> getZonedTime
        -- The card height follows the events there are to show.
        known <- calendarLines <$> localNow <*> readIORef (shAgenda sh)
        area <- canvas box (calendarSize known) draw
        background . calendarLoop $ \agenda -> do
            writeIORef (shAgenda sh) agenda
            void . GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                n <- (`calendarLines` agenda) <$> localNow
                Gtk.drawingAreaSetContentHeight area (round (k * snd (calendarSize n)))
                Gtk.widgetQueueDraw area
                pure False
  where
    th = cfgTheme cfg
    k = cfgScale cfg
    canvas = scaledCanvas desk host cfg kind
    background io = forkIO io >>= \t -> modifyIORef threads (t :)
    -- A card painted from the forecast, which is fetched once for all of them.
    weatherCard size draw = do
        let place = cfgPlace cfg
            source = cfgWeatherSource cfg
        previous <- readIORef (shWeatherPlace sh)
        when (previous /= Just (source, place)) $ writeIORef (shWeather sh) Nothing
        writeIORef (shWeatherPlace sh) (Just (source, place))
        let raining = (cfgRain cfg &&) . isRaining <$> readIORef (shWeather sh)
        area <- canvas box size $ do
            weather <- readIORef (shWeather sh)
            wet <- raining
            micros <- GLib.getMonotonicTime
            pure (draw (if wet && isJust weather then Just (fromIntegral micros / 1e6) else Nothing) weather)
        modifyIORef (shWeatherRedraw sh) (>> redrawLater area)
        -- While it rains the card is repainted 25 times a second; otherwise
        -- the weather is only looked at now and then.
        let tick = do
                alive <- readIORef (dkAlive desk)
                when alive $ do
                    wet <- raining
                    when wet (Gtk.widgetQueueDraw area)
                    void (GLib.timeoutAdd GLib.PRIORITY_DEFAULT (if wet then 40 else 5000) (False <$ tick))
        when (cfgRain cfg) tick
        running <- atomicModifyIORef' (shWeatherRunning sh) (True,)
        unless running . background . weatherLoop source place $ \w -> do
            writeIORef (shWeather sh) (Just w)
            join (readIORef (shWeatherRedraw sh))

-- | Adds to the column a canvas of the given size, painted with @draw@ and
-- enlarged by the configured scale (drawing code always works at the normal
-- size). Right-clicking any widget opens the settings window, and dragging
-- it moves it somewhere else.
scaledCanvas :: Desk -> Host -> Config -> WidgetKind -> Gtk.Box -> (Double, Double) -> IO (Render ()) -> IO Gtk.DrawingArea
scaledCanvas desk host cfg kind box (w, h) draw = do
    let k = cfgScale cfg
        paint ctx = do
            r <- draw
            renderWithContext (Cairo.scale k k >> r) ctx
    area <- Gtk.drawingAreaNew
    Gtk.drawingAreaSetContentWidth area (round (k * w))
    Gtk.drawingAreaSetContentHeight area (round (k * h))
    -- While the card is dragged, only its copy is painted.
    shown <- newIORef True
    Gtk.drawingAreaSetDrawFunc area . Just $ \_ ctx _ _ -> do
        visible <- readIORef shown
        when visible (paint ctx)
    let display visible = writeIORef shown visible >> Gtk.widgetQueueDraw area
    -- The window the card is in changes when it is dragged.
    home <- newIORef host
    modifyIORef (dkCards desk) ((kind, spot cfg home area) :)
    when (dkLayered desk) (draggable desk home cfg kind area paint display)
    secondary <- Gtk.gestureClickNew
    Gtk.gestureSingleSetButton secondary 3
    void $ Gtk.onGestureClickPressed secondary $ \_ _ _ -> openSettings
    Gtk.widgetAddController area secondary
    Gtk.boxAppend box area
    pure area

-- Dragging -----------------------------------------------------------------

-- | A window of widgets, how far it is from the two edges of the corner,
-- and whether it holds a single card placed by hand.
data Host = Host Gtk.ApplicationWindow (Double, Double) Bool

-- | What the widgets of one configuration share to be dragged around.
data Desk = Desk
    { dkApp :: Gtk.Application
    , dkLayered :: Bool
    , dkCards :: IORef [(WidgetKind, IO Spot)]
    -- ^ Where each card is right now.
    , dkWindows :: IORef [Gtk.ApplicationWindow]
    -- ^ The windows besides the main one, closed with the configuration.
    , dkLeft :: IORef ([(WidgetKind, (Int, Int))] -> IO ())
    -- ^ Told the new positions when a card has been dropped.
    , dkAlive :: IORef Bool
    -- ^ Until the configuration is torn down; timers stop when it is not.
    }

-- | A card on the screen: its distance from the two edges of the corner
-- (so it grows away from the corner), its width and its height.
type Spot = (Double, Double, Double, Double)

-- | The vertical and the horizontal edge of a corner.
cornerEdges :: Corner -> (LS.Edge, LS.Edge)
cornerEdges corner = case corner of
    TopLeft -> (LS.EdgeTop, LS.EdgeLeft)
    TopRight -> (LS.EdgeTop, LS.EdgeRight)
    BottomLeft -> (LS.EdgeBottom, LS.EdgeLeft)
    BottomRight -> (LS.EdgeBottom, LS.EdgeRight)

-- | A layer-shell window of its own, at the given distance from the corner.
floating :: Gtk.Application -> Corner -> LS.Layer -> (Int, Int) -> IO Gtk.ApplicationWindow
floating app corner layer (x, y) = do
    own <- Gtk.applicationWindowNew app
    LS.initForWindow own
    LS.setNamespace own (Just "hasklets")
    LS.setLayer own layer
    LS.setKeyboardMode own LS.KeyboardModeNone
    Gtk.widgetAddCssClass own "hasklets"
    let (vertical, horizontal) = cornerEdges corner
    forM_ [vertical, horizontal] $ \edge -> LS.setAnchor own edge True
    LS.setMargin own horizontal (fromIntegral x)
    LS.setMargin own vertical (fromIntegral y)
    pure own

spot :: Config -> IORef Host -> Gtk.DrawingArea -> IO Spot
spot cfg home area = do
    Host win (mx, my) _ <- readIORef home
    origin <- Graphene.pointZero
    (_, corner) <- Gtk.widgetComputePoint area win origin
    x <- realToFrac <$> Graphene.getPointX corner
    y <- realToFrac <$> Graphene.getPointY corner
    w <- fromIntegral <$> Gtk.widgetGetWidth area
    h <- fromIntegral <$> Gtk.widgetGetHeight area
    wide <- fromIntegral <$> Gtk.widgetGetWidth win
    tall <- fromIntegral <$> Gtk.widgetGetHeight win
    let (vertical, horizontal) = cornerEdges (cfgCorner cfg)
    pure
        ( mx + (if horizontal == LS.EdgeLeft then x else wide - x - w)
        , my + (if vertical == LS.EdgeTop then y else tall - y - h)
        , w
        , h
        )

{- | Lets a card be dragged with the left button. The card itself stays where
it is, invisible: what follows the pointer is a copy in a window of its own,
which keeps the pointer coordinates steady. On dropping it the card moves
into that window, which becomes its own, and its position is saved.
-}
draggable :: Desk -> IORef Host -> Config -> WidgetKind -> Gtk.DrawingArea -> (GICairo.Context -> IO ()) -> (Bool -> IO ()) -> IO ()
draggable desk home cfg kind area paint display = do
    drag <- Gtk.gestureDragNew
    moving <- newIORef Nothing
    let corner = cfgCorner cfg
        (vertical, horizontal) = cornerEdges corner
        -- Distances grow away from the corner.
        away edge near = if edge == near then 1 else -1
        gap = fromIntegral (cfgGap cfg) * cfgScale cfg
        margin = fromIntegral (cfgMargin cfg)
        begin = do
            (x0, y0, w, h) <- spot cfg home area
            others <- readIORef (dkCards desk) >>= mapM snd . filter ((/= kind) . fst)
            let start = (round x0, round y0) :: (Int, Int)
                -- Next to another card, or lined up with one of its sides.
                stops from size = margin : concat [[a, a + s - size, a + s + gap, a - gap - size] | o <- others, let (a, s) = from o]
                xs = stops (\(a, _, s, _) -> (a, s)) w
                ys = stops (\(_, a, _, s) -> (a, s)) h
            copy <- Gtk.drawingAreaNew
            Gtk.drawingAreaSetContentWidth copy (round w)
            Gtk.drawingAreaSetContentHeight copy (round h)
            Gtk.drawingAreaSetDrawFunc copy (Just (\_ ctx _ _ -> paint ctx))
            ghost <- floating (dkApp desk) corner LS.LayerTop start
            box <- Gtk.boxNew Gtk.OrientationVertical 0
            Gtk.boxAppend box copy
            Gtk.windowSetChild ghost (Just box)
            Gtk.windowPresent ghost
            -- The card is hidden once its copy has had time to show up.
            carried <- newIORef True
            void . GLib.timeoutAdd GLib.PRIORITY_DEFAULT 80 $ do
                still <- readIORef carried
                when still (display False)
                pure False
            at <- newIORef start
            let follow dx dy = do
                    let to = (snapTo xs (x0 + away horizontal LS.EdgeLeft * dx), snapTo ys (y0 + away vertical LS.EdgeTop * dy))
                    before <- readIORef at
                    when (to /= before) $ do
                        LS.setMargin ghost horizontal (fromIntegral (fst to))
                        LS.setMargin ghost vertical (fromIntegral (snd to))
                        writeIORef at to
                release = writeIORef carried False >> display True
                drop' = do
                    to <- readIORef at
                    onDisk <- loadConfig
                    case onDisk of
                        Right c | to /= start -> do
                            -- The card takes the place of its copy, so nothing has to be rebuilt.
                            Host old _ own <- readIORef home
                            parent <- Gtk.widgetGetParent area
                            forM_ parent $ \p -> Gtk.castTo Gtk.Box p >>= mapM_ (`Gtk.boxRemove` area)
                            Gtk.boxRemove box copy
                            Gtk.boxAppend box area
                            release
                            LS.setLayer ghost LS.LayerBottom
                            writeIORef home (Host ghost (fromIntegral (fst to), fromIntegral (snd to)) True)
                            modifyIORef (dkWindows desk) (ghost :)
                            when own (Gtk.windowDestroy old)
                            let positions = (kind, to) : filter ((/= kind) . fst) (cfgPositions c)
                            readIORef (dkLeft desk) >>= ($ positions)
                            saveConfig c{cfgPositions = positions}
                        _ -> Gtk.windowDestroy ghost >> release
            pure (follow, drop')
    void $ Gtk.onGestureDragDragUpdate drag $ \dx dy -> do
        current <- readIORef moving
        case current of
            Just (follow, _) -> follow dx dy
            -- A click is not a drag: wait until the pointer has moved a bit.
            Nothing -> when (abs dx + abs dy > 8) $ do
                started@(follow, _) <- begin
                writeIORef moving (Just started)
                follow dx dy
    void $ Gtk.onGestureDragDragEnd drag $ \_ _ -> do
        current <- readIORef moving
        writeIORef moving Nothing
        forM_ current snd
    Gtk.widgetAddController area drag
  where
    -- The nearest stop if it is close enough, and never past the edges.
    snapTo stops v = case sortOn (abs . subtract v) (filter (>= 0) stops) of
        (s : _) | abs (s - v) <= 12 -> round s
        _ -> max 0 (round v) :: Int

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
