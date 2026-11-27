{-# LANGUAGE OverloadedStrings #-}

-- | hasklets: widgets drawn on the desktop with gtk4-layer-shell.
--
-- Which widgets are shown, in what order and with which options is read from
-- @~/.config/hasklets/config.json@ (see "Config"); @hasklets --config@ opens
-- a window to edit it.
module Main (main) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Monad (foldM_, forM, forM_, join, unless, void, when)
import Data.Maybe (catMaybes, isJust, isNothing)
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
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Process (spawnProcess, waitForProcess)

import Accounts (accountsPath)
import Calendar
import Config
import Disks
import Draw
import Journal
import Luna
import Mpd
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
cardSize = cardWidth

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
    , shJournal :: IORef Journal
    , shBackups :: IORef (Maybe (Either String Backups))
    , shMpd :: IORef (Maybe (Either String Player))
    , shMopidy :: IORef (Maybe (Either String Player))
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
        <*> newIORef emptyJournal
        <*> newIORef Nothing
        <*> newIORef Nothing
        <*> newIORef Nothing

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
    desk <- Desk app layered <$> newIORef [] <*> newIORef (const (pure ())) <*> newIORef True
    let margin = fromIntegral (cfgMargin cfg)
    cols <- forM (if layered then [] else inColumns) $ \kinds -> do
        col <- Gtk.boxNew Gtk.OrientationVertical gap
        Gtk.widgetSetValign col valign
        Gtk.boxAppend columns col
        forM_ kinds (addWidget sh cfg desk (Host win (margin, margin) False) threads col)
        pure col
    -- Under layer-shell each card of a column has a surface of its own too,
    -- stacked by hand. A compositor that decorates layer surfaces (shadow,
    -- blur, rounded corners) then decorates each card, not one box around
    -- all of them.
    let -- Distances are counted from the corner, so the order follows it.
        towards near edge = if edge == near then id else reverse
        ordered = towards LS.EdgeLeft horizontal (map (towards LS.EdgeTop vertical) inColumns)
        step = round (cfgScale cfg * cardWidth) + gap
    stacks <- fmap (filter (not . null)) . forM (if layered then ordered else []) $ \kinds ->
        fmap catMaybes . forM kinds $ \kind -> do
            own <- floating app (cfgCorner cfg) LS.LayerBottom (cfgMargin cfg, cfgMargin cfg)
            box <- Gtk.boxNew Gtk.OrientationVertical 0
            Gtk.windowSetChild own (Just box)
            addWidget sh cfg desk (Host own (margin, margin) True) threads box kind
            modifyIORef (dkWindows desk) (own :)
            -- A widget that cannot be shown (no media player bus) adds nothing.
            child <- Gtk.widgetGetFirstChild box
            area <- maybe (pure Nothing) (Gtk.castTo Gtk.DrawingArea) child
            pure ((,,) kind own <$> area)
    let -- Puts every card still in its column under the one before. Cards
        -- dragged away since have left their window and are skipped.
        arrange = do
            alive <- readIORef (dkAlive desk)
            placed <- map fst . cfgPositions <$> readIORef current
            when alive . forM_ (zip [0 :: Int ..] stacks) $ \(i, cards) ->
                foldM_
                    ( \y (kind, own, area) ->
                        if kind `elem` placed
                            then pure y
                            else do
                                h <- Gtk.drawingAreaGetContentHeight area
                                LS.setMargin own horizontal (fromIntegral (cfgMargin cfg + i * fromIntegral step))
                                LS.setMargin own vertical y
                                pure (y + h + gap)
                    )
                    (fromIntegral (cfgMargin cfg))
                    cards
    arrange
    forM_ (concat stacks) $ \(_, own, area) -> do
        Gtk.windowPresent own
        -- Some cards change height with what they show.
        void (Gtk.onDrawingAreaResize area (\_ _ -> void (GLib.idleAdd GLib.PRIORITY_DEFAULT (arrange >> pure False))))
    -- When a card is dragged out of its column: the configuration in use
    -- now has it placed, and a column or a window left empty is hidden.
    writeIORef (dkLeft desk) $ \positions -> do
        modifyIORef current (\c -> c{cfgPositions = positions})
        arrange
        empty <- forM cols (fmap isNothing . Gtk.widgetGetFirstChild)
        forM_ (zip cols empty) $ \(col, none) -> when none (Gtk.widgetSetVisible col False)
        when (and empty) (Gtk.widgetSetVisible win False)

    -- With no widgets there is nothing to show.
    if layered || null inColumns
        then Gtk.widgetSetVisible win False
        else Gtk.windowPresent win

    -- A position saved off the grid (by hand, or with another size or
    -- spacing) is shown at the nearest place on it.
    forM_ [(kind, onGrid cfg (fromIntegral x', fromIntegral y')) | (kind, (x', y')) <- free] $ \(kind, at@(x, y)) -> do
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
    WeatherW -> weatherCard (weatherSize gap) (renderWeather th gap (cfgPlace cfg))
    ForecastW -> weatherCard forecastSize (renderForecast th (cfgPlace cfg))
    WeatherFullW -> weatherCard (fullSize gap) (renderFull th gap (cfgPlace cfg))
    SystemW -> do
        area <- canvas box (cardSize, cardSize) (renderSystem th <$> readIORef (shSystem sh))
        writeIORef (shSystemRedraw sh) (Gtk.widgetQueueDraw area)
    MprisW -> forM_ (shBus sh) $ \conn -> do
        area <- canvas box (mprisSize gap) (renderMpris th gap <$> readIORef (shNow sh))
        background . mprisLoop conn $ \n -> do
            writeIORef (shNow sh) n
            redrawLater area
        click <- Gtk.gestureClickNew
        void $ Gtk.onGestureClickPressed click $ \_ x y -> do
            playing <- readIORef (shNow sh)
            case (playing, actionAt gap (x / k) (y / k)) of
                (Just n, Just action) -> void (forkIO (send conn n action))
                _ -> pure ()
        Gtk.widgetAddController area click
    DisksW -> do
        -- The card height follows the disks there are.
        known <- readIORef (shDisks sh)
        area <- canvas box (disksSize gap known) (renderDisks th gap <$> readIORef (shDisks sh))
        background . disksLoop $ \ds -> do
            writeIORef (shDisks sh) ds
            void . GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                Gtk.drawingAreaSetContentHeight area (round (k * snd (disksSize gap ds)))
                Gtk.widgetQueueDraw area
                pure False
    ProcessesW -> do
        -- As many rows as fit in the card that holds the ones requested.
        let rows = cfgProcessRows cfg
            fit = processesRows gap rows
        area <- canvas box (processesSize gap rows) (renderProcesses th gap rows . take fit <$> readIORef (shProcs sh))
        background . processLoop fit $ \ps -> do
            writeIORef (shProcs sh) ps
            redrawLater area
    CalendarW -> do
        let draw = do
                now <- zonedTimeToLocalTime <$> getZonedTime
                renderCalendar th gap now <$> readIORef (shAgenda sh)
            localNow = zonedTimeToLocalTime <$> getZonedTime
        -- The card height follows the events there are to show.
        known <- calendarLines <$> localNow <*> readIORef (shAgenda sh)
        area <- canvas box (calendarSize gap known) draw
        background . calendarLoop $ \agenda -> do
            writeIORef (shAgenda sh) agenda
            void . GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                n <- (`calendarLines` agenda) <$> localNow
                Gtk.drawingAreaSetContentHeight area (round (k * snd (calendarSize gap n)))
                Gtk.widgetQueueDraw area
                pure False
    MpdW -> musicCard "MPD" (cfgMpd cfg) (shMpd sh)
    MopidyW -> musicCard "Mopidy" (cfgMopidy cfg) (shMopidy sh)
    LunaW -> do
        let clock = realToFrac <$> getPOSIXTime
        area <- canvas box lunaSize (renderLuna th <$> clock <*> readIORef (shBackups sh))
        background . lunaLoop $ \report -> do
            writeIORef (shBackups sh) (Just report)
            redrawLater area
    JournalW -> do
        let rows = cfgJournalRows cfg
            clock = realToFrac <$> getPOSIXTime
        -- The card height follows the messages there are to show.
        known <- journalLines rows <$> readIORef (shJournal sh)
        area <- canvas box (journalSize gap known) (renderJournal th gap rows <$> clock <*> readIORef (shJournal sh))
        background . journalLoop $ \j -> do
            writeIORef (shJournal sh) j
            void . GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                Gtk.drawingAreaSetContentHeight area (round (k * snd (journalSize gap (journalLines rows j))))
                Gtk.widgetQueueDraw area
                pure False
  where
    -- The cards as opaque as the configuration says, if it does.
    th = let t = cfgTheme cfg in maybe t (\a -> t{cardFill = (fst (cardFill t), a)}) (cfgOpacity cfg)
    k = cfgScale cfg
    canvas = scaledCanvas desk host cfg kind
    -- The grid of card heights depends on the space between cards.
    gap = fromIntegral (cfgGap cfg) :: Double
    background io = forkIO io >>= \t -> modifyIORef threads (t :)
    -- A card for a music server, with its buttons.
    musicCard name server state = do
        let order o = readIORef state >>= \playing -> void (forkIO (mpdSend server playing o))
        levels <- newIORef []
        area <- canvas box (mpdSize gap server) (renderMpd th gap server name <$> readIORef levels <*> readIORef state)
        -- The meters move many times a second, when there is an mpdvu to ask.
        forM_ (svLevels server) $ \port ->
            background . levelsLoop server port $ \now -> do
                writeIORef levels now
                redrawLater area
        background . mpdLoop server $ \player -> do
            writeIORef state (Just player)
            redrawLater area
        click <- Gtk.gestureClickNew
        void $ Gtk.onGestureClickPressed click $ \_ x y ->
            forM_ (mpdOrderAt gap server (x / k) (y / k)) order
        Gtk.widgetAddController area click
        -- The wheel turns the volume, where the server has one.
        wheel <- Gtk.eventControllerScrollNew [Gtk.EventControllerScrollFlagsVertical, Gtk.EventControllerScrollFlagsDiscrete]
        void $ Gtk.onEventControllerScrollScroll wheel $ \_ dy -> do
            when (dy /= 0) (order (Louder (if dy < 0 then 5 else -5)))
            pure True
        Gtk.widgetAddController area wheel
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
        -- The detail is only fetched when the card that shows it is there.
        let full = WeatherFullW `elem` (concat (cfgColumns cfg) <> map fst (cfgPositions cfg))
        unless running . background . weatherLoop full source place $ \w -> do
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
    Host win at own <- readIORef home
    let (vertical, horizontal) = cornerEdges (cfgCorner cfg)
    -- A card with a window of its own may have been moved since.
    (mx, my) <-
        if own
            then (,) <$> (fromIntegral <$> LS.getMargin win horizontal) <*> (fromIntegral <$> LS.getMargin win vertical)
            else pure at
    origin <- Graphene.pointZero
    (_, corner) <- Gtk.widgetComputePoint area win origin
    x <- realToFrac <$> Graphene.getPointX corner
    y <- realToFrac <$> Graphene.getPointY corner
    w <- fromIntegral <$> Gtk.widgetGetWidth area
    h <- fromIntegral <$> Gtk.widgetGetHeight area
    wide <- fromIntegral <$> Gtk.widgetGetWidth win
    tall <- fromIntegral <$> Gtk.widgetGetHeight win
    pure
        ( mx + (if horizontal == LS.EdgeLeft then x else wide - x - w)
        , my + (if vertical == LS.EdgeTop then y else tall - y - h)
        , w
        , h
        )

{- | The nearest place for a card on the grid the cards are sized for: columns
a card and a space apart, and rows a quarter of that, counted from the margin.
A card placed there lines up with the ones in the columns and with any other
placed by hand, with the same space between them all.
-}
onGrid :: Config -> (Double, Double) -> (Int, Int)
onGrid cfg (x, y) = (cell column x, cell (column / 4) y)
  where
    -- In whole pixels, as GTK lays the columns out.
    k = cfgScale cfg
    column = fromIntegral (round (k * cardWidth) + round (k * fromIntegral (cfgGap cfg)) :: Int) :: Double
    margin = fromIntegral (cfgMargin cfg)
    cell step v = round (margin + step * fromIntegral (max 0 (round ((v - margin) / step)) :: Int))

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
        begin = do
            (x0, y0, w, h) <- spot cfg home area
            let start = (round x0, round y0) :: (Int, Int)
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
                    -- The copy jumps from cell to cell, showing where the card would land.
                    let to = onGrid cfg (x0 + away horizontal LS.EdgeLeft * dx, y0 + away vertical LS.EdgeTop * dy)
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
