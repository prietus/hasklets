{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Settings window (@hasklets --config@): edits the configuration JSON and
-- the calendar accounts. The running widgets watch both files and rearrange
-- when they are saved.
module Settings (runSettings) where

import Control.Concurrent (forkIO)
import Control.Exception (IOException, try)
import Control.Monad (forM_, void, when)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import GI.GLib qualified as GLib
import GI.GObject qualified as GObject
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import Accounts
import Config
import Mpd (Server (..))
import Theme
import Weather (Place (..), Source (..), geocode)

runSettings :: IO ()
runSettings = do
    app <- Gtk.applicationNew (Just "dev.hasklets.Settings") []
    -- If a settings window is already open, bring it to the front.
    open <- newIORef Nothing
    void $ Gio.onApplicationActivate app $ do
        existing <- readIORef open
        win <- maybe (settingsWindow app) pure existing
        writeIORef open (Just win)
        Gtk.windowPresent win
    void (Gio.applicationRun app Nothing)

settingsWindow :: Gtk.Application -> IO Gtk.ApplicationWindow
settingsWindow app = do
    loaded <- loadConfig
    let cfg = either (const defaultConfig) id loaded
    path <- configPath

    win <- Gtk.applicationWindowNew app
    Gtk.windowSetTitle win (Just "hasklets Settings")
    -- An explicit size: a window manager that floats the window needs one
    -- to place it (the content scrolls if it does not fit).
    Gtk.windowSetDefaultSize win 560 760

    root <- Gtk.boxNew Gtk.OrientationVertical 10
    forM_ [Gtk.widgetSetMarginTop, Gtk.widgetSetMarginBottom, Gtk.widgetSetMarginStart, Gtk.widgetSetMarginEnd] $
        \setMargin -> setMargin root 18
    -- Scrollable, in case a tiling window manager makes the window smaller
    -- than its content.
    scroller <- Gtk.scrolledWindowNew
    Gtk.scrolledWindowSetPropagateNaturalWidth scroller True
    Gtk.scrolledWindowSetPropagateNaturalHeight scroller True
    Gtk.scrolledWindowSetChild scroller (Just root)
    Gtk.windowSetChild win (Just scroller)

    -- Widgets: one list per column plus one with the hidden ones.
    heading root "Widgets"
    lists <- Gtk.boxNew Gtk.OrientationVertical 8
    Gtk.boxAppend root lists
    let shown = cfgColumns cfg
        hidden = filter (`notElem` concat shown) [minBound .. maxBound]
    layout <- newIORef (take (max 2 (length shown)) (shown <> repeat []) <> [hidden])
    refreshLists lists layout
    back <- Gtk.buttonNewWithLabel "Put dragged widgets back in their columns"
    Gtk.widgetSetHalign back Gtk.AlignStart
    Gtk.boxAppend root back
    dragNote <- note root "Drag a widget on the desktop to place it anywhere."
    void $ Gtk.onButtonClicked back $ do
        current <- loadConfig
        result <- try (either (const (pure ())) (\c -> saveConfig c{cfgPositions = []}) current)
        Gtk.labelSetText dragNote $ case result of
            Left e -> "Could not save: " <> T.pack (show (e :: IOException))
            Right () -> "Done."

    -- Appearance.
    heading root "Appearance"
    grid <- newGrid root
    theme <- Gtk.dropDownNewFromStrings (map themeLabel themes)
    Gtk.dropDownSetSelected theme (fromIntegral (length (takeWhile ((/= themeId (cfgTheme cfg)) . themeId) themes)))
    gridRow grid 0 "Theme" theme
    corner <- Gtk.dropDownNewFromStrings (map cornerLabel [minBound .. maxBound])
    Gtk.dropDownSetSelected corner (fromIntegral (fromEnum (cfgCorner cfg)))
    gridRow grid 1 "Screen corner" corner
    size <- spin 0.5 3 0.05 2 (cfgScale cfg)
    gridRow grid 4 "Widget size (1 = normal)" size
    margin <- spin 0 500 1 0 (fromIntegral (cfgMargin cfg))
    gridRow grid 2 "Margin from the edge" margin
    gap <- spin 0 200 1 0 (fromIntegral (cfgGap cfg))
    gridRow grid 3 "Space between widgets" gap

    -- Weather.
    heading root "Weather"
    wgrid <- newGrid root
    placeRow <- Gtk.boxNew Gtk.OrientationHorizontal 6
    name <- Gtk.entryNew
    Gtk.editableSetText name (T.pack (placeName (cfgPlace cfg)))
    Gtk.widgetSetHexpand name True
    Gtk.boxAppend placeRow name
    search <- Gtk.buttonNewWithLabel "Look up coordinates"
    Gtk.widgetSetTooltipText search (Just "Sends the name to the Open-Meteo geocoding service")
    Gtk.boxAppend placeRow search
    gridRow wgrid 0 "Location" placeRow
    lat <- spin (-90) 90 0.01 4 (placeLat (cfgPlace cfg))
    gridRow wgrid 1 "Latitude" lat
    lon <- spin (-180) 180 0.01 4 (placeLon (cfgPlace cfg))
    gridRow wgrid 2 "Longitude" lon
    provider' <- Gtk.dropDownNewFromStrings ["Open-Meteo", "wtr server (Apple WeatherKit)"]
    gridRow wgrid 3 "Source" provider'
    server <- Gtk.entryNew
    Gtk.entrySetPlaceholderText server (Just "Only for wtr; its token is read from ~/.config/wtr/api_token")
    gridRow wgrid 4 "Server address" server
    rain <- Gtk.switchNew
    Gtk.switchSetActive rain (cfgRain cfg)
    Gtk.widgetSetHalign rain Gtk.AlignStart
    gridRow wgrid 5 "Falling rain while it rains" rain
    case cfgWeatherSource cfg of
        OpenMeteo -> Gtk.dropDownSetSelected provider' 0
        Wtr address' -> Gtk.dropDownSetSelected provider' 1 >> Gtk.editableSetText server address'
    found <- note root ""
    void $ Gtk.onButtonClicked search $ do
        query <- Gtk.editableGetText name
        Gtk.labelSetText found "Searching…"
        void . forkIO $ do
            result <- geocode (T.unpack query)
            -- Back on the GTK thread to touch the window.
            void . GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                case result of
                    Left err -> Gtk.labelSetText found (T.pack err)
                    Right (description, la, lo) -> do
                        Gtk.spinButtonSetValue lat la
                        Gtk.spinButtonSetValue lon lo
                        Gtk.labelSetText found (T.pack description)
                pure False

    -- Processes.
    heading root "Processes"
    pgrid <- newGrid root
    rows <- spin 1 10 1 0 (fromIntegral (cfgProcessRows cfg))
    gridRow pgrid 0 "Processes shown" rows

    -- Journal.
    heading root "Journal"
    jgrid <- newGrid root
    messages <- spin 1 8 1 0 (fromIntegral (cfgJournalRows cfg))
    gridRow jgrid 0 "Messages shown" messages

    -- Music servers: each card asks its own.
    heading root "Music servers"
    mgrid <- newGrid root
    mpdHost <- Gtk.entryNew
    Gtk.editableSetText mpdHost (svHost (cfgMpd cfg))
    gridRow mgrid 0 "MPD server" mpdHost
    mpdPort <- spin 1 65535 1 0 (fromIntegral (svPort (cfgMpd cfg)))
    gridRow mgrid 1 "MPD port" mpdPort
    mpdQueue <- spin 1 10 1 0 (fromIntegral (svQueue (cfgMpd cfg)))
    gridRow mgrid 2 "MPD: songs of the queue listed" mpdQueue
    mpdLevels <- spin 0 65535 1 0 (maybe 0 fromIntegral (svLevels (cfgMpd cfg)))
    Gtk.widgetSetTooltipText mpdLevels (Just "The port of an mpdvu running next to MPD; 0 for no VU meter")
    gridRow mgrid 7 "MPD: meter port (mpdvu, 0 = off)" mpdLevels
    mpdMeter <- Gtk.dropDownNewFromStrings ["VU meter", "Spectrum (mpdvu 0.2)"]
    Gtk.dropDownSetSelected mpdMeter (if svSpectrum (cfgMpd cfg) then 1 else 0)
    gridRow mgrid 10 "MPD: meter" mpdMeter
    mopidyHost <- Gtk.entryNew
    Gtk.editableSetText mopidyHost (svHost (cfgMopidy cfg))
    gridRow mgrid 3 "Mopidy server" mopidyHost
    mopidyPort <- spin 1 65535 1 0 (fromIntegral (svPort (cfgMopidy cfg)))
    gridRow mgrid 4 "Mopidy port (MPD protocol)" mopidyPort
    mopidyWeb <- spin 1 65535 1 0 (maybe 6680 fromIntegral (svHttp (cfgMopidy cfg)))
    gridRow mgrid 5 "Mopidy port (web, for covers)" mopidyWeb
    mopidyQueue <- spin 1 10 1 0 (fromIntegral (svQueue (cfgMopidy cfg)))
    gridRow mgrid 6 "Mopidy: songs of the queue listed" mopidyQueue
    mopidyGoodies <- Gtk.switchNew
    Gtk.switchSetActive mopidyGoodies (svGoodies (cfgMopidy cfg))
    Gtk.widgetSetHalign mopidyGoodies Gtk.AlignStart
    Gtk.widgetSetTooltipText mopidyGoodies (Just "The signal path to the DAC and the pressing of the album, where the server has mopidy-goodies")
    gridRow mgrid 8 "Mopidy: use mopidy-goodies if present" mopidyGoodies
    mopidyLevels <- Gtk.dropDownNewFromStrings ["Off", "VU meter", "Spectrum"]
    Gtk.dropDownSetSelected mopidyLevels $ case (svLevels (cfgMopidy cfg), svSpectrum (cfgMopidy cfg)) of
        (Nothing, _) -> 0
        (_, False) -> 1
        (_, True) -> 2
    Gtk.widgetSetTooltipText mopidyLevels (Just "From the visualizer feed of mopidy-goodies: the audio comes over the network and the levels are worked out here")
    gridRow mgrid 9 "Mopidy: meter (mopidy-goodies)" mopidyLevels

    -- Calendars. The accounts go to their own file, not to the configuration.
    heading root "Calendars"
    stored <- either (const []) id <$> loadAccounts
    accounts <- newIORef stored
    savedAccounts <- newIORef stored
    accountList <- Gtk.boxNew Gtk.OrientationVertical 2
    Gtk.boxAppend root accountList
    refreshAccounts accountList accounts
    cgrid <- newGrid root
    provider <- Gtk.dropDownNewFromStrings (map providerLabel [minBound .. maxBound])
    gridRow cgrid 0 "Service" provider
    user <- Gtk.entryNew
    gridRow cgrid 1 "Email or user" user
    password <- Gtk.passwordEntryNew
    Gtk.passwordEntrySetShowPeekIcon password True
    gridRow cgrid 2 "App password" password
    address <- Gtk.entryNew
    Gtk.entrySetPlaceholderText address (Just "Only for a CalDAV server or an iCal address")
    gridRow cgrid 3 "Address" address
    add <- Gtk.buttonNewWithLabel "Add account"
    Gtk.widgetSetHalign add Gtk.AlignEnd
    Gtk.boxAppend root add
    calendarNote <-
        note root "Accounts are saved on Apply, to a file only you can read. Move Calendar out of Hidden to show it."
    void $ Gtk.onButtonClicked add $ do
        kind <- toEnum . min (fromEnum (maxBound :: Provider)) . fromIntegral <$> Gtk.dropDownGetSelected provider
        userV <- T.strip <$> Gtk.editableGetText user
        passwordV <- T.strip <$> Gtk.editableGetText password
        addressV <- T.strip <$> Gtk.editableGetText address
        -- Each service only keeps the fields it uses. Google shows its app
        -- passwords in groups separated by spaces that are not part of them.
        let account = case kind of
                ICloud -> Account kind userV passwordV ""
                Google -> Account kind userV (T.filter (/= ' ') passwordV) ""
                CalDav -> Account kind userV passwordV addressV
                IcsUrl -> Account kind "" "" addressV
            complete = case kind of
                IcsUrl -> not (T.null addressV)
                CalDav -> not (any T.null [userV, passwordV, addressV])
                _ -> not (any T.null [userV, passwordV])
        if complete
            then do
                modifyIORef accounts (<> [account])
                refreshAccounts accountList accounts
                forM_ [Gtk.editableSetText user, Gtk.editableSetText password, Gtk.editableSetText address] ($ "")
                Gtk.labelSetText calendarNote "Added. Press Apply to save it."
            else
                Gtk.labelSetText calendarNote $ case kind of
                    IcsUrl -> "An iCal address needs the address."
                    CalDav -> "A CalDAV server needs the address, the user and the password."
                    _ -> "This service needs the email and the app password."

    -- Save.
    status <- note root $ case loaded of
        Left err -> "The current file could not be read (" <> T.pack err <> "); showing the defaults."
        Right _ -> T.pack path
    actions <- Gtk.boxNew Gtk.OrientationHorizontal 8
    Gtk.widgetSetHalign actions Gtk.AlignEnd
    Gtk.boxAppend root actions
    close <- Gtk.buttonNewWithLabel "Close"
    void $ Gtk.onButtonClicked close (Gtk.windowClose win)
    Gtk.boxAppend actions close
    apply <- Gtk.buttonNewWithLabel "Apply"
    Gtk.widgetAddCssClass apply "suggested-action"
    Gtk.boxAppend actions apply
    void $ Gtk.onButtonClicked apply $ do
        groups <- readIORef layout
        themeIx <- Gtk.dropDownGetSelected theme
        cornerIx <- Gtk.dropDownGetSelected corner
        sizeV <- Gtk.spinButtonGetValue size
        marginV <- Gtk.spinButtonGetValue margin
        gapV <- Gtk.spinButtonGetValue gap
        nameV <- Gtk.editableGetText name
        latV <- Gtk.spinButtonGetValue lat
        lonV <- Gtk.spinButtonGetValue lon
        rowsV <- Gtk.spinButtonGetValue rows
        messagesV <- Gtk.spinButtonGetValue messages
        mpdHostV <- T.strip <$> Gtk.editableGetText mpdHost
        mpdPortV <- Gtk.spinButtonGetValue mpdPort
        mopidyHostV <- T.strip <$> Gtk.editableGetText mopidyHost
        mopidyPortV <- Gtk.spinButtonGetValue mopidyPort
        mopidyWebV <- Gtk.spinButtonGetValue mopidyWeb
        mpdQueueV <- Gtk.spinButtonGetValue mpdQueue
        mpdLevelsV <- round <$> Gtk.spinButtonGetValue mpdLevels
        mpdMeterV <- Gtk.dropDownGetSelected mpdMeter
        mopidyQueueV <- Gtk.spinButtonGetValue mopidyQueue
        mopidyGoodiesV <- Gtk.switchGetActive mopidyGoodies
        mopidyLevelsV <- Gtk.dropDownGetSelected mopidyLevels
        sourceIx <- Gtk.dropDownGetSelected provider'
        rainV <- Gtk.switchGetActive rain
        serverV <- T.strip <$> Gtk.editableGetText server
        -- Widgets may have been dragged while this window was open.
        onDisk <- loadConfig
        let new =
                Config
                    { cfgTheme = themes !! min (length themes - 1) (fromIntegral themeIx)
                    , cfgCorner = toEnum (min (fromEnum (maxBound :: Corner)) (fromIntegral cornerIx))
                    , cfgMargin = round marginV
                    , cfgGap = round gapV
                    , cfgScale = sizeV
                    , -- Only set by hand in the file: kept as it is.
                      cfgOpacity = cfgOpacity (either (const cfg) id onDisk)
                    , -- The last list holds the hidden widgets.
                      cfgColumns = filter (not . null) (init groups)
                    , cfgPositions = cfgPositions (either (const cfg) id onDisk)
                    , cfgWeatherSource = if sourceIx == 1 then Wtr serverV else OpenMeteo
                    , cfgRain = rainV
                    , cfgPlace = Place (T.unpack (T.strip nameV)) latV lonV
                    , cfgProcessRows = round rowsV
                    , cfgJournalRows = round messagesV
                    , cfgMpd = Server mpdHostV (round mpdPortV) Nothing (round mpdQueueV) (if mpdLevelsV > 0 then Just mpdLevelsV else Nothing) (mpdMeterV == 1) False
                    , cfgMopidy = Server mopidyHostV (round mopidyPortV) (Just (round mopidyWebV)) (round mopidyQueueV) (if mopidyLevelsV > 0 then Just (round mopidyWebV) else Nothing) (mopidyLevelsV == 2) mopidyGoodiesV
                    }
        saved <- try $ do
            saveConfig new
            -- The accounts file is only written when the accounts changed.
            current <- readIORef accounts
            before <- readIORef savedAccounts
            when (current /= before) $ do
                saveAccounts current
                writeIORef savedAccounts current
        Gtk.labelSetText status $ case saved of
            Left e -> "Could not save: " <> T.pack (show (e :: IOException))
            Right () -> "Saved to " <> T.pack path
    pure win

-- Calendar accounts ---------------------------------------------------------

-- | Rebuilds the list of accounts, each with a button to remove it.
refreshAccounts :: Gtk.Box -> IORef [Account] -> IO ()
refreshAccounts list accounts = do
    clear
    current <- readIORef accounts
    forM_ (zip [0 :: Int ..] current) $ \(i, account) -> do
        row <- Gtk.boxNew Gtk.OrientationHorizontal 6
        Gtk.boxAppend list row
        label <- Gtk.labelNew (Just (accountLabel account))
        Gtk.labelSetXalign label 0
        Gtk.labelSetEllipsize label Pango.EllipsizeModeMiddle
        Gtk.widgetSetHexpand label True
        Gtk.boxAppend row label
        remove <- Gtk.buttonNewWithLabel "Remove"
        Gtk.widgetAddCssClass remove "flat"
        Gtk.boxAppend row remove
        void $ Gtk.onButtonClicked remove $ do
            modifyIORef accounts (\as -> [a | (k, a) <- zip [0 ..] as, k /= i])
            refreshAccounts list accounts
  where
    clear = do
        child <- Gtk.widgetGetFirstChild list
        forM_ child $ \c -> Gtk.boxRemove list c >> clear

-- Widget lists --------------------------------------------------------------

-- | Rebuilds the lists from @layout@ (columns + hidden).
refreshLists :: Gtk.Box -> IORef [[WidgetKind]] -> IO ()
refreshLists lists layout = do
    clear
    groups <- readIORef layout
    let lastGroup = length groups - 1
    forM_ (zip [0 ..] groups) $ \(g, kinds) -> do
        let title n = if n == lastGroup then "Hidden" else "Column " <> T.pack (show (n + 1))
        frame <- Gtk.frameNew (Just (title g))
        Gtk.boxAppend lists frame
        inner <- Gtk.boxNew Gtk.OrientationVertical 2
        forM_ [Gtk.widgetSetMarginTop, Gtk.widgetSetMarginBottom, Gtk.widgetSetMarginStart, Gtk.widgetSetMarginEnd] $
            \setMargin -> setMargin inner 6
        Gtk.frameSetChild frame (Just inner)
        forM_ (zip [0 ..] kinds) $ \(i, kind) -> do
            row <- Gtk.boxNew Gtk.OrientationHorizontal 2
            Gtk.boxAppend inner row
            label <- Gtk.labelNew (Just (widgetLabel kind))
            Gtk.labelSetXalign label 0
            Gtk.widgetSetHexpand label True
            Gtk.boxAppend row label
            let button text tip enabled change = do
                    b <- Gtk.buttonNewWithLabel text
                    Gtk.widgetAddCssClass b "flat"
                    Gtk.widgetSetTooltipText b (Just tip)
                    Gtk.widgetSetSensitive b enabled
                    void $ Gtk.onButtonClicked b $ do
                        modifyIORef layout change
                        refreshLists lists layout
                    Gtk.boxAppend row b
            button "↑" "Move up" (i > 0) (moveWithin g i (i - 1))
            button "↓" "Move down" (i < length kinds - 1) (moveWithin g i (i + 1))
            -- Dropdown to move the widget to another list.
            target <- Gtk.dropDownNewFromStrings (map title [0 .. lastGroup])
            Gtk.dropDownSetSelected target (fromIntegral g)
            Gtk.widgetSetTooltipText target (Just "Move to another list")
            Gtk.boxAppend row target
            void $ GObject.onObjectNotify target (Just "selected") $ \_ -> do
                to <- fromIntegral <$> Gtk.dropDownGetSelected target
                -- Deferred: rebuilding here would destroy the dropdown emitting the signal.
                when (to /= g) . void . GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                    modifyIORef layout (moveAcross g i to)
                    refreshLists lists layout
                    pure False
  where
    clear = do
        child <- Gtk.widgetGetFirstChild lists
        forM_ child $ \c -> Gtk.boxRemove lists c >> clear

-- | Swaps two positions within list @g@.
moveWithin :: Int -> Int -> Int -> [[a]] -> [[a]]
moveWithin g i j groups =
    [ if n == g then [pick k x | (k, x) <- zip [0 ..] xs] else xs
    | (n, xs) <- zip [0 ..] groups
    , let pick k x
            | k == i = xs !! j
            | k == j = xs !! i
            | otherwise = x
    ]

-- | Takes element @i@ out of list @g@ and appends it to list @to@.
moveAcross :: Int -> Int -> Int -> [[a]] -> [[a]]
moveAcross g i to groups =
    [ if n == g
        then [x | (k, x) <- zip [0 ..] xs, k /= i]
        else if n == to then xs <> [groups !! g !! i] else xs
    | (n, xs) <- zip [0 :: Int ..] groups
    ]

-- Window building blocks ----------------------------------------------------

heading :: Gtk.Box -> Text -> IO ()
heading box text = do
    label <- Gtk.labelNew (Just text)
    Gtk.labelSetXalign label 0
    Gtk.widgetAddCssClass label "heading"
    Gtk.widgetSetMarginTop label 6
    Gtk.boxAppend box label

-- | A line of dimmed text; returns the label so it can be updated.
note :: Gtk.Box -> Text -> IO Gtk.Label
note box text = do
    label <- Gtk.labelNew (Just text)
    Gtk.labelSetXalign label 0
    Gtk.labelSetWrap label True
    Gtk.labelSetMaxWidthChars label 60
    Gtk.widgetAddCssClass label "dim-label"
    Gtk.boxAppend box label
    pure label

newGrid :: Gtk.Box -> IO Gtk.Grid
newGrid box = do
    grid <- Gtk.gridNew
    Gtk.gridSetRowSpacing grid 6
    Gtk.gridSetColumnSpacing grid 12
    Gtk.boxAppend box grid
    pure grid

gridRow :: (Gtk.IsWidget w) => Gtk.Grid -> Int -> Text -> w -> IO ()
gridRow grid row text widget = do
    label <- Gtk.labelNew (Just text)
    Gtk.labelSetXalign label 0
    Gtk.widgetSetSizeRequest label 190 (-1)
    Gtk.gridAttach grid label 0 (fromIntegral row) 1 1
    Gtk.widgetSetHexpand widget True
    Gtk.gridAttach grid widget 1 (fromIntegral row) 1 1

spin :: Double -> Double -> Double -> Int -> Double -> IO Gtk.SpinButton
spin lo hi step digits value = do
    s <- Gtk.spinButtonNewWithRange lo hi step
    when (digits > 0) $ Gtk.spinButtonSetDigits s (fromIntegral digits)
    Gtk.spinButtonSetValue s value
    -- The wheel must scroll the window, not change the number that happens
    -- to be under the pointer: that edited values nobody meant to touch.
    controllers <- Gtk.widgetObserveControllers s
    count <- Gio.listModelGetNItems controllers
    forM_ [0 .. count - 1] $ \i -> do
        item <- Gio.listModelGetItem controllers i
        scroll <- maybe (pure Nothing) (GObject.castTo Gtk.EventControllerScroll) item
        forM_ scroll (`Gtk.eventControllerSetPropagationPhase` Gtk.PropagationPhaseNone)
    pure s
