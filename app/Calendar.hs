{-# LANGUAGE OverloadedStrings #-}

-- | Agenda widget: upcoming events from CalDAV accounts (iCloud, Google, any
-- server) and iCal addresses.
module Calendar
    ( Agenda
    , emptyAgenda
    , calendarLoop
    , calendarSize
    , renderCalendar
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, evaluate, try)
import Control.Monad (forM, forM_)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Either (lefts, rights)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time
import Data.Time.Zones (TZ, loadLocalTZ, loadSystemTZ, localTimeToUTCTZ, utcTZ, utcToLocalTimeTZ)
import GI.Cairo.Render hiding (status)
import Numeric (readHex)
import System.Exit (ExitCode (..))
import System.IO (hClose, hPutStrLn, stderr)
import System.Process
import Text.XML.Light (Element, QName (qName), filterElementsName, findAttrBy, parseXMLDoc, strContent)

import Accounts
import Draw
import Ical
import Theme

data Event = Event
    { evStart :: LocalTime
    , evEnd :: LocalTime
    -- ^ Both in the system's time zone.
    , evAllDay :: Bool
    , evTitle :: Text
    , evColor :: Maybe RGB
    -- ^ The calendar's own color, when the server gives one.
    }

data Agenda = Agenda
    { agEvents :: [Event]
    , agErrors :: [Text]
    , agConfigured :: Bool
    -- ^ Whether there is any account.
    , agLoaded :: Bool
    }

emptyAgenda :: Agenda
emptyAgenda = Agenda [] [] False False

-- | Days ahead that are fetched.
horizon :: Integer
horizon = 14

-- Loop ----------------------------------------------------------------------

-- | Hands the agenda to @publish@ every minute (so finished events drop off)
-- and fetches it again every ten. Meant to run on its own thread.
calendarLoop :: (Agenda -> IO ()) -> IO ()
calendarLoop publish = go (0 :: Int) emptyAgenda
  where
    go n previous = do
        agenda <- if n `mod` 10 == 0 then refresh previous else pure previous
        publish agenda
        threadDelay (60 * 1000000)
        go (n + 1) agenda

refresh :: Agenda -> IO Agenda
refresh previous = do
    loaded <- loadAccounts
    case loaded of
        Left err -> pure previous{agErrors = ["accounts.json: " <> T.pack err], agLoaded = True}
        Right [] -> pure (Agenda [] [] False True)
        Right accounts -> do
            zone <- localZone
            today <- localDay . utcToLocalTimeTZ zone <$> getCurrentTime
            results <- forM accounts $ \account -> do
                result <- fetchAccount zone today account
                pure (either (\err -> Left (accountLabel account <> ": " <> err)) Right result)
            let errors = lefts results
            forM_ errors $ \err -> hPutStrLn stderr ("hasklets: calendar: " <> T.unpack err)
            pure
                Agenda
                    { -- If nothing could be fetched, what was there before is better than nothing.
                      agEvents = if null (rights results) then agEvents previous else concat (rights results)
                    , agErrors = errors
                    , agConfigured = True
                    , agLoaded = True
                    }

-- | The system's time zone; UTC on a system with none configured.
localZone :: IO TZ
localZone = either (\(_ :: IOException) -> utcTZ) id <$> try loadLocalTZ

fetchAccount :: TZ -> Day -> Account -> IO (Either Text [Event])
fetchAccount zone today account = case acProvider account of
    ICloud -> caldav "https://caldav.icloud.com/"
    Google -> hint <$> caldav ("https://www.google.com/calendar/dav/" <> acUser account <> "/events/")
    CalDav -> caldav (acUrl account)
    IcsUrl -> do
        -- webcal:// is just https:// under another name.
        let url = maybe (acUrl account) ("https://" <>) (T.stripPrefix "webcal://" (acUrl account))
        response <- http (Request "GET" url Nothing Nothing Nothing)
        case response of
            Left err -> pure (Left err)
            Right (status, body)
                | status == 200 -> Right <$> resolve zone today Nothing (parseIcal (decode body))
                | otherwise -> pure (Left (statusError status))
  where
    auth = Just (acUser account, acPassword account)
    caldav base = do
        found <- discover auth base
        -- A URL that is not an account root is taken as the calendar itself.
        let calendars = case found of
                Right cs@(_ : _) -> cs
                _ -> [(base, Nothing)]
        results <- forM calendars $ \(url, tint) -> do
            objects <- query auth url today
            either (pure . Left) (fmap Right . resolve zone today tint . concatMap parseIcal) objects
        pure $ case (rights results, lefts results, found) of
            (events@(_ : _), _, _) -> Right (concat events)
            (_, _, Left err) -> Left err
            (_, err : _, _) -> Left err
            ([], [], _) -> Right []
    hint (Left err) = Left (err <> " — if Google rejects the app password, add the calendar's secret iCal address instead")
    hint ok = ok

-- CalDAV --------------------------------------------------------------------

-- | The calendars of an account that hold events, with their colors:
-- principal, then its calendar home, then the collections in it.
discover :: Maybe (Text, Text) -> Text -> IO (Either Text [(Text, Maybe RGB)])
discover auth base = do
    principal <- propfind base "0" "<d:current-user-principal/>" (href "current-user-principal")
    case principal of
        Left err -> pure (Left err)
        Right Nothing -> pure (Right [])
        Right (Just principalUrl) -> do
            home <- propfind principalUrl "0" "<c:calendar-home-set/>" (href "calendar-home-set")
            case home of
                Left err -> pure (Left err)
                Right Nothing -> pure (Right [])
                Right (Just homeUrl) ->
                    propfind
                        homeUrl
                        "1"
                        "<d:resourcetype/><d:displayname/><a:calendar-color/><c:supported-calendar-component-set/>"
                        (\url doc -> mapMaybe (calendar url) (named "response" doc))
  where
    propfind url depth props extract = do
        let body =
                "<d:propfind xmlns:d=\"DAV:\" xmlns:c=\"urn:ietf:params:xml:ns:caldav\" xmlns:a=\"http://apple.com/ns/ical/\"><d:prop>"
                    <> props
                    <> "</d:prop></d:propfind>"
        response <- http (Request "PROPFIND" url auth (Just depth) (Just body))
        pure $ case response of
            Left err -> Left err
            Right (status, bytes)
                | status == 207, Just doc <- parseXMLDoc (T.unpack (decode bytes)) -> Right (extract url doc)
                | status == 207 -> Left "unreadable answer from the server"
                | otherwise -> Left (statusError status)
    href name url doc = absolute url . text <$> listToMaybe (named name doc >>= named "href")
    calendar url response = do
        link <- listToMaybe (named "href" response)
        let components = mapMaybe (findAttrBy ((== "name") . qName)) (named "comp" response)
            isCalendar = not (null (named "resourcetype" response >>= named "calendar"))
        if isCalendar && (null components || "VEVENT" `elem` components)
            then Just (absolute url (text link), listToMaybe (named "calendar-color" response) >>= color . text)
            else Nothing

-- | The iCalendar objects of a calendar with events in the next 'horizon'
-- days. The server is asked to expand repeating events; if it does not,
-- 'occurrences' does it here.
query :: Maybe (Text, Text) -> Text -> Day -> IO (Either Text [Text])
query auth url today = do
    let utcStamp d = T.pack (formatTime defaultTimeLocale "%Y%m%dT000000Z" d)
        range = "start=\"" <> utcStamp (addDays (-1) today) <> "\" end=\"" <> utcStamp (addDays (horizon + 1) today) <> "\""
        body =
            "<c:calendar-query xmlns:d=\"DAV:\" xmlns:c=\"urn:ietf:params:xml:ns:caldav\">"
                <> "<d:prop><c:calendar-data><c:expand "
                <> range
                <> "/></c:calendar-data></d:prop>"
                <> "<c:filter><c:comp-filter name=\"VCALENDAR\"><c:comp-filter name=\"VEVENT\"><c:time-range "
                <> range
                <> "/></c:comp-filter></c:comp-filter></c:filter></c:calendar-query>"
    response <- http (Request "REPORT" url auth (Just "1") (Just body))
    pure $ case response of
        Left err -> Left err
        Right (status, bytes)
            | status == 207, Just doc <- parseXMLDoc (T.unpack (decode bytes)) -> Right (map text (named "calendar-data" doc))
            | status == 207 -> Left "unreadable answer from the server"
            | otherwise -> Left (statusError status)

-- | Every element with this local name, whatever its namespace prefix.
named :: String -> Element -> [Element]
named name = filterElementsName ((== name) . qName)

text :: Element -> Text
text = T.strip . T.pack . strContent

-- | A link from an answer, made absolute against the URL that was asked.
absolute :: Text -> Text -> Text
absolute base link
    | "http://" `T.isPrefixOf` link || "https://" `T.isPrefixOf` link = link
    | otherwise = origin <> (if "/" `T.isPrefixOf` link then link else "/" <> link)
  where
    (scheme, rest) = T.breakOn "://" base
    origin = scheme <> "://" <> T.takeWhile (/= '/') (T.drop 3 rest)

-- "#rrggbb" or "#rrggbbaa".
color :: Text -> Maybe RGB
color t = case T.unpack t of
    ('#' : r1 : r2 : g1 : g2 : b1 : b2 : _) -> (,,) <$> channel [r1, r2] <*> channel [g1, g2] <*> channel [b1, b2]
    _ -> Nothing
  where
    channel s = case readHex s of
        [(n, "")] -> Just (fromIntegral (n :: Int) / 255)
        _ -> Nothing

statusError :: Int -> Text
statusError status = case status of
    401 -> "wrong user or password (401)"
    403 -> "access denied (403)"
    404 -> "address not found (404)"
    _ -> "the server answered " <> T.pack (show status)

decode :: BS.ByteString -> Text
decode = TE.decodeUtf8With lenientDecode

-- HTTP ----------------------------------------------------------------------

data Request = Request
    { rqMethod :: Text
    , rqUrl :: Text
    , rqAuth :: Maybe (Text, Text)
    , rqDepth :: Maybe Text
    , rqBody :: Maybe Text
    }

{- | Runs curl and returns the HTTP status and the body. Everything, the
password included, goes to curl through its standard input as a config
file, never on its command line, where other users could see it.
-}
http :: Request -> IO (Either Text (Int, BS.ByteString))
http rq = do
    result <- try (run >>= evaluate)
    pure $ case result of
        Left e -> Left ("could not run curl: " <> T.pack (show (e :: IOException)))
        Right (ExitSuccess, out, _)
            | (body, status) <- BC.breakEnd (== '\n') out
            , Just (code, _) <- BC.readInt status ->
                Right (code, BS.dropEnd 1 body)
        Right (ExitSuccess, _, _) -> Left "unreadable answer from curl"
        Right (ExitFailure _, _, err) -> Left (T.strip (T.takeWhile (/= '\n') (decode err)))
  where
    config =
        T.unlines . catMaybes $
            [ Just ("url = " <> quoted (rqUrl rq))
            , Just ("request = " <> quoted (rqMethod rq))
            , (\(user, password) -> "user = " <> quoted (user <> ":" <> password)) <$> rqAuth rq
            , (\depth -> "header = " <> quoted ("Depth: " <> depth)) <$> rqDepth rq
            , (\body -> "data-binary = " <> quoted body) <$> rqBody rq
            , ("header = " <> quoted "Content-Type: application/xml; charset=utf-8") <$ rqBody rq
            , Just "location"
            , Just "max-time = 30"
            , Just "write-out = \"\\n%{http_code}\""
            ]
    quoted t = "\"" <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" t) <> "\""
    run =
        withCreateProcess
            (proc "curl" ["-sS", "-K", "-"]){std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
            $ \stdin stdout stderr' process -> case (stdin, stdout, stderr') of
                (Just hin, Just hout, Just herr) -> do
                    BS.hPut hin (TE.encodeUtf8 config)
                    hClose hin
                    out <- BS.hGetContents hout
                    err <- BS.hGetContents herr
                    code <- waitForProcess process
                    pure (code, out, err)
                _ -> pure (ExitFailure 1, "", "no pipes")

-- From the file to the screen -------------------------------------------------

-- | Turns raw events into the ones to show: repeats expanded, times moved to
-- the system's zone, and only those touching the next 'horizon' days.
resolve :: TZ -> Day -> Maybe RGB -> [RawEvent] -> IO [Event]
resolve local today calendarColor raw = do
    zones <- fmap (M.fromList . catMaybes) . forM (nub [z | Zoned z _ <- map reStart raw]) $ \name -> do
        loaded <- try (loadSystemTZ (T.unpack name))
        pure $ case loaded of
            Right zone -> Just (name, zone)
            -- An unknown zone name: its times are read as local ones.
            Left (_ :: SomeException) -> Nothing
    let toLocal reference wall = case reference of
            Utc _ -> utcToLocalTimeTZ local (localTimeToUTC utc wall)
            Zoned name _ | Just zone <- M.lookup name zones -> utcToLocalTimeTZ local (localTimeToUTCTZ zone wall)
            _ -> wall
        -- Occurrences replaced by another event (one with RECURRENCE-ID).
        replaced = [(reUid e, at) | e <- raw, Just at <- [reRecurrenceId e]]
        allDay e = case reStart e of
            DateOnly _ -> True
            _ -> False
        -- How long it lasts, in wall-clock terms; an all-day event with no end lasts a day.
        lasts e = case (reEnd e, reDuration e) of
            (Just end, _) -> diffLocalTime (wallClock end) (wallClock (reStart e))
            (_, Just d) -> d
            _ | allDay e -> nominalDay
            _ -> 0
        windowStart = LocalTime today midnight
        windowEnd = LocalTime (addDays horizon today) midnight
    pure
        [ Event start end (allDay e) (if T.null (reSummary e) then "(no title)" else reSummary e) calendarColor
        | e <- raw
        , not (reCancelled e)
        , at <- occurrences (addDays (horizon + 1) today) e
        , -- The original of a replaced occurrence is left out; the replacement stays.
        null (reRule e) || (reUid e, at) `notElem` replaced
        , let start = toLocal (reStart e) at
              end = toLocal (reStart e) (addLocalTime (lasts e) at)
        , end > windowStart || (start >= windowStart && end == start)
        , start < windowEnd
        ]

-- Drawing -------------------------------------------------------------------

-- | Lines of text under the title.
maxLines :: Int
maxLines = 8

lineH :: Double
lineH = 22

calendarSize :: (Double, Double)
calendarSize = (300, 50 + lineH * fromIntegral maxLines)

data Row = DayLine String | EventLine Event | Note String | Problem Text

-- | @now@ is the current local time: events that are over are not shown.
renderCalendar :: Theme -> LocalTime -> Agenda -> Render ()
renderCalendar th now agenda = do
    let (w, h) = calendarSize
        pad = 18
        today = localDay now
    card th (Rect 0.5 0.5 (w - 1) (h - 1))
    font th FontWeightNormal 12
    dim th
    moveTo pad 30
    showText (formatTime defaultTimeLocale "%A, %-e %B" today)

    let upcoming = sortOn evStart [e | e <- agEvents agenda, evEnd e > now || (evEnd e == evStart e && evStart e >= now)]
        -- An event that started on an earlier day is listed under today.
        dayOf e = max today (localDay (evStart e))
        label day
            | day == today = "Today"
            | day == addDays 1 today = "Tomorrow"
            | otherwise = formatTime defaultTimeLocale "%a %-e %b" day
        grouped = concat [DayLine (label day) : map EventLine (filter ((== day) . dayOf) upcoming) | day <- nub (map dayOf upcoming)]
        problems = map Problem (take 1 (agErrors agenda))
        content
            | not (agLoaded agenda) = [Note "Loading…"]
            | not (agConfigured agenda) = [Note "No calendars yet", Note "Right-click to add an account"]
            | null upcoming && null problems = [Note "No upcoming events"]
            | otherwise = grouped
        ls = take (maxLines - length problems) content <> problems
    forM_ (zip [0 :: Int ..] ls) $ \(i, l) -> do
        let y = 44 + lineH * fromIntegral i
        case l of
            DayLine s -> do
                font th FontWeightBold 11
                dim th
                moveTo pad (y + 14)
                showText s
            Note s -> do
                dim th
                pangoLine th pad y (w - 2 * pad) 13 False (T.pack s)
            Problem err -> do
                rgba (critColor th) 1
                pangoLine th pad y (w - 2 * pad) 12 False err
            EventLine e -> do
                rgba (fromMaybe (accentColor th) (evColor e)) 1
                newPath
                arc (pad + 4) (y + 8.5) 3.5 0 (2 * pi)
                fill
                font th FontWeightNormal 12
                dim th
                moveTo (pad + 15) (y + 13)
                showText (time e)
                ink th
                pangoLine th (pad + 68) (y + 1) (w - 2 * pad - 68) 13 False (evTitle e)
  where
    time e
        | evAllDay e = "all day"
        | localDay (evStart e) < localDay now = "now"
        | otherwise = formatTime defaultTimeLocale "%H:%M" (evStart e)
