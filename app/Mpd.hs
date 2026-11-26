{-# LANGUAGE OverloadedStrings #-}

{- | Music server widget: what an MPD server (https://www.musicpd.org) is
playing, asked over its own protocol, so no player has to be open here.
Mopidy (https://mopidy.com) speaks the same protocol with its MPD extension,
except for the cover art, which it gives through its web API.
-}
module Mpd
    ( Server (..)
    , Player
    , mpdLoop
    , Order (..)
    , Levels
    , levelsLoop
    , mpdSend
    , mpdSize
    , renderMpd
    , mpdOrderAt
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, finally, throwIO, try)
import Control.Monad (filterM, forM_, forever, unless, void, when, zipWithM_)
import Data.Aeson (Value (..), decodeStrict, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Bits (shiftL, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAlphaNum, toUpper)
import Data.Complex (Complex (..), cis, magnitude)
import Data.Foldable (toList)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import GI.Cairo.Render
import GI.GLib qualified as GLib
import GI.GdkPixbuf qualified as GdkPixbuf
import GI.Gio qualified as Gio
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

import Draw
import Mpris (Action (..), artwork, transport)
import Theme

data Server = Server
    { svHost :: Text
    , svPort :: Int
    , svHttp :: Maybe Int
    -- ^ The port of Mopidy's web API, which is where its covers come from;
    -- 'Nothing' for a real MPD, which sends them itself.
    , svQueue :: Int
    -- ^ Songs of the queue to list after the one playing, at least.
    , svLevels :: Maybe Int
    -- ^ The port of an mpdvu (https://github.com/prietus/mpdvu) next to the
    -- server, which says how loud each channel is; 'Nothing' for no meter.
    , svSpectrum :: Bool
    -- ^ Whether the meter is a spectrum, a bar for each band of frequencies,
    -- instead of the level of each channel. Only where the audio itself
    -- comes (the visualizer feed of mopidy-goodies).
    , svGoodies :: Bool
    -- ^ Whether to use mopidy-goodies (https://github.com/prietus/mopidy-goodies)
    -- where the Mopidy server has it: it knows what really reaches the DAC
    -- and how the album was pressed.
    }
    deriving (Eq, Ord)

data Playback = Playing | Paused | Stopped
    deriving (Eq)

data Player = Player
    { pState :: Playback
    , pTitle :: Text
    -- ^ Empty when the queue has no current song.
    , pArtist :: Text
    , pAlbum :: Text
    , pEdition :: Text
    -- ^ When and by whom it was released, as far as the tags say.
    , pFormat :: Text
    -- ^ What is going to the outputs: "FLAC · 96 kHz · 24 bit · 2890 kbps".
    , pSource :: Text
    -- ^ Where the song comes from when it is not the server's own files:
    -- "TIDAL", "SPOTIFY", "STREAM"... Empty for its own files.
    , pElapsed :: Double
    , pDuration :: Double
    , pPlace :: Maybe (Int, Int)
    -- ^ Which song of the queue this is, and how many there are.
    , pQueue :: [Entry]
    -- ^ What comes after this song.
    , pOutputs :: Text
    , pRandom :: Bool
    , pRepeat :: Bool
    , pVolume :: Maybe Int
    -- ^ From 0 to 100, where the server has a volume to set.
    , pLibrary :: Text
    -- ^ How much music the server has, to say when nothing plays.
    , pArt :: Maybe GdkPixbuf.Pixbuf
    , pBitPerfect :: Maybe Bool
    -- ^ Whether the audio reaches the DAC untouched, where the server can tell.
    }

-- | A song waiting in the queue: its place in it, its title and its length.
data Entry = Entry Int Text Double

-- | What can be asked of the server from the card.
data Order
    = Button Action
    | -- | Jump to this far into the song, from 0 to 1.
      Seek Double
    | Shuffle
    | Repeat
    | -- | Play the song in this row of the list.
      Jump Int
    | -- | Turn the volume up or down by this much.
      Louder Int

-- Protocol ------------------------------------------------------------------

data Conn = Conn Gio.SocketConnection Gio.DataInputStream Gio.OutputStream

noCancel :: Maybe Gio.Cancellable
noCancel = Nothing

-- | Connects and reads the greeting. Reads and writes give up after a while.
open :: Server -> IO Conn
open sv = do
    client <- Gio.socketClientNew
    Gio.socketClientSetTimeout client 8
    conn <- Gio.socketClientConnectToHost client (svHost sv) (fromIntegral (svPort sv)) noCancel
    input <- Gio.iOStreamGetInputStream conn >>= Gio.dataInputStreamNew
    output <- Gio.iOStreamGetOutputStream conn
    let c = Conn conn input output
    greeting <- line c
    unless ("OK MPD" `BS.isPrefixOf` greeting) (throwIO (userError "not an MPD server"))
    pure c

close :: Conn -> IO ()
close (Conn conn _ _) = void (try (Gio.iOStreamClose conn noCancel) :: IO (Either SomeException ()))

line :: Conn -> IO ByteString
line (Conn _ input _) = do
    (text, _) <- Gio.dataInputStreamReadLine input noCancel
    maybe (throwIO (userError "the server closed the connection")) pure text

say :: Conn -> ByteString -> IO ()
say (Conn _ _ output) command = void (Gio.outputStreamWriteAll output (command <> "\n") noCancel)

-- | Sends a command and returns the fields of its answer, or nothing when
-- the server refuses it.
ask :: Conn -> ByteString -> IO (Maybe [(Text, Text)])
ask c command = say c command >> go []
  where
    go fields = do
        l <- line c
        if l == "OK"
            then pure (Just (reverse fields))
            else
                if "ACK" `BS.isPrefixOf` l
                    then pure Nothing
                    else go (field l : fields)

field :: ByteString -> (Text, Text)
field l = (T.toLower (decodeUtf8Lenient key), decodeUtf8Lenient (BS.drop 2 rest))
  where
    (key, rest) = BS.breakSubstring ": " l

-- | An argument in quotes, as the protocol wants it.
quoted :: Text -> ByteString
quoted t = "\"" <> encodeUtf8 (T.concatMap escape t) <> "\""
  where
    escape ch = if ch == '"' || ch == '\\' then T.pack ['\\', ch] else T.singleton ch

{- | A picture of a song, which the server sends in pieces: embedded in the
file, or else the cover in its folder. Nothing over 20 MB.
-}
picture :: Conn -> Text -> IO (Maybe ByteString)
picture c file = do
    embedded <- fetch "readpicture"
    maybe (fetch "albumart") (pure . Just) embedded
  where
    fetch command = go BS.empty
      where
        go sofar = do
            say c (command <> " " <> quoted file <> " " <> BC.pack (show (BS.length sofar)))
            piece <- chunk Nothing
            case piece of
                Just (size, bytes)
                    | BS.null bytes || size > 20000000 -> pure Nothing
                    | BS.length sofar + BS.length bytes >= size -> pure (Just (sofar <> bytes))
                    | otherwise -> go (sofar <> bytes)
                Nothing -> pure Nothing
    -- The answer to one request: the size of the whole picture and a piece of it.
    chunk size = do
        l <- line c
        let (key, value) = field l
            number = readMaybe (T.unpack value) :: Maybe Int
        case () of
            _
                | l == "OK" || "ACK" `BS.isPrefixOf` l -> pure Nothing
                | key == "size" -> chunk number
                | key == "binary", Just n <- number -> do
                    bytes <- exactly n
                    -- After the bytes come a line break and the OK.
                    _ <- line c
                    _ <- line c
                    pure ((,bytes) <$> size)
                | otherwise -> chunk size
    exactly = readExactly c

-- | This many bytes from the server, however many pieces they come in.
readExactly :: Conn -> Int -> IO ByteString
readExactly (Conn _ input _) n = go n []
  where
    go 0 parts = pure (BS.concat (reverse parts))
    go left parts = do
        got <- Gio.inputStreamReadBytes input (fromIntegral left) noCancel >>= GLib.bytesGetData
        case got of
            Just bytes | not (BS.null bytes) -> go (left - BS.length bytes) (bytes : parts)
            _ -> throwIO (userError "the server closed the connection")

-- Data ----------------------------------------------------------------------

{- | Asks the server once a second while it plays, and less often when it
does not, and hands @publish@ what it says or why it does not answer. The
connection is kept open; when it breaks, it is made again after a while.
-}
mpdLoop :: Server -> (Either String Player -> IO ()) -> IO ()
mpdLoop sv publish = forever $ do
    result <- try (bracket (open sv) close session)
    case result of
        Left (_ :: SomeException) -> publish (Left ("No answer from " <> T.unpack (svHost sv)))
        Right () -> pure ()
    threadDelay 10000000
  where
    session c = do
        -- Larger pieces for the pictures, where the server knows how.
        _ <- ask c "binarylimit 262144"
        extras <- goodiesFeatures sv
        go c extras (0 :: Int) Nothing noPath ("", "")
    -- cache: the file of the last cover and its image. The rest is what
    -- mopidy-goodies said last: the path of the signal, asked every few
    -- turns, and the pressing of the album, asked once for each album.
    go c extras turn cache path pressing = do
        info <- fromMaybe [] <$> ask c "status"
        song <- fromMaybe [] <$> ask c "currentsong"
        -- The songs after this one, or from the start when none is current.
        let from = maybe 0 (+ 1) (lookup "song" info >>= readMaybe . T.unpack) :: Int
            range = BC.pack (show from <> ":" <> show (from + 12))
        queue <- maybe [] entries <$> ask c ("playlistinfo " <> range)
        outputs <- fromMaybe [] <$> ask c "outputs"
        library <- if null song then maybe "" libraryText <$> ask c "stats" else pure ""
        let file = fromMaybe "" (lookup "file" song)
        cache' <- case cache of
            Just (old, _) | old == file -> pure cache
            _ | T.null file -> pure Nothing
            _ -> Just . (file,) <$> cover sv c file
        path' <-
            if "audio" `elem` extras && turn `mod` 3 == 0
                then signalPath sv
                else pure path
        let album = fromMaybe "" (lookup "x-albumuri" song)
        pressing' <-
            if "local_metadata" `elem` extras && "local:album:" `T.isPrefixOf` album && album /= fst pressing
                then (album,) <$> albumPressing sv album
                else pure pressing
        let base = playerFrom info song
            Signal sound dac verdict = path'
            player =
                base
                    { pQueue = queue
                    , pOutputs = if T.null dac then enabled outputs else dac
                    , pLibrary = library
                    , pArt = cache' >>= snd
                    , -- What the DAC is getting says more than the tags of the file.
                      pFormat = if T.null sound then pFormat base else T.intercalate " · " (filter (not . T.null) [T.takeWhile (/= ' ') (pFormat base), sound])
                    , pEdition = if T.null (pEdition base) && fst pressing' == album then snd pressing' else pEdition base
                    , pBitPerfect = verdict
                    }
        publish (Right player)
        threadDelay (if pState player == Playing then 1000000 else 3000000)
        go c extras (turn + 1) cache' path' pressing'

-- mopidy-goodies ------------------------------------------------------------

-- | What reaches the DAC right now: its format, the name of the card and
-- whether the signal gets there untouched.
data Signal = Signal Text Text (Maybe Bool)

noPath :: Signal
noPath = Signal "" "" Nothing

-- | An answer of the web API of a Mopidy server, as JSON, or nothing.
goodies :: Server -> String -> [String] -> IO (Maybe Value)
goodies sv route query = case svHttp sv of
    Just port | svGoodies sv -> do
        let url = "http://" <> T.unpack (svHost sv) <> ":" <> show port <> "/goodies/" <> route
        answer <- try (readProcessWithExitCode "curl" (["-fsS", "--max-time", "6", "-G"] <> concatMap (\q -> ["--data-urlencode", q]) query <> [url]) "")
        pure $ case answer of
            Right (ExitSuccess, out, _) -> decodeStrict (encodeUtf8 (T.pack out))
            Right _ -> Nothing
            Left (_ :: SomeException) -> Nothing
    _ -> pure Nothing

-- | A value inside nested objects.
inside :: [Text] -> Value -> Maybe Value
inside [] v = Just v
inside (key : rest) (Object o) = KM.lookup (Key.fromText key) o >>= inside rest
inside _ _ = Nothing

-- | The features a server says it has, or none when it has no mopidy-goodies
-- or it is not to be used.
goodiesFeatures :: Server -> IO [Text]
goodiesFeatures sv = do
    health <- goodies sv "_health" []
    pure $ case health >>= inside ["features"] of
        Just (Object features) -> [Key.toText key | (key, Bool True) <- KM.toList features]
        _ -> []

signalPath :: Server -> IO Signal
signalPath sv = do
    answer <- goodies sv "audio/active" []
    pure $ case answer of
        Nothing -> noPath
        Just v ->
            let text keys = case inside keys v of
                    Just (String t) -> t
                    _ -> ""
                number keys = case inside keys v of
                    Just (Number n) -> Just (realToFrac n :: Double)
                    _ -> Nothing
                khz hz = let k = hz / 1000 in if k == fromIntegral (round k :: Int) then show (round k :: Int) else show k
                sound = case (number ["format", "rate"], number ["format", "bits"]) of
                    (Just rate, Just bits)
                        | "DSD" `T.isPrefixOf` text ["format", "alsa_format"] -> T.pack ("DSD · " <> khz rate <> " kHz")
                        | otherwise -> T.pack (khz rate <> " kHz · " <> show (round bits :: Int) <> " bit")
                    _ -> ""
                dac = if T.null (text ["output", "card", "name"]) then text ["output", "device"] else text ["output", "card", "name"]
                verdict = case text ["chain", "verdict"] of
                    "bit-perfect" -> Just True
                    "not-bit-perfect" -> Just False
                    _ -> Nothing
             in Signal sound dac verdict

-- | "Sony · SICP-1704 · JP": the label, the catalogue number and the
-- country of an album of the server's own files, from their tags.
albumPressing :: Server -> Text -> IO Text
albumPressing sv album = do
    answer <- goodies sv "local/album" ["uri=" <> T.unpack album]
    pure $ case answer of
        Nothing -> ""
        Just v -> T.intercalate " · " [t | key <- ["label", "catalog_number", "country"], Just (String t) <- [inside ["edition", key] v], not (T.null t)]

-- | The names of the outputs that are switched on.
enabled :: [(Text, Text)] -> Text
enabled fields = T.intercalate ", " [name | (name, True) <- pair fields]
  where
    pair (("outputname", name) : rest) = (name, ("outputenabled", "1") `elem` takeWhile ((/= "outputid") . fst) rest) : pair rest
    pair (_ : rest) = pair rest
    pair [] = []

-- | The songs in an answer that lists several, one after another.
entries :: [(Text, Text)] -> [Entry]
entries fields = case break ((== "file") . fst) (drop 1 fields) of
    _ | null fields -> []
    (rest, more) ->
        let song = take 1 fields <> rest
            place = fromMaybe 0 (lookup "pos" song >>= readMaybe . T.unpack)
            long = songLength song
         in Entry place (title song) long : entries more

-- | How long a song is, in seconds: "duration", or the whole seconds of
-- "time" where that is all there is (Mopidy), or 0 when it does not say.
songLength :: [(Text, Text)] -> Double
songLength song = case [n | key <- ["duration", "time"], Just n <- [lookup key song >>= readMaybe . T.unpack], n > 0] of
    long : _ -> long
    [] -> 0

-- | "1,204 albums · 15,870 songs", or nothing when the server does not count them.
libraryText :: [(Text, Text)] -> Text
libraryText stats = T.intercalate " · " [T.pack (show n) <> " " <> what | (key, what) <- kinds, Just n <- [count key], n > 0]
  where
    kinds = [("albums", "albums"), ("songs", "songs")]
    count key = lookup key stats >>= readMaybe . T.unpack :: Maybe Int

-- | What a song is called: its title, or else the name of its file.
title :: [(Text, Text)] -> Text
title song = case lookup "title" song of
    Just t | not (T.null t) -> t
    _ -> maybe "" (T.takeWhileEnd (/= '/')) (lookup "file" song)

playerFrom :: [(Text, Text)] -> [(Text, Text)] -> Player
playerFrom info song =
    Player
        { pState = case get info "state" of
            "play" -> Playing
            "pause" -> Paused
            _ -> Stopped
        , pTitle = title song
        , pArtist = get song "artist"
        , pAlbum = get song "album"
        , pEdition = edition
        , pFormat = T.intercalate " · " (filter (not . T.null) [codec, sound, bitrate])
        , pSource = source
        , pElapsed = number (get info "elapsed")
        , -- The server says how long the song is, or the song itself does, or
          -- (Mopidy) only the old "time: elapsed:total" of the server does.
          pDuration = case filter (> 0) [number (get info "duration"), songLength song, number (T.takeWhileEnd (/= ':') (get info "time"))] of
            long : _ -> long
            [] -> 0
        , pPlace = (,) . (+ 1) <$> whole (get info "song") <*> whole (get info "playlistlength")
        , pQueue = []
        , pOutputs = ""
        , pRandom = get info "random" == "1"
        , pRepeat = get info "repeat" == "1"
        , pVolume = find (>= 0) (whole (get info "volume"))
        , pLibrary = ""
        , pArt = Nothing
        , pBitPerfect = Nothing
        }
  where
    get fields key = fromMaybe "" (lookup key fields)
    number = fromMaybe 0 . readMaybe . T.unpack
    whole = readMaybe . T.unpack :: Text -> Maybe Int
    year = T.take 4
    -- "1976 · Island · 2011 edition": the year it came out first, the
    -- label, and the year of this edition when it is another one.
    edition =
        let first = year (get song "originaldate")
            this = year (get song "date")
         in T.intercalate " · " . filter (not . T.null) $
                [ if T.null first then this else first
                , get song "label"
                , if T.null first || T.null this || first == this then "" else this <> " edition"
                ]
    -- Mopidy names its songs with the service they come from in front:
    -- "tidal:track:123", "local:track:...". A real MPD has paths, or the
    -- address of a stream.
    source = case T.breakOn ":" (get song "file") of
        (scheme, rest)
            | T.null rest || T.any (== '/') scheme || T.null scheme -> ""
            | scheme `elem` ["local", "file", "m3u"] -> ""
            | scheme `elem` ["http", "https", "rtmp", "rtmps", "rtsp", "mms"] -> "STREAM"
            | otherwise -> T.toUpper scheme
    -- The kind of file, from its name, when it looks like one.
    codec =
        let ext = T.takeWhileEnd (/= '.') (get song "file")
         in if T.length ext `elem` [2 .. 4] && T.all isAlphaNum ext && T.any (== '.') (get song "file")
                then T.map toUpper ext
                else ""
    -- "44100:16:2" or "dsd64:2".
    sound = case T.splitOn ":" (get info "audio") of
        [rate, bits, _]
            | Just hz <- (readMaybe (T.unpack rate) :: Maybe Double) ->
                T.pack (trim (hz / 1000) <> " kHz") <> " · " <> depth bits
        [rate, _] | "dsd" `T.isPrefixOf` rate -> T.toUpper rate
        _ -> ""
    depth bits = case bits of
        "f" -> "float"
        "dsd" -> "DSD"
        _ -> bits <> " bit"
    trim v = if v == fromIntegral (round v :: Int) then show (round v :: Int) else show v
    bitrate = case whole (get info "bitrate") of
        Just kbps | kbps > 0 -> T.pack (show kbps) <> " kbps"
        _ -> ""

-- | The cover of a song, kept in a file of the cache to be read as an image.
cover :: Server -> Conn -> Text -> IO (Maybe GdkPixbuf.Pixbuf)
cover sv c file = do
    dir <- getXdgDirectory XdgCache "hasklets"
    createDirectoryIfMissing True dir
    let path = dir </> ("art-" <> filter isAlphaNum (T.unpack (svHost sv)) <> "-" <> show (svPort sv))
    found <- case svHttp sv of
        Nothing -> do
            bytes <- picture c file
            forM_ bytes (BS.writeFile path)
            pure (bytes /= Nothing)
        Just port -> mopidyCover sv port file path
    if found
        then fromMaybe Nothing <$> (try (GdkPixbuf.pixbufNewFromFileAtScale path 256 256 True) >>= pure . either ignore Just)
        else pure Nothing
  where
    ignore :: SomeException -> Maybe a
    ignore _ = Nothing

-- | Asks Mopidy's web API where the images of a track are and downloads the
-- first one. Whether it worked.
mopidyCover :: Server -> Int -> Text -> FilePath -> IO Bool
mopidyCover sv port uri path = do
    let base = "http://" <> T.unpack (svHost sv) <> ":" <> show port
        request =
            object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "id" .= (1 :: Int)
                , "method" .= ("core.library.get_images" :: Text)
                , "params" .= object ["uris" .= [uri]]
                ]
    answer <-
        try $
            readProcessWithExitCode
                "curl"
                ["-fsS", "--max-time", "8", "-H", "Content-Type: application/json", "--data-binary", "@-", base <> "/mopidy/rpc"]
                (BC.unpack (BL.toStrict (encode request)))
    case answer of
        Right (ExitSuccess, out, _)
            | Just image <- firstImage (BC.pack out) -> do
                -- Local covers come as a path of the same server.
                let url = if "/" `T.isPrefixOf` image then T.pack base <> image else image
                got <- try (readProcessWithExitCode "curl" ["-fsSL", "--max-time", "10", "-o", path, T.unpack url] "")
                pure $ case got of
                    Right (ExitSuccess, _, _) -> True
                    Right _ -> False
                    Left (_ :: SomeException) -> False
        Right _ -> pure False
        Left (_ :: SomeException) -> pure False
  where
    -- {"result": {"<uri>": [{"uri": "..."}, ...]}}
    firstImage out = do
        Object answer <- decodeStrict out
        Object result <- KM.lookup "result" answer
        Array images <- listToMaybe (KM.elems result)
        Object image <- listToMaybe (toList images)
        String url <- KM.lookup "uri" image
        Just url

-- | Tells the server to do what was asked on the card. @state@ is what the
-- card was showing, which is what the order refers to.
mpdSend :: Server -> Maybe (Either String Player) -> Order -> IO ()
mpdSend sv state order = forM_ command $ \text ->
    void (try (bracket (open sv) close (`ask` text)) :: IO (Either SomeException (Maybe [(Text, Text)])))
  where
    player = case state of
        Just (Right p) -> Just p
        _ -> Nothing
    switch on = if on then "0" else "1"
    command = case order of
        Button Previous -> Just "previous"
        Button Next -> Just "next"
        Button PlayPause -> Just $ case pState <$> player of
            Just Playing -> "pause 1"
            Just Paused -> "pause 0"
            _ -> "play"
        Seek part -> do
            p <- player
            if pDuration p > 0 then Just ("seekcur " <> BC.pack (show (part * pDuration p))) else Nothing
        Shuffle -> ("random " <>) . switch . pRandom <$> player
        Repeat -> ("repeat " <>) . switch . pRepeat <$> player
        Jump row -> do
            p <- player
            Entry place _ _ <- listToMaybe (drop row (pQueue p))
            Just ("play " <> BC.pack (show place))
        Louder by -> do
            volume <- player >>= pVolume
            Just ("setvol " <> BC.pack (show (max 0 (min 100 (volume + by)))))

-- Levels --------------------------------------------------------------------

-- | How loud each channel is: its peak and its RMS level, from 0 to 1.
type Levels = [(Double, Double)]

{- | Hands @publish@ the levels of what the server plays as they come, many
times a second, with needles that fall little by little instead of jumping
down. An empty list means there is nothing to show.

For an MPD they come from an mpdvu on the server, already worked out. For a
Mopidy they are worked out here, from the audio that the visualizer feed of
mopidy-goodies sends over a WebSocket on the port of its web API.
-}
levelsLoop :: Server -> Int -> (Levels -> IO ()) -> IO ()
levelsLoop sv port publish = forever $ do
    _ <- try (if isJust (svHttp sv) then feed else session) :: IO (Either SomeException ())
    publish []
    threadDelay 5000000
  where
    -- The WebSocket of mopidy-goodies: an HTTP request that is answered
    -- with "101", and from then on messages, each with a short header.
    feed = do
        client <- Gio.socketClientNew
        Gio.socketClientSetTimeout client 8
        conn <- Gio.socketClientConnectToHost client (svHost sv) (fromIntegral port) noCancel
        input <- Gio.iOStreamGetInputStream conn >>= Gio.dataInputStreamNew
        output <- Gio.iOStreamGetOutputStream conn
        let c = Conn conn input output
            shake = do
                say c . BC.pack $
                    "GET /goodies/audio/visualizer HTTP/1.1\r\nHost: "
                        <> T.unpack (svHost sv)
                        <> ":"
                        <> show port
                        <> "\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: aGFza2xldHMtdnVtZXRlcg==\r\nSec-WebSocket-Version: 13\r\n\r"
                answer <- line c
                unless (" 101" `BS.isInfixOf` answer) (throwIO (userError "no visualizer feed"))
                headers
            headers = do
                l <- line c
                unless (BS.null (BC.filter (/= '\r') l)) headers
        (shake >> messages c BS.empty [] []) `finally` close c
    -- pending: audio not metered yet. heard: the last samples, for the spectrum.
    messages c pending shown heard = do
        header <- readExactly c 2
        let kind = BS.index header 0 .&. 0x0f
            short = fromIntegral (BS.index header 1 .&. 0x7f) :: Int
            number = BS.foldl' (\n b -> n `shiftL` 8 .|. fromIntegral b) (0 :: Int)
        size <- case short of
            126 -> number <$> readExactly c 2
            127 -> number <$> readExactly c 8
            n -> pure n
        when (size > 4000000) (throwIO (userError "a message too large"))
        -- A server does not mask what it sends, but the header says.
        _ <- if testBit (BS.index header 1) 7 then readExactly c 4 else pure BS.empty
        payload <- readExactly c size
        case kind of
            -- Audio: signed 16 bits, little endian, two channels at 44.1 kHz.
            2 -> do
                (rest, shown', heard') <- meter (pending <> payload) shown heard
                messages c rest shown' heard'
            8 -> pure ()
            -- A ping is answered with the same bytes (masked with zeros,
            -- which leaves them as they are).
            9 -> do
                let Conn _ _ output = c
                void (Gio.outputStreamWriteAll output (BS.pack [0x8a, 0x80 .|. fromIntegral (min 125 size), 0, 0, 0, 0] <> BS.take 125 payload) noCancel)
                messages c pending shown heard
            _ -> messages c pending shown heard
    -- A level for every 40 ms of audio there is; what is left over waits
    -- for the next message.
    meter audio shown heard
        | BS.length audio < window = pure (audio, shown, heard)
        | otherwise = do
            let piece = BS.take window audio
                -- The spectrum looks at more audio than one step brings.
                heard' = if svSpectrum sv then lastN spectrumWindow (heard <> pcmMono piece) else []
                fresh
                    | not (svSpectrum sv) = pcmLevels piece
                    | length heard' == spectrumWindow = spectrumBands heard'
                    | otherwise = []
                shown' = settle shown fresh
            when (shown' /= shown) (publish shown')
            -- The samples too, so they do not hold on to the audio they are from.
            foldr seq () heard' `seq` meter (BS.copy (BS.drop window audio)) shown' heard'
    lastN n xs = drop (length xs - n) xs
    window = 1764 * 4
    session = do
        resolver <- Gio.resolverGetDefault
        found <- Gio.resolverLookupByName resolver (svHost sv) noCancel
        v4 <- filterM (fmap (== Gio.SocketFamilyIpv4) . Gio.inetAddressGetFamily) found
        inet <- maybe (throwIO (userError "no address")) pure (listToMaybe v4)
        address <- Gio.inetSocketAddressNew inet (fromIntegral port)
        socket <- Gio.socketNew Gio.SocketFamilyIpv4 Gio.SocketTypeDatagram Gio.SocketProtocolUdp
        listen socket address 0 [] `finally` Gio.socketClose socket
    listen socket address asked shown = do
        now <- GLib.getMonotonicTime
        -- The server forgets who does not ask again.
        asked' <-
            if now - asked > 3000000
                then now <$ Gio.socketSendTo socket (Just address) "mpdvu 1 subscribe" noCancel
                else pure asked
        got <- try (Gio.socketReceiveBytes socket 512 500000 noCancel >>= GLib.bytesGetData)
        let shown' = case got of
                Right (Just bytes) | Just fresh <- message bytes -> settle shown fresh
                Right _ -> shown
                -- Nothing came for a while: nothing is playing.
                Left (_ :: SomeException) -> []
        when (shown' /= shown) (publish shown')
        listen socket address asked' shown'
    -- "mpdvu 1 <sequence> <peak> <rms> ..."
    message bytes = case BC.words bytes of
        ("mpdvu" : "1" : _ : numbers) -> Just (pairs (mapMaybe (readMaybe . BC.unpack) numbers))
        _ -> Nothing
    pairs (peak : rms : rest) = (peak, rms) : pairs rest
    pairs _ = []
    -- A level goes up at once and comes down a step at a time. The numbers
    -- are worked out here and now: left for whoever draws them, each would
    -- keep alive the audio and the levels it comes from, and when the card
    -- is covered and nothing draws it they would pile up without end.
    settle shown fresh
        | all (== (0, 0)) fresh = fresh
        | otherwise = worked (zipWith fall (shown <> repeat (0, 0)) fresh)
    fall (peak0, rms0) (peak, rms) = (max peak (peak0 - 0.012), max rms (rms0 * 0.82))
    worked levels = foldr (\(a, b) rest -> a `seq` b `seq` rest) () levels `seq` levels

-- | Peak and RMS of the two channels of a piece of audio: signed 16 bits,
-- little endian, left and right in turn.
pcmLevels :: ByteString -> Levels
pcmLevels audio = [channel 0, channel 2]
  where
    frames = BS.length audio `div` 4
    sample at =
        let v = fromIntegral (BS.index audio at) + 256 * fromIntegral (BS.index audio (at + 1)) :: Int
         in fromIntegral (if v >= 32768 then v - 65536 else v) / 32768 :: Double
    channel offset =
        let values = [sample (4 * i + offset) | i <- [0 .. frames - 1]]
         in (maximum (0 : map abs values), sqrt (sum (map (\v -> v * v) values) / fromIntegral (max 1 frames)))

-- | The two channels of a piece of that audio mixed into one, from -1 to 1.
pcmMono :: ByteString -> [Double]
pcmMono audio = [(sample (4 * i) + sample (4 * i + 2)) / 2 | i <- [0 .. BS.length audio `div` 4 - 1]]
  where
    sample at =
        let v = fromIntegral (BS.index audio at) + 256 * fromIntegral (BS.index audio (at + 1)) :: Int
         in fromIntegral (if v >= 32768 then v - 65536 else v) / 32768

-- | Samples the spectrum is worked out from: a power of two, some 46 ms.
spectrumWindow :: Int
spectrumWindow = 2048

-- | Bars of the spectrum.
spectrumBars :: Int
spectrumBars = 28

{- | How loud each band of frequencies is in that many samples at 44.1 kHz,
from 0 to 1 on a scale of decibels: bands as wide as each other to the ear,
from 45 Hz to 16 kHz. Each comes twice, to be read like a level (the first
is held a little as a peak).
-}
spectrumBands :: [Double] -> Levels
spectrumBands samples = [(v, v) | v <- map band [0 .. spectrumBars - 1]]
  where
    n = spectrumWindow
    -- The ends are faded out, so the cut does not sound like a click.
    faded = [x * (0.5 - 0.5 * cos (2 * pi * fromIntegral i / fromIntegral (n - 1))) :+ 0 | (i, x) <- zip [0 :: Int ..] samples]
    -- How loud a tone at each frequency is, from 0 to 1.
    loud = map ((* (4 / fromIntegral n)) . magnitude) (take (n `div` 2) (fft faded))
    hz = 44100 / fromIntegral n :: Double
    edge j = 45 * (16000 / 45) ** (fromIntegral j / fromIntegral spectrumBars) :: Double
    band j =
        let lo = max 1 (floor (edge j / hz)) :: Int
            hi = max (lo + 1) (floor (edge (j + 1) / hz))
            top = maximum (1e-7 : take (hi - lo) (drop lo loud))
         in -- From -54 dB at the bottom to -6 dB at the top.
            max 0 (min 1 ((20 * logBase 10 top + 54) / 48))

-- | The frequencies in a number of samples that is a power of two.
fft :: [Complex Double] -> [Complex Double]
fft [x] = [x]
fft xs = zipWith (+) evens turned <> zipWith (-) evens turned
  where
    n = length xs
    (evens, odds) = (fft (every xs), fft (every (drop 1 xs)))
    every (a : rest) = a : every (drop 1 rest)
    every [] = []
    turned = [cis (-2 * pi * fromIntegral k / fromIntegral n) * o | (k, o) <- zip [0 :: Int ..] odds]

-- Drawing -------------------------------------------------------------------

-- | The height on the grid that holds the songs of the queue asked for: a
-- square with one.
mpdSize :: Double -> Server -> (Double, Double)
mpdSize gap sv = (cardWidth, fitHeight gap (queueY sv + rowH * fromIntegral (max 1 (svQueue sv)) + 14))

-- | The songs of the queue that fit in that height: the ones asked for, or a
-- few more.
queueRows :: Double -> Server -> Int
queueRows gap sv = max 1 (floor ((snd (mpdSize gap sv) - queueY sv - 14) / rowH + 0.001))

pad, progressY, metersY, rowH :: Double
pad = 18
progressY = 182
-- Where the meters go when there are any, under the times of the song.
metersY = 210
-- How tall each row of the list of the queue is.
rowH = 22

-- Where the buttons are and where the list of the queue starts: lower down
-- when the meters take their place.
controlsY, queueY :: Server -> Double
controlsY sv = 224 + metersRoom sv
queueY sv = 258 + metersRoom sv

metersRoom :: Server -> Double
metersRoom sv
    | isJust (svLevels sv) = metersTall sv + 10
    | otherwise = 0

-- | How tall the meters are: the spectrum needs more room for its bars.
metersTall :: Server -> Double
metersTall sv = if svSpectrum sv then 36 else 20

buttons :: [(Action, Double)]
buttons = [(Previous, cardWidth / 2 - 46), (PlayPause, cardWidth / 2), (Next, cardWidth / 2 + 46)]

-- The two switches, at the ends of the line of the buttons.
shuffleX, repeatX :: Double
shuffleX = pad + 11
repeatX = cardWidth - pad - 11

-- | What is under the pointer, if it is something to click: a button, a
-- switch, a point of the progress bar or a song of the queue.
mpdOrderAt :: Double -> Server -> Double -> Double -> Maybe Order
mpdOrderAt gap sv x y
    | abs (y - controlsY sv) <= 16, Just (action, _) <- find (\(_, bx) -> abs (x - bx) <= 18) buttons = Just (Button action)
    | abs (y - controlsY sv) <= 14 && abs (x - shuffleX) <= 14 = Just Shuffle
    | abs (y - controlsY sv) <= 14 && abs (x - repeatX) <= 14 = Just Repeat
    | abs (y - progressY) <= 9 && x >= pad && x <= cardWidth - pad = Just (Seek (max 0 (min 1 ((x - pad - 2) / (cardWidth - 2 * pad - 4)))))
    | y >= queueY sv && row < queueRows gap sv && x >= pad && x <= cardWidth - pad = Just (Jump row)
    | otherwise = Nothing
  where
    row = floor ((y - queueY sv) / rowH) :: Int

-- | @name@ is what the card is called: the kind of server it asks.
renderMpd :: Theme -> Double -> Server -> String -> Levels -> Maybe (Either String Player) -> Render ()
renderMpd th gap sv name levels state = do
    let (w, h) = mpdSize gap sv
        wide = w - 2 * pad
        note s = do
            font th FontWeightNormal 12
            dim th
            centered (w / 2) (h / 2 + 10) s
    card th (Rect 0.5 0.5 (w - 1) (h - 1))
    font th FontWeightNormal 12
    dim th
    moveTo pad 30
    showText name
    case state of
        Nothing -> note "Loading…"
        Just (Left problem) -> note problem
        Just (Right p) -> do
            -- Where the sound is going, and how loud; and at the very end,
            -- whether it gets there untouched, where the server can tell.
            nameExt <- textExtents name
            let (verdict, color) = case pBitPerfect p of
                    Just True -> ("bit-perfect", okColor th)
                    Just False -> ("not bit-perfect", warnColor th)
                    Nothing -> ("", okColor th)
            font th FontWeightNormal 11
            verdictExt <- textExtents verdict
            let taken = if null verdict then 0 else textExtentsXadvance verdictExt + 16
            unless (null verdict) $ do
                rgba color 1
                rightAligned (w - pad) 30 verdict
                newPath
                arc (w - pad - textExtentsXadvance verdictExt - 7) 26 2.8 0 (2 * pi)
                fill
            dim th
            let outW = wide - textExtentsXadvance nameExt - 14 - taken
                loud = maybe "" (\v -> T.pack (show v) <> " % · ") (pVolume p)
            rightText th (w - pad - taken) 17 outW 11 (loud <> pOutputs p)

            let art = Rect pad 44 92 92
                left = pad + 92 + 14
                textW = w - pad - left
            artwork th art (pArt p)
            if T.null (pTitle p)
                then do
                    dim th
                    pangoLine th left 72 textW 13 False "Nothing playing"
                    pangoLine th left 93 textW 11 False (pLibrary p)
                else do
                    ink th
                    pangoLine th left 46 textW 14 True (pTitle p)
                    pangoLine th left 67 textW 12.5 False (pArtist p)
                    dim th
                    pangoLine th left 86 textW 12 False (pAlbum p)
                    pangoLine th left 107 textW 11 False (pEdition p)

            -- What it sounds like, and where in the queue it is.
            let place = maybe "" (\(i, n) -> show i <> " / " <> show n) (pPlace p)
            font th FontWeightNormal 11
            dim th
            rightAligned (w - pad) 162 place
            placeExt <- textExtents place
            -- In front, a badge with the service the song comes from.
            badge <-
                if T.null (pSource p)
                    then pure 0
                    else do
                        let label = T.unpack (pSource p)
                        font th FontWeightBold 9
                        labelExt <- textExtents label
                        let badgeW = textExtentsXadvance labelExt + 12
                        roundRect (Rect pad 149.5 badgeW 15) 7.5
                        rgba (accentColor th) 1
                        fill
                        -- The letters are cut out of it, in the color of the card.
                        setColor (fst (cardFill th), 1)
                        moveTo (pad + 6) 160.3
                        showText label
                        pure (badgeW + 7)
            rgba (accentColor th) 1
            pangoLine th (pad + badge) 150 (wide - badge - textExtentsXadvance placeExt - 12) 11.5 True (pFormat p)

            -- How far into the song.
            let frac = if pDuration p > 0 then max 0 (min 1 (pElapsed p / pDuration p)) else 0
            setLineWidth 3
            setLineCap LineCapRound
            track th
            newPath >> moveTo (pad + 2) progressY >> lineTo (w - pad - 2) progressY >> stroke
            when (pDuration p > 0) $ do
                ink th
                newPath >> moveTo (pad + 2) progressY >> lineTo (pad + 2 + (wide - 4) * frac) progressY >> stroke
                font th FontWeightNormal 10.5
                dim th
                moveTo pad 199
                showText (clock (pElapsed p))
                rightAligned (w - pad) 199 (clock (pDuration p))

            when (isJust (svLevels sv)) $
                (if svSpectrum sv then spectrum else meters) th (Rect pad metersY wide (metersTall sv)) levels

            ink th
            transport (controlsY sv) (pState p == Playing) buttons
            -- The switches are lit when they are on.
            let lit on = if on then rgba (accentColor th) 1 else dim th
            lit (pRandom p) >> shuffleSign shuffleX (controlsY sv)
            lit (pRepeat p) >> repeatSign repeatX (controlsY sv)

            -- What comes next: as many songs as fit.
            divider th (pad, queueY sv - 8) (w - pad, queueY sv - 8)
            let coming = take (queueRows gap sv) (pQueue p)
            when (null coming) $ do
                font th FontWeightNormal 11
                dim th
                moveTo pad (queueY sv + 15)
                showText ("Nothing after this" :: String)
            zipWithM_
                ( \i (Entry _ name' long) -> do
                    let y = queueY sv + rowH * fromIntegral (i :: Int)
                    font th FontWeightNormal 10.5
                    dim th
                    rightAligned (w - pad) (y + 14) (if long > 0 then clock long else "")
                    -- The very next one stands out from the rest.
                    if i == 0 then ink th else dim th
                    pangoLine th pad (y + 2) (wide - 44) 12 False name'
                )
                [0 ..]
                coming
  where
    -- "4:56"
    clock :: Double -> String
    clock s = show (total `div` 60) <> ":" <> pad2 (total `mod` 60)
      where
        total = floor s :: Int
        pad2 n = (if n < 10 then "0" else "") <> show n

{- | A meter per channel (two at most), one above the other: a row of
segments lit up to the RMS level, on a scale of decibels, and one more at the
peak. Green, then yellow near the top and red at the very end.
-}
meters :: Theme -> Rect -> Levels -> Render ()
meters th (Rect x y w h) levels = forM_ (zip [0 :: Int ..] channels) $ \(row, (label, (peak, rms))) -> do
    let top = y + (h / 2 + 1) * fromIntegral row
        tall = h / 2 - 2
        left = x + 12
        slot = (w - 12) / fromIntegral segments
        at v = position v * fromIntegral segments
    font th FontWeightNormal 8.5
    dim th
    moveTo x (top + tall - 0.5)
    showText (label :: String)
    forM_ [0 .. segments - 1] $ \i -> do
        let here = fromIntegral i :: Double
            on = here < at rms || (peak > 0 && floor (at peak) == i && here >= at rms)
            color
                | here >= fromIntegral segments * position 0.5 = critColor th
                | here >= fromIntegral segments * position 0.2 = warnColor th
                | otherwise = okColor th
        if on then rgba color 1 else track th
        rectangle (left + slot * here) top (slot - 1.5) tall
        fill
  where
    segments = 36 :: Int
    channels = zip ["L", "R"] (take 2 (levels <> repeat (0, 0)))
    -- From 0 to 1 along the meter: -42 dB at the left end, 0 dB at the right.
    position v = max 0 (min 1 (1 + 20 * logBase 10 (max 1e-6 v) / 42))

-- | The spectrum: a bar for each band, low notes on the left, with a mark
-- where each one was a moment ago.
spectrum :: Theme -> Rect -> Levels -> Render ()
spectrum th (Rect x y w h) levels = forM_ (zip [0 :: Int ..] bands) $ \(i, (held, now)) -> do
    let slot = w / fromIntegral spectrumBars
        left = x + slot * fromIntegral i
        tall v = max 1.5 (h * v)
    rgba (accentColor th) 1
    rectangle left (y + h - tall now) (slot - 1.5) (tall now)
    fill
    when (held > now + 0.03) $ do
        dim th
        rectangle left (y + h - tall held) (slot - 1.5) 1.2
        fill
  where
    bands = take spectrumBars (levels <> repeat (0, 0))

-- | Two arrows that cross, centered on a point, in the current color.
shuffleSign :: Double -> Double -> Render ()
shuffleSign x y = do
    setLineWidth 1.6
    setLineCap LineCapRound
    setLineJoin LineJoinRound
    forM_ [1, -1] $ \dir -> do
        newPath
        moveTo (x - 8) (y + 4.5 * dir)
        curveTo (x - 1) (y + 4.5 * dir) (x - 1) (y - 4.5 * dir) (x + 6) (y - 4.5 * dir)
        stroke
        newPath
        moveTo (x + 4) (y - 4.5 * dir - 2.6)
        lineTo (x + 7.4) (y - 4.5 * dir)
        lineTo (x + 4) (y - 4.5 * dir + 2.6)
        stroke

-- | Two arrows that go round, centered on a point, in the current color.
repeatSign :: Double -> Double -> Render ()
repeatSign x y = do
    setLineWidth 1.6
    setLineCap LineCapRound
    setLineJoin LineJoinRound
    forM_ [1, -1] $ \dir -> do
        -- Half of the loop, and its arrow head at the end.
        newPath
        moveTo (x - 7 * dir) (y + 1 * dir)
        lineTo (x - 7 * dir) (y - 2 * dir)
        curveTo (x - 7 * dir) (y - 5 * dir) (x - 5 * dir) (y - 5 * dir) (x - 4 * dir) (y - 5 * dir)
        lineTo (x + 6 * dir) (y - 5 * dir)
        stroke
        newPath
        moveTo (x + 3.6 * dir) (y - 5 * dir - 2.6)
        lineTo (x + 7 * dir) (y - 5 * dir)
        lineTo (x + 3.6 * dir) (y - 5 * dir + 2.6)
        stroke

-- | One line of text ending at @xr@, cut short if it is wider than @width@.
rightText :: Theme -> Double -> Double -> Double -> Double -> Text -> Render ()
rightText th xr y width size txt = do
    font th FontWeightNormal size
    let fits t = (<= width) . textExtentsXadvance <$> textExtents (T.unpack t)
        shorten t = do
            ok <- fits t
            if ok || T.length t <= 4 then pure t else shorten (T.dropEnd 2 (T.dropWhileEnd (== '…') t) <> "…")
    shown <- shorten txt
    rightAligned xr (y + size + 2) (T.unpack shown)
