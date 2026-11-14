{-# LANGUAGE OverloadedStrings #-}

-- | Calendar accounts, kept apart from the configuration in
-- @~/.config/hasklets/accounts.json@ (readable only by its owner) because
-- they hold passwords.
module Accounts
    ( Account (..)
    , Provider (..)
    , providerLabel
    , accountLabel
    , accountsPath
    , loadAccounts
    , saveAccounts
    ) where

import Control.Exception (IOException, try)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)

data Provider
    = -- | CalDAV at caldav.icloud.com, with an app-specific password.
      ICloud
    | -- | Google's CalDAV endpoint for the main calendar, with an app password.
      Google
    | -- | Any CalDAV server, by URL.
      CalDav
    | -- | A read-only @.ics@ address; no user or password.
      IcsUrl
    deriving (Eq, Enum, Bounded)

providerId :: Provider -> Text
providerId p = case p of
    ICloud -> "icloud"
    Google -> "google"
    CalDav -> "caldav"
    IcsUrl -> "ics"

providerLabel :: Provider -> Text
providerLabel p = case p of
    ICloud -> "iCloud"
    Google -> "Google"
    CalDav -> "CalDAV server"
    IcsUrl -> "iCal address (.ics)"

data Account = Account
    { acProvider :: Provider
    , acUser :: Text
    , acPassword :: Text
    , acUrl :: Text
    -- ^ Only for 'CalDav' and 'IcsUrl'.
    }
    deriving (Eq)

-- | How an account is shown in lists and error messages; never the password.
accountLabel :: Account -> Text
accountLabel a = providerLabel (acProvider a) <> " · " <> (if T.null (acUser a) then acUrl a else acUser a)

instance FromJSON Account where
    parseJSON = withObject "account" $ \o -> do
        provider <- o .: "provider" >>= parseProvider
        Account provider
            <$> o .:? "user" .!= ""
            <*> o .:? "password" .!= ""
            <*> o .:? "url" .!= ""

parseProvider :: Text -> Parser Provider
parseProvider t =
    maybe (fail ("unknown provider: " <> T.unpack t)) pure $
        find ((== t) . providerId) [minBound .. maxBound]

instance ToJSON Account where
    toJSON a =
        object $
            ["provider" .= providerId (acProvider a)]
                <> ["user" .= acUser a | not (T.null (acUser a))]
                <> ["password" .= acPassword a | not (T.null (acPassword a))]
                <> ["url" .= acUrl a | not (T.null (acUrl a))]

accountsPath :: IO FilePath
accountsPath = (</> "accounts.json") <$> getXdgDirectory XdgConfig "hasklets"

-- | Reads the accounts; no file means no accounts.
loadAccounts :: IO (Either String [Account])
loadAccounts = do
    path <- accountsPath
    exists <- doesFileExist path
    if not exists
        then pure (Right [])
        else do
            contents <- try (BS.readFile path)
            pure $ case contents of
                Left e -> Left (show (e :: IOException))
                Right bytes -> eitherDecodeStrict bytes >>= \o -> maybe (Left "no \"calendars\" list") Right (calendars o)
  where
    calendars :: Object -> Maybe [Account]
    calendars o = case fromJSON (Object o) of
        Success (Wrapper as) -> Just as
        Error _ -> Nothing

newtype Wrapper = Wrapper [Account]

instance FromJSON Wrapper where
    parseJSON = withObject "accounts" $ \o -> Wrapper <$> o .:? "calendars" .!= []

-- | Writes the file so that only its owner can read it, and in one step
-- (temp file + rename).
saveAccounts :: [Account] -> IO ()
saveAccounts accounts = do
    path <- accountsPath
    createDirectoryIfMissing True (takeDirectory path)
    let tmp = path <> ".tmp"
    -- Restrict the file before the passwords go in.
    BS.writeFile tmp ""
    setFileMode tmp (ownerReadMode `unionFileModes` ownerWriteMode)
    BL.writeFile tmp (encode (object ["calendars" .= accounts]) <> "\n")
    renameFile tmp path
