{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Talks.Free.Ugly (
  lookupPassword
, mkPassword
, storePassword
, getSetting
, setSetting
, clearSetting
) where

import Control.Applicative

import Talks.Free.Prelude

import Data.IORef
import Data.Text hiding (any)
import Data.Text.Lazy (fromChunks)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Foldable (elem)
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Map as Map

import System.IO.Unsafe

import Crypto.PasswordStore

--
-- (Hopefully) the worst haskell you have seen this week.
--
-- Effectively we have taken _name your favourite java library_ and
-- transmogrified it into a lol variety of haskell.
--
-- Transmogrify: Transform, esp. in a surprising or magical manner.
--

lookupPassword :: Text -> IO (Maybe Text)
lookupPassword key =
  withLog
    ("looking up password: " <> key)
    (\v -> "got password: " <> prnt v)
    $ lookupPassword' key

mkPassword :: IO Text
mkPassword =
  withLog
    "making a password"
    (\v -> "made password " <> v)
    mkPassword'

storePassword :: Text -> Text -> IO ()
storePassword key value =
  withLog
    ("store password " <> key <> " as " <> value)
    (const $ "stored password " <> key )
    $ storePassword' key value

getSetting :: Text -> IO (Maybe Text)
getSetting key =
  withLog
    ("get setting " <> key)
    (\v -> "got setting " <> prnt v)
    $ getSetting' key

setSetting :: Text -> Text -> IO ()
setSetting key value =
  withLog
    ("set setting " <> key <> " as " <> value)
    (const $ "done set setting " <> key <> " to " <> value)
    $ setSetting' key value

clearSetting :: Text -> IO ()
clearSetting key =
  withLog
    ("clearing setting " <> key)
    (const $ "cleared setting " <> key)
    $ clearSetting' key

--------------------------------------------------------------------------------

lookupPassword' :: Text -> IO (Maybe Text)
lookupPassword' key =
  Map.lookup key <$> getPasswords

mkPassword' :: IO Text
mkPassword' =
  do value <- fmap (md5sum . decodeUtf8 . exportSalt) genSaltIO
     maybe value (`take` value) <$> getPasswordLength

storePassword' :: Text -> Text -> IO ()
storePassword' key value =
  (Map.insert key value <$> getPasswords) >>= setPasswords

getSetting' :: Text -> IO (Maybe Text)
getSetting' key =
  Map.lookup key <$> getSettings

setSetting' :: Text -> Text -> IO ()
setSetting' key value =
  (Map.insert key value <$> getSettings) >>= setSettings

clearSetting' :: Text -> IO ()
clearSetting' key =
  (Map.delete key <$> getSettings) >>= setSettings

--------------------------------------------------------------------------------

data PasswordStore =
  PasswordStore {
      settings :: Map Text Text
    , passwords :: Map Text Text
    }

getStore :: IO PasswordStore
getStore =
  readIORef mmmmmmmmmGlobalsAreCool

setStore :: PasswordStore -> IO ()
setStore =
  writeIORef mmmmmmmmmGlobalsAreCool

getPasswords :: IO (Map Text Text)
getPasswords =
  passwords <$> getStore

setPasswords :: Map Text Text -> IO ()
setPasswords ps =
  (\x -> x { passwords = ps }) <$> getStore >>= setStore

getSettings :: IO (Map Text Text)
getSettings =
  settings <$> getStore

setSettings :: Map Text Text -> IO ()
setSettings ss =
  (\x -> x { settings = ss }) <$> getStore >>= setStore

getPasswordLength :: IO (Maybe Int)
getPasswordLength =
  (\s -> read . unpack <$> s) <$> getSetting "password.length"

md5sum :: Text -> Text
md5sum =
  pack . show . md5 . encodeUtf8 . fromChunks . return


prnt :: Show a => a -> Text
prnt =
  pack . show

--------------------------------------------------------------------------------

log :: Text -> IO ()
log message =
  do level <- getSetting' "log.level"
     let noisy = "noisy" `elem` level
     when noisy (putStrLn . unpack $ message)
     when noisy (putStrLn "")


withLog :: Text -> (a -> Text) -> IO a -> IO a
withLog before after v =
  do log before
     value <- v
     log $ after value
     return value

--------------------------------------------------------------------------------

-- This really isn't cool, but not so obvious code that is about
-- the same as this pops up ALL the time, but is glossed over.
mmmmmmmmmGlobalsAreCool :: IORef PasswordStore
{-# NOINLINE mmmmmmmmmGlobalsAreCool #-}
mmmmmmmmmGlobalsAreCool =
  unsafePerformIO . newIORef $ PasswordStore Map.empty Map.empty
