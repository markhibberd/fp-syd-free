module Talks.Free.Ugly (
  lookupPassword
, mkPassword
, storePassword
, getSetting
, setSetting
) where

import Control.Applicative

import Data.IORef
import Data.Text
import Data.Text.Lazy (fromChunks)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Map (Map)
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Map as Map

import System.IO.Unsafe

import Crypto.PasswordStore

--
-- (Hopefully) the worst haskell you have seen this week.
--

data PasswordStore =
  PasswordStore {
      settings :: Map Text Text
    , passwords :: Map Text Text
    }

-- This really isn't cool, but not so obvious code that is about
-- the same as this pops up ALL the time, but is glossed over.
mmmmmmmmmGlobalsAreCool :: IORef PasswordStore
{-# NOINLINE mmmmmmmmmGlobalsAreCool #-}
mmmmmmmmmGlobalsAreCool = unsafePerformIO . newIORef $ PasswordStore Map.empty Map.empty

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

getSetting :: Text -> IO (Maybe Text)
getSetting key =
  Map.lookup key <$> getSettings

setSetting :: Text -> Text -> IO ()
setSetting key value =
  (Map.insert key value <$> getSettings) >>= setSettings

lookupPassword :: Text -> IO (Maybe Text)
lookupPassword key =
  Map.lookup key <$> getPasswords

mkPassword :: IO Text
mkPassword =
  fmap (md5sum . decodeUtf8 . exportSalt) genSaltIO

storePassword :: Text -> Text -> IO ()
storePassword key value =
  (Map.insert key value <$> getPasswords) >>= setPasswords

md5sum :: Text -> Text
md5sum =
  pack . show . md5 . encodeUtf8 . fromChunks . return
