{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Talks.Free.Language where

import Talks.Free.Prelude
import Talks.Free.Ugly

import Control.Monad.Free
import Data.Text

data Language a =
    GenPassword (Maybe Int) Text a
  | SetPassword Text Text a
  | GetPassword Text (Maybe Text -> a)

instance Functor Language where
  fmap f (GenPassword n key a) = GenPassword n key (f a)
  fmap f (SetPassword key pw a) = SetPassword key pw (f a)
  fmap f (GetPassword key t) = GetPassword key (f . t)

type Passwords a = Free Language a

freeLanguage :: Language a -> Passwords a
freeLanguage language=
  Free (fmap Pure language)

genPw :: Maybe Int -> Text -> Passwords ()
genPw n key =
  freeLanguage $ GenPassword n key ()

setPw :: Text -> Text -> Passwords ()
setPw key pw =
  freeLanguage $ SetPassword key pw ()

getPw :: Text -> Passwords (Maybe Text)
getPw key =
  freeLanguage $ GetPassword key id

interpret :: Free Language a -> IO a
interpret (Pure a) =
  return a
interpret (Free (GenPassword n key next)) =
  do void $ mapM (\i -> setSetting "password.length" (pack . show $ i)) n
     pw <- mkPassword
     clearSetting "password.length"
     storePassword key pw
     interpret next
interpret (Free (SetPassword key pw next)) =
  do storePassword key pw
     interpret next
interpret (Free (GetPassword key next)) =
  do pw <- lookupPassword key
     interpret $ next pw
