{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Talks.Free.Language where

import Talks.Free.Prelude
import Talks.Free.Ugly

import Control.Monad.Free
import Data.Text
import Data.Monoid ((<>))

data Language a =
    GenPassword (Maybe Int) Text a
  | SetPassword Text Text a
  | GetPassword Text (Maybe Text -> a)

instance Functor Language where
  fmap f (GenPassword n key a) = GenPassword n key (f a)
  fmap f (SetPassword key pw a) = SetPassword key pw (f a)
  fmap f (GetPassword key t) = GetPassword key (f . t)



genPw :: Maybe Int -> Text -> Free Language ()
genPw n key =
  liftF $ GenPassword n key ()

setPw :: Text -> Text -> Free Language ()
setPw key pw =
  liftF $ SetPassword key pw ()

getPw :: Text -> Free Language (Maybe Text)
getPw key =
  liftF $ GetPassword key id



interpret :: Free Language a -> IO a
interpret (Pure a) =
  return a
interpret (Free (GenPassword n key next)) =
  do clearSetting "password.length"
     mapM_ (\i -> setSetting "password.length" (pack . show $ i)) n
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



describe :: Show a => Free Language a -> [Text]
describe (Pure a) =
  ["a value: " <> (pack . show $ a)]
describe (Free (GenPassword _ key next)) =
  ("Generate and store password with key " <> key) : describe next
describe (Free (SetPassword key _ next)) =
  ("Set a password word with key " <> key) : describe next
describe (Free (GetPassword key next)) =
  ("Get a password word with key " <> key) : describe (next (Just "<password>"))
