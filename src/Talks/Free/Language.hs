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
