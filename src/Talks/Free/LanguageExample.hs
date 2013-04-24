{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Talks.Free.LanguageExample where

import Control.Monad.Free
import Data.Maybe
import Data.Text
import Talks.Free.Prelude
import Talks.Free.Language

program :: Free Language (Maybe Text)
program =
  do genPw Nothing "p1"
     genPw (Just 3) "p2"
     genPw Nothing "p3"
     genPw Nothing "p4"
     setPw "p5" "p4"
     getPw "p2"

runLanguageInterpret :: IO ()
runLanguageInterpret =
  do r <- interpret $ program
     putStrLn $ maybe "nothing" unpack r

runLanguageDescribe :: IO ()
runLanguageDescribe = putStrLn . unpack . intercalate "\n" . describe $ program
