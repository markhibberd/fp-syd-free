{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Talks.Free.LanguageExample where

import Talks.Free.Prelude
import Talks.Free.Language

runLanguageExample :: IO ()
runLanguageExample = interpret $
  do genPw Nothing "p1"
     genPw (Just 6) "p2"
     genPw Nothing "p3"
     genPw Nothing "p4"
     setPw "p5" "p4"
