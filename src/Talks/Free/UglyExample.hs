{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Talks.Free.UglyExample where

import Talks.Free.Prelude
import Talks.Free.Ugly

import Data.Text (unpack)
import Data.Monoid ((<>))
import Data.Foldable (elem)

runUglyExample :: IO ()
runUglyExample =
     -- turn up logging
  do setSetting "log.level" "noisy"

     -- generate some passwords
     p1 <- mkPassword

     setSetting "password.length" "6"

     p2 <- mkPassword

     -- store some passwords

     storePassword "p1" p1

     storePassword "p2" p2

     -- generate and store some more passwords

     clearSetting "password.length"

     setSetting "log.level" "quiet"

     p3 <- mkPassword

     p4 <- mkPassword

     let p5 = "passw0rd"

     storePassword "p3" p4 -- Oops!

     storePassword "p4" p4

     storePassword "p5" p5

     setSetting "log.level" "noisy"

     p1' <- lookupPassword "p1"
     p2' <- lookupPassword "p2"
     p3' <- lookupPassword "p3"
     p4' <- lookupPassword "p4"
     p5' <- lookupPassword "p5"

     putStrLn $ if p1 `elem` p1'
       then "=== store worked as expected: " <> unpack p1
       else "=== what happened?"

     putStrLn $ if p2 `elem` p2'
       then "=== store worked as expected: " <> unpack p2
       else "=== what happened?"

     putStrLn $ if p3 `elem` p3'
       then "=== store worked as expected: " <> unpack p3
       else "=== what happened?"

     putStrLn $ if p4 `elem` p4'
       then "=== store worked as expected: " <> unpack p4
       else "=== what happened?"

     putStrLn $ if p5 `elem` p5'
       then "=== store worked as expected: " <> unpack p5
       else "=== what happened?"
