{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Talks.Free.UglyExample where

import Talks.Free.Prelude
import Talks.Free.Ugly

import Data.Text (unpack)
import Data.Monoid ((<>))
import Data.Foldable (any)

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

     -- generate and store a sekrit password

     clearSetting "password.length"

     setSetting "log.level" "quiet"

     p3 <- mkPassword

     storePassword "p3" p3

     setSetting "log.level" "noisy"

     p1' <- lookupPassword "p1"
     p2' <- lookupPassword "p2"
     p3' <- lookupPassword "p3"

     if any (p1 ==) p1'
       then putStrLn $ "=== store worked as expected: " <> unpack p1
       else putStrLn "=== what happened?"

     if any (p2 ==) p2'
       then putStrLn $ "=== store worked as expected: " <> unpack p2
       else putStrLn "=== what happened?"

     if any (p3 ==) p3'
       then putStrLn $ "=== store worked as expected: " <> unpack p3
       else putStrLn "=== what happened?"
