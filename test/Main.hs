module Main where

import qualified Talks.Free.Tests
import Test.Framework

main ::
  IO ()
main = 
  defaultMain tests 

tests ::
  [Test]
tests =
  [
    testGroup "Tests"
      [
        Talks.Free.Tests.test
      ]
  ]

