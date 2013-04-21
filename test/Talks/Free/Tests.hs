module Talks.Free.Tests
  (
    main
  , test
  ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Talks.Free

main ::
  IO ()
main =
  defaultMain [test]

test ::
  Test
test =
    testGroup "Talks.Free"
      [
        testProperty "Identity" prop_identity
      ]

prop_identity ::
  Int
  -> Bool
prop_identity n =
  bletch n == n

