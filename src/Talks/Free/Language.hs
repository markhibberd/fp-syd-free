module Talks.Free.Language where

import Data.Text

data Language a =

    Get Text (Text -> a)
  | Set Text Text a
