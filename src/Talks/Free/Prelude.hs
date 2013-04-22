module Talks.Free.Prelude (
  module X
) where

import Prelude as X (IO, Int, Eq, Ord, Show, (.), show, (==), putStrLn, ($), const, read)
import Control.Applicative as X ((<$>), (<*>), (*>), (<*), pure)
import Control.Monad as X (void, when, unless, liftM, (>>=), return)
import Data.Functor as X (fmap)
import Data.Traversable as X (mapM)
import Data.Maybe as X
