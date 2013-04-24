module Talks.Free.Prelude (
  module X
) where

import Prelude as X (IO, Int, Eq, Ord, Show, (.), show, (==), putStrLn, ($), const, read, id, print)
import Control.Applicative as X ((<$>), (<*>), (*>), (<*), pure)
import Control.Monad as X (void, when, unless, liftM, (>>=), return)
import Data.Functor as X (Functor, fmap)
import Data.Traversable as X (mapM)
import Data.Foldable as X (mapM_)
import Data.Maybe as X
