module MyPrelude (
    module Prelude
  , module Control.Monad
  , module Control.Category
  , tshow
  ) where

import Data.Text as T
import Control.Category ((.), id, (>>>), (<<<))
import Control.Monad
import Prelude hiding (
    id
  , (.)
  , lookup
  , foldl
  , words
  , exp
  )

tshow :: Show a => a -> Text
tshow = T.pack . show
