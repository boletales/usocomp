module MyPrelude (
    module Prelude
  , module Control.Monad
  , module Control.Category
  , tshow
  , sshow
  ) where

import Data.Text hiding (show)
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
tshow = Prelude.show >>> T.pack

sshow :: Show a => a -> String
sshow = Prelude.show