{-# LANGUAGE OverloadedStrings #-}

module SimpleLang.Tools where

import SimpleLang.Def
import qualified Data.Foldable as M
import Data.Vector as V
import Control.Monad.State
import Data.Text

data SLLocalPos =
        SLLPMulti Int
      | SLLPCaseCond Int
      | SLLPCaseBody Int
      | SLLPCaseElseBody
      | SLLPWhileCond
      | SLLPWhileBody
      | SLLPWhileFooter
      deriving (Show, Eq, Ord)

data SLPos = SLPos {
    slpFuncName :: SLFuncName
  , slpLocalPos :: [SLLocalPos]
 } deriving (Show, Eq, Ord)

pushPos :: SLLocalPos -> SLPos -> SLPos
pushPos x (SLPos f xs) = SLPos f (x:xs)

popPos :: SLPos -> SLPos
popPos (SLPos f (_:xs)) = SLPos f xs

slPosAbbrText :: SLPos -> Text
slPosAbbrText pos =
  let SLPos f xs = pos
  in (pack . show) f <> "." <> (intercalate "." (Prelude.map (pack . show) (Prelude.reverse xs)))