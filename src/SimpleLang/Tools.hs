{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

module SimpleLang.Tools (
    SLPos(..)
  , SLLocalPos(..)
  , pushPos
  , popPos
  , slPosAbbrText
  ) where

import SimpleLang.Def
import Data.Text as T
import Prelude hiding (exp)

data SLLocalPos =
        SLLPMulti Int
      | SLLPCaseCond Int
      | SLLPCaseBody Int
      | SLLPCaseElseBody
      | SLLPWhileCond
      | SLLPWhileBody
      | SLLPWhileFooter
      | SLLPForceReturn
      | SLLPExpr SLExp
      deriving (Show, Eq, Ord)

data SLPos = SLPos {
    slpFuncName :: SLFuncName
  , slpLocalPos :: [SLLocalPos]
 } deriving (Show, Eq, Ord)

pushPos :: SLLocalPos -> SLPos -> SLPos
pushPos x (SLPos f xs) = SLPos f (x:xs)

popPos :: SLPos -> SLPos
popPos (SLPos f (_:xs)) = SLPos f xs
popPos (SLPos f [])     = SLPos f []

slPosAbbrText :: SLPos -> Text
slPosAbbrText pos =
  let SLPos f xs = pos
  in (pack . show) f <> "." <> intercalate "." (Prelude.map (pack . show) (Prelude.reverse xs))
