{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

module SimpleLang.Tools (
    SLPos(..)
  , SLLocalPos(..)
  , pushPos
  , popPos
  , slPosAbbrText
  , prettyPrintSLPos
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
      deriving (Eq, Ord)

instance Show SLLocalPos where
  show = T.unpack . prettyPrintSLLocalPos

prettyPrintSLLocalPos :: SLLocalPos -> Text
prettyPrintSLLocalPos p =
  case p of
    SLLPMulti i -> "[" <> (pack . show) i <> "]"
    SLLPCaseCond i -> ".whenCond_" <> (pack . show) i
    SLLPCaseBody i -> ".whenBody_" <> (pack . show) i
    SLLPCaseElseBody -> ".whenElseBody"
    SLLPWhileCond -> ".whileCond"
    SLLPWhileBody -> ".whileBody"
    SLLPWhileFooter -> ".whileFooter"
    SLLPForceReturn -> ".forceReturn"
    SLLPExpr e -> ".expr " <> (pack . show) e

data SLPos = SLPos {
    slpFuncName :: SLFuncName
  , slpLocalPos :: [SLLocalPos]
 } deriving (Show, Eq, Ord)

prettyPrintSLPos :: SLPos -> Text
prettyPrintSLPos pos =
  let SLPos f xs = pos
  in prettyPrintSLFuncName f <> intercalate "" (Prelude.map prettyPrintSLLocalPos (Prelude.reverse xs))

pushPos :: SLLocalPos -> SLPos -> SLPos
pushPos x (SLPos f xs) = SLPos f (x:xs)

popPos :: SLPos -> SLPos
popPos (SLPos f (_:xs)) = SLPos f xs
popPos (SLPos f [])     = SLPos f []

slPosAbbrText :: SLPos -> Text
slPosAbbrText pos =
  let SLPos f xs = pos
  in (pack . show) f <> intercalate "" (Prelude.map (pack . show) (Prelude.reverse xs))