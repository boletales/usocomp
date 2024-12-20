{-# LANGUAGE MonoLocalBinds #-}

module SimpleLang.Tools (
    SLPos(..)
  , SLLocalPos(..)
  , pushPos
  , popPos
  , slPosAbbrText
  , prettyPrintSLPos
  , rootsSLPos
  ) where

import MyPrelude

import SimpleLang.Def
import Data.Text as T
import qualified Data.List as L

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
 } deriving (Eq, Ord)

instance Show SLPos where
  show = T.unpack . prettyPrintSLPos

rootsSLPos :: SLPos -> [SLPos]
rootsSLPos (SLPos f xs) = 
  SLPos f <$> L.tails xs

-- >>> rootsSLPos (SLPos SLFuncMain [SLLPMulti 1, SLLPCaseCond 2, SLLPCaseBody 3])
-- [main.whenBody_3.whenCond_2[1],main.whenBody_3.whenCond_2,main.whenBody_3,main]

prettyPrintSLPos :: SLPos -> Text
prettyPrintSLPos pos =
  let SLPos f xs = pos
  in prettyPrintSLFuncName f <> intercalate "" (L.map prettyPrintSLLocalPos (L.reverse xs))

pushPos :: SLLocalPos -> SLPos -> SLPos
pushPos x (SLPos f xs) = SLPos f (x:xs)

popPos :: SLPos -> SLPos
popPos (SLPos f (_:xs)) = SLPos f xs
popPos (SLPos f [])     = SLPos f []

slPosAbbrText :: SLPos -> Text
slPosAbbrText pos =
  let SLPos f xs = pos
  in (pack . show) f <> intercalate "" (L.map (pack . show) (L.reverse xs))
