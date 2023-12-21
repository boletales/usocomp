{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleLang.FromFuncLang.Lib where

import FuncLang.Def
import SimpleLang.Def
import SimpleLang.Tools.Manual

import Data.Text as T
import Control.Category
import Prelude hiding ((.), id)

type (->>) = FLTLambda

class FLPrim t a | a -> t where
  flpname   :: a -> Text
  flPrim    :: a -> FLExp Text t
  flptoSL   :: a -> SLFuncBlock

data FLPrimII2I =
      FLPAdd
    | FLPSub
    | FLPMul
    | FLPEqI
    | FLPNeqI
    | FLPLtI
    | FLPGtI
    deriving (Eq, Ord, Show)

instance FLPrim (FLTInt ->> FLTInt ->> FLTInt) FLPrimII2I where
  flpname p = case p of
      FLPAdd  -> "Add"
      FLPSub  -> "Sub"
      FLPMul  -> "Mul"
      FLPEqI  -> "EqI"
      FLPNeqI -> "NeqI"
      FLPLtI  -> "LtI"
      FLPGtI  -> "GtI"
  flPrim p = FLEVar @Text @(FLTInt ->> FLTInt ->> FLTInt) (FLVar (flpname p))
  
  flptoSL p = 
    hsFuncToSLFuncBlock @'[SLTInt, SLTInt] @SLTInt (SLUserFunc "LibFLC" (flpname p)) $ \x1 x2 -> 
      slmReturn $
        (
          case p of
            FLPAdd  -> _add
            FLPSub  -> _sub
            FLPMul  -> _mul
            FLPEqI  -> _eq
            FLPNeqI -> (\a b -> _inv (_eq a b))
            FLPLtI  -> _lt
            FLPGtI  -> _gt
          ) x1 x2