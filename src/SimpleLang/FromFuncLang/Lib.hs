{-# LANGUAGE FunctionalDependencies #-}

module SimpleLang.FromFuncLang.Lib (
    FLPrimAny(..)
  , FLPrim(..)
  , FLPrimII2I(..)
  , libFLCSLFuncs
  , libFLCSigMap
  , libFLCClsMap
) where

import MyPrelude

import FuncLang.Def
import SimpleLang.Def
import SimpleLang.Tools.Manual

import Data.Text as T
import qualified Data.List as L

type (->>) = FLTLambda

data FLPrimAny where 
  FLPrimAny :: FLPrim t a => a -> FLPrimAny


libFLCSLFuncs :: [SLFuncBlock]
libFLCSLFuncs = flptoSL <$> [
    FLPAdd
  , FLPSub
  , FLPMul
  , FLPEqI
  , FLPNeqI
  , FLPLtI
  , FLPGtI
  ]

libFLCSLFuncSigs :: [SLFuncSignature]
libFLCSLFuncSigs = slfSignature <$> libFLCSLFuncs

libFLCFLNames :: [Text]
libFLCFLNames = flpname <$> [
    FLPAdd
  , FLPSub
  , FLPMul
  , FLPEqI
  , FLPNeqI
  , FLPLtI
  , FLPGtI
  ]

libFLCSigMap :: [(Text, SLFuncSignature)]
libFLCSigMap = L.zip libFLCFLNames libFLCSLFuncSigs

libFLCClss :: [([FLType], FLType)]
libFLCClss = flpclstype <$> [
    FLPAdd
  , FLPSub
  , FLPMul
  , FLPEqI
  , FLPNeqI
  , FLPLtI
  , FLPGtI
  ]

libFLCClsMap :: [(Text, ([FLType], FLType))]
libFLCClsMap = L.zip libFLCFLNames libFLCClss

class FLPrim t a | a -> t where
  flpname   :: a -> Text
  flPrim    :: a -> FLExp Text t
  flptoSL   :: a -> SLFuncBlock
  flpclstype :: a -> ([FLType], FLType)

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
    hsFuncToSLFuncBlock @'[SLTInt, SLTInt] @SLTInt (SLUserFunc "LibFLC" (flpname p)) [] $ \x1 x2 -> 
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
  
  flpclstype _ = ([FLTInt, FLTInt], FLTInt)