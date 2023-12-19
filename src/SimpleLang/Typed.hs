{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module SimpleLang.Typed where

import SimpleLang.Def

import Data.Vector as V
import Data.Map as M
import GHC.TypeNats
import Data.Type.Bool
import Data.Proxy
import Data.Text as T
import Data.Kind
import Prelude hiding ((.), id, exp)
import Control.Category
import qualified Data.List as L


type family NatMax (n :: Nat) (m :: Nat) :: Nat where
  NatMax n m = If (n <=? m) m n

type family SLTSizeOf (t :: SLType) :: Nat where
  SLTSizeOf SLTUnit               = 0
  SLTSizeOf SLTInt                = 1
  SLTSizeOf (SLTFuncPtr args ret) = 1
  SLTSizeOf (SLTPtr t           ) = 1
  SLTSizeOf (SLTStruct (t:ts)   ) = SLTSizeOf t + SLTSizeOf (SLTStruct ts)
  SLTSizeOf (SLTStruct '[]      ) = 0
  SLTSizeOf (SLTUnion  (t:ts)   ) = NatMax (SLTSizeOf t) (SLTSizeOf (SLTUnion ts))
  SLTSizeOf (SLTUnion  '[]      ) = 0

class Member (t :: SLType) (ts :: [SLType])

instance {-# OVERLAPPING #-}  Member t (t:ts)
instance {-# OVERLAPPABLE #-} Member t ts => Member t (t':ts)

class StructAt (i :: Nat) (ts :: [SLType]) (t :: SLType) | i ts -> t

instance {-# OVERLAPPING #-}   StructAt 0 (t:ts) t
instance {-# OVERLAPPABLE #-} (StructAt i ts t, j ~ i + 1) => StructAt j (t':ts) t'

type family SLTStructIndexToOffset (i :: Nat) (ts :: [SLType]) :: Nat where
  SLTStructIndexToOffset 0 (t:ts) = 0
  SLTStructIndexToOffset i (t:ts) = SLTSizeOf t + SLTStructIndexToOffset (i - 1) ts

type KnownOffset i ts = KnownNat (SLTStructIndexToOffset i ts)

tsleGetOffset :: forall i ts. (KnownOffset i ts) => TypedSLExp ('SLTStruct ts) -> Proxy i -> Int
tsleGetOffset _ _ = fromIntegral (natVal (Proxy :: Proxy (SLTStructIndexToOffset i ts)))

type KnownSize t = KnownNat (SLTSizeOf t)
type KnownSizes ts = KnownNat (SLTSizeOf ('SLTStruct ts))


data TypedSLExp (t :: SLType) where
    TSLEConst      ::                                  SLVal                                               -> TypedSLExp 'SLTInt
    TSLELocal      :: (KnownSize t                ) => Int                                                 -> TypedSLExp t
    TSLEArg        :: (KnownSize t                ) => Int                                                 -> TypedSLExp t
    TSLEPtr        :: (KnownSize t                ) => TypedSLRef t                                        -> TypedSLExp ('SLTPtr t)
    TSLEPushCall   :: (KnownSize t                ) => TypedSLCall t                                       -> TypedSLExp t
    TSLEFuncPtr    ::                                  TypedSLFuncName args ret                            -> TypedSLExp ('SLTFuncPtr args ret)
    TSLEPrim1      ::                                  SLPrim1 -> TypedSLExp 'SLTInt                       -> TypedSLExp 'SLTInt
    TSLEPrim2      ::                                  SLPrim2 -> TypedSLExp 'SLTInt -> TypedSLExp 'SLTInt -> TypedSLExp 'SLTInt
    TSLEStructNil  ::                                                                                         TypedSLExp ('SLTStruct '[])
    TSLEStructCons :: (KnownSize t, KnownSizes ts ) => TypedSLExp t -> TypedSLExp ('SLTStruct ts)          -> TypedSLExp ('SLTStruct (t:ts)) 
    TSLEUnion      :: (KnownSize t, Member t ts   ) => TypedSLExp t                                        -> TypedSLExp ('SLTUnion     ts ) 
    TSLEDeRef      :: (KnownSize t                ) => TypedSLExp ('SLTPtr t)                              -> TypedSLExp t
    TSLEPtrShift   :: (KnownSize t                ) => TypedSLExp ('SLTPtr t) -> TypedSLExp 'SLTInt        -> TypedSLExp ('SLTPtr t)
    TSLEStructGet  :: (KnownSize t, KnownSizes ts, StructAt i ts t, KnownNat i, KnownOffset i ts) => TypedSLExp ('SLTStruct ts) -> Proxy i -> TypedSLExp t
    TSLECast       :: (KnownSize t, KnownSize u, SLTSizeOf t ~ SLTSizeOf u ) => TypedSLExp t -> TypedSLExp u

instance Show (TypedSLExp t) where
  show = T.unpack . prettyPrintSLExp

data TypedSLRef (t :: SLType) where
    TSLRefPtr   :: (KnownSize t) => TypedSLExp ('SLTPtr t) -> TypedSLRef t
    TSLRefLocal :: (KnownSize t) => Int                    -> TypedSLRef t
instance Show (TypedSLRef t) where
  show = T.unpack . prettyPrintSLRef

  
data TypedSLStatement where
  TSLSInitVar        :: KnownSize t => Int -> TypedSLExp t          -> TypedSLStatement
  TSLSSubst          :: KnownSize t => TypedSLRef t -> TypedSLExp t -> TypedSLStatement
  TSLSReturn         :: KnownSize t => TypedSLExp t                 -> TypedSLStatement
  TSLSTailCallReturn :: KnownSize t => TypedSLCall t                -> TypedSLStatement

data TypedSLCall (t :: SLType) where
    TSLSolidFuncCall :: (KnownSize ('SLTStruct ts)                      ) => TypedSLFuncName ts t          -> TypedSLExp ('SLTStruct ts) -> TypedSLCall t
    TSLFuncRefCall   :: (KnownSize ('SLTStruct ts)                      ) => TypedSLRef ('SLTFuncPtr ts t) -> TypedSLExp ('SLTStruct ts) -> TypedSLCall t
    TSLClosureCall   :: (KnownSize ('SLTStruct ('SLTFuncPtr ts t ': ts))) => TypedSLExp ('SLTStruct ('SLTFuncPtr ts t ': ts))            -> TypedSLCall t


class TypedSLCallable (args :: [SLType]) (ret :: SLType) (t :: Type) | t -> args ret where
  tslCall :: t -> TypedSLExp ('SLTStruct args) -> TypedSLCall ret

instance (KnownSizes args) => TypedSLCallable args ret (TypedSLFuncBlock args ret) where
  tslCall = tslfName >>> TSLSolidFuncCall

instance (KnownSizes args) => TypedSLCallable args ret (TypedSLFuncName args ret) where
  tslCall = TSLSolidFuncCall

instance (KnownSizes args) => TypedSLCallable args ret (TypedSLRef ('SLTFuncPtr args ret)) where
  tslCall = TSLFuncRefCall

  
data TypedSLBlock where
    TSLBSingle :: TypedSLStatement -> TypedSLBlock
    TSLBMulti  :: V.Vector TypedSLBlock -> TypedSLBlock
    TSLBCase   :: V.Vector (TypedSLExp 'SLTInt, TypedSLBlock) -> TypedSLBlock -> TypedSLBlock
    TSLBWhile  :: TypedSLExp 'SLTInt -> TypedSLBlock -> TypedSLBlock

data TypedSLFuncBlock (args :: [SLType]) (ret :: SLType) =
      TSLFuncBlock {
          tslfName     :: TypedSLFuncName args ret
        , tslfBlock    :: TypedSLBlock
      }
      deriving (Show)

unTypedSLFuncBlock :: forall args ret. (KnownNat (SLTSizeOf ('SLTStruct args))) => TypedSLFuncBlock args ret -> SLFuncBlock
unTypedSLFuncBlock (TSLFuncBlock (TypedSLFuncName name) block) =
  SLFuncBlock name ((fromIntegral . natVal) (Proxy :: Proxy (SLTSizeOf ('SLTStruct args)))) block