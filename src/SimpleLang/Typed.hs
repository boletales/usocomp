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

class KnownType (t :: SLType) where
  tsleTypeOf :: Proxy t -> SLType

instance KnownType 'SLTUnit where
    tsleTypeOf _ = SLTUnit

instance KnownType 'SLTInt where
    tsleTypeOf _ = SLTInt

instance (KnownType t) => KnownType ('SLTPtr t) where
    tsleTypeOf _ = SLTPtr (tsleTypeOf (Proxy :: Proxy t))

instance (KnownType t, KnownTypes args) => KnownType ('SLTFuncPtr args t) where
    tsleTypeOf _ = SLTFuncPtr (tsleTypesOf (Proxy :: Proxy args)) (tsleTypeOf (Proxy :: Proxy t))

instance (KnownTypes ts) => KnownType ('SLTStruct ts) where
    tsleTypeOf _ = SLTStruct (tsleTypesOf (Proxy :: Proxy ts))

instance (KnownTypes ts) => KnownType ('SLTUnion ts) where
    tsleTypeOf _ = SLTUnion (tsleTypesOf (Proxy :: Proxy ts))

class KnownTypes (ts :: [SLType]) where
    tsleTypesOf :: Proxy ts -> [SLType]

instance KnownTypes '[] where
    tsleTypesOf _ = []

instance (KnownType t, KnownTypes ts) => KnownTypes (t:ts) where
    tsleTypesOf _ = tsleTypeOf (Proxy :: Proxy t) : tsleTypesOf (Proxy :: Proxy ts)


data TypedSLExp (t :: SLType) where
    TSLEConst      ::                                  SLVal                                               -> TypedSLExp 'SLTInt
    TSLELocal      :: (KnownType t                ) => Int                                                 -> TypedSLExp t
    TSLEArg        :: (KnownType t                ) => Int                                                 -> TypedSLExp t
    TSLEPtr        :: (KnownType t                ) => TypedSLRef t                                        -> TypedSLExp ('SLTPtr t)
    TSLEPushCall   :: (KnownType t                ) => TypedSLCall t                                       -> TypedSLExp t
    TSLEFuncPtr    ::                                  TypedSLFuncName args ret                            -> TypedSLExp ('SLTFuncPtr args ret)
    TSLEPrim1      ::                                  SLPrim1 -> TypedSLExp 'SLTInt                       -> TypedSLExp 'SLTInt
    TSLEPrim2      ::                                  SLPrim2 -> TypedSLExp 'SLTInt -> TypedSLExp 'SLTInt -> TypedSLExp 'SLTInt
    TSLEStructNil  ::                                                                                         TypedSLExp ('SLTStruct '[])
    TSLEStructCons :: (KnownType t, KnownTypes ts ) => TypedSLExp t -> TypedSLExp ('SLTStruct ts)          -> TypedSLExp ('SLTStruct (t:ts)) 
    TSLEUnion      :: (KnownType t, Member t ts   ) => TypedSLExp t                                        -> TypedSLExp ('SLTUnion     ts ) 
    TSLEDeRef      :: (KnownType t                ) => TypedSLExp ('SLTPtr t)                              -> TypedSLExp t
    TSLEPtrShift   :: (KnownType t                ) => TypedSLExp ('SLTPtr t) -> TypedSLExp 'SLTInt        -> TypedSLExp ('SLTPtr t)
    TSLEStructGet  :: (KnownType t, KnownTypes ts, StructAt i ts t, KnownNat i, KnownOffset i ts) => TypedSLExp ('SLTStruct ts) -> Proxy i -> TypedSLExp t
    TSLECast       :: (KnownType t, KnownType u, SLTSizeOf t ~ SLTSizeOf u ) => TypedSLExp t -> TypedSLExp u

instance KnownType t => Show (TypedSLExp t) where
  show = show . unTypedSLExp

data TypedSLRef (t :: SLType) where
    TSLRefPtr   :: (KnownSize t) => TypedSLExp ('SLTPtr t) -> TypedSLRef t
    TSLRefLocal :: (KnownSize t) => Int                    -> TypedSLRef t
instance KnownType t => Show (TypedSLRef t) where
  show = show . unTypedSLRef

unTypedSLExp :: forall t. KnownType t => TypedSLExp t -> SLExp
unTypedSLExp expr =
  let tval = tsleTypeOf (Proxy :: Proxy t)
  in case expr of
      TSLEConst val        -> SLEConst val
      TSLELocal i          -> SLELocal tval i
      TSLEArg i            -> SLEArg tval i
      TSLEPtr @t' ref      -> SLEPtr (tsleTypeOf (Proxy :: Proxy t')) (unTypedSLRef ref)
      TSLEPushCall call    -> SLEPushCall (unTypedSLCall call)
      TSLEFuncPtr name     -> SLEFuncPtr (unTypedSLFuncName name)
      TSLEPrim1 prim e     -> SLEPrim1 prim (unTypedSLExp e)
      TSLEPrim2 prim e1 e2 -> SLEPrim2 prim (unTypedSLExp e1) (unTypedSLExp e2)
      TSLEStructNil        -> SLEStructNil
      TSLEStructCons e es  -> SLEStructCons (unTypedSLExp e) (unTypedSLExp es)
      TSLEUnion e          -> SLEUnion tval (unTypedSLExp e)
      TSLEDeRef e          -> SLEDeRef (unTypedSLExp e)
      TSLEPtrShift e1 e2   -> SLEPtrShift (unTypedSLExp e1) (unTypedSLExp e2)
      TSLEStructGet e i    -> SLEStructGet (unTypedSLExp e) (tsleGetOffset e i)
      TSLECast e           -> SLECast tval (unTypedSLExp e)

unTypedSLRef :: forall t. KnownType t => TypedSLRef t -> SLRef
unTypedSLRef ref =
  case ref of
    TSLRefPtr @t' e   -> SLRefPtr (tsleTypeOf (Proxy :: Proxy t')) (unTypedSLExp e)
    TSLRefLocal i     -> SLRefLocal (tsleTypeOf (Proxy :: Proxy t)) i

unTypedSLCall :: forall t. KnownType t => TypedSLCall t -> SLCall
unTypedSLCall call =
  case call of
    TSLSolidFuncCall name e -> SLSolidFuncCall (unTypedSLFuncName name) (unTypedSLExp e)
    TSLFuncRefCall ref e    -> SLFuncRefCall (unTypedSLRef ref) (unTypedSLExp e)
    TSLClosureCall e        -> SLClosureCall (unTypedSLExp e)
  
data TypedSLStatement where
  TSLSInitVar        :: KnownType t => Int -> TypedSLExp t          -> TypedSLStatement
  TSLSSubst          :: KnownType t => TypedSLRef t -> TypedSLExp t -> TypedSLStatement
  TSLSReturn         :: KnownType t => TypedSLExp t                 -> TypedSLStatement
  TSLSTailCallReturn :: KnownType t => TypedSLCall t                -> TypedSLStatement

data TypedSLCall (t :: SLType) where
    TSLSolidFuncCall :: (KnownTypes ts                      ) => TypedSLFuncName ts t          -> TypedSLExp ('SLTStruct ts) -> TypedSLCall t
    TSLFuncRefCall   :: (KnownTypes ts                      ) => TypedSLRef ('SLTFuncPtr ts t) -> TypedSLExp ('SLTStruct ts) -> TypedSLCall t
    TSLClosureCall   :: (KnownTypes ('SLTFuncPtr ts t ': ts)) => TypedSLExp ('SLTStruct ('SLTFuncPtr ts t ': ts))            -> TypedSLCall t


class TypedSLCallable (args :: [SLType]) (ret :: SLType) (t :: Type) | t -> args ret where
  tslCall :: t -> TypedSLExp ('SLTStruct args) -> TypedSLCall ret

instance (KnownTypes args) => TypedSLCallable args ret (TypedSLFuncBlock args ret) where
  tslCall = tslfName >>> TSLSolidFuncCall

instance (KnownTypes args) => TypedSLCallable args ret (TypedSLFuncName args ret) where
  tslCall = TSLSolidFuncCall

instance (KnownTypes args) => TypedSLCallable args ret (TypedSLRef ('SLTFuncPtr args ret)) where
  tslCall = TSLFuncRefCall

  
data TypedSLBlock where
    TSLBSingle :: TypedSLStatement -> TypedSLBlock
    TSLBMulti  :: V.Vector TypedSLBlock -> TypedSLBlock
    TSLBCase   :: V.Vector (TypedSLExp 'SLTInt, TypedSLBlock) -> TypedSLBlock -> TypedSLBlock
    TSLBWhile  :: TypedSLExp 'SLTInt -> TypedSLBlock -> TypedSLBlock

instance Show TypedSLBlock where
  show = show . unTypedSLBlock

unTypedSLBlock :: TypedSLBlock -> SLBlock
unTypedSLBlock b =
  case b of
    TSLBSingle s   -> SLBSingle (unTypedSLStatement s)
    TSLBMulti  s   -> SLBMulti (V.map unTypedSLBlock s)
    TSLBCase   s d -> SLBCase (V.map (\(e, b) -> (unTypedSLExp e, unTypedSLBlock b)) s) (unTypedSLBlock d)
    TSLBWhile  e b -> SLBWhile (unTypedSLExp e) (unTypedSLBlock b)

unTypedSLStatement :: TypedSLStatement -> SLStatement
unTypedSLStatement s =
  case s of
    TSLSInitVar i e         -> SLSInitVar i (unTypedSLExp e)
    TSLSSubst   ref e       -> SLSSubst     (unTypedSLRef ref) (unTypedSLExp e)
    TSLSReturn  e           -> SLSReturn    (unTypedSLExp e)
    TSLSTailCallReturn call -> SLSTailCallReturn (unTypedSLCall call)

data TypedSLFuncBlock (args :: [SLType]) (ret :: SLType) =
      TSLFuncBlock {
          tslfName     :: TypedSLFuncName args ret
        , tslfBlock    :: TypedSLBlock
      }
      deriving (Show)

unTypedSLFuncBlock :: forall args ret. (KnownNat (SLTSizeOf ('SLTStruct args))) => TypedSLFuncBlock args ret -> SLFuncBlock
unTypedSLFuncBlock (TSLFuncBlock (TypedSLFuncName name) block) =
  SLFuncBlock name ((fromIntegral . natVal) (Proxy :: Proxy (SLTSizeOf ('SLTStruct args)))) (unTypedSLBlock block)