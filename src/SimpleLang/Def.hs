{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}


{-| 
Module      : SimpleLang.Def
Description : SimpleLangの定義
-}

module SimpleLang.Def (
      SLAddr (..)
    , SLVal (..)
    , SLFuncName (..)
    , SLCall (..)
    , SLPrim1 (..)
    , SLPrim2 (..)
    , TypedSLExp (..)
    , SLRef (..)
    , SLStatement (..)
    , SLBlock (..)
    , SLFuncBlock (..)
    , TypedSLFuncBlock (..)
    , SLProgram
    , TypedSLFuncName (..)
    , SLType (..)
    , SLTSizeOf
    , SLCallable (..)
    , KnownSize
    , KnownSizes
    , SLTStructIndexToOffset
    , KnownOffset
    , sleSizeOf
    , sleGetOffset
    , slRefToPtr
    , prettyPrintFuncName
    , prettyPrintSLExp
    , prettyPrintSLRef
    , prettyPrintSLStatement
    , prettyPrintSLBlock
    , prettyPrintSLProgram
    , unTypedSLFuncBlock
    , unTypedSLFuncName
  ) where

import Data.Vector as V
import Data.Map as M
import GHC.TypeNats
import Data.Type.Bool
import Data.Proxy
import Data.Text as T
import Data.Kind
import Prelude hiding ((.), id, exp)
import Control.Category

-- 接頭辞 SL: SimpleLang に関連するものの型

{-|
  SimpleLangは、関数・コールスタック・スコープ付きローカル変数の概念をサポートする言語です。

  変数名の遮蔽の概念はなく、ローカル変数は外側のスコープから順に振られた番号で指定されます。
  包含関係にないスコープ間では変数の番号が一意ではありません。
-}

data SLType =
      SLTUnit
    | SLTInt
    | SLTFuncPtr [SLType] SLType
    | SLTPtr SLType
    | SLTStruct [SLType]
    | SLTUnion  [SLType]
    deriving (Show, Eq)

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

sleGetOffset :: forall i ts. (KnownOffset i ts) => TypedSLExp ('SLTStruct ts) -> Proxy i -> Int
sleGetOffset _ _ = fromIntegral (natVal (Proxy :: Proxy (SLTStructIndexToOffset i ts)))

newtype SLAddr = SLAddr Int deriving (Show, Eq)
newtype SLVal  = SLVal  Int deriving (Show, Eq)

data SLCall (t :: SLType) where
    SLSolidFuncCall :: (KnownSize ('SLTStruct ts)                      ) => TypedSLFuncName ts t     -> TypedSLExp ('SLTStruct ts) -> SLCall t
    SLFuncRefCall   :: (KnownSize ('SLTStruct ts)                      ) => SLRef ('SLTFuncPtr ts t) -> TypedSLExp ('SLTStruct ts) -> SLCall t
    SLClosureCall   :: (KnownSize ('SLTStruct ('SLTFuncPtr ts t ': ts))) => TypedSLExp ('SLTStruct ('SLTFuncPtr ts t ': ts))       -> SLCall t

class SLCallable (args :: [SLType]) (ret :: SLType) (t :: Type) | t -> args ret where
  slCall :: t -> TypedSLExp ('SLTStruct args) -> SLCall ret

instance (KnownSizes args) => SLCallable args ret (TypedSLFuncBlock args ret) where
  slCall = tslfName >>> SLSolidFuncCall

instance (KnownSizes args) => SLCallable args ret (TypedSLFuncName args ret) where
  slCall = SLSolidFuncCall

instance (KnownSizes args) => SLCallable args ret (SLRef ('SLTFuncPtr args ret)) where
  slCall = SLFuncRefCall

deriving instance Show (SLCall t)

type KnownSize t = KnownNat (SLTSizeOf t)
type KnownSizes ts = KnownNat (SLTSizeOf ('SLTStruct ts))

data SLPrim1 =
        SLPrim1Inv
      deriving (Show, Eq)

data SLPrim2 =
        SLPrim2Add
      | SLPrim2Sub
      | SLPrim2Mult
      | SLPrim2Shift
      | SLPrim2And
      | SLPrim2Or
      | SLPrim2Xor
      | SLPrim2Gt
      | SLPrim2Lt
      | SLPrim2Eq
      deriving (Show, Eq)

data TypedSLExp (t :: SLType) where
    SLEConst      ::                                  SLVal                                               -> TypedSLExp 'SLTInt
    SLELocal      :: (KnownSize t                ) => Int                                                 -> TypedSLExp t
    SLEArg        :: (KnownSize t                ) => Int                                                 -> TypedSLExp t
    SLEPtr        :: (KnownSize t                ) => SLRef t                                             -> TypedSLExp ('SLTPtr t)
    SLEPushCall   :: (KnownSize t                ) => SLCall t                                            -> TypedSLExp t
    SLEFuncPtr    :: (KnownSize t                ) => TypedSLFuncName args ret                            -> TypedSLExp ('SLTFuncPtr args ret)
    SLEPrim1      ::                                  SLPrim1 -> TypedSLExp 'SLTInt                       -> TypedSLExp 'SLTInt
    SLEPrim2      ::                                  SLPrim2 -> TypedSLExp 'SLTInt -> TypedSLExp 'SLTInt -> TypedSLExp 'SLTInt
    SLEStructNil  ::                                                                                         TypedSLExp ('SLTStruct '[])
    SLEStructCons :: (KnownSize t, KnownSizes ts ) => TypedSLExp t -> TypedSLExp ('SLTStruct ts)          -> TypedSLExp ('SLTStruct (t:ts)) 
    SLEUnion      :: (KnownSize t, Member t ts   ) => TypedSLExp t                                        -> TypedSLExp ('SLTUnion     ts ) 
    SLEDeRef      :: (KnownSize t                ) => TypedSLExp ('SLTPtr t)                              -> TypedSLExp t
    SLEPtrShift   :: (KnownSize t                ) => TypedSLExp ('SLTPtr t) -> TypedSLExp 'SLTInt        -> TypedSLExp ('SLTPtr t)
    SLEStructGet  :: (KnownSize t, KnownSizes ts, StructAt i ts t, KnownNat i, KnownOffset i ts) => TypedSLExp ('SLTStruct ts) -> Proxy i -> TypedSLExp t
    SLECast       :: (KnownSize t, KnownSize u, SLTSizeOf t ~ SLTSizeOf u ) => TypedSLExp t -> TypedSLExp u

instance Show (TypedSLExp t) where
  show = T.unpack . prettyPrintSLExp

data SLRef (t :: SLType) where
    SLRefPtr   :: (KnownSize t) => TypedSLExp ('SLTPtr t) -> SLRef t
    SLRefLocal :: (KnownSize t) => Int                    -> SLRef t
instance Show (SLRef t) where
  show = T.unpack . prettyPrintSLRef

data SLStatement where
  SLSInitVar        :: KnownSize t => Int -> TypedSLExp t     -> SLStatement
  SLSSubst          :: KnownSize t => SLRef t -> TypedSLExp t -> SLStatement
  SLSReturn         :: KnownSize t => TypedSLExp t            -> SLStatement
  SLSTailCallReturn :: KnownSize t => SLCall t           -> SLStatement

instance Show SLStatement where
  show = T.unpack . prettyPrintSLStatement

data SLBlock where
    SLBSingle :: SLStatement -> SLBlock
    SLBMulti  :: V.Vector SLBlock -> SLBlock
    SLBCase   :: V.Vector (TypedSLExp 'SLTInt, SLBlock) -> SLBlock -> SLBlock
    SLBWhile  :: TypedSLExp 'SLTInt -> SLBlock -> SLBlock

instance Show SLBlock where
  show = T.unpack . T.intercalate "\n" . V.toList . prettyPrintSLBlock 0

data TypedSLFuncBlock (args :: [SLType]) (ret :: SLType) =
      TSLFuncBlock {
          tslfName     :: TypedSLFuncName args ret
        , tslfBlock    :: SLBlock
      }
      deriving (Show)

unTypedSLFuncBlock :: forall args ret. (KnownNat (SLTSizeOf ('SLTStruct args))) => TypedSLFuncBlock args ret -> SLFuncBlock
unTypedSLFuncBlock (TSLFuncBlock (TypedSLFuncName name) block) =
  SLFuncBlock name ((fromIntegral . natVal) (Proxy :: Proxy (SLTSizeOf ('SLTStruct args)))) block


data SLFuncBlock =
      SLFuncBlock {
          slfName     :: SLFuncName
        , slfArgCount :: Int
        , slfBlock    :: SLBlock
      }
      deriving (Show)

newtype TypedSLFuncName (args :: [SLType]) (ret :: SLType) = TypedSLFuncName SLFuncName
  deriving newtype Show


unTypedSLFuncName :: TypedSLFuncName args ret -> SLFuncName
unTypedSLFuncName (TypedSLFuncName name) = name

data SLFuncName =
        SLFuncMain
      | SLUserFunc Text Text
      deriving (Eq, Ord)

instance Show SLFuncName where
  show = T.unpack . prettyPrintFuncName



type SLProgram =
        M.Map SLFuncName SLFuncBlock

sleSizeOf :: forall t. KnownSize t => TypedSLExp t -> Int
sleSizeOf _ = (fromIntegral . natVal) (Proxy :: Proxy (SLTSizeOf t))

slRefToPtr :: KnownSize t => SLRef t -> TypedSLExp (SLTPtr t)
slRefToPtr ref =
  case ref of
    SLRefLocal i -> SLELocal i
    SLRefPtr   e -> e

prettyPrintFuncName :: SLFuncName -> Text
prettyPrintFuncName name =
  case name of
    SLFuncMain                       -> "#main"
    (SLUserFunc moduleName funcName) -> "#" <> moduleName <> "/" <> funcName

prettyPrintSLRef :: forall t. SLRef t -> Text
prettyPrintSLRef ref =
  case ref of
    SLRefPtr expr -> "*" <> prettyPrintSLExp expr
    SLRefLocal x -> "$L" <> pack (show x)

prettyPrintSLCall :: forall t. SLCall t -> Text
prettyPrintSLCall call =
  case call of
    SLSolidFuncCall funcName args -> prettyPrintFuncName (unTypedSLFuncName funcName) <> prettyPrintSLExp args
    SLFuncRefCall   ref      args -> prettyPrintSLRef    ref                          <> prettyPrintSLExp args
    SLClosureCall   closure       -> prettyPrintSLExp    closure

prettyPrintSLExp :: forall t. TypedSLExp t -> Text
prettyPrintSLExp expr =
  case expr of
    SLEConst (SLVal x) -> pack (show x)

    SLELocal x -> "$L" <> pack (show x)

    SLEArg x -> "$A" <> pack (show x)
    
    SLEPtr expr' -> "*" <> prettyPrintSLRef expr'

    SLEPushCall call -> prettyPrintSLCall call
    
    SLEFuncPtr funcName -> prettyPrintFuncName (unTypedSLFuncName funcName)

    SLEPrim1 prim exp1 ->
      let expText = prettyPrintSLExp exp1
          opText = case prim of
                      SLPrim1Inv   -> "!"
      in  opText <> expText
    
    SLEPrim2 prim exp1 exp2 ->
      let exp1Text = prettyPrintSLExp exp1
          exp2Text = prettyPrintSLExp exp2
          opText = case prim of
                      SLPrim2Add   -> "+" 
                      SLPrim2Sub   -> "-" 
                      SLPrim2Mult  -> "*" 
                      SLPrim2Shift -> "<<"
                      SLPrim2And   -> "&" 
                      SLPrim2Or    -> "|" 
                      SLPrim2Xor   -> "^" 
                      SLPrim2Gt    -> ">" 
                      SLPrim2Lt    -> "<" 
                      SLPrim2Eq    -> "=="
      in "(" <> exp1Text <> " " <> opText <> " " <> exp2Text <> ")"
    
    SLEStructNil -> "()"
    SLEStructCons e1 e2 ->
      let go :: forall ts. TypedSLExp ('SLTStruct ts) -> Text
          go exp =
            case exp of
              SLEStructNil -> ")"
              SLEStructCons exp1 SLEStructNil ->
                let exp1Text = prettyPrintSLExp exp1
                in  exp1Text <> ")"
              SLEStructCons exp1 exp2 ->
                let exp1Text = prettyPrintSLExp exp1
                    exp2Text = go exp2
                in  exp1Text <> ", " <> exp2Text
              otherexp -> ", " <> prettyPrintSLExp otherexp <> ") [ERROR! this should not happen]"
      in "(" <> go (SLEStructCons e1 e2)
    
    SLEUnion exp -> prettyPrintSLExp exp

    SLEDeRef exp -> "*" <> prettyPrintSLExp exp

    SLEPtrShift exp1 exp2 ->
      let exp1Text = prettyPrintSLExp exp1
          exp2Text = prettyPrintSLExp exp2
      in "(" <> exp1Text <> " + " <> exp2Text <> ")"

    SLEStructGet exp p -> prettyPrintSLExp exp <> "." <> pack (show (natVal p))

    SLECast exp -> prettyPrintSLExp exp

prettyPrintSLStatement :: SLStatement -> Text
prettyPrintSLStatement stmt =
  case stmt of
    SLSInitVar varid exp -> "var " <> "$L" <> pack (show varid) <> " = " <> prettyPrintSLExp exp
    SLSSubst ref exp -> prettyPrintSLRef ref <> " = " <> prettyPrintSLExp exp
    SLSReturn exp          -> "return "   <> prettyPrintSLExp exp
    SLSTailCallReturn call -> "tailcall " <> prettyPrintSLCall call


prettyPrintSLBlock :: Int -> SLBlock -> V.Vector Text
prettyPrintSLBlock indent block =
  let indentText = T.replicate indent "  "
  in case block of
      SLBSingle stmt -> V.singleton (indentText <> prettyPrintSLStatement stmt)
      SLBMulti blocks ->
            V.singleton (indentText <> "{")
        <>  (prettyPrintSLBlock (indent + 1) =<< blocks)
        <>  V.singleton (indentText <> "}")
      
      SLBCase cases elseBlock ->
            ((\(exp, body) ->
                V.singleton (indentText <> "when " <> prettyPrintSLExp exp ) <>
                prettyPrintSLBlock indent body
              ) =<< cases)
        <>  (   V.singleton (indentText <> "else") <>
                prettyPrintSLBlock indent elseBlock
              )
        <>      V.singleton ""
      
      SLBWhile cond body ->
          V.singleton (indentText <> "while " <> prettyPrintSLExp cond)
        <> prettyPrintSLBlock indent body
        <>      V.singleton ""

prettyPrintSLProgram :: SLProgram -> Text
prettyPrintSLProgram program =
  T.intercalate "\n" $ 
    (\(name, SLFuncBlock _ args block) -> 
          ("\nfunction " 
              <> prettyPrintFuncName name
              <> "(" <> T.intercalate ", " ((\i -> "$A" <> pack (show i)) <$> [0 .. args]) <> ")")
           : V.toList (prettyPrintSLBlock 0 block)
      ) =<< M.assocs program