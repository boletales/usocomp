{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
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
    , SLExp (..)
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
import Data.Text
import GHC.TypeNats
import Data.Type.Bool
import Data.Proxy
import Data.Text as T
import Data.Kind

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

newtype SLAddr = SLAddr Int deriving (Show, Eq)
newtype SLVal  = SLVal  Int deriving (Show, Eq)

data SLCall (t :: SLType) where
    SLSolidFuncCall :: TypedSLFuncName ts t     -> SLExp ('SLTStruct ts) -> SLCall t
    SLFuncRefCall   :: SLRef ('SLTFuncPtr ts t) -> SLExp ('SLTStruct ts) -> SLCall t

class SLCallable (args :: [SLType]) (ret :: SLType) (t :: Type) where
  slCall :: t -> SLExp ('SLTStruct args) -> SLCall ret

instance SLCallable args ret (TypedSLFuncName args ret) where
  slCall = SLSolidFuncCall

instance SLCallable args ret (SLRef ('SLTFuncPtr args ret)) where
  slCall = SLFuncRefCall

deriving instance Show (SLCall t)

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

data SLExp (t :: SLType) where
    SLEConst      :: SLVal                                     -> SLExp 'SLTInt
    SLELocal      :: Int                                       -> SLExp t
    SLEArg        :: Int                                       -> SLExp t
    SLEPtr        :: SLRef t                                   -> SLExp ('SLTPtr t)
    SLEPushCall   :: SLCall t                                  -> SLExp t
    SLEFuncPtr    :: TypedSLFuncName args ret                  -> SLExp ('SLTFuncPtr args ret)
    SLEPrim1      :: SLPrim1 -> SLExp 'SLTInt                  -> SLExp 'SLTInt
    SLEPrim2      :: SLPrim2 -> SLExp 'SLTInt -> SLExp 'SLTInt -> SLExp 'SLTInt
    SLEStructNil  ::                                              SLExp ('SLTStruct '[])
    SLEStructCons :: SLExp t -> SLExp ('SLTStruct ts)          -> SLExp ('SLTStruct (t:ts)) 
    SLEUnion      :: Member t ts => SLExp t                    -> SLExp ('SLTUnion     ts ) 

instance Show (SLExp t) where
  show = T.unpack . prettyPrintSLExp

data SLRef (t :: SLType) = 
          SLRefPtr   (SLExp (SLTPtr t))
        | SLRefLocal Int
instance Show (SLRef t) where
  show = T.unpack . prettyPrintSLRef

data SLStatement where
  SLSInitVar        :: Int -> SLExp t     -> SLStatement
  SLSSubst          :: SLRef t -> SLExp t -> SLStatement
  SLSReturn         :: SLExp t            -> SLStatement
  SLSTailCallReturn :: SLCall t           -> SLStatement

instance Show SLStatement where
  show = T.unpack . prettyPrintSLStatement

data SLBlock where
    SLBSingle :: SLStatement -> SLBlock
    SLBMulti  :: V.Vector SLBlock -> SLBlock
    SLBCase   :: V.Vector (SLExp 'SLTInt, SLBlock) -> SLBlock -> SLBlock
    SLBWhile  :: SLExp 'SLTInt -> SLBlock -> SLBlock

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




prettyPrintFuncName :: SLFuncName -> Text
prettyPrintFuncName name =
  case name of
    SLFuncMain                       -> "#main"
    (SLUserFunc moduleName funcName) -> "#" <> moduleName <> "." <> funcName

prettyPrintSLRef :: forall t. SLRef t -> Text
prettyPrintSLRef ref =
  case ref of
    SLRefPtr exp -> "*" <> prettyPrintSLExp exp
    SLRefLocal x -> "$L" <> pack (show x)

prettyPrintSLCall :: forall t. SLCall t -> Text
prettyPrintSLCall call =
  case call of
    SLSolidFuncCall funcName args -> prettyPrintFuncName (unTypedSLFuncName funcName) <> prettyPrintSLExp args <> ")"
    SLFuncRefCall   ref      args -> prettyPrintSLRef    ref                          <> prettyPrintSLExp args <> ")"

prettyPrintSLExp :: forall t. SLExp t -> Text
prettyPrintSLExp expr =
  case expr of
    SLEConst (SLVal x) -> pack (show x)

    SLELocal x -> "$L" <> pack (show x)

    SLEArg x -> "$A" <> pack (show x)
    
    SLEPtr exp -> "*" <> prettyPrintSLRef exp

    SLEPushCall call -> prettyPrintSLCall call
    
    SLEFuncPtr funcName -> prettyPrintFuncName (unTypedSLFuncName funcName)

    SLEPrim1 prim exp ->
      let expText = prettyPrintSLExp exp
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
      let go :: forall ts. SLExp ('SLTStruct ts) -> Text
          go exp =
            case exp of
              SLEStructNil -> ")"
              SLEStructCons exp1 exp2 ->
                let exp1Text = prettyPrintSLExp exp1
                    exp2Text = go exp2
                in  exp1Text <> ", " <> exp2Text <> ")"
              otherexp -> ", " <> prettyPrintSLExp otherexp <> ") [ERROR! this should not happen]"
      in "(" <> go (SLEStructCons e1 e2)
    
    SLEUnion exp -> prettyPrintSLExp exp

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
              <> " (" <> T.intercalate ", " ((\i -> "$A" <> pack (show i)) <$> [0 .. args]) <> ")")
           : V.toList (prettyPrintSLBlock 0 block)
      ) =<< M.assocs program