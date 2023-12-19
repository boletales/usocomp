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
    , SLExp (..)
    , SLRef (..)
    , SLStatement (..)
    , SLBlock (..)
    , SLFuncBlock (..)
    , SLProgram
    , SLType (..)
    , sleSizeOf
    , sltSizeOf
    , sleGetOffset
    , slRefToPtr
    , prettyPrintFuncName
    , prettyPrintSLExp
    , prettyPrintSLRef
    , prettyPrintSLStatement
    , prettyPrintSLBlock
    , prettyPrintSLProgram
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
import qualified Data.List as L

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

newtype SLAddr = SLAddr Int deriving (Show, Eq)
newtype SLVal  = SLVal  Int deriving (Show, Eq)

data SLCall =
    SLSolidFuncCall SLFuncName SLExp
  | SLFuncRefCall   SLRef      SLExp
  | SLClosureCall   SLExp           

deriving instance Show SLCall

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

data SLExp =
      SLEConst      SLVal                    
    | SLELocal      SLType Int                      
    | SLEArg        SLType Int                      
    | SLEPtr        SLType SLRef
    | SLEPushCall   SLCall
    | SLEFuncPtr    SLFuncName
    | SLEPrim1      SLPrim1 SLExp         
    | SLEPrim2      SLPrim2 SLExp SLExp
    | SLEStructNil                           
    | SLEStructCons SLExp  SLExp          
    | SLEUnion      SLType SLExp          
    | SLEDeRef      SLExp                    
    | SLEPtrShift   SLExp  SLExp           
    | SLEStructGet  SLExp  Int             
    | SLECast       SLType SLExp          

instance Show SLExp where
  show = T.unpack . prettyPrintSLExp

data SLRef =
      SLRefPtr   SLType SLExp
    | SLRefLocal SLType Int
instance Show SLRef where
  show = T.unpack . prettyPrintSLRef

data SLStatement =
    SLSInitVar        Int    SLExp
  | SLSSubst          SLRef  SLExp
  | SLSReturn         SLExp       
  | SLSTailCallReturn SLCall      

instance Show SLStatement where
  show = T.unpack . prettyPrintSLStatement

data SLBlock =
      SLBSingle SLStatement
    | SLBMulti  (V.Vector SLBlock)
    | SLBCase   (V.Vector (SLExp, SLBlock)) SLBlock
    | SLBWhile  SLExp SLBlock

instance Show SLBlock where
  show = T.unpack . T.intercalate "\n" . V.toList . prettyPrintSLBlock 0


data SLFuncBlock =
      SLFuncBlock {
          slfName     :: SLFuncName
        , slfArgCount :: Int
        , slfBlock    :: SLBlock
      }
      deriving (Show)

data SLFuncName =
        SLFuncMain [SLType] SLType
      | SLUserFunc [SLType] SLType Text Text
      deriving (Eq)

instance Ord SLFuncName where
  compare n1 n2 =
    case (n1, n2) of
      (SLFuncMain _ _, SLFuncMain _ _) -> EQ
      (SLFuncMain _ _, SLUserFunc {}) -> LT
      (SLUserFunc {}, SLFuncMain _ _) -> GT
      (SLUserFunc _ _ m1 f1, SLUserFunc _ _ m2 f2) -> compare (m1, f1) (m2, f2)

instance Show SLFuncName where
  show = T.unpack . prettyPrintFuncName

type SLProgram =
        M.Map SLFuncName SLFuncBlock

slFuncTypeOf :: SLFuncName -> SLType
slFuncTypeOf funcName =
  case funcName of
    SLFuncMain args ret     -> SLTFuncPtr args ret
    SLUserFunc args ret _ _ -> SLTFuncPtr args ret

slCallTypeOf :: SLCall -> Either Text SLType
slCallTypeOf call =
  case call of
    SLSolidFuncCall funcName args -> do
      let tfunc = slFuncTypeOf funcName
      targs <- sleTypeOf args
      largs <- case targs of
                 SLTStruct ts -> pure ts
                 other        -> Left $ "Error! (type check of SLang): arg#1 of SLFuncRefCall must be SLTStruct ts, not " <> (show >>> T.pack) other
      case tfunc of
        SLTFuncPtr args' ret
          | args' == largs -> pure ret
          | otherwise      -> Left $ "Error! (type check of SLang): arg#0 of SLFuncRefCall must be SLTFuncPtr args ret, not " <> (show >>> T.pack) (SLTFuncPtr args' ret)
        other              -> Left $ "Error! (type check of SLang): arg#0 of SLFuncRefCall must be SLTFuncPtr args ret, not " <> (show >>> T.pack) other
    
    SLFuncRefCall   ref      args -> do
      tfunc <- sleTypeOf (slRefToPtr ref)
      targs <- sleTypeOf args
      largs <- case targs of
                 SLTStruct ts -> pure ts
                 other        -> Left $ "Error! (type check of SLang): arg#1 of SLFuncRefCall must be SLTStruct ts, not " <> (show >>> T.pack) other
      case tfunc of
        SLTFuncPtr args' ret
          | args' == largs -> pure ret
          | otherwise      -> Left $ "Error! (type check of SLang): arg#0 of SLFuncRefCall must be SLTFuncPtr args ret, not " <> (show >>> T.pack) (SLTFuncPtr args' ret)
        other              -> Left $ "Error! (type check of SLang): arg#0 of SLFuncRefCall must be SLTFuncPtr args ret, not " <> (show >>> T.pack) other
    
    SLClosureCall   closure       ->
      case sleTypeOf closure of
        Right (SLTStruct (SLTFuncPtr args ret : args'))
          | args == args' -> pure ret
          | otherwise     -> Left $ "Error! (type check of SLang): arg#0 of SLClosureCall must be SLTStruct (SLTFuncPtr args ret : args), not " <> (show >>> T.pack) (SLTStruct (SLTFuncPtr args ret : args'))
        Right other       -> Left $ "Error! (type check of SLang): arg#0 of SLClosureCall must be SLTStruct (SLTFuncPtr args ret : args), not " <> (show >>> T.pack) other
        err               -> err

sleTypeOf :: SLExp -> Either Text SLType
sleTypeOf expr =
  case expr of
    SLEConst _          -> pure SLTInt
    SLELocal t _        -> pure t
    SLEArg t _          -> pure t
    SLEPtr t _          -> pure (SLTPtr t)
    SLEPushCall call    -> slCallTypeOf call
    SLEFuncPtr funcName -> pure (slFuncTypeOf funcName)
    SLEPrim1 _ _        -> pure SLTInt
    SLEPrim2 {}         -> pure SLTInt
    SLEStructNil        -> pure (SLTStruct [])
    SLEUnion t _        -> pure t
    SLEPtrShift exp1 _  -> sleTypeOf exp1

    SLECast t exp1      ->
      case sleTypeOf exp1 of
        Right t'
          | sltSizeOf t == sltSizeOf t' -> pure t
          | otherwise                   -> Left $ "Error! (type check of SLang): arg#0 of SLECast must be the same size as arg#1, not " <> (show >>> T.pack) (sleSizeOf exp1) <> " and " <> (show >>> T.pack) (sltSizeOf t)
        err                             -> err

    SLEStructCons exp1 exp2 ->
      case sleTypeOf exp1 of
        Right (SLTStruct ts) -> ((: ts) >>> SLTStruct) <$> sleTypeOf exp2
        Right other          -> Left $ "Error! (type check of SLang): arg#1 of SLEStructCons must be SLTStruct ts, not " <> (show >>> T.pack) other
        err                  -> err
      
    SLEDeRef exp1 ->
      case sleTypeOf exp1 of
        Right (SLTPtr t) -> pure t
        Right other      -> Left $ "Error! (type check of SLang): arg#0 of SLEDeRef must be SLTPtr t, not " <> (show >>> T.pack) other
        err              -> err

    SLEStructGet exp1 i ->
      case sleTypeOf exp1 of
        Right (SLTStruct ts)
          | 0 < i && i < L.length ts -> pure (ts !! i)
          | otherwise                -> Left $ "Error! (type check of SLang): arg#1 SLEStructGet of must be in [0, " <> (show >>> T.pack) (L.length ts) <> "), not " <> (show >>> T.pack) i
        Right other -> Left $ "Error! (type check of SLang): arg#1 of SLEStructGet must be SLTStruct ts, not " <> (show >>> T.pack) other
        err         -> err

sltSizeOf :: SLType -> Int
sltSizeOf t =
  case t of
    SLTUnit        -> 0
    SLTInt         -> 1
    SLTFuncPtr _ _ -> 1
    SLTPtr _       -> 1
    SLTStruct ts   -> L.sum     (sltSizeOf <$> ts)
    SLTUnion  ts   -> L.maximum (sltSizeOf <$> ts)

sleSizeOf :: SLExp -> Either Text Int
sleSizeOf = sleTypeOf >>> fmap sltSizeOf

slRefToPtr :: SLRef -> SLExp
slRefToPtr ref =
  case ref of
    SLRefLocal t i -> SLELocal t i
    SLRefPtr   _ e -> e

sleGetOffset :: [SLType] -> Int -> Either Text Int
sleGetOffset ts i =
  if i < 0 || L.length ts <= i
    then Left $ "Error! (type check of SLang): arg#1 of sleGetOffset must be in [0, " <> (show >>> T.pack) (L.length ts) <> "), not " <> (show >>> T.pack) i
    else Right $ (fmap sltSizeOf >>> L.sum) (L.take i ts)

prettyPrintFuncName :: SLFuncName -> Text
prettyPrintFuncName name =
  case name of
    SLFuncMain _ _                       -> "#main"
    (SLUserFunc _ _ moduleName funcName) -> "#" <> moduleName <> "/" <> funcName

prettyPrintSLRef :: SLRef -> Text
prettyPrintSLRef ref =
  case ref of
    SLRefPtr _ expr -> "*" <> prettyPrintSLExp expr
    SLRefLocal _ x -> "$L" <> pack (show x)

prettyPrintSLCall :: SLCall -> Text
prettyPrintSLCall call =
  case call of
    SLSolidFuncCall funcName args -> prettyPrintFuncName funcName <> prettyPrintSLExp args
    SLFuncRefCall   ref      args -> prettyPrintSLRef    ref      <> prettyPrintSLExp args
    SLClosureCall   closure       -> prettyPrintSLExp    closure

prettyPrintSLExp :: SLExp -> Text
prettyPrintSLExp expr =
  case expr of
    SLEConst (SLVal x) -> pack (show x)

    SLELocal t x -> "$L" <> pack (show x)

    SLEArg t x -> "$A" <> pack (show x)
    
    SLEPtr t expr' -> "*" <> prettyPrintSLRef expr'

    SLEPushCall call -> prettyPrintSLCall call
    
    SLEFuncPtr funcName -> prettyPrintFuncName funcName

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
      let go :: forall ts. SLExp -> Text
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
    
    SLEUnion t exp -> prettyPrintSLExp exp

    SLEDeRef exp -> "*" <> prettyPrintSLExp exp

    SLEPtrShift exp1 exp2 ->
      let exp1Text = prettyPrintSLExp exp1
          exp2Text = prettyPrintSLExp exp2
      in "(" <> exp1Text <> " + " <> exp2Text <> ")"

    SLEStructGet exp p -> prettyPrintSLExp exp <> "." <> pack (show p)

    SLECast t exp -> prettyPrintSLExp exp

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