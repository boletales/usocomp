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
    , SLFuncSignature (..)
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
    , SLTypeError (..)
    , sleTypeOf
    , sleSizeOf
    , sltSizeOf
    , sleGetOffset
    , slRefToPtr
    , slStructConcat
    , prettyPrintSLFuncName
    , prettyPrintSLExp
    , prettyPrintSLRef
    , prettyPrintSLStatement
    , prettyPrintSLBlock
    , prettyPrintSLProgram
    , prettyPrintSLTypeError
    , prettyPrintSLCall
    , prettyPrintSLType
    , prettyPrintSLFuncSignature
  ) where

import Data.Vector as V
import Data.Map as M
import Data.Text as T
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
    deriving (Eq, Ord)

instance Show SLType where
  show = T.unpack . prettyPrintSLType

prettyPrintSLType :: SLType -> Text
prettyPrintSLType t =
  case t of
    SLTUnit        -> "unit"
    SLTInt         -> "int"
    SLTFuncPtr args ret -> "(" <> T.intercalate ", " ((show >>> T.pack) <$> args) <> ") -> " <> (show >>> T.pack) ret
    SLTPtr t'      -> "*" <> prettyPrintSLType t'
    SLTStruct ts   -> "(" <> T.intercalate ", " (prettyPrintSLType <$> ts) <> ")"
    SLTUnion  ts   -> "(" <> T.intercalate " | " (prettyPrintSLType <$> ts) <> ")"

newtype SLAddr = SLAddr Int deriving (Show, Eq, Ord)
newtype SLVal  = SLVal  Int deriving (Show, Eq, Ord)

data SLCall =
    SLSolidFuncCall SLFuncSignature SLExp
  | SLFuncRefCall   SLRef      SLExp
  | SLClosureCall   SLExp
  deriving (Eq, Ord)

deriving instance Show SLCall

data SLPrim1 =
        SLPrim1Inv
      deriving (Show, Eq, Ord)

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
      deriving (Show, Eq, Ord)

data SLExp =
      SLEConst      SLVal
    | SLELocal      SLType Int
    | SLEArg        SLType Int
    | SLEPtr        SLRef
    | SLEPushCall   SLCall
    | SLEFuncPtr    SLFuncSignature
    | SLEPrim1      SLPrim1 SLExp
    | SLEPrim2      SLPrim2 SLExp SLExp
    | SLEStructNil
    | SLEStructCons SLExp  SLExp
    | SLEUnion      SLType SLExp
    | SLEDeRef      SLExp
    | SLEPtrShift   SLExp  SLExp
    | SLEStructGet  SLExp  Int
    | SLECast       SLType SLExp
  deriving (Eq, Ord)

instance Show SLExp where
  show = T.unpack . prettyPrintSLExp

data SLRef =
      SLRefPtr   SLType SLExp
    | SLRefLocal SLType Int
  deriving (Eq, Ord)
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
          slfSignature :: SLFuncSignature
        , slfBlock     :: SLBlock
      }
      deriving (Show)

data SLFuncSignature = SLFuncSignature{
      slfsName :: SLFuncName
    , slfsArgs :: [SLType]
    , slfsRet  :: SLType
  }
      deriving (Eq, Ord)

data SLFuncName =
        SLFuncMain
      | SLUserFunc Text Text
      deriving (Eq, Ord)

instance Show SLFuncName where
  show = T.unpack . prettyPrintSLFuncName

instance Show SLFuncSignature where
  show = T.unpack . prettyPrintSLFuncSignature

type SLProgram =
        M.Map SLFuncName SLFuncBlock

data SLTypeError =
    SLTESolidFuncCall1ArgMustBeStruct SLCall
  | SLTESolidFuncCall1ArgMismatch     SLCall SLType SLType
  | SLTEFuncRefCall0NotFuncPtr        SLCall SLType
  | SLTEFuncRefCall1ArgMustBeStruct   SLCall
  | SLTEFuncRefCall1ArgMismatch       SLCall SLType SLType
  | SLTEClosureCall0MustBeStruct      SLCall
  | SLTEClosureCall0TypeMismatch      SLCall SLType SLType
  | SLTECast1SizeMismatch             SLType SLType
  | SLTEDeRef0MustBePtr               SLExp
  | SLTEPtrShift0MustBePtr            SLExp
  | SLTEPtrShift1MustBeInt            SLExp
  | SLTEStructGet0MustBeStruct        SLExp
  | SLTEStructGet1OutOfRange          SLExp Int
  | SLTEStructCons0MustBeStruct       SLExp
  | SLTEStructCons1MustBeStruct       SLExp
  | SLTEgetOffset1OutOfRange          SLExp Int

instance Show SLTypeError where
  show = T.unpack . prettyPrintSLTypeError

prettyPrintSLTypeError :: SLTypeError -> Text
prettyPrintSLTypeError err =
  case err of
    SLTESolidFuncCall1ArgMustBeStruct call ->
      "Error! (type check of SLang): arg#1 of SLSolidFuncCall must be SLTStruct ts, not " <> (show >>> T.pack) call
    SLTESolidFuncCall1ArgMismatch call expected actual ->
      "Error! (type check of SLang): arg#1 of SLSolidFuncCall does not match! expected:" <> (show >>> T.pack) expected <> " actual:" <> (show >>> T.pack) actual <> " in " <> (show >>> T.pack) call
    SLTEFuncRefCall0NotFuncPtr call actual ->
      "Error! (type check of SLang): arg#0 of SLFuncRefCall must be SLTFuncPtr args ret, not " <> (show >>> T.pack) actual <> " in " <> (show >>> T.pack) call
    SLTEFuncRefCall1ArgMustBeStruct call ->
      "Error! (type check of SLang): arg#1 of SLFuncRefCall must be SLTStruct ts, not " <> (show >>> T.pack) call
    SLTEFuncRefCall1ArgMismatch call expected actual ->
      "Error! (type check of SLang): arg#1 of SLFuncRefCall does not match! expected:" <> (show >>> T.pack) expected <> " actual:" <> (show >>> T.pack) actual <> " in " <> (show >>> T.pack) call
    SLTEClosureCall0MustBeStruct call ->
      "Error! (type check of SLang): arg#0 of SLClosureCall must be SLTStruct (SLTFuncPtr args ret : args), not " <> (show >>> T.pack) call
    SLTEClosureCall0TypeMismatch call expected actual ->
      "Error! (type check of SLang): arg#0 of SLClosureCall does not match! expected:" <> (show >>> T.pack) expected <> " actual:" <> (show >>> T.pack) actual <> " in " <> (show >>> T.pack) call
    SLTECast1SizeMismatch expected actual ->
      "Error! (type check of SLang): arg#1 of SLECast must be the same size as arg#0, not " <> (show >>> T.pack) expected <> " and " <> (show >>> T.pack) actual
    SLTEDeRef0MustBePtr exp ->
      "Error! (type check of SLang): arg#0 of SLEDeRef must be SLTPtr t, not " <> (show >>> T.pack) exp
    SLTEPtrShift0MustBePtr exp ->
      "Error! (type check of SLang): arg#0 of SLEPtrShift must be SLTPtr t, not " <> (show >>> T.pack) exp
    SLTEPtrShift1MustBeInt exp ->
      "Error! (type check of SLang): arg#1 of SLEPtrShift must be SLTInt, not " <> (show >>> T.pack) exp
    SLTEStructGet0MustBeStruct exp ->
      "Error! (type check of SLang): arg#0 of SLEStructGet must be SLTStruct ts, not " <> (show >>> T.pack) exp
    SLTEStructGet1OutOfRange exp i ->
      "Error! (type check of SLang): arg#1 SLEStructGet of must be in [0, " <> (show >>> T.pack) i <> "), not " <> (show >>> T.pack) exp
    SLTEStructCons0MustBeStruct exp ->
      "Error! (type check of SLang): arg#0 of SLEStructCons must be SLTStruct ts, not " <> (show >>> T.pack) exp
    SLTEStructCons1MustBeStruct exp ->
      "Error! (type check of SLang): arg#1 of SLEStructCons must be SLTStruct ts, not " <> (show >>> T.pack) exp
    SLTEgetOffset1OutOfRange exp i -> 
      "Error! (type check of SLang): arg#1 of sleGetOffset must be in [0, " <> (show >>> T.pack) i <> "), not " <> (show >>> T.pack) exp

slFuncTypeOf :: SLFuncSignature -> SLType
slFuncTypeOf funcName =
  case funcName of
    SLFuncSignature _ args ret -> SLTFuncPtr args ret

slCallTypeOf :: SLCall -> Either SLTypeError SLType
slCallTypeOf call =
  case call of
    SLSolidFuncCall funcName args -> do
      let tfunc = slFuncTypeOf funcName
      targs <- sleTypeOf args
      largs <- case targs of
                 SLTStruct ts -> pure ts
                 _            -> Left $ SLTESolidFuncCall1ArgMustBeStruct call
      case tfunc of
        SLTFuncPtr args' ret
          | args' == largs -> pure ret
          | otherwise      -> Left $ SLTESolidFuncCall1ArgMismatch call (SLTStruct args') (SLTStruct largs)
        other              -> Left $ SLTESolidFuncCall1ArgMismatch call (SLTStruct []) other
    
    SLFuncRefCall   ref      args -> do
      tfunc <- sleTypeOf (slRefToPtr ref)
      targs <- sleTypeOf args
      largs <- case targs of
                 SLTStruct ts -> pure ts
                 _            -> Left $ SLTEFuncRefCall1ArgMustBeStruct call
      case tfunc of
        SLTFuncPtr args' ret
          | args' == largs -> pure ret
          | otherwise      -> Left $ SLTEFuncRefCall1ArgMismatch call (SLTStruct args') (SLTStruct largs)
        other              -> Left $ SLTEFuncRefCall0NotFuncPtr call other

    SLClosureCall   closure       ->
      case sleTypeOf closure of
        Right (SLTStruct (SLTFuncPtr args ret : args'))
          | args == args' -> pure ret
          | otherwise     -> Left $ SLTEClosureCall0TypeMismatch call (SLTStruct args) (SLTStruct args')
        Right _           -> Left $ SLTEClosureCall0MustBeStruct call
        err               -> err

sleTypeOf :: SLExp -> Either SLTypeError SLType
sleTypeOf expr =
  case expr of
    SLEConst _          -> pure SLTInt
    SLELocal t _        -> pure t
    SLEArg t _          -> pure t
    SLEPushCall call    -> slCallTypeOf call
    SLEFuncPtr funcName -> pure (slFuncTypeOf funcName)
    SLEPrim1 _ _        -> pure SLTInt
    SLEPrim2 {}         -> pure SLTInt
    SLEStructNil        -> pure (SLTStruct [])
    SLEUnion t _        -> pure t
    SLEPtrShift exp1 _  -> sleTypeOf exp1

    SLEPtr p            ->
      case p of
        SLRefPtr t _   -> pure (SLTPtr t)
        SLRefLocal t _ -> pure (SLTPtr t)

    SLECast t exp1      ->
      case sleTypeOf exp1 of
        Right t'
          | sltSizeOf t == sltSizeOf t' -> pure t
          | otherwise                   -> Left $ SLTECast1SizeMismatch t t'
        err                             -> err

    SLEStructCons exp1 exp2 ->
      case sleTypeOf exp2 of
        Right (SLTStruct ts) -> ((: ts) >>> SLTStruct) <$> sleTypeOf exp1
        Right _              -> Left $ SLTEStructCons1MustBeStruct exp2
        err                  -> err

    SLEDeRef exp1 ->
      case sleTypeOf exp1 of
        Right (SLTPtr t) -> pure t
        Right _ -> Left $ SLTEDeRef0MustBePtr exp1
        err     -> err

    SLEStructGet exp1 i ->
      case sleTypeOf exp1 of
        Right (SLTStruct ts)
          | 0 <= i && i < L.length ts -> pure (ts !! i)
          | otherwise                -> Left $ SLTEStructGet1OutOfRange exp1 i
        Right _ -> Left $ SLTEStructGet0MustBeStruct exp1
        err     -> err

sltSizeOf :: SLType -> Int
sltSizeOf t =
  case t of
    SLTUnit        -> 0
    SLTInt         -> 1
    SLTFuncPtr _ _ -> 1
    SLTPtr _       -> 1
    SLTStruct ts   -> L.sum     (sltSizeOf <$> ts)
    SLTUnion  ts   -> L.maximum (sltSizeOf <$> ts)

sleSizeOf :: SLExp -> Either SLTypeError Int
sleSizeOf = sleTypeOf >>> fmap sltSizeOf

slRefToPtr :: SLRef -> SLExp
slRefToPtr ref =
  case ref of
    SLRefLocal t i -> SLELocal t i
    SLRefPtr   _ e -> e

slStructConcat :: SLExp -> SLExp -> Either SLTypeError SLExp
slStructConcat e1 e2 =
  let isStruct :: SLExp -> Bool
      isStruct e = case e of
        SLEStructNil -> True
        SLEStructCons _ _ -> True
        _ -> False
      
  in  do
    if isStruct e1 && isStruct e2
      then case (e1, e2) of
            (SLEStructNil, _) -> pure e2
            (_, SLEStructNil) -> pure e1
            (SLEStructCons x xs, _) -> SLEStructCons x <$> slStructConcat xs e2
            _ -> Left $ SLTEStructCons0MustBeStruct e1

      else Left $ SLTEStructCons0MustBeStruct e1

-- >>> slStructConcat (SLEConst (SLVal 1) `SLEStructCons` (SLEConst (SLVal 2) `SLEStructCons` (SLEConst (SLVal 3) `SLEStructCons` SLEStructNil))) (SLEConst (SLVal 4) `SLEStructCons` (SLEConst (SLVal 5) `SLEStructCons` (SLEConst (SLVal 6) `SLEStructCons` SLEStructNil)))
-- Right (1, 2, 3, 4, 5, 6)

sleGetOffset :: SLExp -> Int -> Either SLTypeError Int
sleGetOffset expr i =
  case sleTypeOf expr of
    Left err -> Left err
    Right (SLTStruct ts) ->
      if i < 0 || L.length ts <= i
        then Left $ SLTEgetOffset1OutOfRange expr i
        else Right $ (fmap sltSizeOf >>> L.sum) (L.take i ts)
    _ -> Left $ SLTEgetOffset1OutOfRange expr i

prettyPrintSLFuncName :: SLFuncName -> Text
prettyPrintSLFuncName name =
  case name of
    SLFuncMain                       -> "#main"
    (SLUserFunc moduleName funcName) -> "#" <> moduleName <> "/" <> funcName

prettyPrintSLFuncSignature :: SLFuncSignature -> Text
prettyPrintSLFuncSignature (SLFuncSignature name args ret) =
  prettyPrintSLFuncName name <> " :: " <> "(" <> T.intercalate ", " ((show >>> T.pack) <$> args) <> ") -> " <> (show >>> T.pack) ret

prettyPrintSLRef :: SLRef -> Text
prettyPrintSLRef ref =
  case ref of
    SLRefPtr _ expr -> "*" <> prettyPrintSLExp expr
    SLRefLocal _ x -> "$L" <> pack (show x)

prettyPrintSLCall :: SLCall -> Text
prettyPrintSLCall call =
  case call of
    SLSolidFuncCall funcSig  args -> prettyPrintSLFuncName (slfsName funcSig) <> prettyPrintSLExp args
    SLFuncRefCall   ref      args -> prettyPrintSLRef    ref            <> prettyPrintSLExp args
    SLClosureCall   closure       -> prettyPrintSLExp    closure <> "()"

prettyPrintSLExp :: SLExp -> Text
prettyPrintSLExp expr =
  case expr of
    SLEConst (SLVal x) -> pack (show x)

    SLELocal _ x -> "$L" <> pack (show x)

    SLEArg _ x -> "$A" <> pack (show x)

    SLEPtr expr' -> "*" <> prettyPrintSLRef expr'

    SLEPushCall call -> prettyPrintSLCall call

    SLEFuncPtr funcSig -> prettyPrintSLFuncName (slfsName funcSig)

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
      let go :: SLExp -> Text
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

    SLEUnion _ exp -> prettyPrintSLExp exp

    SLEDeRef exp -> "*" <> prettyPrintSLExp exp

    SLEPtrShift exp1 exp2 ->
      let exp1Text = prettyPrintSLExp exp1
          exp2Text = prettyPrintSLExp exp2
      in "(" <> exp1Text <> " + " <> exp2Text <> ")"

    SLEStructGet exp p -> prettyPrintSLExp exp <> "." <> pack (show p)

    SLECast _ exp -> prettyPrintSLExp exp

prettyPrintSLStatement :: SLStatement -> Text
prettyPrintSLStatement stmt =
  case stmt of
    SLSInitVar varid exp -> either (\err -> "[" <> prettyPrintSLTypeError err <> "]") prettyPrintSLType (sleTypeOf exp) <> " $L" <> pack (show varid) <> " = " <> prettyPrintSLExp exp
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
    (\(name, SLFuncBlock sig block) ->
          ("\nfunction "
              <> prettyPrintSLFuncName name
              <> "(" <> T.intercalate ", " ((\(t, i) -> pack (show t) <> " $A" <> pack (show i)) <$> L.zip (slfsArgs sig) [(0::Int)..]) <> ")" <> " -> " <> (show >>> T.pack) (slfsRet sig))
           : V.toList (prettyPrintSLBlock 0 block)
      ) =<< M.assocs program
