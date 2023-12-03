{-# LANGUAGE OverloadedStrings #-}

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
  , prettyPrintFuncName
  , prettyPrintSLRef
  , prettyPrintSLExp
  , prettyPrintSLStatement
  , prettyPrintSLBlock
  , prettyPrintSLProgram
)where

import Data.Vector as V
import Data.Map as M
import Data.Text as T

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
    | SLTPtr      SLType
    | SLTStruct  [SLType]
    | SLTUnion   [SLType]
    deriving (Show, Eq)


newtype SLAddr = SLAddr Int deriving (Show, Eq)
newtype SLVal  = SLVal  Int deriving (Show, Eq)

data SLFuncName =
        SLFuncMain
      | SLUserFunc Text Text
      deriving (Eq, Ord)

instance Show SLFuncName where
  show = T.unpack . prettyPrintFuncName

data SLCall =
        SLSolidFuncCall SLFuncName (V.Vector SLExp)
      | SLFuncRefCall   SLRef      (V.Vector SLExp)
      deriving (Eq)

instance Show SLCall where
  show = T.unpack . prettyPrintSLExp . SLEPushCall


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

data SLStruct =
    SLStructNil
  | SLStructCons SLExp SLStruct
  deriving (Eq)

instance Show SLStruct where
  show = T.unpack . prettyPrintSLStruct


data SLExp =
        SLEConst     SLVal
      | SLELocal     Int
      | SLEArg       Int
      | SLEPtr       SLExp
      | SLEPushCall  SLCall
      | SLEFuncPtr   SLFuncName
      | SLEPrim1     SLPrim1 SLExp
      | SLEPrim2     SLPrim2 SLExp SLExp
      | SLEStruct    SLStruct
      | SLEUnion     SLExp
      deriving (Eq)

instance Show SLExp where
  show = T.unpack . prettyPrintSLExp


data SLRef = 
          SLRefPtr   SLExp
        | SLRefLocal Int
      deriving (Eq)

instance Show SLRef where
  show = T.unpack . prettyPrintSLRef

    

data SLStatement =
      --   SLSPrimPush SLExp
      -- | SLSPrimPop
        SLSInitVar Int SLExp
      | SLSSubst   SLRef SLExp
      | SLSReturn  SLExp
      | SLSTailCallReturn SLCall
      deriving (Eq)

instance Show SLStatement where
  show = T.unpack . prettyPrintSLStatement


data SLBlock =
        SLBSingle  SLStatement
      | SLBMulti  (V.Vector SLBlock)
      | SLBCase   (V.Vector (SLExp, SLBlock)) SLBlock
      | SLBWhile   SLExp SLBlock
      deriving (Eq)

instance Show SLBlock where
  show = T.unpack . T.intercalate "\n" . V.toList . prettyPrintSLBlock 0


data SLFuncBlock =
      SLFuncBlock {
          slfName     :: SLFuncName
        , slfArgCount :: Int
        , slfBlock    :: SLBlock
      }
      deriving (Eq)

instance Show SLFuncBlock where
  show = T.unpack . T.intercalate "\n" . V.toList . prettyPrintSLBlock 0 . slfBlock

type SLProgram =
        M.Map SLFuncName SLFuncBlock


prettyPrintFuncName :: SLFuncName -> Text
prettyPrintFuncName name =
  case name of
    SLFuncMain -> "#main"
    SLUserFunc moduleName funcName -> "#" <> moduleName <> "." <> funcName

prettyPrintSLRef :: SLRef -> Text
prettyPrintSLRef ref =
  case ref of
    SLRefPtr exp -> "*" <> prettyPrintSLExp exp
    SLRefLocal x -> "$L" <> pack (show x)

prettyPrintSLStruct :: SLStruct -> Text
prettyPrintSLStruct struct =
  case struct of
    SLStructNil -> "{}"
    SLStructCons _ _ -> 
      let go s =
            case s of
              SLStructNil           -> ""
              SLStructCons x rest -> prettyPrintSLExp x <> ", " <> go rest
      in "{" <> go struct <> "}"

prettyPrintSLExp :: SLExp -> Text
prettyPrintSLExp expr =
  case expr of
    SLEConst (SLVal x) -> pack (show x)

    SLELocal x -> "$L" <> pack (show x)

    SLEArg x -> "$A" <> pack (show x)
    
    SLEPtr exp -> "*" <> prettyPrintSLExp exp

    SLEPushCall call ->
      case call of
        SLSolidFuncCall funcName args -> prettyPrintFuncName funcName <> "(" <> intercalate ", " (V.toList $ prettyPrintSLExp <$> args) <> ")"
        SLFuncRefCall   ref      args -> prettyPrintSLRef    ref      <> "(" <> intercalate ", " (V.toList $ prettyPrintSLExp <$> args) <> ")"
    
    SLEFuncPtr funcName -> prettyPrintFuncName funcName

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

prettyPrintSLStatement :: SLStatement -> Text
prettyPrintSLStatement stmt =
  case stmt of
    SLSInitVar varid exp -> "var " <> "$L" <> pack (show varid) <> " = " <> prettyPrintSLExp exp
    SLSSubst ref exp -> prettyPrintSLRef ref <> " = " <> prettyPrintSLExp exp
    SLSReturn exp -> "return " <> prettyPrintSLExp exp
    SLSTailCallReturn call ->
      case call of
        SLSolidFuncCall funcName args -> "tailcall " <> prettyPrintFuncName funcName <> "(" <> intercalate ", " (V.toList $ prettyPrintSLExp <$> args) <> ")"
        SLFuncRefCall   ref      args -> "tailcall " <> prettyPrintSLRef ref         <> "(" <> intercalate ", " (V.toList $ prettyPrintSLExp <$> args) <> ")"


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