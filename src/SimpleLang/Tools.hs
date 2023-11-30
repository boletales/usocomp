{-# LANGUAGE OverloadedStrings #-}

module SimpleLang.Tools (
    SLPos(..)
  , SLLocalPos(..)
  , pushPos
  , popPos
  , slPosAbbrText
  , prettyPrintFuncName
  , prettyPrintSLExp
  , prettyPrintSLRef
  , prettyPrintSLStatement
  , prettyPrintSLBlock
  , prettyPrintSLProgram
  ) where

import SimpleLang.Def
import Data.Vector as V
import Data.Map as M
import Control.Monad.State
import Data.Text as T
import Prelude hiding (exp)

data SLLocalPos =
        SLLPMulti Int
      | SLLPCaseCond Int
      | SLLPCaseBody Int
      | SLLPCaseElseBody
      | SLLPWhileCond
      | SLLPWhileBody
      | SLLPWhileFooter
      deriving (Show, Eq, Ord)

data SLPos = SLPos {
    slpFuncName :: SLFuncName
  , slpLocalPos :: [SLLocalPos]
 } deriving (Show, Eq, Ord)

pushPos :: SLLocalPos -> SLPos -> SLPos
pushPos x (SLPos f xs) = SLPos f (x:xs)

popPos :: SLPos -> SLPos
popPos (SLPos f (_:xs)) = SLPos f xs
popPos (SLPos f [])     = SLPos f []

slPosAbbrText :: SLPos -> Text
slPosAbbrText pos =
  let SLPos f xs = pos
  in (pack . show) f <> "." <> intercalate "." (Prelude.map (pack . show) (Prelude.reverse xs))

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