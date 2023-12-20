{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
module SimpleLang.FromFuncLang.Test where

import SimpleLang.FromFuncLang
import FuncLang.Def
import FuncLang.Tools.Manual
import Data.Text as T

type FLExp' = FLExp Text

captureTest = runFLM $ do
  --x <- flmDecl "x" (flmLam "x1" (\(x1 :: FLExp Text FLTInt) -> flmLam "x2" (\(x2 :: FLExp Text FLTInt) -> x1)))
  z <- flmDecl "z" (flmLam "x1" (\(x1 :: FLExp Text (FLTLambda FLTInt FLTInt)) -> flmLam "x2" (\(x2 :: FLExp Text FLTInt) -> flmApp x1 x2)))
  {-
  y <- flmDecl "y" (
      flmLam "ext1" (\(ext1 :: FLExp Text (FLTLambda FLTInt FLTInt)) ->
          flmLam "ext2" (\ext2 ->
              flmApp (flmApp (
                flmLam "x1" (\x1 -> flmLam "x2" (\x2 -> flmApp ext1 ext2))
              ) ext1) ext2
            )))
  -}
  pure ()

-- >>> error $ T.unpack $ either id prettyPrintFLCProgram $ flcRenameAndLift captureTest
-- z() = z!anonymous
-- z!anonymous(x1) = (z!x1!anonymous x1)
-- z!x1!anonymous(x1, x2) = (x1 x2)

-- >>> flcRenameAndLift test
-- Right x = (\x -> x)
-- y = (\x -> x)
-- z = (\x -> x)
