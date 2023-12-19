{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
module SimpleLang.FromFuncLang.Test where

import SimpleLang.FromFuncLang
import FuncLang.Def
import FuncLang.Tools.Manual

{-
captureTest = runFLM $ do
  x <- flmDecl "x" (flmLam "x1" (\x1 -> flmLam "x2" (\x2 -> x1)))
  y <- flmDecl "y" (
      flmLam "ext1" (\ext1 ->
          flmLam "ext2" (\ext2 ->
              flmApp (flmApp (
                flmLam "x1" (\x1 -> flmLam "x2" (\x2 -> flmApp ext1 ext2))
              ) ext1) ext2
            )))
  pure ()

-- ProgressCancelledException
-- "z" = (\"x" -> "x")

-- >>> flcRenameAndLift test
-- Right x = (\x -> x)
-- y = (\x -> x)
-- z = (\x -> x)
-}