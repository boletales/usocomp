{-# LANGUAGE OverloadedStrings #-}
module SimpleLang.FromFuncLang.Test where

import SimpleLang.FromFuncLang
import FuncLang.Def
import FuncLang.Tools.Manual

test = runFLM $ do
        x <- flmDecl "x" (flmLam "x" (\x -> x))
        y <- flmDecl "y" (FLEApp (flmLam "t" (\t -> t)) x)
        pure ()

-- >>> test
-- "x" = (\"x" -> "x")
-- "y" = ((\"x" -> "x") (\"x" -> "x"))
-- "z" = (\"x" -> "x")

-- >>> flcRenameAndLift test
-- Right x = (\x -> x)
-- y = (\x -> x)
-- z = (\x -> x)
