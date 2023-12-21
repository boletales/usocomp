{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
module SimpleLang.FromFuncLang.Test where

import SimpleLang.FromFuncLang
import FuncLang.Def
import FuncLang.Tools.Manual
import Data.Text as T
import Data.Text.IO as TIO

type FLExp' = FLExp Text
type (->>) = FLTLambda

($$$) :: forall (t1 :: FLType) (t :: FLType) tag. (SomeFLType t1, SomeFLType t) => FLExp tag ('FLTLambda t1 t) -> FLExp tag t1 -> FLExp tag t
($$$) = flmApp

captureTest = runFLM $ do
  --x <- flmDecl "x" (flmLam "x1" (\(x1 :: FLExp' FLTInt) -> flmLam "x2" (\(x2 :: FLExp' FLTInt) -> x1)))
  --z <- flmDecl "z" (flmLam "x1" (\(x1 :: FLExp' (FLTInt ->> FLTInt)) -> flmLam "x2" (\(x2 :: FLExp' FLTInt) -> flmApp x1 x2)))
  {-
  
  y <- flmDecl "y" (
      flmLam "ext1" (\(ext1 ::  FLExp' (FLTInt ->> FLTInt)) ->
          flmLam "ext2" (\ext2 ->
              flmApp (flmApp (
                flmLam "x1" (\x1 -> flmLam "x2" (\x2 -> flmApp ext1 ext2))
              ) ext1) ext2
            )))
  -}
  
  true <- flmDecl "true" (flmLam "x" (\(x :: FLExp' FLTInt) -> flmLam "y" (\(y :: FLExp' FLTInt) -> x)))
  --false <- flmDecl "false" (flmLam "x" (\(x :: FLExp' FLTInt) -> flmLam "y" (\(y :: FLExp' FLTInt) -> y)))

  main <- flmDecl "main" ((true $$$ (FLEValI 1)) $$$ (FLEValI 2))

  pure ()


-- >>> error $ T.unpack $ either id (T.pack . show) $ interpretFLC <$> flcRenameAndLift captureTest
-- Right (FLCIEFLCE 2)

-- >>> error $ T.unpack $ either id prettyPrintFLCProgram $ flcRenameAndLift captureTest
-- main() = ((true 1) 2)
-- true() = true!anonymous
-- true!anonymous(x :: Int, y :: Int) = x
