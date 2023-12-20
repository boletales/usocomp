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
type (->>) = FLTLambda

captureTest = runFLM $ do
  x <- flmDecl "x" (flmLam "x1" (\(x1 :: FLExp' FLTInt) -> flmLam "x2" (\(x2 :: FLExp' FLTInt) -> x1)))
  z <- flmDecl "z" (flmLam "x1" (\(x1 :: FLExp' (FLTInt ->> FLTInt)) -> flmLam "x2" (\(x2 :: FLExp' FLTInt) -> flmApp x1 x2)))
  
  y <- flmDecl "y" (
      flmLam "ext1" (\(ext1 ::  FLExp' (FLTInt ->> FLTInt)) ->
          flmLam "ext2" (\ext2 ->
              flmApp (flmApp (
                flmLam "x1" (\x1 -> flmLam "x2" (\x2 -> flmApp ext1 ext2))
              ) ext1) ext2
            )))
  
  pure ()

-- >>> error $ T.unpack $ either id prettyPrintFLCProgram $ flcRenameAndLift captureTest
-- x() = x!anonymous
-- x!anonymous(x2 :: (Int -> Int), x1 :: (Int -> (Int -> Int))) = x1
-- y() = y!anonymous
-- y!anonymous(ext2 :: (Int -> Int), ext1 :: ((Int -> Int) -> (Int -> Int))) = ((((y!anonymous.L.L!anonymous ext1) ext2) ext1) ext2)
-- y!anonymous.L.L!anonymous(ext1 :: (Int -> Int), ext2 :: Int, x2 :: (Int -> Int), x1 :: ((Int -> Int) -> (Int -> Int))) = (ext1 ext2)
-- z() = z!anonymous
-- z!anonymous(x2 :: (Int -> Int), x1 :: ((Int -> Int) -> (Int -> Int))) = (x1 x2)

-- >>> flcRenameAndLift test
-- Right x = (\x -> x)
-- y = (\x -> x)
-- z = (\x -> x)
