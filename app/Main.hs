{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import SimpleLang.Def
import SimpleLang.Tools
import SimpleLang.Tools.Manual
import MachineLang.FromSimpleLang.Debugger
import Data.Text.IO as TIO
import Prelude hiding (exp)
import Data.Vector as V



main :: IO ()
main = do
  let test = tailRecTest
  TIO.putStrLn "this:"
  TIO.putStrLn (prettyPrintSLProgram test)

  TIO.putStrLn ""
  TIO.putStrLn "compiles into this:"
  TIO.putStrLn (mlcResultText test)
  TIO.putStrLn ""
  
  debugMLC test

tailRecTest :: SLProgram
tailRecTest =
  runSLMFuncsM $ do
    let fibonacci = slmVirtualFunc (SLUserFunc "main" "fibonacci") :: SLMFuncOf 3

    _ <- slmFunc SLFuncMain (do
        x <- slmNewVar $ _app fibonacci (_const 20) (_const 0) (_const 1)
        slmReturn (_local x)
        pure ()
      )
    
    slmSetRealFunc fibonacci (\steps a b -> slmFundef $ do
        slmCase (V.fromList [
            ( _arg steps `_eq` _const 0, do
                slmReturn (_arg b)
                pure ()
              )
          ]) (do
            slmTailCall fibonacci (_arg steps `_sub` _const 1) (_arg b) (_arg a `_add` _arg b)
          )
        pure ()
      )

    pure ()