{-# LANGUAGE DataKinds #-}

module Main (main) where

import SimpleLang.Def
import SimpleLang.Tools
import SimpleLang.Tools.Manual
import MachineLang.FromSimpleLang
import MachineLang.FromSimpleLang.Debugger
import Data.Vector as V

import Data.Map as M
import SimpleLang.Tools.Manual


main :: IO ()
main = 
  debugMLC funcTest


funcTest :: SLProgram
funcTest =
  runSLMFuncsM $ do
    let exp = slmVirtualFunc (SLUserFunc "main" "exp") :: SLMFuncOf 2

    main <- slmFunc SLFuncMain (do
        x <- slmNewVar $ _app exp (_const 3) (_const 5)
        i <- slmNewVar $ _app exp (_const 3) (_const 5)
        slmReturn (_local x)
        pure ()
      )


    slmSetRealFunc exp (\a b -> slmFundef $ do
        x <- slmNewVar (_const 1)
        i <- slmNewVar (_const 0)
        slmWhile (_local i `_lt` _arg b) (do
            _reflocal x <<- _local x `_mult` _arg a
            _reflocal i <<- _local i `_add` _const 1
            pure ()
          )
        slmReturn (_local x)
        pure ()
      )

    pure ()