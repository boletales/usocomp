module Main (main) where

import SimpleLang.Def
import SimpleLang.Tools
import SimpleLang.Tools.Manual
import MachineLang.FromSimpleLang
import MachineLang.FromSimpleLang.Debugger
import Data.Vector as V

import Data.Map as M


main :: IO ()
main = 
  debugMLC substTest

substTest :: SLProgram
substTest =
  M.fromList [
      runslm SLFuncMain (do
          i <- slmNewVar (_const 100)
          j <- slmNewVar (_const 200)
          k <- slmNewVar (_const 300)
          l <- slmNewVar (_const 400)
          _reflocal k <<- _const 10000
          pure ()
        )
    ]

whileTest :: SLProgram
whileTest =
  M.fromList [
      runslm SLFuncMain (do
          i <- slmNewVar (SLEConst (SLVal 1))
          slmWhile (_local i `_lt` _const 1000) (do
              _reflocal i <<- _local i `_add` _local i
              pure ()
            )
          pure ()
        )
    ]

{-
funcTest :: SLProgram
funcTest =
  M.fromList [
        runslm SLFuncMain (do
            i <- slmNewVar (SLEConst (SLVal 100))
            j <- slmNewVar (SLEConst (SLVal 200))
            k <- slmNewVar (SLEConst (SLVal 300))
            l <- slmNewVar (SLEConst (SLVal 400))
            slmStmt (SLSSubst (SLRefLocal k) (SLEConst (SLVal 10000)))
            pure ()
          )
      , runslm (SLUserFunc "main" "") (do
            i <- slmNewVar (SLEConst (SLVal 100))
            j <- slmNewVar (SLEConst (SLVal 200))
            k <- slmNewVar (SLEConst (SLVal 300))
            l <- slmNewVar (SLEConst (SLVal 400))
            slmStmt (SLSSubst (SLRefLocal k) (SLEConst (SLVal 10000)))
            pure ()
          )
    ]
-}