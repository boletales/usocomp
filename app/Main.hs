{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main (main) where

import SimpleLang.Def
import SimpleLang.Tools
import SimpleLang.Tools.Manual
import MachineLang.FromSimpleLang.Debugger
import Data.Text as T ()
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

{-
以上のコードで生成される中間言語の抽象構文木を、人間にとって読みやすい形に書き下すと次のようになります：
>>> error $ T.unpack $ prettyPrintSLProgram tailRecTest

function #main ($A0)
{
  var $L0 = #main.fibonacci(20, 0, 1)
  return $L0
}
function #main.fibonacci ($A0, $A1, $A2, $A3)
{
  when ($A0 == 0)
  {
    return $A2
  }
  else
  {
    tailcall #main.fibonacci(($A0 - 1), $A2, ($A1 + $A2))
  }
}

-}



substTest :: SLProgram
substTest =
  runSLMFuncsM $ do
    _ <- slmFunc SLFuncMain (do
          i <- slmNewVar (_const 100)
          j <- slmNewVar (_const 200)
          k <- slmNewVar (_const 300)
          l <- slmNewVar (_const 400)
          _reflocal k <<- _const 10000
          pure ()
        )
    
    pure ()



{-
以上のコードで生成される中間言語の抽象構文木を、人間にとって読みやすい形に書き下すと次のようになります：
>>> error $ T.unpack $ prettyPrintSLProgram substTest
function #main ($A0)
{
  var $L0 = 100
  var $L1 = 200
  var $L2 = 300
  var $L3 = 400
  $L2 = 10000
}

このコードは、以下のような仮想機械語にコンパイルされます：
>>> error $ T.unpack $ mlcResultText substTest
nop                                     SLFuncMain.
const r2 1                              SLFuncMain.
const r1 1                              SLFuncMain.
const r3 57                             SLFuncMain.
store r3 r1                             SLFuncMain.
add   r1 r1 r2                          SLFuncMain.
const r3 0                              SLFuncMain.
store r3 r1                             SLFuncMain.
const r3 0                              SLFuncMain.
add   r1 r1 r2                          SLFuncMain.
store r3 r1                             SLFuncMain.
copy  r0 r1                             SLFuncMain.
const pc 13                             SLFuncMain.
const r2 1                              SLFuncMain.SLLPMulti 0
add   r1 r1 r2                          SLFuncMain.SLLPMulti 0
const r3 100                            SLFuncMain.SLLPMulti 0
store r3 r1                             SLFuncMain.SLLPMulti 0
const r2 1                              SLFuncMain.SLLPMulti 1
add   r1 r1 r2                          SLFuncMain.SLLPMulti 1
const r3 200                            SLFuncMain.SLLPMulti 1
store r3 r1                             SLFuncMain.SLLPMulti 1
const r2 1                              SLFuncMain.SLLPMulti 2
add   r1 r1 r2                          SLFuncMain.SLLPMulti 2
const r3 300                            SLFuncMain.SLLPMulti 2
store r3 r1                             SLFuncMain.SLLPMulti 2
const r2 1                              SLFuncMain.SLLPMulti 3
add   r1 r1 r2                          SLFuncMain.SLLPMulti 3
const r3 400                            SLFuncMain.SLLPMulti 3
store r3 r1                             SLFuncMain.SLLPMulti 3
const r2 1                              SLFuncMain.SLLPMulti 4
add   r1 r1 r2                          SLFuncMain.SLLPMulti 4
const r3 10000                          SLFuncMain.SLLPMulti 4
store r3 r1                             SLFuncMain.SLLPMulti 4
load  r3 r1                             SLFuncMain.SLLPMulti 4
const r2 -1                             SLFuncMain.SLLPMulti 4
add   r1 r1 r2                          SLFuncMain.SLLPMulti 4
const r2 3                              SLFuncMain.SLLPMulti 4
add   r2 r2 r0                          SLFuncMain.SLLPMulti 4
store r3 r2                             SLFuncMain.SLLPMulti 4
const r2 -4                             SLFuncMain.
add   r1 r1 r2                          SLFuncMain.
const r2 1                              SLFuncMain.
add   r1 r1 r2                          SLFuncMain.
const r3 0                              SLFuncMain.
store r3 r1                             SLFuncMain.
const r2 -1                             SLFuncMain.
add   r2 r2 r0                          SLFuncMain.
load  r2 r2                             SLFuncMain.
load  r3 r1                             SLFuncMain.
store r3 r2                             SLFuncMain.
copy  r2 r0                             SLFuncMain.
load  r0 r0                             SLFuncMain.
const r3 -1                             SLFuncMain.
add   r2 r2 r3                          SLFuncMain.
load  r1 r2                             SLFuncMain.
add   r2 r2 r3                          SLFuncMain.
load  pc r2                             SLFuncMain.
nop                                     SLFuncMain.
-}
