{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import MyPrelude

import SimpleLang.Def
import SimpleLang.Tools
import SimpleLang.Tools.Manual
import MachineLang.FromSimpleLang.Debugger
import Data.Text as T ()
import Data.Text.IO as TIO
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
    let fibonacci = slmVirtualFunc (SLUserFunc "main" "fibonacci") :: '[SLTInt, SLTInt, SLTInt] --> SLTInt

    _ :: '[] --> SLTInt <- slmFunc SLFuncMain (do
        x <- slmNewVar $ _app fibonacci (_const 20) (_const 0) (_const 1)
        slmReturn (_local x)
        pure ()
      )
    
    slmSetRealFunc fibonacci (\steps a b -> slmFundef $ do
        slmCase (V.fromList [
            ( steps `_eq` _const 0, do
                slmReturn b
                pure ()
              )
          ]) (do
            slmTailCall fibonacci (steps `_sub` _const 1) b (a `_add` b)
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
    _ :: '[] --> SLTInt <- slmFunc SLFuncMain (do
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
nop                                     #main.
const r2 1                              #main.
const r1 1                              #main.
const r3 59                             #main.
store r3 r1                             #main.
add   r1 r1 r2                          #main.
const r3 0                              #main.
store r3 r1                             #main.
const r3 0                              #main.
add   r1 r1 r2                          #main.
store r3 r1                             #main.
copy  r0 r1                             #main.
const pc 13                             #main.
const r2 1                              #main.SLLPMulti 0
add   r1 r1 r2                          #main.SLLPMulti 0
const r3 100                            #main.SLLPMulti 0
store r3 r1                             #main.SLLPMulti 0
const r2 1                              #main.SLLPMulti 1
add   r1 r1 r2                          #main.SLLPMulti 1
const r3 200                            #main.SLLPMulti 1
store r3 r1                             #main.SLLPMulti 1
const r2 1                              #main.SLLPMulti 2
add   r1 r1 r2                          #main.SLLPMulti 2
const r3 300                            #main.SLLPMulti 2
store r3 r1                             #main.SLLPMulti 2
const r2 1                              #main.SLLPMulti 3
add   r1 r1 r2                          #main.SLLPMulti 3
const r3 400                            #main.SLLPMulti 3
store r3 r1                             #main.SLLPMulti 3
const r2 1                              #main.SLLPMulti 4
add   r1 r1 r2                          #main.SLLPMulti 4
const r3 10000                          #main.SLLPMulti 4
store r3 r1                             #main.SLLPMulti 4
load  r3 r1                             #main.SLLPMulti 4
const r2 -1                             #main.SLLPMulti 4
add   r1 r1 r2                          #main.SLLPMulti 4
const r2 3                              #main.SLLPMulti 4
add   r2 r2 r0                          #main.SLLPMulti 4
store r3 r2                             #main.SLLPMulti 4
const r2 -4                             #main.
add   r1 r1 r2                          #main.
const r2 1                              #main.
add   r1 r1 r2                          #main.
const r3 0                              #main.
store r3 r1                             #main.
const r2 -1                             #main.
add   r4 r2 r0                          #main.
load  r4 r4                             #main.
load  r3 r1                             #main.
store r3 r4                             #main.
add   r1 r1 r2                          #main.
add   r4 r4 r2                          #main.
copy  r2 r0                             #main.
load  r0 r0                             #main.
const r3 -1                             #main.
add   r2 r2 r3                          #main.
load  r1 r2                             #main.
add   r2 r2 r3                          #main.
load  pc r2                             #main.
nop                                     #main.
-}
