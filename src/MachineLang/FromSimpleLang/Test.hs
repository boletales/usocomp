{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module MachineLang.FromSimpleLang.Test () where

import SimpleLang.TypedDef
import SimpleLang.Tools.Manual
import SimpleLang.Tools
import Data.Vector as V
import Data.Proxy
import GHC.TypeNats
import MachineLang.FromSimpleLang.Debugger
import Data.Text.IO as TIO

substTest :: SLProgram
substTest =
  runSLMFuncsM $ do
    main :: ('[] --> 'SLTInt) <- slmFunc SLFuncMain (slmFundef $ do
          i <- slmNewVar (_const 100)
          j <- slmNewVar (_const 200)
          k <- slmNewVar (_const 300)
          l <- slmNewVar (_const 400)
          _reflocal k <<- _const 10000

          slmReturn (_const 0)
          pure ()
        )
    
    pure ()

ifTest :: SLProgram
ifTest =
  runSLMFuncsM $ do
    main :: ('[] --> 'SLTInt) <- slmFunc SLFuncMain (slmFundef $ do
          i <- slmNewVar (_const 100 )
          j <- slmNewVar (_const 200 )
          a <- slmNewVar (_const 1000)
          b <- slmNewVar (_const 2000)
          c <- slmNewVar (_const 3000)
          d <- slmNewVar (_const 4000)
          slmCase (V.fromList [
              (_local i `_gt` _local j , do
                  _reflocal a <<- _const 10000
                  pure ()
                )
            ] ) (do
              _reflocal a <<- _const 20000
              pure ()
            )
          
          
          slmCase (V.fromList [
              (_local i `_lt` _local j , do
                  _reflocal b <<- _const 10000
                  pure ()
                )
            ] ) (do
              _reflocal b <<- _const 20000
              pure ()
            )

          slmCase (V.fromList [
              (_local j `_gt` _local i , do
                  _reflocal c <<- _const 10000
                  pure ()
                )
            ] ) (do
              _reflocal c <<- _const 20000
              pure ()
            )
          
          slmCase (V.fromList [
              (_local j `_lt` _local i , do
                  _reflocal d <<- _const 10000
                  pure ()
                )
            ] ) (do
              _reflocal d <<- _const 20000
              pure ()
            )
          slmReturn (_const 0)
        )

    
    pure ()
    
whileTest :: SLProgram
whileTest =
  runSLMFuncsM $ do
    main :: ('[] --> 'SLTInt) <- slmFunc SLFuncMain (slmFundef $ do
        i <- slmNewVar (_const 1)
        slmWhile (_local i `_lt` _const 1000) (do
            _reflocal i <<- _local i `_add` _local i
            pure ()
          )

        slmReturn (_const 0)
      )

    pure ()


smallTest :: SLProgram
smallTest =
  runSLMFuncsM $ do
    main :: ('[] --> 'SLTInt) <- slmFunc SLFuncMain (do
        slmReturn (_const 12345) 
        pure ()
      )

    pure ()

structTest :: SLProgram
structTest =
  runSLMFuncsM $ do
    _ :: ('[] --> SLTInt) <- slmFunc SLFuncMain (slmFundef $ do
        str <- slmNewVar (_const 100 >: _const 200 >: _const 300 >: TSLEStructNil)
        x :: SLMVar 'SLTInt <- slmNewVar (_local str `TSLEStructGet` Proxy @1)
        slmReturn (_local x)
      )
    pure ()

structTest2 :: SLProgram
structTest2 =
  runSLMFuncsM $ do
    _ :: ('[] --> SLTInt) <- slmFunc SLFuncMain (do
        str <- slmNewVar ((_const 100 >: _const 200 >: _const 300 >: TSLEStructNil) >: (_const 1000 >: _const 2000 >: _const 3000 >: TSLEStructNil) >: (_const 10000 >: _const 20000 >: _const 30000 >: TSLEStructNil) >: TSLEStructNil)
        x :: SLMVar 'SLTInt <- slmNewVar ((_local str `TSLEStructGet` Proxy @1) `TSLEStructGet` Proxy @1)
        slmReturn (_local x)
      )
    pure ()

type SLTComplex = 'SLTStruct '[ 'SLTInt, 'SLTInt ]

complexTest :: SLProgram
complexTest =
  runSLMFuncsM $ do
    complexProd :: ('[SLTComplex, SLTComplex] --> SLTComplex) <- slmFunc (SLUserFunc "main" "complexProd") (\c1 c2 -> do
        re1 <- slmNewVar (c1 `TSLEStructGet` Proxy @0)
        im1 <- slmNewVar (c1 `TSLEStructGet` Proxy @1)
        re2 <- slmNewVar (c2 `TSLEStructGet` Proxy @0)
        im2 <- slmNewVar (c2 `TSLEStructGet` Proxy @1)
        re3 <- slmNewVar ((_local re1 `_mul` _local re2) `_sub` (_local im1 `_mul` _local im2))
        im3 <- slmNewVar ((_local re1 `_mul` _local im2) `_add` (_local im1 `_mul` _local re2))
        --slmReturn (SLEStructCons (_local re3) (SLEStructCons (_local im3) SLEStructNil))
        pure ()
      )

    _ :: ('[] --> SLTInt) <- slmFunc SLFuncMain (do
        c1 <- slmNewVar (TSLEStructCons (_const 100) (TSLEStructCons (_const 200) TSLEStructNil))
        c2 <- slmNewVar (TSLEStructCons (_const 300) (TSLEStructCons (_const 400) TSLEStructNil))
        d <- slmNewVar (_const 1111111)
        c3 <- slmNewVar (_app complexProd (_local c1) (_local c2))
        e <- slmNewVar (_const 2222222)
        slmReturn (_local c2 `TSLEStructGet` Proxy @1)
        pure ()
      )
    pure ()


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
            slmTailCall fibonacci (steps `_sub` _const 1) (b) (a `_add` b)
          )
        pure ()
      )

    pure ()

closureTest :: SLProgram
closureTest =
  runSLMFuncsM $ do
    let func1 = slmVirtualFunc (SLUserFunc "main" "func1") :: '[SLTInt, '[SLTInt, SLTInt] !--> SLTInt] --> SLTInt
    let func2 = slmVirtualFunc (SLUserFunc "main" "func2") :: '[SLTInt, SLTInt] --> SLTInt

    _ :: '[] --> SLTInt <- slmFunc SLFuncMain (do
        slmReturn (TSLEPushCall (TSLClosureCall (_funcptr func1 >: _const 100 >: _funcptr func2 >: TSLEStructNil)))
        pure ()
      )

    slmSetRealFunc func1 (\x g -> slmFundef $ do
        y <- slmNewVar (_const 200)
        slmClsTailCall (g >: x >: _local y >: TSLEStructNil)
        pure ()
      )

    slmSetRealFunc func2 (\x y -> slmFundef $ do
        slmReturn (x `_mul` y)
        pure ()
      )

    pure ()