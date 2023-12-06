{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module MachineLang.FromSimpleLang.Test () where

import SimpleLang.Def
import SimpleLang.Tools.Manual
import SimpleLang.Tools
import Data.Vector as V
import Data.Proxy
import GHC.TypeNats
import MachineLang.FromSimpleLang.Debugger


substTest :: SLProgram
substTest =
  runSLMFuncsM $ do
    main :: ('[] ->> 'SLTInt) <- slmFunc SLFuncMain (do
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
    main :: ('[] ->> 'SLTInt) <- slmFunc SLFuncMain (do
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
        )
    
    pure ()
    
whileTest :: SLProgram
whileTest =
  runSLMFuncsM $ do
    main :: ('[] ->> 'SLTInt) <- slmFunc SLFuncMain (do
        i <- slmNewVar (SLEConst (SLVal 1))
        slmWhile (_local i `_lt` _const 1000) (do
            _reflocal i <<- _local i `_add` _local i
            pure ()
          )
        pure ()
      )

    pure ()


smallTest :: SLProgram
smallTest =
  runSLMFuncsM $ do
    main :: ('[] ->> 'SLTInt) <- slmFunc SLFuncMain (do
        slmReturn (_const 12345) 
        pure ()
      )

    pure ()

structTest :: SLProgram
structTest =
  runSLMFuncsM $ do
    _ :: ('[] ->> SLTInt) <- slmFunc SLFuncMain (do
        str <- slmNewVar (_const 100 >: _const 200 >: _const 300 >: SLEStructNil)
        x :: SLMVar 'SLTInt <- slmNewVar (_local str `SLEStructGet` Proxy @1)
        slmReturn (_local x)
      )
    pure ()

type SLTComplex = 'SLTStruct '[ 'SLTInt, 'SLTInt ]

complexTest :: SLProgram
complexTest =
  runSLMFuncsM $ do
    complexProd :: ('[SLTComplex, SLTComplex] ->> SLTComplex) <- slmFunc (SLUserFunc "main" "complexProd") (\c1 c2 -> do
        re1 <- slmNewVar (c1 `SLEStructGet` Proxy @0)
        im1 <- slmNewVar (c1 `SLEStructGet` Proxy @1)
        re2 <- slmNewVar (c2 `SLEStructGet` Proxy @0)
        im2 <- slmNewVar (c2 `SLEStructGet` Proxy @1)
        re3 <- slmNewVar ((_local re1 `_mul` _local re2) `_sub` (_local im1 `_mul` _local im2))
        im3 <- slmNewVar ((_local re1 `_mul` _local im2) `_add` (_local im1 `_mul` _local re2))
        slmReturn (SLEStructCons (_local re3) (SLEStructCons (_local im3) SLEStructNil))
        pure ()
      )

    _ :: ('[] ->> SLTInt) <- slmFunc SLFuncMain (do
        c1 <- slmNewVar (SLEStructCons (_const 100) (SLEStructCons (_const 200) SLEStructNil))
        c2 <- slmNewVar (SLEStructCons (_const 300) (SLEStructCons (_const 400) SLEStructNil))
        d <- slmNewVar (_const 1111111)
        c3 <- slmNewVar (_app complexProd (_local c1) (_local c2))
        e <- slmNewVar (_const 2222222)
        slmReturn (_local c2 `SLEStructGet` Proxy @1)
        pure ()
      )
    pure ()