{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
--{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module MachineLang.FromSimpleLang.Test (
      mlctTests
    , mlctUnitTest
    , MLCTUnit (..)
  ) where

import MyPrelude

import SimpleLang.TypedDef
import SimpleLang.Tools.Manual
import SimpleLang.Tools
import Data.Vector as V
import Data.Proxy
import GHC.TypeNats
import MachineLang.FromSimpleLang.Debugger
import Data.Text.IO as TIO
import Data.Text as T
import SimpleLang.StaticCheck
import Data.Bifunctor

mlctTests :: [MLCTUnit]
mlctTests = [
      mlctSubstTest
    , mlctIfTest
    , mlctWhileTest
    , mlctSmallTest
    , mlctStructTest
    , mlctStructTest2
    , mlctComplexTest
    , mlctTailRecTest
    , mlctClosureTest
  ]

data MLCTUnit = MLCTUnit {
      mlctName :: Text
    , mlctTest :: SLProgram
    , mlctExpected :: Int
  }

mlctUnitTest :: Int -> SLProgram -> Either Text Text
mlctUnitTest expectedout program = do
  first prettyPrintSLSCError $ slscCheck program
  (out, ticks) <- runMLCinST' program
  if out == expectedout
    then Right $ "(" <> tshow ticks <> " ticks) successfully terminated. code:" <> tshow out
    else Left $ "expected out: " <> tshow expectedout <> " but got: " <> tshow out

substTest :: SLProgram
substTest =
  runSLMFuncsM $ do
    main :: ('[] --> 'SLTInt) <- slmFunc SLFuncMain (slmFundef $ do
          i <- slmNewVar (_const 100)
          j <- slmNewVar (_const 200)
          k <- slmNewVar (_const 300)
          l <- slmNewVar (_const 400)
          _reflocal k <<- _const 10000

          slmReturn (_local i `_add` _local j `_add` _local k `_add` _local l)
          pure ()
        )
    
    pure ()

mlctSubstTest :: MLCTUnit
mlctSubstTest = MLCTUnit {
      mlctName = "substTest"
    , mlctTest = substTest
    , mlctExpected = 100 + 200 + 10000 + 400
  }

-- >>> runMLCinST substTest
-- "(96 ticks) successfully terminated. code:10700"

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

mlctIfTest :: MLCTUnit
mlctIfTest = MLCTUnit {
      mlctName = "ifTest"
    , mlctTest = ifTest
    , mlctExpected = 0
  }
    
whileTest :: SLProgram
whileTest =
  runSLMFuncsM $ do
    main :: ('[] --> 'SLTInt) <- slmFunc SLFuncMain (slmFundef $ do
        i <- slmNewVar (_const 1)
        slmWhile (_local i `_lt` _const 1000) (do
            _reflocal i <<- _local i `_add` _local i
            pure ()
          )

        slmReturn (_local i)
      )

    pure ()

mlctWhileTest :: MLCTUnit
mlctWhileTest = MLCTUnit {
      mlctName = "whileTest"
    , mlctTest = whileTest
    , mlctExpected = 1024
  }

-- >>> runMLCinST whileTest
-- "(22 ticks) successfully terminated. code:-1000000000"

smallTest :: SLProgram
smallTest =
  runSLMFuncsM $ do
    main :: ('[] --> 'SLTInt) <- slmFunc SLFuncMain (do
        slmReturn (_const 12345) 
        pure ()
      )

    pure ()

mlctSmallTest :: MLCTUnit
mlctSmallTest = MLCTUnit {
      mlctName = "smallTest"
    , mlctTest = smallTest
    , mlctExpected = 12345
  }

-- >>> runMLCinST smallTest
-- "(32 ticks) successfully terminated. code:12345"

structTest :: SLProgram
structTest =
  runSLMFuncsM $ do
    _ :: ('[] --> SLTInt) <- slmFunc SLFuncMain (slmFundef $ do
        y <- slmNewVar (_const 100)
        str <- slmNewVar (_const 100 >: _const 200 >: _const 300 >: TSLEStructNil)
        x :: SLMVar 'SLTInt <- slmNewVar (_local str `TSLEStructGet` Proxy @2)
        slmReturn (_local x)
      )
    pure ()

mlctStructTest :: MLCTUnit
mlctStructTest = MLCTUnit {
      mlctName = "structTest"
    , mlctTest = structTest
    , mlctExpected = 300
  }

-- >>> runMLCinST structTest
-- "(55 ticks) successfully terminated. code:300"

structTest2 :: SLProgram
structTest2 =
  runSLMFuncsM $ do
    _ :: ('[] --> SLTInt) <- slmFunc SLFuncMain (do
        str <- slmNewVar ((_const 100 >: _const 200 >: _const 300 >: TSLEStructNil) >: (_const 1000 >: _const 2000 >: _const 3000 >: TSLEStructNil) >: (_const 10000 >: _const 20000 >: _const 30000 >: TSLEStructNil) >: TSLEStructNil)
        x :: SLMVar 'SLTInt <- slmNewVar ((_local str `TSLEStructGet` Proxy @1) `TSLEStructGet` Proxy @1)
        slmReturn (_local x)
      )
    pure ()

mlctStructTest2 :: MLCTUnit
mlctStructTest2 = MLCTUnit {
      mlctName = "structTest2"
    , mlctTest = structTest2
    , mlctExpected = 2000
  }

-- >>> runMLCinST structTest2
-- "(79 ticks) successfully terminated. code:2000"

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
        slmReturn (_local re3 >: _local im3 >: TSLEStructNil)
        pure ()
      )

    _ :: ('[] --> SLTInt) <- slmFunc SLFuncMain (do
        c1 <- slmNewVar (TSLEStructCons (_const 100) (TSLEStructCons (_const 200) TSLEStructNil))
        c2 <- slmNewVar (TSLEStructCons (_const 300) (TSLEStructCons (_const 400) TSLEStructNil))
        d <- slmNewVar (_const 1111111)
        c3 <- slmNewVar (_app complexProd (_local c1) (_local c2))
        e <- slmNewVar (_const 2222222)
        slmReturn (_local c3 `TSLEStructGet` Proxy @1)
        pure ()
      )
    pure ()

mlctComplexTest :: MLCTUnit
mlctComplexTest = MLCTUnit {
      mlctName = "complexTest"
    , mlctTest = complexTest
    , mlctExpected = 100000
  }

-- >>> runMLCinST complexTest
-- "(263 ticks) successfully terminated. code:100000"

-- >>> error $ unpack $ prettyPrintSLProgram complexTest
-- function #main() -> int
-- {
--   (int, int) $L0 = (100, 200)
--   (int, int) $L2 = (300, 400)
--   int $L4 = 1111111
--   (int, int) $L5 = #main/complexProd($L0, $L2)
--   int $L7 = 2222222
--   return $L5.1
-- }
-- function #main/complexProd((int, int) $A0, (int, int) $A1) -> (int, int)
-- {
--   int $L0 = $A0.0
--   int $L1 = $A0.1
--   int $L2 = $A2.0
--   int $L3 = $A2.1
--   int $L4 = (($L0 * $L2) - ($L1 * $L3))
--   int $L5 = (($L0 * $L3) + ($L1 * $L2))
--   return ($L4, $L5)
-- }


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

mlctTailRecTest :: MLCTUnit
mlctTailRecTest = MLCTUnit {
      mlctName = "tailRecTest"
    , mlctTest = tailRecTest
    , mlctExpected = 10946
  }

-- >>> runMLCinST tailRecTest
-- "(2570 ticks) successfully terminated. code:10946"
-- 


closureTest :: SLProgram
closureTest =
  runSLMFuncsM $ do
    let func1 = slmVirtualFunc (SLUserFunc "main" "func1") :: '[SLTInt, '[SLTInt, SLTInt] !--> SLTInt] --> SLTInt
    let func2 = slmVirtualFunc (SLUserFunc "main" "func2") :: '[SLTInt, SLTInt] --> SLTInt

    _ :: '[] --> SLTInt <- slmFunc SLFuncMain (do
        slmReturn (TSLEPushCall (TSLClosureCall (_funcptr func1 >: _const 12345 >: _funcptr func2 >: TSLEStructNil)))
        pure ()
      )

    slmSetRealFunc func1 (\x g -> slmFundef $ do
        y <- slmNewVar (_const 2345)
        slmClsTailCall (g >: x >: _local y >: TSLEStructNil)
        pure ()
      )

    slmSetRealFunc func2 (\x y -> slmFundef $ do
        slmReturn (x `_sub` y)
        pure ()
      )

    pure ()

mlctClosureTest :: MLCTUnit
mlctClosureTest = MLCTUnit {
      mlctName = "closureTest"
    , mlctTest = closureTest
    , mlctExpected = 10000
  }

-- >>> runMLCinST closureTest
-- "(168 ticks) successfully terminated. code:10000"

-- >>> error $ unpack $ prettyPrintSLProgram closureTest
-- function #main() -> int
-- {
--   return (#main/func1, 12345, #main/func2)()
-- }
-- function #main/func1(int $A0, (int, int) -> int $A1) -> int
-- {
--   int $L0 = 2345
--   tailcall ($A1, $A0, $L0)()
-- }
-- function #main/func2(int $A0, int $A1) -> int
-- {
--   return ($A0 - $A1)
-- }
