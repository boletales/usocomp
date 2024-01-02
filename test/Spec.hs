{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


import SimpleLang.TypedDef
import SimpleLang.Tools.Manual
import SimpleLang.Tools
import Data.Vector as V hiding (forM_)
import Data.Proxy
import GHC.TypeNats
import MachineLang.FromSimpleLang.Debugger
import Data.Text.IO as TIO
import Data.Text as T
import MachineLang.FromSimpleLang.Test
import Test.Hspec
import Control.Monad
import Data.Bifunctor

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Tests for FromSimpleLang" (
      forM_ mlctTests $ \test -> do
        specify (unpack $ mlctName test) $ do
          let program = mlctTest test
          let expected = mlctExpected test
          let result = mlctUnitTest expected program
          case result of
            Left err -> expectationFailure $ unpack err
            Right _  -> pure ()
    )