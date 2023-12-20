{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


import SimpleLang.TypedDef
import SimpleLang.Tools.Manual
import SimpleLang.Tools
import Data.Vector as V
import Data.Proxy
import GHC.TypeNats
import MachineLang.FromSimpleLang.Debugger
import Data.Text.IO as TIO
import Data.Text as T
import MachineLang.FromSimpleLang.Test
import Test.Hspec

main = pure ()

-- T.unpack $ runMLCinST complexTest