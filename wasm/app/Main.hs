{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module SlangC where

import MyPrelude

import Data.Text as T
import Data.Text.IO as TIO
import Tools.SimpleLangC
import GHC.Wasm.Prim

foreign export javascript "compile" compile :: JSString -> JSString

compile :: JSString -> JSString
compile = fromJSString >>> T.pack >>> compileToJSON >>> T.unpack >>> toJSString