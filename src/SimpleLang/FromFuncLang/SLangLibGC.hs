{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module SimpleLang.FromFuncLang.SLangLibGC where

import MyPrelude

import SimpleLang.Def
import SimpleLang.Tools.Manual
import Data.Vector as V

heapstart :: Int
heapstart = 6000

heapend :: Int
heapend = 100000

secondHalfStart :: Int
secondHalfStart = (heapend - heapstart) `div` 2 + heapstart

