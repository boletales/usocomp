module SimpleLang.FromFuncLang.SLangLibGC where

import SimpleLang.Def
import SimpleLang.Tools.Manual
import Data.Vector as V

heapstart :: Int
heapstart = 6000

heapend :: Int
heapend = 100000

secondHalfStart :: Int
secondHalfStart = (heapend - heapstart) `div` 2 + heapstart

