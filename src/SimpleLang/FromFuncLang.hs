module SimpleLang.FromFuncLang where

import SimpleLang.Def

{-
コンパイル手順：
0. 識別子を一意なものに変換する
1. すべての関数をトップレベルに持ち上げる
2. 不完全な適用はクロージャ（SLExp）に、完全な適用はSLEPushCallなりslmTailCallReturnなりにする
-}

