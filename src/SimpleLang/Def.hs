{-| 
Module      : SimpleLang.Def
Description : SimpleLangの定義
-}

module SimpleLang.Def (
    SLAddr (..)
  , SLVal (..)
  , SLFuncName (..)
  , SLCallable (..)
  , SLPrim1 (..)
  , SLPrim2 (..)
  , SLExp (..)
  , SLRef (..)
  , SLStatement (..)
  , SLBlock (..)
  , SLFuncBlock (..)
  , SLProgram
)where

import Data.Vector as V
import Data.Map as M
import Data.Text

-- 接頭辞 SL: SimpleLang に関連するものの型

{-|
  SimpleLangは、関数・コールスタック・スコープ付きローカル変数の概念をサポートする言語です。

  変数名の遮蔽の概念はなく、ローカル変数は外側のスコープから順に振られた番号で指定されます。
  包含関係にないスコープ間では変数の番号が一意ではありません。
-}


newtype SLAddr = SLAddr Int deriving (Show, Eq)
newtype SLVal  = SLVal  Int deriving (Show, Eq)

data SLFuncName =
        SLFuncMain
      | SLUserFunc Text Text
      deriving (Show, Eq, Ord)

data SLCallable =
        SLSolidFunc SLFuncName
      | SLFuncRef   SLExp
      deriving (Show, Eq)

data SLPrim1 =
        SLPrim1Inv
      deriving (Show, Eq)

data SLPrim2 =
        SLPrim2Add
      | SLPrim2Sub
      | SLPrim2Mult
      | SLPrim2Shift
      | SLPrim2And
      | SLPrim2Or
      | SLPrim2Xor
      | SLPrim2Gt
      | SLPrim2Lt
      | SLPrim2Eq
      deriving (Show, Eq)

data SLExp =
        SLEConst    SLVal
      | SLELocal    Int
      | SLEArg      Int
      | SLEPtr      SLExp
      | SLEPushCall SLCallable (V.Vector SLExp)
      | SLEFuncPtr  SLFuncName
      | SLEPrim1    SLPrim1 SLExp
      | SLEPrim2    SLPrim2 SLExp SLExp
      deriving (Show, Eq)

data SLRef = 
          SLRefPtr   SLExp
        | SLRefLocal Int
      deriving (Show, Eq)

data SLStatement =
      --   SLSPrimPush SLExp
      -- | SLSPrimPop
        SLSInitVar Int SLExp
      | SLSSubst   SLRef SLExp
      | SLSReturn  SLExp
      | SLSTailCallReturn SLCallable (V.Vector SLExp)
      deriving (Show, Eq)

data SLBlock =
        SLBSingle  SLStatement
      | SLBMulti  (V.Vector SLBlock)
      | SLBCase   (V.Vector (SLExp, SLBlock)) SLBlock
      | SLBWhile   SLExp SLBlock
      deriving (Show, Eq)

data SLFuncBlock =
      SLFuncBlock {
          slfName     :: SLFuncName
        , slfArgCount :: Int
        , slfBlock    :: SLBlock
      }
      deriving (Show, Eq)

type SLProgram =
        M.Map SLFuncName SLFuncBlock
