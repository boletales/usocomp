{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SimpleLang.FromFuncLang where

import FuncLang.Def
import SimpleLang.Def
import SimpleLang.Tools.Manual
import Data.Text as T
import Control.Category
import Prelude hiding ((.), id)
import Data.Map as M

type family FLTypeToSLType (t :: FLType) :: SLType where
    FLTypeToSLType 'FLTInt  = 'SLTInt
    FLTypeToSLType 'FLTBool = 'SLTInt
    FLTypeToSLType ('FLTLambda t1 t2) = 'SLTStruct ('SLTFuncPtr (FLTypeToSLType t1 ': '[]) (FLTypeToSLType t2) ': (FLTypeToSLType t1 ': '[]))  
    FLTypeToSLType ('FLTTuple ts)     = 'SLTStruct (MapFLTypeToSLType ts)

type family MapFLTypeToSLType (ts :: [FLType]) :: [SLType] where
    MapFLTypeToSLType '[] = '[]
    MapFLTypeToSLType (t:ts) = FLTypeToSLType t ': MapFLTypeToSLType ts


{-
コンパイル手順：
0. 識別子を一意なものに変換する
1. すべての変数内関数定義をトップレベルに持ち上げる
2. 不完全な適用はクロージャ（SLExp）に、完全な適用はSLEPushCallなりslmTailCallReturnなりにする
-}

-- 識別子という言葉を英語に翻訳するとidentifierだが、
data FLCUniqueIdentifier

-- 0+1
flcPreprocess :: FLProgram Text -> FLProgram FLCUniqueIdentifier
flcPreprocess = flcRenameIdentifier >>> flcLiftLambda

-- 0
flcRenameIdentifier :: FLProgram Text -> FLProgram FLCUniqueIdentifier
flcRenameIdentifier = undefined

-- 1
flcLiftLambda :: FLProgram FLCUniqueIdentifier -> FLProgram FLCUniqueIdentifier
flcLiftLambda = undefined

-- 2
flcCompleExp :: FLExp FLCUniqueIdentifier t -> TypedSLExp (FLTypeToSLType t)
flcCompleExp = undefined
