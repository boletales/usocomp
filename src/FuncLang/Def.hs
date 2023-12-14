{-# LANGUAGE GADTs #-}

module FuncLang.Def where

import Data.Vector as V
import Data.Map as M
import Data.Text as T
import Data.Kind

data FLType =
      FLTInt
    | FLTBool
    | FLTLambda FLType FLType
    | FLTTuple [FLType]
    deriving (Show, Eq)

newtype FLVar (tag :: Type) (t :: FLType) = FLVar tag deriving (Show, Eq)
data UntypedFLVar (tag :: Type) = UFLVar tag FLType
data FLTopLevelVar (tag :: Type) where
  FLTopLevelVar :: FLVar tag t -> FLExp tag t -> FLTopLevelVar tag

data FLExp (tag :: Type) (t :: FLType) where
  FLEValI   :: forall   tag      . Int -> FLExp tag 'FLTInt
  FLEValB   :: forall   tag      . Int -> FLExp tag 'FLTBool
  FLEVar    :: forall t tag      . FLVar tag t -> FLExp tag t
  FLELambda :: forall t tag t1 t2. FLVar tag t -> FLExp tag t2  -> FLExp tag ('FLTLambda t1 t2)
  FLEApp    :: forall t tag t1 t2. FLExp tag ('FLTLambda t1 t2) -> FLExp tag t1 -> FLExp tag t2
  FLELet    :: forall t tag t1 t2. FLVar tag t1 -> FLExp tag t2 -> FLExp tag t2

data FLProgram (tag :: Type) = FLProgram {
      flpTopLevelVars :: M.Map tag (FLTopLevelVar tag)
  }