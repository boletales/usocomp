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
data FLVarDecl (tag :: Type) where
  FLVarDecl :: FLVar tag t -> FLExp tag t -> FLVarDecl tag

data FLExp (tag :: Type) (t :: FLType) where
  FLEValI   :: forall tag      . Int -> FLExp tag 'FLTInt
  FLEValB   :: forall tag      . Int -> FLExp tag 'FLTBool
  FLEVar    :: forall tag t1   . FLVar tag t1 -> FLExp tag t1
  FLELambda :: forall tag t1 t2. FLVar tag t1 -> FLExp tag t2  -> FLExp tag ('FLTLambda t1 t2)
  FLEApp    :: forall tag t1 t2. FLExp tag ('FLTLambda t1 t2) -> FLExp tag t1 -> FLExp tag t2
  FLELet    :: forall tag t2   . [FLVarDecl tag] -> FLExp tag t2 -> FLExp tag t2

data FLProgram (tag :: Type) = FLProgram {
      flpTopLevelVars :: M.Map tag (FLVarDecl tag)
  }