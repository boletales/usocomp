module FuncLang.Def where

import Data.Vector as V
import Data.Text as T
import Data.Kind

data FLType =
      FLTInt
    | FLTBool
    | FLTLambda FLType FLType
    | FLTTuple [FLType]
    deriving (Show, Eq)

newtype FLVar (tag :: Type) (t :: FLType) = FLVar tag deriving (Show, Eq)

data FLExp (t :: FLType) where
  FLEValI   :: Int -> FLExp 'FLTInt
  FLEValB   :: Int -> FLExp 'FLTBool
  FLEVar    :: FLVar tag t -> FLExp t
  FLELambda :: FLVar tag t -> FLExp t2 -> FLExp ('FLTLambda t1 t2)
  FLEApp    :: FLExp ('FLTLambda t1 t2) -> FLExp t1 -> FLExp t2
  FLELet    :: FLVar tag t1 -> FLExp t1 -> FLExp t2 -> FLExp t2g