{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module FuncLang.Def (
      FLType(..)
    , FLVar(..)
    , UntypedFLVar(..)
    , FLVarDecl(..)
    , FLExp(..)
    , FLProgram(..)
    , flOverRideTypeCheck
  ) where

import Data.Map as M
import Data.Kind
import Unsafe.Coerce (unsafeCoerce)
import Data.Text as T

data FLType =
      FLTInt
    | FLTBool
    | FLTLambda FLType FLType
    | FLTTuple [FLType]
    deriving (Show, Eq)

newtype FLVar (tag :: Type) (t :: FLType) = FLVar tag deriving (Eq)
instance Show tag => Show (FLVar tag t) where
  show (FLVar tag) = show tag

data UntypedFLVar (tag :: Type) = UFLVar tag FLType
data FLVarDecl (tag :: Type) where
  FLVarDecl :: FLVar tag t -> FLExp tag t -> FLVarDecl tag

instance Show tag => Show (FLVarDecl tag) where
  show = T.unpack . prettyPrintFLVarDecl

prettyPrintFLVarDecl :: Show tag => FLVarDecl tag -> Text
prettyPrintFLVarDecl (FLVarDecl v e) = T.pack (show v) <> " = " <> prettyPrintFLExp e

data FLExp (tag :: Type) (t :: FLType) where
  FLEValI   :: forall tag      . Int -> FLExp tag 'FLTInt
  FLEValB   :: forall tag      . Int -> FLExp tag 'FLTBool
  FLEVar    :: forall tag t1   . FLVar tag t1 -> FLExp tag t1
  FLELambda :: forall tag t1 t2. FLVar tag t1 -> FLExp tag t2  -> FLExp tag ('FLTLambda t1 t2)
  FLEApp    :: forall tag t1 t2. FLExp tag ('FLTLambda t1 t2) -> FLExp tag t1 -> FLExp tag t2
  FLELet    :: forall tag t2   . [FLVarDecl tag] -> FLExp tag t2 -> FLExp tag t2

instance Show tag => Show (FLExp tag t) where
  show = T.unpack . prettyPrintFLExp

prettyPrintFLExp :: Show tag => FLExp tag t -> Text
prettyPrintFLExp e = 
  let tshow :: Show a => a -> Text
      tshow = T.pack . show
  in
    case e of
      (FLEValI i)      -> tshow i
      (FLEValB b)      -> tshow b
      (FLEVar v)       -> tshow v
      (FLELambda v e)  -> "(\\" <> tshow v <> " -> " <> prettyPrintFLExp e <> ")"
      (FLEApp e1 e2)   -> "(" <>   prettyPrintFLExp e1 <> " " <> prettyPrintFLExp e2 <> ")"
      (FLELet decls e) -> "(let " <> T.intercalate "; " (prettyPrintFLVarDecl <$> decls) <> " in " <> prettyPrintFLExp e <> ")"

data FLProgram (tag :: Type) = FLProgram {
      flpTopLevelVars :: M.Map tag (FLVarDecl tag)
  }

instance Show tag => Show (FLProgram tag) where
  show = T.unpack . prettyPrintFLProgram

prettyPrintFLProgram :: Show tag => FLProgram tag -> Text
prettyPrintFLProgram (FLProgram decls) = T.intercalate "\n" (prettyPrintFLVarDecl <$> M.elems decls)

{-# WARNING flOverRideTypeCheck "This function is unsafe. It should be used only in the compiler." #-}
flOverRideTypeCheck :: FLExp tag t -> FLExp tag u
flOverRideTypeCheck = unsafeCoerce