{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module FuncLang.Def (
      FLType(..)
    , FLVar(..)
    , UntypedFLVar(..)
    , FLVarDecl(..)
    , FLExp(..)
    , FLProgram(..)
    , SomeFLType(..)
    , flTypeOf
  ) where

import Data.Map as M
import Data.Kind
import Unsafe.Coerce (unsafeCoerce)
import Data.Text as T
import Data.Proxy
import Data.Text.Encoding (Decoding(Some))

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

class SomeFLType t where
  someFLType :: Proxy t -> FLType

instance SomeFLType FLTInt where
  someFLType _ = FLTInt

instance SomeFLType FLTBool where
  someFLType _ = FLTBool

instance (SomeFLType t1, SomeFLType t2) => SomeFLType (FLTLambda t1 t2) where
  someFLType _ = FLTLambda (someFLType (Proxy :: Proxy t1)) (someFLType (Proxy :: Proxy t2))

instance SomeFLType (FLTTuple '[]) where
  someFLType _ = FLTTuple []

instance (SomeFLType (FLTTuple ts), SomeFLType t) => SomeFLType (FLTTuple (t ': ts)) where
  someFLType _ = FLTTuple (someFLType (Proxy :: Proxy t) : (case someFLType (Proxy :: Proxy (FLTTuple ts)) of
      FLTTuple ts' -> ts'
      _ -> error "impossible"
    ))

flTypeOf :: FLExp tag t -> FLType
flTypeOf e = case e of
  (FLEValI _)                        -> FLTInt
  (FLEValB _)                        -> FLTBool
  (FLEVar    @_ @t1     (FLVar _))   -> someFLType (Proxy :: Proxy t1)
  (FLELambda @_ @t1 @t2 (FLVar _) e) -> FLTLambda (flTypeOf e) (flTypeOf e)
  (FLEApp    @_ @t1 @t2 e1 _)        -> case flTypeOf e1 of FLTLambda _ t2' -> t2'; _ -> error "impossible"
  (FLELet    @_ @t1     _ e)         -> someFLType (Proxy :: Proxy t1)
  (UnsafeFLECast t _)                -> t

prettyPrintFLVarDecl :: Show tag => FLVarDecl tag -> Text
prettyPrintFLVarDecl (FLVarDecl v e) = T.pack (show v) <> " = " <> prettyPrintFLExp e

{-# WARNING UnsafeFLECast "This function is unsafe. It should be used only in the compiler." #-}
data FLExp (tag :: Type) (t :: FLType) where
  FLEValI   :: forall tag      . (                            ) => Int  -> FLExp tag 'FLTInt
  FLEValB   :: forall tag      . (                            ) => Bool -> FLExp tag 'FLTBool
  FLEVar    :: forall tag t1   . (SomeFLType t1               ) => FLVar tag t1 -> FLExp tag t1
  FLELambda :: forall tag t1 t2. (SomeFLType t1, SomeFLType t2) => FLVar tag t1 -> FLExp tag t2  -> FLExp tag ('FLTLambda t1 t2)
  FLEApp    :: forall tag t1 t2. (SomeFLType t1, SomeFLType t2) => FLExp tag ('FLTLambda t1 t2) -> FLExp tag t1 -> FLExp tag t2
  FLELet    :: forall tag t1   . (SomeFLType t1               ) => [FLVarDecl tag] -> FLExp tag t1 -> FLExp tag t1
  UnsafeFLECast :: forall tag t1 t2. FLType -> FLExp tag t1 -> FLExp tag t2

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
      (UnsafeFLECast t e) -> prettyPrintFLExp e --"(" <> prettyPrintFLExp e <> " :: " <> tshow t <> ")"
data FLProgram (tag :: Type) = FLProgram {
      flpTopLevelVars :: M.Map tag (FLVarDecl tag)
  }

instance Show tag => Show (FLProgram tag) where
  show = T.unpack . prettyPrintFLProgram

prettyPrintFLProgram :: Show tag => FLProgram tag -> Text
prettyPrintFLProgram (FLProgram decls) = T.intercalate "\n" (prettyPrintFLVarDecl <$> M.elems decls)
