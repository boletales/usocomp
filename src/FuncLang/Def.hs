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

import MyPrelude

import Data.Map.Strict as M
import Data.Kind
import Data.Text as T
import Data.Proxy

data FLType =
      FLTInt
    | FLTBool
    | FLTLambda FLType FLType
    | FLTTuple [FLType]
    deriving (Eq)

instance Show FLType where
  show = T.unpack . prettyPrintFLType

prettyPrintFLType :: FLType -> Text
prettyPrintFLType t =
  case t of
    FLTInt -> "Int"
    FLTBool -> "Bool"
    FLTLambda t1 t2 -> "(" <> prettyPrintFLType t1 <> " -> " <> prettyPrintFLType t2 <> ")"
    FLTTuple ts     -> "(" <> T.intercalate ", " (prettyPrintFLType <$> ts) <> ")"

newtype FLVar (tag :: Type) (t :: FLType) = FLVar tag deriving (Eq)
instance Show tag => Show (FLVar tag t) where
  show (FLVar tag) = sshow tag

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
  (FLELambda @_ @t1 @t2 (FLVar _) _) -> FLTLambda (someFLType (Proxy @t1)) (someFLType (Proxy @t2))
  (FLEApp    @_ @t1 @t2 _ _)         -> case someFLType (Proxy @t1) of FLTLambda a r | a == someFLType (Proxy @t2) -> r; _ -> error "impossible"
  (FLELet    @_ @t1     _ _)         -> someFLType (Proxy @t1)
  --(FLEPrim   @_ @t1     _)           -> someFLType (Proxy @t1)
  --(UnsafeFLECast t _)                -> t

prettyPrintFLVarDecl :: Show tag => FLVarDecl tag -> Text
prettyPrintFLVarDecl (FLVarDecl v e) = tshow v <> " = " <> prettyPrintFLExp e

--{-# WARNING UnsafeFLECast "This function is unsafe. It should be used only in the compiler." #-}
data FLExp (tag :: Type) (t :: FLType) where
  FLEValI   :: forall tag      . (                            ) => Int  -> FLExp tag 'FLTInt
  FLEValB   :: forall tag      . (                            ) => Bool -> FLExp tag 'FLTBool
  FLEVar    :: forall tag t1   . (SomeFLType t1               ) => FLVar tag t1 -> FLExp tag t1
  FLELambda :: forall tag t1 t2. (SomeFLType t1, SomeFLType t2) => FLVar tag t1 -> FLExp tag t2  -> FLExp tag ('FLTLambda t1 t2)
  FLEApp    :: forall tag t1 t2. (SomeFLType t1, SomeFLType t2) => FLExp tag ('FLTLambda t1 t2) -> FLExp tag t1 -> FLExp tag t2
  FLELet    :: forall tag t1   . (SomeFLType t1               ) => [FLVarDecl tag] -> FLExp tag t1 -> FLExp tag t1
  --FLEPrim   :: forall tag t    . (SomeFLType t                ) => Text -> FLExp tag t
  --UnsafeFLECast :: forall tag t1 t2. FLType -> FLExp tag t1 -> FLExp tag t2

instance Show tag => Show (FLExp tag t) where
  show = T.unpack . prettyPrintFLExp

prettyPrintFLExp :: Show tag => FLExp tag t -> Text
prettyPrintFLExp e = 
  case e of
    (FLEValI i)      -> tshow i
    (FLEValB b)      -> tshow b
    (FLEVar v)       -> tshow v
    (FLELambda v b)  -> "(\\" <> tshow v <> " -> " <> prettyPrintFLExp b <> ")"
    (FLEApp e1 e2)   -> "(" <>   prettyPrintFLExp e1 <> " " <> prettyPrintFLExp e2 <> ")"
    (FLELet decls b) -> "(let " <> T.intercalate "; " (prettyPrintFLVarDecl <$> decls) <> " in " <> prettyPrintFLExp b <> ")"
    --(FLEPrim p)      -> p
    --(UnsafeFLECast t e) -> prettyPrintFLExp e --"(" <> prettyPrintFLExp e <> " :: " <> tshow t <> ")"
data FLProgram (tag :: Type) = FLProgram {
      flpTopLevelVars :: M.Map tag (FLVarDecl tag)
  }

instance Show tag => Show (FLProgram tag) where
  show = T.unpack . prettyPrintFLProgram

prettyPrintFLProgram :: Show tag => FLProgram tag -> Text
prettyPrintFLProgram (FLProgram decls) = T.intercalate "\n" (prettyPrintFLVarDecl <$> M.elems decls)
