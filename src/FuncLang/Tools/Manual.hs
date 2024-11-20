module FuncLang.Tools.Manual (flmLam, flmDecl, flmApp, runFLM) where

import FuncLang.Def
import Data.Text as T
import Prelude hiding ((.), id)
import Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Identity

type MonadFLM = StateT (M.Map Text (FLVarDecl Text)) Identity

flmLam :: (SomeFLType u, SomeFLType t) => Text -> (FLExp Text t -> FLExp Text u) -> FLExp Text ('FLTLambda t u)
flmLam t f = FLELambda (FLVar t) (f (FLEVar (FLVar t)))

flmDecl :: (SomeFLType t) =>Text -> FLExp Text t -> MonadFLM (FLExp Text t)
flmDecl t e = do
    modify (M.insert t (FLVarDecl (FLVar t) e))
    pure (FLEVar (FLVar t))


flmApp :: (SomeFLType t1, SomeFLType t) => FLExp tag ('FLTLambda t1 t) -> FLExp tag t1 -> FLExp tag t
flmApp = FLEApp

runFLM :: MonadFLM () -> FLProgram Text
runFLM m = FLProgram (runIdentity (execStateT m M.empty))