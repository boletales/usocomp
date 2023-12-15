module FuncLang.Tools.Manual where

import FuncLang.Def
import Data.Text as T
import Control.Category
import Prelude hiding ((.), id)
import Data.Map as M
import Control.Monad.State
import Control.Monad.Identity

type MonadFLM = StateT (M.Map Text (FLVarDecl Text)) Identity

flmLam :: Text -> (FLExp Text t -> FLExp Text u) -> FLExp Text ('FLTLambda t u)
flmLam t f = FLELambda (FLVar t) (f (FLEVar (FLVar t)))

flmDecl :: Text -> FLExp Text t -> MonadFLM (FLExp Text t)
flmDecl t e = do
    modify (M.insert t (FLVarDecl (FLVar t) e))
    pure (FLEVar (FLVar t))


flmApp :: FLExp tag ('FLTLambda t1 t) -> FLExp tag t1 -> FLExp tag t
flmApp = FLEApp

runFLM :: MonadFLM () -> FLProgram Text
runFLM m = FLProgram (runIdentity (execStateT m M.empty))