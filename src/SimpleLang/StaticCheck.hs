module SimpleLang.StaticCheck where

import SimpleLang.Def
import Control.Monad
import Control.Category
import Prelude hiding ((.), id)
import Data.Text as T
import Data.Map as M
import Control.Monad.State
import Control.Monad.Except

slscCheck :: SLProgram -> Either Text ()
slscCheck (SLProgram decls) = 
  forM_ (M.assocs decls) (\(fname, fblock) ->do
        let SLFuncBlock sig block = fblock
        let SLFuncSignature name args ret = sig
        if name /= fname
          then throwError $ "function name mismatch"
          else pure ()
        
    )