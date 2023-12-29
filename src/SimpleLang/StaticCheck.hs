{-# LANGUAGE OverloadedStrings #-}

module SimpleLang.StaticCheck where

import SimpleLang.Def
import Control.Monad
import Control.Category
import Data.Text as T hiding (
    words
  )
import Data.Map as M
import Data.Set as S
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable as F
import Prelude hiding (
    id
  , (.)
  , lookup
  , foldl
  , words
  , exp
  )


data SLSCFlags = SLSCFlags {
  } deriving (Show)
{-
slscCheck' :: SLSCFlags -> SLProgram -> Either Text ()
slscCheck' flags decls = 
  forM_ (M.assocs decls) (\(fname, fblock) -> do
            let SLFuncBlock sig block = fblock
            let SLFuncSignature name args rettype = sig
            if name /= fname
              then throwError $ "function name mismatch"
              else pure ()
            
            let argset =
                  S.fromList $ snd $ F.foldl (\(argpos, argexps) t -> (argpos + sltSizeOf t, SLEArg t argpos : argexps)) (0, []) args

            _ <- (`evalState` S.empty) $ runExceptT
              let go 


            pure ()            
        )
-}