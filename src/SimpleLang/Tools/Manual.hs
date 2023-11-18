{-# LANGUAGE DataKinds #-}

{-|
  Module      : SimpleLang.Tools.Manual
  Description : SimpleLangを手書きするためのツール。re-exportしないこと！
-}

module SimpleLang.Tools.Manual (
    slmNewVar
  , slmStmt
  , slmBlk
  , slmWhile
  , slmCase
  , slmFunc
  , SLMFuncsM
  , runSLMFuncsM
  , _const
  , _local
  , _arg
  , _reflocal
  , _ptr
  , _refptr
  , (<<-)
  , _add
  , _sub
  , _mult
  , _shift
  , _and
  , _or
  , _xor
  , _gt
  , _lt
  , _eq
  , _inv
  , _app
  , fundef
  , SLMArg
  , SLMVar
  , SLMState
  , SLManualBlockM
) where

import SimpleLang.Tools.Manual.Internal
import SimpleLang.Def
import qualified Data.Foldable as M
import Data.Vector as V
import Control.Monad.State
import Control.Category
import Prelude hiding ((.), id)

slmNewVar :: SLExp -> SLManualBlockM SLMVar
slmNewVar exp = do
  SLMState cnt blocks <- get
  let newVarId = cnt
  put (SLMState (cnt + 1) blocks)
  slmStmt (SLSInitVar exp)
  pure (SLMVar newVarId)

slmStmt :: SLStatement -> SLManualBlockM ()
slmStmt stmt = do
  SLMState cnt blocks <- get
  put (SLMState cnt (V.snoc blocks (SLBSingle stmt)))

slmBlk :: SLBlock -> SLManualBlockM ()
slmBlk block = do
  SLMState cnt blocks <- get
  put (SLMState cnt (V.snoc blocks block))

slmWhile :: SLExp -> SLManualBlockM () -> SLManualBlockM ()
slmWhile cond body = do
  block <- clipslm body
  slmBlk (SLBWhile cond block)

slmCase :: V.Vector (SLExp, SLManualBlockM ()) -> SLManualBlockM () -> SLManualBlockM ()
slmCase cases elsecase = do
  cases' <- V.mapM (\(cond, body) -> do
      block <- clipslm body
      pure (cond, block)
    ) cases
  elsecase' <- clipslm elsecase
  slmBlk (SLBCase cases' elsecase')


_const :: Int -> SLExp
_const = SLVal >>> SLEConst

_local :: SLMVar -> SLExp
_local = unSLMVar >>> SLELocal

_arg :: SLMArg -> SLExp
_arg = unSLMArg >>> SLEArg

_reflocal :: SLMVar -> SLRef
_reflocal = unSLMVar >>> SLRefLocal

_ptr :: SLExp -> SLExp
_ptr = SLEPtr

_refptr :: SLExp -> SLRef
_refptr = SLRefPtr

fundef :: SLManualBlockM () -> NAryFamD SLMArg (SLManualBlockM ()) 'MyZero
fundef = id

infix 1 <<-

(<<-) :: SLRef -> SLExp -> SLManualBlockM ()
(<<-) a b = slmStmt (SLSSubst a b)


{- SLEPrim2のラッパ -}

_add :: SLExp -> SLExp -> SLExp
_add = SLEPrim2 SLPrim2Add

_sub :: SLExp -> SLExp -> SLExp
_sub = SLEPrim2 SLPrim2Sub

_mult :: SLExp -> SLExp -> SLExp
_mult = SLEPrim2 SLPrim2Mult

_shift :: SLExp -> SLExp -> SLExp
_shift = SLEPrim2 SLPrim2Shift

_and :: SLExp -> SLExp -> SLExp
_and = SLEPrim2 SLPrim2And

_or :: SLExp -> SLExp -> SLExp
_or = SLEPrim2 SLPrim2Or

_xor :: SLExp -> SLExp -> SLExp
_xor = SLEPrim2 SLPrim2Xor

_gt :: SLExp -> SLExp -> SLExp
_gt = SLEPrim2 SLPrim2Gt

_lt :: SLExp -> SLExp -> SLExp
_lt = SLEPrim2 SLPrim2Lt

_eq :: SLExp -> SLExp -> SLExp
_eq = SLEPrim2 SLPrim2Eq

{- SLEPrim1のラッパ -}

_inv :: SLExp -> SLExp
_inv = SLEPrim1 SLPrim1Inv