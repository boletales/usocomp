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
  , slmReturn
  , slmFunc
  , slmVirtualFunc
  , slmSetRealFunc
  , slmFundef
  , slmTailCall
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
  , SLMArg
  , SLMVar
  , SLMState
  , SLManualBlockM
  , SLMNaryF
) where

import SimpleLang.Tools.Manual.Internal
import SimpleLang.Def
import Data.Vector as V
import Control.Monad.State
import Control.Category
import Prelude hiding ((.), id, exp)
import GHC.TypeNats
import Data.Proxy

slmNewVar :: forall t r. (KnownNat (SLTSizeOf t)) => TypedSLExp t -> SLManualBlockM r (SLMVar t)
slmNewVar exp = do
  SLMState cnt blocks <- get
  let newVarId = cnt
  put (SLMState (cnt + (natVal >>> fromIntegral) (Proxy :: Proxy (SLTSizeOf t))) blocks)
  slmStmt (SLSInitVar newVarId exp)
  pure (SLMVar newVarId)

slmStmt :: SLStatement -> SLManualBlockM r ()
slmStmt stmt = do
  SLMState cnt blocks <- get
  put (SLMState cnt (V.snoc blocks (SLBSingle stmt)))

slmBlk :: SLBlock -> SLManualBlockM r ()
slmBlk block = do
  SLMState cnt blocks <- get
  put (SLMState cnt (V.snoc blocks block))

slmWhile :: TypedSLExp 'SLTInt -> SLManualBlockM r () -> SLManualBlockM r ()
slmWhile cond body = do
  block <- clipslm body
  slmBlk (SLBWhile cond block)

slmCase :: V.Vector (TypedSLExp 'SLTInt, SLManualBlockM r ()) -> SLManualBlockM r () -> SLManualBlockM r ()
slmCase cases elsecase = do
  cases' <- V.mapM (\(cond, body) -> do
      block <- clipslm body
      pure (cond, block)
    ) cases
  elsecase' <- clipslm elsecase
  slmBlk (SLBCase cases' elsecase')

slmReturn :: KnownSize r => TypedSLExp r -> SLManualBlockM r ()
slmReturn expr = slmStmt (SLSReturn expr)

_const :: Int -> TypedSLExp 'SLTInt
_const = SLVal >>> SLEConst

_local :: KnownSize t =>SLMVar t -> TypedSLExp t
_local = unSLMVar >>> SLELocal

_arg :: KnownSize t => SLMArg t -> TypedSLExp t
_arg = unSLMArg >>> SLEArg

_reflocal :: KnownSize t => SLMVar t -> SLRef t
_reflocal = unSLMVar >>> SLRefLocal

_ptr :: KnownSize t => SLRef t -> TypedSLExp ('SLTPtr t)
_ptr = SLEPtr

_refptr :: KnownSize t => TypedSLExp ('SLTPtr t) -> SLRef t
_refptr = SLRefPtr

slmFundef :: SLManualBlockM r () -> SLMNaryF '[] (SLManualBlockM r ())
slmFundef = id

infix 1 <<-

(<<-) :: KnownSize t => SLRef t -> TypedSLExp t -> SLManualBlockM r ()
(<<-) a b = slmStmt (SLSSubst a b)



{- SLEPrim2のラッパ -}

type P2I = TypedSLExp 'SLTInt -> TypedSLExp 'SLTInt -> TypedSLExp 'SLTInt
type P1I = TypedSLExp 'SLTInt -> TypedSLExp 'SLTInt

_add  :: P2I
_add = SLEPrim2 SLPrim2Add

_sub  :: P2I
_sub = SLEPrim2 SLPrim2Sub

_mult :: P2I
_mult = SLEPrim2 SLPrim2Mult

_shift :: P2I
_shift = SLEPrim2 SLPrim2Shift

_and :: P2I
_and = SLEPrim2 SLPrim2And

_or :: P2I
_or = SLEPrim2 SLPrim2Or

_xor :: P2I
_xor = SLEPrim2 SLPrim2Xor

_gt :: P2I
_gt = SLEPrim2 SLPrim2Gt

_lt :: P2I
_lt = SLEPrim2 SLPrim2Lt

_eq :: P2I
_eq = SLEPrim2 SLPrim2Eq

{- SLEPrim1のラッパ -}

_inv :: P1I
_inv = SLEPrim1 SLPrim1Inv