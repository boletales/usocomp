{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
  Module      : SimpleLang.Tools.Manual
  Description : SimpleLangを手書きするためのツール。re-exportしないこと！
-}

module SimpleLang.Tools.Manual (
    slmNewVar
  , slmNewNamedVar
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
  , slmClsTailCall
  , SLMFuncsM
  , runSLMFuncsM
  , hsFuncToSLFuncBlock
  , _cls
  , _const
  , _local
  , _arg
  , _reflocal
  , _ptr
  , _refptr
  , _funcptr
  , (<<-)
  , (>:)
  , _add
  , _sub
  , _mul
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
  , type (-->)
  , type (!-->)
) where

import MyPrelude

import SimpleLang.Tools.Manual.Internal
import SimpleLang.Def
import SimpleLang.TypedDef
import Data.Vector as V
import Control.Monad.State
import Data.Proxy
import Data.Text as T

type (-->) args ret = TypedSLFuncBlock args ret
type (!-->) args ret = 'SLTFuncPtr args ret

slmNewVar :: forall t r. KnownType t => TypedSLExp t -> SLManualBlockM r (SLMVar t)
slmNewVar exp = do
  SLMState cnt blocks <- get
  put (SLMState (cnt + (tslTypeVal >>> sltSizeOf) (Proxy :: Proxy t)) blocks)
  let varname = tshow $ cnt
  slmStmt (TSLSInitVar varname exp)
  pure (SLMVar varname)

slmNewNamedVar :: forall t r. KnownType t => Text -> TypedSLExp t -> SLManualBlockM r (SLMVar t)
slmNewNamedVar varname exp = do
  SLMState cnt blocks <- get
  put (SLMState (cnt + (tslTypeVal >>> sltSizeOf) (Proxy :: Proxy t)) blocks)
  slmStmt (TSLSInitVar varname exp)
  pure (SLMVar varname)

slmStmt :: TypedSLStatement -> SLManualBlockM r ()
slmStmt stmt = do
  SLMState cnt blocks <- get
  put (SLMState cnt (V.snoc blocks (TSLBSingle stmt)))

slmBlk :: TypedSLBlock -> SLManualBlockM r ()
slmBlk block = do
  SLMState cnt blocks <- get
  put (SLMState cnt (V.snoc blocks block))

slmWhile :: TypedSLExp 'SLTInt -> SLManualBlockM r () -> SLManualBlockM r ()
slmWhile cond body = do
  block <- clipslm body
  slmBlk (TSLBWhile cond block)

slmCase :: V.Vector (TypedSLExp 'SLTInt, SLManualBlockM r ()) -> SLManualBlockM r () -> SLManualBlockM r ()
slmCase cases elsecase = do
  cases' <- V.mapM (\(cond, body) -> do
      block <- clipslm body
      pure (cond, block)
    ) cases
  elsecase' <- clipslm elsecase
  slmBlk (TSLBCase cases' elsecase')

slmReturn :: KnownType r => TypedSLExp r -> SLManualBlockM r ()
slmReturn expr = slmStmt (TSLSReturn expr)

slmClsTailCall :: forall args ret. (KnownTypes ('SLTFuncPtr args ret ': args), KnownType ret) => TypedSLExp ('SLTStruct ('SLTFuncPtr args ret ': args)) -> SLManualBlockM ret ()
slmClsTailCall cls = slmStmt (TSLSTailCallReturn (TSLClosureCall cls))

_const :: Int -> TypedSLExp 'SLTInt
_const = SLVal >>> TSLEConst

_local :: KnownType t =>SLMVar t -> TypedSLExp t
_local = unSLMVar >>> TSLELocal

_arg :: KnownType t => SLMArg t -> TypedSLExp t
_arg = unSLMArg >>> TSLEArg

_reflocal :: KnownType t => SLMVar t -> TypedSLRef t
_reflocal = unSLMVar >>> TSLRefLocal

_ptr :: KnownType t => TypedSLRef t -> TypedSLExp ('SLTPtr t)
_ptr = TSLEAddrOf

_refptr :: KnownType t => TypedSLExp ('SLTPtr t) -> TypedSLRef t
_refptr = TSLRefPtr

_cls :: (KnownTypes ('SLTFuncPtr ts t ': ts), KnownType t) => TypedSLExp ('SLTStruct ('SLTFuncPtr ts t ': ts)) -> TypedSLExp t
_cls = TSLClosureCall >>> TSLEPushCall

_funcptr :: forall a b. (a --> b) -> TypedSLExp ('SLTFuncPtr a b)
_funcptr = tslfSignature >>> TSLEFuncPtr

slmFundef :: SLManualBlockM r () -> SLMNaryF '[] (SLManualBlockM r ())
slmFundef = id

infix 1 <<-

(<<-) :: KnownType t => TypedSLRef t -> TypedSLExp t -> SLManualBlockM r ()
(<<-) a b = slmStmt (TSLSSubst a b)

infixr 2 >:

(>:) :: (KnownType t, KnownTypes ts ) => TypedSLExp t -> TypedSLExp ('SLTStruct ts) -> TypedSLExp ('SLTStruct (t:ts))
(>:) = TSLEStructCons



{- TSLEPrim2のラッパ -}

type P2I = TypedSLExp 'SLTInt -> TypedSLExp 'SLTInt -> TypedSLExp 'SLTInt
type P1I = TypedSLExp 'SLTInt -> TypedSLExp 'SLTInt

_add  :: P2I
_add = TSLEPrim2 SLPrim2Add

_sub  :: P2I
_sub = TSLEPrim2 SLPrim2Sub

_mul :: P2I
_mul = TSLEPrim2 SLPrim2Mult

_shift :: P2I
_shift = TSLEPrim2 SLPrim2Shift

_and :: P2I
_and = TSLEPrim2 SLPrim2And

_or :: P2I
_or = TSLEPrim2 SLPrim2Or

_xor :: P2I
_xor = TSLEPrim2 SLPrim2Xor

_gt :: P2I
_gt = TSLEPrim2 SLPrim2Gt

_lt :: P2I
_lt = TSLEPrim2 SLPrim2Lt

_eq :: P2I
_eq = TSLEPrim2 SLPrim2Eq

{- TSLEPrim1のラッパ -}

_inv :: P1I
_inv = TSLEPrim1 SLPrim1Inv