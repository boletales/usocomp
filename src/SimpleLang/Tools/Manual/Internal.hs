{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SimpleLang.Tools.Manual.Internal where

import SimpleLang.Def
import Data.Vector as V
import Control.Monad.State as S
import Control.Category
import Prelude hiding ((.), id)
import Data.Proxy
import Data.Kind
import Data.Map qualified as M
import GHC.TypeNats
import SimpleLang.TypedDef

newtype SLMVar (t :: SLType) = SLMVar Int deriving (Show, Eq, Ord)

unSLMVar :: SLMVar t -> Int
unSLMVar (SLMVar x) = x

newtype SLMArg (t :: SLType) = SLMArg Int deriving (Show, Eq, Ord)

unSLMArg :: SLMArg t -> Int
unSLMArg (SLMArg x) = x

newtype SLMBlock (ret :: SLType) = SLMBlock TypedSLBlock

unSLMBlock :: SLMBlock ret -> TypedSLBlock
unSLMBlock (SLMBlock x) = x

data SLMState (ret :: SLType) = SLMState {
      slmVarCnt :: Int              -- 今のスコープで有効なローカル変数の個数
    , slmBlocks :: V.Vector TypedSLBlock -- コード
  }

type SLManualBlockM ret = State (SLMState ret)

-- 今のスコープで有効なローカル変数の個数を覚えたままコード片を抽出
clipslm :: SLManualBlockM ret () -> SLManualBlockM ret TypedSLBlock
clipslm m = do
  cnt <- gets slmVarCnt
  let SLMState _ blocks = execState m (SLMState cnt V.empty)
  pure (TSLBMulti blocks)

{-
runslm :: Int -> SLFuncName -> SLManualBlockM ret () -> TypedSLFuncBlock args ret
runslm args name m =
  let SLMState _ blocks = execState m (SLMState 0 V.empty)
  in SLFuncBlock name args (SLBMulti blocks)
-}

class SLMNAryC (args :: [SLType]) where
  type SLMNaryF args (x :: Type) -- :: t[0] -> t[1] -> ... -> r
  slmfuncToHsFunc       :: (TypedSLExp ('SLTStruct args) -> x) -> SLMNaryF args x
  hsFuncToSLMFuncHelper :: Int -> SLMNaryF args x -> x

instance SLMNAryC '[] where
  type SLMNaryF '[] x = x
  slmfuncToHsFunc       f          = f TSLEStructNil
  hsFuncToSLMFuncHelper _ f = f

instance forall args newarg. (SLMNAryC args , KnownType newarg, KnownTypes args) => SLMNAryC (newarg : args) where
  type SLMNaryF (newarg : args) x = TypedSLExp newarg -> SLMNaryF args x
  slmfuncToHsFunc       f arg       = slmfuncToHsFunc (f . TSLEStructCons arg)
  hsFuncToSLMFuncHelper wordscnt f  = (hsFuncToSLMFuncHelper @args)  (wordscnt + (tslTypeVal >>> sltSizeOf) (Proxy :: Proxy newarg)) (f (TSLEArg wordscnt :: TypedSLExp newarg))

hsFuncToSLMFunc :: forall args ret. (SLMNAryC args) => SLFuncName -> SLMNaryF args (SLManualBlockM ret ()) -> TypedSLFuncBlock args ret
hsFuncToSLMFunc name f =
  TSLFuncBlock {
      tslfName     = TypedSLFuncName name :: TypedSLFuncName args ret
    , tslfBlock    =  (hsFuncToSLMFuncHelper @args @(SLManualBlockM ret ()) 0
                        >>> flip execState (SLMState 0 V.empty)
                        >>> slmBlocks
                        >>> TSLBMulti ) f
  }

_app :: forall args ret t. (TypedSLCallable args ret t, SLMNAryC args, KnownType ret) => t -> SLMNaryF args (TypedSLExp ret)
_app callable = slmfuncToHsFunc (TSLEPushCall . tslCall callable) :: SLMNaryF args (TypedSLExp ret)

slmTailCall :: forall args ret t. (SLMNAryC args, TypedSLCallable args ret t, KnownType ret) => t -> SLMNaryF args (SLManualBlockM ret ())
slmTailCall x = slmfuncToHsFunc $ (\(args :: TypedSLExp ('SLTStruct args)) -> do
    SLMState cnt blocks <- get
    put (SLMState cnt (V.snoc blocks (TSLBSingle (TSLSTailCallReturn (tslCall @args @ret x args)))))
    (pure () :: SLManualBlockM ret ())
  )
newtype SLMFuncsM x =
      SLMFuncsM (State (M.Map SLFuncName SLFuncBlock) x)
    deriving (Functor, Applicative, Monad, MonadState (M.Map SLFuncName SLFuncBlock))

runSLMFuncsM :: SLMFuncsM () -> SLProgram
runSLMFuncsM (SLMFuncsM m) = execState m M.empty

--  ((natVal >>> fromIntegral) (Proxy :: Proxy n))
slmFunc :: forall (args :: [SLType]) (ret :: SLType). (SLMNAryC args, KnownNat (SLTSizeOf ('SLTStruct args))) => SLFuncName -> SLMNaryF args (SLManualBlockM ret ()) -> SLMFuncsM (TypedSLFuncBlock args ret)
slmFunc name f = do
  let fblock = hsFuncToSLMFunc @args @ret name f
  S.modify (M.insert name (unTypedSLFuncBlock fblock))
  pure fblock


slmVirtualFunc :: SLFuncName -> TypedSLFuncBlock args ret
slmVirtualFunc name =
  TSLFuncBlock {
      tslfName     = TypedSLFuncName name :: TypedSLFuncName args ret
    , tslfBlock    = TSLBMulti V.empty
  }

slmSetRealFunc :: forall (args :: [SLType]) (ret :: SLType). (SLMNAryC args, KnownNat (SLTSizeOf ('SLTStruct args))) => TypedSLFuncBlock args ret -> SLMNaryF args (SLManualBlockM ret ()) ->  SLMFuncsM ()
slmSetRealFunc v f = void $ slmFunc @args @ret (unTypedSLFuncName $ tslfName v) f
