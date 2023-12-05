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

newtype SLMVar = SLMVar Int deriving (Show, Eq, Ord)

unSLMVar :: SLMVar -> Int
unSLMVar (SLMVar x) = x

newtype SLMArg = SLMArg Int deriving (Show, Eq, Ord)

unSLMArg :: SLMArg -> Int
unSLMArg (SLMArg x) = x

newtype SLMBlock (ret :: SLType) = SLMBlock SLBlock

unSLMBlock :: SLMBlock ret -> SLBlock
unSLMBlock (SLMBlock x) = x

data SLMState (ret :: SLType) = SLMState {
      slmVarCnt :: Int              -- 今のスコープで有効なローカル変数の個数
    , slmBlocks :: V.Vector SLBlock -- コード
  }

type SLManualBlockM ret = State (SLMState ret)

-- 今のスコープで有効なローカル変数の個数を覚えたままコード片を抽出
clipslm :: SLManualBlockM ret () -> SLManualBlockM ret SLBlock
clipslm m = do
  cnt <- gets slmVarCnt
  let SLMState _ blocks = execState m (SLMState cnt V.empty)
  pure (SLBMulti blocks)

{-
runslm :: Int -> SLFuncName -> SLManualBlockM ret () -> TypedSLFuncBlock args ret
runslm args name m =
  let SLMState _ blocks = execState m (SLMState 0 V.empty)
  in SLFuncBlock name args (SLBMulti blocks)
-}

class SLMNAryC (args :: [SLType]) where
  type SLMNaryF args (x :: Type) -- :: t[0] -> t[1] -> ... -> r
  slmfuncToHsFunc       :: (SLExp ('SLTStruct args) -> x) -> SLMNaryF args x
  hsFuncToSLMFuncHelper :: Int -> SLMNaryF args x -> x

instance SLMNAryC '[] where
  type SLMNaryF '[] x = x
  slmfuncToHsFunc       f          = f SLEStructNil
  hsFuncToSLMFuncHelper _ f = f

instance forall args newarg. (SLMNAryC args , KnownNat (SLTSizeOf newarg)) => SLMNAryC (newarg : args) where
  type SLMNaryF (newarg : args) x = SLExp newarg -> SLMNaryF args x
  slmfuncToHsFunc       f expr      = slmfuncToHsFunc (f . SLEStructCons expr)
  hsFuncToSLMFuncHelper wordscnt f  = (hsFuncToSLMFuncHelper @args)  (wordscnt + (natVal >>> fromIntegral) (Proxy :: Proxy (SLTSizeOf newarg))) (f (SLEArg wordscnt :: SLExp newarg))

hsFuncToSLMFunc :: forall args ret. (SLMNAryC args) => SLFuncName -> SLMNaryF args (SLMBlock ret) -> TypedSLFuncBlock args ret
hsFuncToSLMFunc name f = 
  TSLFuncBlock {
      tslfName     = TypedSLFuncName name :: TypedSLFuncName args ret
    , tslfBlock    = unSLMBlock $ hsFuncToSLMFuncHelper @args @(SLMBlock ret) 0 f
  }

_app :: forall args ret t. (SLMNAryC args, SLCallable args ret t) => t -> SLMNaryF args (SLCall ret)
_app = slCall >>> (slmfuncToHsFunc :: (SLExp ('SLTStruct args) -> SLCall ret) -> SLMNaryF args (SLCall ret))

slmTailCall :: forall args ret t. (SLMNAryC args, SLCallable args ret t) => t -> SLMNaryF args (SLManualBlockM ret ())
slmTailCall x = slmfuncToHsFunc $ (\(args :: SLExp ('SLTStruct args)) -> do
    SLMState cnt blocks <- get
    put (SLMState cnt (V.snoc blocks (SLBSingle (SLSTailCallReturn (slCall @args @ret x args)))))
    (pure () :: SLManualBlockM ret ())
  )
newtype SLMFuncsM x =
      SLMFuncsM (State (M.Map SLFuncName SLFuncBlock) x)
    deriving (Functor, Applicative, Monad, MonadState (M.Map SLFuncName SLFuncBlock))

runSLMFuncsM :: SLMFuncsM () -> SLProgram
runSLMFuncsM (SLMFuncsM m) = execState m M.empty

--  ((natVal >>> fromIntegral) (Proxy :: Proxy n))
slmFunc :: forall (args :: [SLType]) (ret :: SLType). (SLMNAryC args, KnownNat (SLTSizeOf ('SLTStruct args))) => SLFuncName -> SLMNaryF args (SLMBlock ret) -> SLMFuncsM (TypedSLFuncBlock args ret)
slmFunc name f = do
  let fblock = hsFuncToSLMFunc @args @ret name f
  S.modify (M.insert name (unTypedSLFuncBlock fblock))
  pure fblock


slmVirtualFunc :: SLFuncName -> TypedSLFuncBlock args ret
slmVirtualFunc name = 
  TSLFuncBlock {
      tslfName     = TypedSLFuncName name :: TypedSLFuncName args ret
    , tslfBlock    = SLBMulti V.empty
  }

slmSetRealFunc :: forall (args :: [SLType]) (ret :: SLType). (SLMNAryC args, KnownNat (SLTSizeOf ('SLTStruct args))) => TypedSLFuncBlock args ret -> SLMNaryF args (SLMBlock ret) ->  SLMFuncsM ()
slmSetRealFunc v f = void $ slmFunc @args @ret (unTypedSLFuncName $ tslfName v) f
