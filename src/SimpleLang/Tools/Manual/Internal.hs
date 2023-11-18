{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module SimpleLang.Tools.Manual.Internal where

import SimpleLang.Def
import Data.Vector as V
import Control.Monad.State as S
import Control.Category
import Prelude hiding ((.), id)
import Data.Proxy
import Data.Kind
import Data.List qualified as L
import Data.Map qualified as M
import GHC.TypeNats hiding (natVal)


newtype SLMVar = SLMVar Int deriving (Show, Eq, Ord)

unSLMVar :: SLMVar -> Int
unSLMVar (SLMVar x) = x

newtype SLMArg = SLMArg Int deriving (Show, Eq, Ord)

unSLMArg :: SLMArg -> Int
unSLMArg (SLMArg x) = x

data SLMState = SLMState {
      slmVarCnt :: Int
    , slmBlocks :: V.Vector SLBlock
  }

type SLManualBlockM = State SLMState

clipslm :: SLManualBlockM () -> SLManualBlockM SLBlock
clipslm m = do
  cnt <- gets slmVarCnt
  let SLMState _ blocks = execState m (SLMState cnt V.empty)
  pure (SLBMulti blocks)

runslm :: SLFuncName -> SLManualBlockM () -> SLFuncBlock
runslm name m =
  let SLMState _ blocks = execState m (SLMState 0 V.empty)
  in SLFuncBlock name (SLBMulti blocks)

data MyNat =
        MyZero
      | MySucc MyNat

class MyNatC (n :: MyNat) where
  natVal :: Proxy n -> Int

instance MyNatC 'MyZero where
  natVal _ = 0

instance MyNatC n => MyNatC ('MySucc n) where
  natVal _ = 1 + natVal (Proxy :: Proxy n)

-- type family FromGHCNat (n :: Nat) = (m :: MyNat) | m -> n where
--   FromGHCNat 0 = 'MyZero
--   FromGHCNat n = 'MySucc (FromGHCNat (n - 1))

newtype SLMFunc (n :: MyNat) =
  SLMFunc SLFuncBlock


class NAryFamC (n :: MyNat) where
  type NAryFamD a b n
  naryCreateHelper :: Proxy n -> ([a] -> b) -> [a] -> NAryFamD a b n
  naryCrush   :: Proxy n -> (Int -> a) -> NAryFamD a b n -> b

instance NAryFamC 'MyZero where
  type NAryFamD a b 'MyZero = b
  naryCreateHelper _ f = L.reverse >>> f
  naryCrush _ _ = id

instance (NAryFamC n, MyNatC n) => NAryFamC ('MySucc n) where
  type NAryFamD a b ('MySucc n) = a -> NAryFamD a b n
  naryCreateHelper _ f xs x = naryCreateHelper (Proxy :: Proxy n) f (x:xs)
  naryCrush _ arggen f = naryCrush (Proxy :: Proxy n) arggen (f (arggen (natVal (Proxy :: Proxy n))))

naryCreate :: NAryFamC n => Proxy n -> ([a] -> b) -> NAryFamD a b n
naryCreate p f = naryCreateHelper p f []

_app :: forall n. NAryFamC n => SLMFunc n -> NAryFamD SLExp SLExp n
_app (SLMFunc (SLFuncBlock name _)) =
  naryCreate (Proxy :: Proxy n) (V.fromList >>> SLEPushCall (SLSolidFunc name))

newtype SLMFuncsM x =
      SLMFuncsM (State (M.Map SLFuncName SLFuncBlock) x)
    deriving (Functor, Applicative, Monad, MonadState (M.Map SLFuncName SLFuncBlock))

runSLMFuncsM :: SLMFuncsM () -> SLProgram
runSLMFuncsM (SLMFuncsM m) = execState m M.empty

class SLMFGenNAryC (n :: MyNat) t | t -> n where
  toNAryFamD :: Proxy n -> t -> NAryFamD SLMArg (SLManualBlockM ()) n

instance SLMFGenNAryC 'MyZero (SLManualBlockM ()) where
  toNAryFamD _ = id

instance (SLMFGenNAryC n t) => SLMFGenNAryC ('MySucc n) (SLMArg -> t) where
  toNAryFamD _ f arg = toNAryFamD (Proxy :: Proxy n) (f arg)


slmFunc :: forall (n :: MyNat) (t :: Type). (NAryFamC n, SLMFGenNAryC n t) => SLFuncName -> t -> SLMFuncsM (SLMFunc n)
slmFunc name f = do
  let fblock = runslm name $ naryCrush (Proxy :: Proxy n) SLMArg (toNAryFamD (Proxy :: Proxy n) f)
  S.modify (M.insert name fblock)
  pure (SLMFunc fblock)



virtualFunc :: SLFuncName -> SLMFunc n
virtualFunc name = SLMFunc (SLFuncBlock name (SLBMulti V.empty))

setRealFunc :: forall (n :: MyNat) (t :: Type). (NAryFamC n, SLMFGenNAryC n t) => SLMFunc n -> t -> SLMFuncsM ()
setRealFunc (SLMFunc (SLFuncBlock name _)) f = void $ slmFunc name f