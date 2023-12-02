{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

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
import Internal.MyNat

newtype SLMVar = SLMVar Int deriving (Show, Eq, Ord)

unSLMVar :: SLMVar -> Int
unSLMVar (SLMVar x) = x

newtype SLMArg = SLMArg Int deriving (Show, Eq, Ord)

unSLMArg :: SLMArg -> Int
unSLMArg (SLMArg x) = x

data SLMState = SLMState {
      slmVarCnt :: Int              -- 今のスコープで有効なローカル変数の個数
    , slmBlocks :: V.Vector SLBlock -- コード
  }

type SLManualBlockM = State SLMState

-- 今のスコープで有効なローカル変数の個数を覚えたままコード片を抽出
clipslm :: SLManualBlockM () -> SLManualBlockM SLBlock
clipslm m = do
  cnt <- gets slmVarCnt
  let SLMState _ blocks = execState m (SLMState cnt V.empty)
  pure (SLBMulti blocks)

runslm :: Int -> SLFuncName -> SLManualBlockM () -> SLFuncBlock
runslm args name m =
  let SLMState _ blocks = execState m (SLMState 0 V.empty)
  in SLFuncBlock name args (SLBMulti blocks)


newtype SLMFunc (n :: MyNat) =
  SLMFunc SLFuncBlock

type family SLMFuncOf (n :: Nat) where
  SLMFuncOf n = SLMFunc (FromGHCNat n)

-- NAryFamD a b n := a -> a -> ... -> a -> b
class NAryFamC (t :: [SLType]) where
  type NAryFamD t
  naryCreateHelper :: [a] -> Proxy n -> ([a] -> b) -> NAryFamD a b n
  naryCrush  :: Proxy n -> (ctr -> (a, ctr)) -> ctr -> NAryFamD a b n -> b

instance NAryFamC 'MyZero where
  type NAryFamD a b 'MyZero = b
  naryCreateHelper l _ f = (L.reverse >>> f) l
  naryCrush _ _ _ = id

instance NAryFamC n => NAryFamC ('MySucc n) where
  type NAryFamD a b ('MySucc n) = a -> NAryFamD a b n
  naryCreateHelper xs _ f x = naryCreateHelper (x:xs) (Proxy :: Proxy n) f
  naryCrush _ arggen ctr f = 
    let (a, ctr') = arggen ctr
    in  naryCrush (Proxy :: Proxy n) arggen ctr' (f a)

naryCreate :: forall (a :: Type) (b :: Type) (n :: MyNat).
                NAryFamC n => Proxy n -> ([a] -> b) -> NAryFamD a b n
naryCreate = naryCreateHelper []

naryCreateList :: forall (a :: Type) (n :: MyNat).
                    NAryFamC n => Proxy n -> NAryFamD a [a] n
naryCreateList p = naryCreateHelper [] p (id :: [a] -> [a])


_app :: forall n. NAryFamC n => SLMFunc n -> NAryFamD SLExp SLExp n
_app (SLMFunc (SLFuncBlock name _ _)) =
  naryCreate (Proxy :: Proxy n) (V.fromList >>> SLSolidFuncCall name >>> SLEPushCall)

slmTailCall :: forall n. NAryFamC n => SLMFunc n -> NAryFamD SLExp (SLManualBlockM ()) n
slmTailCall (SLMFunc (SLFuncBlock name _ _)) = 
  naryCreate (Proxy :: Proxy n) ((\l -> do
      SLMState cnt blocks <- get
      put (SLMState cnt (V.snoc blocks (SLBSingle (SLSTailCallReturn (SLSolidFuncCall name (V.fromList l)) ))))
    ) :: [SLExp] -> SLManualBlockM ())
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


slmFunc :: forall (n :: MyNat) (t :: Type). (KnownMyNat n, NAryFamC n, SLMFGenNAryC n t) => SLFuncName -> t -> SLMFuncsM (SLMFunc n)
slmFunc name f = do
  let fblock = runslm (natVal (Proxy :: Proxy n)) name $ naryCrush (Proxy :: Proxy n) (\(i :: Int) -> (SLMArg i, i+1)) 0 (toNAryFamD (Proxy :: Proxy n) f)
  S.modify (M.insert name fblock)
  pure (SLMFunc fblock)



slmVirtualFunc :: forall (n :: MyNat). KnownMyNat n => SLFuncName -> SLMFunc n
slmVirtualFunc name = SLMFunc (SLFuncBlock name (natVal (Proxy :: Proxy n)) (SLBMulti V.empty))

slmSetRealFunc :: forall (n :: MyNat) (t :: Type). (KnownMyNat n, NAryFamC n, SLMFGenNAryC n t) => SLMFunc n -> t -> SLMFuncsM ()
slmSetRealFunc (SLMFunc (SLFuncBlock name _ _)) f = void $ slmFunc name f
