{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Internal.MyNat where

import Data.Kind
import GHC.TypeNats hiding (natVal)
import Data.Proxy
import Control.Category
import Prelude hiding ((.), id)
import Data.List qualified as L


data MyNat =
        MyZero
      | MySucc MyNat

class KnownMyNat (n :: MyNat) where
  natVal :: Proxy n -> Int

instance KnownMyNat 'MyZero where
  natVal _ = 0

instance KnownMyNat n => KnownMyNat ('MySucc n) where
  natVal _ = 1 + natVal (Proxy :: Proxy n)

type family FromGHCNat (n :: Nat) = (m :: MyNat) where
  FromGHCNat 0 = 'MyZero
  FromGHCNat n = 'MySucc (FromGHCNat (n - 1))

type family ToGHCNat (m :: MyNat) = (n :: Nat) where
  ToGHCNat 'MyZero = 0
  ToGHCNat ('MySucc n) = 1 + ToGHCNat n

-- NAryFamD a b n := a -> a -> ... -> a -> b
class NAryFamC (n :: MyNat) where
  type NAryFamD a b n
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


-- >>> naryCreate (Proxy :: Proxy ('MySucc ('MySucc ('MySucc MyZero)))) id 1 2 3
-- [1,2,3]
