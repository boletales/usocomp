{-# LANGUAGE DerivingStrategies #-}

module MachineLang.Def (
    MLReg(..),
    MLVal(..),
    MLInst'(..),
    MLInst,
  ) where

import MyPrelude

import Data.Bifunctor
import Data.Bitraversable
import Data.Bifoldable

-- 接頭辞 ML: MachineLang に関連するものの型

{-| レジスタ -}
data MLReg =
    MLReg0
  | MLReg1
  | MLReg2
  | MLReg3
  | MLReg4
  | MLReg5
  | MLReg6
  | MLReg7
  | MLRegPC
  deriving (Show, Eq, Enum, Bounded)

newtype MLVal  = MLVal  Int deriving (Show, Eq)

{-| r: レジスタの型, v: 即値の型. それぞれ抽象化できるように型変数としています -}
data MLInst' r v =
    MLINop
  | MLIConst   r v   --
  | MLIAddI    r r v --
  | MLILoad    r r   --
  | MLIStore   r r   --
  | MLIAdd     r r r --
  | MLISub     r r r --
  | MLIMult    r r r --
  | MLIShift   r r r --
  | MLIAnd     r r r --
  | MLIOr      r r r --
  | MLIXor     r r r --
  | MLIEq      r r r --
  | MLIGt      r r r --
  | MLILt      r r r --
  | MLIInv     r r   --
  | MLICopy    r r   --
  | MLIJump    r     --
  | MLIIfJump  r r   --
  | MLINotJump r r   --
  deriving (Show, Eq, Functor)

instance Bifunctor MLInst' where
  bimap :: (a -> b) -> (c -> d) -> MLInst' a c -> MLInst' b d
  bimap f g x =
    case x of
      MLINop              -> MLINop
      MLIConst   r1 v     -> MLIConst   (f r1) (g v)
      MLIAddI    r1 r2 v  -> MLIAddI    (f r1) (f r2) (g v)
      MLILoad    r1 r2    -> MLILoad    (f r1) (f r2)
      MLIStore   r1 r2    -> MLIStore   (f r1) (f r2)
      MLIAdd     r1 r2 r3 -> MLIAdd     (f r1) (f r2) (f r3)
      MLISub     r1 r2 r3 -> MLISub     (f r1) (f r2) (f r3)
      MLIMult    r1 r2 r3 -> MLIMult    (f r1) (f r2) (f r3)
      MLIShift   r1 r2 r3 -> MLIShift   (f r1) (f r2) (f r3)
      MLIAnd     r1 r2 r3 -> MLIAnd     (f r1) (f r2) (f r3)
      MLIOr      r1 r2 r3 -> MLIOr      (f r1) (f r2) (f r3)
      MLIXor     r1 r2 r3 -> MLIXor     (f r1) (f r2) (f r3)
      MLIEq      r1 r2 r3 -> MLIEq      (f r1) (f r2) (f r3)
      MLIGt      r1 r2 r3 -> MLIGt      (f r1) (f r2) (f r3)
      MLILt      r1 r2 r3 -> MLILt      (f r1) (f r2) (f r3)
      MLIInv     r1 r2    -> MLIInv     (f r1) (f r2)
      MLICopy    r1 r2    -> MLICopy    (f r1) (f r2)
      MLIJump    r1       -> MLIJump    (f r1)
      MLIIfJump  r1 r2    -> MLIIfJump  (f r1) (f r2)
      MLINotJump r1 r2    -> MLINotJump (f r1) (f r2)

instance Bifoldable MLInst' where
  bifoldMap :: Monoid m => (a -> m) -> (b -> m) -> MLInst' a b -> m
  bifoldMap f g x =
    case x of
      MLINop              -> mempty
      MLIConst   r1 v     -> f r1 <> g v
      MLIAddI    r1 r2 v  -> f r1 <> f r2 <> g v
      MLILoad    r1 r2    -> f r1 <> f r2
      MLIStore   r1 r2    -> f r1 <> f r2
      MLIAdd     r1 r2 r3 -> f r1 <> f r2 <> f r3
      MLISub     r1 r2 r3 -> f r1 <> f r2 <> f r3
      MLIMult    r1 r2 r3 -> f r1 <> f r2 <> f r3
      MLIShift   r1 r2 r3 -> f r1 <> f r2 <> f r3
      MLIAnd     r1 r2 r3 -> f r1 <> f r2 <> f r3
      MLIOr      r1 r2 r3 -> f r1 <> f r2 <> f r3
      MLIXor     r1 r2 r3 -> f r1 <> f r2 <> f r3
      MLIEq      r1 r2 r3 -> f r1 <> f r2 <> f r3
      MLIGt      r1 r2 r3 -> f r1 <> f r2 <> f r3
      MLILt      r1 r2 r3 -> f r1 <> f r2 <> f r3
      MLIInv     r1 r2    -> f r1 <> f r2
      MLICopy    r1 r2    -> f r1 <> f r2
      MLIJump    r1       -> f r1
      MLIIfJump  r1 r2    -> f r1 <> f r2
      MLINotJump r1 r2    -> f r1 <> f r2

instance Bitraversable MLInst' where
  bitraverse :: Applicative f => (a -> f b) -> (c -> f d) -> MLInst' a c -> f (MLInst' b d)
  bitraverse f g x =
    case x of
      MLINop              -> pure MLINop
      MLIConst   r1 v     -> MLIConst   <$> f r1 <*> g v
      MLIAddI    r1 r2 v  -> MLIAddI    <$> f r1 <*> f r2 <*> g v
      MLILoad    r1 r2    -> MLILoad    <$> f r1 <*> f r2
      MLIStore   r1 r2    -> MLIStore   <$> f r1 <*> f r2
      MLIAdd     r1 r2 r3 -> MLIAdd     <$> f r1 <*> f r2 <*> f r3
      MLISub     r1 r2 r3 -> MLISub     <$> f r1 <*> f r2 <*> f r3
      MLIMult    r1 r2 r3 -> MLIMult    <$> f r1 <*> f r2 <*> f r3
      MLIShift   r1 r2 r3 -> MLIShift   <$> f r1 <*> f r2 <*> f r3
      MLIAnd     r1 r2 r3 -> MLIAnd     <$> f r1 <*> f r2 <*> f r3
      MLIOr      r1 r2 r3 -> MLIOr      <$> f r1 <*> f r2 <*> f r3
      MLIXor     r1 r2 r3 -> MLIXor     <$> f r1 <*> f r2 <*> f r3
      MLIEq      r1 r2 r3 -> MLIEq      <$> f r1 <*> f r2 <*> f r3
      MLIGt      r1 r2 r3 -> MLIGt      <$> f r1 <*> f r2 <*> f r3
      MLILt      r1 r2 r3 -> MLILt      <$> f r1 <*> f r2 <*> f r3
      MLIInv     r1 r2    -> MLIInv     <$> f r1 <*> f r2
      MLICopy    r1 r2    -> MLICopy    <$> f r1 <*> f r2
      MLIJump    r1       -> MLIJump    <$> f r1
      MLIIfJump  r1 r2    -> MLIIfJump  <$> f r1 <*> f r2
      MLINotJump r1 r2    -> MLINotJump <$> f r1 <*> f r2

{-| MachineはMLInstでしか動きません -}
type MLInst = MLInst' MLReg MLVal
