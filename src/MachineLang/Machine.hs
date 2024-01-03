{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module MachineLang.Machine (
    MLMachine (..)
  , MLConfig (..)
  , initMLMacine
  , MLDebugger
  , MLRuntimeError (..)
  , getInst
  , runMLMachine1
  , runMLMachine1Fast
 ) where

import Data.Vector as V
import Data.Vector.Mutable as MV
import MachineLang.Def
import Control.Monad.Except
import Data.Bits
import Data.STRef
import Control.Monad.Primitive
import SimpleLang.Tools


data MLMachine posdata s = MLMachine {
      mlprogram :: V.Vector (MLInst, posdata)
    , mlmemory  :: MV.MVector s Int
    , mlregs    :: MV.MVector s Int
    , mltime    :: STRef s Int
  }

data MLConfig = MLConfig {
      memsize :: Int
  }

initMLMacine :: forall t m. (PrimMonad m) => MLConfig -> V.Vector (MLInst, t) -> m (MLMachine t (PrimState m))
initMLMacine conf program = do
  mem <- MV.replicate (memsize conf)                     0
  reg <- MV.replicate (fromEnum (maxBound :: MLReg) + 1) 0
  time <- stToPrim $ newSTRef 0
  pure $ MLMachine {
              mlprogram = program
            , mlmemory  = mem
            , mlregs    = reg
            , mltime    = time
          }
--                    (inst  , pos, pc , mem         , reg         , time)
type MLDebugger p m = (MLInst, p  , Int, V.Vector Int, V.Vector Int, Int ) -> m ()

data MLRuntimeError = 
      MLREMemOutOfRange     Int
    | MLREProgramOutOfRange Int
    | MLRETimeout
    | MLRESuccess           Int
    deriving (Show, Eq)

getInst :: forall p m. (PrimMonad m) => MLMachine p (PrimState m) -> m (Maybe (MLInst, p))
getInst machine = 
  let readReg :: MV.MVector (PrimState m) Int -> MLReg -> m MLVal
      readReg reg ix = 
        MLVal <$> MV.unsafeRead reg (fromEnum ix)
  in do
      let reg = mlregs machine
      (MLVal pc) <- readReg reg MLRegPC
      pure $ mlprogram machine V.!? pc

{-# SPECIALISE runMLMachine1 :: MLMachine SLPos (PrimState (ExceptT MLRuntimeError IO)) -> MLDebugger SLPos (ExceptT MLRuntimeError IO) -> ExceptT MLRuntimeError IO () #-}
runMLMachine1 :: forall p m. (PrimMonad m, MonadError MLRuntimeError m) => MLMachine p (PrimState m) -> MLDebugger p m -> m ()
runMLMachine1 machine debugger = 
  let readMem :: MV.MVector (PrimState m) Int -> MLVal -> m MLVal
      readMem mem (MLVal addr) = 
        if 0 <= addr && addr < MV.length mem
          then MLVal <$> MV.read mem addr
          else throwError (MLREMemOutOfRange addr)

      writeMem :: MV.MVector (PrimState m) Int -> MLVal -> MLVal -> m ()
      writeMem mem (MLVal addr) (MLVal val) = 
        if 0 <= addr && addr < MV.length mem
          then MV.write mem addr val
          else throwError (MLREMemOutOfRange addr)

      readReg :: MV.MVector (PrimState m) Int -> MLReg -> m MLVal
      readReg reg ix = 
        MLVal <$> MV.unsafeRead reg (fromEnum ix)

      writeReg :: MV.MVector (PrimState m) Int -> MLReg -> MLVal -> m ()
      writeReg reg ix (MLVal val) = 
        MV.unsafeWrite reg (fromEnum ix) val

      over1 :: (Int -> Int) -> MLVal -> MLVal
      over1 f (MLVal x) = MLVal (f x)

      over2 :: (Int -> Int -> Int) -> MLVal -> MLVal -> MLVal
      over2 f (MLVal x) (MLVal y) = MLVal (f x y)
      
  in do
        let mem = mlmemory machine
        let reg = mlregs machine

        (MLVal pc) <- readReg reg MLRegPC
        let minst = mlprogram machine V.!? pc

        case minst of
          Nothing -> 
            if pc == V.length (mlprogram machine)
              then MV.read mem 0 >>= throwError . MLRESuccess
              else throwError (MLREProgramOutOfRange pc)
          Just (inst, pos) -> do
            memold <- V.freeze mem
            regold <- V.freeze reg
            timeold <- stToPrim $ readSTRef (mltime machine)
            debugger (inst, pos, pc, memold, regold, timeold)

            stToPrim $ modifySTRef (mltime machine) (+ 1)

            writeReg reg MLRegPC (MLVal $ pc + 1)

            case inst of
              MLINop                            -> pure ()
              MLILoad    dest   addr            -> readReg reg addr >>= \addrval -> readMem mem addrval >>= writeReg reg dest                                                   
              MLIStore   source addr            -> readReg reg addr >>= \addrval -> readReg reg source  >>= writeMem mem addrval                                                  
              MLIConst   dest   val             -> writeReg reg dest val                                                                      
              MLIAddI    dest   source val      -> do v1 <- readReg reg source; writeReg reg dest (over2 (+) v1 val)
              MLIAdd     dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (+)   v1 v2);
              MLISub     dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (-)   v1 v2);
              MLIMult    dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (*)   v1 v2);
              MLIShift   dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 shift v1 v2);
              MLIAnd     dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (.&.) v1 v2);
              MLIOr      dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (.|.) v1 v2);
              MLIXor     dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 xor   v1 v2);
              MLIEq      dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (\a b -> if a == b then 1 else 0) v1 v2);
              MLIGt      dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (\a b -> if a >  b then 1 else 0) v1 v2);
              MLILt      dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (\a b -> if a <  b then 1 else 0) v1 v2);
              MLIInv     dest   source1         -> do v1 <- readReg reg source1;                            writeReg reg dest (over1 complement v1);
              MLICopy    dest   source1         -> do v1 <- readReg reg source1;                            writeReg reg dest v1;
              MLIJump    jumpto                 -> do ja <- readReg reg jumpto; writeReg reg MLRegPC ja
              MLIIfJump  jumpto cond            -> do ja <- readReg reg jumpto; val <- readReg reg cond; when (val /= MLVal 0) (writeReg reg MLRegPC ja)
              MLINotJump jumpto cond            -> do ja <- readReg reg jumpto; val <- readReg reg cond; when (val == MLVal 0) (writeReg reg MLRegPC ja)
            




runMLMachine1Fast :: MLMachine () (PrimState IO) -> (Int -> IO ()) -> IO (Either MLRuntimeError ())
runMLMachine1Fast machine ticker = 
  let {-# INLINE readMem #-}
      readMem :: MV.MVector (PrimState IO) Int -> MLVal -> IO MLVal
      readMem mem (MLVal addr) = 
          MLVal <$> MV.unsafeRead mem addr

      {-# INLINE writeMem #-}
      writeMem :: MV.MVector (PrimState IO) Int -> MLVal -> MLVal -> IO ()
      writeMem mem (MLVal addr) (MLVal val) = 
          MV.unsafeWrite mem addr val

      {-# INLINE readReg #-}
      readReg :: MV.MVector (PrimState IO) Int -> MLReg -> IO MLVal
      readReg reg ix = 
        MLVal <$> MV.unsafeRead reg (fromEnum ix)

      {-# INLINE writeReg #-}
      writeReg :: MV.MVector (PrimState IO) Int -> MLReg -> MLVal -> IO ()
      writeReg reg ix (MLVal val) = 
        MV.unsafeWrite reg (fromEnum ix) val

      {-# INLINE over1 #-}
      over1 :: (Int -> Int) -> MLVal -> MLVal
      over1 f (MLVal x) = MLVal (f x)

      {-# INLINE over2 #-}
      over2 :: (Int -> Int -> Int) -> MLVal -> MLVal -> MLVal
      over2 f (MLVal x) (MLVal y) = MLVal (f x y)
      
  in do
        let mem = mlmemory machine
        let reg = mlregs machine

        (MLVal pc) <- readReg reg MLRegPC
        let minst = mlprogram machine V.!? pc

        case minst of
          Nothing -> 
            if pc == V.length (mlprogram machine)
              then MV.read mem 0 >>= \i -> pure $ Left (MLRESuccess i)
              else pure $ Left (MLREProgramOutOfRange pc)
          Just (inst, _) -> do
            -- memold <- V.freeze mem
            -- regold <- V.freeze reg
            timeold <- stToPrim $ readSTRef (mltime machine)
            -- debugger (inst, pos, pc, memold, regold, timeold)

            when (timeold `mod` 1000000 == 0) (ticker timeold)

            stToPrim $ modifySTRef (mltime machine) (+ 1)

            writeReg reg MLRegPC (MLVal $ pc + 1)

            case inst of
              MLINop                            -> pure (Right ())   
              MLILoad    dest   addr            -> readReg reg addr >>= \addrval@(MLVal a) -> if 0 <= a && a < MV.length mem then readMem mem addrval >>= writeReg reg dest    >> pure (Right ()) else pure (Left (MLREMemOutOfRange a))
              MLIStore   source addr            -> readReg reg addr >>= \addrval@(MLVal a) -> if 0 <= a && a < MV.length mem then readReg reg source  >>= writeMem mem addrval >> pure (Right ()) else pure (Left (MLREMemOutOfRange a))
              MLIConst   dest   val             -> writeReg reg dest val >> pure (Right ())                                                            
              MLIAddI    dest   source val      -> do v1 <- readReg reg source; writeReg reg dest (over2 (+) v1 val)                                                            >> pure (Right ())   
              MLIAdd     dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (+)   v1 v2);                              >> pure (Right ())   
              MLISub     dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (-)   v1 v2);                              >> pure (Right ())   
              MLIMult    dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (*)   v1 v2);                              >> pure (Right ())   
              MLIShift   dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 shift v1 v2);                              >> pure (Right ())   
              MLIAnd     dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (.&.) v1 v2);                              >> pure (Right ())   
              MLIOr      dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (.|.) v1 v2);                              >> pure (Right ())   
              MLIXor     dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 xor   v1 v2);                              >> pure (Right ())   
              MLIEq      dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (\a b -> if a == b then 1 else 0) v1 v2);  >> pure (Right ())   
              MLIGt      dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (\a b -> if a >  b then 1 else 0) v1 v2);  >> pure (Right ())   
              MLILt      dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (\a b -> if a <  b then 1 else 0) v1 v2);  >> pure (Right ())   
              MLIInv     dest   source1         -> do v1 <- readReg reg source1;                            writeReg reg dest (over1 complement v1);                            >> pure (Right ())   
              MLICopy    dest   source1         -> do v1 <- readReg reg source1;                            writeReg reg dest v1;                                               >> pure (Right ())   
              MLIJump    jumpto                 -> do ja <- readReg reg jumpto; writeReg reg MLRegPC ja                                                                         >> pure (Right ())   
              MLIIfJump  jumpto cond            -> do ja <- readReg reg jumpto; val <- readReg reg cond; when (val /= MLVal 0) (writeReg reg MLRegPC ja)                        >> pure (Right ())   
              MLINotJump jumpto cond            -> do ja <- readReg reg jumpto; val <- readReg reg cond; when (val == MLVal 0) (writeReg reg MLRegPC ja)                        >> pure (Right ())   
            
