{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module MachineLang.Machine where

import Data.Vector as V
import Data.Vector.Mutable as MV
import MachineLang.Def
import Control.Monad.Except
import Data.Bits
import Data.STRef
import Control.Monad.Primitive

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
          Nothing -> throwError (MLREProgramOutOfRange pc)
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
              MLIAdd     dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (+)   v1 v2);
              MLISub     dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (-)   v1 v2);
              MLIMult    dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (*)   v1 v2);
              MLIShift   dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 shift v1 v2);
              MLIAnd     dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (.&.) v1 v2);
              MLIOr      dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 (.|.) v1 v2);
              MLIXor     dest   source1 source2 -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (over2 xor   v1 v2);
              MLIInv     dest   source1         -> do v1 <- readReg reg source1;                            writeReg reg dest (over1 complement v1);
              MLICopy    dest   source1         -> do v1 <- readReg reg source1;                            writeReg reg dest v1;
              MLIJump    jumpto                 -> do ja <- readReg reg jumpto; writeReg reg MLRegPC ja
              MLIIfJump  jumpto cond            -> do ja <- readReg reg jumpto; val <- readReg reg cond; when (val /= MLVal 0) (writeReg reg MLRegPC ja)
              MLINotJump jumpto cond            -> do ja <- readReg reg jumpto; val <- readReg reg cond; when (val == MLVal 0) (writeReg reg MLRegPC ja)
            
