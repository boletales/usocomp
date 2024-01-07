{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module MachineLang.Machine (
    MLMachine (..)
  , MLMachineFast (..)
  , MLConfig (..)
  , initMLMacine
  , initMLMacineFast
  , MLDebugger
  , MLRuntimeError (..)
  , getInst
  , runMLMachine1
  , runMLMachine1Fast
 ) where

import Data.Vector as V
import Data.Vector.Mutable as MV
import Data.Vector.Unboxed as VP
import Data.Vector.Unboxed.Mutable as MVP
import MachineLang.Def
import MachineLang.Packed
import Control.Monad.Except
import Data.Bits
import Data.STRef
import Control.Monad.Primitive
import SimpleLang.Tools


data MLMachine posdata s = MLMachine {
      mlprogram :: V.Vector (MLInst, posdata)
    , mlmemory  :: MVP.MVector s Int
    , mlregs    :: MVP.MVector s Int
    , mltime    :: STRef s Int
  }

data MLConfig = MLConfig {
      memsize :: Int
  }

initMLMacine :: forall t m. (PrimMonad m) => MLConfig -> V.Vector (MLInst, t) -> m (MLMachine t (PrimState m))
initMLMacine conf program = do
  mem <- MVP.replicate (memsize conf)                     0
  reg <- MVP.replicate (fromEnum (maxBound :: MLReg) + 1) 0
  time <- stToPrim $ newSTRef 0
  pure $ MLMachine {
              mlprogram = program
            , mlmemory  = mem
            , mlregs    = reg
            , mltime    = time
          }
--                    (inst  , pos, pc , mem          , reg          , time)
type MLDebugger p m = (MLInst, p  , Int, VP.Vector Int, VP.Vector Int, Int ) -> m ()

data MLRuntimeError = 
      MLREMemOutOfRange     Int
    | MLREProgramOutOfRange Int
    | MLRETimeout
    | MLRESuccess           Int
    deriving (Show, Eq)

getInst :: forall p m. (PrimMonad m) => MLMachine p (PrimState m) -> m (Maybe (MLInst, p))
getInst machine = 
  let readReg :: MVP.MVector (PrimState m) Int -> MLReg -> m MLVal
      readReg reg ix = 
        MLVal <$> MVP.unsafeRead reg (fromEnum ix)
  in do
      let reg = mlregs machine
      (MLVal pc) <- readReg reg MLRegPC
      pure $ mlprogram machine V.!? pc

{-# SPECIALISE runMLMachine1 :: MLMachine SLPos (PrimState (ExceptT MLRuntimeError IO)) -> MLDebugger SLPos (ExceptT MLRuntimeError IO) -> ExceptT MLRuntimeError IO () #-}
runMLMachine1 :: forall p m. (PrimMonad m, MonadError MLRuntimeError m) => MLMachine p (PrimState m) -> MLDebugger p m -> m ()
runMLMachine1 machine debugger = 
  let readMem :: MVP.MVector (PrimState m) Int -> MLVal -> m MLVal
      readMem mem (MLVal addr) = 
        if 0 <= addr && addr < MVP.length mem
          then MLVal <$> MVP.read mem addr
          else throwError (MLREMemOutOfRange addr)

      writeMem :: MVP.MVector (PrimState m) Int -> MLVal -> MLVal -> m ()
      writeMem mem (MLVal addr) (MLVal val) = 
        if 0 <= addr && addr < MVP.length mem
          then MVP.write mem addr val
          else throwError (MLREMemOutOfRange addr)

      readReg :: MVP.MVector (PrimState m) Int -> MLReg -> m MLVal
      readReg reg ix = 
        MLVal <$> MVP.unsafeRead reg (fromEnum ix)

      writeReg :: MVP.MVector (PrimState m) Int -> MLReg -> MLVal -> m ()
      writeReg reg ix (MLVal val) = 
        MVP.unsafeWrite reg (fromEnum ix) val

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
              then MVP.read mem 0 >>= throwError . MLRESuccess
              else throwError (MLREProgramOutOfRange pc)
          Just (inst, pos) -> do
            memold <- VP.freeze mem
            regold <- VP.freeze reg
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
            


data MLMachineFast s = MLMachineFast {
      mlfprogram :: VP.Vector MLInstPacked
    , mlfmemory  :: MVP.MVector s Int
    , mlfregs    :: MVP.MVector s Int
    , mlftime    :: STRef s Int
  }

initMLMacineFast :: forall m t. (PrimMonad m) => MLConfig -> V.Vector (MLInst, t) -> m (MLMachineFast (PrimState m))
initMLMacineFast conf prog = do
  mem <- MVP.replicate (memsize conf)                     0
  reg <- MVP.replicate (fromEnum (maxBound :: MLReg) + 1) 0
  time <- stToPrim $ newSTRef 0
  let program = VP.convert (V.map (packMLInst . fst) prog)
  pure $ MLMachineFast {
              mlfprogram = program
            , mlfmemory  = mem
            , mlfregs    = reg
            , mlftime    = time
          }


runMLMachine1Fast :: MLMachineFast (PrimState IO) -> (Int -> IO ()) -> IO (Either MLRuntimeError ())
runMLMachine1Fast machine ticker = 
  let {-# INLINE readMem #-}
      readMem :: MVP.MVector (PrimState IO) Int -> Int -> IO Int
      readMem = MVP.unsafeRead

      {-# INLINE writeMem #-}
      writeMem :: MVP.MVector (PrimState IO) Int -> Int -> Int -> IO ()
      writeMem = MVP.unsafeWrite

      {-# INLINE readReg #-}
      readReg :: MVP.MVector (PrimState IO) Int -> Int -> IO Int
      readReg = MVP.unsafeRead

      {-# INLINE writeReg #-}
      writeReg :: MVP.MVector (PrimState IO) Int -> Int -> Int -> IO ()
      writeReg = MVP.unsafeWrite
      
  in do
        let mem = mlfmemory machine
        let reg = mlfregs machine

        pc <- readReg reg (fromEnum MLRegPC)

        if pc >= VP.length (mlfprogram machine) then
            if pc == VP.length (mlfprogram machine)
              then MVP.read mem 0 >>= \i -> pure $ Left (MLRESuccess i)
              else pure $ Left (MLREProgramOutOfRange pc)
        else do
            -- memold <- V.freeze mem
            -- regold <- V.freeze reg
            timeold <- stToPrim $ readSTRef (mlftime machine)
            -- debugger (inst, pos, pc, memold, regold, timeold)

            when (timeold `mod` 10000000 == 0) (ticker timeold)

            stToPrim $ modifySTRef (mlftime machine) (+ 1)

            writeReg reg (fromEnum MLRegPC) (pc + 1)

            let inst = mlfprogram machine VP.! pc
            case inst of
              (0 , _     , _       , _     ) -> pure (Right ())   
              (1 , dest  , addr    , _     ) -> readReg reg addr >>= \a -> if 0 <= a && a < MVP.length mem then readMem mem a       >>= writeReg reg dest    >> pure (Right ()) else pure (Left (MLREMemOutOfRange a))
              (2 , source, addr    , _     ) -> readReg reg addr >>= \a -> if 0 <= a && a < MVP.length mem then readReg reg source  >>= writeMem mem a       >> pure (Right ()) else pure (Left (MLREMemOutOfRange a))
              (3 , dest  , val     , _     ) -> writeReg reg dest val >> pure (Right ())                                                            
              (4 , dest  , source  ,val    ) -> do v1 <- readReg reg source; writeReg reg dest ((+) v1 val)                                                            >> pure (Right ())   
              (5 , dest  , source1 ,source2) -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest ((+)   v1 v2);                              >> pure (Right ())   
              (6 , dest  , source1 ,source2) -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest ((-)   v1 v2);                              >> pure (Right ())   
              (7 , dest  , source1 ,source2) -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest ((*)   v1 v2);                              >> pure (Right ())   
              (8 , dest  , source1 ,source2) -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (shift v1 v2);                              >> pure (Right ())   
              (9 , dest  , source1 ,source2) -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest ((.&.) v1 v2);                              >> pure (Right ())   
              (10, dest  , source1 ,source2) -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest ((.|.) v1 v2);                              >> pure (Right ())   
              (11, dest  , source1 ,source2) -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest (xor   v1 v2);                              >> pure (Right ())   
              (12, dest  , source1 ,source2) -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest ((\a b -> if a == b then 1 else 0) v1 v2);  >> pure (Right ())   
              (13, dest  , source1 ,source2) -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest ((\a b -> if a >  b then 1 else 0) v1 v2);  >> pure (Right ())   
              (14, dest  , source1 ,source2) -> do v1 <- readReg reg source1; v2 <- readReg reg source2; writeReg reg dest ((\a b -> if a <  b then 1 else 0) v1 v2);  >> pure (Right ())   
              (15, dest  , source1 , _     ) -> do v1 <- readReg reg source1;                            writeReg reg dest (complement v1);                            >> pure (Right ())   
              (16, dest  , source1 , _     ) -> do v1 <- readReg reg source1;                            writeReg reg dest v1;                                               >> pure (Right ())   
              (17, jumpto, _       , _     ) -> do ja <- readReg reg jumpto ; writeReg reg (fromEnum MLRegPC) ja                                                                         >> pure (Right ())   
              (18, jumpto, cond    , _     ) -> do ja <- readReg reg jumpto ; val <- readReg reg cond; when (val /= 0) (writeReg reg (fromEnum MLRegPC) ja)                        >> pure (Right ())   
              (19, jumpto, cond    , _     ) -> do ja <- readReg reg jumpto ; val <- readReg reg cond; when (val == 0) (writeReg reg (fromEnum MLRegPC) ja)                        >> pure (Right ())   
              _ -> error "Unknown opcode"