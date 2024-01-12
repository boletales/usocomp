{-# LANGUAGE UnboxedTuples #-}

module MachineLang.Packed (
    MLInstPacked,
    packMLInst,
    unpackMLInst
) where

import MachineLang.Def
import Data.Text (unpack)

type MLInstPacked = (Int, Int, Int, Int)

{-
MLINop                            
MLILoad    dest   addr            
MLIStore   source addr            
MLIConst   dest   val             
MLIAddI    dest   source val      
MLIAdd     dest   source1 source2 
MLISub     dest   source1 source2 
MLIMult    dest   source1 source2 
MLIShift   dest   source1 source2 
MLIAnd     dest   source1 source2 
MLIOr      dest   source1 source2 
MLIXor     dest   source1 source2 
MLIEq      dest   source1 source2 
MLIGt      dest   source1 source2 
MLILt      dest   source1 source2 
MLIInv     dest   source1         
MLICopy    dest   source1         
MLIJump    jumpto                 
MLIIfJump  jumpto cond            
MLINotJump jumpto cond            
-}

packr :: MLReg -> Int
packr = fromEnum

packv :: MLVal -> Int
packv (MLVal v) = v

unpackr :: Int -> MLReg
unpackr = toEnum

unpackv :: Int -> MLVal
unpackv = MLVal

packMLInst :: MLInst' MLReg MLVal -> MLInstPacked
packMLInst inst = 
  case inst of
    MLINop              -> (0 , 0       , 0       , 0       )       
    MLILoad    r1 r2    -> (1 , packr r1, packr r2, 0       )
    MLIStore   r1 r2    -> (2 , packr r1, packr r2, 0       )
    MLIConst   r1 v     -> (3 , packr r1, packv v , 0       )
    MLIAddI    r1 r2 v  -> (4 , packr r1, packr r2, packv v )
    MLIAdd     r1 r2 r3 -> (5 , packr r1, packr r2, packr r3)
    MLISub     r1 r2 r3 -> (6 , packr r1, packr r2, packr r3)
    MLIMult    r1 r2 r3 -> (7 , packr r1, packr r2, packr r3)
    MLIShift   r1 r2 r3 -> (8 , packr r1, packr r2, packr r3)
    MLIAnd     r1 r2 r3 -> (9 , packr r1, packr r2, packr r3)
    MLIOr      r1 r2 r3 -> (10, packr r1, packr r2, packr r3)
    MLIXor     r1 r2 r3 -> (11, packr r1, packr r2, packr r3)
    MLIEq      r1 r2 r3 -> (12, packr r1, packr r2, packr r3)
    MLIGt      r1 r2 r3 -> (13, packr r1, packr r2, packr r3)
    MLILt      r1 r2 r3 -> (14, packr r1, packr r2, packr r3)
    MLIInv     r1 r2    -> (15, packr r1, packr r2, 0       )
    MLICopy    r1 r2    -> (16, packr r1, packr r2, 0       )
    MLIJump    r1       -> (17, packr r1, 0       , 0       )
    MLIIfJump  r1 r2    -> (18, packr r1, packr r2, 0       )
    MLINotJump r1 r2    -> (19, packr r1, packr r2, 0       )

unpackMLInst :: MLInstPacked -> MLInst' MLReg MLVal
unpackMLInst packed =
  case packed of
    (0 , 0 , 0 , 0)       -> MLINop
    (1 , r1, r2, 0)       -> MLILoad    (unpackr r1) (unpackr r2)
    (2 , r1, r2, 0)       -> MLIStore   (unpackr r1) (unpackr r2)
    (3 , r1, v , 0)       -> MLIConst   (unpackr r1) (unpackv v )
    (4 , r1, r2, v)       -> MLIAddI    (unpackr r1) (unpackr r2) (unpackv v)
    (5 , r1, r2, r3)      -> MLIAdd     (unpackr r1) (unpackr r2) (unpackr r3)
    (6 , r1, r2, r3)      -> MLISub     (unpackr r1) (unpackr r2) (unpackr r3)
    (7 , r1, r2, r3)      -> MLIMult    (unpackr r1) (unpackr r2) (unpackr r3)
    (8 , r1, r2, r3)      -> MLIShift   (unpackr r1) (unpackr r2) (unpackr r3)
    (9 , r1, r2, r3)      -> MLIAnd     (unpackr r1) (unpackr r2) (unpackr r3)
    (10, r1, r2, r3)      -> MLIOr      (unpackr r1) (unpackr r2) (unpackr r3)
    (11, r1, r2, r3)      -> MLIXor     (unpackr r1) (unpackr r2) (unpackr r3)
    (12, r1, r2, r3)      -> MLIEq      (unpackr r1) (unpackr r2) (unpackr r3)
    (13, r1, r2, r3)      -> MLIGt      (unpackr r1) (unpackr r2) (unpackr r3)
    (14, r1, r2, r3)      -> MLILt      (unpackr r1) (unpackr r2) (unpackr r3)
    (15, r1, r2, 0)       -> MLIInv     (unpackr r1) (unpackr r2)
    (16, r1, r2, 0)       -> MLICopy    (unpackr r1) (unpackr r2)
    (17, r1, 0 , 0)       -> MLIJump    (unpackr r1)   
    (18, r1, r2, 0)       -> MLIIfJump  (unpackr r1) (unpackr r2)
    (19, r1, r2, 0)       -> MLINotJump (unpackr r1) (unpackr r2)
    _                     -> error $ "unpackMLInst: " ++ show packed
