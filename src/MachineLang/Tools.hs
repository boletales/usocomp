{-# LANGUAGE OverloadedStrings #-}

module MachineLang.Tools (
    mlregAbbrText
  , mliAbbrText
  ) where

import MyPrelude

import MachineLang.Def
import Data.Text as T

mlregAbbrText :: MLReg -> Text
mlregAbbrText reg =
  case reg of
    MLReg0 -> "r0"
    MLReg1 -> "r1"
    MLReg2 -> "r2"
    MLReg3 -> "r3"
    MLReg4 -> "r4"
    MLReg5 -> "r5"
    MLReg6 -> "r6"
    MLReg7 -> "r7"
    MLRegPC -> "pc"

mliAbbrText :: MLInst' MLReg MLVal -> Text
mliAbbrText inst =
  case inst of
    MLINop                     -> "nop   "
    MLIConst   r1    (MLVal v) -> "const " <> mlregAbbrText r1 <> " " <> tshow v
    MLIAddI    r1 r2 (MLVal v) -> "addi  " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2 <> " " <> tshow v
    MLILoad    r1 r2           -> "load  " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2
    MLIStore   r1 r2           -> "store " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2
    MLIAdd     r1 r2 r3        -> "add   " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2 <> " " <> mlregAbbrText r3
    MLISub     r1 r2 r3        -> "sub   " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2 <> " " <> mlregAbbrText r3
    MLIMult    r1 r2 r3        -> "mult  " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2 <> " " <> mlregAbbrText r3
    MLIShift   r1 r2 r3        -> "shift " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2 <> " " <> mlregAbbrText r3
    MLIAnd     r1 r2 r3        -> "and   " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2 <> " " <> mlregAbbrText r3
    MLIOr      r1 r2 r3        -> "or    " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2 <> " " <> mlregAbbrText r3
    MLIXor     r1 r2 r3        -> "xor   " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2 <> " " <> mlregAbbrText r3
    MLIEq      r1 r2 r3        -> "eq    " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2 <> " " <> mlregAbbrText r3
    MLIGt      r1 r2 r3        -> "gt    " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2 <> " " <> mlregAbbrText r3
    MLILt      r1 r2 r3        -> "lt    " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2 <> " " <> mlregAbbrText r3
    MLIInv     r1 r2           -> "inv   " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2
    MLICopy    r1 r2           -> "copy  " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2
    MLIJump    r1              -> "jump  " <> mlregAbbrText r1
    MLIIfJump  r1 r2           -> "jnz   " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2
    MLINotJump r1 r2           -> "jz    " <> mlregAbbrText r1 <> " " <> mlregAbbrText r2