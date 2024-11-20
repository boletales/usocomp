{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module SimpleLangDemo where

import MyPrelude

import SimpleLang.FromString
import SimpleLang.Tools
import MachineLang.FromSimpleLang
import MachineLang.Def
import MachineLang.Tools
import MachineLang.FromSimpleLang.Debugger

import Data.Text as T
import Data.Text.IO as TIO
import Data.Map.Strict as M
import Data.Ord
import Data.Maybe
import qualified Data.List as L
import Data.Vector as V
import Text.Megaparsec.Pos
import Control.Monad

import MachineLang.FromSimpleLang.Test
import SimpleLang.Def
import System.Directory
import Debug.Trace
import Tools.SimpleLangC



main :: IO ()
main = do
  text <- TIO.getContents
  TIO.putStrLn $ compileToJSON text

showAsmMap :: M.Map SLPos (Int, Int) -> Text
showAsmMap m = T.unlines $ (\(p, s) -> pack (show s) <> "\t" <> prettyPrintSLPos p) <$> M.assocs m

-- >>> compileToJSON $ prettyPrintSLProgram $ mlctTest (mlctTests !! 7)
-- "{\"src\": \"function main() -> int\\n{\\n  int $0 = main.fibonacci(20, 0, 1)\\n  return $0\\n}\\n\\nfunction main.fibonacci(int $A0, int $A1, int $A2) -> int\\n{\\n  when ($A0 == 0)\\n  {\\n    return $A2\\n  }\\n  else\\n  {\\n    tailcall main.fibonacci(($A0 - 1), $A2, ($A1 + $A2))\\n  }\\n\\n}\", \"asm\": \"nop   \\nconst r1 1\\nconst r2 214\\nstore r2 r1\\naddi  r1 r1 1\\nconst r2 0\\nstore r2 r1\\nconst r2 0\\naddi  r1 r1 1\\nstore r2 r1\\ncopy  r0 r1\\nconst pc 12\\nconst r2 4993\\nlt    r2 r1 r2\\naddi  r3 pc 6\\nijmp  r3 r2\\nconst r2 0\\nconst r3 -1000000000\\nstore r3 r2\\nconst r3 214\\njump  r3\\naddi  r1 r1 1\\naddi  r1 r1 1\\nconst r2 20\\nstore r2 r1\\naddi  r1 r1 1\\nconst r2 0\\nstore r2 r1\\naddi  r1 r1 1\\nconst r2 1\\nstore r2 r1\\nconst r3 74\\naddi  r1 r1 1\\naddi  r2 pc 8\\nstore r2 r1\\naddi  r1 r1 1\\naddi  r2 r1 -5\\nstore r2 r1\\naddi  r1 r1 1\\nstore r0 r1\\ncopy  r0 r1\\ncopy  pc r3\\naddi  r1 r1 1\\naddi  r2 r0 1\\nload  r2 r2\\nstore r2 r1\\naddi  r3 r0 -1\\nload  r3 r3\\nload  r2 r1\\nstore r2 r3\\naddi  r1 r1 -1\\naddi  r3 r3 -1\\ncopy  r2 r0\\nload  r0 r0\\naddi  r2 r2 -1\\nload  r1 r2\\naddi  r2 r2 -1\\nload  pc r2\\naddi  r1 r1 -1\\naddi  r1 r1 1\\nconst r2 0\\nstore r2 r1\\naddi  r3 r0 -1\\nload  r3 r3\\nload  r2 r1\\nstore r2 r3\\naddi  r1 r1 -1\\naddi  r3 r3 -1\\ncopy  r2 r0\\nload  r0 r0\\naddi  r2 r2 -1\\nload  r1 r2\\naddi  r2 r2 -1\\nload  pc r2\\nconst r2 4994\\nlt    r2 r1 r2\\naddi  r3 pc 6\\nijmp  r3 r2\\nconst r2 0\\nconst r3 -1000000000\\nstore r3 r2\\nconst r3 214\\njump  r3\\naddi  r1 r1 1\\naddi  r2 r0 -1\\nload  r2 r2\\naddi  r2 r2 1\\nload  r2 r2\\nstore r2 r1\\naddi  r1 r1 1\\nconst r2 0\\nstore r2 r1\\nload  r3 r1\\naddi  r1 r1 -1\\nload  r2 r1\\neq    r2 r2 r3\\nstore r2 r1\\nload  r3 r1\\naddi  r1 r1 -1\\naddi  r2 pc 21\\nnjmp  r2 r3\\naddi  r1 r1 1\\naddi  r2 r0 -1\\nload  r2 r2\\naddi  r2 r2 3\\nload  r2 r2\\nstore r2 r1\\naddi  r3 r0 -1\\nload  r3 r3\\nload  r2 r1\\nstore r2 r3\\naddi  r1 r1 -1\\naddi  r3 r3 -1\\ncopy  r2 r0\\nload  r0 r0\\naddi  r2 r2 -1\\nload  r1 r2\\naddi  r2 r2 -1\\nload  pc r2\\naddi  r2 pc 79\\njump  r2\\naddi  r1 r1 1\\naddi  r2 r0 -1\\nload  r2 r2\\naddi  r2 r2 1\\nload  r2 r2\\nstore r2 r1\\naddi  r1 r1 1\\nconst r2 1\\nstore r2 r1\\nload  r3 r1\\naddi  r1 r1 -1\\nload  r2 r1\\nsub   r2 r2 r3\\nstore r2 r1\\naddi  r1 r1 1\\naddi  r2 r0 -1\\nload  r2 r2\\naddi  r2 r2 3\\nload  r2 r2\\nstore r2 r1\\naddi  r1 r1 1\\naddi  r2 r0 -1\\nload  r2 r2\\naddi  r2 r2 2\\nload  r2 r2\\nstore r2 r1\\naddi  r1 r1 1\\naddi  r2 r0 -1\\nload  r2 r2\\naddi  r2 r2 3\\nload  r2 r2\\nstore r2 r1\\nload  r3 r1\\naddi  r1 r1 -1\\nload  r2 r1\\nadd   r2 r2 r3\\nstore r2 r1\\nconst r4 74\\naddi  r2 r0 -2\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r2 1\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r2 1\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r1 -5\\naddi  r3 r1 -1\\nload  r1 r3\\naddi  r1 r1 1\\naddi  r0 r1 5\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r2 1\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r2 1\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r2 1\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r2 1\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r2 1\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\ncopy  pc r4\\naddi  r1 r1 1\\nconst r2 0\\nstore r2 r1\\naddi  r3 r0 -1\\nload  r3 r3\\nload  r2 r1\\nstore r2 r3\\naddi  r1 r1 -1\\naddi  r3 r3 -1\\ncopy  r2 r0\\nload  r0 r0\\naddi  r2 r2 -1\\nload  r1 r2\\naddi  r2 r2 -1\\nload  pc r2\\nnop   \\n\", \"sourcemap\": [{\"sourcestart\": {\"line\": 15, \"col\": 45}, \"sourceend\": {\"line\": 15, \"col\": 56}, \"asmstart\": 142, \"asmend\": 158, \"slpos\": \"main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A1 + $A2)\"}, {\"sourcestart\": {\"line\": 15, \"col\": 40}, \"sourceend\": {\"line\": 15, \"col\": 43}, \"asmstart\": 136, \"asmend\": 141, \"slpos\": \"main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr $A2\"}, {\"sourcestart\": {\"line\": 15, \"col\": 29}, \"sourceend\": {\"line\": 15, \"col\": 38}, \"asmstart\": 122, \"asmend\": 135, \"slpos\": \"main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A0 - 1)\"}, {\"sourcestart\": {\"line\": 15, \"col\": 28}, \"sourceend\": {\"line\": 15, \"col\": 57}, \"asmstart\": 122, \"asmend\": 158, \"slpos\": \"main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2))\"}, {\"sourcestart\": {\"line\": 15, \"col\": 5}, \"sourceend\": {\"line\": 15, \"col\": 57}, \"asmstart\": 122, \"asmend\": 199, \"slpos\": \"main.fibonacci[0].whenElseBody[0]\"}, {\"sourcestart\": {\"line\": 11, \"col\": 12}, \"sourceend\": {\"line\": 11, \"col\": 15}, \"asmstart\": 102, \"asmend\": 107, \"slpos\": \"main.fibonacci[0].whenBody_0[0].expr $A2\"}, {\"sourcestart\": {\"line\": 11, \"col\": 5}, \"sourceend\": {\"line\": 11, \"col\": 15}, \"asmstart\": 102, \"asmend\": 119, \"slpos\": \"main.fibonacci[0].whenBody_0[0]\"}, {\"sourcestart\": {\"line\": 9, \"col\": 8}, \"sourceend\": {\"line\": 9, \"col\": 18}, \"asmstart\": 84, \"asmend\": 97, \"slpos\": \"main.fibonacci[0].whenCond_0.expr ($A0 == 0)\"}, {\"sourcestart\": {\"line\": 4, \"col\": 10}, \"sourceend\": {\"line\": 4, \"col\": 12}, \"asmstart\": 43, \"asmend\": 46, \"slpos\": \"main[1].expr $0\"}, {\"sourcestart\": {\"line\": 4, \"col\": 3}, \"sourceend\": {\"line\": 4, \"col\": 12}, \"asmstart\": 43, \"asmend\": 58, \"slpos\": \"main[1]\"}, {\"sourcestart\": {\"line\": 3, \"col\": 34}, \"sourceend\": {\"line\": 3, \"col\": 35}, \"asmstart\": 29, \"asmend\": 31, \"slpos\": \"main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1).expr 1\"}, {\"sourcestart\": {\"line\": 3, \"col\": 31}, \"sourceend\": {\"line\": 3, \"col\": 32}, \"asmstart\": 26, \"asmend\": 28, \"slpos\": \"main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1).expr 0\"}, {\"sourcestart\": {\"line\": 3, \"col\": 27}, \"sourceend\": {\"line\": 3, \"col\": 29}, \"asmstart\": 23, \"asmend\": 25, \"slpos\": \"main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1).expr 20\"}, {\"sourcestart\": {\"line\": 3, \"col\": 26}, \"sourceend\": {\"line\": 3, \"col\": 36}, \"asmstart\": 23, \"asmend\": 31, \"slpos\": \"main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1)\"}, {\"sourcestart\": {\"line\": 3, \"col\": 3}, \"sourceend\": {\"line\": 3, \"col\": 36}, \"asmstart\": 22, \"asmend\": 42, \"slpos\": \"main[0]\"}, {\"sourcestart\": {\"line\": 3, \"col\": 12}, \"sourceend\": {\"line\": 3, \"col\": 36}, \"asmstart\": 22, \"asmend\": 42, \"slpos\": \"main[0].expr main.fibonacci(20, 0, 1)\"}]}"

-- >>> error $ T.unpack $ textToSourcemap $ prettyPrintSLProgram $ mlctTest (mlctTests !! 7)
-- main.slang:3:3 - main.slang:3:36	main[0]
-- main.slang:4:3 - main.slang:4:12	main[1]
-- main.slang:3:31 - main.slang:3:32	main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1).expr 0
-- main.slang:3:34 - main.slang:3:35	main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1).expr 1
-- main.slang:3:27 - main.slang:3:29	main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1).expr 20
-- main.slang:4:10 - main.slang:4:12	main[1].expr $0
-- main.slang:3:12 - main.slang:3:36	main[0].expr main.fibonacci(20, 0, 1)
-- main.slang:3:26 - main.slang:3:36	main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1)
-- main.slang:11:5 - main.slang:11:15	main.fibonacci[0].whenBody_0[0]
-- main.slang:15:5 - main.slang:15:57	main.fibonacci[0].whenElseBody[0]
-- main.slang:11:12 - main.slang:11:15	main.fibonacci[0].whenBody_0[0].expr $A2
-- main.slang:15:40 - main.slang:15:43	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr $A2
-- main.slang:15:46 - main.slang:15:55	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A1 + $A2).expr ($A1 + $A2)
-- main.slang:15:45 - main.slang:15:56	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A1 + $A2)
-- main.slang:15:30 - main.slang:15:37	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A0 - 1).expr ($A0 - 1)
-- main.slang:15:29 - main.slang:15:38	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A0 - 1)
-- main.slang:9:8 - main.slang:9:18	main.fibonacci[0].whenCond_0.expr ($A0 == 0)
-- main.slang:9:9 - main.slang:9:17	main.fibonacci[0].whenCond_0.expr ($A0 == 0).expr ($A0 == 0)
-- main.slang:15:28 - main.slang:15:57	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2))

-- >>> error $ either show T.unpack $ (showAsmMap . generateSourceMap) <$> (compileSLProgram (mlctTest (mlctTests !! 7)))
-- (1,215)	main
-- (22,42)	main[0]
-- (43,58)	main[1]
-- (60,74)	main.forceReturn
-- (60,62)	main.forceReturn.expr 0
-- (26,28)	main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1).expr 0
-- (29,31)	main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1).expr 1
-- (23,25)	main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1).expr 20
-- (43,46)	main[1].expr $0
-- (22,42)	main[0].expr main.fibonacci(20, 0, 1)
-- (23,31)	main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1)
-- (75,214)	main.fibonacci
-- (84,199)	main.fibonacci[0]
-- (102,119)	main.fibonacci[0].whenBody_0[0]
-- (122,199)	main.fibonacci[0].whenElseBody[0]
-- (84,101)	main.fibonacci[0].whenCond_0
-- (102,121)	main.fibonacci[0].whenBody_0
-- (122,199)	main.fibonacci[0].whenElseBody
-- (200,214)	main.fibonacci.forceReturn
-- (200,202)	main.fibonacci.forceReturn.expr 0
-- (90,92)	main.fibonacci[0].whenCond_0.expr ($A0 == 0).expr 0
-- (128,130)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A0 - 1).expr 1
-- (122,127)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A0 - 1).expr $A0
-- (84,89)	main.fibonacci[0].whenCond_0.expr ($A0 == 0).expr $A0
-- (142,147)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A1 + $A2).expr $A1
-- (102,107)	main.fibonacci[0].whenBody_0[0].expr $A2
-- (148,153)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A1 + $A2).expr $A2
-- (136,141)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr $A2
-- (142,158)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A1 + $A2)
-- (122,135)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A0 - 1)
-- (84,97)	main.fibonacci[0].whenCond_0.expr ($A0 == 0)
-- (122,158)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2))
