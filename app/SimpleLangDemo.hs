{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module SimpleLangDemo where


import SimpleLang.FromString
import SimpleLang.Tools
import MachineLang.FromSimpleLang
import MachineLang.Def
import MachineLang.Tools

import Data.Text as T
import Data.Text.IO as TIO
import Data.Map.Strict as M
import Data.Ord
import Data.Maybe
import qualified Data.List as L
import Data.Vector as V
import Text.Megaparsec.Pos

import MachineLang.FromSimpleLang.Test
import SimpleLang.Def
import Debug.Trace


data ASMMapRow = ASMMapRow {
  asmmrSourceStart :: (Int, Int),
  asmmrSourceEnd :: (Int, Int),
  asmmrASMStart :: Int,
  asmmrASMEnd :: Int,
  asmmrSLPos :: SLPos
} deriving Eq

instance Ord ASMMapRow where
  compare (ASMMapRow _ _ asmStart1 asmEnd1 _) (ASMMapRow _ _ asmStart2 asmEnd2 _) =
    compare (Down asmStart1, asmEnd1) (Down asmStart2, asmEnd2)

sourceMapToJSON :: ASMMapRow -> Text
sourceMapToJSON (ASMMapRow (lstart, cstart) (lend, cend) asmStart asmEnd slPos) =
    objToJSON [
      ("sourcestart", objToJSON [("line", tshow lstart), ("col", tshow cstart)]),
      ("sourceend"  , objToJSON [("line", tshow lend  ), ("col", tshow cend)]),
      ("asmstart"   , tshow asmStart),
      ("asmend"     , tshow asmEnd),
      ("slpos"      , tshow $ tshow slPos)
    ]

tshow :: Show a => a -> Text
tshow = T.pack . show

objToJSON :: [(Text,Text)] -> Text
objToJSON obj = "{" <> T.intercalate ", " (Prelude.map (\(k,v) -> "\"" <> k <> "\": " <> v) obj) <> "}"

listToJSON :: [Text] -> Text
listToJSON list = "[" <> T.intercalate ", " list <> "]"

sourcePosToTuple :: SourcePos -> (Int, Int)
sourcePosToTuple (SourcePos _ l c) = (unPos l, unPos c)

combineSourceMaps :: M.Map SLPos SourcePosRange -> V.Vector (MLInst, SLPos) -> [ASMMapRow]
combineSourceMaps sourcemap asm =
  let
    asmMap = generateSourceMap asm
    rows   = Data.Maybe.mapMaybe (\slpos -> (\(SourcePosRange srcFrom srcTo _) (asmFrom, asmTo) -> ASMMapRow (sourcePosToTuple srcFrom) (sourcePosToTuple srcTo) asmFrom asmTo slpos) <$> M.lookup slpos sourcemap <*> M.lookup slpos asmMap) (M.keys sourcemap)
  in L.sort rows

mlToText :: V.Vector (MLInst, SLPos) -> Text
mlToText ml =
  T.unlines $ Prelude.map (\(inst, _) -> mliAbbrText inst) $ V.toList ml

compileToJSON :: Text -> Text
compileToJSON text =
  case textToSLParseResult text of
    Left err -> objToJSON [("error", tshow (show err))]
    Right (program, sourcemap) ->
      case compileSLProgram program of
        Left err -> objToJSON [("error", tshow (show err))]
        Right compiled ->
          objToJSON [
              ("src", tshow text),
              ("asm", tshow $ mlToText compiled),
              ("sourcemap", listToJSON $ sourceMapToJSON <$> combineSourceMaps sourcemap compiled)
            ]


main :: IO ()
main = do
  text <- TIO.getContents
  TIO.putStrLn $ compileToJSON text


showAsmMap :: M.Map SLPos (Int, Int) -> Text
showAsmMap m = T.unlines $ (\(p, s) -> pack (show s) <> "\t" <> prettyPrintSLPos p) <$> M.assocs m

-- >>> compileToJSON $ prettyPrintSLProgram $ mlctTest (mlctTests !! 7)
-- "{\"src\": \"function main() -> int\\n{\\n  int $0 = main.fibonacci(20, 0, 1)\\n  return $0\\n}\\n\\nfunction main.fibonacci(int $A0, int $A1, int $A2) -> int\\n{\\n  when ($A0 == 0)\\n  {\\n    return $A2\\n  }\\n  else\\n  {\\n    tailcall main.fibonacci(($A0 - 1), $A2, ($A1 + $A2))\\n  }\\n\\n}\", \"asm\": \"nop   \\nconst r1 1\\nconst r2 214\\nstore r2 r1\\naddi  r1 r1 1\\nconst r2 0\\nstore r2 r1\\nconst r2 0\\naddi  r1 r1 1\\nstore r2 r1\\ncopy  r0 r1\\nconst pc 12\\nconst r2 4993\\nlt    r2 r1 r2\\naddi  r3 pc 6\\nijmp  r3 r2\\nconst r2 0\\nconst r3 -1000000000\\nstore r3 r2\\nconst r3 214\\njump  r3\\naddi  r1 r1 1\\naddi  r1 r1 1\\nconst r2 20\\nstore r2 r1\\naddi  r1 r1 1\\nconst r2 0\\nstore r2 r1\\naddi  r1 r1 1\\nconst r2 1\\nstore r2 r1\\nconst r3 74\\naddi  r1 r1 1\\naddi  r2 pc 8\\nstore r2 r1\\naddi  r1 r1 1\\naddi  r2 r1 -5\\nstore r2 r1\\naddi  r1 r1 1\\nstore r0 r1\\ncopy  r0 r1\\ncopy  pc r3\\naddi  r1 r1 1\\naddi  r2 r0 1\\nload  r2 r2\\nstore r2 r1\\naddi  r3 r0 -1\\nload  r3 r3\\nload  r2 r1\\nstore r2 r3\\naddi  r1 r1 -1\\naddi  r3 r3 -1\\ncopy  r2 r0\\nload  r0 r0\\naddi  r2 r2 -1\\nload  r1 r2\\naddi  r2 r2 -1\\nload  pc r2\\naddi  r1 r1 -1\\naddi  r1 r1 1\\nconst r2 0\\nstore r2 r1\\naddi  r3 r0 -1\\nload  r3 r3\\nload  r2 r1\\nstore r2 r3\\naddi  r1 r1 -1\\naddi  r3 r3 -1\\ncopy  r2 r0\\nload  r0 r0\\naddi  r2 r2 -1\\nload  r1 r2\\naddi  r2 r2 -1\\nload  pc r2\\nconst r2 4994\\nlt    r2 r1 r2\\naddi  r3 pc 6\\nijmp  r3 r2\\nconst r2 0\\nconst r3 -1000000000\\nstore r3 r2\\nconst r3 214\\njump  r3\\naddi  r1 r1 1\\naddi  r2 r0 -1\\nload  r2 r2\\naddi  r2 r2 1\\nload  r2 r2\\nstore r2 r1\\naddi  r1 r1 1\\nconst r2 0\\nstore r2 r1\\nload  r3 r1\\naddi  r1 r1 -1\\nload  r2 r1\\neq    r2 r2 r3\\nstore r2 r1\\nload  r3 r1\\naddi  r1 r1 -1\\naddi  r2 pc 21\\nnjmp  r2 r3\\naddi  r1 r1 1\\naddi  r2 r0 -1\\nload  r2 r2\\naddi  r2 r2 3\\nload  r2 r2\\nstore r2 r1\\naddi  r3 r0 -1\\nload  r3 r3\\nload  r2 r1\\nstore r2 r3\\naddi  r1 r1 -1\\naddi  r3 r3 -1\\ncopy  r2 r0\\nload  r0 r0\\naddi  r2 r2 -1\\nload  r1 r2\\naddi  r2 r2 -1\\nload  pc r2\\naddi  r2 pc 79\\njump  r2\\naddi  r1 r1 1\\naddi  r2 r0 -1\\nload  r2 r2\\naddi  r2 r2 1\\nload  r2 r2\\nstore r2 r1\\naddi  r1 r1 1\\nconst r2 1\\nstore r2 r1\\nload  r3 r1\\naddi  r1 r1 -1\\nload  r2 r1\\nsub   r2 r2 r3\\nstore r2 r1\\naddi  r1 r1 1\\naddi  r2 r0 -1\\nload  r2 r2\\naddi  r2 r2 3\\nload  r2 r2\\nstore r2 r1\\naddi  r1 r1 1\\naddi  r2 r0 -1\\nload  r2 r2\\naddi  r2 r2 2\\nload  r2 r2\\nstore r2 r1\\naddi  r1 r1 1\\naddi  r2 r0 -1\\nload  r2 r2\\naddi  r2 r2 3\\nload  r2 r2\\nstore r2 r1\\nload  r3 r1\\naddi  r1 r1 -1\\nload  r2 r1\\nadd   r2 r2 r3\\nstore r2 r1\\nconst r4 74\\naddi  r2 r0 -2\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r2 1\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r2 1\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r1 -5\\naddi  r3 r1 -1\\nload  r1 r3\\naddi  r1 r1 1\\naddi  r0 r1 5\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r2 1\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r2 1\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r2 1\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r2 1\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\naddi  r2 r2 1\\naddi  r1 r1 1\\nload  r3 r2\\nstore r3 r1\\ncopy  pc r4\\naddi  r1 r1 1\\nconst r2 0\\nstore r2 r1\\naddi  r3 r0 -1\\nload  r3 r3\\nload  r2 r1\\nstore r2 r3\\naddi  r1 r1 -1\\naddi  r3 r3 -1\\ncopy  r2 r0\\nload  r0 r0\\naddi  r2 r2 -1\\nload  r1 r2\\naddi  r2 r2 -1\\nload  pc r2\\nnop   \\n\", \"sourcemap\": [{\"sourcestart\": {\"line\": 15, \"col\": 45}, \"sourceend\": {\"line\": 15, \"col\": 56}, \"asmstart\": 141, \"asmend\": 157, \"slpos\": \"main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A1 + $A2)\"}, {\"sourcestart\": {\"line\": 15, \"col\": 40}, \"sourceend\": {\"line\": 15, \"col\": 43}, \"asmstart\": 135, \"asmend\": 140, \"slpos\": \"main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr $A2\"}, {\"sourcestart\": {\"line\": 15, \"col\": 29}, \"sourceend\": {\"line\": 15, \"col\": 38}, \"asmstart\": 121, \"asmend\": 134, \"slpos\": \"main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A0 - 1)\"}, {\"sourcestart\": {\"line\": 15, \"col\": 28}, \"sourceend\": {\"line\": 15, \"col\": 57}, \"asmstart\": 121, \"asmend\": 157, \"slpos\": \"main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2))\"}, {\"sourcestart\": {\"line\": 15, \"col\": 5}, \"sourceend\": {\"line\": 15, \"col\": 57}, \"asmstart\": 121, \"asmend\": 198, \"slpos\": \"main.fibonacci[0].whenElseBody[0]\"}, {\"sourcestart\": {\"line\": 11, \"col\": 12}, \"sourceend\": {\"line\": 11, \"col\": 15}, \"asmstart\": 101, \"asmend\": 106, \"slpos\": \"main.fibonacci[0].whenBody_0[0].expr $A2\"}, {\"sourcestart\": {\"line\": 11, \"col\": 5}, \"sourceend\": {\"line\": 11, \"col\": 15}, \"asmstart\": 101, \"asmend\": 118, \"slpos\": \"main.fibonacci[0].whenBody_0[0]\"}, {\"sourcestart\": {\"line\": 9, \"col\": 8}, \"sourceend\": {\"line\": 9, \"col\": 18}, \"asmstart\": 83, \"asmend\": 96, \"slpos\": \"main.fibonacci[0].whenCond_0.expr ($A0 == 0)\"}, {\"sourcestart\": {\"line\": 4, \"col\": 10}, \"sourceend\": {\"line\": 4, \"col\": 12}, \"asmstart\": 42, \"asmend\": 45, \"slpos\": \"main[1].expr $0\"}, {\"sourcestart\": {\"line\": 4, \"col\": 3}, \"sourceend\": {\"line\": 4, \"col\": 12}, \"asmstart\": 42, \"asmend\": 57, \"slpos\": \"main[1]\"}, {\"sourcestart\": {\"line\": 3, \"col\": 34}, \"sourceend\": {\"line\": 3, \"col\": 35}, \"asmstart\": 28, \"asmend\": 30, \"slpos\": \"main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1).expr 1\"}, {\"sourcestart\": {\"line\": 3, \"col\": 31}, \"sourceend\": {\"line\": 3, \"col\": 32}, \"asmstart\": 25, \"asmend\": 27, \"slpos\": \"main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1).expr 0\"}, {\"sourcestart\": {\"line\": 3, \"col\": 27}, \"sourceend\": {\"line\": 3, \"col\": 29}, \"asmstart\": 22, \"asmend\": 24, \"slpos\": \"main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1).expr 20\"}, {\"sourcestart\": {\"line\": 3, \"col\": 26}, \"sourceend\": {\"line\": 3, \"col\": 36}, \"asmstart\": 22, \"asmend\": 30, \"slpos\": \"main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1)\"}, {\"sourcestart\": {\"line\": 3, \"col\": 3}, \"sourceend\": {\"line\": 3, \"col\": 36}, \"asmstart\": 21, \"asmend\": 41, \"slpos\": \"main[0]\"}, {\"sourcestart\": {\"line\": 3, \"col\": 12}, \"sourceend\": {\"line\": 3, \"col\": 36}, \"asmstart\": 21, \"asmend\": 41, \"slpos\": \"main[0].expr main.fibonacci(20, 0, 1)\"}]}"

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
-- (0,214)	main
-- (21,41)	main[0]
-- (42,57)	main[1]
-- (59,73)	main.forceReturn
-- (59,61)	main.forceReturn.expr 0
-- (25,27)	main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1).expr 0
-- (28,30)	main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1).expr 1
-- (22,24)	main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1).expr 20
-- (42,45)	main[1].expr $0
-- (21,41)	main[0].expr main.fibonacci(20, 0, 1)
-- (22,30)	main[0].expr main.fibonacci(20, 0, 1).expr (20, 0, 1)
-- (74,213)	main.fibonacci
-- (83,198)	main.fibonacci[0]
-- (101,118)	main.fibonacci[0].whenBody_0[0]
-- (121,198)	main.fibonacci[0].whenElseBody[0]
-- (83,100)	main.fibonacci[0].whenCond_0
-- (101,120)	main.fibonacci[0].whenBody_0
-- (121,198)	main.fibonacci[0].whenElseBody
-- (199,213)	main.fibonacci.forceReturn
-- (199,201)	main.fibonacci.forceReturn.expr 0
-- (89,91)	main.fibonacci[0].whenCond_0.expr ($A0 == 0).expr 0
-- (127,129)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A0 - 1).expr 1
-- (121,126)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A0 - 1).expr $A0
-- (83,88)	main.fibonacci[0].whenCond_0.expr ($A0 == 0).expr $A0
-- (141,146)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A1 + $A2).expr $A1
-- (101,106)	main.fibonacci[0].whenBody_0[0].expr $A2
-- (147,152)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A1 + $A2).expr $A2
-- (135,140)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr $A2
-- (141,157)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A1 + $A2)
-- (121,134)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2)).expr ($A0 - 1)
-- (83,96)	main.fibonacci[0].whenCond_0.expr ($A0 == 0)
-- (121,157)	main.fibonacci[0].whenElseBody[0].expr (($A0 - 1), $A2, ($A1 + $A2))
