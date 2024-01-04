{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module SimpleLang.FromString.Test where

import MachineLang.FromSimpleLang.Test
import SimpleLang.FromString
import SimpleLang.Def
import Data.Text as T
import Data.Text.IO as TIO
import Control.Monad

-- >>> error $ T.unpack $ prettyPrintSLProgram (mlctTest (mlctTests !! 0))
-- function #main() -> int
-- {
--   int $0 = 100
--   int $1 = 200
--   int $2 = 300
--   int $3 = 400
--   $2 = 10000
--   return ($0 + $1)
-- }

-- >>> error $ T.unpack $ either id prettyPrintSLProgram $ textToSLProgram $ prettyPrintSLProgram (mlctTest (mlctTests !! 0))
-- main.slang:2:20:
--   |
-- 2 | function #main() -> int
--   |                    ^^^
-- unexpected " in"
-- expecting "int", '(', or '['

diffPrintParsePrint :: SLProgram -> Text
diffPrintParsePrint program = 
  let printed = prettyPrintSLProgram program
      reprinted = either id prettyPrintSLProgram $ textToSLProgram printed
  in  if printed == reprinted
        then "OK."
        else T.unlines [
              "Original program:"
            , prettyPrintSLProgram program
            , "Reprinted program:"
            , either id prettyPrintSLProgram $ textToSLProgram printed
            ]

testPrintParsePrint :: SLProgram -> Bool
testPrintParsePrint program = 
  let printed = prettyPrintSLProgram program
      reprinted = either id prettyPrintSLProgram $ textToSLProgram printed
  in  printed == reprinted


-- >>> testPrintParsePrint . mlctTest <$> mlctTests
-- [True,True,True,True,True,True,True,True,True]



-- >>> error $ T.unpack $ diffPrintParsePrint $ mlctTest (mlctTests !! 6)
-- Original program:
-- function main() -> int
-- {
--   (int, int) $0 = (100, 200)
--   (int, int) $2 = (300, 400)
--   int $4 = 1111111
--   (int, int) $5 = main.complexProd($0, $2)
--   int $7 = 2222222
--   return $5.1
-- }
-- function main.complexProd((int, int) $A0, (int, int) $A1) -> (int, int)
-- {
--   int $0 = $0.0
--   int $1 = $0.1
--   int $2 = $1.0
--   int $3 = $1.1
--   int $4 = (($0 * $2) - ($1 * $3))
--   int $5 = (($0 * $3) + ($1 * $2))
--   return ($4, $5)
-- }
-- Reprinted program:
-- main.slang:14:3:
--    |
-- 14 |   int $0 = $0.0
--    |   ^
-- unexpected 'i'
-- expecting ';', '}', end of input, newline, or white space


-- >>> error $ T.unpack $ textToSourcemap $ prettyPrintSLProgram $ mlctTest (mlctTests !! 5)
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 4, sourceColumn = Pos 3}	main[0]
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 5, sourceColumn = Pos 3}	main[1]
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 6, sourceColumn = Pos 3}	main[2]
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 4, sourceColumn = Pos 65}	main[0].expr ((100, 200, 300), (1000, 2000, 3000), (10000, 20000, 30000)).expr (100, 200, 300).expr 100
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 4, sourceColumn = Pos 70}	main[0].expr ((100, 200, 300), (1000, 2000, 3000), (10000, 20000, 30000)).expr (100, 200, 300).expr 200
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 4, sourceColumn = Pos 75}	main[0].expr ((100, 200, 300), (1000, 2000, 3000), (10000, 20000, 30000)).expr (100, 200, 300).expr 300
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 4, sourceColumn = Pos 83}	main[0].expr ((100, 200, 300), (1000, 2000, 3000), (10000, 20000, 30000)).expr (1000, 2000, 3000).expr 1000
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 4, sourceColumn = Pos 89}	main[0].expr ((100, 200, 300), (1000, 2000, 3000), (10000, 20000, 30000)).expr (1000, 2000, 3000).expr 2000
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 4, sourceColumn = Pos 95}	main[0].expr ((100, 200, 300), (1000, 2000, 3000), (10000, 20000, 30000)).expr (1000, 2000, 3000).expr 3000
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 4, sourceColumn = Pos 104}	main[0].expr ((100, 200, 300), (1000, 2000, 3000), (10000, 20000, 30000)).expr (10000, 20000, 30000).expr 10000
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 4, sourceColumn = Pos 111}	main[0].expr ((100, 200, 300), (1000, 2000, 3000), (10000, 20000, 30000)).expr (10000, 20000, 30000).expr 20000
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 4, sourceColumn = Pos 118}	main[0].expr ((100, 200, 300), (1000, 2000, 3000), (10000, 20000, 30000)).expr (10000, 20000, 30000).expr 30000
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 6, sourceColumn = Pos 12}	main[2].expr $9
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 4, sourceColumn = Pos 76}	main[0].expr ((100, 200, 300), (1000, 2000, 3000), (10000, 20000, 30000)).expr (100, 200, 300)
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 4, sourceColumn = Pos 96}	main[0].expr ((100, 200, 300), (1000, 2000, 3000), (10000, 20000, 30000)).expr (1000, 2000, 3000)
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 4, sourceColumn = Pos 119}	main[0].expr ((100, 200, 300), (1000, 2000, 3000), (10000, 20000, 30000)).expr (10000, 20000, 30000)
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 4, sourceColumn = Pos 120}	main[0].expr ((100, 200, 300), (1000, 2000, 3000), (10000, 20000, 30000))
-- SourcePos {sourceName = "main.slang", sourceLine = Pos 5, sourceColumn = Pos 18}	main[1].expr $0.1.1
