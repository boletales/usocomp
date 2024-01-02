{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
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
