{-# LANGUAGE OverloadedStrings #-}

module SimpleLang.FromString where

import SimpleLang.Def
import Data.Text as T
import Data.Text.IO as TIO
import Text.Megaparsec as MP
import Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Data.Void
import Data.Functor

type Parser = Parsec Void Text



parseStructType :: Parser [SLType]
parseStructType = 
  char '(' *> sepBy parseType (char ',') <* char ')'

parseUnionType :: Parser [SLType]
parseUnionType = 
  char '(' *> sepBy parseType (char '|') <* char ')'

parseType :: Parser SLType
parseType = 
  choice [
      try $ string "int"  >> pure SLTInt
    , try $ string "unit" >> pure SLTUnit
    , try $ SLTStruct <$> parseStructType
    , try $ SLTFuncPtr <$> parseStructType <* string "->" <*> parseType
    , try $ SLTUnion <$> parseUnionType
    ]

{-
parseStatement :: Parser SLStatement
parseStatement = do
  choice [
      try $ SLSInitVar
    ]
-}

parseSpace :: Parser ()
parseSpace = MPL.space 
  MP.space1
  (MPL.skipLineComment "//")
  (MPL.skipBlockComment "/*" "*/")