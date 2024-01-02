{-# LANGUAGE OverloadedStrings #-}

module SimpleLang.FromString (
    textToSLProgram
) where

import SimpleLang.Def
import Data.Text as T
import Text.Megaparsec as MP hiding (State)
import Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Control.Monad.Combinators.Expr
import Data.Functor
import Data.Map as M
import Control.Monad.State
import Prelude hiding (exp)
import qualified Data.Vector as V
import qualified Data.List as L
import Data.Bifunctor
import Debug.Trace
import Data.Maybe

newtype SLParserError = SLParserError Text deriving (Show, Eq, Ord)
instance ShowErrorComponent  SLParserError where
  showErrorComponent (SLParserError t) = T.unpack t

type Parser = Parsec SLParserError Text

data LocalParserState = LocalParserState {
    lpsLocals :: M.Map Text SLType
  , lpsArgs   :: M.Map Text SLType
  , lpsFuncs  :: M.Map SLFuncName SLFuncSignature
  , lpsFirstPath :: Bool
  } deriving (Show)

emptyState :: LocalParserState
emptyState = LocalParserState M.empty M.empty M.empty True

type LocalParser = StateT LocalParserState Parser


parseStructType :: MonadParsec SLParserError Text m => m [SLType]
parseStructType =
  char '(' *> sepBy parseType (char ',') <* char ')'

parseUnionType :: MonadParsec SLParserError Text m => m [SLType]
parseUnionType =
  char '[' *> sepBy parseType (char '|') <* char ']'

parseType :: MonadParsec SLParserError Text m => m SLType
parseType = unwrapspace $ do
  choice [
      try $ SLTInt  <$ string "int"
    , try $ SLTFuncPtr <$> parseStructType <* hspace <* string "->" <* hspace <*> parseType
    , try $ SLTStruct <$> parseStructType
    , try $ SLTUnion <$> parseUnionType
    ]

parseName :: MonadParsec SLParserError Text m => m Text
parseName = T.pack <$> some alphaNumChar

parseLocal :: LocalParser (SLType, Text)
parseLocal =  unwrapspace $ do
  n <- string "$" *> parseName
  vdict <- gets lpsLocals
  case M.lookup n vdict of
    Just t  -> pure (t, n)
    Nothing -> customFailure $ SLParserError $ "Local variable " <> n <> " not found"

parseArg :: LocalParser (SLType, Text)
parseArg = unwrapspace $ do
  n <- string "$" *> parseName
  vdict <- gets lpsArgs
  case M.lookup n vdict of
    Just t  -> pure (t, n)
    Nothing -> customFailure $ SLParserError $ "Argument " <> n <> " not found"

parseRef :: LocalParser SLRef
parseRef =
  choice [
      try $ uncurry SLRefLocal <$> parseLocal
    , try $ uncurry SLRefPtr   <$> parseTypedExp
    ]

parseFuncSignature :: LocalParser SLFuncSignature
parseFuncSignature = do
  n <- parseFuncName
  fdict <- gets lpsFuncs
  isFirstPath <- gets lpsFirstPath
  if isFirstPath then pure $ SLFuncSignature n [] SLTInt
  else 
    case M.lookup n fdict of
      Just t  -> pure t
      Nothing -> customFailure $ SLParserError $ "Function " <> prettyPrintSLFuncName n <> " not found"

parseCall :: LocalParser SLCall
parseCall =
  choice [
      try $ SLSolidFuncCall <$> parseFuncSignature <* hspace <*> parseExp
    , try $ SLClosureCall   <$ string "@@" <*> parseExp
    , try $ SLFuncRefCall   <$ string "@" <*> parseRef <*> parseExp
  ]

operators :: [[Operator LocalParser SLExp]]
operators = [
    [
      Postfix (flip (L.foldl SLEStructGet) <$> some (char '.' *> MPL.decimal))
    ]
  , [
      Prefix (SLEIndirection   <$ string "*" )
    ]
  , [
      Prefix (SLEUnion <$ string "%" <*> parseType)
    ]
  , [
      --Prefix (SLECast <$ string "(" <*> parseType <* string ")")
    ]
  , [
      Prefix (SLEPrim1 SLPrim1Inv   <$ string "!" )
    ]
  , [
      InfixL (SLEPrim2 SLPrim2Mult  <$ string "*" )
    ]
  , [
      InfixL (SLEPrim2 SLPrim2Add   <$ string "+" )
    , InfixL (SLEPrim2 SLPrim2Sub   <$ string "-" )
    ]
  , [
      InfixL (SLEPtrShift           <$ string "@+" )
    ]
  , [
      InfixL (SLEPrim2 SLPrim2Shift <$ string "<<")
    ]
  , [
      InfixL (SLEPrim2 SLPrim2And   <$ string "&" )
    ]
  , [
      InfixL (SLEPrim2 SLPrim2Or    <$ string "|" )
    ]
  , [
      InfixL (SLEPrim2 SLPrim2Xor   <$ string "^" )
    ]
  , [
      InfixL (SLEPrim2 SLPrim2Gt    <$ string ">" )
    , InfixL (SLEPrim2 SLPrim2Lt    <$ string "<" )
    ]
  , [
      InfixL (SLEPrim2 SLPrim2Eq    <$ string "==")
    ]
  , [
      InfixL (SLEPrim2 SLPrim2And   <$ string "&&" )
    ]
  , [
      InfixL (SLEPrim2 SLPrim2Or    <$ string "||" )
    ]
  ]

parseParensExpr :: LocalParser SLExp
parseParensExpr = char '(' *> parseExp <* char ')'

parseExp :: LocalParser SLExp
parseExp = unwrapspace $
  choice [
        try $ makeExprParser parseTerm operators
      --, try parseTerm
    ]

parseTerm :: LocalParser SLExp
parseTerm = unwrapspace $
  choice [
      try $ SLEConst . SLVal <$> MPL.signed (pure ()) MPL.decimal
    , try $ uncurry SLELocal <$> parseLocal
    , try $ uncurry SLEArg   <$> parseArg
    , try $ string "&" $> SLEAddrOf <*> parseRef
    , try $ string "&" $> SLEFuncPtr <*> parseFuncSignature
    , try $ SLEPushCall <$> parseCall
    , try parseParensExpr
    , try $ L.foldr SLEStructCons SLEStructNil <$> (char '(' *> sepBy parseExp (char ',') <* char ')')
    ]

parseTypedExp :: LocalParser (SLType, SLExp)
parseTypedExp = do
  exp <- parseExp
  isFirstPath <- gets lpsFirstPath
  if isFirstPath then pure (SLTInt, exp)
  else
    case sleTypeOf exp of
      Right t  -> pure (t, exp)
      Left err -> customFailure $ SLParserError $ prettyPrintSLTypeError err

parseStatement :: LocalParser SLStatement
parseStatement = do
  choice [
      try $ SLSReturn <$ string "return" <* hspace <*> parseExp
    , try $ SLSTailCallReturn <$ string "tailcall" <* hspace <*> parseCall
    , try (do
          t <- parseType
          _ <- hspace <* string "$"
          n <- parseName
          _ <- hspace <* string "="  <* hspace
          (t', e) <- parseTypedExp
          isFirstPath <- gets lpsFirstPath
          when (not isFirstPath && t /= t') $ customFailure $ SLParserError $ "Type mismatch in var decl. expected: " <> prettyPrintSLType t <> " , got: " <> prettyPrintSLType t'
          modify (\s -> s { lpsLocals = M.insert n t (lpsLocals s) })
          pure $ SLSInitVar n e
        )
    , try $ SLSSubst <$> parseRef  <* hspace <* string "="  <* hspace <*> parseExp
    ]


parseBlock :: LocalParser SLBlock
parseBlock = do
  choice [
      try $ SLBMulti  <$ scn <*> (char '{' *> scn *> (V.fromList . catMaybes <$> sepBy (try (Just <$> parseBlock) <|> pure Nothing) parseEOS) <* scn <* char '}')
    , try $ SLBCase   <$ scn <*> (fmap V.fromList . some $ ((,) <$ string "when" <* hspace <*> parseExp <* scn <*> parseBlock)) <* scn <* string "else" <* scn <*> parseBlock
    , try $ SLBWhile  <$ scn <*  string "while" <*> parseExp <*> parseBlock
    , try $ SLBSingle <$ scn <*> parseStatement
    ]

parseFuncName :: MonadParsec SLParserError Text m => m SLFuncName
parseFuncName = do
  n <- sepBy parseName (char '.')
  case n of
    ["main"] -> pure SLFuncMain
    [fname]  -> pure $ SLUserFunc "main" fname
    _    -> 
      case L.uncons (L.reverse n) of
        Just (fname, revm) -> pure $ SLUserFunc (T.intercalate "." (L.reverse revm)) fname
        Nothing         -> customFailure $ SLParserError $ "Invalid function name: " <> T.intercalate "." n

parseFunction :: M.Map SLFuncName SLFuncSignature -> Bool -> Parser SLFuncBlock
parseFunction fdict errorOnNonExistentFunc = do
  _ <- scn *> string "function" <* scn
  fname <- parseFuncName
  fargs <- char '(' *> sepBy (flip (,) <$> parseType <* string "$" <*> parseName) (char ',') <* char ')'
  _ <- scn
  fret  <- string "->" *> scn *> parseType
  fbody <- runStateT parseBlock (emptyState { lpsArgs = M.fromList fargs, lpsFuncs = fdict, lpsFirstPath = errorOnNonExistentFunc})
  let fsig = SLFuncSignature fname (snd <$> fargs) fret
  pure $ SLFuncBlock fsig (fst <$> fargs) (fst fbody)

parseFDict :: Parser (M.Map SLFuncName SLFuncSignature)
parseFDict = (\fbs -> M.fromList $ (\fb -> ((slfsName . slfSignature) fb, slfSignature fb)) <$> fbs) <$ scn <*> many (parseFunction M.empty True) <* eof

parseSLProgram :: M.Map SLFuncName SLFuncSignature -> Parser SLProgram
parseSLProgram fdict = (\fbs -> M.fromList $ (\fb -> ((slfsName . slfSignature) fb, fb)) <$> fbs) <$ scn <*> many (parseFunction fdict False) <* eof

parseEOS :: LocalParser ()
parseEOS =
  choice [
      try $ MP.hspace *> char ';' $> ()
    , try $ MP.hspace *> char '\n' $> ()
    , try $ MP.hspace *> eof
    ]

scn :: MonadParsec SLParserError Text m => m ()
scn = MPL.space
  MP.space1
  (MPL.skipLineComment "//")
  (MPL.skipBlockComment "/*" "*/")


unwrapspace :: MonadParsec SLParserError Text m => m a -> m a
unwrapspace p = MP.hspace *> p <* MP.hspace

textToSLProgram :: Text -> Either Text SLProgram
textToSLProgram t = do
  fdict <- first (pack . errorBundlePretty) $ parse parseFDict "main.slang" t
  first (pack . errorBundlePretty) $ parse (parseSLProgram fdict) "main.slang" t