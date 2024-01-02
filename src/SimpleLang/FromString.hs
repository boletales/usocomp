{-# LANGUAGE OverloadedStrings #-}

module SimpleLang.FromString (
    textToSLProgram
) where

import SimpleLang.Def
import SimpleLang.Tools
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
import SimpleLang.StaticCheck
import Control.Applicative hiding (many, some)
import Data.Set as S

newtype SLParserError = SLParserError Text deriving (Show, Eq, Ord)
instance ShowErrorComponent  SLParserError where
  showErrorComponent (SLParserError t) = T.unpack t

type Parser = Parsec SLParserError Text

data LocalParserState = LocalParserState {
    lpsLocals :: M.Map Text SLType
  , lpsArgs   :: M.Map Text SLType
  , lpsFuncs  :: M.Map SLFuncName SLFuncSignature
  , lpsFirstPath :: Bool
  , lpsSLPos  :: Maybe SLPos
  , lpsReportingError :: Maybe SLSCError
  } deriving (Show)

emptyState :: LocalParserState
emptyState = LocalParserState M.empty M.empty M.empty True Nothing Nothing

type LocalParser = StateT LocalParserState Parser

{-# SPECIALISE parseStructType :: LocalParser [SLType] #-}
parseStructType :: MonadParsec SLParserError Text m => m [SLType]
parseStructType =
  char '(' *> sepBy parseType (char ',') <* char ')'

{-# SPECIALISE parseUnionType :: LocalParser [SLType] #-}
parseUnionType :: MonadParsec SLParserError Text m => m [SLType]
parseUnionType =
  char '[' *> sepBy parseType (char '|') <* char ']'

{-# SPECIALISE parseType :: LocalParser SLType #-}
parseType :: MonadParsec SLParserError Text m => m SLType
parseType = unwrapspace $ do
  choice [
      try $ SLTInt  <$ string "int"
    , try $ SLTFuncPtr <$> parseStructType <* hspace <* string "->" <* hspace <*> parseType
    , try $ SLTStruct <$> parseStructType
    , try $ SLTUnion <$> parseUnionType
    ]

{-# SPECIALISE parseFuncName :: LocalParser SLFuncName #-}
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
parseExp = unwrapspace $ choice [
        try $ makeExprParser parseTerm operators
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
    ] <* reportNonParserError


parseBlock :: LocalParser SLBlock
parseBlock = do
  choice [
      try $ SLBMulti  <$ scn <*> (char '{' *> scn *> (V.fromList . catMaybes <$> sepByIndex (\i -> try (Just <$> inPos (SLLPMulti i) parseBlock) <|> pure Nothing) parseEOS) <* scn <* char '}')
    , try $ SLBCase   <$ scn <*> (fmap V.fromList . someIndex $ (\i -> (,) <$ string "when" <* hspace <*> inPos (SLLPCaseCond i) parseExp <* scn <*> inPos (SLLPCaseBody i) parseBlock)) <* scn <* string "else" <* scn <*> inPos SLLPCaseElseBody parseBlock
    , try $ SLBWhile  <$ scn <*  string "while" <*> inPos SLLPWhileCond parseExp <*> inPos SLLPWhileBody parseBlock
    , try $ SLBSingle <$ scn <*> parseStatement
    ]

{-# SPECIALISE parseFuncName :: LocalParser SLFuncName #-}
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
  fbody <- runStateT parseBlock (emptyState { lpsArgs = M.fromList fargs, lpsFuncs = fdict, lpsFirstPath = errorOnNonExistentFunc, lpsSLPos = Just (SLPos fname [])})
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

{-# SPECIALISE scn :: LocalParser () #-}
scn :: MonadParsec SLParserError Text m => m ()
scn = MPL.space
  MP.space1
  (MPL.skipLineComment "//")
  (MPL.skipBlockComment "/*" "*/")

{-# SPECIALISE unwrapspace :: LocalParser a -> LocalParser a #-}
unwrapspace :: MonadParsec SLParserError Text m => m a -> m a
unwrapspace p = MP.hspace *> p <* MP.hspace


inPos :: SLLocalPos -> LocalParser x -> LocalParser x
inPos newpos v = do
    oldpos <- gets lpsSLPos
    modify (\s -> s {lpsSLPos = pushPos newpos <$> oldpos})
    x <- v
    modify (\s -> s {lpsSLPos = oldpos})
    pure x

outPos :: LocalParser x -> LocalParser x
outPos v = do
    oldpos <- gets lpsSLPos
    modify (\s -> s {lpsSLPos = popPos <$> oldpos})
    x <- v
    modify (\s -> s {lpsSLPos = oldpos})
    pure x

sepByIndex :: Alternative m => (Int -> m a) -> m sep -> m [a]
sepByIndex p sep = sepBy1Index p sep <|> pure []
{-# INLINE sepByIndex #-}

sepBy1Index :: Alternative m => (Int -> m a) -> m sep -> m [a]
sepBy1Index p sep = liftA2 (:) (p 0) (manyIndex (\i -> sep *> p i))
{-# INLINE sepBy1Index #-}

manyIndex :: Alternative f => (Int -> f a)-> f [a]
manyIndex v = many_v 0
  where
    many_v i = some_v i <|> pure []
    some_v i = liftA2 (:) (v i) (many_v (i + 1))

someIndex :: Alternative f => (Int -> f a)-> f [a]
someIndex v = some_v 0
  where
    many_v i = some_v i <|> pure []
    some_v i = liftA2 (:) (v i) (many_v (i + 1))

reportNonParserError :: LocalParser ()
reportNonParserError = do
  e <- gets lpsReportingError
  case e of
    Nothing -> pure ()
    Just e' -> do
      pos <- gets lpsSLPos
      case pos of
        Nothing -> pure ()
        Just pos' -> when (arePosOnSameStatement (slscegetPos e') pos') $
          (registerFancyFailure . S.singleton . ErrorCustom) $ SLParserError $ prettyPrintSLSCError e'

arePosOnSameStatement :: SLPos -> SLPos -> Bool
arePosOnSameStatement (SLPos f1 xs1) (SLPos f2 xs2) =
  if L.length xs1 < L.length xs2 then arePosOnSameStatement (SLPos f2 xs2) (SLPos f1 xs1)
  else
    (f1 == f2 && xs2 `L.isPrefixOf` xs1) && (case L.drop (L.length xs2) xs1 of
        [] -> True
        (x:_) -> case x of
          SLLPExpr _ -> True
          _ -> False)




textToSLProgram :: Text -> Either Text SLProgram
textToSLProgram t = do
  fdict <- first (pack . errorBundlePretty) $ parse parseFDict "main.slang" t
  program <- first (pack . errorBundlePretty) $ parse (parseSLProgram fdict) "main.slang" t
  first prettyPrintSLSCError $ slscCheck program
  pure program
