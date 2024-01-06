{-# LANGUAGE OverloadedStrings #-}

module SimpleLang.FromString (
    textToSLProgram
  , textToSourcemap
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
import Data.Maybe
import SimpleLang.StaticCheck
import Control.Applicative hiding (many, some)
import Data.Set as S
-- import Debug.Trace

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
  , lpsExprPosMap :: M.Map [SLLocalPos] (SourcePos, Int)
  , lpsSourceMap :: M.Map SLPos (SourcePos, Int)
  } deriving (Show)

emptyState :: LocalParserState
emptyState = LocalParserState {
    lpsLocals = M.empty
  , lpsArgs   = M.empty
  , lpsFuncs  = M.empty
  , lpsFirstPath = False
  , lpsSLPos  = Nothing
  , lpsSourceMap = M.empty
  , lpsExprPosMap = M.empty
}

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
    Nothing -> pure (SLTInt, n) <$ registerCustomError $ SLParserError $ "Local variable " <> n <> " not found"

parseArg :: LocalParser (SLType, Text)
parseArg = unwrapspace $ do
  n <- string "$" *> parseName
  vdict <- gets lpsArgs
  case M.lookup n vdict of
    Just t  -> pure (t, n)
    Nothing -> pure (SLTInt, n) <$ registerCustomError $ SLParserError $ "Argument " <> n <> " not found"


parseVar :: LocalParser SLExp
parseVar =  unwrapspace $ do
  n <- string "$" *> parseName
  ldict <- gets lpsLocals
  adict <- gets lpsArgs
  case M.lookup n ldict of
    Just t  -> pure (SLELocal t n)
    Nothing ->
      case M.lookup n adict of
        Just t  -> pure (SLEArg t n)
        Nothing -> pure (SLEConst (SLVal 0)) <$ registerCustomError $ SLParserError $ "variable " <> n <> " not found"

parseRef :: LocalParser SLRef
parseRef =
  choice [
      try $ uncurry SLRefLocal <$> parseLocal
    , try $ uncurry SLRefPtr   <$> parseTypedExpInExp
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
      Nothing -> pure (SLFuncSignature n [] SLTInt) <$ registerCustomError $ SLParserError $ "Function " <> prettyPrintSLFuncName n <> " not found"

parseCall :: LocalParser SLCall
parseCall =
  choice [
      try $ SLSolidFuncCall <$> parseFuncSignature <* hspace <*> (parseExp <&> \e -> case e of SLEStructCons _ _ -> e; SLEStructNil -> e; _ -> SLEStructCons e SLEStructNil)
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
      Prefix (SLEUnion <$ string "%%" <*> parseType)
    ]
  , [
      Prefix (SLECast <$ string "%(" <*> parseType <* string ")")
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
parseParensExpr = char '(' *> parseExpInExp <* char ')'

parseExpInExp :: LocalParser SLExp
parseExpInExp = unwrapspace $ do
  exprposmapext <- gets lpsExprPosMap
  modify (\s -> s {lpsExprPosMap = M.empty})
  sourcepos <- getSourcePos
  offset <- getOffset
  expr <- choice [
        makeExprParser parseTerm operators
    ]
  exprposmap <- gets (M.mapKeys (SLLPExpr expr :) . M.insert [] (sourcepos, offset) . lpsExprPosMap)
  modify (\s -> s {lpsExprPosMap = exprposmapext <> exprposmap})
  pure expr

appendExprPosMap :: LocalParser SLExp -> LocalParser SLExp
appendExprPosMap p = do
  sourcepos <- getSourcePos
  offset <- getOffset
  expr <- p
  exprposmap <- gets (M.insert [SLLPExpr expr] (sourcepos, offset) . lpsExprPosMap)
  modify (\s -> s {lpsExprPosMap = exprposmap})
  pure expr



parseExp :: LocalParser SLExp
parseExp = do
  pos <- gets lpsSLPos
  expr <- parseExpInExp
  exprposmap <- gets lpsExprPosMap
  case pos of
    Nothing -> pure ()
    Just pos' -> do
      sourcemap <- gets ((<> M.mapKeys (L.foldl (flip pushPos) pos') exprposmap) . lpsSourceMap)
      modify (\s -> s {lpsExprPosMap = M.empty, lpsSourceMap = sourcemap})
  pure expr

parseTerm :: LocalParser SLExp
parseTerm = unwrapspace $
  choice [
      try $ appendExprPosMap $ SLEConst . SLVal <$> MPL.signed (pure ()) MPL.decimal
    , try $ appendExprPosMap $ parseVar
    , try $ appendExprPosMap $ string "&" $> SLEAddrOf <*> parseRef
    , try $ appendExprPosMap $ string "&" $> SLEFuncPtr <*> parseFuncSignature
    , try $ appendExprPosMap $ SLEPushCall <$> parseCall
    , try parseParensExpr
    , try $ L.foldr SLEStructCons SLEStructNil <$> (char '(' *> sepBy parseExpInExp (char ',') <* char ')')
    , try $ some alphaNumChar $> SLELocal SLTInt "dummy" <* registerCustomError (SLParserError "Invalid expression (forgot to put $?)")
    ]

parseTypedExp :: LocalParser (SLType, SLExp)
parseTypedExp = parseTypedExp' False

parseTypedExpInExp :: LocalParser (SLType, SLExp)
parseTypedExpInExp = parseTypedExp' True

parseTypedExp' :: Bool -> LocalParser (SLType, SLExp)
parseTypedExp' inexp = do
  exp <- if inexp then parseExpInExp else parseExp
  isFirstPath <- gets lpsFirstPath
  if isFirstPath then pure (SLTInt, exp)
  else
    case sleTypeOf exp of
      Right t  -> pure (t, exp)
      Left err -> pure (SLTInt, exp) <$ registerCustomError $ SLParserError $ prettyPrintSLTypeError err

parseStatement :: LocalParser SLStatement
parseStatement = do
  appendSourceMapHere *> choice [
      try $ SLSReturn <$ string "return" <* hspace <*> parseExp
    , try $ SLSTailCallReturn <$ string "tailcall" <* hspace <*> parseCall
    , try (do
          t <- parseType
          _ <- hspace <* string "$"
          n <- parseName
          _ <- hspace <* string "="  <* hspace
          (t', e) <- parseTypedExp
          isFirstPath <- gets lpsFirstPath
          when (not isFirstPath && t /= t') $ registerCustomError $ SLParserError $ "Type mismatch in var decl. expected: " <> prettyPrintSLType t <> " , got: " <> prettyPrintSLType t'
          modify (\s -> s { lpsLocals = M.insert n t (lpsLocals s) })
          pure $ SLSInitVar n e
        )
    , try $ SLSSubst <$> parseRef  <* hspace <* string "="  <* hspace <*> parseExp
    ]


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
        Nothing            -> customFailure $ SLParserError $ "Invalid function name: " <> T.intercalate "." n

parseFunction :: LocalParserState -> Parser (SLFuncBlock, M.Map SLPos (SourcePos, Int))
parseFunction initstate = do
  _ <- scn *> string "function" <* scn
  fname <- parseFuncName
  fargs <- char '(' *> sepBy (flip (,) <$> parseType <* string "$" <*> parseName) (char ',') <* char ')'
  _ <- scn
  fret  <- string "->" *> scn *> parseType
  (fbody, LocalParserState {lpsSourceMap = smap}) <- runStateT parseBlock (initstate { lpsArgs = M.fromList fargs, lpsSLPos = Just (SLPos fname [])})
  let fsig = SLFuncSignature fname (snd <$> fargs) fret
  pure $ (SLFuncBlock fsig (fst <$> fargs) fbody, smap)

parseFDict :: Parser (M.Map SLFuncName SLFuncSignature)
parseFDict = (\fbs -> M.fromList $ (\(fb, _) -> ((slfsName . slfSignature) fb, slfSignature fb)) <$> fbs) <$ scn <*> many (parseFunction (emptyState {lpsFirstPath = True})) <* eof

parseSLProgram :: M.Map SLFuncName SLFuncSignature -> Parser (SLProgram, M.Map SLPos (SourcePos, Int))
parseSLProgram fdict = (\fbs -> (M.fromList $ (\(fb, _) -> ((slfsName . slfSignature) fb, fb)) <$> fbs, L.foldl (<>) M.empty (snd <$> fbs))) <$ scn <*> many (parseFunction (emptyState {lpsFuncs = fdict})) <* eof

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

sepByIndex :: Alternative m => (Int -> m a) -> m sep -> m [a]
sepByIndex p sep = sepBy1Index p sep <|> pure []
{-# INLINE sepByIndex #-}

sepBy1Index :: Alternative m => (Int -> m a) -> m sep -> m [a]
sepBy1Index p sep = liftA2 (:) (p 0) (manyIndex (\i -> sep *> p (i + 1)))
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

registerCustomError :: SLParserError -> LocalParser ()
registerCustomError = registerFancyFailure . S.singleton . ErrorCustom


appendSourceMapHere :: LocalParser ()
appendSourceMapHere = do
  pos <- gets lpsSLPos
  case pos of
    Nothing -> pure ()
    Just pos' -> do
      sourceMap <- gets lpsSourceMap
      spos <- getSourcePos
      offset <- getOffset
      modify (\s -> s {lpsSourceMap = M.insert pos' (spos, offset) sourceMap})


textToSLProgram :: Text -> Either Text SLProgram
textToSLProgram t = do
  fdict <- first (pack . errorBundlePretty) $ parse parseFDict "main.slang" t
  (program, sourcemap) <- first (pack . errorBundlePretty) $ parse (parseSLProgram fdict) "main.slang" t
  case  slscCheck program of
    Right _ -> pure program
    Left err -> do
      _ <- case M.lookup (slscegetPos err) sourcemap of
            Just (_, offset) -> do
              first (pack . errorBundlePretty) $ parse (parseError (FancyError offset ((S.singleton . ErrorCustom . SLParserError . slsceMessage) err))) "main.slang" t
            Nothing -> 
              pure ()
      Left $ prettyPrintSLSCError err


textToSourcemap :: Text -> Text
textToSourcemap t = either id id $ do
  fdict <- first (pack . errorBundlePretty) $ parse parseFDict "main.slang" t
  (program, sourcemap) <- first (pack . errorBundlePretty) $ parse (parseSLProgram fdict) "main.slang" t
  case  slscCheck program of
    Right _ -> pure $ T.unlines $ (\(p, s) -> pack (show s) <> "\t" <> prettyPrintSLPos p) <$> M.assocs sourcemap
    Left err -> do
      Left $ prettyPrintSLSCError err