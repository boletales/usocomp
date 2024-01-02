{-# LANGUAGE OverloadedStrings #-}

module SimpleLang.StaticCheck (
    SLSCError(..)
  , slscCheck
  , slscegetPos
  , slsceMessage
  , prettyPrintSLSCError
) where

import SimpleLang.Def
import SimpleLang.Tools
import Control.Monad
import Data.Text as T hiding (
    words
  )
import Data.Map as M
import Control.Monad.State as S
import Control.Monad.Except
import Data.Foldable as F
import Data.Vector as V hiding (forM_)
import qualified Data.List as L
import Prelude hiding (
    id
  , (.)
  , lookup
  , foldl
  , words
  , exp
  )

data SLSCError =
    SLSCTypeError SLTypeError SLPos
  | SLSCNoSuchVariable Text SLPos
  | SLSCNoSuchArgument Text SLPos
  | SLSCNoSuchFunction Text SLPos
  | SLSCWrongVariableType Text SLType SLType SLPos
  | SLSCWrongArgumentType Text SLType SLType SLPos
  | SLSCWrongFunctionType Text SLType SLType SLPos
  | SLSCWrongReturnType Text SLType SLType SLPos
  | SLSCFunctionNameMismatch SLFuncName SLFuncName SLPos
  | SLSCWrongArgTypeToPrim Text SLType SLPos
  | SLSCWrongUnionType Text SLType SLType SLPos
  | SLSCUnionMustBeUnion SLExp SLPos
  | SLSCRefTypeMismatch Text SLType SLType SLPos
  | SLSCArgumentMismatch Text SLType SLType SLPos
  | SLSCWrongTypeForCond SLType SLPos
  | SLSCWrongLocalArgAddr Text SLPos

instance Show SLSCError where
  show err = T.unpack $ slsceMessage err <> " at " <> prettyPrintSLPos (slscegetPos err)

slscegetPos :: SLSCError -> SLPos
slscegetPos e = case e of
  SLSCTypeError _ pos -> pos
  SLSCNoSuchVariable _ pos -> pos
  SLSCNoSuchArgument _ pos -> pos
  SLSCNoSuchFunction _ pos -> pos
  SLSCWrongVariableType _ _ _ pos -> pos
  SLSCWrongArgumentType _ _ _ pos -> pos
  SLSCWrongFunctionType _ _ _ pos -> pos
  SLSCWrongReturnType _ _ _ pos -> pos
  SLSCFunctionNameMismatch _ _ pos -> pos
  SLSCWrongArgTypeToPrim _ _ pos -> pos
  SLSCWrongUnionType _ _ _ pos -> pos
  SLSCUnionMustBeUnion _ pos -> pos
  SLSCRefTypeMismatch _ _ _ pos -> pos
  SLSCArgumentMismatch _ _ _ pos -> pos
  SLSCWrongTypeForCond _ pos -> pos
  SLSCWrongLocalArgAddr _ pos -> pos

slsceMessage :: SLSCError -> Text
slsceMessage err = case err of
  SLSCTypeError e _ -> prettyPrintSLTypeError e
  SLSCNoSuchVariable name _ -> "No such variable: " <> name
  SLSCNoSuchArgument name _ -> "No such argument: " <> name
  SLSCNoSuchFunction name _ -> "No such function: " <> name
  SLSCWrongVariableType name t1 t2 _ -> "Wrong variable type: " <> name <> " expected " <> prettyPrintSLType t1 <> " but got " <> prettyPrintSLType t2
  SLSCWrongArgumentType name t1 t2 _ -> "Wrong argument type: " <> name <> " expected " <> prettyPrintSLType t1 <> " but got " <> prettyPrintSLType t2
  SLSCWrongFunctionType name t1 t2 _ -> "Wrong function type: " <> name <> " expected " <> prettyPrintSLType t1 <> " but got " <> prettyPrintSLType t2
  SLSCWrongReturnType name t1 t2 _ -> "Wrong return type: " <> name <> " expected " <> prettyPrintSLType t1 <> " but got " <> prettyPrintSLType t2
  SLSCFunctionNameMismatch name1 name2 _ -> "Function name mismatch: " <> prettyPrintSLFuncName name1 <> " vs " <> prettyPrintSLFuncName name2
  SLSCWrongArgTypeToPrim name t _ -> "Wrong argument type to prim: " <> name <> " expected " <> prettyPrintSLType SLTInt <> " but got " <> prettyPrintSLType t
  SLSCWrongUnionType name t1 t2 _ -> "Wrong union type: " <> name <> " expected " <> prettyPrintSLType t1 <> " but got " <> prettyPrintSLType t2
  SLSCUnionMustBeUnion _ _ -> "Union must be union"
  SLSCRefTypeMismatch name t1 t2 _ -> "Wrong reference type: " <> name <> " expected " <> prettyPrintSLType t1 <> " but got " <> prettyPrintSLType t2
  SLSCArgumentMismatch name t1 t2 _ -> "Wrong argument type: " <> name <> " expected " <> prettyPrintSLType t1 <> " but got " <> prettyPrintSLType t2
  SLSCWrongTypeForCond t _ -> "Wrong type for condition: expected " <> prettyPrintSLType SLTInt <> " but got " <> prettyPrintSLType t
  SLSCWrongLocalArgAddr name _ -> "Wrong local argument address: " <> name

prettyPrintSLSCError :: SLSCError -> Text
prettyPrintSLSCError err =
  let pos = slscegetPos err
      msg = slsceMessage err
  in prettyPrintSLPos pos <> ": " <> msg

data SLSCState = SLSCState {
      slscsVars  :: Map Int SLType
    , slscsArgs  :: Map Int SLType
    , slscsFuncs :: Map SLFuncName SLFuncSignature
    , slscsRetType :: SLType
    , slscsPos :: SLPos
    , stackCount :: Int
  } deriving (Show)


type MonadSLSC = ExceptT SLSCError (State SLSCState)

slscCheck :: SLProgram -> Either SLSCError ()
slscCheck decls = 
  let funcdict = M.fromList $ (\(SLFuncBlock sig _) -> (slfsName sig, sig)) <$> M.elems decls
        

  in  forM_ (M.assocs decls) (\(fname, fblock) -> do
                let SLFuncBlock sig block = fblock
                let SLFuncSignature name args rettype = sig
                when (name /= fname)
                  $ throwError $ SLSCFunctionNameMismatch name fname (SLPos fname [])
                
                let argdict =
                      M.fromList $ snd $ F.foldl (\(argpos, argdata) t -> (argpos + sltSizeOf t, (argpos, t): argdata)) (0, []) args

                let initstate = SLSCState {
                        slscsVars = M.empty
                      , slscsArgs = argdict
                      , slscsFuncs = funcdict
                      , slscsRetType = rettype
                      , slscsPos = SLPos fname []
                      , stackCount = 0
                      }

                _ <- (`evalState` initstate) $ runExceptT $ checkBlock block

                pure ()            
            )




throwSLSCError :: (SLPos -> SLSCError) -> MonadSLSC x
throwSLSCError f = do
  pos <- gets slscsPos
  throwError $ f pos

inPos :: SLLocalPos -> MonadSLSC x -> MonadSLSC x
inPos newpos v = do
    oldpos <- gets slscsPos
    S.modify (\s -> s {slscsPos = pushPos newpos oldpos})
    x <- v
    S.modify (\s -> s {slscsPos = oldpos})
    pure x

outPos :: MonadSLSC x -> MonadSLSC x
outPos v = do
    oldpos <- gets slscsPos
    S.modify (\s -> s {slscsPos = popPos oldpos})
    x <- v
    S.modify (\s -> s {slscsPos = oldpos})
    pure x

scope :: MonadSLSC x -> MonadSLSC x
scope v = do
  oldstate <- get
  x <- v
  S.put oldstate
  pure x

checkBlock :: SLBlock -> MonadSLSC ()
checkBlock block = case block of
  SLBCase cases other -> do
    forM_ (L.zip (V.toList cases) [0..]) $ \((cond, body), i) -> do
      t <- inPos (SLLPCaseCond i) $ checkExpr cond
      when (t /= SLTInt) $ throwSLSCError $ SLSCWrongTypeForCond t
      _ <- inPos (SLLPCaseBody i) $ scope $ checkBlock body
      pure ()
    _ <- inPos SLLPCaseElseBody $ checkBlock other
    pure ()

  SLBWhile cond body -> do
    t <- inPos SLLPWhileCond $ checkExpr cond
    when (t /= SLTInt) $ throwSLSCError $ SLSCWrongTypeForCond t
    _ <- inPos SLLPWhileBody $ scope $ checkBlock body
    pure ()
  
  SLBMulti blocks -> 
    scope $ forM_ (L.zip (V.toList blocks) [0..]) $ \(body, i) -> do
      _ <- inPos (SLLPMulti i) $ checkBlock body
      pure ()
  
  SLBSingle stmt -> do
    _ <- checkStmt stmt
    pure ()

checkStmt :: SLStatement -> MonadSLSC ()
checkStmt stmt = case stmt of
  SLSReturn exp -> do
    t <- checkExpr exp
    rettype <- gets slscsRetType
    when (t /= rettype) $ throwSLSCError $ SLSCWrongReturnType (prettyPrintSLStatement stmt) t rettype

  SLSTailCallReturn call -> do
    t <- checkCall call
    rettype <- gets slscsRetType
    when (t /= rettype) $ throwSLSCError $ SLSCWrongReturnType (prettyPrintSLStatement stmt) t rettype
  
  SLSSubst ref exp -> do
    t <- checkExpr exp
    t' <- checkRef ref
    when (t /= t') $ throwSLSCError $ SLSCRefTypeMismatch (prettyPrintSLStatement stmt) t t'
  
  SLSInitVar i exp -> do
    t <- checkExpr exp
    vars <- gets slscsVars
    scnt <- gets stackCount
    when (i /= scnt) $ throwSLSCError $ SLSCWrongLocalArgAddr (prettyPrintSLStatement stmt)
    S.modify (\s -> s {slscsVars = M.insert i t vars, stackCount = scnt + sltSizeOf t})

checkExpr :: SLExp -> MonadSLSC SLType
checkExpr exp = inPos (SLLPExpr exp) $ case exp of
  SLELocal t i -> do
    vars <- gets slscsVars
    case M.lookup i vars of
      Nothing -> throwSLSCError $ SLSCNoSuchVariable (prettyPrintSLExp exp)
      Just t'
        | t == t' -> pure t
        | otherwise -> throwSLSCError $ SLSCWrongVariableType (prettyPrintSLExp exp) t t'
  
  SLEArg t i -> do
    args <- gets slscsArgs
    case M.lookup i args of
      Nothing -> throwSLSCError $ SLSCNoSuchArgument (prettyPrintSLExp exp)
      Just t'
        | t == t' -> pure t
        | otherwise -> throwSLSCError $ SLSCWrongArgumentType (prettyPrintSLExp exp) t t'
  
  SLEPushCall call -> checkCall call

  SLEConst _ -> pure SLTInt
  SLEPtr ref -> SLTPtr <$> checkRef ref
  SLEFuncPtr sig -> do
    funcs <- gets slscsFuncs
    case M.lookup (slfsName sig) funcs of
      Nothing -> throwSLSCError $ SLSCNoSuchFunction (prettyPrintSLExp exp)
      Just sig'
        | sig == sig' -> pure (SLTFuncPtr (slfsArgs sig) (slfsRet sig))
        | otherwise -> throwSLSCError $ SLSCWrongFunctionType (prettyPrintSLExp exp) (SLTFuncPtr (slfsArgs sig) (slfsRet sig)) (SLTFuncPtr (slfsArgs sig') (slfsRet sig'))
  
  SLEPrim1 _ e1    -> do
    t1 <- checkExpr e1
    when (t1 /= SLTInt) $ throwSLSCError $ SLSCWrongArgTypeToPrim (prettyPrintSLExp exp) t1
    pure SLTInt

  SLEPrim2 _ e1 e2 ->  do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    when (t1 /= SLTInt) $ throwSLSCError $ SLSCWrongArgTypeToPrim (prettyPrintSLExp exp) t1
    when (t2 /= SLTInt) $ throwSLSCError $ SLSCWrongArgTypeToPrim (prettyPrintSLExp exp) t2
    pure SLTInt
  
  SLEStructNil     -> pure $ SLTStruct []

  SLEStructCons e1 e2 -> do
    t1 <- checkExpr e1
    t2 <- outPos $ checkExpr e2
    case t2 of
      SLTStruct ts -> pure $ SLTStruct (t1:ts)
      _ -> throwSLSCError $ SLSCTypeError $ SLTEStructCons0MustBeStruct exp
  
  SLEUnion t e     -> do
    t' <- checkExpr e
    case t of
      SLTUnion ts
        | t' `L.elem` ts -> pure t
        | otherwise -> throwSLSCError $ SLSCWrongUnionType (prettyPrintSLExp exp) t t'
      _ -> throwSLSCError $ SLSCUnionMustBeUnion exp
  
  SLEDeRef e       -> do
    t <- checkExpr e
    case t of
      SLTPtr t' -> pure t'
      _ -> throwSLSCError $ SLSCTypeError $ SLTEDeRef0MustBePtr exp
  
  SLEPtrShift e1 _ -> do
    t <- checkExpr e1
    case t of
      SLTPtr t' -> pure $ SLTPtr t'
      _ -> throwSLSCError $ SLSCTypeError $ SLTEPtrShift0MustBePtr exp
    
  SLEStructGet e i -> do
    t <- checkExpr e
    case t of
      SLTStruct ts
        | 0 <= i && i < L.length ts -> pure (ts !! i)
        | otherwise                -> throwSLSCError $ SLSCTypeError $ SLTEStructGet1OutOfRange e i
      _ -> throwSLSCError $ SLSCTypeError $ SLTEStructGet0MustBeStruct exp

  SLECast t e      -> do
    t' <- checkExpr e
    when (sltSizeOf t /= sltSizeOf t') $ throwSLSCError $ SLSCTypeError $ SLTECast1SizeMismatch t t'
    pure t

checkRef :: SLRef -> MonadSLSC SLType
checkRef ref = case ref of
  SLRefPtr t e -> do
    t' <- checkExpr e
    when (t /= t') $ throwSLSCError $ SLSCRefTypeMismatch (prettyPrintSLRef ref) t t'
    pure t

  SLRefLocal t i -> do
    vars <- gets slscsVars
    case M.lookup i vars of
      Nothing -> throwSLSCError $ SLSCNoSuchVariable (prettyPrintSLRef ref)
      Just t' 
        | t == t' -> pure t
        | otherwise -> throwSLSCError $ SLSCWrongVariableType (prettyPrintSLRef ref) t t'

checkCall :: SLCall -> MonadSLSC SLType
checkCall call = case call of
  SLSolidFuncCall sig args -> do
    funcs <- gets slscsFuncs
    case M.lookup (slfsName sig) funcs of
      Nothing -> throwSLSCError $ SLSCNoSuchFunction (prettyPrintSLCall call)
      Just sig'
        | sig /= sig' -> throwSLSCError $ SLSCWrongFunctionType (prettyPrintSLCall call) (SLTFuncPtr (slfsArgs sig) (slfsRet sig)) (SLTFuncPtr (slfsArgs sig') (slfsRet sig'))
        | otherwise -> do
            t <- checkExpr args
            when (t /= SLTStruct (slfsArgs sig)) $ throwSLSCError $ SLSCArgumentMismatch (prettyPrintSLCall call) t (SLTStruct (slfsArgs sig))
            pure (slfsRet sig)
  
  SLFuncRefCall fref args -> do
    p <- checkRef fref
    a <- checkExpr args

    case p of
      SLTFuncPtr argtypes rettype
        | a /= SLTStruct argtypes -> throwSLSCError $ SLSCArgumentMismatch (prettyPrintSLCall call) a (SLTStruct argtypes)
        | otherwise -> pure rettype
      _ -> throwSLSCError $ SLSCTypeError $ SLTEFuncRefCall0NotFuncPtr call p
  
  SLClosureCall exp -> do
    t <- checkExpr exp
    case t of
      SLTStruct (SLTFuncPtr argtypes rettype : argtypes') 
        | argtypes == argtypes' -> pure rettype
        | otherwise -> throwSLSCError $ SLSCTypeError $ SLTEClosureCall0TypeMismatch call (SLTStruct argtypes) (SLTStruct argtypes')
      _ -> throwSLSCError $ SLSCTypeError $ SLTEClosureCall0MustBeStruct call
