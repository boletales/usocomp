{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SimpleLang.FromFuncLang where


import FuncLang.Def
import SimpleLang.Def
import SimpleLang.Tools.Manual
import Data.Text as T
import Control.Category
import Prelude hiding ((.), id)
import Data.Map.Strict as M
import Data.Foldable as F
import qualified Data.List as L
import Control.Monad
import Control.Monad.State
import Data.Function ((&))
import Control.Monad.Except
import Control.Applicative
import Data.Bifunctor
import Data.Maybe
import Debug.Trace
import SimpleLang.FromFuncLang.Lib
import qualified Data.Vector as V

tshow :: Show a => a -> Text
tshow = T.pack . show

type family FLTypeToSLType (t :: FLType) :: SLType where
    FLTypeToSLType 'FLTInt  = 'SLTInt
    FLTypeToSLType 'FLTBool = 'SLTInt
    FLTypeToSLType ('FLTLambda t1 t2) = 'SLTStruct ('SLTFuncPtr (FLTypeToSLType t1 ': '[]) (FLTypeToSLType t2) ': (FLTypeToSLType t1 ': '[]))
    FLTypeToSLType ('FLTTuple ts)     = 'SLTStruct (MapFLTypeToSLType ts)

type family MapFLTypeToSLType (ts :: [FLType]) :: [SLType] where
    MapFLTypeToSLType '[] = '[]
    MapFLTypeToSLType (t:ts) = FLTypeToSLType t ': MapFLTypeToSLType ts


-- 中間表現として、FLExpから式中のLambdaを抜いたFLCExpと、トップレベルの関数定義を表すFLCFunDeclを使うことにする（変数捕獲がつらいので）
data FLCExp where
    FLCEValI       :: Int -> FLCExp
    FLCEValB       :: Bool -> FLCExp
    FLCEVar        :: FLCUniqueIdentifier -> FLType -> FLCExp
    FLCEApp        :: FLCExp -> FLCExp -> FLCExp
    --FLCELet        :: [FLCVarDecl] -> FLCExp -> FLCExp
    --FLCEUncapture  :: FLCUniqueIdentifier -> [(FLCUniqueIdentifier, FLType)] -> FLCExp
    deriving Eq
instance Show FLCExp where
    show = T.unpack . prettyPrintFLCExp

data FLCVarDecl = FLCVarDecl FLCUniqueIdentifier FLType FLCExp deriving Eq

data FLCFuncDecl = FLCFuncDecl {
      flcfdName :: FLCUniqueIdentifier
    , flcfdArgs :: [(FLCUniqueIdentifier, FLType)]
    , flcfdRet  :: FLType
    , flcfdBody :: FLCExp
  }

newtype FLCProgram = FLCProgram (M.Map FLCUniqueIdentifier FLCFuncDecl)
instance Show FLCProgram where
    show = T.unpack . prettyPrintFLCProgram

flceTypeOf :: FLCExp -> Either Text FLType
flceTypeOf expr =
  case expr of
    FLCEValI _ -> pure FLTInt
    FLCEValB _ -> pure FLTBool
    FLCEVar (FLCUniqueIdentifier _ _) t -> pure t
    FLCEApp f x -> do
      t1 <- flceTypeOf f
      t2 <- flceTypeOf x
      case t1 of
        FLTLambda t1' t2' -> if t1' == t2 then pure t2' else throwError "Error! (type check of FLang): missmatch type of argument"
        _ -> throwError "Error! (type check of FLang): not a function"
    --FLCELet _ body -> flceTypeOf body

{-
コンパイル手順：
0. 識別子を一意なものに変換する
1. すべての変数内関数定義をトップレベルに持ち上げる
2. 不完全な適用はクロージャ（SLExp）に、完全な適用はSLEPushCallなりslmTailCallReturnなりにする

もしかして rename と lift は統合すべき？
-}

-- 識別子初出位置へのパス
data FLCPath =
      FLCPathTop
    | FLCPathTopLiftedFrom FLCPath
    | FLCPathInTopDecl Text
    | FLCPathInLiftedLambda FLCPath Text
    | FLCPathInLetDecl Text FLCPath
    | FLCPathInLetBody      FLCPath
    | FLCPathInAppR         FLCPath
    | FLCPathInAppL         FLCPath
    | FLCPathInLambda       FLCPath
    deriving (Eq, Ord)

getRootName :: FLCPath -> Maybe Text
getRootName path =
  case path of
    FLCPathTop -> Nothing
    FLCPathTopLiftedFrom p    -> getRootName p
    FLCPathInTopDecl t        -> Just t
    FLCPathInLiftedLambda p _ -> getRootName p
    FLCPathInLetDecl _ p      -> getRootName p
    FLCPathInLetBody p        -> getRootName p
    FLCPathInAppR p           -> getRootName p
    FLCPathInAppL p           -> getRootName p
    FLCPathInLambda p         -> getRootName p

instance Show FLCPath where
    show path =
        case path of
            FLCPathTop                -> "top"
            FLCPathInTopDecl t        -> T.unpack t
            FLCPathTopLiftedFrom p    -> show p <> "!"
            FLCPathInLiftedLambda p t -> show p <> "!" <> T.unpack t
            FLCPathInLetDecl t p      -> show p <> ".$" <> T.unpack t
            FLCPathInLetBody   p      -> show p <> ".b"
            FLCPathInAppR      p      -> show p <> ".R"
            FLCPathInAppL      p      -> show p <> ".L"
            FLCPathInLambda    p      -> show p <> ".λ"

flcPathRoots :: FLCPath -> [FLCPath]
flcPathRoots path =
    path :
    case path of
        FLCPathTop                -> []
        FLCPathInTopDecl _        -> []
        FLCPathTopLiftedFrom _    -> []
        FLCPathInLiftedLambda _ _ -> []
        FLCPathInLetDecl _ p -> flcPathRoots p
        FLCPathInLetBody   p -> flcPathRoots p
        FLCPathInAppR      p -> flcPathRoots p
        FLCPathInAppL      p -> flcPathRoots p
        FLCPathInLambda    p -> flcPathRoots p

-- 完全な包含関係にあるときのみTrue
flcPathDeeperEq :: FLCPath -> FLCPath -> Bool
flcPathDeeperEq p1 p2 =
    let r1 = flcPathRoots p1
        -- r2 = flcPathRoots p2
    in  L.elem p2 r1

data FLCUniqueIdentifier = FLCUniqueIdentifier {
      flcuiPath :: FLCPath
    , flcuiName :: Text
    } deriving (Eq)

instance Ord FLCUniqueIdentifier where
  compare (FLCUniqueIdentifier p1 n1) (FLCUniqueIdentifier p2 n2) =
    let root1 = fromMaybe n1 (getRootName p1)
        root2 = fromMaybe n2 (getRootName p2)
    in  if root1 == root2
        then compare n1 n2
        else compare root1 root2


instance Show FLCUniqueIdentifier where
    show (FLCUniqueIdentifier path name) =
        case path of
            FLCPathTopLiftedFrom p -> show p <> "!" <> T.unpack name
            _ -> T.unpack name

isTopLevelLambda :: FLCPath -> Bool
isTopLevelLambda path =
  case path of
      FLCPathInLambda p -> isTopLevelLambda p
      FLCPathInTopDecl _ -> True
      FLCPathInLiftedLambda _ _ -> True
      _ -> False

data FLExpIgnoreType where
  FLExpIgnoreType :: (FLExp Text x) -> FLExpIgnoreType

ignoringType :: (forall t. FLExp Text t -> a) -> FLExpIgnoreType -> a
ignoringType f (FLExpIgnoreType e) = f e

-- 0+1
flcRenameAndLift :: FLProgram Text -> Either Text FLCProgram
flcRenameAndLift program =
    let globalDict :: M.Map Text FLCUniqueIdentifier
        globalDict = (\(FLVarDecl (FLVar n) _) -> FLCUniqueIdentifier FLCPathTop n) <$> flpTopLevelVars program


        searchExternalVar :: FLExp Text t -> M.Map Text FLCUniqueIdentifier -> FLCPath -> Either Text [(FLCUniqueIdentifier, FLType)]
        searchExternalVar expr dict pathorig =
          let search1 :: FLExp Text t -> M.Map Text FLCUniqueIdentifier -> FLCPath -> Either Text [(FLCUniqueIdentifier, FLType)]
              search1 expr' dict' path' =
                  case expr' of
                      FLEValI _           -> pure []
                      FLEValB _           -> pure []
                      FLEVar (FLVar name) ->
                          case M.lookup name dict' of
                              Just newName ->
                                if flcPathDeeperEq (flcuiPath newName) pathorig
                                then pure []
                                else pure [(newName, flTypeOf expr')]
                              Nothing      -> throwError $ "Undefined variable: " <> name <> " at " <> T.pack (show path')
                      FLELambda (FLVar name) body ->
                          let newpath = FLCPathInLambda path'
                              newdict = M.insert name (FLCUniqueIdentifier newpath name) dict'
                          in  search1 body   newdict newpath
                      FLEApp f x ->
                          let newF = search1 f dict' (FLCPathInAppL path')
                              newX = search1 x dict' (FLCPathInAppR path')
                          in liftA2 (<>) newF newX
                      FLELet vs body ->
                          let newDict = F.foldl' (\d (FLVarDecl (FLVar n) _) -> M.insert n (FLCUniqueIdentifier (FLCPathInLetBody path') n) d) dict' vs
                              newDecl = mapM (\(FLVarDecl (FLVar n) e) -> search1 e newDict (FLCPathInLetDecl n path')) vs
                              newBody = search1 body newDict (FLCPathInLetBody path')
                          in liftA2 (<>) (fmap join newDecl) newBody
            in search1 expr dict pathorig


        --captureExternalVar :: [(FLCUniqueIdentifier, FLType)] -> FLCUniqueIdentifier -> FLCExp -> FLCFuncDecl
        --captureExternalVar vars funcname = FLCFuncDecl funcname vars

        appCapturedVar :: [(FLCUniqueIdentifier, FLType)] -> FLCExp -> FLCExp
        appCapturedVar vars expr =
          case vars of
              []   -> expr
              (v, t):vs -> appCapturedVar vs (FLCEApp expr (FLCEVar v t))

        liftLambda e dict path = do
          let newname = FLCUniqueIdentifier (FLCPathTopLiftedFrom path) "anonymous"
          let newpath = FLCPathInLambda path

          -- Lambdaをまとめる
          (args, core) <-
                let go :: FLExp Text t -> [(Text, FLType)] -> Either Text ([(Text, FLType)], FLExpIgnoreType)
                    go expr' as =
                      case expr' of
                        FLELambda (FLVar arg') body' -> do
                          argtype <-
                            case flTypeOf expr' of
                              FLTLambda t1 _ -> pure t1
                              _ -> throwError "Error! (compiling FLang, squashing lambda): impossible!"
                          go body' ((arg', argtype):as)
                        _ -> pure (L.reverse as, FLExpIgnoreType expr')
                in  lift $ go e []

          let argdict = M.fromList ((\(a, _) -> (a, FLCUniqueIdentifier newpath a)) <$> args)
          extvars <- lift $ ignoringType (\c -> searchExternalVar c (dict <> argdict) newpath) core

          let innerdict =
                    globalDict
                <> M.singleton (flcuiName newname) newname
                <> M.fromList ((\(v,_) -> (flcuiName v, v)) <$> extvars)
                <> argdict

          lambdabody <- ignoringType (\c -> renameLift1 c innerdict (FLCPathInLiftedLambda path (flcuiName newname))) core
          bodytype <- lift $ flceTypeOf lambdabody
          let newlambda = FLCFuncDecl newname (extvars <> (first (FLCUniqueIdentifier newpath) <$> args)) bodytype lambdabody
          modify (newlambda :)
          pure $ appCapturedVar extvars (FLCEVar newname (flTypeOf e))

        liftLet vs body dict path = do
          let newbasedict = globalDict <> M.fromList ((\(FLVarDecl (FLVar n) _) -> (n, FLCUniqueIdentifier (FLCPathTopLiftedFrom path) ("let_" <> n))) <$> vs)

          forM_ vs (\(FLVarDecl (FLVar n) core) -> do
              let newname = FLCUniqueIdentifier (FLCPathTopLiftedFrom path) ("let_" <> n)
              let newpath = FLCPathInLetDecl n path

              extvars <- lift $ (\c -> searchExternalVar c newbasedict newpath) core

              let innerdict =
                      globalDict
                    <> M.fromList ((\(v,_) -> (flcuiName v, v)) <$> extvars)

              lambdabody <- (\c -> renameLift1 c innerdict (FLCPathInLiftedLambda path (flcuiName newname))) core
              bodytype <- lift $ flceTypeOf lambdabody
              let newlambda = FLCFuncDecl newname extvars bodytype lambdabody
              modify (newlambda :)
            )
          --liftLambda body newbasedict (FLCPathInLetBody path)
          let newname = FLCUniqueIdentifier (FLCPathTopLiftedFrom path) "letbody"
          let newpath = FLCPathInLambda path

          -- Lambdaをまとめる
          (args, core) <-
                let go :: FLExp Text t -> [(Text, FLType)] -> Either Text ([(Text, FLType)], FLExpIgnoreType)
                    go expr' as =
                      case expr' of
                        FLELambda (FLVar arg') body' -> do
                          argtype <-
                            case flTypeOf expr' of
                              FLTLambda t1 _ -> pure t1
                              _ -> throwError "Error! (compiling FLang, squashing lambda): impossible!"
                          go body' ((arg', argtype):as)
                        _ -> pure (L.reverse as, FLExpIgnoreType expr')
                in  lift $ go body []

          let argdict = M.fromList ((\(a, _) -> (a, FLCUniqueIdentifier newpath a)) <$> args)
          extvars <- lift $ ignoringType (\c -> searchExternalVar c (dict <> argdict) newpath) core

          let innerdict =
                    globalDict
                <> M.singleton (flcuiName newname) newname
                <> M.fromList ((\(v,_) -> (flcuiName v, v)) <$> extvars)
                <> argdict

          lambdabody <- ignoringType (\c -> renameLift1 c innerdict (FLCPathInLiftedLambda path (flcuiName newname))) core
          bodytype <- lift $ flceTypeOf lambdabody
          let newlambda = FLCFuncDecl newname (extvars <> (first (FLCUniqueIdentifier newpath) <$> args)) bodytype lambdabody
          modify (newlambda :)
          pure $ appCapturedVar extvars (FLCEVar newname (flTypeOf body))

        renameLift1 :: FLExp Text t -> M.Map Text FLCUniqueIdentifier -> FLCPath -> StateT [FLCFuncDecl] (Either Text) FLCExp
        renameLift1 e dict path =
            case e of
                FLEValI i           -> pure $ FLCEValI i
                FLEValB b           -> pure $ FLCEValB b
                FLEVar (FLVar name) ->
                    case M.lookup name dict of
                        Just newName ->  pure $ FLCEVar newName (flTypeOf e)
                        Nothing -> throwError $ "Undefined variable: " <> name <> " at " <> T.pack (show path)
                FLELambda _ _ -> liftLambda e dict path

                FLEApp f x ->
                    let newF = renameLift1 f dict (FLCPathInAppL path)
                        newX = renameLift1 x dict (FLCPathInAppR path)
                    in FLCEApp <$> newF <*> newX

                FLELet vs body -> liftLet vs body dict path

        result = flpTopLevelVars program &
              (
                M.toList >>>
                fmap
                  (\(_, FLVarDecl (FLVar t) decl) ->
                    (\(body, decls) ->
                        (\r -> FLCFuncDecl (FLCUniqueIdentifier FLCPathTop t) [] r body : decls) <$> flceTypeOf body
                        ) =<< runStateT (renameLift1 decl globalDict (FLCPathInTopDecl t)) []
                   ) >>>
                sequence >>>
                fmap (
                  join >>>
                  fmap (\decl@(FLCFuncDecl t _ _ _) -> (t, decl)) >>>
                  M.fromList >>>
                  FLCProgram >>>
                  id
                ) >>>
                id
              )
    in  result

-- misc


prettyPrintFLCVarDecl :: FLCVarDecl -> Text
prettyPrintFLCVarDecl (FLCVarDecl v _ e) = T.pack (show v) <> " = " <> prettyPrintFLCExp e


prettyPrintFLCExp :: FLCExp -> Text
prettyPrintFLCExp e =
    case e of
      (FLCEValI i)      -> tshow i
      (FLCEValB b)      -> tshow b
      (FLCEVar v _)     -> tshow v -- <> " :: " <> tshow t
      (FLCEApp e1 e2)   -> "(" <>   prettyPrintFLCExp e1 <> " " <> prettyPrintFLCExp e2 <> ")"
      --(FLCELet decls b) -> "(let " <> T.intercalate "; " (prettyPrintFLCVarDecl <$> decls) <> " in " <> prettyPrintFLCExp b <> ")"

prettyPrintFLCFuncDecl :: FLCFuncDecl -> Text
prettyPrintFLCFuncDecl (FLCFuncDecl name args ret body) =
    tshow name <> "(" <> T.intercalate ", " ((\(n, t) -> tshow n <> " :: " <> tshow t) <$> args) <> ") -> " <> tshow ret <> " = " <> prettyPrintFLCExp body

prettyPrintFLCProgram :: FLCProgram -> Text
prettyPrintFLCProgram (FLCProgram decls) = T.intercalate "\n" (prettyPrintFLCFuncDecl <$> M.elems decls)

data FLCIExp =
        FLCIEFLCE FLCExp
      | FLCIEcls  [FLType] FLType FLCUniqueIdentifier [FLCIExp]
  deriving (Show, Eq)

flcieTypeOf :: FLCIExp -> Either Text FLType
flcieTypeOf expr =
  case expr of
    FLCIEFLCE e -> flceTypeOf e
    FLCIEcls targs tresult (FLCUniqueIdentifier _ _) _ -> pure (L.foldr FLTLambda tresult targs)

flcieSLTypeOf :: FLCIExp -> Either Text SLType
flcieSLTypeOf expr =
  case expr of
    FLCIEFLCE e -> flTypeToSLType <$> flceTypeOf e
    FLCIEcls targs tresult (FLCUniqueIdentifier _ _) _ -> 
      let targs' = flTypeToSLType <$> targs
          tresult' = flTypeToSLType tresult
      in  pure (SLTFuncPtr targs' tresult')

flTypeToSLType :: FLType -> SLType
flTypeToSLType t =
  case t of
    FLTInt  -> SLTInt
    FLTBool -> SLTInt
    FLTLambda t1 t2 -> SLTFuncPtr [flTypeToSLType t1] (flTypeToSLType t2)
    FLTTuple ts -> SLTStruct (flTypeToSLType <$> ts)

interpretFLC :: FLCProgram -> Either Text FLCIExp
interpretFLC (FLCProgram decls) =
  let initialdict = M.mapWithKey (\k f -> FLCIEcls (snd <$> flcfdArgs f) (flcfdRet f) k []) decls

      evalUntilStop dict expr =
        case eval dict expr of
          Left _            -> pure expr
          Right expr'
            | expr == expr' -> pure expr
            | otherwise     -> evalUntilStop dict expr'

      eval dict expr =
        case expr of
          FLCIEFLCE flce ->
            case flce of
              FLCEValI _  -> pure expr
              FLCEValB _  -> pure expr
              FLCEVar v _ -> maybe (Left ("No variable named " <> tshow v)) pure (M.lookup v dict)
              FLCEApp f x -> do
                f' <- evalUntilStop dict (FLCIEFLCE f)
                x' <- evalUntilStop dict (FLCIEFLCE x)
                case f' of
                  FLCIEcls ta tr funid args -> do
                    case L.uncons ta of
                      Just (t, ta')
                        | Right t == flcieTypeOf x' -> evalUntilStop dict (FLCIEcls ta' tr funid (args <> [x']))
                        | otherwise -> Left "Type missmatch"
                      Nothing -> Left "Too many arguments"
                  _ -> Left  "Not a function"

              {-
              FLCELet letdecls body -> do
                newdict <- (\dgen -> dict <> M.fromList dgen) <$> forM letdecls (\(FLCVarDecl v _ e) -> do
                    e' <- evalUntilStop dict (FLCIEFLCE e)
                    pure (v, e')
                  )
                evalUntilStop newdict (FLCIEFLCE body)
              -}
          FLCIEcls targs _ funid args ->
            case targs of
              _:_ -> pure expr
              [] ->
                case M.lookup funid decls of
                  Nothing -> Left $ "No function named" <> tshow funid
                  Just f -> do
                    let newdict = initialdict <> M.fromList (L.zip (fst <$> flcfdArgs f) args)
                    evalUntilStop newdict (FLCIEFLCE (flcfdBody f))
  in case M.lookup (FLCUniqueIdentifier FLCPathTop "main") initialdict of
      Just cls -> evalUntilStop initialdict cls
      Nothing -> Left "No main function"

-- 2

-- todo: クロージャのサイズは実行時までわからないので、ヒープ上に確保する必要がある？
--       これに際して、アロケーションとGCを行うライブラリを実装する必要がある！
flcCompileProgram :: FLCProgram -> Either Text SLProgram
flcCompileProgram program =
  let toSLName :: FLCUniqueIdentifier -> SLFuncName
      toSLName (FLCUniqueIdentifier p t) = SLUserFunc "generated" (tshow p <> "_" <> t)

      toSLSignature :: FLCUniqueIdentifier -> [(FLCUniqueIdentifier, FLType)] -> FLType -> SLFuncSignature
      toSLSignature name args ret = SLFuncSignature (toSLName name) ((snd >>> flTypeToSLType) <$> args) (flTypeToSLType ret)

      funcdict :: M.Map FLCUniqueIdentifier SLExp
      funcdict =
        --M.fromList ((\(name, sig) -> (FLCUniqueIdentifier FLCPathTop name, SLEFuncPtr sig `SLEStructCons` SLEStructNil)) <$> libFLCSigMap) <>
        ((\(FLCFuncDecl name args ret _) -> SLEFuncPtr (toSLSignature name args ret) `SLEStructCons` SLEStructNil) <$> (\(FLCProgram decls) -> decls) program)

      flciedict :: M.Map FLCUniqueIdentifier FLCIExp
      flciedict =
        --M.fromList ((\(name, (args, ret)) -> (FLCUniqueIdentifier FLCPathTop name, FLCIEcls args ret (FLCUniqueIdentifier FLCPathTop name) [])) <$> libFLCClsMap) <>
        ((\(FLCFuncDecl name args ret body) -> FLCIEcls (snd <$> args) ret name []) <$> (\(FLCProgram decls) -> decls) program)

      funcTextName :: FLCUniqueIdentifier -> Text
      funcTextName (FLCUniqueIdentifier _ t) = t

      remainingArgs :: SLExp -> Either Text [SLType]
      remainingArgs expr = do
        t <- first prettyPrintSLTypeError $ sleTypeOf expr
        case t of
          SLTStruct (SLTFuncPtr args _ : given) ->
            foldM (\acc g -> do
                case acc of
                  [] -> throwError $ "Error! (compilation of FLang): too many arguments! (expr " <> tshow expr <> " applies "  <> tshow given <> " to a function requiring " <> tshow args <> ")"
                  (a:as) -> if g == a then pure as else throwError "Error! (compilation of FLang): type missmatch!"
              ) args given
          _ -> throwError "Error! (compilation of FLang): this is not closure!"

      tryEval :: SLExp -> Either Text SLExp
      tryEval expr = do
        t <- first prettyPrintSLTypeError $ sleTypeOf expr
        case t of
          SLTStruct (SLTFuncPtr args ret : given) -> do
            rema <- remainingArgs expr
            case rema of
              [] -> tryEval ((SLClosureCall >>> SLEPushCall) expr)
              _  -> pure expr
          _ -> pure expr

      fptrToClosure :: SLExp -> Either Text SLExp
      fptrToClosure expr = do
        t <- first prettyPrintSLTypeError $ sleTypeOf expr
        case t of
          SLTFuncPtr _ _ -> pure (expr `SLEStructCons` SLEStructNil)
          _ -> pure expr

      flceToFLCIE :: FLCExp -> Either Text FLCIExp
      flceToFLCIE expr =
        case expr of
          FLCEApp f x -> do
            f' <- flceToFLCIE f
            x' <- flceToFLCIE x
            case f' of
              FLCIEcls targs tresult funid args -> do
                case L.uncons targs of
                  Just (t, ta')
                    | Right t == flcieTypeOf x' -> pure $ FLCIEcls ta' tresult funid (args <> [x'])
                    | otherwise -> Left "Type missmatch"
                  Nothing -> Left "Error! (compilation of FLang): too many arguments!"
              _ -> Left "Error! (compilation of FLang): not a function"
          _ -> pure (FLCIEFLCE expr)

      flcCompileExpr1 :: M.Map FLCUniqueIdentifier SLExp -> FLCIExp -> Either Text SLExp
      flcCompileExpr1 dict expr =
        case expr of
          FLCIEFLCE flce ->
            case flce of
              FLCEValI i  -> pure (SLEConst (SLVal i))
              FLCEValB b  -> pure (SLEConst (SLVal (if b then 1 else 0)))
              FLCEVar v _ -> maybe (Left ("Error! (compilation of FLang): No variable named " <> tshow v)) pure (M.lookup v dict)
              FLCEApp f x -> Left $ "Error! (compilation of FLang): impossible! (FLCEApp should be removed in flcCompileExpr1, but " <> tshow flce <> " is given)" {- do
                f' <- fptrToClosure =<< tryEval =<< flcCompileExpr1 dict (FLCIEFLCE f)
                x' <- flcCompileExpr1 dict (FLCIEFLCE x)
                tryEval =<< slStructConcat f' (x' `SLEStructCons` SLEStructNil) -}

          FLCIEcls targs _ funid args -> Left "Error! (compilation of FLang): closure should not be here!"

      flcGenerateFunc :: FLCFuncDecl -> Either Text SLFuncBlock
      flcGenerateFunc (FLCFuncDecl name args ret body) = do
        let argdict = fst $ L.foldl' (\(d, pos) (aname, t) -> (M.insert aname (SLEArg (flTypeToSLType t) (tshow pos)) d, pos + sltSizeOf (flTypeToSLType t))) (M.empty, 0) args
        body' <- flcCompileExpr1 (funcdict <> argdict) (FLCIEFLCE body)
        pure $ SLFuncBlock (toSLSignature name args ret) (tshow <$> args) ((SLBSingle >>> V.singleton >>> SLBMulti) (
            case body' of
              SLEPushCall c -> SLSTailCallReturn c
              _ -> SLSReturn body'
          ))

      slmain :: Either Text SLFuncBlock
      slmain = do
        flmainbody <- maybe (Left "Error! (compilation of FLang): No main function") pure (M.lookup (FLCUniqueIdentifier FLCPathTop "main") funcdict)
        flmainoriginal <- maybe (Left "Error! (compilation of FLang): No main function") pure (M.lookup (FLCUniqueIdentifier FLCPathTop "main") ((\(FLCProgram p) -> p) program))
        case flmainoriginal of
          FLCFuncDecl _ [] FLTInt _ -> pure $ SLFuncBlock (SLFuncSignature SLFuncMain [] SLTInt) [] ((SLBSingle >>> V.singleton >>> SLBMulti) (SLSTailCallReturn (SLClosureCall flmainbody)))
          _ -> Left "Error! (compilation of FLang): type of main should be Int"
  in  do
        funcs <- M.mapKeys toSLName <$> forM ((\(FLCProgram decls) -> decls) program) flcGenerateFunc
        main  <- M.singleton SLFuncMain <$> slmain
        pure $ funcs <> main



compileFLangToSLang :: FLProgram Text -> Either Text SLProgram
compileFLangToSLang = flcRenameAndLift >=> flcCompileProgram