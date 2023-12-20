{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SimpleLang.FromFuncLang where


import FuncLang.Def
import SimpleLang.Def
import SimpleLang.Tools.Manual
import Data.Text as T
import Control.Category
import Prelude hiding ((.), id)
import Data.Map as M
import Data.Foldable as F
import qualified Data.List as L
import Control.Monad
import Control.Monad.State
import Data.Function ((&))
import Control.Monad.Except
import Control.Applicative (Applicative(liftA2))
import Data.Bifunctor

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
    FLCELet        :: [FLCVarDecl] -> FLCExp -> FLCExp
    --FLCEUncapture  :: FLCUniqueIdentifier -> [(FLCUniqueIdentifier, FLType)] -> FLCExp

data FLCVarDecl = FLCVarDecl FLCUniqueIdentifier FLType FLCExp

data FLCFuncDecl = FLCFuncDecl FLCUniqueIdentifier [(FLCUniqueIdentifier, FLType)] FLCExp

newtype FLCProgram = FLCProgram (M.Map FLCUniqueIdentifier FLCFuncDecl)

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
    FLCELet vs body -> flceTypeOf body

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
    } deriving (Eq, Ord)

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


        renameLift1 :: FLExp Text t -> M.Map Text FLCUniqueIdentifier -> FLCPath -> StateT [FLCFuncDecl] (Either Text) FLCExp
        renameLift1 e dict path =
            case e of
                FLEValI i           -> pure $ FLCEValI i
                FLEValB b           -> pure $ FLCEValB b
                FLEVar (FLVar name) ->
                    case M.lookup name dict of
                        Just newName ->  pure $ FLCEVar newName (flTypeOf e)
                        Nothing -> throwError $ "Undefined variable: " <> name <> " at " <> T.pack (show path)
                FLELambda (FLVar arg) body -> do
                  let newname = FLCUniqueIdentifier (FLCPathTopLiftedFrom path) "anonymous"
                  let newpath = FLCPathInLambda path
                  argtype <-
                    case flTypeOf e of
                      FLTLambda t1 _ -> pure t1
                      _ -> throwError "Error! (type check of FLang): not a function"

                  let args = [(arg, argtype)]

                  let argdict = M.fromList ((\(a, t) -> (a, FLCUniqueIdentifier newpath a)) <$> args)
                  extvars <- lift $ searchExternalVar body (dict <> argdict) newpath

                  let innerdict =
                           globalDict
                        <> M.fromList ((\(v,_) -> (flcuiName v, v)) <$> extvars)
                        <> argdict

                  lambdabody <- renameLift1 body innerdict (FLCPathInLiftedLambda path arg)

                  let newlambda = FLCFuncDecl newname (extvars <> (first (FLCUniqueIdentifier newpath) <$> args)) lambdabody
                  modify (newlambda :)
                  pure $ appCapturedVar extvars (FLCEVar newname (flTypeOf e))


                FLEApp f x ->
                    let newF = renameLift1 f dict (FLCPathInAppL path)
                        newX = renameLift1 x dict (FLCPathInAppR path)
                    in FLCEApp <$> newF <*> newX
                FLELet vs body ->
                    let newDict = F.foldl' (\d (FLVarDecl (FLVar n) _) -> M.insert n (FLCUniqueIdentifier (FLCPathInLetBody path) n) d) dict vs
                        newDecl = mapM (\(FLVarDecl (FLVar n) expr) -> FLCVarDecl (FLCUniqueIdentifier path n) (flTypeOf expr) <$> renameLift1 expr newDict (FLCPathInLetDecl n path)) vs
                        newBody = renameLift1 body newDict (FLCPathInLetBody path)
                    in FLCELet <$> newDecl <*> newBody

        result = flpTopLevelVars program &
              (
                M.toList >>>
                fmap
                  (\(_, FLVarDecl (FLVar t) decl) ->
                    (\(body, decls) -> FLCFuncDecl (FLCUniqueIdentifier FLCPathTop t) [] body : decls) <$> runStateT (renameLift1 decl globalDict (FLCPathInTopDecl t)) []
                   ) >>>
                sequence >>>
                fmap (
                  join >>>
                  fmap (\decl@(FLCFuncDecl t _ _) -> (t, decl)) >>>
                  M.fromList >>>
                  FLCProgram >>>
                  id
                ) >>>
                id
              )
    in  result


-- 2
flcComipleExp :: FLExp FLCUniqueIdentifier t -> SLExp
flcComipleExp expr =
  case expr of
    FLEValI   i -> SLEConst (SLVal i)
    FLEValB   b -> SLEConst (SLVal (if b then 1 else 0))
    FLEVar (FLVar (FLCUniqueIdentifier path name)) -> undefined

    --  SLEPushCall (SLClosureCall (SLFuncPtr (SLUserFunc path name) (FLTypeToSLType (flVarType (FLVar (FLCUniqueIdentifier path name)))) >: SLEStructNil))
    --FLELambda :: forall tag t1 t2. FLVar tag t1 -> FLExp tag t2  -> FLExp tag ('FLTLambda t1 t2)
    --FLEApp    :: forall tag t1 t2. FLExp tag ('FLTLambda t1 t2) -> FLExp tag t1 -> FLExp tag t2
    --FLELet    :: forall tag t2   . [FLVarDecl tag] -> FLExp tag t2 -> FLExp tag t2




prettyPrintFLCVarDecl :: FLCVarDecl -> Text
prettyPrintFLCVarDecl (FLCVarDecl v t e) = T.pack (show v) <> " = " <> prettyPrintFLCExp e


prettyPrintFLCExp :: FLCExp -> Text
prettyPrintFLCExp e =
  let tshow :: Show a => a -> Text
      tshow = T.pack . show
  in
    case e of
      (FLCEValI i)      -> tshow i
      (FLCEValB b)      -> tshow b
      (FLCEVar v t)     -> tshow v -- <> " :: " <> tshow t
      (FLCEApp e1 e2)   -> "(" <>   prettyPrintFLCExp e1 <> " " <> prettyPrintFLCExp e2 <> ")"
      (FLCELet decls b) -> "(let " <> T.intercalate "; " (prettyPrintFLCVarDecl <$> decls) <> " in " <> prettyPrintFLCExp b <> ")"

prettyPrintFLCFuncDecl :: FLCFuncDecl -> Text
prettyPrintFLCFuncDecl (FLCFuncDecl name args body) =
  let tshow :: Show a => a -> Text
      tshow = T.pack . show
  in
    tshow name <> "(" <> T.intercalate ", " ((fst >>> tshow) <$> args) <> ") = " <> prettyPrintFLCExp body

prettyPrintFLCProgram :: FLCProgram -> Text
prettyPrintFLCProgram (FLCProgram decls) = T.intercalate "\n" (prettyPrintFLCFuncDecl <$> M.elems decls)