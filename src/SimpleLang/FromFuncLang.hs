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

type family FLTypeToSLType (t :: FLType) :: SLType where
    FLTypeToSLType 'FLTInt  = 'SLTInt
    FLTypeToSLType 'FLTBool = 'SLTInt
    FLTypeToSLType ('FLTLambda t1 t2) = 'SLTStruct ('SLTFuncPtr (FLTypeToSLType t1 ': '[]) (FLTypeToSLType t2) ': (FLTypeToSLType t1 ': '[]))
    FLTypeToSLType ('FLTTuple ts)     = 'SLTStruct (MapFLTypeToSLType ts)

type family MapFLTypeToSLType (ts :: [FLType]) :: [SLType] where
    MapFLTypeToSLType '[] = '[]
    MapFLTypeToSLType (t:ts) = FLTypeToSLType t ': MapFLTypeToSLType ts


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
            FLCPathInAppR      p      -> show p <> ".r"
            FLCPathInAppL      p      -> show p <> ".l"
            FLCPathInLambda    p      -> show p <> ".f"

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
flcRenameAndLift :: FLProgram Text -> Either Text (FLProgram FLCUniqueIdentifier)
flcRenameAndLift program =
    let globalDict :: M.Map Text FLCUniqueIdentifier
        globalDict = (\(FLVarDecl (FLVar n) _) -> FLCUniqueIdentifier FLCPathTop n) <$> flpTopLevelVars program

        
        searchExternalVar :: FLExp Text t -> M.Map Text FLCUniqueIdentifier -> FLCPath -> Either Text [FLCUniqueIdentifier]
        searchExternalVar expr dict pathorig =
          let search1 :: FLExp Text t -> M.Map Text FLCUniqueIdentifier -> FLCPath -> Either Text [FLCUniqueIdentifier]
              search1 expr' dict' path' =
                  case expr' of
                      FLEValI _           -> pure []
                      FLEValB _           -> pure []
                      FLEVar (FLVar name) ->
                          case M.lookup name dict' of
                              Just newName -> 
                                if flcPathDeeperEq (flcuiPath newName) pathorig
                                then pure []
                                else pure [newName]
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

        captureExternalVar :: [FLCUniqueIdentifier] -> FLExp Text t -> FLExp Text t
        captureExternalVar vars expr =
          case vars of
              [] -> expr
              v:vs -> flOverRideTypeCheck (FLELambda (FLVar (flcuiName v)) (captureExternalVar vs expr))

        appCapturedVar :: [FLCUniqueIdentifier] -> FLExp FLCUniqueIdentifier t -> FLExp FLCUniqueIdentifier t
        appCapturedVar vars expr =
          case vars of
              []   -> expr
              v:vs -> appCapturedVar vs (FLEApp (flOverRideTypeCheck expr) (FLEVar (FLVar v)))

        renameLift1 :: FLExp Text t -> M.Map Text FLCUniqueIdentifier -> FLCPath -> StateT [FLVarDecl FLCUniqueIdentifier] (Either Text) (FLExp FLCUniqueIdentifier t)
        renameLift1 e dict path =
            case e of
                FLEValI i           -> pure $ FLEValI i
                FLEValB b           -> pure $ FLEValB b
                FLEVar (FLVar name) ->
                    case M.lookup name dict of
                        Just newName ->  pure $ FLEVar (FLVar newName)
                        Nothing -> throwError $ "Undefined variable: " <> name <> " at " <> T.pack (show path)
                FLELambda (FLVar name) body ->
                    if isTopLevelLambda path
                    then
                      let newpath = FLCPathInLambda path
                          newdict = M.insert name (FLCUniqueIdentifier newpath name) dict
                      in  FLELambda (FLVar (FLCUniqueIdentifier newpath name)) <$> renameLift1 body newdict newpath
                    else do
                      let newname = FLCUniqueIdentifier (FLCPathTopLiftedFrom path) "lambda"
                      let newpath = FLCPathInLambda path
                      let newdict = M.insert name (FLCUniqueIdentifier newpath name) dict
                      extvars <- lift $ searchExternalVar body newdict newpath
                      newlambda <- renameLift1 (captureExternalVar extvars e) globalDict (FLCPathInLiftedLambda path name)
                      modify (FLVarDecl (FLVar newname) newlambda :)
                      pure $ appCapturedVar extvars (FLEVar (FLVar newname))
                FLEApp f x ->
                    let newF = renameLift1 f dict (FLCPathInAppL path)
                        newX = renameLift1 x dict (FLCPathInAppR path)
                    in FLEApp <$> newF <*> newX
                FLELet vs body ->
                    let newDict = F.foldl' (\d (FLVarDecl (FLVar n) _) -> M.insert n (FLCUniqueIdentifier (FLCPathInLetBody path) n) d) dict vs
                        newDecl = mapM (\(FLVarDecl (FLVar n) expr) -> FLVarDecl (FLVar (FLCUniqueIdentifier path n)) <$> renameLift1 expr newDict (FLCPathInLetDecl n path)) vs
                        newBody = renameLift1 body newDict (FLCPathInLetBody path)
                    in FLELet <$> newDecl <*> newBody

        result = flpTopLevelVars program &
              (
                M.toList >>>
                fmap
                  (\(_, FLVarDecl (FLVar t) decl) ->
                    (\(body, decls) -> FLVarDecl (FLVar (FLCUniqueIdentifier FLCPathTop t)) body : decls) <$> runStateT (renameLift1 decl globalDict (FLCPathInTopDecl t)) []
                   ) >>>
                sequence >>>
                fmap (
                  join >>>
                  fmap (\decl@(FLVarDecl (FLVar t) _) -> (t, decl)) >>>
                  M.fromList >>>
                  FLProgram >>>
                  id
                ) >>>
                id
              )
    in  result


-- 2
flcComipleExp :: FLExp FLCUniqueIdentifier t -> TypedSLExp (FLTypeToSLType t)
flcComipleExp = undefined
