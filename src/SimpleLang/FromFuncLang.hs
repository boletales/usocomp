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
-}

-- 識別子初出位置へのパス
data FLCPath =
      FLCPathTop
    | FLCPathInTopDecl Text
    | FLCPathInLetDecl Text FLCPath
    | FLCPathInLetBody      FLCPath
    | FLCPathInAppR         FLCPath
    | FLCPathInAppL         FLCPath
    | FLCPathInLambda       FLCPath
    deriving (Eq, Show, Ord)

flcPathRoots :: FLCPath -> [FLCPath]
flcPathRoots path =
    path :
    case path of
        FLCPathTop           -> []
        FLCPathInTopDecl _   -> []
        FLCPathInLetDecl _ p -> flcPathRoots p
        FLCPathInLetBody   p -> flcPathRoots p
        FLCPathInAppR      p -> flcPathRoots p
        FLCPathInAppL      p -> flcPathRoots p
        FLCPathInLambda    p -> flcPathRoots p

-- 完全な包含関係にあるときのみTrue
flcPathDeeperEq :: FLCPath -> FLCPath -> Bool
flcPathDeeperEq p1 p2 =
    let r1 = flcPathRoots p1
        r2 = flcPathRoots p2
    in  L.length r1 <= L.length r2 && L.elem p2 r1

data FLCUniqueIdentifier = FLCUniqueIdentifier {
      flcuiPath :: FLCPath
    , flcuiName :: Text
    } deriving (Eq, Show, Ord)

-- 0+1
flcPreprocess :: FLProgram Text -> Either Text (FLProgram FLCUniqueIdentifier)
flcPreprocess = flcRenameIdentifier >>> fmap flcLiftLambda

-- 0
flcRenameIdentifier :: FLProgram Text -> Either Text (FLProgram FLCUniqueIdentifier)
flcRenameIdentifier program =
    let globalDict :: M.Map Text FLCUniqueIdentifier
        globalDict = (\(FLVarDecl (FLVar n) _) -> FLCUniqueIdentifier FLCPathTop n) <$> flpTopLevelVars program

        go :: FLExp Text t -> M.Map Text FLCUniqueIdentifier -> FLCPath -> Either Text (FLExp FLCUniqueIdentifier t)
        go e dict path =
            case e of
                FLEValI i           -> Right $ FLEValI i
                FLEValB b           -> Right $ FLEValB b
                FLEVar (FLVar name) ->
                    case M.lookup name dict of
                        Just newName ->  Right $ FLEVar (FLVar newName)
                        Nothing -> Left $ "Undefined variable: " <> name <> " at " <> T.pack (show path)
                FLELambda (FLVar name) body ->
                    let newpath = FLCPathInLambda path
                        newdict = M.insert name (FLCUniqueIdentifier newpath name) dict
                    in  FLELambda (FLVar (FLCUniqueIdentifier newpath name)) <$> go body newdict newpath
                FLEApp f x ->
                    let newF = go f dict (FLCPathInAppL path)
                        newX = go x dict (FLCPathInAppR path)
                    in FLEApp <$> newF <*> newX
                FLELet vs body ->
                    let newDict = F.foldl' (\d (FLVarDecl (FLVar n) _) -> M.insert n (FLCUniqueIdentifier (FLCPathInLetBody path) n) d) dict vs
                        newDecl = mapM (\(FLVarDecl (FLVar n) expr) -> FLVarDecl (FLVar (FLCUniqueIdentifier path n)) <$> go expr newDict (FLCPathInLetDecl n path)) vs
                        newBody = go body newDict (FLCPathInLetBody path)
                    in FLELet <$> newDecl <*> newBody
    in  M.map (\(FLVarDecl (FLVar t) decl) ->
                  FLVarDecl (FLVar (FLCUniqueIdentifier FLCPathTop t))
                    <$> go decl globalDict FLCPathTop
              ) >>>
        M.mapKeys (FLCUniqueIdentifier FLCPathTop) >>>
        sequence >>>
        fmap FLProgram
        $ flpTopLevelVars program

-- 1
flcLiftLambda :: FLProgram FLCUniqueIdentifier -> FLProgram FLCUniqueIdentifier
flcLiftLambda = undefined

-- 2
flcComipleExp :: FLExp FLCUniqueIdentifier t -> TypedSLExp (FLTypeToSLType t)
flcComipleExp = undefined
