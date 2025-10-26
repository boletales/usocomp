{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SimpleLang.Tools.Manual.Internal where

import MyPrelude

import SimpleLang.Def
import Data.Vector as V
import Control.Monad.State as S
import Data.Proxy
import Data.Kind
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import SimpleLang.TypedDef
import Data.Text as T
import qualified Data.List as L

newtype SLMVar (t :: SLType) = SLMVar Text deriving (Show, Eq, Ord)

unSLMVar :: SLMVar t -> Text
unSLMVar (SLMVar x) = x

newtype SLMArg (t :: SLType) = SLMArg Text deriving (Show, Eq, Ord)

unSLMArg :: SLMArg t -> Text
unSLMArg (SLMArg x) = x

newtype SLMBlock (ret :: SLType) = SLMBlock TypedSLBlock

unSLMBlock :: SLMBlock ret -> TypedSLBlock
unSLMBlock (SLMBlock x) = x

data SLMState (ret :: SLType) = SLMState {
      slmVarCnt :: Int              -- 今のスコープで有効なローカル変数の個数
    , slmBlocks :: V.Vector TypedSLBlock -- コード
  }

type SLManualBlockM ret = State (SLMState ret)

-- 今のスコープで有効なローカル変数の個数を覚えたままコード片を抽出
clipslm :: SLManualBlockM ret () -> SLManualBlockM ret TypedSLBlock
clipslm m = do
  cnt <- gets slmVarCnt
  let SLMState _ blocks = execState m (SLMState cnt V.empty)
  pure (TSLBMulti blocks)

{-
runslm :: Int -> SLFuncSignature -> SLManualBlockM ret () -> TypedSLFuncBlock args ret
runslm args name m =
  let SLMState _ blocks = execState m (SLMState 0 V.empty)
  in SLFuncBlock name args (SLBMulti blocks)
-}

class SLMNAryC (args :: [SLType]) where
  type SLMNaryF args (x :: Type) -- :: t[0] -> t[1] -> ... -> r
  slmfuncToHsFunc       :: (TypedSLExp ('SLTStruct args) -> x) -> SLMNaryF args x
  hsFuncToSLMFuncHelper :: [Text] -> SLMNaryF args x -> x

instance SLMNAryC '[] where
  type SLMNaryF '[] x = x
  slmfuncToHsFunc       f          = f TSLEStructNil
  hsFuncToSLMFuncHelper _ f = f

instance forall args newarg. (SLMNAryC args , KnownType newarg, KnownTypes args) => SLMNAryC (newarg : args) where
  type SLMNaryF (newarg : args) x = TypedSLExp newarg -> SLMNaryF args x
  slmfuncToHsFunc       f arg       = slmfuncToHsFunc (f . TSLEStructCons arg)
  hsFuncToSLMFuncHelper (argname : argnames) f  = (hsFuncToSLMFuncHelper @args) argnames (f (TSLEArg argname :: TypedSLExp newarg))
  hsFuncToSLMFuncHelper _ _ = error "hsFuncToSLMFuncHelper: argument names exhausted"

normalizeArgNames :: forall args. KnownTypes args => [Text] -> [Text]
normalizeArgNames argnames = 
  let argcount = L.length (tslTypesVal (Proxy @args))
      renamed = fst $ L.foldl (\(args, argset) name ->
                          let go n =
                                if Set.member n argset
                                  then go (n <> "_")
                                  else n
                          in (args <> [go name], Set.insert (go name) argset)
                      ) ([], Set.empty) argnames
      filled = renamed <> (("A" <>) . tshow <$> [L.length renamed + 1 .. argcount])
  in filled

hsFuncToSLMFunc :: forall args ret. (SLMNAryC args, KnownTypes args, KnownType ret) => SLFuncName -> [Text] -> SLMNaryF args (SLManualBlockM ret ()) -> TypedSLFuncBlock args ret
hsFuncToSLMFunc name argnames f =
  let normalized = normalizeArgNames @args argnames
  in TSLFuncBlock { 
          tslfSignature = TypedSLFunc (SLFuncSignature name (tslTypesVal (Proxy @args)) (tslTypeVal (Proxy @ret))) :: TypedSLFunc args ret
        , tslfArgs     = normalized
        , tslfBlock    =  (hsFuncToSLMFuncHelper @args @(SLManualBlockM ret ()) normalized
                            >>> flip execState (SLMState 0 V.empty)
                            >>> slmBlocks
                            >>> TSLBMulti ) f
      }

hsFuncToSLFuncBlock :: forall args ret. (SLMNAryC args, KnownTypes args, KnownType ret) => SLFuncName -> [Text] -> SLMNaryF args (SLManualBlockM ret ()) -> SLFuncBlock
hsFuncToSLFuncBlock name argnames f =
  let b = hsFuncToSLMFunc @args @ret name argnames f
  in unTypedSLFuncBlock b

_app :: forall args ret t. (TypedSLCallable args ret t, SLMNAryC args, KnownType ret) => t -> SLMNaryF args (TypedSLExp ret)
_app callable = slmfuncToHsFunc (TSLEPushCall . tslCall callable) :: SLMNaryF args (TypedSLExp ret)

slmTailCall :: forall args ret t. (SLMNAryC args, TypedSLCallable args ret t, KnownType ret) => t -> SLMNaryF args (SLManualBlockM ret ())
slmTailCall x = slmfuncToHsFunc (\(args :: TypedSLExp ('SLTStruct args)) -> do
    SLMState cnt blocks <- get
    put (SLMState cnt (V.snoc blocks (TSLBSingle (TSLSTailCallReturn (tslCall @args @ret x args)))))
    (pure () :: SLManualBlockM ret ())
  )
newtype SLMFuncsM x =
      SLMFuncsM (State (M.Map SLFuncName SLFuncBlock) x)
    deriving (Functor, Applicative, Monad, MonadState (M.Map SLFuncName SLFuncBlock))

runSLMFuncsM :: SLMFuncsM () -> SLProgram
runSLMFuncsM (SLMFuncsM m) = execState m M.empty

--  ((natVal >>> fromIntegral) (Proxy :: Proxy n))
slmFunc :: forall (args :: [SLType]) (ret :: SLType). (KnownTypes args, KnownType ret, SLMNAryC args) => SLFuncName -> [Text] -> SLMNaryF args (SLManualBlockM ret ()) -> SLMFuncsM (TypedSLFuncBlock args ret)
slmFunc name argnames f = do
  let fblock = hsFuncToSLMFunc @args @ret name argnames f
  S.modify (M.insert name (unTypedSLFuncBlock fblock))
  pure fblock


slmVirtualFunc :: forall args ret. (KnownType ret, KnownTypes args) => SLFuncName -> TypedSLFuncBlock args ret
slmVirtualFunc name =
  TSLFuncBlock {
      tslfSignature = TypedSLFunc (SLFuncSignature name (tslTypesVal (Proxy @args)) (tslTypeVal (Proxy @ret))) :: TypedSLFunc args ret
    , tslfArgs      = tshow <$> [0 .. L.length (tslTypesVal (Proxy @args)) - 1]
    , tslfBlock     = TSLBMulti V.empty
  }

slmSetRealFunc :: forall (args :: [SLType]) (ret :: SLType). (KnownTypes args, KnownType ret, SLMNAryC args) => TypedSLFuncBlock args ret -> [Text] -> SLMNaryF args (SLManualBlockM ret ()) ->  SLMFuncsM ()
slmSetRealFunc v argnames f = void $ slmFunc @args @ret (slfsName (unTypedSLFunc $ tslfSignature v)) argnames f
