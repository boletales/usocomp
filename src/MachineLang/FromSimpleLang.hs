{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MonoLocalBinds #-}

module MachineLang.FromSimpleLang (
      compileSLProgram
    , interpretReg
    , MLCReg(..)
  ) where

import MachineLang.Def
import SimpleLang.Def
import SimpleLang.Tools

import Data.Vector as V
import Data.Map as M
import qualified Data.List as L
import Control.Category
import Prelude hiding ((.), id)

import Control.Monad.State as S
import Control.Monad
import VectorBuilder.Builder as VB
import VectorBuilder.Vector as VB
import Data.Bitraversable (Bitraversable(bitraverse))
import Control.Monad.Except
import Data.Text as T

-- 接頭辞 MLC: MachineLang.FromSimpleLang の内部でのみ利用する型


{-

スタックフレームの構造：

=============
ローカル変数       ← MLCRegStackPtr
...
ローカル変数
旧フレームポインタ ← MLCRegFramePtr
旧スタックポインタ
リターンアドレス
引数
...
引数
=============
[返り値のための空き] ← 旧スタックポインタ
ローカル変数
....
-}

{-| 抽象化された即値。MLCValConst以外はcompileSLProgramでしかるべき値に書き換わります -}
data MLCVal =
        MLCValConst Int
      | MLCValJumpDestLocal Int
      | MLCValJumpDestFunc  SLFuncName
      | MLCValJumpDestEnd

{-| レジスタ別名 -}
data MLCReg =
        MLCRegX -- 引数用レジスタ
      | MLCRegY -- 〃
      | MLCRegZ -- 〃
      | MLCRegW
      | MLCRegStackPtr
      | MLCRegFramePtr
      | MLCRegPC

type MLCInst = MLInst' MLCReg MLCVal

{-| コンパイル中のMachineLangコード片 -}
type MLCFlagment = VB.Builder (MLCInst, SLPos)

data MLCError =
        MLCENoMain
      | MLCNoSuchFunc SLFuncName SLPos
      | MLCCannotSubstToArg SLPos
      | MLCTypeError Text SLPos
      | MLCNoSuchVar Text SLPos
      | MLCNoSuchArg Text SLPos
      deriving stock (Show, Eq)

throwMLCError :: (SLPos -> MLCError) -> MonadMLCFunc a
throwMLCError e = MonadMLCFunc (S.gets mmlcfsLineInfo) >>= \pos -> MonadMLCFunc (throwError (e pos))

liftTypeError :: Either SLTypeError a -> MonadMLCFunc a
liftTypeError = either (throwMLCError . MLCTypeError . prettyPrintSLTypeError) pure

getVarAddr :: Text -> MonadMLCFunc Int
getVarAddr name = MonadMLCFunc (gets (mmlcfsLocalAddrDict >>> M.lookup name)) >>= \case
    Nothing -> throwMLCError (MLCNoSuchVar name)
    Just v  -> pure v

getArgAddr :: Text -> MonadMLCFunc Int
getArgAddr name = MonadMLCFunc (gets (mmlcfsArgAddrDict >>> M.lookup name)) >>= \case
    Nothing -> throwMLCError (MLCNoSuchArg name)
    Just v  -> pure v

data MonadMLCFuncState = MonadMLCFuncState {
      mmlcfsFlagment :: MLCFlagment
    , mmlcfsLineInfo :: SLPos
    , mmlcfsVarCnt   :: Int
    , mmlcfsLocalAddrDict :: M.Map Text Int
    , mmlcfsArgAddrDict :: M.Map Text Int
  }

newtype MonadMLCFunc x =
          MonadMLCFunc (StateT MonadMLCFuncState (Either MLCError) x)
            deriving newtype (
                Functor
              , Applicative
              , Monad
              , MonadError MLCError
            )

unMonadMLCFunc :: MonadMLCFunc x -> StateT MonadMLCFuncState (Either MLCError) x
unMonadMLCFunc (MonadMLCFunc v) = v

-- ソースマップの位置情報に階層を追加
inPos :: SLLocalPos -> MonadMLCFunc x -> MonadMLCFunc x
inPos pos (MonadMLCFunc v) = MonadMLCFunc (do
    oldpos <- gets mmlcfsLineInfo
    S.modify (\s -> s {mmlcfsLineInfo = pushPos pos (mmlcfsLineInfo s)})
    x <- v
    S.modify (\s -> s {mmlcfsLineInfo = oldpos})
    pure x
  )

outPos :: MonadMLCFunc x -> MonadMLCFunc x
outPos (MonadMLCFunc v) = MonadMLCFunc (do
    oldpos <- gets mmlcfsLineInfo
    S.modify (\s -> s {mmlcfsLineInfo = popPos (mmlcfsLineInfo s)})
    x <- v
    S.modify (\s -> s {mmlcfsLineInfo = oldpos})
    pure x
  )

stateWriteFromList :: [MLCInst] -> MonadMLCFunc ()
stateWriteFromList v2 = MonadMLCFunc (do
    lineinfo <- gets mmlcfsLineInfo
    S.modify (\s -> s {mmlcfsFlagment = mmlcfsFlagment s <> VB.foldable ((, lineinfo) <$> v2)})
  )

stateWriteFromFlagment :: MLCFlagment -> MonadMLCFunc ()
stateWriteFromFlagment v2 = MonadMLCFunc (do
    S.modify (\s -> s {mmlcfsFlagment = mmlcfsFlagment s <> v2})
  )


execMonadMLCFunc :: MonadMLCFunc () -> SLPos -> M.Map Text Int -> M.Map Text Int -> Either MLCError MLCFlagment
execMonadMLCFunc v lineinfo ldict adict =
  (flip execStateT (MonadMLCFuncState VB.empty lineinfo 0 ldict adict)
    >>> fmap mmlcfsFlagment) (unMonadMLCFunc v)

-- ソースマップの情報を覚えたままコード片を抽出
clipBlockFlagment :: MonadMLCFunc () -> MonadMLCFunc MLCFlagment
clipBlockFlagment v = MonadMLCFunc $ do
  lineinfo <- gets mmlcfsLineInfo
  ldict    <- gets mmlcfsLocalAddrDict
  adict    <- gets mmlcfsArgAddrDict
  case execMonadMLCFunc v lineinfo ldict adict of
    Left  err      -> throwError err
    Right flagment -> pure flagment

getFlagmentSize :: MLCFlagment -> Int
getFlagmentSize = VB.size

{-
getLength :: MonadMLCFunc Int
getLength = MonadMLCFunc (gets (mmlcfsFlagment >>> VB.size))
-}

poshere :: MonadMLCFunc SLPos
poshere = MonadMLCFunc (gets mmlcfsLineInfo)

slReturnToMLC :: SLExp -> MonadMLCFunc ()
slReturnToMLC expr = do
  slPushToMLC expr
  stateWriteFromList [
        MLIConst  MLCRegX       (MLCValConst (-1))
      , MLIAdd    MLCRegZ        MLCRegX MLCRegFramePtr  -- MLCRegZ ← 旧スタックポインタ置き場のアドレス
      , MLILoad   MLCRegZ        MLCRegZ                 -- MLCRegZ ← 旧スタックポインタの中身(返り値アドレス)
    ]

  exprsize <- liftTypeError $ sleSizeOf expr
  Control.Monad.forM_ [0 .. (exprsize - 1)] (\_ ->
    stateWriteFromList [
        MLILoad   MLCRegY        MLCRegStackPtr          -- MLCRegY ← 返り値の中身
      , MLIStore  MLCRegY        MLCRegZ                 -- 返り値を入れる
      , MLIAdd    MLCRegStackPtr MLCRegStackPtr MLCRegX
      , MLIAdd    MLCRegZ        MLCRegZ        MLCRegX
    ])

  stateWriteFromList [
        MLICopy   MLCRegX        MLCRegFramePtr          -- 現フレームポインタを覚えておく
      , MLILoad   MLCRegFramePtr MLCRegFramePtr          -- フレームポインタを戻す

      , MLIConst  MLCRegY       (MLCValConst (-1))
      , MLIAdd    MLCRegX        MLCRegX MLCRegY
      , MLILoad   MLCRegStackPtr MLCRegX                 -- スタックポインタを戻す

      , MLIAdd    MLCRegX        MLCRegX MLCRegY
      , MLILoad   MLCRegPC       MLCRegX                 -- ジャンプ
    ]

pushCallToRegZSnippet :: Int -> [MLInst' MLCReg MLCVal]
pushCallToRegZSnippet argsize =
  let jumplength = 9
  in  [
        -- リターンアドレス
          MLIConst  MLCRegX         (MLCValConst 1)
        , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
        , MLIConst  MLCRegY         (MLCValConst jumplength)
        , MLIAdd    MLCRegY          MLCRegY        MLCRegPC             -- MLCRegY ← call処理の次の命令アドレス  jumplength: このつぎの命令から  
        , MLIStore  MLCRegY          MLCRegStackPtr

        -- 旧スタックポインタ
        , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
        , MLIConst  MLCRegY         (MLCValConst (-argsize - 2))
        , MLIAdd    MLCRegY          MLCRegStackPtr MLCRegY              -- MLCRegY ← 現スタックポインタ - 引数の個数 - 2 (= 返り値置き場のアドレス)
        , MLIStore  MLCRegY          MLCRegStackPtr

        -- 旧フレームポインタ
        , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
        , MLIStore  MLCRegFramePtr   MLCRegStackPtr

        -- フレームポインタ書き換え
        , MLICopy   MLCRegFramePtr   MLCRegStackPtr



        -- ジャンプ
        , MLICopy   MLCRegPC         MLCRegZ                            --jumplength: この命令までの命令数
      ]



slSolidCallToMLC :: SLFuncSignature -> SLExp -> MonadMLCFunc ()
slSolidCallToMLC funcsig args = do
  slPushToMLC args

  argsize <- liftTypeError $ sleSizeOf args

  stateWriteFromList [
      MLIConst MLCRegZ (MLCValJumpDestFunc (slfsName funcsig))
    ]

  stateWriteFromList (pushCallToRegZSnippet argsize)

slPtrCallToMLC :: SLRef -> SLExp -> MonadMLCFunc ()
slPtrCallToMLC ref args = do
  argsize <- liftTypeError $ sleSizeOf args

  slPushToMLC (slRefToPtr ref)
  slPushToMLC args
  stateWriteFromList [
        MLIConst  MLCRegX         (MLCValConst (negate argsize))
      , MLIAdd    MLCRegZ          MLCRegStackPtr MLCRegX
      , MLILoad   MLCRegZ          MLCRegZ
    ]

  stateWriteFromList (pushCallToRegZSnippet argsize)


slClosureCallToMLC :: SLExp -> MonadMLCFunc ()
slClosureCallToMLC cls = do
  clssize <- liftTypeError $ sleSizeOf cls
  let argsize = clssize - 1

  slPushToMLC cls

  stateWriteFromList [
        MLIConst  MLCRegX         (MLCValConst (negate argsize))
      , MLIAdd    MLCRegZ          MLCRegStackPtr MLCRegX
      , MLILoad   MLCRegZ          MLCRegZ
    ]

  stateWriteFromList (pushCallToRegZSnippet argsize)


tailCallToRegWSnippet :: Int -> [MLInst' MLCReg MLCVal]
tailCallToRegWSnippet argsize =
    [
        MLIConst MLCRegY        (MLCValConst (-2))
      , MLIAdd   MLCRegY         MLCRegY        MLCRegFramePtr
      , MLIConst MLCRegX        (MLCValConst 1)
      , MLIAdd   MLCRegStackPtr  MLCRegStackPtr MLCRegX
      , MLILoad  MLCRegZ         MLCRegY
      , MLIStore MLCRegZ         MLCRegStackPtr          -- リターンアドレスをスタックトップにコピー

      , MLIAdd   MLCRegY         MLCRegY        MLCRegX
      , MLIAdd   MLCRegStackPtr  MLCRegStackPtr MLCRegX
      , MLILoad  MLCRegZ         MLCRegY
      , MLIStore MLCRegZ         MLCRegStackPtr          -- 旧スタックポインタをスタックトップにコピー

      , MLIAdd   MLCRegY         MLCRegY        MLCRegX
      , MLIAdd   MLCRegStackPtr  MLCRegStackPtr MLCRegX
      , MLILoad  MLCRegZ         MLCRegY
      , MLIStore MLCRegZ         MLCRegStackPtr          -- 旧フレームポインタをスタックトップにコピー

      , MLIConst MLCRegY        (MLCValConst (-2 - argsize))
      , MLIAdd   MLCRegY         MLCRegY MLCRegStackPtr  -- 引っ越し元開始位置: ↑で評価した引数の一番下

      , MLIConst MLCRegZ        (MLCValConst (-1))
      , MLIAdd   MLCRegZ         MLCRegStackPtr MLCRegZ
      , MLILoad  MLCRegStackPtr  MLCRegZ
      , MLIAdd   MLCRegStackPtr  MLCRegStackPtr MLCRegX  -- 引っ越し先開始位置: スタックフレームの底

      , MLIConst MLCRegZ        (MLCValConst (argsize + 2))
      , MLIAdd   MLCRegFramePtr  MLCRegStackPtr MLCRegZ  -- フレームポインタ書き換え
    ]

  <> L.intercalate [
        MLIAdd   MLCRegY        MLCRegY        MLCRegX
      , MLIAdd   MLCRegStackPtr MLCRegStackPtr MLCRegX
      ] (L.replicate (argsize + 3) [
        MLILoad  MLCRegZ        MLCRegY
      , MLIStore MLCRegZ        MLCRegStackPtr
    ]) -- 引っ越し

  <> [
        MLICopy MLCRegPC MLCRegW
    ]


slSolidTailCallReturnToMLC :: SLFuncSignature -> SLExp -> MonadMLCFunc ()
slSolidTailCallReturnToMLC funcsig args = do
  slPushToMLC args

  argsize <- liftTypeError $ sleSizeOf args

  stateWriteFromList [
      MLIConst MLCRegW (MLCValJumpDestFunc (slfsName funcsig))
    ]

  stateWriteFromList (tailCallToRegWSnippet argsize)

slPtrTailCallReturnToMLC :: SLRef -> SLExp -> MonadMLCFunc ()
slPtrTailCallReturnToMLC ref args = do
  argsize <- liftTypeError $ sleSizeOf args

  slPushToMLC (slRefToPtr ref)
  slPushToMLC args
  stateWriteFromList [
        MLIConst  MLCRegX         (MLCValConst (negate argsize))
      , MLIAdd    MLCRegW          MLCRegStackPtr MLCRegX
      , MLILoad   MLCRegW          MLCRegW
    ]

  stateWriteFromList (tailCallToRegWSnippet argsize)


slClosureTailCallReturnToMLC :: SLExp -> MonadMLCFunc ()
slClosureTailCallReturnToMLC cls = do
  clssize <- liftTypeError $ sleSizeOf cls
  let argsize = clssize - 1


  slPushToMLC cls

  stateWriteFromList [
        MLIConst  MLCRegX         (MLCValConst (negate argsize))
      , MLIAdd    MLCRegW          MLCRegStackPtr MLCRegX
      , MLILoad   MLCRegW          MLCRegW
    ]

  stateWriteFromList (tailCallToRegWSnippet argsize)


slPushToMLC :: SLExp -> MonadMLCFunc ()
slPushToMLC expr = inPos (SLLPExpr expr) $ do
  case expr of
    SLEConst (SLVal v) ->
      stateWriteFromList [
            MLIConst  MLCRegX         (MLCValConst 1)
          , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
          , MLIConst  MLCRegY         (MLCValConst v)
          , MLIStore  MLCRegY          MLCRegStackPtr
        ]

    SLEFuncPtr fsig ->
      stateWriteFromList [
            MLIConst  MLCRegX         (MLCValConst 1)
          , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
          , MLIConst  MLCRegY         (MLCValJumpDestFunc (slfsName fsig))
          , MLIStore  MLCRegY          MLCRegStackPtr
        ]

    SLELocal t vname -> do
      v <- getVarAddr vname
      Control.Monad.forM_ [0 .. (sltSizeOf t - 1)] (\i ->
          stateWriteFromList [
                MLIConst  MLCRegX         (MLCValConst  1)
              , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
              , MLIConst  MLCRegX         (MLCValConst (1 + v + i))
              , MLIAdd    MLCRegX          MLCRegX MLCRegFramePtr -- i番目のローカル変数は、フレームポインタ指し先 + i + 1
              , MLILoad   MLCRegX          MLCRegX
              , MLIStore  MLCRegX          MLCRegStackPtr
            ]
        )

    SLEArg t aname -> do
      a <- getArgAddr aname
      Control.Monad.forM_ [0 .. (sltSizeOf t - 1)] (\i ->
          stateWriteFromList [
                MLIConst  MLCRegX         (MLCValConst 1)
              , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
              , MLIConst  MLCRegX         (MLCValConst (-1))
              , MLIAdd    MLCRegX          MLCRegX MLCRegFramePtr -- MLCRegX ← 旧スタックポインタ置き場のアドレス
              , MLILoad   MLCRegX          MLCRegX                -- MLCRegX ← 旧スタックポインタの指し先
              , MLIConst  MLCRegY         (MLCValConst (1 + a + i))
              , MLIAdd    MLCRegX          MLCRegX MLCRegY        -- i番目の引数は、旧スタックポインタ指し先 + i + 1
              , MLILoad   MLCRegX          MLCRegX
              , MLIStore  MLCRegX          MLCRegStackPtr
            ]
        )

    SLEPushCall call -> do
      exprsize <- liftTypeError $ sleSizeOf expr
      stateWriteFromList [
            MLIConst  MLCRegX         (MLCValConst exprsize)
          , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
        ] -- 返り値のためにスタックを高くしておく
      case call of
        SLSolidFuncCall fname args -> slSolidCallToMLC fname args
        SLFuncRefCall   fref  args -> slPtrCallToMLC   fref  args
        SLClosureCall   closure    -> slClosureCallToMLC closure

    SLEPrim1 prim exp1       -> slPrim1ToMLC prim exp1

    SLEPrim2 prim exp1 exp2  -> slPrim2ToMLC prim exp1 exp2


    SLEStructNil       -> pure ()
    SLEStructCons e es -> do
      pos <- poshere
      (case slpLocalPos pos of
          _ : SLLPExpr (SLEStructCons _ _) : _ -> outPos
          _ -> id
        ) (slPushToMLC e >> slPushToMLC es)

    SLEUnion t inner -> do
      innersize <- liftTypeError $ sleSizeOf inner
      slPushToMLC inner
      stateWriteFromList [
            MLIConst  MLCRegX         (MLCValConst (sltSizeOf t - innersize))
          , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
        ]


    SLEIndirection ptr -> do
      slPushToMLC ptr
      stateWriteFromList [
            MLILoad   MLCRegX          MLCRegStackPtr
          , MLILoad   MLCRegX          MLCRegX
          , MLIStore  MLCRegX          MLCRegStackPtr
        ] -- expr'の評価先を再利用

    SLEAddrOf ref ->
      case ref of
        SLRefLocal _ vname -> do
          v <- getVarAddr vname
          stateWriteFromList [
                MLIConst  MLCRegX         (MLCValConst  1)
              , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
              , MLIConst  MLCRegX         (MLCValConst (1 + v))
              , MLIAdd    MLCRegX          MLCRegX MLCRegFramePtr -- i番目のローカル変数は、フレームポインタ指し先 + i + 1
              , MLIStore  MLCRegX          MLCRegStackPtr
            ]

        SLRefPtr _ ptr ->
          slPushToMLC ptr

    SLEPtrShift ptr shift -> do
      ptrtype <- liftTypeError $ sleTypeOf ptr
      slPrim2ToMLC SLPrim2Add ptr (SLECast ptrtype shift)

    SLEStructGet str p -> do
      exprsize <- liftTypeError $ sleSizeOf expr
      slStructGetToMLC str exprsize p

    SLECast _ expr' -> slPushToMLC expr'

slStructGetToMLC :: SLExp -> Int -> Int -> MonadMLCFunc ()
slStructGetToMLC expr returnsize p =
  let recHelper :: Int -> SLExp -> Int -> MonadMLCFunc ()
      recHelper offset expr' _ =
        case expr' of
            SLEStructGet expr'' p' -> do
              newoffset <- liftTypeError $ sleGetOffset expr'' p'
              recHelper (offset + newoffset) expr'' p'

            _ -> slStructGetRawToMLC expr' returnsize offset
  in do
    initialOffset <- liftTypeError $ sleGetOffset expr p
    recHelper initialOffset expr p


slStructGetRawToMLC :: SLExp -> Int -> Int -> MonadMLCFunc ()
slStructGetRawToMLC expr returnsize offset =
  let slegetRec expr' offset' = do
        case expr' of
            --SLEStructGet expr''' (Proxy :: Proxy j) -> slegetRec (offset + sleSizeOf expr''') expr'''

            SLELocal _ vname -> do
              v <- getVarAddr vname
              stateWriteFromList [
                    MLIConst  MLCRegY         (MLCValConst (1 + v - 1))
                  , MLIAdd    MLCRegY          MLCRegY MLCRegFramePtr -- i番目のローカル変数は、フレームポインタ指し先 + i + 1 なので、その直前
                ]
              copyStractValNextToRegYToStackTop offset'

            SLEArg _ aname -> do
              a <- getArgAddr aname
              stateWriteFromList [
                    MLIConst  MLCRegX         (MLCValConst (-1))
                  , MLIAdd    MLCRegX          MLCRegX MLCRegFramePtr -- MLCRegX ← 旧スタックポインタ置き場のアドレス
                  , MLILoad   MLCRegX          MLCRegX                -- MLCRegX ← 旧スタックポインタの指し先
                  , MLIConst  MLCRegY         (MLCValConst (1 + a    - 1))
                  , MLIAdd    MLCRegY          MLCRegX MLCRegY        -- i番目の引数は、旧スタックポインタ指し先 + i + 1 なので、その直前
                ]
              copyStractValNextToRegYToStackTop offset'

            _ -> do
              slPushToMLC expr'
              exprsize' <- liftTypeError $ sleSizeOf expr'

              stateWriteFromList [
                    MLIConst  MLCRegX         (MLCValConst (negate exprsize'))
                  , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
                  , MLIAdd    MLCRegY          MLCRegStackPtr MLCRegX
                ]

              copyStractValNextToRegYToStackTop offset


      copyStractValNextToRegYToStackTop offset' = do
        stateWriteFromList [
              MLIConst  MLCRegX         (MLCValConst offset')
            , MLIAdd    MLCRegY          MLCRegY MLCRegX
            , MLIConst  MLCRegX         (MLCValConst 1)
          ]

        Control.Monad.forM_ [0 .. (returnsize - 1)] (\_ ->
            stateWriteFromList [
                  MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
                , MLIAdd    MLCRegY          MLCRegY        MLCRegX
                , MLILoad   MLCRegZ          MLCRegY
                , MLIStore  MLCRegZ          MLCRegStackPtr
              ]
          )
  in slegetRec expr offset

slPrim1ToMLC :: SLPrim1 -> SLExp ->MonadMLCFunc ()
slPrim1ToMLC prim exp1 =
  let prim1helper inst = do
        stateWriteFromList [
              MLILoad   MLCRegZ          MLCRegStackPtr
            , inst      MLCRegZ          MLCRegZ
            , MLIStore  MLCRegZ          MLCRegStackPtr
          ]

  in case prim of
      SLPrim1Inv   -> slPushToMLC exp1 >> prim1helper MLIInv

slPrim2ToMLC :: SLPrim2 -> SLExp -> SLExp -> MonadMLCFunc ()
slPrim2ToMLC prim exp1 exp2 =
  let prim2helper inst = do
        stateWriteFromList [
              MLIConst  MLCRegX         (MLCValConst (-1))
            , MLILoad   MLCRegZ          MLCRegStackPtr
            , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
            , MLILoad   MLCRegY          MLCRegStackPtr
            , inst      MLCRegY          MLCRegY MLCRegZ
            , MLIStore  MLCRegY          MLCRegStackPtr
          ]

  in case prim of
      SLPrim2Add   -> slPushToMLC exp1 >> slPushToMLC exp2 >> prim2helper MLIAdd
      SLPrim2Sub   -> slPushToMLC exp1 >> slPushToMLC exp2 >> prim2helper MLISub
      SLPrim2Mult  -> slPushToMLC exp1 >> slPushToMLC exp2 >> prim2helper MLIMult
      SLPrim2Shift -> slPushToMLC exp1 >> slPushToMLC exp2 >> prim2helper MLIShift
      SLPrim2And   -> slPushToMLC exp1 >> slPushToMLC exp2 >> prim2helper MLIAnd
      SLPrim2Or    -> slPushToMLC exp1 >> slPushToMLC exp2 >> prim2helper MLIOr
      SLPrim2Xor   -> slPushToMLC exp1 >> slPushToMLC exp2 >> prim2helper MLIXor
      SLPrim2Gt    -> slPushToMLC exp1 >> slPushToMLC exp2 >> prim2helper MLIGt
      SLPrim2Lt    -> slPushToMLC exp1 >> slPushToMLC exp2 >> prim2helper MLILt
      SLPrim2Eq    -> slPushToMLC exp1 >> slPushToMLC exp2 >> prim2helper MLIEq

--mlcInternalSetVarCnt :: Int -> MonadMLCFunc ()
--mlcInternalSetVarCnt n =
--  MonadMLCFunc (S.modify (\s -> s {mmlcfsVarCnt = n}))

mlcInternalSetVarDict :: M.Map Text Int -> MonadMLCFunc ()
mlcInternalSetVarDict dict =
  MonadMLCFunc (S.modify (\s -> s {mmlcfsLocalAddrDict = dict}))

mlcInternalShiftVarCnt :: Int -> MonadMLCFunc ()
mlcInternalShiftVarCnt n =
  MonadMLCFunc (S.modify (\s -> s {mmlcfsVarCnt = mmlcfsVarCnt s + n}))

mlcInternalAddVar :: Text -> MonadMLCFunc ()
mlcInternalAddVar vname = do
  varcnt <- MonadMLCFunc (gets mmlcfsVarCnt)
  MonadMLCFunc (S.modify (\s -> s {mmlcfsLocalAddrDict = M.insert vname varcnt (mmlcfsLocalAddrDict s)}))

mlcVarScope :: MonadMLCFunc () -> MonadMLCFunc ()
mlcVarScope v =do
      oldvarcnt <- MonadMLCFunc (gets mmlcfsVarCnt)
      oldvdict <- MonadMLCFunc (gets mmlcfsLocalAddrDict)

      v

      varcnt <- MonadMLCFunc (gets mmlcfsVarCnt)
      when (varcnt > oldvarcnt) $ do
        slPopnToMLC (varcnt - oldvarcnt)
        mlcInternalShiftVarCnt (-(varcnt - oldvarcnt))
    
      mlcInternalSetVarDict oldvdict

slPopnToMLC :: Int -> MonadMLCFunc ()
slPopnToMLC n = do
  stateWriteFromList [
        MLIConst  MLCRegX         (MLCValConst (-n))
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
    ]


slSubstVarToMLC :: Text -> SLExp -> MonadMLCFunc ()
slSubstVarToMLC vname expr = do
  var <- getVarAddr vname
  slPushToMLC expr
  exprsize <- liftTypeError $ sleSizeOf expr
  Control.Monad.forM_ (L.reverse [0..(exprsize - 1)]) (\i ->
      stateWriteFromList [
            MLILoad   MLCRegY          MLCRegStackPtr
          , MLIConst  MLCRegX         (MLCValConst (-1))
          , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
          , MLIConst  MLCRegX         (MLCValConst (1 + var + i))
          , MLIAdd    MLCRegX          MLCRegX MLCRegFramePtr -- i番目のローカル変数は、フレームポインタ指し先 + i + 1
          , MLIStore  MLCRegY          MLCRegX
        ]
    )

slSubstPtrToMLC :: SLExp -> SLExp -> MonadMLCFunc ()
slSubstPtrToMLC ptr expr = do
  slPushToMLC expr -- -> RegY
  slPushToMLC ptr  -- -> RegZ
  stateWriteFromList [
        MLILoad   MLCRegZ          MLCRegStackPtr
      , MLIConst  MLCRegX         (MLCValConst (-1))
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
    ]
  exprsize <- liftTypeError $ sleSizeOf expr
  Control.Monad.forM_ (L.reverse [0..(exprsize - 1)]) (\i ->
    stateWriteFromList [
              MLILoad   MLCRegY          MLCRegStackPtr
            , MLIConst  MLCRegX         (MLCValConst (-1))
            , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
            , MLIConst  MLCRegX         (MLCValConst i)
            , MLIAdd    MLCRegX          MLCRegX MLCRegZ
            , MLIStore  MLCRegY          MLCRegX
      ]
    )



slBlockToMLC :: SLBlock -> MonadMLCFunc ()
slBlockToMLC block = do
  case block of
    SLBSingle statement          -> slSingleToMLC statement
    SLBMulti  blocks             -> slMultiToMLC      blocks
    SLBCase   cases defaultBlock -> slCaseToMLC cases defaultBlock
    SLBWhile  cond body          -> slWhileToMLC cond body

slSingleToMLC :: SLStatement -> MonadMLCFunc ()
slSingleToMLC statement =
  case statement of
    --SLSPrimPush exp -> slPushToMLC exp
    --SLSPrimPop      -> slPopToMLC
    SLSInitVar vname expr -> do
      exprsize <- liftTypeError $ sleSizeOf expr
      --trace ("SLSInitVar: " <> show t <> " " <> show exprsize <> " " <> show expr) $ pure ()
      slPushToMLC expr >> mlcInternalAddVar vname >> mlcInternalShiftVarCnt exprsize
    SLSSubst ref expr ->
      case ref of
        SLRefPtr   _ ptr -> slSubstPtrToMLC ptr expr
        SLRefLocal _ var -> slSubstVarToMLC var expr
    SLSReturn expr  -> slReturnToMLC expr
    SLSTailCallReturn call ->
      case call of
        SLSolidFuncCall fname args -> slSolidTailCallReturnToMLC fname args
        SLFuncRefCall   fref  args -> slPtrTailCallReturnToMLC   fref  args
        SLClosureCall   closure    -> slClosureTailCallReturnToMLC closure

slMultiToMLC :: V.Vector SLBlock -> MonadMLCFunc ()
slMultiToMLC blocks = do
  stateWriteFromFlagment =<< clipBlockFlagment (mlcVarScope $
        V.imapM_ (\i block ->
          inPos (SLLPMulti i) (slBlockToMLC block)
        ) blocks
      )


slWhileToMLC :: SLExp -> SLBlock -> MonadMLCFunc ()
slWhileToMLC cond block = do
  body <- clipBlockFlagment $ mlcVarScope $ inPos SLLPWhileBody $ slBlockToMLC block
  let bodysize = getFlagmentSize body

  header <- clipBlockFlagment $ inPos SLLPWhileCond (do
          slPushToMLC cond
          stateWriteFromList [
                MLILoad    MLCRegX MLCRegStackPtr
              , MLIConst   MLCRegY (MLCValConst (-1))
              , MLIAdd     MLCRegStackPtr MLCRegStackPtr MLCRegY
              , MLIConst   MLCRegY (MLCValConst ( 1 {- block頭まで -} + bodysize + 4 {- 後半の直後まで -}))
              , MLIAdd     MLCRegY MLCRegY MLCRegPC
              , MLINotJump MLCRegY MLCRegX
            ]
        )

  let headersize = getFlagmentSize header
  footer <- clipBlockFlagment $ inPos SLLPWhileFooter (do
          stateWriteFromList [
                MLIConst   MLCRegY (MLCValConst (-2 {- block直後まで -} - bodysize {- block頭まで -} - headersize {- header頭まで -}))
              , MLIAdd     MLCRegY MLCRegY MLCRegPC
              , MLIJump    MLCRegY
            ]
        )

  stateWriteFromFlagment header >> stateWriteFromFlagment body >> stateWriteFromFlagment footer

slCaseToMLC :: V.Vector (SLExp, SLBlock) -> SLBlock -> MonadMLCFunc ()
slCaseToMLC cases defaultBlock = do
  elseflagment <- clipBlockFlagment $ mlcVarScope $ inPos SLLPCaseElseBody (slBlockToMLC defaultBlock)
  stateWriteFromFlagment =<< V.ifoldM (\code i (cond, block)-> do
        f <- clipBlockFlagment (do
            body <- clipBlockFlagment $ mlcVarScope $ inPos (SLLPCaseBody i) $ slBlockToMLC block
            let bodysize = getFlagmentSize body + 3
            inPos (SLLPCaseCond i) (do
              slPushToMLC cond
              stateWriteFromList [
                    MLILoad    MLCRegX MLCRegStackPtr
                  , MLIConst   MLCRegY (MLCValConst (-1))
                  , MLIAdd     MLCRegStackPtr MLCRegStackPtr MLCRegY
                  , MLIConst   MLCRegY (MLCValConst (1 {- body頭まで -} +  bodysize {- body直後まで -}))
                  , MLIAdd     MLCRegY MLCRegY MLCRegPC
                  , MLINotJump MLCRegY MLCRegX
                ])
            inPos (SLLPCaseBody i) (
              stateWriteFromFlagment body >>
              stateWriteFromList [
                    MLIConst   MLCRegY (MLCValConst (1 + getFlagmentSize code))
                  , MLIAdd     MLCRegY MLCRegY MLCRegPC
                  , MLIJump    MLCRegY
                ])
          )
        pure $ f <> code
      ) elseflagment (V.reverse cases)

initializer :: MonadMLCFunc ()
initializer = do
  stateWriteFromList [
        MLINop

      -- リターンアドレス
      , MLIConst  MLCRegX         (MLCValConst 1)
      , MLIConst  MLCRegStackPtr  (MLCValConst 1)
      , MLIConst  MLCRegY         MLCValJumpDestEnd
      , MLIStore  MLCRegY         MLCRegStackPtr

      -- スタックポインタ
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
      , MLIConst  MLCRegY         (MLCValConst 0)
      , MLIStore  MLCRegY         MLCRegStackPtr

      -- フレームポインタ
      , MLIConst  MLCRegY         (MLCValConst 0)
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
      , MLIStore  MLCRegY         MLCRegStackPtr

      -- フレームポインタ書き換え
      , MLICopy   MLCRegFramePtr   MLCRegStackPtr


      -- ジャンプ
      , MLIConst  MLCRegPC        (MLCValJumpDestFunc SLFuncMain)        --jumplength: この命令のつぎまでの命令数
    ]


compileSLFunc :: SLFuncBlock -> Either MLCError MLCFlagment
compileSLFunc slfunc =
  let argdict =
        fst $ L.foldl (\(dict, pos) (argname, argtype) -> (M.insert argname pos dict, pos + sltSizeOf argtype)) (M.empty, 0) (L.zip (slfArgs slfunc) (slfsArgs (slfSignature slfunc)))
  in  execMonadMLCFunc (slBlockToMLC (slfBlock slfunc) >> inPos SLLPForceReturn (slReturnToMLC (SLEConst (SLVal 0)))) (SLPos (slfsName (slfSignature slfunc)) []) M.empty argdict



interpretReg :: MLCReg -> MLReg
interpretReg = \case
  MLCRegPC       -> MLRegPC
  MLCRegFramePtr -> MLReg0
  MLCRegStackPtr -> MLReg1
  MLCRegX        -> MLReg2
  MLCRegY        -> MLReg3
  MLCRegZ        -> MLReg4
  MLCRegW        -> MLReg5

{-|
  SimpleLangからMachineLangを生成します。
-}

compileSLProgram :: SLProgram -> Either MLCError (V.Vector (MLInst, SLPos))
compileSLProgram program =
  case M.lookup SLFuncMain program of
    Nothing -> Left MLCENoMain
    Just fmain -> do
      initcode <- execMonadMLCFunc initializer (SLPos SLFuncMain []) M.empty M.empty
      (mlccode, mlcfuncmap) <- Control.Monad.foldM (\(code, funcmap) slfunc -> do
              code' <- compileSLFunc slfunc
              pure (code <> code' , M.insert (slfsName (slfSignature slfunc)) (VB.size code) funcmap)
            ) (initcode, M.empty) (fmain : (M.toList >>> fmap snd) (M.delete SLFuncMain program))

      V.mapM (\(inst :: MLCInst, pos) ->
          ( , pos) <$>
            bitraverse (pure <<< interpretReg) (\case
                  MLCValConst v            -> pure $ MLVal v
                  MLCValJumpDestLocal v    -> pure $ MLVal v
                  MLCValJumpDestFunc fname -> maybe (Left (MLCNoSuchFunc fname   pos)) (MLVal >>> Right) (M.lookup fname mlcfuncmap)
                  MLCValJumpDestEnd        -> pure $ MLVal (VB.size mlccode)
                ) inst
            ) (VB.build (mlccode <> VB.singleton (MLINop, SLPos SLFuncMain [])))

