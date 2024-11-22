{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MonoLocalBinds #-}

module MachineLang.FromSimpleLang (
      compileSLProgram
    , interpretReg
    , MLCReg(..)
    , mlcOptionDefault
    , generateSourceMap
  ) where

import MyPrelude

import MachineLang.Def
import SimpleLang.Def
import SimpleLang.Tools

import Data.Vector as V
import Data.Map.Strict as M
import qualified Data.List as L

import Control.Monad.State as S
import Control.Monad
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
        MLCRegX -- 汎用レジスタ
      | MLCRegY -- 〃
      | MLCRegZ -- 〃
      | MLCRegStackPtr
      | MLCRegFramePtr
      | MLCRegPC

type MLCInst = MLInst' MLCReg MLCVal

{-| コンパイル中のMachineLangコード片 -}
type MLCFlagment = V.Vector (MLCInst, SLPos)

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
    , mmlcfsCompilerOpt :: MLCOption
    , mmlcfsMaxStackFrameSize :: Int
    , mmlcfsStackSizeHere     :: Int
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
    S.modify (\s -> s {mmlcfsFlagment = mmlcfsFlagment s <> V.fromList ((, lineinfo) <$> v2)})
  )

stateWriteFromFlagment :: MLCFlagment -> MonadMLCFunc ()
stateWriteFromFlagment v2 = MonadMLCFunc (do
    S.modify (\s -> s {mmlcfsFlagment = mmlcfsFlagment s <> v2})
  )


execMonadMLCFunc :: MonadMLCFunc () -> SLPos -> M.Map Text Int -> M.Map Text Int -> MLCOption -> Int -> Int -> Either MLCError (MLCFlagment, Int)
execMonadMLCFunc v lineinfo ldict adict opt maxstackframesize stacksizehere =
  (flip execStateT (MonadMLCFuncState V.empty lineinfo 0 ldict adict opt maxstackframesize stacksizehere)
    >>> fmap (\s -> (mmlcfsFlagment s, mmlcfsMaxStackFrameSize s))) (unMonadMLCFunc v)

-- ソースマップの情報を覚えたままコード片を抽出
clipBlockFlagment :: MonadMLCFunc () -> MonadMLCFunc MLCFlagment
clipBlockFlagment v = MonadMLCFunc $ do
  lineinfo <- gets mmlcfsLineInfo
  ldict    <- gets mmlcfsLocalAddrDict
  adict    <- gets mmlcfsArgAddrDict
  opt      <- gets mmlcfsCompilerOpt
  maxstackframesize <- gets mmlcfsMaxStackFrameSize
  stacksizehere <- gets mmlcfsStackSizeHere
  case execMonadMLCFunc v lineinfo ldict adict opt maxstackframesize stacksizehere of
    Left  err      -> throwError err
    Right (flagment, sfsize) -> do
      S.modify (\s -> s {mmlcfsMaxStackFrameSize = max (mmlcfsMaxStackFrameSize s) sfsize})
      pure flagment

getFlagmentSize :: MLCFlagment -> Int
getFlagmentSize = V.length

useStackSize :: Int -> MonadMLCFunc a -> MonadMLCFunc a
useStackSize s x = MonadMLCFunc (do
    sizenow <- gets mmlcfsStackSizeHere
    S.modify (\st -> st {mmlcfsMaxStackFrameSize = max (sizenow + s) (mmlcfsMaxStackFrameSize st), mmlcfsStackSizeHere = sizenow + s})
    result <- unMonadMLCFunc x
    S.modify (\st -> st {mmlcfsStackSizeHere = sizenow})
    pure result
  )

addStackSize :: Int -> MonadMLCFunc ()
addStackSize s = MonadMLCFunc (do
    sizenow <- gets mmlcfsStackSizeHere
    S.modify (\st -> st {mmlcfsMaxStackFrameSize = max (sizenow + s) (mmlcfsMaxStackFrameSize st), mmlcfsStackSizeHere = sizenow + s})
  )

{-
getLength :: MonadMLCFunc Int
getLength = MonadMLCFunc (gets (mmlcfsFlagment >>> VB.size))
-}

incr :: MLCVal
incr = MLCValConst 1

decr :: MLCVal
decr = MLCValConst (-1)

poshere :: MonadMLCFunc SLPos
poshere = MonadMLCFunc (gets mmlcfsLineInfo)

slReturnToMLC :: SLExp -> MonadMLCFunc ()
slReturnToMLC expr = do
  slPushToMLC expr
  stateWriteFromList [
        MLIAddI   MLCRegY        MLCRegFramePtr  decr -- MLCRegY ← 旧スタックポインタ置き場のアドレス
      , MLILoad   MLCRegY        MLCRegY                            -- MLCRegY ← 旧スタックポインタの中身(返り値アドレス)
    ]

  exprsize <- liftTypeError $ sleSizeOf expr
  Control.Monad.forM_ [0 .. (exprsize - 1)] (\_ ->
    stateWriteFromList [
        MLILoad   MLCRegX        MLCRegStackPtr          -- MLCRegX ← 返り値の中身
      , MLIStore  MLCRegX        MLCRegY                 -- 返り値を入れる
      , MLIAddI   MLCRegStackPtr MLCRegStackPtr decr
      , MLIAddI   MLCRegY        MLCRegY        decr
    ])

  stateWriteFromList [
        MLICopy   MLCRegX        MLCRegFramePtr          -- 現フレームポインタを覚えておく
      , MLILoad   MLCRegFramePtr MLCRegFramePtr          -- フレームポインタを戻す

      , MLIAddI   MLCRegX        MLCRegX decr
      , MLILoad   MLCRegStackPtr MLCRegX                 -- スタックポインタを戻す

      , MLIAddI   MLCRegX        MLCRegX decr
      , MLILoad   MLCRegPC       MLCRegX                 -- ジャンプ
    ]

pushCallToRegYSnippet :: Int -> [MLInst' MLCReg MLCVal]
pushCallToRegYSnippet argsize =
  let jumplength = 8
  in  [
        -- リターンアドレス
          MLIAddI   MLCRegStackPtr   MLCRegStackPtr incr
        , MLIAddI   MLCRegX          MLCRegPC       (MLCValConst jumplength) -- MLCRegX ← call処理の次の命令アドレス  jumplength: このつぎの命令から  
        , MLIStore  MLCRegX          MLCRegStackPtr

        -- 旧スタックポインタ
        , MLIAddI   MLCRegStackPtr   MLCRegStackPtr incr
        , MLIAddI   MLCRegX          MLCRegStackPtr (MLCValConst (-argsize - 2)) -- MLCRegX ← 現スタックポインタ - 引数の個数 - 2 (= 返り値置き場のアドレス)
        , MLIStore  MLCRegX          MLCRegStackPtr

        -- 旧フレームポインタ
        , MLIAddI   MLCRegStackPtr   MLCRegStackPtr incr
        , MLIStore  MLCRegFramePtr   MLCRegStackPtr

        -- フレームポインタ書き換え
        , MLICopy   MLCRegFramePtr   MLCRegStackPtr



        -- ジャンプ
        , MLICopy   MLCRegPC         MLCRegY                            --jumplength: この命令までの命令数
      ]



slSolidCallToMLC :: SLFuncSignature -> SLExp -> MonadMLCFunc ()
slSolidCallToMLC funcsig args = do
  slPushToMLC args

  argsize <- liftTypeError $ sleSizeOf args

  stateWriteFromList [
      MLIConst MLCRegY (MLCValJumpDestFunc (slfsName funcsig))
    ]

  useStackSize (argsize + sltSizeOf SLTInt * 3) $ stateWriteFromList (pushCallToRegYSnippet argsize)

slPtrCallToMLC :: SLRef -> SLExp -> MonadMLCFunc ()
slPtrCallToMLC ref args = do

  slPushToMLC (slRefToPtr ref)
  slPushToMLC args

  argsize <- liftTypeError $ sleSizeOf args

  stateWriteFromList [
        MLIAddI   MLCRegY          MLCRegStackPtr (MLCValConst (negate argsize))
      , MLILoad   MLCRegY          MLCRegY
    ]

  useStackSize (argsize + sltSizeOf SLTInt * 3) $ stateWriteFromList (pushCallToRegYSnippet argsize)


slClosureCallToMLC :: SLExp -> MonadMLCFunc ()
slClosureCallToMLC cls = do
  clssize <- liftTypeError $ sleSizeOf cls
  let argsize = clssize - 1

  slPushToMLC cls

  stateWriteFromList [
        MLIAddI   MLCRegY          MLCRegStackPtr (MLCValConst (negate argsize))
      , MLILoad   MLCRegY          MLCRegY
    ]

  useStackSize (argsize + sltSizeOf SLTInt * 3) $ stateWriteFromList (pushCallToRegYSnippet argsize)


tailCallToRegZSnippet :: Int -> [MLInst' MLCReg MLCVal]
tailCallToRegZSnippet argsize =
    [
        MLIAddI  MLCRegX         MLCRegFramePtr (MLCValConst (-2))
      , MLIAddI  MLCRegStackPtr  MLCRegStackPtr incr
      , MLILoad  MLCRegY         MLCRegX
      , MLIStore MLCRegY         MLCRegStackPtr          -- リターンアドレスをスタックトップにコピー

      , MLIAddI  MLCRegX         MLCRegX        incr
      , MLIAddI  MLCRegStackPtr  MLCRegStackPtr incr
      , MLILoad  MLCRegY         MLCRegX
      , MLIStore MLCRegY         MLCRegStackPtr          -- 旧スタックポインタをスタックトップにコピー

      , MLIAddI  MLCRegX         MLCRegX        incr
      , MLIAddI  MLCRegStackPtr  MLCRegStackPtr incr
      , MLILoad  MLCRegY         MLCRegX
      , MLIStore MLCRegY         MLCRegStackPtr          -- 旧フレームポインタをスタックトップにコピー

      , MLIAddI  MLCRegX         MLCRegStackPtr (MLCValConst (-2 - argsize)) -- 引っ越し元開始位置: ↑で評価した引数の一番下

      , MLIAddI  MLCRegY         MLCRegStackPtr (MLCValConst (-1))
      , MLILoad  MLCRegStackPtr  MLCRegY
      , MLIAddI  MLCRegStackPtr  MLCRegStackPtr incr  -- 引っ越し先開始位置: スタックフレームの底

      , MLIAddI  MLCRegFramePtr  MLCRegStackPtr (MLCValConst (argsize + 2))  -- フレームポインタ書き換え
    ]

  <> L.intercalate [
        MLIAddI  MLCRegX        MLCRegX        incr
      , MLIAddI  MLCRegStackPtr MLCRegStackPtr incr
      ] (L.replicate (argsize + 3) [
        MLILoad  MLCRegY        MLCRegX
      , MLIStore MLCRegY        MLCRegStackPtr
    ]) -- 引っ越し

  <> [
        MLICopy MLCRegPC MLCRegZ
    ]


slSolidTailCallReturnToMLC :: SLFuncSignature -> SLExp -> MonadMLCFunc ()
slSolidTailCallReturnToMLC funcsig args = do
  slPushToMLC args

  argsize <- liftTypeError $ sleSizeOf args

  stateWriteFromList [
      MLIConst MLCRegZ (MLCValJumpDestFunc (slfsName funcsig))
    ]

  useStackSize (argsize + sltSizeOf SLTInt * 3) $ stateWriteFromList (tailCallToRegZSnippet argsize)

slPtrTailCallReturnToMLC :: SLRef -> SLExp -> MonadMLCFunc ()
slPtrTailCallReturnToMLC ref args = do
  argsize <- liftTypeError $ sleSizeOf args

  slPushToMLC (slRefToPtr ref)
  slPushToMLC args
  stateWriteFromList [
        MLIAddI   MLCRegZ          MLCRegStackPtr (MLCValConst (negate argsize))
      , MLILoad   MLCRegZ          MLCRegZ
    ]

  useStackSize (argsize + sltSizeOf SLTInt * 3) $ stateWriteFromList (tailCallToRegZSnippet argsize)


slClosureTailCallReturnToMLC :: SLExp -> MonadMLCFunc ()
slClosureTailCallReturnToMLC cls = do
  clssize <- liftTypeError $ sleSizeOf cls
  let argsize = clssize - 1

  slPushToMLC cls

  stateWriteFromList [
        MLIAddI   MLCRegZ          MLCRegStackPtr (MLCValConst (negate argsize))
      , MLILoad   MLCRegZ          MLCRegZ
    ]

  useStackSize (argsize + sltSizeOf SLTInt * 3) $ stateWriteFromList (tailCallToRegZSnippet argsize)

mlcCheckStackOverflow :: Int -> MonadMLCFunc ()
mlcCheckStackOverflow sizeToBeUsed = do
  limit <- MonadMLCFunc (gets (mmlcfsCompilerOpt >>> mlcoptStackSize))
  stateWriteFromList [
        MLIConst  MLCRegX (MLCValConst (limit - sizeToBeUsed))
      , MLILt     MLCRegX MLCRegStackPtr MLCRegX

      , MLIAddI   MLCRegY MLCRegPC (MLCValConst 6)

      , MLIIfJump MLCRegY MLCRegX
      , MLIConst  MLCRegX (MLCValConst 0)
      , MLIConst  MLCRegY (MLCValConst (mlcSpecialCode MLCREStackOverflow))
      , MLIStore  MLCRegY MLCRegX
      , MLIConst  MLCRegY MLCValJumpDestEnd
      , MLIJump   MLCRegY
    ]

slPushToMLC :: SLExp -> MonadMLCFunc ()
slPushToMLC expr = inPos (SLLPExpr expr) $ do
  case expr of
    SLEConst (SLVal v) -> useStackSize (sltSizeOf SLTInt) $
      stateWriteFromList [
            MLIAddI   MLCRegStackPtr   MLCRegStackPtr incr
          , MLIConst  MLCRegX         (MLCValConst v)
          , MLIStore  MLCRegX          MLCRegStackPtr
        ]

    SLEFuncPtr fsig -> useStackSize (sltSizeOf SLTInt) $
      stateWriteFromList [
            MLIAddI   MLCRegStackPtr   MLCRegStackPtr incr
          , MLIConst  MLCRegX         (MLCValJumpDestFunc (slfsName fsig))
          , MLIStore  MLCRegX          MLCRegStackPtr
        ]

    SLELocal t vname -> useStackSize (sltSizeOf SLTInt) $ do
      v <- getVarAddr vname
      Control.Monad.forM_ [0 .. (sltSizeOf t - 1)] (\i ->
          stateWriteFromList [
                MLIAddI   MLCRegStackPtr   MLCRegStackPtr incr
              , MLIAddI   MLCRegX          MLCRegFramePtr (MLCValConst (1 + v + i)) -- i番目のローカル変数は、フレームポインタ指し先 + i + 1
              , MLILoad   MLCRegX          MLCRegX
              , MLIStore  MLCRegX          MLCRegStackPtr
            ]
        )

    SLEArg t aname -> useStackSize (sltSizeOf SLTInt) $ do
      a <- getArgAddr aname
      Control.Monad.forM_ [0 .. (sltSizeOf t - 1)] (\i ->
          stateWriteFromList [
                MLIAddI   MLCRegStackPtr   MLCRegStackPtr incr
              , MLIAddI   MLCRegX          MLCRegFramePtr (MLCValConst (-1)) -- MLCRegX ← 旧スタックポインタ置き場のアドレス
              , MLILoad   MLCRegX          MLCRegX                           -- MLCRegX ← 旧スタックポインタの指し先
              , MLIAddI   MLCRegX          MLCRegX (MLCValConst (1 + a + i)) -- i番目の引数は、旧スタックポインタ指し先 + i + 1
              , MLILoad   MLCRegX          MLCRegX
              , MLIStore  MLCRegX          MLCRegStackPtr
            ]
        )

    SLEPushCall call -> do
      exprsize <- liftTypeError $ sleSizeOf expr
      stateWriteFromList [
          MLIAddI   MLCRegStackPtr   MLCRegStackPtr (MLCValConst exprsize)
        ] -- 返り値のためにスタックを高くしておく
      useStackSize exprsize $
        case call of
          SLSolidFuncCall fname args -> slSolidCallToMLC fname args
          SLFuncRefCall   fref  args -> slPtrCallToMLC   fref  args
          SLClosureCall   closure    -> slClosureCallToMLC closure

    SLEPrim1 prim exp1       -> useStackSize (sltSizeOf SLTInt) $ slPrim1ToMLC prim exp1

    SLEPrim2 prim exp1 exp2  -> useStackSize (sltSizeOf SLTInt) $ slPrim2ToMLC prim exp1 exp2


    SLEStructNil       -> pure ()
    SLEStructCons e es -> do
      pos <- poshere
      (case slpLocalPos pos of
          _ : SLLPExpr (SLEStructCons _ _) : _ -> outPos
          _ -> id
        ) (do
          slPushToMLC e
          exprsize <- liftTypeError $ sleSizeOf e
          useStackSize exprsize $ slPushToMLC es
        )

    SLEUnion t inner -> do
      innersize <- liftTypeError $ sleSizeOf inner
      slPushToMLC inner
      stateWriteFromList [
          MLIAddI   MLCRegStackPtr   MLCRegStackPtr (MLCValConst (sltSizeOf t - innersize))
        ]


    SLEIndirection ptr -> do
      slPushToMLC ptr
      stateWriteFromList [
            MLILoad   MLCRegX          MLCRegStackPtr
          , MLILoad   MLCRegX          MLCRegX
          , MLIStore  MLCRegX          MLCRegStackPtr
        ] -- expr'の評価先を再利用

    SLEAddrOf ref -> useStackSize (sltSizeOf SLTInt) $
      case ref of
        SLRefLocal _ vname -> do
          v <- getVarAddr vname
          stateWriteFromList [
                MLIAddI   MLCRegStackPtr   MLCRegStackPtr incr
              , MLIAddI   MLCRegX          MLCRegFramePtr (MLCValConst (1 + v)) -- i番目のローカル変数は、フレームポインタ指し先 + i + 1
              , MLIStore  MLCRegX          MLCRegStackPtr
            ]

        SLRefPtr _ ptr ->
          slPushToMLC ptr

    SLEPtrShift ptr shift -> useStackSize (sltSizeOf SLTInt) $ do
      ptrtype <- liftTypeError $ sleTypeOf ptr
      slPrim2ToMLC SLPrim2Add ptr (SLECast ptrtype shift)

    SLEStructGet str p -> do
      exprsize <- liftTypeError $ sleSizeOf expr
      useStackSize exprsize $
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
                  MLIAddI   MLCRegX  MLCRegFramePtr (MLCValConst (1 + v - 1)) -- i番目のローカル変数は、フレームポインタ指し先 + i + 1 なので、その直前
                ]
              copyStractValNextToRegXToStackTop offset'

            SLEArg _ aname -> do
              a <- getArgAddr aname
              stateWriteFromList [
                    MLIAddI   MLCRegY          MLCRegFramePtr (MLCValConst (-1))    -- MLCRegY ← 旧スタックポインタ置き場のアドレス
                  , MLILoad   MLCRegY          MLCRegY                              -- MLCRegY ← 旧スタックポインタの指し先
                  , MLIAddI   MLCRegX          MLCRegY (MLCValConst (1 + a    - 1)) -- i番目の引数は、旧スタックポインタ指し先 + i + 1 なので、その直前
                ]
              copyStractValNextToRegXToStackTop offset'

            _ -> do
              slPushToMLC expr'
              exprsize' <- liftTypeError $ sleSizeOf expr'

              stateWriteFromList [
                    MLIAddI   MLCRegStackPtr   MLCRegStackPtr (MLCValConst (negate exprsize'))
                  , MLIAddI   MLCRegX          MLCRegStackPtr (MLCValConst (negate exprsize'))
                ]

              copyStractValNextToRegXToStackTop offset


      copyStractValNextToRegXToStackTop offset' = do
        stateWriteFromList [
              MLIAddI   MLCRegX          MLCRegX (MLCValConst offset')
          ]

        Control.Monad.forM_ [0 .. (returnsize - 1)] (\_ ->
            stateWriteFromList [
                  MLIAddI   MLCRegStackPtr   MLCRegStackPtr incr
                , MLIAddI   MLCRegX          MLCRegX        incr
                , MLILoad   MLCRegY          MLCRegX
                , MLIStore  MLCRegY          MLCRegStackPtr
              ]
          )
  in slegetRec expr offset

slPrim1ToMLC :: SLPrim1 -> SLExp ->MonadMLCFunc ()
slPrim1ToMLC prim exp1 =
  let prim1helper inst = do
        stateWriteFromList [
              MLILoad   MLCRegY          MLCRegStackPtr
            , inst      MLCRegY          MLCRegY
            , MLIStore  MLCRegY          MLCRegStackPtr
          ]

  in case prim of
      SLPrim1Inv   -> slPushToMLC exp1 >> prim1helper MLIInv

slPrim2ToMLC :: SLPrim2 -> SLExp -> SLExp -> MonadMLCFunc ()
slPrim2ToMLC prim exp1 exp2 =
  let prim2helper inst = do
        stateWriteFromList [
              MLILoad   MLCRegY          MLCRegStackPtr
            , MLIAddI   MLCRegStackPtr   MLCRegStackPtr decr
            , MLILoad   MLCRegX          MLCRegStackPtr
            , inst      MLCRegX          MLCRegX MLCRegY
            , MLIStore  MLCRegX          MLCRegStackPtr
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
        MLIAddI    MLCRegStackPtr   MLCRegStackPtr (MLCValConst (-n))
    ]


slSubstDumpToMLC :: SLExp -> MonadMLCFunc ()
slSubstDumpToMLC expr = do
  slPushToMLC expr
  exprsize <- liftTypeError $ sleSizeOf expr
  slPopnToMLC exprsize

slSubstVarToMLC :: Text -> SLExp -> MonadMLCFunc ()
slSubstVarToMLC vname expr = do
  var <- getVarAddr vname
  slPushToMLC expr
  exprsize <- liftTypeError $ sleSizeOf expr
  Control.Monad.forM_ (L.reverse [0..(exprsize - 1)]) (\i ->
      stateWriteFromList [
            MLILoad   MLCRegX          MLCRegStackPtr
          , MLIAddI   MLCRegStackPtr   MLCRegStackPtr decr
          , MLIAddI   MLCRegY          MLCRegFramePtr (MLCValConst (1 + var + i)) -- i番目のローカル変数は、フレームポインタ指し先 + i + 1
          , MLIStore  MLCRegX          MLCRegY
        ]
    )

slSubstPtrToMLC :: SLExp -> SLExp -> MonadMLCFunc ()
slSubstPtrToMLC ptr expr = do
  slPushToMLC expr -- -> RegX
  slPushToMLC ptr  -- -> RegY
  stateWriteFromList [
        MLILoad   MLCRegY          MLCRegStackPtr
      , MLIAddI   MLCRegStackPtr   MLCRegStackPtr decr
    ]
  exprsize <- liftTypeError $ sleSizeOf expr
  Control.Monad.forM_ (L.reverse [0..(exprsize - 1)]) (\i ->
    stateWriteFromList [
              MLILoad   MLCRegX          MLCRegStackPtr
            , MLIAddI   MLCRegStackPtr   MLCRegStackPtr decr
            , MLIAddI   MLCRegY          MLCRegY (MLCValConst i)
            , MLIStore  MLCRegX          MLCRegY
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
      slPushToMLC expr >> mlcInternalAddVar vname >> mlcInternalShiftVarCnt exprsize >> addStackSize exprsize
    SLSSubst ref expr ->
      case ref of
        SLRefPtr   _ ptr -> slSubstPtrToMLC ptr expr
        SLRefLocal _ var -> slSubstVarToMLC var expr
    SLSSubstDump expr -> slSubstDumpToMLC expr
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
                MLILoad    MLCRegY MLCRegStackPtr
              , MLIAddI    MLCRegStackPtr MLCRegStackPtr decr
              , MLIAddI    MLCRegX MLCRegPC (MLCValConst ( 1 {- block頭まで -} + bodysize + 2 {- 後半の直後まで -}))
              , MLINotJump MLCRegX MLCRegY
            ]
        )

  let headersize = getFlagmentSize header
  footer <- clipBlockFlagment $ inPos SLLPWhileFooter (do
          stateWriteFromList [
                MLIAddI    MLCRegX MLCRegPC (MLCValConst (-1 {- block直後まで -} - bodysize {- block頭まで -} - headersize {- header頭まで -}))
              , MLIJump    MLCRegX
            ]
        )

  stateWriteFromFlagment header >> stateWriteFromFlagment body >> stateWriteFromFlagment footer

slCaseToMLC :: V.Vector (SLExp, SLBlock) -> SLBlock -> MonadMLCFunc ()
slCaseToMLC cases defaultBlock = do
  elseflagment <- clipBlockFlagment $ mlcVarScope $ inPos SLLPCaseElseBody (slBlockToMLC defaultBlock)
  stateWriteFromFlagment =<< V.ifoldM (\code i (cond, block)-> do
        f <- clipBlockFlagment (do
            body <- clipBlockFlagment $ mlcVarScope $ inPos (SLLPCaseBody i) $ slBlockToMLC block
            let bodysize = getFlagmentSize body + 2
            inPos (SLLPCaseCond i) (do
              slPushToMLC cond
              stateWriteFromList [
                    MLILoad    MLCRegY MLCRegStackPtr
                  , MLIAddI    MLCRegStackPtr MLCRegStackPtr decr
                  , MLIAddI    MLCRegX MLCRegPC (MLCValConst (1 {- body頭まで -} +  bodysize {- body直後まで -}))
                  , MLINotJump MLCRegX MLCRegY
                ])
            inPos (SLLPCaseBody i) (
              stateWriteFromFlagment body >>
              stateWriteFromList [
                    MLIAddI    MLCRegX MLCRegPC (MLCValConst (1 + getFlagmentSize code))
                  , MLIJump    MLCRegX
                ])
          )
        pure $ f <> code
      ) elseflagment (V.reverse cases)

initializer :: MonadMLCFunc ()
initializer = do
  stateWriteFromList [
        MLINop

      -- リターンアドレス
      , MLIConst  MLCRegStackPtr  (MLCValConst 1)
      , MLIConst  MLCRegX         MLCValJumpDestEnd
      , MLIStore  MLCRegX         MLCRegStackPtr

      -- スタックポインタ
      , MLIAddI   MLCRegStackPtr   MLCRegStackPtr incr
      , MLIConst  MLCRegX         (MLCValConst 0)
      , MLIStore  MLCRegX         MLCRegStackPtr

      -- フレームポインタ
      , MLIConst  MLCRegX         (MLCValConst 0)
      , MLIAddI   MLCRegStackPtr   MLCRegStackPtr incr
      , MLIStore  MLCRegX          MLCRegStackPtr

      -- フレームポインタ書き換え
      , MLICopy   MLCRegFramePtr   MLCRegStackPtr


      -- ジャンプ
      , MLIConst  MLCRegPC        (MLCValJumpDestFunc SLFuncMain)        --jumplength: この命令のつぎまでの命令数
    ]



compileSLFunc :: MLCOption -> SLFuncBlock -> Either MLCError MLCFlagment
compileSLFunc opt slfunc =
  let argdict =
        fst $ L.foldl (\(dict, pos) (argname, argtype) -> (M.insert argname pos dict, pos + sltSizeOf argtype)) (M.empty, 0) (L.zip (slfArgs slfunc) (slfsArgs (slfSignature slfunc)))
  in do
    (_, ssize) <- execMonadMLCFunc
                            (slBlockToMLC (slfBlock slfunc) >> inPos SLLPForceReturn (slReturnToMLC (SLEConst (SLVal 0))))
                            (SLPos (slfsName (slfSignature slfunc)) []) M.empty argdict opt 0 0
    (path2, _) <- execMonadMLCFunc
                            (mlcCheckStackOverflow ssize >>  slBlockToMLC (slfBlock slfunc) >> inPos SLLPForceReturn (slReturnToMLC (SLEConst (SLVal 0))))
                            (SLPos (slfsName (slfSignature slfunc)) []) M.empty argdict opt 0 0
    pure path2



interpretReg :: MLCReg -> MLReg
interpretReg = \case
  MLCRegPC       -> MLRegPC
  MLCRegFramePtr -> MLReg0
  MLCRegStackPtr -> MLReg1
  MLCRegX        -> MLReg2
  MLCRegY        -> MLReg3
  MLCRegZ        -> MLReg4

{-|
  SimpleLangからMachineLangを生成します。
-}

compileSLProgram :: SLProgram -> Either MLCError (V.Vector (MLInst, SLPos))
compileSLProgram = compileSLProgram' mlcOptionDefault

data MLCOption = MLCOption {
      mlcoptDebug :: Bool
    , mlcoptStackSize :: Int
  }

mlcOptionDefault :: MLCOption
mlcOptionDefault = MLCOption {
      mlcoptDebug = False
    , mlcoptStackSize = 5000
  }

compileSLProgram' :: MLCOption -> SLProgram -> Either MLCError (V.Vector (MLInst, SLPos))
compileSLProgram' opt program =
  case M.lookup SLFuncMain program of
    Nothing -> Left MLCENoMain
    Just fmain -> do
      (initcode, _) <- execMonadMLCFunc initializer (SLPos SLFuncMain []) M.empty M.empty opt 0 0
      (mlccode, mlcfuncmap) <- Control.Monad.foldM (\(code, funcmap) slfunc -> do
              code' <- compileSLFunc opt slfunc
              pure (code <> code' , M.insert (slfsName (slfSignature slfunc)) (V.length code) funcmap)
            ) (initcode, M.empty) (fmain : (M.toList >>> fmap snd) (M.delete SLFuncMain program))

      V.mapM (\(inst :: MLCInst, pos) ->
          ( , pos) <$>
            bitraverse (pure <<< interpretReg) (\case
                  MLCValConst v            -> pure $ MLVal v
                  MLCValJumpDestLocal v    -> pure $ MLVal v
                  MLCValJumpDestFunc fname -> maybe (Left (MLCNoSuchFunc fname   pos)) (MLVal >>> Right) (M.lookup fname mlcfuncmap)
                  MLCValJumpDestEnd        -> pure $ MLVal (V.length mlccode)
                ) inst
            ) (mlccode <> V.singleton (MLINop, SLPos SLFuncMain []))


generateSourceMap :: V.Vector (a, SLPos) -> M.Map SLPos (Int, Int)
generateSourceMap = V.ifoldl' (\m i (_, pos) ->
      L.foldl (flip (M.alter (maybe (Just (i+1,i+1)) (\(s,_) -> Just (s,i+1))))) m (rootsSLPos pos)
    ) M.empty

data MLCRuntimeException =
  MLCREStackOverflow
  deriving (Show, Eq)

mlcSpecialCode :: MLCRuntimeException -> Int
mlcSpecialCode = \case
  MLCREStackOverflow -> -1000000000