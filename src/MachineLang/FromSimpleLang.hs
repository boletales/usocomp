{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}

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

data MonadMLCFuncState = MonadMLCFuncState {
      mmlcfsFlagment :: MLCFlagment
    , mmlcfsLineInfo :: SLPos
    , mmlcfsVarCnt   :: Int
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

inPos :: SLLocalPos -> MonadMLCFunc x -> MonadMLCFunc x
inPos pos (MonadMLCFunc v) = MonadMLCFunc (do
    oldpos <- gets mmlcfsLineInfo
    S.modify (\s -> s {mmlcfsLineInfo = pushPos pos (mmlcfsLineInfo s)})
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


execMonadMLCFunc :: MonadMLCFunc () -> SLPos -> Either MLCError MLCFlagment
execMonadMLCFunc v lineinfo =
  (flip execStateT (MonadMLCFuncState VB.empty lineinfo 0)
    >>> fmap mmlcfsFlagment) (unMonadMLCFunc v)

clipBlockFlagment :: MonadMLCFunc () -> MonadMLCFunc MLCFlagment
clipBlockFlagment v = MonadMLCFunc $ do
  lineinfo <- gets mmlcfsLineInfo
  case execMonadMLCFunc v lineinfo of
    Left  err      -> throwError err
    Right flagment -> pure flagment

getFlagmentSize :: MLCFlagment -> Int
getFlagmentSize = VB.size

{-
getLength :: MonadMLCFunc Int
getLength = MonadMLCFunc (gets (mmlcfsFlagment >>> VB.size))

poshere :: MonadMLCFunc SLPos
poshere = MonadMLCFunc (gets mmlcfsLineInfo)
-}


slReturnToMLC :: SLExp -> MonadMLCFunc ()
slReturnToMLC expr = do
  slPushToMLC expr
  stateWriteFromList [
        MLIConst  MLCRegX       (MLCValConst (-1))
      , MLIAdd    MLCRegX        MLCRegX MLCRegFramePtr  -- MLCRegX ← 旧スタックポインタ置き場のアドレス
      , MLILoad   MLCRegX        MLCRegX                 -- MLCRegX ← 旧スタックポインタの中身(返り値アドレス)
      , MLILoad   MLCRegY        MLCRegStackPtr          -- MLCRegY ← 返り値の中身
      , MLIStore  MLCRegY        MLCRegX                 -- 返り値を入れる

      , MLICopy   MLCRegX        MLCRegFramePtr          -- 現フレームポインタを覚えておく
      , MLILoad   MLCRegFramePtr MLCRegFramePtr          -- フレームポインタを戻す

      , MLIConst  MLCRegY       (MLCValConst (-1))
      , MLIAdd    MLCRegX        MLCRegX MLCRegY
      , MLILoad   MLCRegStackPtr MLCRegX                 -- スタックポインタを戻す

      , MLIAdd    MLCRegX        MLCRegX MLCRegY
      , MLILoad   MLCRegPC       MLCRegX                 -- ジャンプ
    ]
slSolidCallToMLC :: SLFuncName -> V.Vector SLExp -> MonadMLCFunc ()
slSolidCallToMLC funcname args = do
  V.forM_ args slPushToMLC

  let jumplength = 10
  stateWriteFromList [
      -- リターンアドレス
        MLIConst  MLCRegX         (MLCValConst 1)
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
      , MLIConst  MLCRegY         (MLCValConst jumplength)
      , MLIAdd    MLCRegY          MLCRegY        MLCRegPC             -- MLCRegY ← call処理の次の命令アドレス  jumplength: このつぎの命令から  
      , MLIStore  MLCRegY          MLCRegStackPtr

      -- 旧スタックポインタ
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
      , MLIConst  MLCRegY         (MLCValConst (-(V.length args) - 2))
      , MLIAdd    MLCRegY          MLCRegStackPtr MLCRegY              -- MLCRegY ← 現スタックポインタ - 引数の個数 - 2 (= 返り値置き場のアドレス)
      , MLIStore  MLCRegY          MLCRegStackPtr

      -- 旧フレームポインタ
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
      , MLIStore  MLCRegFramePtr   MLCRegStackPtr

      -- フレームポインタ書き換え
      , MLICopy   MLCRegFramePtr   MLCRegStackPtr



      -- ジャンプ
      , MLIConst  MLCRegPC        (MLCValJumpDestFunc funcname)        --jumplength: この命令のつぎまでの命令数
    ]

slPtrCallToMLC :: SLExp -> V.Vector SLExp -> MonadMLCFunc ()
slPtrCallToMLC funcptr args = do
  slPushToMLC funcptr
  stateWriteFromList [
        MLILoad   MLCRegZ          MLCRegStackPtr
      , MLIConst  MLCRegX         (MLCValConst -1)
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
    ]

  V.forM_ args slPushToMLC

  let jumplength = 10
  stateWriteFromList [
      -- リターンアドレス
        MLIConst  MLCRegX         (MLCValConst 1)
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
      , MLIConst  MLCRegY         (MLCValConst jumplength)
      , MLIAdd    MLCRegY          MLCRegY        MLCRegPC             -- MLCRegX ← call処理の次の命令アドレス  jumplength: このつぎの命令から  
      , MLIStore  MLCRegY          MLCRegStackPtr

      -- 旧スタックポインタ
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
      , MLIConst  MLCRegY         (MLCValConst (-(V.length args) - 2))
      , MLIAdd    MLCRegY          MLCRegStackPtr MLCRegY              -- MLCRegX ← 現スタックポインタ - 引数の個数 - 2 (= 返り値置き場のアドレス)
      , MLIStore  MLCRegY          MLCRegStackPtr

      -- 旧フレームポインタ
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
      , MLIStore  MLCRegY          MLCRegStackPtr

      -- フレームポインタ書き換え
      , MLICopy   MLCRegFramePtr   MLCRegStackPtr



      -- ジャンプ
      , MLICopy   MLCRegPC         MLCRegZ                            --jumplength: この命令のつぎまでの命令数
    ]

slSolidTailCallReturnToMLC :: SLFuncName -> V.Vector SLExp -> MonadMLCFunc ()
slSolidTailCallReturnToMLC funcname args = do
  V.forM_ args slPushToMLC
  --slPushToMLC func
  stateWriteFromList [
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

      , MLIConst MLCRegY        (MLCValConst (-2 - V.length args))
      , MLIAdd   MLCRegY         MLCRegY MLCRegStackPtr  -- 引っ越し元開始位置: ↑で評価した引数の一番下

      , MLIConst MLCRegX        (MLCValConst (-1))
      , MLIAdd   MLCRegX         MLCRegStackPtr MLCRegX
      , MLILoad  MLCRegStackPtr  MLCRegX
      , MLIConst MLCRegX        (MLCValConst 1)
      , MLIAdd   MLCRegX         MLCRegStackPtr MLCRegX  -- 引っ越し先開始位置: スタックフレームの底
    ]

  stateWriteFromList ( (L.replicate (V.length args + 3) >>> join) [
        MLILoad  MLCRegZ        MLCRegY
      , MLIStore MLCRegZ        MLCRegStackPtr
      , MLIAdd   MLCRegZ        MLCRegZ        MLCRegX
      , MLIAdd   MLCRegStackPtr MLCRegStackPtr MLCRegX
    ]) -- 引っ越し

  stateWriteFromList [
        MLIConst MLCRegPC (MLCValJumpDestFunc funcname)
    ]

slPtrTailCallReturnToMLC :: SLExp -> V.Vector SLExp -> MonadMLCFunc ()
slPtrTailCallReturnToMLC funcptr args = do
  slPushToMLC funcptr
  stateWriteFromList [
        MLILoad   MLCRegW          MLCRegStackPtr
      , MLIConst  MLCRegX         (MLCValConst -1)
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
    ]

  V.forM_ args slPushToMLC
  --slPushToMLC exp
  stateWriteFromList [
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

      , MLIConst MLCRegY        (MLCValConst (-3 - V.length args))
      , MLIAdd   MLCRegY         MLCRegY MLCRegStackPtr  -- 引っ越し元開始位置: ↑で評価した引数の一番下

      , MLIConst MLCRegX        (MLCValConst (-1))
      , MLIAdd   MLCRegX         MLCRegStackPtr MLCRegX
      , MLILoad  MLCRegStackPtr  MLCRegX
      , MLIConst MLCRegX        (MLCValConst 1)
      , MLIAdd   MLCRegX         MLCRegStackPtr MLCRegX  -- 引っ越し先開始位置: スタックフレームの底
    ]

  stateWriteFromList ( (L.replicate (V.length args + 3) >>> join) [
        MLILoad  MLCRegZ        MLCRegY
      , MLIStore MLCRegZ        MLCRegStackPtr
      , MLIAdd   MLCRegZ        MLCRegZ        MLCRegX
      , MLIAdd   MLCRegStackPtr MLCRegStackPtr MLCRegX
    ]) -- 引っ越し

  stateWriteFromList [
        MLICopy MLCRegPC MLCRegW
    ]

slPushToMLC :: SLExp -> MonadMLCFunc ()
slPushToMLC expr = do
  case expr of
    SLEConst (SLVal v) ->
      stateWriteFromList [
            MLIConst  MLCRegX         (MLCValConst 1)
          , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
          , MLIConst  MLCRegY         (MLCValConst v)
          , MLIStore  MLCRegY          MLCRegStackPtr
        ]

    SLEFuncPtr fname ->
      stateWriteFromList [
            MLIConst  MLCRegX         (MLCValConst 1)
          , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
          , MLIConst  MLCRegStackPtr  (MLCValJumpDestFunc fname)
        ]

    SLELocal i ->
      stateWriteFromList [
            MLIConst  MLCRegX         (MLCValConst 1)
          , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
          , MLIConst  MLCRegX         (MLCValConst (1 + i))
          , MLIAdd    MLCRegX          MLCRegX MLCRegFramePtr -- i番目のローカル変数は、フレームポインタ指し先 + i + 1
          , MLILoad   MLCRegX          MLCRegX
          , MLIStore  MLCRegX          MLCRegStackPtr
        ]

    SLEArg i ->
      stateWriteFromList [
            MLIConst  MLCRegX         (MLCValConst 1)
          , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
          , MLIConst  MLCRegX         (MLCValConst (-1))
          , MLIAdd    MLCRegX          MLCRegX MLCRegFramePtr -- MLCRegX ← 旧スタックポインタ置き場のアドレス
          , MLILoad   MLCRegX          MLCRegX                -- MLCRegX ← 旧スタックポインタの指し先
          , MLIConst  MLCRegY         (MLCValConst (1 + i))
          , MLIAdd    MLCRegX          MLCRegX MLCRegY        -- i番目のローカル変数は、旧スタックポインタ指し先 + i + 1
          , MLILoad   MLCRegX          MLCRegX
          , MLIStore  MLCRegX          MLCRegStackPtr
        ]

    SLEPtr expr' -> do
      slPushToMLC expr'
      stateWriteFromList [
            MLILoad   MLCRegX          MLCRegStackPtr
          , MLILoad   MLCRegX          MLCRegX
          , MLIStore  MLCRegX          MLCRegStackPtr
        ] -- expr'の評価先を再利用

    SLEPushCall f args -> do
      stateWriteFromList [
            MLIConst  MLCRegX         (MLCValConst 1)
          , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
        ] -- 返り値のためにスタックを一個高くしておく
      case f of
        SLSolidFunc fname -> slSolidCallToMLC fname args
        SLFuncRef   fptr  -> slPtrCallToMLC   fptr  args

    SLEPrim1 prim exp1      -> slPrim1ToMLC prim exp1

    SLEPrim2 prim exp1 exp2 -> slPrim2ToMLC prim exp1 exp2


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

      gt = do
        stateWriteFromList [
              MLIConst  MLCRegX         (MLCValConst (-1))
            , MLILoad   MLCRegZ          MLCRegStackPtr
            , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
            , MLILoad   MLCRegY          MLCRegStackPtr
            , MLISub    MLCRegZ          MLCRegZ MLCRegY
            , MLIConst  MLCRegY         (MLCValConst (minBound :: Int))
            , MLIAnd    MLCRegY          MLCRegY MLCRegZ
            , MLIStore  MLCRegY          MLCRegStackPtr
          ]

      lt = do
        stateWriteFromList [
              MLIConst  MLCRegX         (MLCValConst (-1))
            , MLILoad   MLCRegZ          MLCRegStackPtr
            , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
            , MLILoad   MLCRegY          MLCRegStackPtr
            , MLISub    MLCRegZ          MLCRegY MLCRegZ
            , MLIConst  MLCRegY         (MLCValConst (minBound :: Int))
            , MLIAnd    MLCRegY          MLCRegY MLCRegZ
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
      SLPrim2Gt    -> slPushToMLC exp1 >> slPushToMLC exp2 >> gt                  
      SLPrim2Lt    -> slPushToMLC exp1 >> slPushToMLC exp2 >> lt                  
      SLPrim2Eq    -> slPushToMLC exp1 >> slPushToMLC exp2 >> prim2helper MLISub  

mlcInternalSetVarCnt :: Int -> MonadMLCFunc ()
mlcInternalSetVarCnt n =
  MonadMLCFunc (S.modify (\s -> s {mmlcfsVarCnt = n}))

mlcInternalShiftVarCnt :: Int -> MonadMLCFunc ()
mlcInternalShiftVarCnt n =
  MonadMLCFunc (S.modify (\s -> s {mmlcfsVarCnt = mmlcfsVarCnt s + n}))

mlcVarScope :: MonadMLCFunc () -> MonadMLCFunc ()
mlcVarScope v = 
  let 
    trashScopedVars :: MonadMLCFunc ()
    trashScopedVars = do
      varcnt <- MonadMLCFunc (gets mmlcfsVarCnt)
      when (varcnt > 0) $ do
        slPopnToMLC varcnt
        mlcInternalShiftVarCnt (-varcnt)
  in do
      varcnt <- MonadMLCFunc (gets mmlcfsVarCnt)
      mlcInternalSetVarCnt 0
      v
      trashScopedVars
      mlcInternalSetVarCnt varcnt

slPopnToMLC :: Int -> MonadMLCFunc ()
slPopnToMLC n = do
  stateWriteFromList [
        MLIConst  MLCRegX         (MLCValConst (-n))
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
    ]


slSubstVarToMLC :: Int -> SLExp -> MonadMLCFunc ()
slSubstVarToMLC var expr = do
  slPushToMLC expr
  stateWriteFromList [
        MLILoad   MLCRegY          MLCRegStackPtr
      , MLIConst  MLCRegX         (MLCValConst (-1))
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
      , MLIConst  MLCRegX         (MLCValConst (1 + var))
      , MLIAdd    MLCRegX          MLCRegX MLCRegFramePtr -- i番目のローカル変数は、フレームポインタ指し先 + i + 1
      , MLIStore  MLCRegY          MLCRegX
    ]

slSubstPtrToMLC :: SLExp -> SLExp -> MonadMLCFunc ()
slSubstPtrToMLC ptr expr = do
  slPushToMLC expr -- -> RegZ
  slPushToMLC ptr  -- -> RegY
  stateWriteFromList [
        MLILoad   MLCRegY          MLCRegStackPtr
      , MLIConst  MLCRegX         (MLCValConst (-1))
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
      , MLILoad   MLCRegZ          MLCRegStackPtr
      , MLIAdd    MLCRegStackPtr   MLCRegStackPtr MLCRegX
      , MLIStore  MLCRegZ          MLCRegY
    ]



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
    SLSInitVar expr   -> slPushToMLC expr >> mlcInternalShiftVarCnt 1
    SLSSubst ref expr ->
      case ref of
        SLRefPtr   ptr -> slSubstPtrToMLC ptr expr
        SLRefLocal var -> slSubstVarToMLC var expr
    SLSReturn expr  -> slReturnToMLC expr
    SLSTailCallReturn f args ->
      case f of
        SLSolidFunc fname -> slSolidTailCallReturnToMLC fname args
        SLFuncRef   fptr  -> slPtrTailCallReturnToMLC   fptr  args

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

data MLCError =
        MLCENoMain
      | MLCNoSuchFunc SLFuncName SLPos
      | MLCCannotSubstToArg SLPos
      deriving stock (Show, Eq)

compileSLFunc :: SLFuncBlock -> Either MLCError MLCFlagment
compileSLFunc slfunc =
  execMonadMLCFunc (slBlockToMLC (slfBlock slfunc) >> slReturnToMLC (SLEConst (SLVal 0))) (SLPos (slfName slfunc) [])


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
      initcode <- execMonadMLCFunc initializer (SLPos SLFuncMain [])
      (mlccode, mlcfuncmap) <- Control.Monad.foldM (\(code, funcmap) slfunc -> do
              code' <- compileSLFunc slfunc
              pure (code <> code' , M.insert (slfName slfunc) (VB.size code) funcmap)
            ) (initcode, M.empty) (fmain : (M.toList >>> fmap snd) (M.delete SLFuncMain program))

      V.mapM (\(inst :: MLCInst, pos) ->
          ( , pos) <$>
            bitraverse (pure <<< interpretReg) (\case
                  MLCValConst v            -> pure $ MLVal v
                  MLCValJumpDestLocal v    -> pure $ MLVal v
                  MLCValJumpDestFunc fname -> maybe (Left (MLCNoSuchFunc fname pos)) (MLVal >>> Right) (M.lookup fname mlcfuncmap)
                  MLCValJumpDestEnd        -> pure $ MLVal (VB.size mlccode)
                ) inst
            ) (VB.build (mlccode <> VB.singleton (MLINop, SLPos SLFuncMain [])))

