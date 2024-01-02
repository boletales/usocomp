{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-|
  Module      : MachineLang.FromSimpleLang.Debugger
  Description : MachineLang.FromSimpleLangのデバッガ
-}
module MachineLang.FromSimpleLang.Debugger (
    debugMLC
  , runMLC
  , mlcResultText
  , mlcResultEither
  , runMLCinST
  , runMLCinST'
) where


import SimpleLang.Def
import MachineLang.FromSimpleLang
import SimpleLang.Tools
import MachineLang.Tools
import MachineLang.Machine

import Data.Vector as V
import Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Data.STRef (readSTRef)


tshow :: Show a => a -> Text
tshow = T.pack . show

{-
slTextRep :: SLProgram -> (V.Vector Text, M.Map SLPos Int)
slTextRep program =
-}

prettyPrintMemState :: V.Vector Int -> V.Vector Int -> Text
prettyPrintMemState regs mem =
  let regTexts =
          "Registers:\n" <>
          "  " <> "RegPC  : " <> tshow (regs V.! fromEnum (interpretReg MLCRegPC)) <> "\n" <>
          "  " <> "RegFPtr: " <> tshow (regs V.! fromEnum (interpretReg MLCRegFramePtr)) <> "\n" <>
          "  " <> "RegSPtr: " <> tshow (regs V.! fromEnum (interpretReg MLCRegStackPtr)) <> "\n" <>
          "  " <> "RegX   : " <> tshow (regs V.! fromEnum (interpretReg MLCRegX)) <> "\n" <>
          "  " <> "RegY   : " <> tshow (regs V.! fromEnum (interpretReg MLCRegY)) <> "\n" <>
          "  " <> "RegZ   : " <> tshow (regs V.! fromEnum (interpretReg MLCRegZ)) <> "\n" <>
          "  " <> "RegW   : " <> tshow (regs V.! fromEnum (interpretReg MLCRegW)) <> "\n"

      stackTopAddr    = regs V.! fromEnum (interpretReg MLCRegStackPtr)
      oldFramePtrAddr = regs V.! fromEnum (interpretReg MLCRegFramePtr)


      stackFrameTexts = fromMaybe "stack frame not found" (do

          let oldStackPtrAddr = oldFramePtrAddr - 1
          let returnAddrAddr  = oldFramePtrAddr - 2
          stackBottomAddr <- (+ 1) <$> (mem V.!? oldStackPtrAddr)

          locals <-
            Control.Monad.foldM (\t i -> do
                v <- mem V.!? i
                pure $ t <> "    " <> tshow i <> ": " <> tshow v <> "\n"
              ) "" (Prelude.reverse [oldFramePtrAddr + 1 .. stackTopAddr])

          oldfptr    <- mem V.!? oldFramePtrAddr
          oldsptr    <- mem V.!? oldStackPtrAddr
          returnaddr <- mem V.!? returnAddrAddr

          args <-
            Control.Monad.foldM (\t i -> do
                v <- mem V.!? i
                pure $ t <> "    " <> tshow i <> ": " <> tshow v <> "\n"
              ) "" (Prelude.reverse [stackBottomAddr .. returnAddrAddr - 1])

          pure $

            "Stack Frame:\n" <>
            "  " <> "=== stack top ===\n" <>
            "  " <> "local vars:\n" <>
            locals <>
            "  " <> "\n" <>
            "  " <> "frame:\n" <>
            "    " <> tshow oldFramePtrAddr <> " (old FPtr) : " <> tshow oldfptr    <> "\n" <>
            "    " <> tshow oldStackPtrAddr <> " (old SPtr) : " <> tshow oldsptr    <> "\n" <>
            "    " <> tshow returnAddrAddr  <> " (return)   : " <> tshow returnaddr <> "\n\n" <>
            "  " <> "args:\n" <>
            args <>
            "  " <> "=== frame bottom ===\n"
        )


  in regTexts <> "\n" <> stackFrameTexts



debugMLCDebugger :: MLDebugger SLPos (ExceptT MLRuntimeError IO)
debugMLCDebugger (inst, pos, _, mem, regs, time) = lift $ do
  TIO.putStrLn "==============================="
  TIO.putStrLn ("Tick: " <> tshow time)
  TIO.putStrLn ""
  TIO.putStrLn "Instruction:"
  TIO.putStrLn (mliAbbrText inst)
  TIO.putStrLn ""
  TIO.putStrLn "Position:"
  TIO.putStrLn (slPosAbbrText pos)
  TIO.putStrLn ""
  TIO.putStrLn (prettyPrintMemState regs mem)
  TIO.putStrLn "==============================="
  TIO.putStrLn ""
  TIO.putStrLn ""

silentDebugger :: forall m. Monad m => MLDebugger SLPos (ExceptT MLRuntimeError m)
silentDebugger _ = pure ()

tickDebugger :: MLDebugger SLPos (ExceptT MLRuntimeError IO)
tickDebugger (_, _, _, _, _, time) = lift $ do
  TIO.putStr ("\rtick: " <> tshow time)

{-| インタラクティブなデバッガを起動します -}

debugMLC :: SLProgram -> IO ()
debugMLC program =
  case compileSLProgram program of
    Left err -> TIO.putStrLn (tshow err)
    Right mlp -> do
      TIO.putStrLn "Compiled!"
      TIO.putStrLn "Running..."
      machine <- initMLMacine (MLConfig 100000) mlp

      let end e = do
            case e of
              MLRESuccess c -> TIO.putStrLn ("successfully terminated. code: " <> tshow c)
              _ -> TIO.putStrLn (tshow e)
            -- mem <- V.unsafeFreeze $ mlmemory machine
            -- let memmini = V.slice 0 20 mem
            -- TIO.putStrLn (tshow memmini)

          mainloop m = do
            result <- runExceptT $ runMLMachine1 m debugMLCDebugger
            case result of
              Left e -> end e
              Right _  -> do
                TIO.putStrLn "Enter: s (skip to next), q (quit), else (step)"
                str <- TIO.getLine
                case str of
                  "q" -> pure ()
                  "s" -> skipWithPosCond m (/=)
                  _   -> mainloop m

          silentloop m exitcond = do
            result <- runExceptT $ runMLMachine1 m silentDebugger
            case result of
              Left e -> end e
              Right _  -> do
                cond <- exitcond m
                if cond
                  then mainloop m
                  else silentloop m exitcond
          
          skipWithPosCond m poscond = 
            getInst m >>= \case
              Nothing       -> mainloop m
              Just (_, posnow) ->
                silentloop m (
                    fmap (maybe True (\(_, pos) -> poscond posnow pos)) . getInst
                  )

        in mainloop machine


runMLC :: SLProgram -> IO ()
runMLC program =
  case compileSLProgram program of
    Left err -> TIO.putStrLn (tshow err)
    Right mlp -> do
      TIO.putStrLn "Compiled!"
      TIO.putStrLn "Running..."
      machine <- initMLMacine (MLConfig 100000) mlp

      let end e = do
            TIO.putStrLn ""
            case e of
              MLRESuccess c -> TIO.putStrLn ("successfully terminated. code:" <> tshow c)
              _ -> TIO.putStrLn ("runtime error:" <> tshow e)

          mainloop m = do
            result <- runExceptT $ runMLMachine1 m tickDebugger
            case result of
              Left e -> end e
              Right _  ->  mainloop m
        in mainloop machine


runMLCinST :: SLProgram -> Text
runMLCinST program =
  case compileSLProgram program of
    Left err -> "compile error: " <> (tshow err)
    Right mlp -> runST ((do
      machine <- primToST (initMLMacine (MLConfig 100000) mlp  :: ST s (MLMachine SLPos s))
      let end :: MLRuntimeError -> ST s Text
          end e = do
            time <- readSTRef $ mltime machine
            case e of
              MLRESuccess c -> pure $ "(" <> tshow time <> " ticks) successfully terminated. code:" <> tshow c
              _ -> pure $ "(" <> tshow time <> " ticks) runtime error:" <> tshow e

          mainloop :: MLMachine SLPos s -> ST s Text
          mainloop m = do
            result <- primToST $ (runExceptT $ runMLMachine1 m silentDebugger :: ST s (Either MLRuntimeError ()))
            case result of
              Left e  -> end e
              Right _ -> mainloop m
        in mainloop machine) :: forall s. ST s Text)

-- | returns Either error (code, time)
runMLCinST' :: SLProgram -> Either Text (Int, Int)
runMLCinST' program =
  case compileSLProgram program of
    Left err -> Left $ "compile error: " <> tshow err
    Right mlp -> runST ((do
      machine <- primToST (initMLMacine (MLConfig 100000) mlp  :: ST s (MLMachine SLPos s))
      let end :: MLRuntimeError -> ST s (Either Text (Int, Int))
          end e = do
            time <- readSTRef $ mltime machine
            case e of
              MLRESuccess c -> pure $ Right (c, time)
              _ -> pure $ Left $ "(" <> tshow time <> " ticks) runtime error:" <> tshow e

          mainloop :: MLMachine SLPos s -> ST s (Either Text (Int, Int))
          mainloop m = do
            result <- primToST (runExceptT $ runMLMachine1 m silentDebugger :: ST s (Either MLRuntimeError ()))
            case result of
              Left e  -> end e
              Right _ -> mainloop m
        in mainloop machine) :: forall s. ST s (Either Text (Int, Int)))

{-| 人間可読なコンパイル成果物を吐きます -}
mlcResultText :: SLProgram -> Text
mlcResultText program = either id id $ mlcResultEither program

mlcResultEither :: SLProgram -> Either Text Text
mlcResultEither program =
  case compileSLProgram program of
    Left err -> Left $ tshow err
    Right mlp ->
        Right $ V.foldl (\ac (inst, pos) ->
            let instText = mliAbbrText inst
                posText  = slPosAbbrText pos

            in  ac <> "\n" <>
                 instText <> T.replicate (40 - T.length instText) " " <> "#" <> posText
          ) "" mlp