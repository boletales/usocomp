{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module SimpleLangC where

import MyPrelude

import SimpleLang.Def
import SimpleLang.FromString
import MachineLang.FromSimpleLang.Debugger
import Data.Text as T
import Data.Text.IO as TIO
import System.Environment
import Control.Monad.Except
import MachineLang.FromSimpleLang.Test
import Data.Set as S
import System.IO 
import Control.Monad
import Control.Monad.IO.Class
import Tools.SimpleLangC
import qualified Data.List as L
import Data.Maybe
import System.Directory

data SLCOption = SLCOptionDebug deriving (Eq, Ord, Show)

main :: IO ()
main = do
  args <- getArgs
  hSetBuffering stdout NoBuffering
  let parsed = L.foldl (\(filename, opts) arg -> case arg of
          "--debug" -> (filename, S.insert SLCOptionDebug opts)
          "-d" -> (filename, S.insert SLCOptionDebug opts)
          f -> (Just f, opts)
        ) (Nothing, S.empty) args
  
  case parsed of
    (Just filename, opts) -> do
      result <- runExceptT $ main' opts filename
      case result of
        Left err -> TIO.putStrLn err
        Right _  -> pure ()
    _ -> TIO.putStrLn "Usage: SimpleLangC [--debug | -d] <filename>"

main' :: Set SLCOption -> String -> ExceptT Text IO ()
main' opts filename = do
  text <- liftIO $ TIO.readFile filename
  slprogram <- except $ textToSLProgram text
  case mlcResultEither slprogram of
    Left err -> throwError err
    Right result -> do
      liftIO $ TIO.writeFile (filename <> ".mlang") result
      liftIO $ 
        if S.member SLCOptionDebug opts
          then debugMLC slprogram
          else runMLCFast slprogram
        


except :: MonadError e m => Either e a -> m a
except = either throwError pure


genExampleFiles :: IO ()
genExampleFiles = 
  forM_ mlctTests $ \test -> do
    let program = mlctTest test
    let title   = mlctName test
    TIO.writeFile ("./examples/" <> T.unpack title <> ".slang") (prettyPrintSLProgram program)
    TIO.writeFile ("./examples/" <> T.unpack title <> ".slang.mlang") (mlcResultText program)


folderToJSON :: IO ()
folderToJSON = do
  files <- listDirectory "./examples"
  result <- Control.Monad.forM files $ \file -> do
    -- check if file is a .slang file
    if (".slang" `L.isSuffixOf` file) then do
      text <- TIO.readFile ("./examples/" <> file)
      pure $ Just $ objToJSON [
            ("name", tshow (pack file))
          , ("result", compileToJSON text)
        ] <> "\n"
    else pure Nothing
  TIO.writeFile "./docs/results.json" $ listToJSON $ Data.Maybe.catMaybes result