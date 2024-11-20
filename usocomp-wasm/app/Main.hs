{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import MyPrelude

import Data.Text as T
import Data.Text.IO as TIO
import Tools.SimpleLangC

main :: IO ()
main = do
  text <- TIO.getContents
  TIO.putStrLn $ compileToJSON text