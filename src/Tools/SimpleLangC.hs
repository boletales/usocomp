{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Tools.SimpleLangC (
    compileToJSON
  , showAsmMap
  , sourceMapToJSON
  , listToJSON
  , objToJSON
  ) where

import MyPrelude

import SimpleLang.FromString
import SimpleLang.Tools
import MachineLang.FromSimpleLang
import MachineLang.Def
import MachineLang.Tools

import Data.Text as T
import Data.Map.Strict as M
import Data.Ord
import Data.Maybe
import qualified Data.List as L
import Data.Vector as V
import Text.Megaparsec.Pos

data ASMMapRow = ASMMapRow {
  asmmrSourceStart :: (Int, Int),
  asmmrSourceEnd :: (Int, Int),
  asmmrASMStart :: Int,
  asmmrASMEnd :: Int,
  asmmrSLPos :: SLPos
} deriving Eq

instance Ord ASMMapRow where
  compare (ASMMapRow _ _ asmStart1 asmEnd1 _) (ASMMapRow _ _ asmStart2 asmEnd2 _) =
    compare (Down asmStart1, asmEnd1) (Down asmStart2, asmEnd2)

sourceMapToJSON :: ASMMapRow -> Text
sourceMapToJSON (ASMMapRow (lstart, cstart) (lend, cend) asmStart asmEnd slPos) =
    objToJSON [
      ("sourcestart", objToJSON [("line", tshow lstart), ("col", tshow cstart)]),
      ("sourceend"  , objToJSON [("line", tshow lend  ), ("col", tshow cend)]),
      ("asmstart"   , tshow asmStart),
      ("asmend"     , tshow asmEnd),
      ("slpos"      , tshow $ tshow slPos)
    ]


objToJSON :: [(Text,Text)] -> Text
objToJSON obj = "{" <> T.intercalate ", " (fmap (\(k,v) -> "\"" <> k <> "\": " <> v) obj) <> "}"

listToJSON :: [Text] -> Text
listToJSON list = "[" <> T.intercalate ", " list <> "]"

sourcePosToTuple :: SourcePos -> (Int, Int)
sourcePosToTuple (SourcePos _ l c) = (unPos l, unPos c)

combineSourceMaps :: M.Map SLPos SourcePosRange -> V.Vector (MLInst, SLPos) -> [ASMMapRow]
combineSourceMaps sourcemap asm =
  let
    asmMap = generateSourceMap asm
    rows   = Data.Maybe.mapMaybe (\slpos -> (\(SourcePosRange srcFrom srcTo _) (asmFrom, asmTo) -> ASMMapRow (sourcePosToTuple srcFrom) (sourcePosToTuple srcTo) asmFrom asmTo slpos) <$> M.lookup slpos sourcemap <*> M.lookup slpos asmMap) (M.keys sourcemap)
  in L.sort rows

mlToText :: V.Vector (MLInst, SLPos) -> Text
mlToText ml =
  T.unlines ((\(inst, _) -> mliAbbrText inst) <$> V.toList ml)

compileToJSON :: Text -> Text
compileToJSON text =
  case textToSLParseResult text of
    Left err -> objToJSON [("error", tshow err)]
    Right (program, sourcemap) ->
      case compileSLProgram program of
        Left err -> objToJSON [("error", tshow err)]
        Right compiled ->
          objToJSON [
              ("src", tshow text),
              ("asm", tshow $ mlToText compiled),
              ("sourcemap", listToJSON $ sourceMapToJSON <$> combineSourceMaps sourcemap compiled)
            ]

showAsmMap :: M.Map SLPos (Int, Int) -> Text
showAsmMap m = T.unlines $ (\(p, s) -> pack (show s) <> "\t" <> prettyPrintSLPos p) <$> M.assocs m

