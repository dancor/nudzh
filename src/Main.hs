module Main where

import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe
import FUtil
import LCMC
import System.Directory
import System.FilePath
import Text.XML.Light
import qualified Data.Map as M

type CharList = M.Map String Int

type Res = (String, ((Int, Int), String))

maxHard :: Int
maxHard = 20000

maxMb :: Ord a => [a] -> Maybe a
maxMb l = if null l then Nothing else Just $ maximum l

processSentence :: CharList -> String -> String -> [Content] -> [Res]
processSentence charList f s cs = if len < 7 then []
  else [(wd, ((hardness, len), f ++ "/" ++ s)) | (wd, _) <- wdsVals]
  where
  len = length wdsVals
  wdsVals :: [(String, Int)]
  wdsVals = catMaybes . map (\ x -> (,) x <$> M.lookup x charList) . nub .
    concatMap (map (fromUtf8 . cdData . unText) . elContent) $ onlyElems cs
  hardness :: Int
  hardness = maybe maxHard id . maxMb $ map snd wdsVals

processFile :: CharList -> String -> [Content] -> [Res]
processFile charList f = concatMap (concatMap
  (uncurry $ processSentence charList f) . tupleXmlOn "s") . onlyElems

indexSentences :: CharList -> String -> String -> [Res]
indexSentences charList fN =
  concatMap (uncurry $ processFile charList) . tupleXmlOn "file" .
  headOrDie fN . filterChildren ((== "text") . qName . elName) .
  headOrDie fN . filter ((== "LCMC") . qName . elName) . onlyElems .
  parseXML

readCharList :: String -> CharList
readCharList = M.fromList . flip zip [1..] .
  map (fromUtf8 . takeWhile (/= '|')) . filter ((/= '#') . head) .
  filter (not . null) . lines

showRes :: Res -> String
showRes (w, ((h, l), s)) = toUtf8 w ++ " " ++ show h ++ " " ++ show l ++
  " " ++ s

main :: IO ()
main = do
  let corpDataDir = "corpus/2474/Lcmc/data/character"
  charList <- readCharList <$> readFile "corpus/z.mem"
  filenames <- map (corpDataDir </>) .
    filter ((/= '.') . head) <$> getDirectoryContents corpDataDir
  res <- sort . concat <$>
    mapM (\ f -> indexSentences charList f <$> readFile f) filenames
  putStr . unlines $ map showRes res
