module Main where

import Control.Applicative
import FUtil
import System.Directory
import System.FilePath
import Text.XML.Light
import qualified Data.Map as M

unText (Text a) = a

indexSentences :: M.Map String Int -> String -> IO ()
indexSentences charList filename = do
  fs <-
    filterChildren ((== "file") . qName . elName) .
    head . filterChildren ((== "text") . qName . elName) .
    head . filter ((== "LCMC") . qName . elName) . onlyElems . parseXML <$>
    readFile filename
  let
    sents = map (\ f -> (attrVal . head $ elAttribs f,
      --concatMap (filterChildren ((== "s") . qName . elName)) .

      map (map (maximum .
        map (maybe 20000 id . flip M.lookup charList . fromUtf8 . cdData .
          unText) .
      elContent) .
      onlyElems . elContent) .
      filter ((== "s") . qName . elName) . onlyElems .
      concatMap elContent . filter ((== "p") . qName . elName) .
      onlyElems $ elContent f)) fs
  print $ take 800 $ show charList
  print $ take 800 $ show sents

readCharList :: String -> IO (M.Map String Int)
readCharList x = M.fromList . flip zip [1..] .
  map (fromUtf8 . takeWhile (/= '|')) . filter ((/= '#') . head) .
  filter (not . null) . lines <$> readFile x

main :: IO ()
main = do
  corpDataDir <- return "corpus/2474/Lcmc/data/character"
  charList <- readCharList =<< (</> "l/l/z/z.mem") <$> getHomeDirectory
  filenames <- take 1 . map (corpDataDir </>) <$>
    getDirectoryContents corpDataDir
  mapM_ (indexSentences charList) filenames

