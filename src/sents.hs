import Control.Applicative
import Control.Arrow
import Data.Maybe
import FUtil
import LCMC
import System.Environment
import System.FilePath
import Text.XML.Light

grabSent :: (String, String) -> String -> String
grabSent (f, s) =
  concatMap (\ (t, z) -> t ++ z) .
  map (\ x ->
    (attrVal . head $ elAttribs x, cdData . unText . head $ elContent x)) .
  onlyElems . snd . head . filter ((== s) . fst) . concatMap (tupleXmlOn "s") .
  onlyElems . snd . head . filter ((== f) . fst) . tupleXmlOn "file" .
  headOrDie f . filterChildren ((== "text") . qName . elName) .
  headOrDie f . filter ((== "LCMC") . qName . elName) .
  onlyElems . parseXML

main :: IO ()
main = do
  [wd] <- getArgs
  let
    corpDataDir = "corpus/2474/Lcmc/data/character"
    indexFN = "index"
    processIndexLine (_:h:n:ind:[]) =
      (second tail $ break (== '/') ind, (h, n))
  sentInds <- take 10 . map processIndexLine .
    filter ((== wd) . head) . map words . lines <$> readFile indexFN
  sents <- mapM (\ (x@(c:_, _), _) -> grabSent x <$>
    readFile (corpDataDir </> "LCMC_" ++ [c] ++ ".XML")) sentInds
  putStr . unlines $
    zipWith (\ ((a, b), (h, n)) c -> a ++ "/" ++ b ++ " " ++ h ++ " " ++ n ++
      "\n" ++ filter (`notElem` ['a'..'z']) c ++
      "\n" ++ c)
    sentInds sents

