module LCMC where

import Data.Maybe
import Text.XML.Light

unText :: Content -> CData
unText (Text a) = a

tupleXmlOn :: String -> Element -> [(String, [Content])]
tupleXmlOn s = map (\ e -> (attrVal . head $ elAttribs e, elContent e)) .
  filterChildren ((== s) . qName . elName)

headOrDie :: String -> [x] -> x
headOrDie e = maybe (error e) id . listToMaybe


