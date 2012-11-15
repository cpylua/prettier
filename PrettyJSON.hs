module PrettyJSON
       (
         renderJValue
       ) where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), (</>), char, line, empty, double, fsep, fcat, hcat, punctuate, text, nest, pretty)
  
renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber n) = double n
renderJValue (JString s) = escape s
renderJValue (JArray a) = series '[' ']' renderJValue a
renderJValue (JObject o) = series '{' '}' field o
  where field (k,v) = escape k <> text ": " <> renderJValue v
        
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close f xs = encloseIndent open close 2 $ (fsep . punctuate (char ',') $ init fields) <> last fields
  where fields = map f xs

escape :: String -> Doc
escape = enclose '"' '"' . hcat . map escapeChar

escapeChar :: Char -> Doc
escapeChar c = case lookup c escapeRules of
  Just s -> text s
  Nothing | mustEscape c -> hexEscape c
          | otherwise -> char c
    where
      mustEscape c = c < ' ' || c == '\x7f' || c > '\xff' 
  where
    escapeRules = zipWith (\a b -> (a, ['\\',b])) "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
      
hexEscape :: Char -> Doc
hexEscape c
  | d < 0x10000 = smallHex d
  | otherwise = astral (d - 0x10000)
  where
    d = ord c
    smallHex x = text "\\u" <> text (replicate (4 - length h) '0') <> text h
      where h = showHex x ""
    astral n = smallHex (a+0xd800) <> smallHex (b+0xdc00)
      where a = (n `shiftR` 10) .&. 0x3ff
            b = n .&. 0x3ff

encloseIndent :: Char -> Char -> Int -> Doc -> Doc
encloseIndent open close indent x = hcat [char open,
                                          nest indent (line <> x),
                                          line,
                                          char close]

enclose :: Char -> Char -> Doc -> Doc
enclose open close x = char open <> x <> char close

