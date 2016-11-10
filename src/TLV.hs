-- An implementation of BER-TLV parsing according to EMV Book 3, Annex B
module TLV ( parseTag ) where

import Data.Bits
import Data.Char (intToDigit)
import Data.Word
import Numeric (showHex, showIntAtBase)
import Text.Printf (printf)

type Byte = Word8
type Bytes = [Byte]

-- | The tag field (T) consists of one or more consecutive bytes. It
-- indicates a class, a type, and a number. The tag field field of the data
-- object in this specification is coded on one or two bytes.
parseTag :: Bytes -> (Bytes, Bytes)
parseTag [] = ([], [])
parseTag (x:xs) | (x .&. 0x1F) == 0x1f = parseTagSubsequentBytes [x] xs
                | otherwise = ([x], xs)

parseTagSubsequentBytes :: Bytes -> Bytes -> (Bytes, Bytes)
parseTagSubsequentBytes acc [] = (reverse acc, [])
parseTagSubsequentBytes acc (x:xs)
  | (x .&. 0x80) == 0x80 = parseTagSubsequentBytes (x : acc) xs
  | otherwise            = (reverse (x : acc), xs)

byteAsHex :: Byte -> String
byteAsHex b = printf "%02s" (showHex b "")

byteAsBinary :: Byte -> String
byteAsBinary b = printf "%08s" (showIntAtBase 2 intToDigit b "")
