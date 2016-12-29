-- An implementation of BER-TLV parsing according to EMV Book 3, Annex B
module TLV
  ( parseTLVs
  , byteAsBinary
  , byteAsHex
  ) where

import Data.Bits
import Data.Char (intToDigit)
import Data.Word
import Numeric (showHex, showIntAtBase)
import Text.Printf (printf)

type Byte = Word8
type Bytes = [Byte]

parseTLVs :: Bytes -> [(Bytes, Bytes)]
parseTLVs [] = []
parseTLVs xs = [(t, v)] ++ parseTLVs rest3
    where (t, rest)  = parseTag xs
          (l, rest2) = parseLength rest
          (v, rest3) = parseValue l rest2

-- | The tag field (T) consists of one or more consecutive bytes. It
-- indicates a class, a type, and a number.
parseTag :: Bytes -> (Bytes, Bytes)
parseTag [] = ([], [])
parseTag (x:xs) | (x .&. 0x1F) == 0x1f = parseTagSubsequentBytes [x] xs
                | otherwise = ([x], xs)

parseTagSubsequentBytes :: Bytes -> Bytes -> (Bytes, Bytes)
parseTagSubsequentBytes acc [] = (reverse acc, [])
parseTagSubsequentBytes acc (x:xs)
  | (x .&. 0x80) == 0x80 = parseTagSubsequentBytes (x : acc) xs
  | otherwise            = (reverse (x : acc), xs)

-- | The length field (L) consists of one or more consecutive bytes. It
-- indicates the length of the following field.
parseLength :: Bytes -> (Byte, Bytes)
parseLength [] = (0x00, [])
parseLength (x:xs)
  | (x .&. 0x80) == 0x80,
    let nSubsequentBytes = fromIntegral (x .&. 0x7f)
        (lengthBytes, rest) = splitAt nSubsequentBytes xs
        loopByte acc byte = (shift acc 8) .|. byte
  = (foldl loopByte 0x00 lengthBytes, rest)
  | otherwise            = (x .&. 0x7f, xs)

parseValue :: Byte -> Bytes -> (Bytes, Bytes)
parseValue _ [] = ([], [])
parseValue l xs = splitAt (fromIntegral l) xs

byteAsBinary :: Byte -> String
byteAsBinary b = printf "%08s" (showIntAtBase 2 intToDigit b "")

byteAsHex :: Byte -> String
byteAsHex b = printf "%02s" (showHex b "")
