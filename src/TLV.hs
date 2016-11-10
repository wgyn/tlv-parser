module TLV ( parseTag ) where

import Data.Bits
import Data.Word

parseTag :: [Word8] -> ([Word8], [Word8])
parseTag raw = ([head raw], tail raw)
