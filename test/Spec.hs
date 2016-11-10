{-# LANGUAGE ScopedTypeVariables #-}

import Data.Word

import Test.Hspec

import TLV

main :: IO ()
main = hspec $ do
  describe "parseTag" $ do
    it "can parse a tag from an array" $ do
      let input :: [Word8] = [0x9F, 0x34, 0x01, 0x01]
      let expected :: ([Word8], [Word8]) = ([0x9F, 0x34], [0x01, 0x01])
      parseTag input `shouldBe` expected
