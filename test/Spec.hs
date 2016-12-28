import Test.Hspec

import TLV

main :: IO ()
main = hspec $ do
  describe "parseTLVs" $ do
    it "gracefully handles no bytes" $ do
      parseTLVs [] `shouldBe` []

    describe "a single TLV" $ do
      it "can parse a 1-byte long tag" $ do
        let input = [0x5A, 0x01, 0x01]
            expected = [([0x5A], [0x01])]
        parseTLVs input `shouldBe` expected

      it "can parse a 2-byte long tag" $ do
        let input = [0x9F, 0x34, 0x01, 0x01]
            expected = [([0x9F, 0x34], [0x01])]
        parseTLVs input `shouldBe` expected

      it "can parse a 3-byte long tag" $ do
        let input = [0x9F, 0x9F, 0x34, 0x01, 0x01]
            expected = [([0x9F, 0x9F, 0x34], [0x01])]
        parseTLVs input `shouldBe` expected

    it "can parse multiple concatenated TLVs" $ do
      let input = [0x5A, 0x01, 0x01, 0x5B, 0x01, 0x01]
          expected = [([0x5A], [0x01]), ([0x5B], [0x01])]
      parseTLVs input `shouldBe` expected
