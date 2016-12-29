import Test.Hspec

import TLV

main :: IO ()
main = hspec $ do
  describe "parseTLVs" $ do
    it "gracefully handles no bytes" $ do
      parseTLVs [] `shouldBe` []

    describe "a single TLV" $ do
      describe "by tag" $ do
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

      describe "by length" $ do
        it "can parse a length value of 0" $ do
          let input = [0x5A, 0x00]
              expected = [([0x5A], [])]
          parseTLVs input `shouldBe` expected

        it "can parse a 2-byte length" $ do
          let input = [0x5A, 0x81, 0x01, 0x01]
              expected = [([0x5A], [0x01])]
          parseTLVs input `shouldBe` expected

        it "can parse a 3-byte length" $ do
          let input = [0x5A, 0x82, 0x00, 0x01, 0x01]
              expected = [([0x5A], [0x01])]
          parseTLVs input `shouldBe` expected

      it "can parse a multi-byte value" $ do
        let input = [0x5A, 0x06, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06]
            expected = [([0x5A], [0x01, 0x02, 0x03, 0x04, 0x05, 0x06])]
        parseTLVs input `shouldBe` expected

    it "can parse multiple concatenated TLVs" $ do
      let input = [0x5A, 0x01, 0x01, 0x5B, 0x01, 0x01]
          expected = [([0x5A], [0x01]), ([0x5B], [0x01])]
      parseTLVs input `shouldBe` expected
