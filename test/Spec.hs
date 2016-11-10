import Test.Hspec

import TLV

main :: IO ()
main = hspec $ do
  describe "parseTag" $ do
    it "can parse a 1-byte long tag" $ do
      let input = [0x57, 0x01, 0x01]
          expected = ([0x57], [0x01, 0x01])
      parseTag input `shouldBe` expected

    it "can parse a 2-byte long tag" $ do
      let input = [0x9F, 0x34, 0x01, 0x01]
          expected = ([0x9F, 0x34], [0x01, 0x01])
      parseTag input `shouldBe` expected

    it "can parse a 3-byte long tag" $ do
      let input = [0x9F, 0x9F, 0x34, 0x01, 0x01]
          expected = ([0x9F, 0x9F, 0x34], [0x01, 0x01])
      parseTag input `shouldBe` expected

    it "gracefully handles no bytes" $ do
      parseTag [] `shouldBe` ([], [])
