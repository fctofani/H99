module Part03Spec where

import Part03
import Test.Hspec

spec :: Spec
spec = do
  describe "Part Three" $ do
    it "exercise 21 test A" $ do
      shouldBe (insertAt 'X' "abcd" 2) "aXbcd"
    it "exercise 22 test A" $ do
      shouldBe (myRange 4 9) [4, 5, 6, 7, 8, 9]