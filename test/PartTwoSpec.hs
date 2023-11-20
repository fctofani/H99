module PartTwoSpec where

import PartTwo
import Test.Hspec

spec :: Spec
spec = do
  describe "Part Two" $ do
    it "exercise 11 test A" $ do
      shouldBe (encodeModified "aaaabccaadeeee") [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
    it "exercise 12 test A" $ do
      shouldBe (decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']) "aaaabccaadeeee"