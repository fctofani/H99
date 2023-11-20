module PartTwoSpec where

import PartTwo
import Test.Hspec

spec :: Spec
spec = do
  describe "Part Two" $ do
    it "exercise 11 Test A" $ do
      shouldBe (encodeModified "aaaabccaadeeee") [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']