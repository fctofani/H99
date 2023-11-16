module PartOneSpec where

import PartOne
import Test.Hspec

spec :: Spec
spec = do
  describe "Part One" $ do
    it "exercise 1 test 1" $ do
      shouldBe (myLast [1, 2, 3, 4]) 4
    it "exercise 1 test 2" $ do
      shouldBe (myLast ['x', 'y', 'z']) 'z'