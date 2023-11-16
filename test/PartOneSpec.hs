module PartOneSpec where

import PartOne
import Test.Hspec

spec :: Spec
spec = do
  describe "Part One" $ do
    it "exercise 01 test A" $ do
      shouldBe (myLast [1, 2, 3, 4]) 4
    it "exercise 01 test B" $ do
      shouldBe (myLast ['x', 'y', 'z']) 'z'
    it "exercise 02 test A" $ do
      shouldBe (mySecLast [1, 2, 3, 4]) 3
    it "exercise 02 test B" $ do
      shouldBe (mySecLast ['a' .. 'z']) 'y'
    it "exercise 03 test A" $ do
      shouldBe (findElem [1, 2, 3] 2) 2
    it "exercise 03 test B" $ do
      shouldBe (findElem "haskell" 5) 'e'
    it "exercise 04 test A" $ do
      shouldBe (myLen [123, 456, 789]) 3
    it "exercise 04 test B" $ do
      shouldBe (myLen "Hello, world!") 13
    it "exercise 05 test A" $ do
      shouldBe (myReverse "A man, a plan, a canal, panama!") "!amanap ,lanac a ,nalp a ,nam A"
    it "exercise 05 test B" $ do
      shouldBe (myReverse [1, 2, 3, 4]) [4, 3, 2, 1]
    it "exercise 06 test A" $ do
      shouldBe (isPalindrome [1, 2, 3]) False
    it "exercise 06 test B" $ do
      shouldBe (isPalindrome "madamimadam") True
    it "exercise 06 test C" $ do
      shouldBe (isPalindrome [1, 2, 4, 8, 16, 8, 4, 2, 1]) True
