module Data.RedBlackTreeSpec (spec) where

import Test.Hspec
import Data.BinaryTree
import Data.RedBlackTree

-- RedBlackNode's Eq instance is colorblind, so we need to test color separately
shouldBeColor :: (Ord a) => RedBlackTree a -> RedBlack -> Expectation
shouldBeColor (Branch _ (RedBlackNode color content) _) expectedColor =
  color `shouldBe` expectedColor

spec :: Spec
spec =
  describe "insert" $
    it "if node is inserted at root, it is painted black" $ do
      let tree = Leaf :: RedBlackTree Int
      let newItem = 1
      let expectedTree = Branch Leaf (RedBlackNode Black 1) Leaf
      let modifiedTree = insert tree newItem

      modifiedTree `shouldBe` expectedTree
      modifiedTree `shouldBeColor` Black
