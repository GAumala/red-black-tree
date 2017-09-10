module Data.TestUtils (createTestTree, shouldBeColor) where

import Test.Hspec
import Data.BinaryTree
import Data.RedBlackTree

instance BinaryTreeNode Int where
  mergeNodes leftInt rightInt = leftInt

createTestTree :: (BinaryTreeNode a) => [a] -> RedBlackTree a
createTestTree = foldl insert emptyRedBlackTree

-- RedBlackNode's Eq instance is colorblind, so we need to test color separately
shouldBeColor :: (BinaryTreeNode a) => RedBlackTree a -> RedBlack -> Expectation
shouldBeColor Leaf expectedColor = Black `shouldBe` expectedColor
shouldBeColor (Branch _ (RedBlackNode color content) _) expectedColor =
  color `shouldBe` expectedColor
