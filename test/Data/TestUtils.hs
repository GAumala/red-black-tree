module Data.TestUtils (
  createTestTree,
  shouldBeColor,
  ListNode (ListNode),
  nodeValuesList
  ) where

import Test.Hspec
import Data.BinaryTree
import Data.RedBlackTree
import Data.List (foldl')

instance BinaryTreeNode Int where
  mergeNodes leftInt rightInt = leftInt

data ListNode = ListNode {
  nodeId :: Int,
  nodeValuesList :: [String]
} deriving Show

instance Eq ListNode where
  (==) leftNode rightNode = nodeId leftNode == nodeId rightNode

instance Ord ListNode where
  (<=) leftNode rightNode = nodeId leftNode <= nodeId rightNode

instance BinaryTreeNode ListNode where
  mergeNodes (ListNode id leftValues) (ListNode _ rightValues) =
    ListNode id (leftValues ++ rightValues)


createTestTree :: (BinaryTreeNode a) => [a] -> RedBlackTree a
createTestTree = foldl' insert emptyRedBlackTree

-- RedBlackNode's Eq instance is colorblind, so we need to test color separately
shouldBeColor :: (BinaryTreeNode a) => RedBlackTree a -> RedBlack -> Expectation
shouldBeColor Leaf expectedColor = Black `shouldBe` expectedColor
shouldBeColor (Branch _ (RedBlackNode color content) _) expectedColor =
  color `shouldBe` expectedColor
