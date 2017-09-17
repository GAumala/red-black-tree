module Data.TestUtils (
  createTestTree,
  ListNode (ListNode),
  nodeValuesList
  ) where

import Test.Hspec
import Data.RedBlackTree.BinaryTree
import Data.RedBlackTree.Internal
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
