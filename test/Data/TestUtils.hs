module Data.TestUtils (
  createTestTree,
  newTree
) where

import Data.RedBlackTree.BinaryTree
import Data.List (foldl')

createTestTree :: (Ord a) => [a] -> RedBlackTree a
createTestTree = foldl' redBlackTreeInsert Leaf

newTree :: RBColor -> a -> RedBlackTree a
newTree c v = Branch Leaf (RBNode c v) Leaf
