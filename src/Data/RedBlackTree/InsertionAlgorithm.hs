module Data.RedBlackTree.InsertionAlgorithm (
  insert,
) where

import Data.RedBlackTree.BinaryTree
import Data.RedBlackTree.Internal

-- | inserts a new node to the tree, performing the necessary rotations to
-- guarantee that the red black properties are kept after the insertion.
insert :: RedBlackTree a -> a -> RedBlackTree a
insert tree newItem = Branch Leaf (RedBlackNode Red newItem) tree
