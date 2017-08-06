module Data.RedBlackTree (
  RedBlack (Red, Black),
  RedBlackNode (RedBlackNode),
  RedBlackTree
) where

import Data.BinaryTree

data RedBlack = Red | Black deriving (Show, Eq, Ord)

data (Ord a) => RedBlackNode a = RedBlackNode {
  nodeColor :: RedBlack,
  content :: a
} deriving (Show)

instance (Ord a) => Ord (RedBlackNode a) where
  (RedBlackNode _ lcontent) <= (RedBlackNode _ rcontent) =
    lcontent <= rcontent

instance (Ord a) => Eq (RedBlackNode a) where
  (RedBlackNode _ lcontent) == (RedBlackNode _ rcontent) =
    lcontent == rcontent

type RedBlackTree a = BinaryTree (RedBlackNode a)
