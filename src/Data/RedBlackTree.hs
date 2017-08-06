module Data.RedBlackTree (
  NodeColor (Red, Black),
  RedBlackTree (Leaf, Branch),
  TreeDirection (LeftTree, RightTree),
  TreeDirections,
  TreeInsertResult (InsertOk, InsertNotYet, InsertFail),
  TreeNode (TreeNode),
  RBZipper,

  appendLeftChild,
  appendRightChild,
  binaryTreeInsert,
  goLeft,
  goToTop,
  goUp,
  goRight) where

import Data.Maybe

-- Possible colors for a node in the tree
data NodeColor = Red | Black deriving (Show, Eq, Ord)

data (Ord a) => TreeNode a = TreeNode {
  nodeColor :: NodeColor,
  content :: a
} deriving (Show)

instance (Ord a) => Ord (TreeNode a) where
  (TreeNode _ lcontent) <= (TreeNode _ rcontent) =
    lcontent <= rcontent

instance (Ord a) => Eq (TreeNode a) where
  (TreeNode _ lcontent) == (TreeNode _ rcontent) =
    lcontent == rcontent

-- A RedBlackTree is either a leaf (empty) or a node with 2 children, left and
-- right
data (Ord a) => RedBlackTree a = Leaf
  | Branch (RedBlackTree a) (TreeNode a) (RedBlackTree a) deriving (Eq, Ord)

instance (Ord a, Show a) => Show (RedBlackTree a) where
  show tree = prettyPrintTree tree 0
    where
      addSpaces num = replicate num ' '
      prettyPrintTree Leaf spaces = " Leaf"
      prettyPrintTree (Branch leftTree content rightTree) spaces =
        " " ++ show content ++ "\n" ++
        addSpaces (spaces + 2) ++ "L:" ++ prettyPrintTree leftTree (spaces + 2) ++ "\n" ++
        addSpaces (spaces + 2) ++ "R:" ++ prettyPrintTree rightTree (spaces + 2) ++ "\n"


-- Directions to reconstruct any parent of a focused node.
data (Ord a) => TreeDirection a = LeftTree (TreeNode a) (RedBlackTree a)
  | RightTree (TreeNode a) (RedBlackTree a) deriving (Show, Eq, Ord)

-- List of directions
type TreeDirections a = [TreeDirection a]

type RBZipper a = (RedBlackTree a, TreeDirections a)

type BranchTuple a = (RedBlackTree a, TreeNode a, RedBlackTree a)

-- The result from inserting a node to the left or right of a tree can be:
-- (InsertOk insertedTree directionToNewTree) if there is a leaf at the
-- attempted insert position
-- (InsertNotYet obstructingTree directionToObstructingTree nodeToInsert) if there
-- already is a tree obstructing the desired position, we must go further down
-- InsertFail Fatal error, can't create direction to new node
data TreeInsertResult a =
  InsertOk (BranchTuple a) (TreeDirection a)
  | InsertNotYet (RedBlackTree a) (TreeDirection a) (TreeNode a)
  | InsertFail deriving (Show, Eq)

isLeftTreeDirection :: (Ord a) => TreeDirection a -> Bool
isLeftTreeDirection (LeftTree _ _) = True
isLeftTreeDirection (RightTree _ _) = False

-- Move the zipper down to the left child, returns nothing if focused node is
--  leaf
goLeft :: (Ord a) => RBZipper a -> Maybe (RBZipper a)
goLeft (Leaf, _) = Nothing
goLeft (Branch leftChild treeNode rightChild, xs) =
  Just (leftChild, LeftTree treeNode rightChild:xs)

-- Move the zipper down to the right child, returns nothing if focused node is
-- a leaf
goRight :: (Ord a) => RBZipper a -> Maybe (RBZipper a)
goRight (Leaf, _) = Nothing
goRight (Branch leftChild treeNode rightChild, xs) =
  Just (rightChild, RightTree treeNode leftChild:xs)

-- Move the zipper up to the parent, returns nothing directions list is empty
goUp :: (Ord a) => RBZipper a -> Maybe (RBZipper a)
goUp (_, []) = Nothing
goUp (tree, LeftTree treeNode rightChild:xs) =
  Just (Branch tree treeNode rightChild, xs)
goUp (tree, RightTree treeNode leftChild:xs) =
  Just (Branch leftChild treeNode tree, xs)

goToTop :: (Ord a) => RBZipper a -> RBZipper a
goToTop (tree, []) = (tree, [])
goToTop zipper = case goUp zipper of
  Just prevZipper -> goToTop prevZipper
  Nothing -> zipper

appendLeftChild :: (Ord a) => BranchTuple a -> TreeNode a -> TreeInsertResult a
appendLeftChild (leftChild, treeContent, rightChild) childToAppend =
  if leftChild == Leaf then
    InsertOk newBranchTuple newDirection
  else
    InsertNotYet leftChild newDirection childToAppend
  where newChildTree = Branch Leaf childToAppend Leaf
        newBranchTuple = (newChildTree, treeContent, rightChild)
        newDirection = LeftTree treeContent rightChild

appendRightChild :: (Ord a) => BranchTuple a -> TreeNode a -> TreeInsertResult a
appendRightChild (leftChild, treeContent, rightChild) childToAppend =
  if rightChild == Leaf then
    InsertOk newBranchTuple newDirection
  else
    InsertNotYet rightChild newDirection childToAppend
  where newChildTree = Branch Leaf childToAppend Leaf
        newBranchTuple = (leftChild, treeContent, newChildTree)
        newDirection = RightTree treeContent leftChild

insertOrGoDown :: (Ord a) => TreeDirections a -> TreeInsertResult a -> RBZipper a
insertOrGoDown treeDirections (InsertOk (leftChild, nodeContent, rightChild) directionToNewTree) =
  (insertedTree, directionToNewTree:treeDirections)
  where insertedTree = if isLeftTreeDirection directionToNewTree
                        then leftChild else rightChild
insertOrGoDown treeDirections (InsertNotYet existingChild directionToChild childToInsert) =
  binaryTreeInsert (existingChild, directionToChild:treeDirections) childToInsert

binaryTreeInsert :: (Ord a) => RBZipper a -> TreeNode a -> RBZipper a
binaryTreeInsert (Leaf, xs) newNode = (Branch Leaf newNode Leaf, xs)
binaryTreeInsert (Branch leftChild treeNode rightChild, xs) newNode =
  insertOrGoDown xs (appendFunction focusedTreeTuple newNode)
  where
    focusedTreeTuple = (leftChild, treeNode, rightChild)
    appendFunction = if newNode <= treeNode then appendLeftChild
                                            else appendRightChild
