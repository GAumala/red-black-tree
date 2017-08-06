module Data.BinaryTree (
  BinaryTree (Leaf, Branch),
  TreeBranch (TreeBranch),
  TreeDirection (LeftTree, RightTree),
  TreeDirections,
  TreeInsertResult (InsertOk, InsertNotYet, InsertFail),
  TreeZipper,

  appendLeftChild,
  appendRightChild,
  binaryTreeInsert,
  branchZipperInsert,
  getTreeRoot,
  goLeft,
  goUp,
  goRight) where

import Data.Maybe


-- A RedBlackTree is either a leaf (empty) or a node with 2 children, left and
-- right
data (Ord a) => BinaryTree a = Leaf
  | Branch (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord)

instance (Ord a, Show a) => Show (BinaryTree a) where
  show tree = prettyPrintTree tree 0
    where
      addSpaces num = replicate num ' '
      prettyPrintTree Leaf spaces = " Leaf"
      prettyPrintTree (Branch leftTree content rightTree) spaces =
        " " ++ show content ++ "\n" ++
        addSpaces (spaces + 2) ++ "L:" ++ prettyPrintTree leftTree (spaces + 2) ++ "\n" ++
        addSpaces (spaces + 2) ++ "R:" ++ prettyPrintTree rightTree (spaces + 2) ++ "\n"


-- Directions to reconstruct any parent of a focused node.
data (Ord a) => TreeDirection a = LeftTree a (BinaryTree a)
  | RightTree a (BinaryTree a) deriving (Show, Eq, Ord)

-- List of directions
type TreeDirections a = [TreeDirection a]

type TreeZipper a = (BinaryTree a, TreeDirections a)

data (Ord a) => TreeBranch a = TreeBranch (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord)

instance (Ord a, Show a) => Show (TreeBranch a) where
  show (TreeBranch leftChild content rightChild) =
    show (Branch leftChild content rightChild) 

type BranchZipper a = (TreeBranch a, TreeDirections a)

-- The result from inserting a node to the left or right of a tree can be:
-- (InsertOk insertedTree directionToNewTree) if there is a leaf at the
-- attempted insert position
-- (InsertNotYet obstructingTree directionToObstructingTree nodeToInsert) if there
-- already is a tree obstructing the desired position, we must go further down
-- InsertFail Fatal error, can't create direction to new node
data TreeInsertResult a =
  InsertOk (TreeBranch a) (TreeDirection a)
  | InsertNotYet (BinaryTree a) (TreeDirection a) a
  | InsertFail deriving (Show, Eq)

isLeftTreeDirection :: (Ord a) => TreeDirection a -> Bool
isLeftTreeDirection (LeftTree _ _) = True
isLeftTreeDirection (RightTree _ _) = False

getTreeContent :: (Ord a) => BinaryTree a -> Maybe a
getTreeContent (Branch _ content _) = Just content
getTreeContent Leaf = Nothing

-- Move the zipper down to the left child, returns nothing if focused node is
--  leaf
goLeft :: (Ord a) => TreeZipper a -> Maybe (TreeZipper a)
goLeft (Leaf, _) = Nothing
goLeft (Branch leftChild treeNode rightChild, xs) =
  Just (leftChild, LeftTree treeNode rightChild:xs)

-- Move the zipper down to the right child, returns nothing if focused node is
-- a leaf
goRight :: (Ord a) => TreeZipper a -> Maybe (TreeZipper a)
goRight (Leaf, _) = Nothing
goRight (Branch leftChild treeNode rightChild, xs) =
  Just (rightChild, RightTree treeNode leftChild:xs)

-- Move the zipper up to the parent, returns nothing directions list is empty
goUp :: (Ord a) => BranchZipper a -> Maybe (BranchZipper a)
goUp (_, []) = Nothing
goUp (TreeBranch leftChild content rightChild, direction:xs) =
  case direction of
    LeftTree parentContent rightSibling ->
      Just (TreeBranch currentTree parentContent rightSibling, xs)
    RightTree parentContent leftSibling ->
      Just (TreeBranch leftSibling parentContent currentTree, xs)
  where currentTree = Branch leftChild content rightChild

getTreeRoot :: (Ord a) => BranchZipper a -> BranchZipper a
getTreeRoot (branch, []) = (branch, [])
getTreeRoot zipper = case goUp zipper of
  Just prevZipper -> getTreeRoot prevZipper
  Nothing -> zipper

appendLeftChild :: (Ord a) => TreeBranch a -> a -> TreeInsertResult a
appendLeftChild (TreeBranch leftChild treeContent rightChild) nodeToAppend =
  if leftChild == Leaf then
    InsertOk newBranch newDirection
  else
    InsertNotYet leftChild newDirection nodeToAppend
  where newBranch = TreeBranch Leaf nodeToAppend Leaf
        newDirection = LeftTree treeContent rightChild

appendRightChild :: (Ord a) => TreeBranch a -> a -> TreeInsertResult a
appendRightChild (TreeBranch leftChild treeContent rightChild) nodeToAppend =
  if rightChild == Leaf then
    InsertOk newBranch newDirection
  else
    InsertNotYet rightChild newDirection nodeToAppend
  where newBranch = TreeBranch Leaf nodeToAppend Leaf
        newDirection = RightTree treeContent leftChild

insertOrGoDown :: (Ord a) => TreeDirections a -> TreeInsertResult a -> BranchZipper a
insertOrGoDown treeDirections (InsertOk newBranch newDirection) =
  (newBranch, newDirection:treeDirections)
insertOrGoDown treeDirections (InsertNotYet existingChild directionToChild childToInsert) =
  binaryTreeInsert (existingChild, directionToChild:treeDirections) childToInsert

branchZipperToTreeZipper :: (Ord a) => BranchZipper a -> TreeZipper a
branchZipperToTreeZipper (TreeBranch leftChild content rightChild, xs) =
  (Branch leftChild content rightChild, xs)

branchZipperInsert :: (Ord a) => BranchZipper a -> a -> BranchZipper a
branchZipperInsert (TreeBranch leftChild treeNode rightChild, xs) newNode =
  insertOrGoDown xs (appendFunction focusedBranch newNode)
  where
    focusedBranch = TreeBranch leftChild treeNode rightChild
    appendFunction = if newNode <= treeNode then appendLeftChild
                                            else appendRightChild

binaryTreeInsert :: (Ord a) => TreeZipper a -> a -> BranchZipper a
binaryTreeInsert (Leaf, xs) newNode = (TreeBranch Leaf newNode Leaf, xs)
binaryTreeInsert (Branch leftChild treeNode rightChild, xs) newNode =
  branchZipperInsert (TreeBranch leftChild treeNode rightChild, xs) newNode
