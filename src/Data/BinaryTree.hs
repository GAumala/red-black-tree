module Data.BinaryTree (
  BinaryTreeNode (mergeNodes),
  BinaryTree (Leaf, Branch),
  BranchType (LeftBranch, RightBranch),
  BranchZipper,
  TreeBranch (TreeBranch),
  TreeDirection (TreeDirection),
  TreeDirections,
  TreeFamily (IsRoot, HasParent, HasGrandparent),
  TreeInsertResult (InsertOk, InsertNotYet, InsertFail),
  TreeZipper,

  appendLeftChild,
  appendRightChild,
  binaryTreeFind,
  branch2Tree,
  branchZipperInsert,
  getTreeRoot,
  goLeft,
  goUp,
  goRight,
  reconstructAncestor,
  treeZipperInsert) where

import Data.Maybe


-- Only types that are members of @BinaryTreeNode@ can be inserted into a
-- @BinaryTree@. The purpose of the class is to provide a method to merge nodes
-- with equal values since inserting different nodes with equal values can
-- corrupt the tree.
class (Ord a) => BinaryTreeNode a where
  -- The @BinaryTree@ will call this function when it tries to insert a value
  -- that already exists in the tree. The first argument is guaranteed to be the
  -- one that is already in the tree, while the second argument is the node that
  -- the tree is trying to insert. Since the two nodes can't exist in the same tree
  -- The result should be a 'merged' node that will be inserted instead of the
  -- other two.
  mergeNodes :: a -> a -> a

-- A BinaryTree is either a leaf (empty) or a @BinaryTreeNode@ with 2
-- @BinaryTree@ children, left and right
data (BinaryTreeNode a) => BinaryTree a = Leaf
  | Branch (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord)

instance (BinaryTreeNode a, Show a) => Show (BinaryTree a) where
  show tree = prettyPrintTree tree 0
    where
      addSpaces num = replicate num ' '
      prettyPrintTree Leaf spaces = " Leaf"
      prettyPrintTree (Branch leftTree content rightTree) spaces =
        " " ++ show content ++ "\n" ++
        addSpaces (spaces + 2) ++ "L:" ++ prettyPrintTree leftTree (spaces + 2) ++ "\n" ++
        addSpaces (spaces + 2) ++ "R:" ++ prettyPrintTree rightTree (spaces + 2) ++ "\n"


-- A BinaryTree can only have two types of branches: Left or Right
data BranchType = LeftBranch | RightBranch deriving (Show, Eq, Ord)

-- Minimum necessary to reconstruct the parent of any focused node. First argument
-- is the @BranchType@ of the focused node relative to the parent. Second argument
-- is the parent's node. The third argument is the sibling tree of the focused
-- node.
data (BinaryTreeNode a) => TreeDirection a = TreeDirection BranchType a (BinaryTree a)
  deriving (Show, Eq, Ord)

-- List of @TreeDirection@
type TreeDirections a = [TreeDirection a]

-- A @BinaryTree@ zipper. the first value of the tuple is the focused @BinaryTree@,
-- while the second argument is the list of directions used to move up to the
-- parent and other ancestors.
type TreeZipper a = (BinaryTree a, TreeDirections a)

-- Holds the data of a @BinaryTree@ created with the @Branch@ constructor. Useful
-- type when you want to guarantee that the element is not a @Leaf@
data (BinaryTreeNode a) => TreeBranch a = TreeBranch (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord)

instance (BinaryTreeNode a, Show a) => Show (TreeBranch a) where
  show (TreeBranch leftChild content rightChild) =
    show (Branch leftChild content rightChild)

-- A @TreeBranch@ zipper. It is identical to @TreeZipper@ except for the fact
-- that @Leaf@ values are not allowed in the zipper.
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

-- This type is used to answer questions about the current size of the tree and
-- how deep can we go up in the tree. This is only used internally for the insert
-- algorithm. Does it only have one element (IsRoot),
-- Does it have less than 3 elements? HasParent or is it way bigger? (HasGrandparent)
-- The @IsRoot@ constructor has only one parameter: the only existing node as a @TreeBranch@.
-- The @HasParent@ constructor has two parameters, a @TreeBranch@ and a
-- @TreeDirection@ that can be used to get a reference to the parent.
-- The @HasGrandparent@ constructor has 4 parameters, the 4th parameter is a
-- @TreeBranch@. 3rd parameter is a @TreeDirection@ to reconstruct the parent.
-- 2nd paramenter is a @TreeDirection@ to reconstruct the grandparent. the first
-- argument is a list of @TreeDirection@ to reconstruct the rest of the ancestors.
data TreeFamily a =
  IsRoot (TreeBranch a) |
  HasParent (TreeDirection a) (TreeBranch a) |
  HasGrandparent (TreeDirections a) (TreeDirection a) (TreeDirection a) (TreeBranch a)

isLeftTreeDirection :: (BinaryTreeNode a) => TreeDirection a -> Bool
isLeftTreeDirection (TreeDirection branchType _ _) = branchType == LeftBranch

getTreeContent :: (BinaryTreeNode a) => BinaryTree a -> Maybe a
getTreeContent (Branch _ content _) = Just content
getTreeContent Leaf = Nothing

branch2Tree :: (BinaryTreeNode a) => TreeBranch a -> BinaryTree a
branch2Tree (TreeBranch leftChild content rightChild) =
  Branch leftChild content rightChild

-- Move the zipper down to the left child, returns nothing if focused node is
--  leaf
goLeft :: (BinaryTreeNode a) => BranchZipper a -> TreeZipper a
goLeft (TreeBranch leftChild treeNode rightChild, xs) =
  (leftChild, TreeDirection LeftBranch treeNode rightChild:xs)

-- Move the zipper down to the right child, returns nothing if focused node is
-- a leaf
goRight :: (BinaryTreeNode a) => BranchZipper a -> TreeZipper a
goRight (TreeBranch leftChild treeNode rightChild, xs) =
  (rightChild, TreeDirection RightBranch treeNode leftChild:xs)

-- get the parent of a branch given the direction from the parent to the branch
reconstructAncestor :: (BinaryTreeNode a) => TreeBranch a -> TreeDirection a -> TreeBranch a
reconstructAncestor currentBranch (TreeDirection branchType parentContent sibling) =
  if branchType == LeftBranch
    then TreeBranch currentTree parentContent sibling
    else TreeBranch sibling parentContent currentTree
    where currentTree = branch2Tree currentBranch

-- Move the zipper up to the parent, returns nothing directions list is empty
goUp :: (BinaryTreeNode a) => BranchZipper a -> Maybe (BranchZipper a)
goUp (_, []) = Nothing
goUp (currentBranch, direction:xs) =
  Just (reconstructAncestor currentBranch direction, xs)

getTreeRoot :: (BinaryTreeNode a) => BranchZipper a -> BranchZipper a
getTreeRoot (branch, []) = (branch, [])
getTreeRoot zipper = case goUp zipper of
  Just prevZipper -> getTreeRoot prevZipper
  Nothing -> zipper

appendLeftChild :: (BinaryTreeNode a) => TreeBranch a -> a -> TreeInsertResult a
appendLeftChild (TreeBranch leftChild treeContent rightChild) nodeToAppend =
  if leftChild == Leaf then
    InsertOk newBranch newDirection
  else
    InsertNotYet leftChild newDirection nodeToAppend
  where newBranch = TreeBranch Leaf nodeToAppend Leaf
        newDirection = TreeDirection LeftBranch treeContent rightChild

appendRightChild :: (BinaryTreeNode a) => TreeBranch a -> a -> TreeInsertResult a
appendRightChild (TreeBranch leftChild treeContent rightChild) nodeToAppend =
  if rightChild == Leaf then
    InsertOk newBranch newDirection
  else
    InsertNotYet rightChild newDirection nodeToAppend
  where newBranch = TreeBranch Leaf nodeToAppend Leaf
        newDirection = TreeDirection RightBranch treeContent leftChild

insertOrGoDown :: (BinaryTreeNode a) => TreeDirections a -> TreeInsertResult a -> BranchZipper a
insertOrGoDown treeDirections (InsertOk newBranch newDirection) =
  (newBranch, newDirection:treeDirections)
insertOrGoDown treeDirections (InsertNotYet existingChild directionToChild childToInsert) =
  treeZipperInsert (existingChild, directionToChild:treeDirections) childToInsert

branchZipperToTreeZipper :: (BinaryTreeNode a) => BranchZipper a -> TreeZipper a
branchZipperToTreeZipper (TreeBranch leftChild content rightChild, xs) =
  (Branch leftChild content rightChild, xs)

branchZipperInsert :: (BinaryTreeNode a) => BranchZipper a -> a -> BranchZipper a
branchZipperInsert (TreeBranch leftChild treeNode rightChild, xs) newNode =
  insertOrGoDown xs (appendFunction focusedBranch newNode)
  where
    focusedBranch = TreeBranch leftChild treeNode rightChild
    appendFunction = if newNode <= treeNode then appendLeftChild
                                            else appendRightChild

treeZipperInsert :: (BinaryTreeNode a) => TreeZipper a -> a -> BranchZipper a
treeZipperInsert (Leaf, xs) newNode = (TreeBranch Leaf newNode Leaf, xs)
treeZipperInsert (Branch leftChild treeNode rightChild, xs) newNode =
  branchZipperInsert (TreeBranch leftChild treeNode rightChild, xs) newNode

binaryTreeFind :: (BinaryTreeNode a) => BinaryTree a -> a -> Maybe a
binaryTreeFind Leaf _ = Nothing
binaryTreeFind (Branch leftTree content rightTree) target
  | target == content = Just content
  | target < content = binaryTreeFind leftTree target
  | target > content = binaryTreeFind rightTree target
