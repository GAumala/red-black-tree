module Data.RedBlackTree.BinaryTree (
  BinaryTreeNode (mergeNodes),
  BinaryTree (Leaf, Branch),
  BranchType (LeftBranch, RightBranch),
  BranchZipper,
  TreeBranch (TreeBranch),
  TreeDirection (TreeDirection),
  TreeDirections,
  TreeInsertResult (InsertOk, InsertNotYet, InsertMerge),
  TreeZipper,

  appendLeftChild,
  appendRightChild,
  binaryTreeInsert,
  binaryTreeFind,
  branch2Tree,
  branchZipperInsert,
  getTreeRoot,
  goLeft,
  goUp,
  goRight,
  reconstructAncestor
  ) where

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
data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord)

instance (BinaryTreeNode a, Show a) => Show (BinaryTree a) where
  show tree = prettyPrintTree tree 0
    where
      addSpaces num = replicate num ' '
      prettyPrintTree Leaf spaces = " Leaf"
      prettyPrintTree (Branch leftTree content rightTree) spaces =
        " " ++ show content ++ "\n" ++
        identation ++ "L:" ++ prettyPrintSubtree leftTree ++
        identation ++ "R:" ++ prettyPrintSubtree rightTree
        where identation = addSpaces (spaces + 2)
              prettyPrintSubtree subtree =  prettyPrintTree subtree (spaces + 2)
                ++ "\n"


-- A BinaryTree can only have two types of branches: Left or Right
data BranchType = LeftBranch | RightBranch deriving (Show, Eq, Ord)

-- Minimum necessary to reconstruct the parent of any focused node. First argument
-- is the @BranchType@ of the focused node relative to the parent. Second argument
-- is the parent's node. The third argument is the sibling tree of the focused
-- node.
data TreeDirection a = TreeDirection BranchType a (BinaryTree a)
  deriving (Show, Eq, Ord)

-- List of @TreeDirection@
type TreeDirections a = [TreeDirection a]

-- A @BinaryTree@ zipper. the first value of the tuple is the focused @BinaryTree@,
-- while the second argument is the list of directions used to move up to the
-- parent and other ancestors.
type TreeZipper a = (BinaryTree a, TreeDirections a)

-- Holds the data of a @BinaryTree@ created with the @Branch@ constructor. Useful
-- type when you want to guarantee that the element is not a @Leaf@
data TreeBranch a = TreeBranch (BinaryTree a) a (BinaryTree a)
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
-- InsertMerge the node to insert is equal to the tree's node so they were merged
-- and the tree's size remains the same
data TreeInsertResult a =
  InsertOk (TreeBranch a) (TreeDirection a)
  | InsertNotYet (BinaryTree a) (TreeDirection a) a
  | InsertMerge (TreeBranch a)
  deriving (Show, Eq)


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
reconstructAncestor :: (BinaryTreeNode a) => TreeBranch a -> TreeDirection a ->
  TreeBranch a
reconstructAncestor currentBranch (TreeDirection branchType parentContent
  sibling) =
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

appendRightChild :: (BinaryTreeNode a) => TreeBranch a -> a ->
  TreeInsertResult a
appendRightChild (TreeBranch leftChild treeContent rightChild) nodeToAppend =
  if rightChild == Leaf then
    InsertOk newBranch newDirection
  else
    InsertNotYet rightChild newDirection nodeToAppend
  where newBranch = TreeBranch Leaf nodeToAppend Leaf
        newDirection = TreeDirection RightBranch treeContent leftChild


appendWithMerge :: (BinaryTreeNode a) => TreeBranch a -> a ->
  TreeInsertResult a
appendWithMerge (TreeBranch leftChild treeNode rightChild) nodeToAppend =
  InsertMerge (TreeBranch leftChild mergedNode rightChild)
  where mergedNode = mergeNodes treeNode nodeToAppend

insertOrGoDown :: (BinaryTreeNode a) => TreeDirections a -> TreeInsertResult a
  -> BranchZipper a
insertOrGoDown treeDirections (InsertMerge newBranch) =
  (newBranch, treeDirections)
insertOrGoDown treeDirections (InsertOk newBranch newDirection) =
  (newBranch, newDirection:treeDirections)
insertOrGoDown treeDirections (InsertNotYet existingChild directionToChild
  childToInsert) =
  treeZipperInsert (existingChild, directionToChild:treeDirections)
    childToInsert

branchZipperToTreeZipper :: (BinaryTreeNode a) => BranchZipper a -> TreeZipper a
branchZipperToTreeZipper (TreeBranch leftChild content rightChild, xs) =
  (Branch leftChild content rightChild, xs)

branchZipperInsert :: (BinaryTreeNode a) => BranchZipper a -> a ->
  BranchZipper a
branchZipperInsert (TreeBranch leftChild treeNode rightChild, xs) newNode =
  insertOrGoDown xs (appendFunction focusedBranch newNode)
  where
    focusedBranch = TreeBranch leftChild treeNode rightChild
    appendFunction
      | newNode < treeNode = appendLeftChild
      | newNode > treeNode = appendRightChild
      | otherwise = appendWithMerge

treeZipperInsert :: (BinaryTreeNode a) => TreeZipper a -> a -> BranchZipper a
treeZipperInsert (Leaf, xs) newNode = (TreeBranch Leaf newNode Leaf, xs)
treeZipperInsert (Branch leftChild treeNode rightChild, xs) newNode =
  branchZipperInsert (TreeBranch leftChild treeNode rightChild, xs) newNode

binaryTreeInsert :: (BinaryTreeNode a) => BinaryTree a -> a -> BranchZipper a
binaryTreeInsert tree = treeZipperInsert treeZipper
  where treeZipper = (tree, [])

binaryTreeFind :: (BinaryTreeNode a) => BinaryTree a -> a -> Maybe a
binaryTreeFind Leaf _ = Nothing
binaryTreeFind (Branch leftTree content rightTree) target
  | target == content = Just content
  | target < content = binaryTreeFind leftTree target
  | target > content = binaryTreeFind rightTree target
