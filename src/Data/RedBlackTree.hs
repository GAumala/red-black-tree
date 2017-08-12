module Data.RedBlackTree (
  insert,

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

data TreeFamily a =
  IsRoot (TreeBranch a) |
  HasParent (TreeDirection a) (TreeBranch a) |
  HasGrandparent (TreeDirections a) (TreeDirection a) (TreeDirection a) (TreeBranch a)


isColor :: (Ord a) => RedBlackNode a -> RedBlack -> Bool
isColor (RedBlackNode color _) expectedColor = color == expectedColor

branchIsColor :: (Ord a) => TreeBranch (RedBlackNode a) -> RedBlack -> Bool
branchIsColor (TreeBranch leftChild node rightChild) = isColor node

paintItBlack :: (Ord a) => RedBlackNode a -> RedBlackNode a
paintItBlack (RedBlackNode _ item) = RedBlackNode Black item


getTreeFamily' :: (Ord a) => BranchZipper a -> TreeDirection a ->
  TreeBranch a -> TreeFamily a
getTreeFamily' (parentBranch, []) direction branch =
  HasParent direction branch
getTreeFamily' (_, grandparentDirection:xs) parentDirection branch =
  HasGrandparent xs grandparentDirection parentDirection branch

getTreeFamily :: (Ord a) => BranchZipper a -> TreeFamily a
getTreeFamily (branch, []) = IsRoot branch
getTreeFamily (branch, direction:xs) =
  getTreeFamily' parentZipper direction branch
  where parentBranch = reconstructAncestor branch direction
        parentZipper = (parentBranch, xs)

handleCase1 :: (Ord a) => TreeBranch (RedBlackNode a) -> RedBlackTree a
handleCase1 (TreeBranch leftChild content rightChild) =
  Branch leftChild (paintItBlack content) rightChild

handleInsertedTreeFamily :: (Ord a) => TreeFamily (RedBlackNode a) -> RedBlackTree a
handleInsertedTreeFamily (IsRoot rootBranch) = handleCase1 rootBranch
handleInsertedTreeFamily (HasParent direction insertedBranch) =
  branch2Tree parentBranch
  where parentBranch = reconstructAncestor insertedBranch direction
handleInsertedTreeFamily (HasGrandparent directions grandparentDirection
  parentDirection insertedBranch) =
    if parentBranch `branchIsColor` Black
      then branch2Tree rootBranch
      else handleCase1 rootBranch
  where parentBranch = reconstructAncestor insertedBranch parentDirection
        grandparentBranch = reconstructAncestor parentBranch grandparentDirection
        grandparentZipper = (grandparentBranch, directions)
        (rootBranch, _) = getTreeRoot grandparentZipper




insert :: (Ord a) => RedBlackTree a -> a -> RedBlackTree a
insert treeRoot newItem = handleInsertedTreeFamily treeFamily
  where newNode = RedBlackNode Red newItem
        rootZipper = (treeRoot, [])
        (TreeBranch leftChild content rightChild, directions) =
          binaryTreeInsert rootZipper newNode
        newTreeBranch = TreeBranch leftChild content rightChild
        newTreeZipper = (newTreeBranch, directions)
        treeFamily = getTreeFamily newTreeZipper
