module Data.RedBlackTree.TreeFamily (
  getTreeFamily,
  TreeFamily (IsRoot, HasParent, HasGrandparent)
) where

import Data.RedBlackTree.BinaryTree
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
  HasGrandparent (TreeDirections a) (TreeDirection a) (TreeDirection a)
    (TreeBranch a)

getTreeFamily' :: (BinaryTreeNode a) => BranchZipper a -> TreeDirection a ->
  TreeBranch a -> TreeFamily a
getTreeFamily' (parentBranch, []) direction branch =
  HasParent direction branch
getTreeFamily' (_, grandparentDirection:xs) parentDirection branch =
  HasGrandparent xs grandparentDirection parentDirection branch

getTreeFamily :: (BinaryTreeNode a) => BranchZipper a -> TreeFamily a
getTreeFamily (branch, []) = IsRoot branch
getTreeFamily (branch, direction:xs) =
  getTreeFamily' parentZipper direction branch
  where parentBranch = reconstructAncestor branch direction
        parentZipper = (parentBranch, xs)
