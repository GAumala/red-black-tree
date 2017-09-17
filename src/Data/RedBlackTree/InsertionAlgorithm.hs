module Data.RedBlackTree.InsertionAlgorithm (
  identifyRBTCase,
  insert,
  RBTCase (Case1, Case2, Case3, Case4, Case5)
) where

import Data.RedBlackTree.BinaryTree
import Data.RedBlackTree.TreeFamily
import Data.RedBlackTree.Internal

-- The 5 possible cases of red–black tree insertion to handle:
-- 1) inserted node is the root node, i.e., first node of red–black tree.
-- Stored as a @WhiteBranch@without because it should always be black.
-- 2) inserted node has a parent, and it is black. The inserted node is stored
-- as a @RedBlackBranch@ along with a @RedBlackDirections@ to reconstruct all of
-- the ancestors
-- 3) inserted node has a parent and an uncle, and they are both red.
-- 4th parameter is the inserted node as a @WhiteBranch@ because it is assumed
-- to be red.
-- 3rd parameter is the uncle as @WhiteBranch@ because it is also
-- assumed to be red.
-- 2nd parameter is the node content of the grandparent.
-- 1st parameter is a @RedBlackDirections@ to reconstruct all of the remaining
-- ancestors
-- 4) inserted node is placed to right of left child of grandparent, or to left
-- of right child of grandparent.
-- 5th parameter is the inserted node as a  @RedBlackBranch@ because it is
-- assumed to be red but we don't care about it right now.
-- 4th parameter is the sibling of the inserted node as a @RedBlackTree@.
-- 3rd parameter is the parent as a @RedBlackNode@.
-- 2nd parameter is a @RedBlackDirection@ used to reconstruct the grandparent.
-- 1st parameter is a @RedBlackDirections@ to reconstruct all of the remaining
-- ancestors
-- 5) inserted node is placed to left of left child of grandparent, or to right
-- of right child of grandparent.
-- 5th parameter is the inserted node as a @RedBlackBranch@ because it is
-- assumed to be red but we don't care about it right now.
-- 4th parameter is the sibling of the inserted node as a @RedBlackTree@.
-- 3rd parameter is content of the parent.
-- 2nd parameter is a @RedBlackDirection@ used to reconstruct the grandparent.
-- 1st parameter is a @RedBlackDirections@ to reconstruct all of the remaining
-- ancestors.
data RBTCase a
  = Case1 (WhiteBranch a)
  | Case2 (RedBlackDirections a) (RedBlackBranch a)
  | Case3 (RedBlackDirections a) a (WhiteBranch a) (WhiteBranch a)
  | Case4 (RedBlackDirections a) (RedBlackDirection a) (RedBlackNode a)
    (RedBlackTree a) (RedBlackBranch a)
  | Case5 (RedBlackDirections a) (RedBlackDirection a) a (RedBlackTree a)
    (RedBlackBranch a)
  deriving (Eq, Ord, Show)

identifyCases345 :: (BinaryTreeNode a) => RedBlackDirections a ->
  RedBlackDirection a -> RedBlackDirection a -> RedBlackBranch a -> RBTCase a
identifyCases345 directions
  (TreeDirection grandparentBranchType grandparentNode
  (Branch leftCousin (RedBlackNode Red uncleContent) rightCousin))
  parentDirection newBranch =
    case grandparentBranchType of
      LeftBranch ->
        Case3 directions grandparentContent whiteParent whiteUncle
      RightBranch ->
        Case3 directions grandparentContent whiteUncle whiteParent
  where uncleNode = RedBlackNode Red uncleContent
        uncleBranch = TreeBranch leftCousin uncleNode rightCousin
        parentBranch = reconstructAncestor newBranch parentDirection
        grandparentDirection = TreeDirection grandparentBranchType
          grandparentNode (Branch leftCousin uncleNode rightCousin)
        RedBlackNode _ grandparentContent = grandparentNode
        whiteUncle = removeBranchColor uncleBranch
        whiteParent = removeBranchColor parentBranch
identifyCases345 directions grandparentDirection parentDirection newBranch
  | grandparentBranchType /= parentBranchType =
    Case4 directions grandparentDirection parentNode siblingTree newBranch
  | grandparentBranchType == parentBranchType =
    Case5 directions grandparentDirection parentContent siblingTree newBranch
  where TreeDirection grandparentBranchType _ _ = grandparentDirection
        TreeDirection parentBranchType parentNode siblingTree = parentDirection
        RedBlackNode _ parentContent =  parentNode


handleCase1 :: (BinaryTreeNode a) => TreeBranch (RedBlackNode a) -> RedBlackTree a
handleCase1 (TreeBranch leftChild content rightChild) =
  Branch leftChild (paintItBlack content) rightChild

handleRBTCase :: (BinaryTreeNode a) => RBTCase a -> RedBlackTree a
handleRBTCase (Case1 whiteRoot) = Branch leftChild rootNode rightChild
  where WhiteBranch leftChild content rightChild = whiteRoot
        rootNode = RedBlackNode Black content
handleRBTCase (Case2 directionsFromRoot newBranch) = branch2Tree rootBranch
  where branchZipper = (newBranch, directionsFromRoot)
        (rootBranch, _) = getTreeRoot branchZipper
handleRBTCase (Case3 directionsFromRoot content leftWBranch rightWBranch) =
  (handleRBTCase . identifyRBTCase . getTreeFamily) branchZipper
  where leftChild = whiteBranch2Tree leftWBranch Black
        rightChild = whiteBranch2Tree rightWBranch Black
        newNode = RedBlackNode Red content
        newBranch = TreeBranch leftChild newNode rightChild
        branchZipper = (newBranch, directionsFromRoot)
handleRBTCase (Case4 directions grandparentDirection parentNode siblingTree
  latestBranch) =
  handleRBTCase (Case5 directions grandparentDirection newParentContent
  newSiblingTree newLatestBranch)
  where TreeBranch latestLeftTree (RedBlackNode _ childContent)
          latestRightTree = latestBranch
        TreeDirection grandparentDirectionType _ _ = grandparentDirection
        newParentContent = childContent
        newLatestNode = parentNode
        newLatestBranch = if grandparentDirectionType == LeftBranch then
          TreeBranch siblingTree newLatestNode latestLeftTree else
          TreeBranch latestRightTree newLatestNode siblingTree
        newSiblingTree = if grandparentDirectionType == LeftBranch then
          latestRightTree else latestLeftTree
handleRBTCase (Case5 directions grandparentDirection parentContent
  siblingTree latestBranch) =
  branch2Tree rootBranch
  where TreeDirection grandparentDirectionType grandparentNode uncleTree =
          grandparentDirection
        RedBlackNode _ grandparentContent = grandparentNode
        newTopNode = RedBlackNode Black parentContent
        rotatedGrandparentNode = RedBlackNode Red grandparentContent
        latestTree = branch2Tree latestBranch
        needsRightRotation = grandparentDirectionType == LeftBranch
        newSiblingTree = if needsRightRotation
          then Branch siblingTree rotatedGrandparentNode uncleTree
          else Branch uncleTree rotatedGrandparentNode siblingTree
        rotatedBranch = if needsRightRotation
          then TreeBranch latestTree newTopNode newSiblingTree
          else TreeBranch newSiblingTree newTopNode latestTree
        branchZipper = (rotatedBranch, directions)
        (rootBranch, _) = getTreeRoot branchZipper

identifyRBTCase :: (BinaryTreeNode a) => TreeFamily (RedBlackNode a) ->
  RBTCase a
identifyRBTCase (IsRoot rootBranch) = Case1 (removeBranchColor rootBranch)
identifyRBTCase (HasParent direction insertedBranch) = Case2 [] parentBranch
  where parentBranch = reconstructAncestor insertedBranch direction
identifyRBTCase (HasGrandparent directions grandparentDirection
  parentDirection insertedBranch) =
    if parentBranch `branchIsColor` Black
      then Case2 (grandparentDirection:directions) parentBranch
      else identifyCases345 directions grandparentDirection parentDirection
      insertedBranch
  where parentBranch = reconstructAncestor insertedBranch parentDirection
        grandparentBranch = reconstructAncestor parentBranch
          grandparentDirection
        TreeDirection _ _ uncleTree = parentDirection
        TreeBranch _ parentContent _ = parentBranch
        TreeBranch _ grandparentContent _ = grandparentBranch


handleInsertedTreeFamily :: (BinaryTreeNode a) => TreeFamily (RedBlackNode a)
  -> RedBlackTree a
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
        grandparentBranch = reconstructAncestor parentBranch
          grandparentDirection
        grandparentZipper = (grandparentBranch, directions)
        (rootBranch, _) = getTreeRoot grandparentZipper

-- | inserts a new node to the tree, performing the necessary rotations to
-- guarantee that the red black properties are kept after the insertion.
insert :: (BinaryTreeNode a) => RedBlackTree a -> a -> RedBlackTree a
insert tree newItem = if insertedNode `isColor` Black
                      then newTreeWithNewItem
                      else (handleRBTCase . identifyRBTCase) insertedTreeFamily
  where newNode = RedBlackNode Red newItem
        (insertedTreeBranch, directions) = binaryTreeInsert tree newNode
        TreeBranch _ insertedNode _ = insertedTreeBranch
        insertedBranchZipper = (insertedTreeBranch, directions)
        (rootBranch, _) = getTreeRoot insertedBranchZipper
        newTreeWithNewItem = branch2Tree rootBranch
        insertedTreeFamily = getTreeFamily insertedBranchZipper
