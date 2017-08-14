module Data.RedBlackTree (
  identifyRBTCase,
  insert,

  RBTCase (Case1, Case2, Case3, Case4, Case5),
  RedBlack (Red, Black),
  RedBlackNode (RedBlackNode),
  RedBlackTree,
  TreeFamily (IsRoot, HasParent, HasGrandparent),
  WhiteBranch (WhiteBranch)
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

data TreeFamily a =
  IsRoot (TreeBranch a) |
  HasParent (TreeDirection a) (TreeBranch a) |
  HasGrandparent (TreeDirections a) (TreeDirection a) (TreeDirection a) (TreeBranch a)

type RedBlackTree a = BinaryTree (RedBlackNode a)

type RedBlackBranch a = TreeBranch (RedBlackNode a)

type RedBlackDirection a = TreeDirection (RedBlackNode a)

type RedBlackDirections a = [ RedBlackDirection a ]

data (Ord a) => WhiteBranch a = WhiteBranch (RedBlackTree a) a (RedBlackTree a)
  deriving (Eq, Ord, Show)

data RBTCase a =
  Case1 (WhiteBranch a) |
  Case2 (RedBlackDirections a) (RedBlackBranch a) |
  Case3 (RedBlackDirections a) a (WhiteBranch a) (WhiteBranch a) |
  Case4 (RedBlackDirections a) (RedBlackBranch a) (RedBlackBranch a) |
  Case5 (RedBlackDirections a) (WhiteBranch a) (WhiteBranch a) (RedBlackBranch a)
  deriving (Eq, Ord, Show)


isColor :: (Ord a) => RedBlackNode a -> RedBlack -> Bool
isColor (RedBlackNode color _) expectedColor = color == expectedColor

branchIsColor :: (Ord a) => TreeBranch (RedBlackNode a) -> RedBlack -> Bool
branchIsColor (TreeBranch leftChild node rightChild) = isColor node

treeIsColor :: (Ord a) => RedBlackTree a -> RedBlack -> Bool
treeIsColor Leaf expectedColor = expectedColor == Black
treeIsColor (Branch leftChild node rightChild) expectedColor =
  isColor node expectedColor

paintItBlack :: (Ord a) => RedBlackNode a -> RedBlackNode a
paintItBlack (RedBlackNode _ item) = RedBlackNode Black item

removeBranchColor :: (Ord a) => RedBlackBranch a -> WhiteBranch a
removeBranchColor (TreeBranch leftChild (RedBlackNode _ content) rightChild) =
  WhiteBranch leftChild content rightChild

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

identifyCases345 :: (Ord a) => RedBlackDirections a -> RedBlackDirection a ->
  RedBlackDirection a -> RedBlackBranch a -> RBTCase a
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
    Case4 (grandparentDirection:directions) parentBranch newBranch
  | grandparentBranchType == parentBranchType =
    Case5 directions whiteGrandparent whiteParent newBranch
  where TreeDirection grandparentBranchType _ _ = grandparentDirection
        TreeDirection parentBranchType _ _ = parentDirection
        parentBranch = reconstructAncestor newBranch parentDirection
        grandparentBranch = reconstructAncestor parentBranch
                            grandparentDirection
        whiteParent = removeBranchColor parentBranch
        whiteGrandparent = removeBranchColor grandparentBranch


identifyRBTCase :: (Ord a) => TreeFamily (RedBlackNode a) -> RBTCase a
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
        grandparentBranch = reconstructAncestor parentBranch grandparentDirection
        TreeDirection _ _ uncleTree = parentDirection
        TreeBranch _ parentContent _ = parentBranch
        TreeBranch _ grandparentContent _ = grandparentBranch


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
