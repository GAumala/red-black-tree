module Data.RedBlackTree (
  getBlackHeight,
  identifyRBTCase,
  insert,
  emptyRedBlackTree,
  find,

  RBTCase (Case1, Case2, Case3, Case4, Case5),
  RedBlack (Red, Black),
  RedBlackNode (RedBlackNode),
  RedBlackBranch,
  RedBlackTree,
  WhiteBranch (WhiteBranch)
) where

import Data.BinaryTree

-- Red black trees can only have two types of nodes: Red and Black
data RedBlack = Red | Black deriving (Show, Eq, Ord)

-- a @RedBlackNode@ contains only two elements, the color of the node and the
-- actual content.
data RedBlackNode a = RedBlackNode {
  nodeColor :: RedBlack,
  content :: a
} deriving (Show)

instance (BinaryTreeNode a) => BinaryTreeNode (RedBlackNode a)  where
  mergeNodes leftNode rightNode = RedBlackNode color mergedContent
    where RedBlackNode color leftContent = leftNode
          RedBlackNode _ rightContent = rightNode
          mergedContent = leftContent `mergeNodes` rightContent

instance (BinaryTreeNode a) => Ord (RedBlackNode a) where
  (RedBlackNode _ lcontent) <= (RedBlackNode _ rcontent) =
    lcontent <= rcontent

instance (BinaryTreeNode a) => Eq (RedBlackNode a) where
  (RedBlackNode _ lcontent) == (RedBlackNode _ rcontent) =
    lcontent == rcontent



-- A @BinaryTree@ with only nodes of type @RedBlackNode. is either a leaf
-- (empty) or a @RedBlackNode@ with 2 @RedBlackTree@ children, left and right
type RedBlackTree a = BinaryTree (RedBlackNode a)

-- A @TreeBranch@ with only nodes of type @RedBlackNode. Holds the data of a
-- @RedBlackTree@ created with the @Branch@ constructor. Useful
-- type when you want to guarantee that the element is not a @Leaf@
type RedBlackBranch a = TreeBranch (RedBlackNode a)

-- @TreeDirection@ for trees of type @RedBlackTree@. Minimum necessary to
-- reconstruct the parent of any focused node. First argument is the @BranchType@
-- of the focused node relative to the parent. Second argument is the parent's
-- node. The third argument is the sibling tree of the focused node.
type RedBlackDirection a = TreeDirection (RedBlackNode a)

-- List of @RedBlackDirection@
type RedBlackDirections a = [ RedBlackDirection a ]

-- Holds all the data of a @RedBlackBranch@ except for the color of the node
-- at the top of the branch
data WhiteBranch a = WhiteBranch (RedBlackTree a) a (RedBlackTree a)
  deriving (Eq, Ord, Show)

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


isColor :: (BinaryTreeNode a) => RedBlackNode a -> RedBlack -> Bool
isColor (RedBlackNode color _) expectedColor = color == expectedColor

branchIsColor :: (BinaryTreeNode a) => TreeBranch (RedBlackNode a) -> RedBlack
  -> Bool
branchIsColor (TreeBranch leftChild node rightChild) = isColor node

treeIsColor :: (BinaryTreeNode a) => RedBlackTree a -> RedBlack -> Bool
treeIsColor Leaf expectedColor = expectedColor == Black
treeIsColor (Branch leftChild node rightChild) expectedColor =
  isColor node expectedColor

paintItBlack :: (BinaryTreeNode a) => RedBlackNode a -> RedBlackNode a
paintItBlack (RedBlackNode _ item) = RedBlackNode Black item

removeBranchColor :: (BinaryTreeNode a) => RedBlackBranch a -> WhiteBranch a
removeBranchColor (TreeBranch leftChild (RedBlackNode _ content) rightChild) =
  WhiteBranch leftChild content rightChild

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

handleCase1 :: (BinaryTreeNode a) => TreeBranch (RedBlackNode a) -> RedBlackTree a
handleCase1 (TreeBranch leftChild content rightChild) =
  Branch leftChild (paintItBlack content) rightChild

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

whiteBranch2Tree :: (BinaryTreeNode a) => WhiteBranch a -> RedBlack ->
  RedBlackTree a
whiteBranch2Tree (WhiteBranch leftChild content rightChild) color =
  Branch leftChild newNode rightChild
  where newNode = RedBlackNode color content

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

getBlackHeight :: (BinaryTreeNode a) => RedBlackTree a -> Int
getBlackHeight Leaf = 1
getBlackHeight (Branch _ (RedBlackNode Black _) rightSubtree) =
  1 + getBlackHeight rightSubtree
getBlackHeight (Branch _ (RedBlackNode Red _) rightSubtree) =
  getBlackHeight rightSubtree

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


insert :: (BinaryTreeNode a) => RedBlackTree a -> a -> RedBlackTree a
insert treeRoot newItem = (handleRBTCase . identifyRBTCase) treeFamily
  where newNode = RedBlackNode Red newItem
        rootZipper = (treeRoot, [])
        (TreeBranch leftChild content rightChild, directions) =
          treeZipperInsert rootZipper newNode
        newTreeBranch = TreeBranch leftChild content rightChild
        newTreeZipper = (newTreeBranch, directions)
        treeFamily = getTreeFamily newTreeZipper

getNodeContent :: (BinaryTreeNode a) => RedBlackNode a -> a
getNodeContent (RedBlackNode _ content) = content

find :: (BinaryTreeNode a) => RedBlackTree a -> a -> Maybe a
find redBlackTree target = fmap getNodeContent maybeResult
  where maybeResult = binaryTreeFind redBlackTree (RedBlackNode Black target)

emptyRedBlackTree :: RedBlackTree a
emptyRedBlackTree = Leaf
