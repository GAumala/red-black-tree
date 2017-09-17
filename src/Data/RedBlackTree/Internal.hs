module Data.RedBlackTree.Internal (
  branchIsColor,
  getBlackHeight,
  isColor,
  emptyRedBlackTree,
  find,
  paintItBlack,
  removeBranchColor,
  whiteBranch2Tree,

  RedBlack (Red, Black),
  RedBlackNode (RedBlackNode),
  RedBlackBranch,
  RedBlackTree,
  RedBlackDirection,
  RedBlackDirections,
  WhiteBranch (WhiteBranch)
) where

import Data.RedBlackTree.BinaryTree

-- | Red black trees can only have two types of nodes: Red and Black
data RedBlack = Red | Black deriving (Show, Eq, Ord)

-- | a @RedBlackNode@ contains only two elements, the color of the node and the
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



-- | A @BinaryTree@ with only nodes of type @RedBlackNode. is either a leaf
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

whiteBranch2Tree :: (BinaryTreeNode a) => WhiteBranch a -> RedBlack ->
  RedBlackTree a
whiteBranch2Tree (WhiteBranch leftChild content rightChild) color =
  Branch leftChild newNode rightChild
  where newNode = RedBlackNode color content

getBlackHeight :: (BinaryTreeNode a) => RedBlackTree a -> Int
getBlackHeight Leaf = 1
getBlackHeight (Branch _ (RedBlackNode Black _) rightSubtree) =
  1 + getBlackHeight rightSubtree
getBlackHeight (Branch _ (RedBlackNode Red _) rightSubtree) =
  getBlackHeight rightSubtree

getNodeContent :: (BinaryTreeNode a) => RedBlackNode a -> a
getNodeContent (RedBlackNode _ content) = content

-- | Lookup a target node in the tree. The target value doesn't need to be the
-- exact same value that is already in the tree. It only needs to satisfy the
-- @Eq@ instance
find :: (BinaryTreeNode a) => RedBlackTree a -> a -> Maybe a
find redBlackTree target = fmap getNodeContent maybeResult
  where maybeResult = binaryTreeFind redBlackTree (RedBlackNode Black target)

-- | Convenient function to "create" a new empty tree.
emptyRedBlackTree :: RedBlackTree a
emptyRedBlackTree = Leaf
