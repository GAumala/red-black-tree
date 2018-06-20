module Data.RedBlackTree.BinaryTree (
  BinaryTree (Leaf, Branch),
  RedBlackTree2,
  TreeBranch (TreeBranch),
  TreeDirection (TreeDirection),
  TreeDirections,

  binaryTreeInsertWith,
  binaryTreeInsert,
  binaryTreeFind,

  redBlackTreeInsertWith,
  redBlackTreeInsert,
  ) where

import Data.Maybe


-- A BinaryTree is either a leaf (empty) or a @BinaryTreeNode@ with 2
-- @BinaryTree@ children, left and right
data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord)

instance (Show a) => Show (BinaryTree a) where
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
data TreeSide = LeftBranch | RightBranch deriving (Show, Eq, Ord)

-- Minimum necessary to reconstruct the parent of any focused node. First argument
-- is the @TreeSide@ of the focused node relative to the parent. Second argument
-- is the parent's node. The third argument is the sibling tree of the focused
-- node.
data TreeDirection a = TreeDirection !TreeSide a (BinaryTree a)
  deriving (Show, Eq, Ord)

-- List of @TreeDirection@
type TreeDirections a = [TreeDirection a]

-- Holds the data of a @BinaryTree@ created with the @Branch@ constructor. Useful
-- type when you want to guarantee that the element is not a @Leaf@
data TreeBranch a = TreeBranch (BinaryTree a) !a (BinaryTree a)
  deriving (Eq, Ord)

{-# SPECIALIZE binaryTreeInsertWith :: (Ord a) => MergeFn (RBNode a) -> BinaryTree (RBNode a) -> (RBNode a) -> BinaryTree (RBNode a) #-}
binaryTreeInsertWith :: (Ord a) => MergeFn a -> BinaryTree a -> a -> BinaryTree a
binaryTreeInsertWith _ Leaf newItem = Branch Leaf newItem Leaf
binaryTreeInsertWith mergeFn tree newItem
  | newItem < currentItem =  
    let updatedTree = binaryTreeInsertWith mergeFn leftTree newItem
    in Branch updatedTree currentItem rightTree

  | newItem > currentItem =  
    let updatedTree = binaryTreeInsertWith mergeFn rightTree newItem
    in Branch leftTree currentItem updatedTree

  | otherwise = 
    let mergedItem = mergeFn currentItem newItem
    in Branch leftTree mergedItem rightTree

  where Branch leftTree currentItem rightTree = tree 

{-# SPECIALIZE binaryTreeInsert :: Ord a => BinaryTree (RBNode a) -> (RBNode a) -> BinaryTree (RBNode a) #-}
binaryTreeInsert :: (Ord a) => BinaryTree a -> a -> BinaryTree a
binaryTreeInsert = binaryTreeInsertWith const

-- | Looks up an item in the binary tree. Returns Nothing if it was not found.
binaryTreeFind :: (Eq a, Ord a) => BinaryTree a -> a -> Maybe a
binaryTreeFind Leaf _ = Nothing
binaryTreeFind (Branch leftTree content rightTree) target
  | target == content = Just content
  | target < content = binaryTreeFind leftTree target
  | target > content = binaryTreeFind rightTree target


data RBColor = Red | Black
  deriving (Eq, Ord, Show)

data RBNode a = RBNode RBColor a
  deriving (Show)

instance (Eq a) => Eq (RBNode a) where
  (RBNode _ x) == (RBNode _ y) = x == y

instance (Ord a) => Ord (RBNode a) where
  (RBNode _ x) <= (RBNode _ y) = x <= y

type MergeFn a = a -> a -> a

type RedBlackTree2 a = BinaryTree (RBNode a)

mergeRBNodes :: MergeFn a -> (RBNode a -> RBNode a -> RBNode a)
mergeRBNodes mergeFn (RBNode existingColor x) (RBNode _ y) = 
  RBNode existingColor (mergeFn x y)

redBlackTreeInsertWith :: (Ord a) => MergeFn a -> RedBlackTree2 a -> a -> RedBlackTree2 a
redBlackTreeInsertWith _ Leaf n = Branch Leaf (RBNode Black n) Leaf
redBlackTreeInsertWith mergeFn (Branch lc gNode rc) newValue
  | newValue < gValue =  
    case lc of
      Leaf -> 
        let lc' = Branch Leaf (RBNode Red newValue) Leaf
        in Branch lc' (RBNode gColor gValue) rc

      Branch llc pNode lrc 
        | newValue < pValue ->
          case llc of
            Leaf -> 
              if pColor == Red then
                case rc of
                  Leaf -> 
                    let 
                        g' = Branch lrc (RBNode Red gValue) Leaf
                        n' = Branch Leaf (RBNode Red newValue) Leaf
                    in  Branch n' (RBNode Black pValue) g'   

                  Branch rlc (RBNode uColor uValue) rrc ->
                    if uColor == Red then
                      let u' = Branch rlc (RBNode Black uValue) rrc
                          n' = Branch Leaf (RBNode Red newValue) Leaf
                          p' = Branch n' (RBNode Black pValue) lrc
                      in Branch p' (RBNode Red gValue) u'
                    else 
                      let u' = Branch rlc (RBNode uColor uValue) rrc
                          g' = Branch lrc (RBNode Red gValue) u'
                          n' = Branch Leaf (RBNode Red newValue) Leaf
                      in Branch n' (RBNode Black pValue) g'
              else
                let n' = Branch Leaf (RBNode Red newValue) Leaf
                    pNode = RBNode pColor pValue
                    lc' = Branch  n' pNode lrc  
                in Branch lc' gNode rc  
            _ ->
              redBlackTreeInsertWith mergeFn (Branch llc pNode lrc) newValue

        | newValue > pValue ->
          case lrc of
            Leaf ->
              if pColor == Red then
                case rc of 
                  Leaf ->
                    let g' = Branch Leaf (RBNode Red gValue) Leaf
                        n' = Branch Leaf (RBNode Red newValue) Leaf
                    in Branch n' (RBNode Black pValue) g'

                  Branch rlc (RBNode uColor uValue) rrc ->
                    if uColor == Red then
                      let u' = Branch rlc (RBNode Black uValue) rrc
                          n' = Branch Leaf (RBNode Red newValue) Leaf
                          p' = Branch n' (RBNode Black pValue) lrc
                      in Branch p' (RBNode Red gValue) u'
                    else 
                      let u' = Branch rlc (RBNode uColor uValue) rrc
                          g' = Branch Leaf (RBNode Red gValue) u'
                          n' = Branch llc (RBNode Red newValue) Leaf
                      in Branch n' (RBNode Black pValue) g'
              else 
                let n' = Branch Leaf (RBNode Red newValue) Leaf
                    lc' = Branch llc pNode n'  
                in Branch lc' gNode rc  

            _ ->
              redBlackTreeInsertWith mergeFn (Branch llc pNode lrc) newValue


        | otherwise -> 
          let mergedValue = mergeFn pValue newValue
              lc' = Branch llc (RBNode pColor mergedValue) lrc
          in  Branch lc' gNode rc


        where RBNode pColor pValue = pNode     

  | otherwise =  
    let RBNode gColor gValue = gNode
        mergedValue = mergeFn gValue newValue
    in  Branch lc (RBNode gColor mergedValue) rc
  where RBNode gColor gValue = gNode

redBlackTreeInsert :: (Ord a) => RedBlackTree2 a -> a -> RedBlackTree2 a
redBlackTreeInsert = redBlackTreeInsertWith const
