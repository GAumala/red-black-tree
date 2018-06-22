
module Data.RedBlackTree.BinaryTree (
  BinaryTree (Leaf, Branch),
  RedBlackTree,
  RBColor(Red, Black),
  RBNode (RBNode),
  TreeDirection (TreeDirection),
  TreeDirections,

  binaryTreeInsertWith,
  binaryTreeInsert,
  binaryTreeFind,

  redBlackTreeInsertWith,
  redBlackTreeInsert,
  zipperDInsertWith,
  zipperDInsert,
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
data TreeDirection a = TreeDirection TreeSide a (BinaryTree a)
  deriving (Show, Eq, Ord)

-- List of @TreeDirection@
type TreeDirections a = [TreeDirection a]

getRoot :: (Ord a) => [BinaryTree a] -> BinaryTree a -> BinaryTree a
getRoot [] root = root
getRoot (parent:ancestors) tree = 
  case parent of 
    Leaf -> Leaf -- this should never happen

    Branch lt parentNode rt ->
      case tree of 
        Leaf -> getRoot ancestors parent

        Branch _ childNode _ ->
          let parent' = -- assume parent & child nodes can't ever be equal
                if childNode < parentNode 
              then Branch tree parentNode rt 
              else Branch lt parentNode tree
          in parent' `seq` getRoot ancestors parent'

getRootD :: (Ord a) => TreeDirections a -> BinaryTree a -> BinaryTree a
getRootD [] root = root
getRootD (parent:ancestors) tree =
  let TreeDirection treeSide parentNode sibling = parent
      parent' = 
          if treeSide == LeftBranch 
        then Branch tree parentNode sibling
        else Branch sibling parentNode tree
  in parent' `seq` getRootD ancestors parent'

getRBRoot :: (Ord a) => [RedBlackTree a] -> RedBlackTree a -> RedBlackTree a
getRBRoot [] root = 
  case root of
    Leaf -> Leaf
    Branch lt (RBNode _ rootValue) rt -> 
      Branch lt (RBNode Black rootValue) rt

getRBRoot (p:ancestors) n = 
  case p of 
    Leaf -> Leaf -- this should never happen

    Branch pLeft pNode pRight ->
      case n of 
        Leaf -> getRBRoot ancestors p

        Branch nLeft nNode nRight 
          | nColor == Red -> 
            case ancestors of 
              [] -> 
                  if nValue < pValue
                then Branch n (RBNode Black pValue) pRight
                else Branch pLeft (RBNode Black pValue) n
                

              [ Leaf ] -> Leaf -- this should never happen

              (( Branch gLeft gNode gRight ):gAncestors) ->
                if pValue < gValue then
                  if nValue < pValue then
                    if pColor == Black then
                      let p' = Branch n pNode pRight
                          g' = Branch p gNode gRight
                      in  getRBRoot gAncestors g'
                    else case gRight of
                      Leaf -> 
                        let g' = Branch pRight (RBNode Red gValue) Leaf 
                            p' = Branch n (RBNode Black pValue) g'
                        in getRBRoot gAncestors p' 

                      Branch uLeft uNode uRight ->
                        let RBNode uColor uValue = uNode
                            u = Branch uLeft uNode uRight
                        in 
                          if uColor == Black then    
                            let g' = Branch pRight (RBNode Red gValue) u 
                                p' = Branch n (RBNode Black pValue) g'
                            in getRBRoot gAncestors p' 
                          else
                            let p' = Branch n (RBNode Black pValue) pRight
                                u' = Branch uLeft (RBNode Black uValue) uRight
                                g' = Branch p' (RBNode Red gValue) u'
                            in  getRBRoot gAncestors g'

                  else 
                    if pColor == Black then 
                      let p' = Branch pLeft pNode n
                          g' = Branch p gNode gRight
                      in  getRBRoot gAncestors g'
                    else case gRight of
                      Leaf -> 
                        let g' = Branch nRight (RBNode Red gValue) Leaf 
                            p' = Branch pLeft (RBNode Red pValue) nLeft
                            n' = Branch p' (RBNode Black nValue) g'
                        in  getRBRoot gAncestors n' 

                      Branch uLeft uNode uRight ->
                        let RBNode uColor uValue = uNode
                            u = Branch uLeft uNode uRight
                        in 
                          if uColor == Black then    
                            let g' = Branch nRight (RBNode Red gValue) u 
                                p' = Branch pLeft (RBNode Red pValue) nLeft
                                n' = Branch p' (RBNode Black nValue) g'
                            in  getRBRoot gAncestors n' 
                          else
                            let p' = Branch pLeft (RBNode Black pValue) n
                                u' = Branch uLeft (RBNode Black uValue) uRight
                                g' = Branch p' (RBNode Red gValue) u'
                            in  getRBRoot gAncestors g'

                else 
                  if nValue > pValue then
                    if pColor == Black then
                      let p' = Branch pLeft pNode n
                          g' = Branch gLeft gNode p'
                      in  getRBRoot gAncestors g'
                    else case gLeft of
                      Leaf -> 
                        let g' = Branch Leaf (RBNode Red gValue) pLeft 
                            p' = Branch g' (RBNode Black pValue) n
                        in  getRBRoot gAncestors p' 
                      
                      Branch uLeft uNode uRight -> 
                        let RBNode uColor uValue = uNode
                            u = Branch uLeft uNode uRight
                        in 
                          if uColor == Black then
                            let g' = Branch u (RBNode Red gValue) pLeft 
                                p' = Branch g' (RBNode Black pValue) n
                            in getRBRoot gAncestors p' 
                          else
                            let p' = Branch pLeft (RBNode Black pValue) n
                                u' = Branch uLeft (RBNode Black uValue) uRight
                                g' = Branch u' (RBNode Red gValue) p'
                            in  getRBRoot gAncestors g'
                  else 
                    if pColor == Black then 
                      let p' = Branch n pNode pRight
                          g' = Branch gLeft gNode p'
                      in  getRBRoot gAncestors g' 
                    else case gLeft of
                      Leaf -> 
                        let g' = Branch Leaf (RBNode Red gValue) nLeft 
                            p' = Branch nRight (RBNode Red pValue) pRight
                            n' = Branch g' (RBNode Black nValue) p'
                        in  getRBRoot gAncestors n' 
                            
                      Branch uLeft uNode uRight ->
                        let RBNode uColor uValue = uNode
                            u = Branch uLeft uNode uRight
                        in 
                          if uColor == Black then    
                            let g' = Branch u (RBNode Red gValue) nLeft 
                                p' = Branch nRight (RBNode Red pValue) pRight
                                n' = Branch g' (RBNode Black nValue) p'
                            in  getRBRoot gAncestors n' 
                          else
                            let p' = Branch n (RBNode Black pValue) pRight
                                u' = Branch uLeft (RBNode Black uValue) uRight
                                g' = Branch u' (RBNode Red gValue) p'
                            in  getRBRoot gAncestors g'

                where RBNode gColor gValue = gNode
                      g = Branch gLeft gNode gRight
            

          | otherwise -> 
            let p' = -- assume p & child nodes can't ever be equal
                  if nValue < pValue 
                then Branch n pNode pRight 
                else Branch pLeft pNode n
            in p' `seq` getRBRoot ancestors p'

            where RBNode pColor pValue = pNode
                  RBNode nColor nValue = nNode

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

_redBlackTreeInsertWith :: (Ord a) => MergeFn a -> [RedBlackTree a] -> RedBlackTree a -> a -> RedBlackTree a
_redBlackTreeInsertWith _ ancestors Leaf newValue = getRBRoot ancestors newBranch
  where newBranch = Branch Leaf (RBNode Red newValue) Leaf
_redBlackTreeInsertWith mergeFn ancestors (Branch pLeft pNode pRight) newValue 
  | newValue < pValue =
    parentTree `seq` _redBlackTreeInsertWith mergeFn (parentTree:ancestors) pLeft newValue

  | newValue > pValue =
    parentTree `seq` _redBlackTreeInsertWith mergeFn (parentTree:ancestors) pRight newValue

  | otherwise =
    let mergedValue = mergeFn pValue newValue
        parentTree' =  Branch pLeft (RBNode pColor mergedValue) pRight 
    in  parentTree' `seq` getRBRoot ancestors parentTree' 

  where parentTree = Branch pLeft pNode pRight
        RBNode pColor pValue = pNode

redBlackTreeInsertWith :: (Ord a) => MergeFn a -> RedBlackTree a -> a -> RedBlackTree a
redBlackTreeInsertWith mergeFn = _redBlackTreeInsertWith mergeFn [] 

redBlackTreeInsert :: (Ord a) => RedBlackTree a -> a -> RedBlackTree a
redBlackTreeInsert = redBlackTreeInsertWith const

_zipperDInsertWith :: (Ord a) => MergeFn a -> [TreeDirection a] -> BinaryTree a -> a -> BinaryTree a
_zipperDInsertWith _ directions Leaf newValue = getRootD directions newBranch
  where newBranch = Branch Leaf newValue Leaf
_zipperDInsertWith mergeFn directions (Branch lt pValue rt) newValue 
  | newValue < pValue =
    let newDirection = TreeDirection LeftBranch pValue rt
        directions' = newDirection:directions
    in  newDirection `seq` _zipperDInsertWith mergeFn directions' lt newValue

  | newValue > pValue =
    let newDirection = TreeDirection RightBranch pValue lt
        directions' = newDirection:directions
    in  newDirection `seq` _zipperDInsertWith mergeFn directions' rt newValue

  | otherwise =
    let mergedValue = mergeFn pValue newValue
        updatedTree = Branch lt mergedValue rt 
    in  updatedTree `seq` getRootD directions updatedTree 

zipperDInsertWith :: (Ord a) => MergeFn a -> BinaryTree a -> a -> BinaryTree a
zipperDInsertWith mergeFn = _zipperDInsertWith mergeFn ([] :: TreeDirections a) 

zipperDInsert :: (Ord a) => BinaryTree a -> a -> BinaryTree a
zipperDInsert = zipperDInsertWith const

{-# SPECIALIZE binaryTreeInsert :: (Ord a) => BinaryTree (RBNode a) -> (RBNode a) -> BinaryTree (RBNode a) #-}
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
  deriving (Eq, Ord, Show)

type MergeFn a = a -> a -> a

type RedBlackTree a = BinaryTree (RBNode a)

