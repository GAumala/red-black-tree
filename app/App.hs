import Data.List (foldl')
import qualified Data.Sequence as S
import Data.RedBlackTree.BinaryTree


getBalancedValues :: [Int] -> [Int]
getBalancedValues [] = [] 
getBalancedValues [a] = [a] 
getBalancedValues [a, b] = [a, b] 
getBalancedValues [a, b, c] = [b, a, c] 
getBalancedValues originalValues = centerValue `seq` centerValue:remainder
  where len = length originalValues
        breakPoint = truncate $ (fromIntegral len) / 2
        lvalues = take breakPoint originalValues
        (centerValue:rvalues) = drop breakPoint originalValues
        remainder = getBalancedValues lvalues ++ getBalancedValues rvalues

insertToBalancedTree :: RedBlackTree2 Int -> Int -> RedBlackTree2 Int
insertToBalancedTree tree newValue = newTree
  where newTree = redBlackTreeInsert tree newValue

leftMostValue :: BinaryTree a -> Maybe a
leftMostValue Leaf = Nothing
leftMostValue (Branch Leaf value _) = Just value
leftMostValue (Branch ltree _  _) = leftMostValue ltree

rightMostValue :: BinaryTree a -> Maybe a
rightMostValue Leaf = Nothing
rightMostValue (Branch _ value Leaf) = Just value
rightMostValue (Branch _ _ rtree) = rightMostValue rtree

runBinaryTreeTest :: [Int] -> String
runBinaryTreeTest items =
    let 
        tree = foldl' insertToBalancedTree Leaf items
        leftMost = leftMostValue tree
    in
      "constructed tree of with left most value:  " ++ (show leftMost)

insertTriples :: S.Seq Int -> Int -> S.Seq Int
insertTriples sequence newInt = sequence S.|> newTriple 
  where newTriple = newInt `seq` newInt * 3

runSequenceTest :: [Int] -> String
runSequenceTest ints =  
  let 
      sequenceOfTriples = foldl' insertTriples S.empty ints
      lastIndex = length ints - 1
      lastTriple = S.index sequenceOfTriples lastIndex 
  in
      "constructed sequence. last triple is " ++ (show lastTriple)

main :: IO ()
main = putStrLn $ runBinaryTreeTest items
  where items = getBalancedValues $ [1..(truncate 1e5)] :: [Int]

