{-# LANGUAGE BangPatterns #-}
import Data.List (foldl')
import Data.RedBlackTree.BinaryTree
import Data.RedBlackTree

instance BinaryTreeNode Int where
  mergeNodes leftInt rightInt = leftInt

createTree :: [Int] -> RedBlackTree Int
createTree = foldl' myInsert emptyRedBlackTree
  where myInsert !tree !newNode = insert tree newNode

main :: IO ()
main = do
    let items = [1..(truncate 5e6)] :: [Int]
        tree = createTree items 
        height = getBlackHeight tree

    putStrLn $ "constructed tree of size " ++ (show height)

