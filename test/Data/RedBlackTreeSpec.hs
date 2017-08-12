module Data.RedBlackTreeSpec (spec) where

import Test.Hspec
import Data.BinaryTree
import Data.RedBlackTree

-- RedBlackNode's Eq instance is colorblind, so we need to test color separately
shouldBeColor :: (Ord a) => RedBlackTree a -> RedBlack -> Expectation
shouldBeColor (Branch _ (RedBlackNode color content) _) expectedColor =
  color `shouldBe` expectedColor

getLeftTree :: (Ord a) => RedBlackTree a -> RedBlackTree a
getLeftTree (Branch leftChild content rightChild) = leftChild

spec :: Spec
spec =
  describe "insert" $ do
    it "if node is inserted at root, it is painted black" $ do
      let tree = Leaf :: RedBlackTree Int
      let newItem = 1
      let expectedTree = Branch Leaf (RedBlackNode Black 1) Leaf
      let modifiedTree = insert tree newItem

      modifiedTree `shouldBe` expectedTree
      modifiedTree `shouldBeColor` Black

    it "if inserted node lacks grandparent but parent is black, returns the root tree" $ do
      let rootNode = RedBlackNode Black 2
      let rootTree = Branch Leaf rootNode Leaf
      let newItem = 1
      let expectedInsertedTree = Branch Leaf (RedBlackNode Red 1) Leaf
      let expectedTree = Branch expectedInsertedTree rootNode Leaf

      let newTree = insert rootTree newItem
      let newTreeLeftChild = getLeftTree newTree

      newTree `shouldBe` expectedTree
      newTreeLeftChild `shouldBeColor` Red

    it "if inserted node has grandparent but parent is black, returns the root tree" $ do
      let rootNode = RedBlackNode Black 4
      let parentNode = RedBlackNode Black 3
      let parentTree = Branch Leaf parentNode Leaf
      let rootTree = Branch parentTree rootNode Leaf
      let newItem = 2
      let expectedInsertedTree = Branch Leaf (RedBlackNode Red 2) Leaf
      let expectedParentTree = Branch expectedInsertedTree parentNode Leaf
      let expectedTree = Branch expectedParentTree rootNode Leaf

      let newTree = insert rootTree newItem
      let insertedTree = (getLeftTree . getLeftTree) newTree

      newTree `shouldBe` expectedTree
      insertedTree `shouldBeColor` Red
