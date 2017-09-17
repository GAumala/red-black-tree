module Data.RedBlackTree.RedBlackTreeAssertionsSpec where

import Test.Hspec

import Data.RedBlackTree.BinaryTree
import Data.RedBlackTree.Internal
import Data.RedBlackTree.RedBlackTreeAssertions
import Data.TestUtils

spec :: Spec
spec =
  describe "assertRedBlackTreeProperties" $ do
    let leftSubtreeNode = RedBlackNode Red 1 :: RedBlackNode Int
    let rightSubtreeNode = RedBlackNode Red 3 :: RedBlackNode Int

    it "throws error if tree is not sorted correctly" $ do
      let leftSubtree = Branch Leaf leftSubtreeNode Leaf
      let rightSubtree = Branch Leaf rightSubtreeNode Leaf
      let tree = Branch leftSubtree (RedBlackNode Black 4) rightSubtree

      assertRedBlackTreeProperties tree 2 `shouldThrow` anyException

    it "throws error if a red tree has red children" $ do
      let leftSubtree = Branch Leaf leftSubtreeNode Leaf
      let rightSubtree = Branch Leaf rightSubtreeNode Leaf
      let tree = Branch leftSubtree (RedBlackNode Red 2) rightSubtree

      assertRedBlackTreeProperties tree 2 `shouldThrow` anyException
    it "throws error if not all paths have the same black depth" $ do
      let wrongRightSubtreeNode = RedBlackNode Black 3 :: RedBlackNode Int
      let leftSubtree = Branch Leaf leftSubtreeNode Leaf
      let rightSubtree = Branch Leaf wrongRightSubtreeNode Leaf
      let tree = Branch leftSubtree (RedBlackNode Black 2) rightSubtree

      assertRedBlackTreeProperties tree 2 `shouldThrow` anyException
