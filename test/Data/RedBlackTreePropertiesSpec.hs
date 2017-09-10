module Data.RedBlackTreePropertiesSpec where

import Test.Hspec
import Data.BinaryTree
import Data.RedBlackTree
import Data.TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

assertBranchIsSorted :: (BinaryTreeNode a, Show a) => TreeBranch a ->
  Expectation
assertBranchIsSorted (TreeBranch Leaf _ Leaf) = return ()
assertBranchIsSorted (TreeBranch (Branch _ leftContent _) content Leaf) =
  leftContent `shouldSatisfy` (< content)
assertBranchIsSorted (TreeBranch Leaf content (Branch _ rightContent _)) =
  rightContent `shouldSatisfy` (> content)
assertBranchIsSorted (TreeBranch (Branch _ leftContent _) content
  (Branch _ rightContent _)) = do
  leftContent `shouldSatisfy` (< content)
  rightContent `shouldSatisfy` (> content)

assertRedBlackBranchProperties :: (BinaryTreeNode a, Show a) =>
  RedBlackBranch a -> Int -> Int -> Expectation
assertRedBlackBranchProperties (TreeBranch leftTree (RedBlackNode Black _)
  rightTree) expectedBlackHeight currentBlackDepth = do
    let newBlackDepth = currentBlackDepth + 1
    assertRedBlackTreeProperties leftTree expectedBlackHeight newBlackDepth
    assertRedBlackTreeProperties rightTree expectedBlackHeight newBlackDepth
assertRedBlackBranchProperties (TreeBranch leftTree (RedBlackNode Red _)
  rightTree) expectedBlackHeight currentBlackDepth = do
    leftTree `shouldBeColor` Black
    rightTree `shouldBeColor` Black
    assertRedBlackTreeProperties leftTree expectedBlackHeight currentBlackDepth
    assertRedBlackTreeProperties rightTree expectedBlackHeight currentBlackDepth


assertRedBlackTreeProperties:: (BinaryTreeNode a, Show a) => RedBlackTree a ->
  Int -> Int -> Expectation
assertRedBlackTreeProperties Leaf expectedBlackHeight currentBlackDepth =
  currentBlackDepth + 1 `shouldBe` expectedBlackHeight
assertRedBlackTreeProperties (Branch leftSubtree node rightSubtree)
  expectedBlackHeight currentBlackDepth = do
  let currentBranch = TreeBranch leftSubtree node rightSubtree
  assertBranchIsSorted currentBranch
  assertRedBlackBranchProperties currentBranch expectedBlackHeight
    currentBlackDepth

spec :: Spec
spec = do
  describe "assertRedBlackTreeProperties" $ do
    it "throws error if tree is not sorted correctly" $ do
      let leftSubtreeNode = RedBlackNode Red 1 :: RedBlackNode Int
      let rightSubtreeNode = RedBlackNode Red 3 :: RedBlackNode Int
      let leftSubtree = Branch Leaf leftSubtreeNode Leaf
      let rightSubtree = Branch Leaf rightSubtreeNode Leaf
      let tree = Branch leftSubtree (RedBlackNode Black 4) rightSubtree

      assertRedBlackTreeProperties tree 2 0 `shouldThrow` anyException

    it "throws error if a red tree has red children" $ do
      let leftSubtreeNode = RedBlackNode Red 1 :: RedBlackNode Int
      let rightSubtreeNode = RedBlackNode Red 3 :: RedBlackNode Int
      let leftSubtree = Branch Leaf leftSubtreeNode Leaf
      let rightSubtree = Branch Leaf rightSubtreeNode Leaf
      let tree = Branch leftSubtree (RedBlackNode Red 2) rightSubtree

      assertRedBlackTreeProperties tree 2 0 `shouldThrow` anyException
    it "throws error if not all paths have the same black depth" $ do
      let leftSubtreeNode = RedBlackNode Red 1 :: RedBlackNode Int
      let rightSubtreeNode = RedBlackNode Black 3 :: RedBlackNode Int
      let leftSubtree = Branch Leaf leftSubtreeNode Leaf
      let rightSubtree = Branch Leaf rightSubtreeNode Leaf
      let tree = Branch leftSubtree (RedBlackNode Black 2) rightSubtree

      assertRedBlackTreeProperties tree 2 0 `shouldThrow` anyException
  describe "Red Black Tree Properties" $ 
    it "should not break if 1M integers are inserted to the tree" $ do
      let items = [1..1000000] :: [Int]
      let tree = createTestTree items
      let expectedBlackHeight = getBlackHeight tree

      tree `shouldBeColor` Black
      assertRedBlackTreeProperties tree expectedBlackHeight 0
