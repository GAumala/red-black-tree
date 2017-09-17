module Data.RedBlackTree.RedBlackTreeAssertions (
  assertRedBlackTreeProperties,
  shouldBeColor
  ) where

import Test.Hspec

import Data.RedBlackTree.BinaryTree
import Data.RedBlackTree.Internal

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
    assertRedBlackTreeProperties' leftTree expectedBlackHeight newBlackDepth
    assertRedBlackTreeProperties' rightTree expectedBlackHeight newBlackDepth
assertRedBlackBranchProperties (TreeBranch leftTree (RedBlackNode Red _)
  rightTree) expectedBlackHeight currentBlackDepth = do
    leftTree `shouldBeColor` Black
    rightTree `shouldBeColor` Black
    assertRedBlackTreeProperties' leftTree expectedBlackHeight currentBlackDepth
    assertRedBlackTreeProperties' rightTree expectedBlackHeight currentBlackDepth


assertRedBlackTreeProperties':: (BinaryTreeNode a, Show a) => RedBlackTree a ->
  Int -> Int -> Expectation
assertRedBlackTreeProperties' Leaf expectedBlackHeight currentBlackDepth =
  currentBlackDepth + 1 `shouldBe` expectedBlackHeight
assertRedBlackTreeProperties' (Branch leftSubtree node rightSubtree)
  expectedBlackHeight currentBlackDepth = do
  let currentBranch = TreeBranch leftSubtree node rightSubtree
  assertBranchIsSorted currentBranch
  assertRedBlackBranchProperties currentBranch expectedBlackHeight
    currentBlackDepth

assertRedBlackTreeProperties :: (BinaryTreeNode a, Show a) => RedBlackTree a ->
  Int -> Expectation
assertRedBlackTreeProperties tree expectedBlackHeight = do
    tree `shouldBeColor` Black
    assertRedBlackTreeProperties' tree expectedBlackHeight 0

-- RedBlackNode's Eq instance is colorblind, so we need to test color separately
shouldBeColor :: (BinaryTreeNode a) => RedBlackTree a -> RedBlack -> Expectation
shouldBeColor Leaf expectedColor = Black `shouldBe` expectedColor
shouldBeColor (Branch _ (RedBlackNode color content) _) expectedColor =
  color `shouldBe` expectedColor
