module Data.RedBlackTreeSpec (spec) where

import Test.Hspec
import Data.RedBlackTree



spec :: Spec
spec = do
  let leftChildContent = TreeNode Red 2 :: TreeNode Int
  let rightChildContent = TreeNode Red 3 :: TreeNode Int
  let treeContent = TreeNode Red 1 :: TreeNode Int
  let emptyBranchContent = TreeNode Black 4 :: TreeNode Int
  let emptyTreeBranch = TreeBranch Leaf emptyBranchContent Leaf
  let leftChild = Branch Leaf leftChildContent Leaf
  let rightChild = Branch Leaf rightChildContent Leaf
  let tree = Branch leftChild treeContent rightChild
  let emptyBranch = Branch Leaf emptyBranchContent Leaf
  let leaf = Leaf :: RedBlackTree Int

  describe "goLeft" $ do
    it "returns a new zipper focusing on the left child of current tree" $ do
      let Just (newTree, _) = goLeft (tree, [])

      newTree `shouldBe` leftChild

    it "returns a new zipper with a LeftTree direction, with node's data on top of its direction list" $ do
      let Just (_, [direction]) = goLeft (tree, [])

      direction `shouldBe` LeftTree (TreeNode Red 1) rightChild

    it "returns Nothing if tree is focused on a leaf" $ do
      let maybeZipper = goLeft (leaf, [])

      maybeZipper `shouldBe` Nothing

  describe "goRight" $ do
    it "returns a new zipper focusing on the right child of current tree" $ do
      let Just (newTree, _) = goRight (tree, [])

      newTree `shouldBe` rightChild

    it "returns a new zipper with a RightTree direction with node's data on top of its direction list" $ do
      let Just (_, [direction]) = goRight (tree, [])

      direction `shouldBe` RightTree (TreeNode Red 1) leftChild

    it "returns Nothing if tree is focused on a leaf" $ do
      let maybeZipper = goLeft (leaf, [])

      maybeZipper `shouldBe` Nothing

  describe "goUp" $ do
    let treeContent = TreeNode Red 2
    let treeBranch = TreeBranch Leaf treeContent Leaf
    let parentContent = TreeNode Red 1
    let expectedLeftChild = Branch Leaf treeContent Leaf

    it "returns a new zipper focusing on the current tree's parent" $ do
      let Just (parentBranch, _) = goUp(treeBranch, [LeftTree parentContent rightChild])

      parentBranch `shouldBe` TreeBranch expectedLeftChild parentContent rightChild

    it "returns a new zipper with the direction list's head removed" $ do
      let Just (_, directions) = goUp(treeBranch, [LeftTree parentContent rightChild])

      directions `shouldBe` []

    it "returns Nothing if the directions list is empty" $ do
      let maybeZipper = goUp(treeBranch, [])

      maybeZipper `shouldBe` Nothing

  describe "getTreeRoot" $
    it "returns the root branch of the tree" $ do
      let latestNode = TreeNode Red 5
      let grandChild = Branch Leaf latestNode Leaf
      let grandChildBranch = TreeBranch Leaf latestNode Leaf
      let grandChildDirections = [ LeftTree rightChildContent Leaf
                                 , RightTree treeContent leftChild ]

      let modifiedRightChild = Branch grandChild rightChildContent Leaf
      let expectedRootBranch = TreeBranch leftChild treeContent modifiedRightChild

      let grandChildZipper = (grandChildBranch, grandChildDirections)
      let rootZipper = getTreeRoot grandChildZipper

      rootZipper `shouldBe` (expectedRootBranch, [])

  describe "appendLeftChild" $ do
    it "returns InsertOk with a BranchZipper focusing on the child inserted left" $ do
      let nodeToAppend = TreeNode Red 0

      let expectedBranch = TreeBranch Leaf nodeToAppend Leaf
      let expectedDirection = LeftTree emptyBranchContent Leaf

      let insertResult = appendLeftChild emptyTreeBranch nodeToAppend

      insertResult `shouldBe` InsertOk expectedBranch expectedDirection

    it "returns InsertNotYet if the tree already has a left child" $ do
      let nodeToInsert = TreeNode Red 0

      let treeBranch = TreeBranch leftChild treeContent rightChild
      let expectedDirectionToObstuction = LeftTree treeContent rightChild
      let expectedFailiure = InsertNotYet leftChild expectedDirectionToObstuction nodeToInsert

      let insertResult = appendLeftChild treeBranch nodeToInsert

      insertResult `shouldBe` expectedFailiure

  describe "appendRightChild" $ do
    it "returns InsertOk with a BranchZipper focusing on the child inserted right" $ do
      let nodeToAppend = TreeNode Red 1

      let expectedBranch = TreeBranch Leaf nodeToAppend Leaf
      let expectedDirection = RightTree emptyBranchContent Leaf

      let insertResult = appendRightChild emptyTreeBranch nodeToAppend

      insertResult `shouldBe` InsertOk expectedBranch expectedDirection

    it "returns InsertNotYet if the tree already has a left child" $ do
      let nodeToInsert = TreeNode Red 6

      let treeBranch = TreeBranch leftChild treeContent rightChild
      let expectedDirectionToObstuction = RightTree treeContent leftChild
      let expectedFailiure = InsertNotYet rightChild expectedDirectionToObstuction nodeToInsert

      let insertResult = appendRightChild treeBranch nodeToInsert

      insertResult `shouldBe` expectedFailiure


  describe "binaryTreeInsert" $ do
    let node1 = TreeNode Black 10
    let node2 = TreeNode Red 8
    let node3 = TreeNode Red 12
    let node4 = TreeNode Black 7
    let node5 = TreeNode Black 9
    let node6 = TreeNode Black 11
    let node7 = TreeNode Black 13

    it "inserts at the correct position" $ do
      let newContent = TreeNode Red 8
      let startZipper = (emptyBranch, [])
      let newZipper = binaryTreeInsert startZipper newContent

      newZipper `shouldBe` (TreeBranch Leaf newContent Leaf, [RightTree emptyBranchContent Leaf])

    it "should be able to create a full tree of size 3 from scratch only wth insertions" $ do
      let zipper1 = (Leaf, [])
      let zipper2 = binaryTreeInsert zipper1 node1
      zipper2 `shouldBe` (TreeBranch Leaf node1 Leaf, [])

      let treeBranchNode2 = TreeBranch Leaf node2 Leaf
      let treeNode2 = Branch Leaf node2 Leaf
      let zipper3 = branchZipperInsert zipper2 node2
      zipper3 `shouldBe` (treeBranchNode2, [LeftTree node1 Leaf])

      let zipper4 = getTreeRoot zipper3

      let fullZipper = branchZipperInsert zipper4 node3
      fullZipper `shouldBe` (TreeBranch Leaf node3 Leaf, [RightTree node1 treeNode2])

    it "should be able to create a full tree of size 5 wth insertions" $ do
      let initialLeftChild = Branch Leaf node2 Leaf
      let initialRightChild = Branch Leaf node3 Leaf
      let zipper1 = (Branch initialLeftChild node1 initialRightChild, [])

      let zipper2Branch = TreeBranch Leaf node4 Leaf
      let zipper2Tree = Branch Leaf node4 Leaf
      let zipper2Directions = [ LeftTree node2 Leaf
                              , LeftTree node1 initialRightChild ]
      let zipper2 = binaryTreeInsert zipper1 node4

      zipper2 `shouldBe` (zipper2Branch, zipper2Directions)

      let zipper3Branch = TreeBranch Leaf node5 Leaf
      let zipper3Directions = [ RightTree node2 zipper2Tree
                              , LeftTree node1 initialRightChild ]
      let zipper3 = branchZipperInsert (getTreeRoot zipper2) node5

      zipper3 `shouldBe` (zipper3Branch, zipper3Directions)

    it "should be able to create a full tree of size 7 wth insertions" $ do
      let initialLeftLeftChild = Branch Leaf node4 Leaf
      let initialLeftRightChild = Branch Leaf node5 Leaf
      let initialLeftChild = Branch initialLeftLeftChild node2 initialLeftRightChild
      let initialRightChild = Branch Leaf node3 Leaf
      let zipper1 = (Branch initialLeftChild node1 initialRightChild, [])

      let zipper2Branch = TreeBranch Leaf node6 Leaf
      let zipper2Tree = Branch Leaf node6 Leaf
      let zipper2Directions = [ LeftTree node3 Leaf
                              , RightTree node1 initialLeftChild ]
      let zipper2 = binaryTreeInsert zipper1 node6

      zipper2 `shouldBe` (zipper2Branch, zipper2Directions)

      let zipper3Branch = TreeBranch Leaf node7 Leaf
      let zipper3Directions = [ RightTree node3 zipper2Tree
                              , RightTree node1 initialLeftChild ]
      let zipper3 = branchZipperInsert (getTreeRoot zipper2) node7

      zipper3 `shouldBe` (zipper3Branch, zipper3Directions)
