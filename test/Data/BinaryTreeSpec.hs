module Data.BinaryTreeSpec (spec) where

import Test.Hspec
import Data.BinaryTree


instance BinaryTreeNode Int where
  mergeNodes leftInt rightInt = leftInt

spec :: Spec
spec = do
  let leftChildContent = 2 :: Int
  let rightChildContent = 3
  let treeContent = 1
  let emptyBranchContent = 4 :: Int
  let emptyTreeBranch = TreeBranch Leaf emptyBranchContent Leaf
  let leftChild = Branch Leaf leftChildContent Leaf
  let rightChild = Branch Leaf rightChildContent Leaf
  let tree = Branch leftChild treeContent rightChild
  let emptyBranch = Branch Leaf emptyBranchContent Leaf
  let leaf = Leaf :: BinaryTree Int

  describe "goLeft" $ do
    let branch = TreeBranch leftChild treeContent rightChild
    it "returns a new zipper focusing on the left child of current tree" $ do
      let (newTree, _) = goLeft (branch, [])

      newTree `shouldBe` leftChild

    it "returns a new zipper with a LeftTree direction, with node's data on top of its direction list" $ do
      let (_, [direction]) = goLeft (branch, [])

      direction `shouldBe` TreeDirection LeftBranch 1 rightChild

  describe "goRight" $ do
    let branch = TreeBranch leftChild treeContent rightChild
    it "returns a new zipper focusing on the right child of current tree" $ do
      let (newTree, _) = goRight (branch, [])

      newTree `shouldBe` rightChild

    it "returns a new zipper with a RightTree direction with node's data on top of its direction list" $ do
      let (_, [direction]) = goRight (branch, [])

      direction `shouldBe` TreeDirection RightBranch 1 leftChild

  describe "goUp" $ do
    let treeContent = 2
    let treeBranch = TreeBranch Leaf treeContent Leaf
    let parentContent = 1
    let expectedLeftChild = Branch Leaf treeContent Leaf

    it "returns a new zipper focusing on the current tree's parent" $ do
      let directionsToParent = [ TreeDirection LeftBranch parentContent
                               rightChild ]
      let Just (parentBranch, _) = goUp(treeBranch, directionsToParent)

      parentBranch `shouldBe` TreeBranch expectedLeftChild parentContent rightChild

    it "returns a new zipper with the direction list's head removed" $ do
      let directionsToParent = [ TreeDirection LeftBranch parentContent
                               rightChild ]
      let Just (_, directions) = goUp(treeBranch, directionsToParent)

      directions `shouldBe` []

    it "returns Nothing if the directions list is empty" $ do
      let maybeZipper = goUp(treeBranch, [])

      maybeZipper `shouldBe` Nothing

  describe "getTreeRoot" $
    it "returns the root branch of the tree" $ do
      let latestNode = 5
      let grandChild = Branch Leaf latestNode Leaf
      let grandChildBranch = TreeBranch Leaf latestNode Leaf
      let grandChildDirections = [ TreeDirection LeftBranch rightChildContent Leaf
                                 , TreeDirection RightBranch treeContent leftChild ]

      let modifiedRightChild = Branch grandChild rightChildContent Leaf
      let expectedRootBranch = TreeBranch leftChild treeContent modifiedRightChild

      let grandChildZipper = (grandChildBranch, grandChildDirections)
      let rootZipper = getTreeRoot grandChildZipper

      rootZipper `shouldBe` (expectedRootBranch, [])

  describe "appendLeftChild" $ do
    it "returns InsertOk with a BranchZipper focusing on the child inserted left" $ do
      let nodeToAppend = 0

      let expectedBranch = TreeBranch Leaf nodeToAppend Leaf
      let expectedDirection = TreeDirection LeftBranch emptyBranchContent Leaf

      let insertResult = appendLeftChild emptyTreeBranch nodeToAppend

      insertResult `shouldBe` InsertOk expectedBranch expectedDirection

    it "returns InsertNotYet if the tree already has a left child" $ do
      let nodeToInsert = 0

      let treeBranch = TreeBranch leftChild treeContent rightChild
      let expectedDirectionToObstuction = TreeDirection LeftBranch treeContent rightChild
      let expectedFailiure = InsertNotYet leftChild expectedDirectionToObstuction nodeToInsert

      let insertResult = appendLeftChild treeBranch nodeToInsert

      insertResult `shouldBe` expectedFailiure

  describe "appendRightChild" $ do
    it "returns InsertOk with a BranchZipper focusing on the child inserted right" $ do
      let nodeToAppend = 1

      let expectedBranch = TreeBranch Leaf nodeToAppend Leaf
      let expectedDirection = TreeDirection RightBranch emptyBranchContent Leaf

      let insertResult = appendRightChild emptyTreeBranch nodeToAppend

      insertResult `shouldBe` InsertOk expectedBranch expectedDirection

    it "returns InsertNotYet if the tree already has a left child" $ do
      let nodeToInsert = 6

      let treeBranch = TreeBranch leftChild treeContent rightChild
      let expectedDirectionToObstuction = TreeDirection RightBranch treeContent leftChild
      let expectedFailiure = InsertNotYet rightChild expectedDirectionToObstuction nodeToInsert

      let insertResult = appendRightChild treeBranch nodeToInsert

      insertResult `shouldBe` expectedFailiure


  describe "binaryTreeInsert" $ do
    let node1 = 10 :: Int
    let node2 = 8
    let node3 = 12
    let node4 = 7
    let node5 = 9
    let node6 = 11
    let node7 = 13

    it "inserts at the correct position" $ do
      let newContent = 8
      let startZipper = (emptyBranch, [])
      let expectedDirections = [ TreeDirection RightBranch emptyBranchContent Leaf ]
      let newZipper = treeZipperInsert startZipper newContent

      newZipper `shouldBe` (TreeBranch Leaf newContent Leaf, expectedDirections)

    it "should be able to create a full tree of size 3 from scratch only wth insertions" $ do
      let zipper1 = (Leaf, [])
      let zipper2 = treeZipperInsert zipper1 node1
      zipper2 `shouldBe` (TreeBranch Leaf node1 Leaf, [])

      let treeBranchNode2 = TreeBranch Leaf node2 Leaf
      let treeNode2 = Branch Leaf node2 Leaf
      let zipper3 = branchZipperInsert zipper2 node2
      zipper3 `shouldBe` (treeBranchNode2, [ TreeDirection LeftBranch node1 Leaf ])

      let zipper4 = getTreeRoot zipper3

      let expectedFullDirections = [ TreeDirection RightBranch node1 treeNode2 ]
      let fullZipper = branchZipperInsert zipper4 node3
      fullZipper `shouldBe` (TreeBranch Leaf node3 Leaf, expectedFullDirections)

    it "should be able to create a full tree of size 5 wth insertions" $ do
      let initialLeftChild = Branch Leaf node2 Leaf
      let initialRightChild = Branch Leaf node3 Leaf
      let zipper1 = (Branch initialLeftChild node1 initialRightChild, [])

      let zipper2Branch = TreeBranch Leaf node4 Leaf
      let zipper2Tree = Branch Leaf node4 Leaf
      let zipper2Directions = [ TreeDirection LeftBranch node2 Leaf
                              , TreeDirection LeftBranch node1 initialRightChild ]
      let zipper2 = treeZipperInsert zipper1 node4

      zipper2 `shouldBe` (zipper2Branch, zipper2Directions)

      let zipper3Branch = TreeBranch Leaf node5 Leaf
      let zipper3Directions = [ TreeDirection RightBranch node2 zipper2Tree
                              , TreeDirection LeftBranch node1 initialRightChild ]
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
      let zipper2Directions = [ TreeDirection LeftBranch node3 Leaf
                              , TreeDirection RightBranch node1 initialLeftChild ]
      let zipper2 = treeZipperInsert zipper1 node6

      zipper2 `shouldBe` (zipper2Branch, zipper2Directions)

      let zipper3Branch = TreeBranch Leaf node7 Leaf
      let zipper3Directions = [ TreeDirection RightBranch node3 zipper2Tree
                              , TreeDirection RightBranch node1 initialLeftChild ]
      let zipper3 = branchZipperInsert (getTreeRoot zipper2) node7

      zipper3 `shouldBe` (zipper3Branch, zipper3Directions)
