module Data.RedBlackTreeSpec (spec) where

import Test.Hspec
import Data.BinaryTree
import Data.RedBlackTree

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- RedBlackNode's Eq instance is colorblind, so we need to test color separately

case3FamilyAndExpectation :: (TreeFamily (RedBlackNode Int), RBTCase Int)
case3FamilyAndExpectation = (treeFamily, expectedCase)
  where rootNode = RedBlackNode Black 10
        granduncleNode = RedBlackNode Red 15
        grandparentNode = RedBlackNode Black 5
        parentNode = RedBlackNode Red 3
        uncleNode = RedBlackNode Red 7
        newNode = RedBlackNode Red 1
        newBranch = TreeBranch Leaf newNode Leaf
        newTree = branch2Tree newBranch
        parentBranch = TreeBranch newTree parentNode Leaf
        parentDirection = TreeDirection LeftBranch parentNode Leaf
        uncleTree = Branch Leaf uncleNode Leaf
        granduncleTree = Branch Leaf granduncleNode Leaf
        grandparentDirection = TreeDirection LeftBranch grandparentNode
                                 uncleTree
        rootDirection = TreeDirection LeftBranch rootNode granduncleTree
        treeFamily = HasGrandparent [ rootDirection ] grandparentDirection
                       parentDirection newBranch
        whiteUncle = WhiteBranch Leaf 7 Leaf
        whiteParent = WhiteBranch newTree 3 Leaf
        expectedCase = Case3 [ rootDirection ] 5 whiteParent whiteUncle

case4FamilyAndExpectation :: (TreeFamily (RedBlackNode Int), RBTCase Int)
case4FamilyAndExpectation = (treeFamily, expectedCase)
  where rootNode = RedBlackNode Black 10
        granduncleNode = RedBlackNode Red 15
        grandparentNode = RedBlackNode Black 5
        parentNode = RedBlackNode Red 3
        uncleNode = RedBlackNode Black 7
        newNode = RedBlackNode Red 4
        newTree = branch2Tree newBranch
        uncleTree = Branch Leaf uncleNode Leaf
        granduncleTree = Branch Leaf granduncleNode Leaf
        parentDirection = TreeDirection RightBranch parentNode Leaf
        grandparentDirection = TreeDirection LeftBranch grandparentNode
                                 uncleTree
        rootDirection = TreeDirection LeftBranch rootNode granduncleTree
        parentBranch = TreeBranch Leaf parentNode newTree
        newBranch = TreeBranch Leaf newNode Leaf
        treeFamily = HasGrandparent [ rootDirection ] grandparentDirection
                       parentDirection newBranch
        expectedCase = Case4 [ rootDirection ] grandparentDirection
                       parentBranch newBranch

invertedCase4FamilyAndExpectation :: (TreeFamily (RedBlackNode Int), RBTCase Int)
invertedCase4FamilyAndExpectation = (treeFamily, expectedCase)
  where rootNode = RedBlackNode Black 10
        granduncleNode = RedBlackNode Red 15
        grandparentNode = RedBlackNode Black 5
        parentNode = RedBlackNode Red 7
        uncleNode = RedBlackNode Black 3
        newNode = RedBlackNode Red 6
        newTree = branch2Tree newBranch
        uncleTree = Branch Leaf uncleNode Leaf
        granduncleTree = Branch Leaf granduncleNode Leaf
        parentDirection = TreeDirection LeftBranch parentNode Leaf
        grandparentDirection = TreeDirection RightBranch grandparentNode
                                 uncleTree
        rootDirection = TreeDirection LeftBranch rootNode granduncleTree
        parentBranch = TreeBranch newTree parentNode Leaf
        newBranch = TreeBranch Leaf newNode Leaf
        treeFamily = HasGrandparent [ rootDirection ] grandparentDirection
                       parentDirection newBranch
        expectedCase = Case4 [ rootDirection ] grandparentDirection
                       parentBranch newBranch

case5FamilyAndExpectation :: (TreeFamily (RedBlackNode Int), RBTCase Int)
case5FamilyAndExpectation = (treeFamily, expectedCase)
  where rootNode = RedBlackNode Black 10
        granduncleNode = RedBlackNode Red 15
        grandparentNode = RedBlackNode Black 5
        parentNode = RedBlackNode Red 3
        uncleNode = RedBlackNode Black 7
        newNode = RedBlackNode Red 1
        newBranch = TreeBranch Leaf newNode Leaf
        newTree = branch2Tree newBranch
        uncleTree = Branch Leaf uncleNode Leaf
        granduncleTree = Branch Leaf granduncleNode Leaf
        parentDirection = TreeDirection LeftBranch parentNode Leaf
        grandparentDirection = TreeDirection LeftBranch grandparentNode
                                 uncleTree
        rootDirection = TreeDirection LeftBranch rootNode granduncleTree
        parentBranch = TreeBranch newTree parentNode Leaf
        parentTree = branch2Tree parentBranch
        treeFamily = HasGrandparent [ rootDirection ] grandparentDirection
                       parentDirection newBranch
        whiteParent = WhiteBranch newTree 3 Leaf
        expectedCase = Case5 [ rootDirection ] grandparentDirection whiteParent
                       newBranch

invertedCase5FamilyAndExpectation :: (TreeFamily (RedBlackNode Int), RBTCase Int)
invertedCase5FamilyAndExpectation = (treeFamily, expectedCase)
  where rootNode = RedBlackNode Black 10
        granduncleNode = RedBlackNode Red 15
        grandparentNode = RedBlackNode Black 5
        parentNode = RedBlackNode Red 7
        uncleNode = RedBlackNode Black 3
        newNode = RedBlackNode Red 8
        newBranch = TreeBranch Leaf newNode Leaf
        newTree = branch2Tree newBranch
        uncleTree = Branch Leaf uncleNode Leaf
        granduncleTree = Branch Leaf granduncleNode Leaf
        parentDirection = TreeDirection RightBranch parentNode Leaf
        grandparentDirection = TreeDirection RightBranch grandparentNode
                                 uncleTree
        rootDirection = TreeDirection LeftBranch rootNode granduncleTree
        parentBranch = TreeBranch Leaf parentNode newTree
        parentTree = branch2Tree parentBranch
        treeFamily = HasGrandparent [ rootDirection ] grandparentDirection
                       parentDirection newBranch
        whiteParent = WhiteBranch Leaf 7 newTree
        expectedCase = Case5 [ rootDirection ] grandparentDirection whiteParent
                       newBranch

shouldBeColor :: (Ord a) => RedBlackTree a -> RedBlack -> Expectation
shouldBeColor (Branch _ (RedBlackNode color content) _) expectedColor =
  color `shouldBe` expectedColor

getLeftTree :: (Ord a) => RedBlackTree a -> RedBlackTree a
getLeftTree (Branch leftChild content rightChild) = leftChild

spec :: Spec
spec = do
  describe "identifyRBTCase" $ do
    it "identifies insertion case #1" $ do
      let rootNode = RedBlackNode Red 1
      let rootBranch = TreeBranch Leaf rootNode Leaf
      let treeFamily = IsRoot rootBranch
      let expectedCase = Case1 (WhiteBranch Leaf 1 Leaf)

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

    it "identifies insertion case #2 in a family that only has a parent" $ do
      let rootNode = RedBlackNode Black 3
      let leftNode = RedBlackNode Red 2
      let directionToChild = TreeDirection LeftBranch rootNode Leaf
      let leftChildBranch = TreeBranch Leaf leftNode Leaf
      let leftChild = branch2Tree leftChildBranch
      let treeFamily = HasParent directionToChild leftChildBranch
      let expectedCase = Case2 [] (TreeBranch leftChild rootNode Leaf)

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

    it "identifies insertion case #2 in a larger family" $ do
      let rootNode = RedBlackNode Black 10
      let granduncleNode = RedBlackNode Red 15
      let grandparentNode = RedBlackNode Red 5
      let parentNode = RedBlackNode Black 3
      let uncleNode = RedBlackNode Black 7
      let newNode = RedBlackNode Red 2
      let newBranch = TreeBranch Leaf newNode Leaf
      let newTree = branch2Tree newBranch
      let parentBranch = TreeBranch newTree parentNode Leaf
      let parentDirection = TreeDirection LeftBranch parentNode Leaf
      let uncleTree = Branch Leaf uncleNode Leaf
      let granduncleTree = Branch Leaf granduncleNode Leaf
      let grandparentDirection = TreeDirection LeftBranch grandparentNode
                                 uncleTree
      let rootDirection = TreeDirection LeftBranch rootNode granduncleTree
      let treeFamily = HasGrandparent [rootDirection ] grandparentDirection
                       parentDirection newBranch
      let expectedCase = Case2 [ grandparentDirection, rootDirection ]
                         parentBranch

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

    it "identifies insertion case #3" $ do
      let (treeFamily, expectedCase) = case3FamilyAndExpectation

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

    it "identifies insertion case #4" $ do
      let (treeFamily, expectedCase) = case4FamilyAndExpectation

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

    it "identifies insertion case #4 (inverted example)" $ do
      let (treeFamily, expectedCase) = invertedCase4FamilyAndExpectation

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

    it "identifies insertion case #5" $ do
      let (treeFamily, expectedCase) = case5FamilyAndExpectation

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

    it "identifies insertion case #5 (inverted example)" $ do
      let (treeFamily, expectedCase) = invertedCase5FamilyAndExpectation

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

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
