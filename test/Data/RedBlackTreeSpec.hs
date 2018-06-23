module Data.RedBlackTreeSpec where

import Test.Hspec
import Data.RedBlackTree.BinaryTree
import Data.TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = 
  describe "Data.RedBlackTree.insert" $ do

    it "should construct a valid tree after inserting integers 1-7 in order" $ do
      let items = [1..7]

      let rrt = Branch (newTree Red 5) (RBNode Black 6) (newTree Red 7)
      let rt = Branch (newTree Black 3) (RBNode Red 4) rrt
      let lt = newTree Black 1
      let expectedTree = Branch lt (RBNode Black 2) rt

      let createdTree = createTestTree items

      createdTree `shouldBe` expectedTree
      
  
    it "should construct a valid tree after inserting integers 1-7 in backwards order" $ do
      let items = reverse [1..7]

      let llt = Branch (newTree Red 1) (RBNode Black 2) (newTree Red 3)
      let lt = Branch  llt (RBNode Red 4) (newTree Black 5)
      let rt = newTree Black 7
      let expectedTree = Branch lt (RBNode Black 6) rt

      let createdTree = createTestTree items

      createdTree `shouldBe` expectedTree

    it "should construct a valid tree after inserting integers 1-12 in order" $ do
      let items = [1..12]

      let rlt = Branch (newTree Black 5) (RBNode Red 6) (newTree Black 7)
      let rrrt = Branch Leaf (RBNode Black 11) (newTree Red 12)
      let rrt = Branch (newTree Black 9) (RBNode Red 10) rrrt
      let lt = Branch (newTree Black 1) (RBNode Black 2) (newTree Black 3)
      let rt = Branch rlt (RBNode Black 8) rrt
      let expectedTree = Branch lt (RBNode Black 4) rt

      let createdTree = createTestTree items

      createdTree `shouldBe` expectedTree
      
  
    it "should construct a valid tree after inserting integers 1-12 in backwards order" $ do
      let items = reverse [1..12]

      let lllt = Branch (newTree Red 1) (RBNode Black 2) Leaf
      let llt = Branch lllt (RBNode Red 3) (newTree Black 4)
      let lrt = Branch (newTree Black 6) (RBNode Red 7) (newTree Black 8)
      let lt = Branch llt (RBNode Black 5) lrt
      let rt = Branch (newTree Black 10) (RBNode Black 11) (newTree Black 12)
      let expectedTree = Branch lt (RBNode Black 9) rt

      let createdTree = createTestTree items

      createdTree `shouldBe` expectedTree
      
