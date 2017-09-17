module Data.RedBlackTreeSpec where

import Test.Hspec
import Data.RedBlackTree.RedBlackTreeAssertions
import Data.RedBlackTree
import Data.TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = do
  describe "Data.RedBlackTree.insert" $

    it "should not break if 1M integers are inserted to the tree" $ do
      let items = [1..1000000] :: [Int]
      let tree = createTestTree items
      let expectedBlackHeight = getBlackHeight tree

      assertRedBlackTreeProperties tree expectedBlackHeight

  describe "duplicate value handling" $ do
      let tree = createTestTree
                [
                  ListNode 1 ["yellow"],
                  ListNode 2 ["red"],
                  ListNode 3 ["blue"],
                  ListNode 4 ["orange"],
                  ListNode 5 ["purple"],
                  ListNode 6 ["pink"],
                  ListNode 7 ["green"],
                  ListNode 2 ["crimson"],
                  ListNode 2 ["ruby"],
                  ListNode 1 ["gold"],
                  ListNode 7 ["turquoise"],
                  ListNode 3 ["sapphire"]
                ]

      it "should merge equal nodes instead of inserting them to the tree" $ do
        let node1 = find tree (ListNode 1 [])
        let node2 = find tree (ListNode 2 [])
        let node3 = find tree (ListNode 3 [])
        let node7 = find tree (ListNode 7 [])

        let values1 = fmap nodeValuesList node1
        let values2 = fmap nodeValuesList node2
        let values3 = fmap nodeValuesList node3
        let values7 = fmap nodeValuesList node7

        values1 `shouldBe` Just ["yellow", "gold"]
        values2 `shouldBe` Just ["red", "crimson", "ruby"]
        values3 `shouldBe` Just ["blue", "sapphire"]
        values7 `shouldBe` Just ["green", "turquoise"]

      it "duplicate values should not break red black tree properties" $
        assertRedBlackTreeProperties tree 3
