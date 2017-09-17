module Data.RedBlackTree.InternalSpec (spec) where

import Test.Hspec
import Data.RedBlackTree.BinaryTree
import Data.RedBlackTree.Internal
import Data.RedBlackTree.RedBlackTreeAssertions
import Data.TestUtils

spec :: Spec
spec =
  describe "find" $
    it "should find every number in a tree that has numbers [1-9]" $ do
      let tree = createTestTree [1..9] :: RedBlackTree Int

      find tree 1 `shouldBe` Just 1
      find tree 2 `shouldBe` Just 2
      find tree 3 `shouldBe` Just 3
      find tree 4 `shouldBe` Just 4
      find tree 5 `shouldBe` Just 5
      find tree 6 `shouldBe` Just 6
      find tree 7 `shouldBe` Just 7
      find tree 8 `shouldBe` Just 8
      find tree 9 `shouldBe` Just 9
      find tree 10 `shouldBe` Nothing
