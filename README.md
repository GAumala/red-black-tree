# red-black-tree

[![Build Status](https://travis-ci.org/GAumala/red-black-tree.svg?branch=master)](https://travis-ci.org/GAumala/red-black-tree)

[Red Black Tree](https://en.wikipedia.org/wiki/Red%E2%80%93black_tree) data structure implemented in Haskell.

The goal of this project is to provide an efficient generic structure that can insert and find elements in O(log(n)) time.

## Usage

### Implement `BinaryTreeNode`

To insert values to a `RedBlackTree`, their type must have an instance of `BinaryTreeNode`. Members of this typeclass must implement `Ord`, and consequently `Eq`, so that values can be compared and sorted within the tree. Since inserting duplicate values can corrupt the tree, this typeclass provides the following function:

``` Haskell
  mergeNodes :: a -> a -> a
```

This function takes two `BinaryTreeNode` values and returns a new "merged" node. It is only called when a value is about to be inserted, but the tree already contains another value that is equal to it. This new "merged" node replaces the existing one without doing any further modifications to the tree.

If you want to ignore duplicate nodes, you can just return the first parameter, since that one is guaranteed to be the one that is already **in** the tree, whereas the second parameter is the node that we are are trying to insert. On the other hand, if you want to replace older nodes with newer ones then you can return the second parameter. If your use case is completely different then you can create a new node with the best parts from each node and return that. Implementing `mergeNodes` is all you need to do to get a working `BinaryTreeNode` instance.

### The API

To use this data structure you only need to know about these three functions:

``` Haskell
emptyRedBlackTree :: RedBlackTree a

insert :: (BinaryTreeNode a) => RedBlackTree a -> a -> RedBlackTree a

find :: (BinaryTreeNode a) => RedBlackTree a -> a -> Maybe a
```

`emptyRedBlackTree` is pretty self explanatory, you can use this as a constructor.

`insert` takes an existing tree and a new value and returns a new tree that contains the new value.

`find` takes an existing tree and a target value and returns the value inside that tree that is equal to the target. Returns `Nothing` if no such value is found.

## Example

Suppose we wanted to store our app's users on a `RedBlackTree`. For simplicity, let's assume that we only know two things about the user: the user's email address, a string guaranteed to be unique for each user, and the user's name. The type `User` could look like this:

``` Haskell
data User = User {
  userEmail :: String,
  userName :: String
} deriving Show
```

Since e-mail adresses are guaranteed to be unique, we don't care about duplicates so the `User` instances for `Eq`, `Ord` and `BinaryTreeNode` would look like this:

``` Haskell
instance Eq User where
  (==) leftNode rightNode = userEmail leftNode == userEmail rightNode

instance Ord User where
  (<=) leftNode rightNode = userEmail leftNode <= userEmail rightNode

instance BinaryTreeNode User where
  mergeNodes leftNode _ = leftNode
```

The final step is to write a small program that inserts 8 users, and then looks
up the name of the user whose address is "frank@gmail.com".

``` Haskell

import Data.List (foldl')
import Data.RedBlackTree
import Data.User -- This module defines our User type

main = do
-- create users to insert
let users = [
              User "gabriel@hotmail.com" "Gabriel",
              User "jimmy@live.com" "Jimmy",
              User "paul@yahoo.com" "Paul",
              User "george@aol.com" "George",
              User "frank@gmail.com" "Frank",
              User "david@hotmail.com" "David",
              User "bryan@live.com" "Bryan",
              User "john@yahoo.com" "John"
            ]

-- insert one by one            
let treeWithOneUser = insert emptyRedBlackTree (User "gabriel@hotmail.com" "gabriel")
let treeWithTwoUsers = insert treeWithOneUser (User "jimmy@live.com" "jimmy")

-- insert the whole list with foldl'
let userTree = foldl' insert emptyRedBlackTree users

-- find the username with address "frank@gmail.com". The Eq instance of User only
-- checks for email address, so we can leave the username empty in the target value
let targetNode1 = User "frank@gmail.com" ""
print $ fmap userName (find userTree targetNode1)
-- should print "Just frank"

-- find the username with address "frank@aol.com"
let targetNode2 = User "frank@aol.com" ""
print $ fmap userName (find userTree targetNode2)
-- should print Nothing
```

## Limitations

- Currently the only operations supported are `find` and `insert`. There is no `remove` at the moment.
- Uses a lot of memory. A `RedBlackTree` with 10 million integers needs over 1 GB of RAM. I'm not sure how to fix this. Suggestions are welcome!

## Development

To build this library, clone this repo and use Stack.

``` bash
git clone https://github.com/GAumala/red-black-tree
stack setup
stack build
```

To run unit tests with HSpec:

``` bash
stack test
```
