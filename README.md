# red-black-tree

[Red Black Tree](https://twitter.com/kakimari/status/904075085652398080) data structure implemented in Haskell. 

The goal of this project is to provide an effiecient structure that can insert and find elements in O(n) time. 

## Usage

Currently the Red Black Tree only supports types that implement `Eq`, `Ord`, and `Show`. For example if you wanted to add all numbers between 1 and 10, you could do something like this:

``` Haskell

import Data.RedBlackTree

main = do

-- create empty tree
let emptyTree = emptyRedBlackTree

-- insert elements
let treeWith1 = insert emptyTree 1
let treeWith2 = insert treeWith1 2
let treeWith3 = insert treeWith1 4
...
let treeWith10 = insert treeWith9 10

-- Or you could insert all 10 in a single line with foldl
let treeWith10 = foldl insert emptyRedBlackTree [1..10] 

-- find items
let maybe1 = find treeWith10 1
let maybe2 = find treeWith10 2
```

## Limitations
- Currently the only operations supported are `find` and `insert`. There is no `remove` at the moment.
- Inserting multiple elements with the same value, is not supported, if you do this, the tree will get corrupted and may behave erratically. Eventually I'll try to add a way to merge elements that are "equal". 
