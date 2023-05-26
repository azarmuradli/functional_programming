module pt7gr4
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf


Tree1 :: Tree Int
Tree1 = Node 7 Leaf (Node 5 Leaf (Node 3 Leaf Leaf))


Tree2 :: Tree Int
Tree2 = Node 0 (Node 1 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf))  (Node 2 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf)) 

Tree3 :: Tree Int
Tree3 = Node 0 (Node 1 (Node 3 Leaf (Node 8 Leaf Leaf)) Leaf)  (Node 2 Leaf Leaf)


/*
Write a function that takes a tree and returns the number of nodes having only right children.

example :
           4
         /   \
        2      5
       / \   /    \
      1   3  Leaf 6

      only 5 is the node having only right children
      so the function should return 1
*/


countRight :: (Tree a) -> Int
countRight Leaf = 0
countRight (Node x le ri)
| (countRight le)==0 && (countRight ri)<>0 = 1 + countRight le + countRight ri
= 0+ countRight le + countRight ri




//Start = countRight Tree1 // 2
Start = countRight Tree3 // 1
//Start = countRight Tree2 // 0