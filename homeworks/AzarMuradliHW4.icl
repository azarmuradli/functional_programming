module AzarMuradliHW4
import StdEnv

/*
write a function that takes a matrix as list of lists and returns the transpose of the matrix
eg :
Input : [[1,2,3],
         [4,5,6],
         [7,8,9]]
Output : [[1,4,7],
          [2,5,8],
          [3,6,9]]
If there is an empty list or a list of different length, return an empty list

***Needs to use atleast one higher order function***
*/


//alma :: [Int] -> [Int]
//alma x = [1]

//lengths :: [[Int]] -> [Int]
//lengths [] = []
//lengths [x:xs] = [length x] ++ lengths xs

//Start = lengths [[1,2,3],[4,5,6],[7,8,9]]

goOver :: [[a]] Int Int -> [[a]]
goOver x a b 
| a==b = []
= [map(\y= y!!b) x] ++ goOver x a (b+1)

checkDifferentLength :: Int [[a]] -> Bool
checkDifferentLength a [] = True
checkDifferentLength a [x:xs]
| a == (length x) = checkDifferentLength a xs
= False

funTran :: [[a]] -> [[a]]
funTran [] = []
funTran [x:xs] 
|checkDifferentLength (length x) xs = goOver ([x:xs]) (length x) 0
= []



//Start = funTran [[1,2,3],[4,5,6],[7,8,9]] // [[1,4,7],[2,5,8],[3,6,9]]
//Start = funTran [[],[]]
//Start = funTran [[1,2],[3,4],[5,6]] // [[1,3,5],[2,4,6]]
//Start = funTran [['a','b','c'],['d','e','f'],['g','h','i']] // [['a','d','g'],['b','e','h'],['c','f','i']]


/*
Write a function that takes two matrices and returns the sum of the two matrices
If the matrices are not of the same order, return an error message
eg :
Input : [[1,2,3],     [[1,2,3],
         [4,5,6],      [4,5,6],
         [7,8,9]]      [7,8,9]]
Output : [[2,4,6],
          [8,10,12],
          [14,16,18]]
Needs to use atleast one higher order function
*/


loop2 :: [Int] [Int] Int Int -> [Int]
loop2 x y max index 
| max == index = []
= [foldr(+) 0 [x!!index,y!!index]] ++ loop2 x y max (index+1)

//Start = baki [1,2,3] [1,2,3] 3 0

loop1 :: [[Int]] [[Int]] Int Int -> [[Int]]
loop1 [] [] max index = []
loop1 [x:xs] [y:ys] max index
| max == index = []
 =  [loop2 x y max index] ++ loop1 xs ys max index
 
//Start = ciyer [[1,2],[3,4],[5,6]] [[1,2],[3,4],[5,6]] 2 0


checkDifferentLength2 :: Int [[Int]] -> Bool
checkDifferentLength2 a [] = True
checkDifferentLength2 a [x:xs]
| a == (length x) = checkDifferentLength2 a xs
= False

funSum :: [[Int]] [[Int]] -> [[Int]]
funSum [x:xs] [y:ys]
| length ([x:xs]) <> length ([y:ys]) = abort "Error"
| (checkDifferentLength2 (length x) [x:xs]) && (checkDifferentLength2 (length y) [y:ys]) = result
= abort "Error"
where result = loop1 [x:xs] [y:ys] (length x) 0

//Start = funSum [[1,2,3],[4,5,6],[7,8,9]] [[1,2,3],[4,5,6],[7,8,9]] // [[2,4,6],[8,10,12],[14,16,18]]
//Start = funSum [[1,2],[3,4],[5,6]] [[1,2],[3,4],[5,6]] // [[2,4],[6,8],[10,12]]
//Start = funSum [[1,2],[3,4],[5,6]] [[1,2],[3,4]] // Error
//Start = funSum [[],[1,3]] [[1,2],[4,5]] // Error








