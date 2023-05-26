module pt2gr3
import StdEnv

/*
    Write a function that takes a list of integers and returns a list where every even 
    number is replaced by the character 'e' and every odd number is replaced by the 
    character 'o'.
    zero is replaced by 'x'. 
    For example, the list [0,1,2,3,4,5,6] should be transformed into ['x','o','e','o','e','o','e'].
    
    More test cases are given below.
*/

PTfun :: [Int] -> [Char]
PTfun [] = []
PTfun [x:xs]
| x==0 = ['x'] ++ PTfun xs
| x rem 2 == 0 = ['e'] ++ PTfun xs
= ['o'] ++ PTfun xs




//Start = PTfun [1,2,3,4,5,6] // ['o','e','o','e','o','e']
//Start = PTfun [1,5,3,2,0,3,87,1,2,0] // ['o','o','o','e','x','o','o','o','e','x']
//Start = PTfun [-12,-9..12] //['e','o','e','o','x','o','e','o','e'] 