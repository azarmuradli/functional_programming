module HW2
import StdEnv

// Your Neptune code goes here : EZEEEL

/*
A happy number is a number which eventually reaches 1 when replaced by the sum of the square of each digit.
      e.g: 13 is a happy number
            13 -> 1^2 + 3^2 = 10
           10 -> 1^2 + 0^2 = 1
      13 eventually reaches 1, so 13 is a happy number
      4 is not a happy number because it never reaches 1

      Write a function to check if a number is a happy number or not.

        ***Please dont use the abort function.***
*/

SumSq :: Int -> Int
SumSq x 
| x==0 = 0
= (x rem 10)*(x rem 10) + SumSq (x/10)

Happy :: Int -> Bool
Happy x
| x==1 = True
| x==4 = False
= Happy (SumSq(x))

//Start = Happy 14

HappyNumber :: Int -> String
HappyNumber 0 = "Not Happy"
HappyNumber x
| Happy x == True = "Happy"
= "Not Happy"




//Start = HappyNumber 68 // "Happy"
//Start = HappyNumber 4 // "Not Happy"
//Start = HappyNumber 13 // "Happy"
//Start = HappyNumber 0 // "Not Happy"
//Start = HappyNumber 1 // "Happy"




/*
    Write a function that takes two Integer lists of the same length and returns a list containing 
    sublists having three elements each, where the first element is the sum of the elements 
    at the same index in the two lists, and the second element is the product of the elements.
    e.g: [1,2,3] [4,5,6] -> [[5,4],[7,10],[9,18]]
        from first list      from second list      result
        1                     4                     [1+4, 1*4] = [5,4]
        2                     5                     [2+5, 2*5] = [7,10]
        3                     6                     [3+6, 3*6] = [9,18]
*/

AllPairs :: [Int] [Int] -> [[Int]]
AllPairs [] [] = []
AllPairs x [] = []
AllPairs [] y = []
AllPairs [x:xs] [y:ys] = [[x+y,x*y]] ++ AllPairs xs ys



//Start = AllPairs [1,2,3] [4,5,6] // [[5,4],[7,10],[9,18]]
//Start = AllPairs [4,6,2,7] [8,3,5,1] // [[12,32],[9,18],[7,10],[8,7]]
//Start = AllPairs [] [] // []
//Start = AllPairs [] [1,2,3] // []
//Start = AllPairs [1,2,3] [] // []



