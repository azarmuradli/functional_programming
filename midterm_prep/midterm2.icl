module midterm2

import StdEnv 


// Please fill the data required below.
//<Name>
//<Neptun_code>
//Functional Programming & mid-term
//2021.September.14 
//This solution was submitted and prepared by <Name, Neptun_code> 
//for the mid-term assignment of the Functional Programming course.
//I declare that this solution is my own work.
//I have not copied or used third party solutions.
//I have not passed my solution to my classmates, neither made it public.
//Students’ regulation of Eotvos Lorand University 
//(ELTE Regulations Vol. II. 74/C.) 
//states that as long as a student presents another student’s work - 
//or at least the significant part of it - as his/her own performance, 
//it will count as a disciplinary fault. 
//The most serious consequence of a disciplinary fault can be 
//dismissal of the student from the University.


/* 1. Armstrong number

 If sum of cubes of each digit of the number is equal to the number itself,
 then the number is called an Armstrong number.
 153 = 1^3 + 5^3 + 3^3
 Given a positive integer number, write a function to determine whether it is
 an Armstrong number or not.
*/

sumall :: Int -> Int
sumall 0 = 0
sumall x = (x rem 10)^3 + sumall (x/10)

armstrong :: Int -> Bool
armstrong x = sumall x == x

//Start = armstrong 153 // True
//Start = armstrong 370 // True
//Start = armstrong 0 // True
//Start = armstrong 12 // False


/* 2. Occurrences

 Given a list of integers, replace every element in the list with its number
 of occurrences in the list.
*/

countOccur :: Int [Int] Int -> Int
countOccur a [] b = b
countOccur a [x:xs] b
| a==x = countOccur a xs (b+1)
= countOccur a xs b

//Start = countOccur 1 [1,1,1,1,2,3,2,5,6,2,2,2,5] 0 

occNum :: [Int] -> [Int]
occNum x = map(\y = countOccur y x 0 ) x

//Start = occNum [1,1,1,1,2,3,2,5,6,2,2,2,5] // [4,4,4,4,5,1,5,2,1,5,5,5,2]
//Start = occNum [1..5] // [1,1,1,1,1]
// Start = occNum ([1..5] ++ [1..7]) // [2,2,2,2,2,2,2,2,2,2,1,1]
// Start = occNum([7..9] ++ [7..9] ++ [7..9]) // [3,3,3,3,3,3,3,3,3]


/* 3. Gap2
 
 Given a list of numbers, convert the list in such a way that 
 the difference between two consecutive elements is always 2,
 you may have to add numbers in order to achieve that.
 e.g: [1,5,8] = [1,3,5,7,9]
*/

gap2 :: [Int] -> [Int]
gap2 [] = []
gap2 x 
| ((x!!0) rem 2)==((x!!(length (x)-1)) rem 2) = [(x!!0),((x!!0)+2)..(x!!(length (x)-1))]
= [(x!!0),((x!!0)+2)..((x!!(length (x)-1))+1)]
//Start = gap2 [1,5,8] // [1,3,5,7,9]
//Start = gap2 [1,5,15] // [1,3,5,7,9,11,13,15]
//Start = gap2 [] 


/* 4. Not Palindrome
 Given a list of lists of integers,
 write a function that gets rid of Palindrome numbers.
 A palindrome number is a number that can be read from left to right or
 from right to left and gets the same number, 
 e.g. 12521 is a palindrome number. 
*/

checkNum :: Int -> Int
checkNum 0 = 0
checkNum x = 1 + checkNum (x/10)


checkPalin :: Int -> Int
checkPalin 0 = 0
checkPalin x = (x rem 10)* (10^((checkNum x)-1)) + checkPalin (x/10)

//Start = checkPalin 1

notPalin :: [Int] -> [Int]
notPalin [] = []
notPalin [x:xs]
| x<10 = [x] ++ notPalin xs
| (checkPalin x) <> x = [x] ++ notPalin xs
= [] ++ notPalin xs

//Start = notPalin [1,2,1111]


getRidPal :: [[Int]] -> [[Int]]
getRidPal x = map(\y = notPalin y) x

//Start = getRidPal [[1,2,1111],[151,22,3455]] // [[1,2],[3455]]
//Start = getRidPal [[1,222],[151,202,50505]] // [[1],[]]
//Start = getRidPal [[],[22]] // [[],[]]


/* 5. Not Primes
 Given a list of integers, write a function which removes the prime numbers   from the list.
 There will be no negative integers and consider the number 1 not a prime.
*/
notPrime :: Int [Int] ->Bool
notPrime a [] = False
notPrime a [x:xs]
| a rem x == 0 = True
= notPrime a xs

checkPrime :: Int -> Bool
checkPrime x 
| x == 1 = True
= (notPrime x [2..(x-1)])

//Start = checkPrime 12

removeNotPrime :: [Int] -> [Int]
removeNotPrime x = filter(\y = checkPrime y) x

//Start = removeNotPrime [1..10] // [1,4,6,8,9,10]
//Start = removeNotPrime [13..20] // [14,15,16,18,20]
//Start = removeNotPrime [2,4,8,9,10,23] // [4,8,9,10]
//Start = removeNotPrime [] // []


/* 6. zipWith

 Implement the function zipWith that takes a function, 
 and two lists, and combines them in such a way that 
 elements that are in the same positions get the function 
 applied to them.

 E.g: zipWith addTwoNumbers [1,2,3] [5,6,7] = [1+5,2+6,3+7] = [6,8,10]
*/
//DON'T DELETE THESE FUNCTIONS !!!
//addTwoNumber x y = x + y
//prodTwoNumber x y = x * y
//niceTwoNumber x y = x rem y
//

//zipWith :: (Int Int -> Int) [Int] [Int] -> [Int]

//Start = zipWith addTwoNumber [1,2,3] [5,6,7] // [6,8,10]
//Start = zipWith prodTwoNumber [1,2,3] [5,6,7] // [5,12,21]
//Start = zipWith niceTwoNumber [5,6,7] [1,2,3] // [0,0,1]


/* 7. Collatz conjecture

 Given a positive number greater than 1, return how many iterations does it 
 take for that number to fall down to "2" if we keep applying the
 Collatz equation on it.
 Collatz conjecture equation:
 If the number is even -> x/2
 If the number is odd -> 3x+1
 e.g: input: 10 
      steps: 10 -> 5 -> 16 -> 4 -> 2
      output: 4 recursive calls
*/

numCollatz :: Int Int -> Int
numCollatz 2 b = b-1
numCollatz a b
| a rem 2 ==0 = numCollatz (a/2) (b+1)
= numCollatz (3*a+1) (b+1)

collatz :: Int -> Int
collatz x
| x < 1 = abort "The number must be greater than 1"
=numCollatz x 0

//Start = collatz 10 // 4
//Start = collatz 20000000 // 144
//Start = collatz 5 // 3
//Start = collatz 0 // "The number must be greater than 1"


/* 8. Good Lists

 Given a list of lists, count how many of the sublists are good lists.
 A list is good if it is empty or its 1st number is odd, 2nd is even, 
 3rd is odd, 4th is even and so on.
 E.g: [[],[1,2,3,4],[8,3,4],[9],[3,4,4]] your function should return
 3 as only [], [1,2,3,4] and [9] are "good".
*/

good2 :: [Int] -> Bool
good2 [x] = x rem 2 == 1
good2 [] = True
good2 [x1,x2:xs]
| (x1 rem 2 == 1) && (x2 rem 2 ==0) = good2 xs
= False

//Start = good2 [2,3,4]

isGood :: [[Int]] -> Int
isGood [] = 0
isGood [x:xs]
| good2 x = 1 + isGood xs
= 0 + isGood xs

//Start = isGood [[],[1,2,3,4],[8,3,4],[9],[3,4,4]] // 3
//Start = isGood [[1,2,3,4],[3,4,4],[3,42],[12,2,1,2]] // 2
//Start = isGood [[],[1,2,3,4],[],[8,3,4],[1],[2],[9],[3,4,4]] // 5
//Start = isGood [] // 0


/* 9. Symmetrical lists
 Given a list of lists of integers, write a function 
 which returns a list of symmetrical lists, 
 if the sum of the sublist is greater than 10.
*/

sumList :: [Int] -> Int
sumList [] = 0
sumList [x:xs] = x + sumList xs

symSumGreater10 :: [[Int]] -> [[Int]]
symSumGreater10 [] = []
symSumGreater10 [x:xs]
| sumList x > 10 = [x ++ reverse x] ++ symSumGreater10 xs
= symSumGreater10 xs

//Start = symSumGreater10 [[1,2,3],[3,4,5,6],[4,5,1,2]] // [[3,4,5,6,6,5,4,3],[4,5,1,2,2,1,5,4]]
//Start = symSumGreater10 [] // []
//Start = symSumGreater10 [[1..10],[1,2]] // [[1,2,3,4,5,6,7,8,9,10,10,9,8,7,6,5,4,3,2,1]]

 
/* 10. Elements in interval

 Given a list of triple tuples consisting of two integer values and 
 and a list of integers (left,right,[Int]),
 for every tuple return only the elements from the list 
 which positions' are inside the interval [left..right]
 Assume that the indexes are all valid.
*/

banan :: (Int,Int,Int) -> Int
banan (x1,x2,x3) = x1

//Start = banan (1,2,3)

interval :: (Int,Int,[Int]) -> [Int]
interval (a,b,x)
| a<=b = [x!!a] ++ interval ((a+1),b,x)
= []

//Start = interval (2,5,[1..10])

elementInInterval :: [(Int ,Int,[Int])]-> [[Int]]
elementInInterval x = map(\y = interval y) x

//Start = elementInInterval [(2,5,[1..10])] //[[3,4,5,6]]
//Start = elementInInterval [(5,6,[1..8]), (3,5,[4..9])] //[[6,7],[7,8,9]]
//Start = elementInInterval [(4,7,[1,2,3,4,5,6,7,8,9])] //[[5,6,7,8]]

