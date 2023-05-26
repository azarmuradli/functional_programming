module midterm3

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


/* 1. Parasitic Number

 A Parasitic number (in base 10) is a positive number which can be multiplied 
 by a certain n by moving the rightmost digit of its decimal representation 
 to the front.
 e.g. 102564 × 4 = 410256
 Given a positive integer number and n, write a function to determine whether 
 it is a Parasitic number or not.
*/

numDig :: Int Int -> Int
numDig 0 b = b
numDig a b = numDig (a/10) (b+1)

//Start = numDig 123 0


lastDig :: Int -> Int
lastDig x =  x rem 10

//Start = lastDig 123

parasitic :: Int Int -> Bool
parasitic a b = ((lastDig a)* (10^((numDig a 0)-1)) + a/10) == a*b

//Start = parasitic 102564 4 // True
//Start = parasitic 142857 5 // True
//Start = parasitic 714285 8 // False
//Start = parasitic 105263157894736842 2 // True


/* 2. Double Ones

 Given a list of integers, write a function which will keep only the numbers
 that contain at least two '1' digits. For example:
 [1,2,21,121,11,234131,111111,123,0,334] -> [121,11,234131,111111]
*/
countOnes :: Int Int -> Int
countOnes 0 b = b
countOnes a b
| a rem 10 == 1 = countOnes (a/10) (b+1)
= countOnes (a/10) b

//Start = countOnes 1123 0

doubleOne :: [Int] -> [Int]
doubleOne x = filter(\y = (countOnes y 0)>1) x

//Start = doubleOne [1,2,21,121,11,234131,111111,123,0,334] // [121,11,234131,111111]
//Start = doubleOne [12,1,11,33] // [11]
// Start = doubleOne [11,111,21] // [11,111]
//Start = doubleOne [] // []
// Start = doubleOne [21,3,1] // []


/* 3. Multiples

 Given an n>0 integer value, write a function that creates the double, the triple
 and so on n-th multiple of the number.
*/

multiples2 :: [Int] Int -> [Int]
multiples2 [] a = []
multiples2 [x:xs] a = [x*a] ++ multiples2 xs a 

multiple :: Int -> [Int]
multiple x = multiples2 [2..x] x

//Start = multiple 5 // [10,15,20,25]
//Start = multiple 2 // [2]
//Start = multiple 1 // []


/* 4. List difference
 
 Given two lists (A and B) containing sublists of integer numbers, 
 both A and B are of the same length,
 for every sublist in A and B, return the difference of the two sublists.  

 The difference is defined as follows:  
 The List L1-L2 consists of elements that are in L1 but not in L2. 
 For example if L1=[1,2,3] and L2=[3,5], then L1-L2=[1,2].
*/
inList2 :: Int [Int] -> Bool
inList2 a [] = False
inList2 a [x:xs]
| a==x = True
= inList2 a xs

Start = inList2 5 [1,2,3,4,5]

merged :: [[Int]] [[Int]] -> [[Int]]
merged [] [] = []
merged [x:xs] [y:ys] = [x++y] ++ merged xs ys



difference :: [[Int]] [[Int]] -> [[Int]] 
difference x y = filter(\z = ) (merged x y)

//Start = difference [[1..5]] [[4..7]] // [[1,2,3]]
//Start = difference [[1..10] , [10..15] , [1..4]] [[1..10] , [11..20] , [5]] // [[],[10],[1,2,3,4]]
//Start = difference [] [] // [] 

 
/* 5. Replace middle

 Given a list of lists of integers and an integer, write a function that replaces 
 the middle element with the given integer in every sublist. 
*/

repMid :: [[Int]] Int -> [[Int]]
repMid x a = map(\y= take (length(y)/2) y ++ [a] ++ drop ((length(y)/2)+1) y) x

//Start = repMid [[1,2,3],[1..4]] 10 // [[1,10,3],[1,2,10,4]]
//Start = repMid [[1..6], [9,8..1], [(-1),(-2)..(-10)]] 5 
          // [[1,2,3,5,5,6],[9,8,7,6,5,4,3,2,1],[-1,-2,-3,-4,-5,5,-7,-8,-9,-10]]
//Start = repMid [[1,3],[]] 5 // [[1,5],[5]]


/* 6. Primes7

 Given a list of numbers, keep only the prime numbers that end with the digit 7
*/
checkPrime :: [Int] Int -> Bool
checkPrime x 1 = False
checkPrime [] a = True
checkPrime [x:xs] a
| a rem x == 0 = False
= checkPrime xs a

//Start = checkPrime [2,3,4,5,6,7,8,9,10,11]  12

lastDigit7 :: Int -> Bool
lastDigit7 x = ((x rem 10) == 7)


primes7 :: [Int] -> [Int]
primes7 x =  filter(\z = lastDigit7 z) (filter(\y = checkPrime [2..(y-1)] y) x)

//Start = primes7 [1..10] // [7]
//Start = primes7 [1..100] // [7,17,37,47,67,97]
//Start = primes7 [1..6] // []


/* 7. Property check

 Given a list of tuples, write a function to determine
 whether all of the tuples inside of the list hold the (Even, Odd) property.
 [(2,1),(2,3),(4,1)] = True
*/

checkProperty :: (Int,Int) -> Bool
checkProperty (x,y) = (x rem 2 == 0) == (y rem 2 == 1)

//Start = alma (3,5)

holdsTrue :: [(Int, Int)] -> Bool
holdsTrue [] = False
holdsTrue x =  and(map(\y= checkProperty y) x)

//Start = holdsTrue [(2,1),(2,3),(4,1)] // True
//Start = holdsTrue [(1,3),(2,3),(3,4)] // False
//Start = holdsTrue [] // False


/* 8. Super Digit

 We define super digit of an integer x using the following rules.
 If x has only 1 digit, then its super digit is x.
 Otherwise, the super digit of x is equal to the super digit of the digit-sum of x.
 Here, the digit-sum of a number is defined as the sum of its digits.

 E.g  : super_digit(9875) = super_digit(9+8+7+5) 
                          = super_digit(29) 
                          = super_digit(2+9)
                          = super_digit(11)
                          = super_digit(1+1)
                          = super_digit(2)
                          = 2

 Given a list of integers, return a list containing the super digit
 of every number in the list.  
*/
sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits x = (x rem 10) + sumDigits(x/10)

superDigit2 :: Int -> Int
superDigit2 x
| x<10 = x
= superDigit2 (sumDigits x)

//Start = superDigit2 9875

super_digit :: [Int] -> [Int]
super_digit x = map(\y = superDigit2 y) x

//Start = super_digit [148148148 , 9875 ] // [3,2]
//Start = super_digit [884555 , 456 , 2351 , 21587 , 88 ] // [8,6,2,5,7]
//Start = super_digit [] // [] 


/* 9. Powers 
 Given a list of integers and an integer, write a function which returns a list 
 which only contains the powers of the integer.
*/
power :: Int Int -> Bool
power 1 b = True
power a b 
| (a rem b) <>0 = False
= power (a/b) b

//Start = power 23 3

powersList :: [Int] Int -> [Int]
powersList x a = filter(\y = power y a) x

//Start = powersList [2,4,8,16,32,33,55] 2 // [2,4,8,16,32]
//Start = powersList [] 3 // []
//Start = powersList [1..10] 3 // [1,3,9]
//Start = powersList [-1,-2,4,8] 4 // [4]


/* 10. Twin primes
 
 Twin primes is a pair of primes, such that it contains a prime number that is either 
 2 less or 2 more than the pair prime number.
 For example, (41, 43) is a twin prime pair.
 Given a range of numbers left..right write a function that returns the count of 
 twin primes within the range.

 E.g: between 1 and 50 there are 6 pairs of twin prime numbers:
 [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43)].
*/

checkPrime2 :: [Int] Int -> Bool
checkPrime2 x 1 = False
checkPrime2 [] a = True
checkPrime2 [x:xs] a
| a rem x == 0 = False
= checkPrime2 xs a

pairPrimes :: [Int] -> Int
pairPrimes [x] = 0
pairPrimes [x1,x2:xs]
| (x2-x1)==2 = 1 + pairPrimes [x2:xs]
= 0+ pairPrimes [x2:xs]

//Start = pairPrimes [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]

twinPrimes :: Int Int -> Int
twinPrimes a b 
| b<2 = 0
| a<2 = pairPrimes (filter(\y = checkPrime2 [2..(y-1)] y) [2..b])
= pairPrimes (filter(\y = checkPrime2 [2..(y-1)] y) [a..b])


//Start = twinPrimes 1 50 // 6
//Start = twinPrimes 1 1000 // 35
//Start = twinPrimes 0 2 // 0
//Start = twinPrimes 0 -5 // 0

