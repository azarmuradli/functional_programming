module pt1gr4
import StdEnv

/*
 Write a function that takes an integers does the following :
    1. If the number is positive , make it negative and triple it
    2. If the number is negative , make it positive and double it
    3. If the number is zero , abort the program with the message "Zero is not allowed".

*/

PTfun :: Int -> Int
PTfun x
| x>0 = -3*x
| x<0 = -2*x
= abort "Zero is not allowed"



//Start = PTfun 0 // "Zero is not allowed"
//Start = PTfun 1 // -3
Start = PTfun -1 // 2