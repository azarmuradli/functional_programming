module pt6gr4
import StdEnv

/*
In a list , If the sum of a number at a certain index from the left and a number at the same index from
the right is more than 10 , then its called a good pair. Find the number of good pairs in a list.

If the list is odd ignore the middle element.

***Should be done without recursion***

eg : [1,2,3,4,5,6,7,8,9,10] has 5 good pairs
    1 + 10 = 11
    2 + 9 = 11
    3 + 8 = 11
    4 + 7 = 11
    5 + 6 = 11

Output : 5 (all pairs are good)

    [2,6,3,5,3,1,5,9,3,1]
    2 + 1 = 3  not good
    6 + 3 = 9  not good
    3 + 9 = 12 good
    5 + 5 = 10 good
    3 + 1 = 4  not good

Output : 2 (3rd and 4th pairs are good)

    [2,6,3,2,3,5,9,3,1]
    2 + 1 = 3  not good
    6 + 3 = 9  not good
    3 + 9 = 12 good
    2 + 5 = 7  not good
    3 should be ignored
    Output : 1 (3rd pair is good)

*/

PTfun :: [Int] -> Int
PTfun x = length(filter(\b = b>10) [(x!!a)+(x!!(length(x)-a-1))\\a<-[0..(length(x)/2)-1]])

//Start = PTfun [1,2,3,4,5,6,7,8,9,10] //5
//Start = PTfun [2,6,3,5,3,1,5,9,3,1] //2
Start = PTfun [2,6,3,2,3,5,9,3,1] //1