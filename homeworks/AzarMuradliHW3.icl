module AzarMuradliHW3
import StdEnv

//Please Rename the file as YourNameHW3.icl

/*

    You're safe online if all your passwords are strong.
    A password is strong if it is at least 8 characters long and contains at 
    least one uppercase letter, one lowercase letter, one digit and 
    one special character. 
    The special characters are: !@#$%^&*()_+-=[]{}|;':",./<>?~`

    Write a function strongPassword that takes a list of passwords and checks if 
    all your passwords are strong.

    Eg : Input : ["Hello@World9", "HelloWorld1!", "HelloWorld1!"]
        Output : "All passwords are strong"

        Input : ["JohnDow","Hellom","yotoo@123"]
        Output : "All passwords are not strong"

        Hint : use the function fromString x to convert string x to a list of char.

        **** Please dont put your own passwords as a test case ****
*/

strList :: String -> [Char]
strList x = fromString x


checkLength :: [Char] -> Bool
checkLength x = length x >=8


checkUpper :: [Char] -> Bool
checkUpper [] = False
checkUpper [x:xs]
| toInt x >=65 && toInt x <=90 = True
= checkUpper xs

checkLower :: [Char] -> Bool
checkLower [] = False
checkLower [x:xs]
| toInt x >=97 && toInt x <=122 = True
= checkLower xs

checkDigit :: [Char] -> Bool
checkDigit [] = False
checkDigit [x:xs]
| toInt x >=48 && toInt x <=57 = True
= checkDigit xs

checkSpecial :: [Char] -> Bool
checkSpecial [] = False
checkSpecial [x:xs]
| (toInt x >=123 && toInt x <=126) || (toInt x >=33 && toInt x <=47) || (toInt x >=58 && toInt x <=64) || (toInt x >=91 && toInt x <=96) = True
= checkSpecial xs



//Start = checkSpecial ['H','e','l','l','o','@','W','o','r','l','d','9']

strongPassword :: [String] -> String
strongPassword [] = "All passwords are strong"
strongPassword [x:xs]
| checkLength y && checkUpper y && checkLower y && checkDigit y && checkSpecial y = strongPassword xs
= "All passwords are not strong"
where y = strList x




//Start = strongPassword ["Hello@World9", "HelloWorld1!", "Helloworld@123"] // "All passwords are strong"
//Start = strongPassword ["JohnDow","Hellom","yotoo@123"] // "All passwords are not strong"


/*
    Write a function that takes a list of Real numbers and returns a list of lists 
    where each sublist contains two numbers where first number is the element from the list 
    and the second number is the percentage of frequency of that element in the list.

    eg : Input : [1,2,1,3] 
        Output : [[1,50],[2,25],[3,25]]
        explanation : 1 -> occurs 2 times in the list , total elements in the list = 4 so its percentage is 50
                    2 -> occurs 1 time in the list , total elements in the list = 4 so its percentage is 25
                    3 -> occurs 1 time in the list , total elements in the list = 4 so its percentage is 25

        percentage as integer is fine
*/

percentage :: Int [Int] Int Int -> [Int]
percentage a [] b c = [a,b*100/c]
percentage a [x:xs] b c
| a==x = percentage a xs (b+1) c
= percentage a xs b c

rest :: [Int] [[Int]] -> [[Int]]
rest a [] = []
rest a [x:xs]
| a==x = rest a xs
= [x] ++ rest a xs

//Start = rest [1,50] [[1,50],[2,25],[3,25]]

dropDup :: [[Int]] -> [[Int]]
dropDup [] = []
dropDup [x:xs] = [x] ++ dropDup (rest x xs)

//Start = dropDup [[1,50],[2,25],[3,25]]

frequency :: [Int] -> [[Int]]
frequency x = dropDup (map(\a = percentage a x 0 c ) x)
where c = length x




Start = frequency [1,2,1,3] // [[1,50],[2,25],[3,25]]
//Start = frequency [1,1,1,3,2,2] // [[1,50],[2,33],[3,16]]
//Start = frequency [1,2,5,5,1,1,0,0] // [[1,37],[2,12],[5,25],[0,25]]