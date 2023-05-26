module pt8gr4
import StdEnv


/*
    write an instance * for two strings 
    such that the result has all the consonants of both strings in order
    i.e consonants of first string followed by consonants of second string
    * repetition of consonants is allowed

    * consonants are all letters except a,e,i,o,u

    eg:
    "abc" * "def" = "bcdf"

    "Functional" * "Programming" = "FnctnlPrgrmmng"
*/

instance * String
where (*) a b = { x \\ x<-:(a+++b) | not(isMember x ['a','e','i','o','u'])}





//Start = "abc" * "def" // "bcdf"
//Start = "Functional" * "Programming" // "FnctnlPrgrmmng"

