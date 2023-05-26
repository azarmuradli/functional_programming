module AzarMuradliHW7
import StdEnv



:: Tree a = Node a (Tree a) (Tree a) | Leaf
tree1 = Node 7 
						( Node 2 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf)) 
						( Node 20 (Node 12 Leaf Leaf) (Node 4 Leaf Leaf))
						
						
tree2 = Node 5 
						( Node 3 (Node 13 Leaf Leaf) (Node 11 Leaf Leaf)) 
						( Node 1 (Node 7 Leaf Leaf) (Node 9 Leaf Leaf))


/*
    write a function that takes a tree and a list of tuples of the form (a,b), 
    you need to find the node with value a and change its value to b times
    its level in the tree.

    eg:
    
    input: 
             7                    1st level
           /   \                
          2     20               2nd level
         / \    / \ 
       10  30  12  4        3rd level

        [(10,2),(30,3),(4,6),(20,5)]

       output 
                  7
                /   \
               2     10
              / \    / \
            6   9  12   18
    (10,2) => 10 is at level 3, so its value is changed to 2*3 = 6
    (30,3) => 30 is at level 3, so its value is changed to 3*3 = 9
    (4,6) => 4 is at level 3, so its value is changed to 6*3 = 18
    (20,5) => 20 is at level 2, so its value is changed to 5*2 = 10

*/


alma :: (Tree Int) (Int,Int) Int -> (Tree Int)
alma Leaf (a,b) c = Leaf
alma (Node x le ri) (a,b) c
| x==a  = (Node new le ri)
= Node x (alma le (a,b) (c+1)) (alma ri (a,b) (c+1))
where 
	tree = (Node x le ri)
	new = b*c

//Start = alma tree1 (30,3) 1

SwapLevel :: (Tree Int)  [(Int,Int)] -> (Tree Int)
SwapLevel (Node x le ri) [] = Node x le ri
SwapLevel (Node x le ri) [y:ys] = SwapLevel new ys
where
	tree = (Node x le ri) 
	new = alma tree y 1


//Start = SwapLevel tree1 [(10,2),(30,3),(4,6),(20,5)]
//(Node 7 
//          (Node 2 (Node 6 Leaf Leaf) (Node 9 Leaf Leaf)) 
//          (Node 10 (Node 12 Leaf Leaf) (Node 18 Leaf Leaf)))


//Start = SwapLevel tree2 [(13,7),(11,1),(1,5)] 
//(Node 5
//          (Node 3 (Node 21 Leaf Leaf) (Node 3 Leaf Leaf))
//          (Node 10 (Node 7 Leaf Leaf) (Node 9 Leaf Leaf)))





//Task 2

:: Utility = Gas | Electricity
:: University = ELTE | BME | Corvinus 
:: UniRelation = Teacher | Student


:: Citizen = {id::Int, rel::UniRelation, uni::University , utilitySaved::Utility}

/*
In a imaginary universe , budapest has decided to award the citizens who save the most energy with a scholarship.
But the data they recieved is a list of citizens, and they want to know 
how many citizens were chosen from each university and how much scholarship they will get.

Write a function that takes a list of citizens and returns a list of tuples of the form (University, count , TotalScholarship)
where count is the number of citizens from that university who were chosen and TotalScholarship is the total amount of scholarship
allocated to that university.

Scholarship is calculated as follows:
    A student who saved electricity gets 10000 HUF
    A student who saved gas gets 5000 HUF
    A teacher who saved electricity gets 20000 HUF
    A teacher who saved gas gets 10000 HUF

*/

//some test data
citizen1 = {id=1, rel=Student, uni=ELTE, utilitySaved=Electricity} 
citizen2 = {id=2, rel=Student, uni=ELTE, utilitySaved=Gas}
citizen3 = {id=3, rel=Student, uni=BME, utilitySaved=Electricity}
citizen4 = {id=4, rel=Student, uni=BME, utilitySaved=Gas}
citizen5 = {id=5, rel=Student, uni=Corvinus, utilitySaved=Electricity}
citizen6 = {id=6, rel=Student, uni=Corvinus, utilitySaved=Gas}
citizen7 = {id=7, rel=Teacher, uni=ELTE, utilitySaved=Electricity}
citizen8 = {id=8, rel=Teacher, uni=ELTE, utilitySaved=Gas}
citizen9 = {id=9, rel=Teacher, uni=BME, utilitySaved=Electricity}
citizen10 = {id=10, rel=Teacher, uni=BME, utilitySaved=Gas}
citizen11 = {id=11, rel=Teacher, uni=Corvinus, utilitySaved=Electricity}
citizen12 = {id=12, rel=Teacher, uni=Corvinus, utilitySaved=Gas}

instance == University
where
  (==) ELTE ELTE = True
  (==) BME BME = True
  (==) Corvinus Corvinus = True
  (==) a b = False
	
//Start = ELTE == Corvinus

amount :: Utility UniRelation -> Int
amount Electricity Student = 10000
amount Electricity Teacher = 20000
amount Gas Student = 5000
amount Gas Teacher = 10000


scholarship :: University [Citizen] -> Int
scholarship uni x = sum([ amount a.utilitySaved a.rel\\ a<-x | a.uni == uni])

//Start = scholarship ELTE [citizen7,citizen5,citizen2]

count :: University [Citizen] -> Int
count uni x = length([1\\a<-x | a.uni==uni])

//Start = count ELTE [citizen7,citizen5,citizen2]


AwardScholarship :: [Citizen] -> [(University, Int, Int)]
AwardScholarship x = [(a,count a x,scholarship a x)\\ a<-[ELTE,BME,Corvinus]]


//Start = AwardScholarship [citizen1,citizen2,citizen3,citizen4,citizen5,citizen6,citizen7,citizen8,citizen9,citizen10,citizen11,citizen12] // [(ELTE,4,4500),(BME,4,45000),(Corvinus,4,45000)]
Start = AwardScholarship [citizen7,citizen5,citizen2] // [(ELTE,2,25000),(BME,0,0),(Corvinus,1,10000)]