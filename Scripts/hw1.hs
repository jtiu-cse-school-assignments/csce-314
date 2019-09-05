-- ********************************************************************* hw1

-- ************************************************************* increaseTen

increaseTen x = x + 10

-- ************************************************************** circleArea

circleArea r = pi * r * r

-- ***************************************************************** midList

midList [] = []
midList ls = tail (init ls)

-- *********************************************************** countdownList
-- given two numbers where the second is larger than the first, get a list     of integers from the second down to the first (inclusive)

countdownList :: Int -> Int -> [Int]
countdownList x y = reverse  [x..y]

-- ***************************************************************** isRight
-- given the lengths of three sides of a triangle in order from smallest to    largest, determine whether the triangle is a right triangle 

isRight x y z = if ( (x*x) + (y*y) ) == z*z then True
                else False

-- ************************************************************* multComplex
-- given two complex numbers (i.e. of the form a+bi), compute their product.   Each complex number should be represented as a tuple with a real part and   an imaginary part.

-- multComplex (a,b) (c,d) = a*b + c*d 

-- *************************************************************** countChar-- given a character and a string, count how many times that character         appears in the string (recursive)

countChars     :: Char -> [Char] -> Int
countChars _ [] = 0

countChars c s  = if c == (head s) then 1+countChars c (tail s)
                                   else countChars c (tail s)

-- *************************************************************** getFirsts
-- from a list of 2-tuples, return a list of just the first elements from      the tuples (recursive) 

getFirsts [] = []
--getFirsts ls = fst (getFirsts' ls)
--getFirsts' ls = fst (getFirsts ls)

-- **************************************************************** halfList
-- create a new list that contains every other element of an input list        (i.e., retain the first element then drop the next, retain, drop, etc.)     (recursive)

halfList [] = []
halfList (x:y:xs) = x : halfList xs
halfList a = a

-- *********************************************************** upperCaseList
-- given a string, create a list of tuples, where the tuple contains three     Boolean variables: one indicating if it is an upper case letter, one        indicating if it is lower case, and one indicating if it is a number.
-- For this, you must first include the line "import Data.Char" at the top     of your source file.

uppercaseList [] = []
uppercaseList (x:xs) = (a,b,c) : uppercaseList xs
                        where a = isUpper x
                              b = isLower x
                              c = isDigit x

-- ****************************************************** Alternating Series

altSeries :: [Int] -> Int

altSeries [] = 0
altSeries ls = (head ls) - altSeries' (tail ls)

altSeries' [] = 0
altSeries' ls = (head ls) - altSeries (tail ls)
