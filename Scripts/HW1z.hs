--HW1
import Data.Char
-- 1 increase a number by 10 

increaseTen x = 10*x

-- 2 compute the area of a circle of a given radius

circleArea x = 3.14*x*x

-- 3 given a list, return a new list that does not include the first or last element of the original list

midlist x = [ x!!i| i <- [1..((length x)-2)] ]

-- 4 given two numbers where the second is larger than the first, 
--   get a list of integers from the second down to the first (inclusive)

countdownList x y= [ i | i <- [x..y] ]

-- 5 given the lengths of three sides of a triangle in order from smallest to largest,
--   determine whether the triangle is a right triangle

isRight x y z = if x^2+y^2==z^2 then True
				else False
				
-- 6 given two complex numbers (i.e. of the form a+bi), compute their product. 
--   Each complex number should be represented as a tuple with a real part and an imaginary part.

multComplex (x,y) (u,v) = (x*u-y*v, x*v+y*u)

-- 7 given a character and a string, count how many times that character appears in the string (recursive)

countChars _ [] = 0
countChars x y = if x == (head y) then  1+countChars x (tail y)
				 else  countChars x (tail y)
				 
-- 8 from a list of 2-tuples, return a list of just the first elements from the tuples (recursive)

getFirsts [] = []
getFirsts (l:[]) = [fst l]
getFirsts ls = (getFirsts (take 1 ls)) ++ (getFirsts (drop 1 ls))

-- 9 create a new list that contains every other element of an input list 
--   (i.e., retain the first element then drop the next, retain, drop, etc.) (recursive) 

halfList [] = []
halfList (x:xs) = x: halfList(tail xs)

-- 10 given a string, create a list of tuples, where the tuple contains three Boolean variables: 
--    one indicating if it is an upper case letter, one indicating if it is lower case, and one indicating if it is a number. 

uppercaseList x = zip3 [isUpper x][isLower x][isDigit x]

-- Alternating Series
altS :: [Int] -> Int
altS ls = altSHelper (-1) ls 

altSHelper initMult [] = 0
altSHelper initMult ls = (head ls) + initMult*(altSHelper ((initMult)) (tail ls))
