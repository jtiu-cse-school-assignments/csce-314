-- *************************************************************** myReverse
-- given a list, reverse it. (recursive)

myReverse [] = []
myReverse (x:xs) = myReverse xs ++ x 

-- *************************************************************** isElement

isElement e [] = False
isElement e ls  | (head ls) == e    = True
                | otherwise         = isElement e (tail ls)

-- *************************************************************** duplicate
-- duplicate the elements of a list. For example, duplicate [1,2] returns      [1,1,2,2]. (recursive)

duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs

-- ********************************************************* removeDuplicate
-- given a sorted list, remove the duplicated elements in this list.           (recursive)

removeDuplicate [] = []
removeDuplicate (x:xs)    | x `elem` xs = removeDuplicate xs
--                          | otherwise   = x : xs 
                          | otherwise   = x : removeDuplicate xs

-- ****************************************************************** rotate
-- rotate a list n places to the left, where n is an integer. For example,     rotate "abcde" 2 returns "cdeab".

rotate [] n = [] 
rotate ls n = drop n ls ++ take n ls

-- ***************************************************************** flatten
-- flatten a list of lists into a single list formed by concatenation. For     example, flatten [[1,2],[3,4],[5,6]] returns [1,2,3,4,5,6]. (recursive)

flatten [] = []
flatten (x:xs) = x ++ flatten xs

-- ************************************************************ isPalindrome
-- given a list, judge if it is a palindrome.

isPalindrome ls = if reverse ((take ((length ls) `div` 2) ls)) == (drop (((length ls) `div` 2)+1) ls) then True else False

isPalindrome1 ls = reverse ((take ((length ls) `div` 2) ls))
isPalindrome2 ls = drop (((length ls) `div` 2)+1) ls

-- ***************************************************************** coPrime
-- given two positive integer numbers, determine whether they are coprime.     Two numbers are coprime if their greatest common divisor equals 1.          Hint: Euclid's algorithm. (recursive)

coPrime' a b  
             | a == 0    = b
             | b == 0    = a
             | otherwise = coPrime' b (a `mod` b)
coPrime  a b
             | coPrime' a b == 1    = True
             | otherwise            = False

-- *********************************************************** Aaah! (Easy)
-- When we go to see a doctor, the doctor always asks us to say "aaah".        Sometimes, the doctor needs us to say "aaaaaah", but we are only able to    say "aaah". In that case, the doctor is unable to diagnose our disease,     because the 'a's in our "aaah" are fewer than his or her requirements.      Now, write a Haskell function called seeDoctor to judge if the doctor can   diagnose us with our "aah". The input of the function consists of two       strings. The first string is the "aaaah" the doctor needs and the second    string is the "aah" we are able to say. Output "True" if our "aah" meets    the requirements of the doctor, and output "False" otherwise. The test      should pass with a "True" only when lowercase 'a's and 'h's are used,       and each string contains a certain number of 'a's followed by a single      'h'.

seeDoctor :: String -> String -> Bool
seeDoctor a b = if a == b then True else False

-- ************************************************************* waterGate
-- There are n water gates in a reservoir that are initially closed. To        adjust water in the reservoir, we open/close the water gates according to   the following rule: on the first day, we open all the gates. Then, on the   second day, we close every second gate. On the third day, we change the     state of every third gate (open it if it's closed or close it if it's       open) ... For the ith day, we change the state of every i gate. Finally,    for the nth day, we change the state of the last gate. Our question is,     how many gates are open after n days?

waterGate n = floor (sqrt(n))

-- ************************************************************* goldbachNum
-- Goldbach's other conjecture: Christian Goldbach once proposed that every    odd composite number can be written as the sum of a prime and twice a       square. However, this conjecture turns out to be false. Write a Haskell     function called goldbachNum to find the smallest odd composite that does    not match Goldbach's other conjecture.
{-
primeTest :: Integer -> Bool
primeTest 1 = False
primeTest t = and [(gcd t i) == 1 | i <- [2..t-1]]

twiceSquares :: Integer -> [Integer]
twiceSquares n = takeWhile (<n) [ 2 * x^2 | x <- [1..n]]

oddList = map (\x -> 2*x + 1) [0..]
allOddComp = [o | o <- drop 1 oddList, not (primeTest o)]

satsConds n = or [primeTest k | k <- map ( (\x -> (n-x)) twiceSquares n)] 

goldbachNum = head [ x | x <- allOddComp, not (satsConds x) ] 
-}
