-- ********************************************************* Set Constructor
-- Write a recursive function that constructs a set.
-- Constructing a set from a list simply means removing all duplicate          values. (Hint: Use isElement in the definition.)

type Set a = [a]

mkSet :: Eq a => [a] -> Set a
mkSet [] = []
mkSet (x:xs) | elem x xs = mkSet xs
             | otherwise = x : mkSet xs

-- ****************************************************************** Subset
-- Write a recursive function subset, such that subset a b returns True if a   is a subset of b and False otherwise. (Hint: you should write a function    that can determine if some value is a member of a set.)

subset :: Eq a => Set a -> Set a -> Bool
subset [] _ = True
subset _ [] = False
subset (x:xs) b | x `elem` b     = subset xs b
                | otherwise      = False

-- CounterExample:
-- * subset [1,2,3,4,4,4] [1,2,3,3,3,3,4]
-- returns True even though 'a' is not a set because it has duplicates

-- ************************************************************ Set Equality
-- Using subset you have already defined, write a function setEqual that       returns True if the two sets contain exactly the same elements, and False   otherwise.

setEqual :: Eq a => Set a -> Set a -> Bool
setEqual a b | (subset a b) && (subset b a) == True = True
             | otherwise                            = False

-- ************************************************************* Set Product
-- The product of two sets A and B is the set consisting of all pairs draw     from either set, where the pairs are ordered having elements (ai, bj).      The first element is from A and the second from B.

{-
setProd :: (Eq t, Eq t1) => Set t -> Set t1 -> Set (t, t1)
setProd [] _ = []
setProd _ [] = []
setProd (x:xs) b = map (function1 x) b 

function1 a b = (a, b)
-}

setProd :: (Eq t, Eq t1) => Set t -> Set t1 -> Set (t, t1)
setProd xs ls = [(x,y) | x <- xs, y <- ls]

-- *********************************************************** Set Partition
-- The partition of a set S is defined as a set of nonempty, pairwise          disjoint subsets of S whose union is S. For example, the set                {red, green, blue} can be partitioned in 5 ways:

{-
 - Set Partition Problem
 Example:
	[ 1 ] = [ [ [1] ] ] 
	[ 1, 2 ] = [ [ [1], [2] ], [ [1,2] ] ]
	[ 1, 2, 3 ] = [ [ [1], [2], [3] ], [ [1,2], [3] ] , -- outside
			[ [1,3], [2] ], [ [1], [2,3] ], [ [1,2,3] ] ] -- inside

-}

--setPart (x:[]) = [[[ x ]]]
--setPart (x:xs) = outside x (setPart xs) ++ inside x (setPart xs)

outside element set = [ [element]:i | i <- set ]
