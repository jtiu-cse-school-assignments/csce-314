-- ********************************************************************* crt-- Chinese Remainder Theorem
-- Criteria can be found online

crt [(a1,n1), (a2,n2), (a3,n3)] = (head (list4), (n1*n2*n3))
                       where
                        list1 = [x | x <- [1..(n1*n2*n3)], x `mod` n1 == a1]
                        list2 = [y | y <- [1..(n1*n2*n3)], y `mod` n2 == a2]
                        list3 = [z | z <- [1..(n1*n2*n3)], z `mod` n3 == a3]
                        list4 = [x | x <- list1, y <- list2, z <- list3,                                     y == x, z == x]

-- ************************************************************** kcomposite
-- Write a function using list comprehension that, given k produces the        ordered (infinite) list of positive k-composite numbers.

kcomposite :: Int -> [Int]
kcomposite k = [x | x <- [1..]]

lista = [(x,y) | x <- [1..100], y <- [1..100], 0 == x `mod` y]
