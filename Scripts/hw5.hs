data Tree a b = Leaf a | Branch b (Tree a b) (Tree a b)

mytree = Branch "A" (Branch "B" (Leaf (1::Int)) (Leaf (2::Int))) (Leaf (3::Int))

mytree2 = Branch "A" (Leaf (1::Int)) (Leaf (2::Int))

-- ************************************************************ Showing Tree

instance (Show a, Show b) => Show (Tree a b) where
    show (Leaf i) = show i
    show (Branch x ltree rtree) = show x ++ "\n      "  ++ show ltree ++ "      " ++ show rtree

-- ********************************************************* Tree Traversals

f a = show a
g b = b

preorder :: (a -> c) -> (b -> c) -> Tree a b -> [c] 
preorder f g (Leaf a) = [f a]
preorder f g (Branch b l r) = (g b : preorder f g l) ++ preorder f g r

postorder :: (a -> c) -> (b -> c) -> Tree a b -> [c]
postorder f g (Leaf a) = [f a]
postorder f g (Branch b l r) = postorder f g l ++ postorder f g r ++ [g b]

inorder   :: (a -> c) -> (b -> c) -> Tree a b -> [c]
inorder f g (Leaf a) = [f a]
inorder f g (Branch b l r) = (inorder f g l) ++ (g b : inorder f g r)


