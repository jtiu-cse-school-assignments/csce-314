data E = IntLit Int
       | BoolLit Bool
       | Plus E E
       | Minus E E
       | Multiplies E E
       | Exponentiate E E
       | Equals E E
         deriving (Eq, Show)

program = Equals
            (Plus (IntLit 1) (IntLit 2))
            (Minus
              (IntLit 5)
              (Minus (IntLit 3) (IntLit 1)))

program' = Equals
            (Plus (IntLit 1) (IntLit 2))
            (Minus
              (IntLit 5)
              (Exponentiate (IntLit 3) (IntLit 1)))

{-
eval :: E -> E
eval (IntLit i) = IntLit i
eval (BoolLit b) = BoolLit b
eval (Plus p1 p2) = Plus (eval p1) (eval p2)
eval (Minus m1 m2) = Minus (eval m1) (eval m2)
eval (Multiplies mu1 mu2) = Multiplies (eval mu1) (eval mu2)
eval (Exponentiate e1 e2) = Exponentiate (eval e1) (eval e2)
eval (Equals eq1 eq2) = Equals (eval eq1) (eval eq2)
-}

val :: E -> Int
val (IntLit i) = i
eval :: E -> E
--eval (BoolLit b) = BoolLit b
eval (Plus p1 p2) = Plus (val p1) (valp2)
eval (Minus m1 m2) = (val m1) - (val m2)
eval (Multiplies mu1 mu2) = (val mu1) * (val mu2)
eval (Exponentiate e1 e2) = (val e1) ^ (val e2)
--eval (Equals eq1 eq2) = Equals (eval eq1) (eval eq2)



