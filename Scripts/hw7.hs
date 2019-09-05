-- assignment 7, CSCE-314

module Main where

import Prelude hiding (lookup)

import Test.HUnit
import System.Exit

-- AST definition for W
data WValue = VInt Int 
            | VBool Bool 
              deriving (Eq, Show)

data WExp = Val WValue

          | Var String

          | Plus WExp WExp
          | Minus WExp WExp
          | Multiplies WExp WExp
          | Divides WExp WExp

          | Equals WExp WExp
          | NotEqual WExp WExp
          | Less WExp WExp
          | Greater WExp WExp
          | LessOrEqual WExp WExp
          | GreaterOrEqual WExp WExp

          | And WExp WExp
          | Or WExp WExp
          | Not WExp

data WStmt = Empty
           | VarDecl String WExp
           | Assign String WExp
           | If WExp WStmt WStmt
           | While WExp WStmt
           | Block [WStmt]

type Memory = [(String, WValue)]
marker = ("|", undefined)
isMarker (x, _) = x == "|"

-- eval function
eval :: WExp -> Memory -> WValue
eval (Val v) m = v
eval (Var varName) m = case lookup varName m of
                       Nothing -> error $ "Variable \""++varName++"\" is used before it is declared"
                       Just val -> val

eval (Plus e1 e2) m = VInt ((asInt (eval e1 m)) + (asInt (eval e2 m))) 
eval (Multiplies e1 e2) m = VInt ((asInt (eval e1 m)) * (asInt (eval e2 m)))
eval (Minus e1 e2) m = VInt ((asInt (eval e1 m)) - (asInt (eval e2 m)))
eval (Divides e1 e2) m = VInt ((asInt (eval e1 m)) `div` (asInt (eval e2 m)))

eval (Equals e1 e2) m = case (s1, s2) of
						((VBool b1), (VBool b2)) -> VBool (b1 == b2)
						((VInt i1), (VInt i2)) -> VBool (i1 == i2)
						_ -> error $ "Equality operator asked to compare different types"
						where 
							s1 = (eval e1 m)
							s2 = (eval e2 m)
eval (NotEqual e1 e2) m = case (s1, s2) of
						((VBool b1), (VBool b2)) -> VBool (b1 /= b2)
			 			((VInt i1), (VInt i2)) -> VBool (i1 /= i2)
			 			_ -> error $ "NotEqual operator asked to compare different types"
			 			where 
							s1 = (eval e1 m)
							s2 = (eval e2 m)
							
eval (Less e1 e2) m = VBool ((asInt (eval e1 m)) < (asInt (eval e1 m)))
eval (Greater e1 e2) m = VBool ((asInt (eval e1 m)) > (asInt (eval e1 m)))
eval (LessOrEqual e1 e2) m = VBool ((asInt (eval e1 m)) <= (asInt (eval e1 m)))
eval (GreaterOrEqual e1 e2) m = VBool ((asInt (eval e1 m)) >= (asInt (eval e1 m)))

eval (And e1 e2) m = VBool ((asBool (eval e1 m)) && (asBool (eval e2 m)))
eval (Or e1 e2) m = VBool ((asBool (eval e1 m)) || (asBool (eval e2 m)))
eval (Not e) m = VBool (not (asBool (eval e m)))


-- exec function
exec :: WStmt -> Memory -> Memory
exec = undefined

-- example programs
factorial = 
  Block
  [
    VarDecl "acc" (Val (VInt 1)),
    While (Greater (Var "x") (Val (VInt 1)))
    (
      Block
      [
        Assign "acc" (Multiplies (Var "acc") (Var "x")),
        Assign "x" (Minus (Var "x") (Val (VInt 1)))         
      ]
    ),
    Assign "result" (Var "acc")
  ]

p1 = Block
     [
       VarDecl "x" (Val (VInt 0)),
       VarDecl "b" (Greater (Var "x") (Val (VInt 0))),
       If (Or (Var "b") (Not (GreaterOrEqual (Var "x") (Val (VInt 0)))))
         ( Block [ Assign "x" (Val (VInt 1)) ] )
         ( Block [ Assign "x" (Val (VInt 2)) ] )
     ]

-- some useful helper functions
lookup s [] = Nothing
lookup s ((k,v):xs) | s == k = Just v
                    | otherwise = lookup s xs

asInt (VInt v) = v
asInt x = error $ "Expected a number, got " ++ show x

asBool (VBool v) = v
asBool x = error $ "Expected a boolean, got " ++ show x

fromJust (Just v) = v
fromJust Nothing = error "Expected a value in Maybe, but got Nothing"

-- unit tests
myTestList =

  TestList [
    test $ assertEqual "p1 test" [] (exec p1 []),

    let res = lookup "result" (exec factorial [("result", undefined), ("x", VInt 10)])
    in test $ assertBool "factorial of 10" (3628800 == asInt (fromJust res))
    ]    

-- main: run the unit tests  
main = do c <- runTestTT myTestList
          putStrLn $ show c
          let errs = errors c
              fails = failures c
          if (errs + fails /= 0) then exitFailure else return ()

-- testing expressions
e1 :: WExp
e1 = (Val (VInt 42))
e1' = (Val (VBool True))
e2 = (Plus (Val (VInt 12)) (Val (VInt 1)))
e2' = (Plus (Val (VInt 12)) (Val (VBool True)))
e3 = (Multiplies (Val (VInt 2)) e2)
e4 = (Var "b")
e4' = (Multiplies (Val (VInt 2)) (Var "c"))

f1 = (Val (VInt 42))
f1' = (Val (VBool True))

m1 = [("c", VInt 3), ("b", VInt 2), ("|", undefined), ("a", VInt 1), ("|", undefined)]
m2 = [("a", VInt 4), ("|", undefined)]
