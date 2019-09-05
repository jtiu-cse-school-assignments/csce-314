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

marker = ("|", VMarker)
isMarker (x, _) = x == "|"

************************************************************************** eval definition
eval :: WExp -> Memory -> WValue

eval (Val v) m = v

eval (Var varName) m =
  case lookup varName m of
    Nothing -> error $ "Unknown variable " ++ varName ++ " in memory " ++ show m
    Just v -> v

eval (Plus e1 e2) m =
  let e1' = eval e1 m
      e2' = eval e2 m
  in
   VInt $ asInt e1' + asInt e2'

eval (Minus e1 e2) m =
  let e1' = eval e1 m
      e2' = eval e2 m
  in
   VInt $ asInt e1' - asInt e2'

eval (Multiplies e1 e2) m =
  let e1' = eval e1 m
      e2' = eval e2 m
  in
   VInt $ asInt e1' * asInt e2'

eval (Divides e1 e2) m =
  let e1' = eval e1 m
      e2' = eval e2 m
  in
   VInt $ asInt e1' `div` asInt e2'

eval (Equals e1 e2) m = case (s1,s2) of
			 ((VBool b1), (VBool b2)) -> VBool (b1 == b2)
			 ((VInt i1), (VInt i2)) -> VBool (i1 == i2)
			 _ -> error $ "Equality operator asked to compare different types"
			where 
				s1 = (eval e1 m)
				s2 = (eval e2 m)

eval (NotEqual e1 e2) m = case (s1,s2) of
			 ((VBool b1), (VBool b2)) -> VBool (b1 /= b2)
			 ((VInt i1), (VInt i2)) -> VBool (i1 /= i2)
			 _ -> error $ "NotEqual operator asked to compare different types"
			where 
				s1 = (eval e1 m)
				s2 = (eval e2 m)

eval (Less e1 e2) m =
  let e1' = eval e1 m
      e2' = eval e2 m
  in
   VBool $ asInt e1' < asInt e2'

eval (LessOrEqual e1 e2) m =
  let e1' = eval e1 m
      e2' = eval e2 m
  in
   VBool $ asInt e1' <= asInt e2'

eval (Greater e1 e2) m =
  let e1' = eval e1 m
      e2' = eval e2 m
  in
   VBool $ asInt e1' > asInt e2'

eval (GreaterOrEqual e1 e2) m =
  let e1' = eval e1 m
      e2' = eval e2 m
  in
   VBool $ asInt e1' >= asInt e2'

eval (And e1 e2) m = 
	VBool ((asBool (eval e1 m)) && (asBool (eval e2 m)))

eval (Or e1 e2) m = 
	VBool ((asBool (eval e1 m)) || (asBool (eval e2 m)))

eval (Not e) m = 
	VBool (not (asBool (eval e m)))

************************************************************************** exec definition
exec :: WStmt -> Memory -> IO Memory
exec Empty m = m

exec (VarDecl varName varExpr) m | not (definedInThisScope m) = (varName, eval varExpr m) : m
                     	   		 | otherwise = error $ "Variable " ++ varName ++ " already defined in this scope"
    where
      definedInThisScope (hd@(d, _):ds) | isMarker hd = False
                                        | d == s = True
                                        | otherwise = definedInThisScope ds                                              

exec (Assign s e) m = replaceFirstDef (eval e m) m
    where replaceFirstDef _ [] = error $ "Undefined variable " ++ s ++ " in assignment"
          replaceFirstDef v (hd@(n, _):m) | n == s = (n, v):m
                                          | otherwise = hd:replaceFirstDef v m 

exec (If condEx tStm fStm) m = if (asBool (eval condEx m)) then
					exec tStm m
				 else
					exec fStm m

exec (While condEx wStm) m = if (asBool (eval condEx m)) then
								exec (While condEx wStm) m' 
							 else
								m	
							 where m' = (exec wStm m)

exec (Block ss) m = bexec ss (marker:m) >>= \m' -> return (popMarker m')
    where bexec [] m = return m
          bexec (s:ss) m = exec s m >>= \m' ->
                           bexec ss m'
          popMarker [] = []
          popMarker (x:xs) | isMarker x = xs
                           | otherwise = popMarker xs

exec (Print e) m = putStr (show (eval e m)) >> return m

************************************************************************* example programs
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

************************************************************* some useful helper functions
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


********************************************************************** testing expressions

t0 = VarDecl "x" (Val (VInt 0))
t1 = Block
     [
       VarDecl "x" (Val (VInt 0)),
       VarDecl "b" (Greater (Var "x") (Val (VInt 0))),
       VarDecl "x" (Val (VInt 10))
     ]

t2 = Assign "b" (Val (VInt 56))
t3 = If (Plus (Var "a") (Val (VInt 7))) t2 t0
t3' = If (Greater (Var "x") (Val (VInt 7))) t2 t0

{-
 - var a = 4;
 - while a < 10
 -	a = a * 2;
 -}

m2 = [("a", VInt 4), ("|", undefined)]
t4 = While (Less (Var "a") (Val (VInt 10))) (Assign "a" (Multiplies (Var "a") (Val (VInt 2))))


t5 = Block [
       VarDecl "a" (Val (VInt 0)),
       VarDecl "b" (Val (VInt 5)),
       Assign "x" (Plus (Var "a") (Var "b"))
	]

t5' = Block [
       VarDecl "a" (Val (VInt 0)),
       Block [ 
	       Assign "a" (Val (VInt 7)),
	       VarDecl "a" (Val (VInt 10)),
	       Empty
		],
       Assign "x" (Plus (Var "a") (Val (VInt 6)))
	]

{-
	int a = 0;
	{
		int a = 10;
		a = 7;	
		;
	}	
	X = a + 6;
 -}