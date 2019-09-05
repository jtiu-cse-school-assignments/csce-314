module Main where
  
    import System.Environment (getArgs)
    import Control.Monad (when)
          
    import W
    import WParser

    main = do
      args <- getArgs -- get the command line arguments
        
      let (a:as) = args
      let debug = a == "-d"
      let fileName = if debug then head as else a
        
      str <- readFile fileName
        
      let prog = parse wprogram str 
        
      let ast = case prog of  
                  [(p, [])] -> p
                  [(_, inp)] -> error ("Unused program text: " 
                                       ++ take 256 inp) -- this helps in debugging
                  [] -> error "Syntax error"
                  _ -> error "Ambiguous parses"
      result <- exec ast []
          
      when debug $ print "AST:" >> print prog
      when debug $ print "RESULT:" >> print result
W.hs(eval and exec)
module W ( WValue(..), 
           WExp(..), 
           WStmt(..), 
           exec, 
           eval) where

    data WValue = VInt Int 
                | VBool Bool 
                | VString String
                | VMarker
                  deriving Eq
        
    instance Show WValue where
        show (VInt i) = show i
        show (VBool b) = show b
        show (VString s) = s 
        show (VMarker) = "_" 

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
                deriving Show

    data WStmt = Empty 
               | VarDecl String WExp
               | Assign String WExp
               | If WExp WStmt WStmt
               | While WExp WStmt
               | Block [WStmt]
               | Print WExp
                 deriving Show

    -- some useful helper functions
    asInt (VInt v) = v 
    asInt x = error $ "Expected a number, got " ++ show x
        
    asBool (VBool v) = v 
    asBool x = error $ "Expected a boolean, got " ++ show x

    ----------
    -- eval --
    ----------
    type Memory = [(String, WValue)]
        
    marker = ("|", VMarker)
    isMarker (x, _) = x == "|"

    eval :: WExp -> Memory -> WValue

    eval (Val v) _ = v

    eval (Var s) m =
      case lookup s m of
        Nothing -> error $ "Unknown variable " ++ s ++ " in memory " ++ show m
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

    eval (Equals e1 e2) m =
      let e1' = eval e1 m
          e2' = eval e2 m
      in
       VBool $ e1' == e2'

    eval (NotEqual e1 e2) m = VBool $ not $ asBool $ eval (Equals e1 e2) m

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

    eval (And e1 e2) m | not (asBool (eval e1 m)) = VBool False
                       | otherwise = VBool (asBool (eval e2 m))

    eval (Or e1 e2) m | asBool (eval e1 m) = VBool True
                      | otherwise = VBool (asBool (eval e2 m))

    eval (Not e) m = VBool $ not $ asBool $ eval e m

    ----------
    -- exec --
    ----------
    exec :: WStmt -> Memory -> IO Memory
    exec Empty m = return m

    exec (VarDecl s e) m | not (definedInThisScope m) = return $ (s, eval e m) : m
                         | otherwise = error $ "Variable " ++ s ++ " already defined in this scope"
        where
          definedInThisScope (hd@(d, _):ds) | isMarker hd = False
                                            | d == s = True
                                            | otherwise = definedInThisScope ds

    exec (Assign s e) m = return $ replaceFirstDef (eval e m) m
        where replaceFirstDef _ [] = error $ "Undefined variable " ++ s ++ " in assignment"
              replaceFirstDef v (hd@(n, _):m) | n == s = (n, v):m
                                              | otherwise = hd:replaceFirstDef v m

    exec (If e s1 s2) m | eval e m == VBool True = exec s1 m
                        | eval e m == VBool False = exec s2 m
                        | otherwise = error "Non-boolean in condition of if"

    exec (While e s) m | eval e m == VBool True = exec s m >>= \m' ->
                                                  exec (While e s) m'
                       | eval e m == VBool False = return m
                       | otherwise = error "Non-boolean in condition of while"

    exec (Block ss) m = bexec ss (marker:m) >>= \m' -> return (popMarker m')
        where bexec [] m = return m
              bexec (s:ss) m = exec s m >>= \m' ->
                               bexec ss m'
              popMarker [] = []
              popMarker (x:xs) | isMarker x = xs
                               | otherwise = popMarker xs

    exec (Print e) m = putStr (show (eval e m)) >> return m
Interpreter
module WParser ( parse,
                 wprogram ) where

    import Data.Char
    import W

    import Control.Applicative (Applicative(..))
    import Control.Monad (liftM, ap) 

    -----------------------------
    -- This is the main parser --
    -----------------------------
    wprogram = whitespace >> many stmt >>= \ss -> return (Block ss) 
    -- a program is a sequence of statements; the parser returns them
    -- as a single block-statement

    -- only two of the statement types above are supported, the rest are undefined.
    -- please implement them
    stmt = varDeclStmt +++ assignStmt +++ ifStmt +++ whileStmt +++ 
           blockStmt +++ emptyStmt +++ printStmt

    emptyStmt = 
      symbol ";" >>
      return Empty
  
    printStmt = 
      keyword "print" >>
      expr >>= \e ->
      symbol ";" >>
      return (Print e)

    varDeclStmt = 
      keyword "var" >>
      whitespace >>
      identifier >>= \v ->
      whitespace >>
      symbol "=" >>
      whitespace >>
      expr >>= \e ->
      symbol ";" >>                                                               
      return (VarDecl v e)   

    assignStmt = failure
    ifStmt = failure
    whileStmt = failure
    blockStmt = failure                     

    -- the only kind of expression supported for now is stringLiterals
    -- implement the full expression language of W
    expr = stringLiteral 

    -- stringLiterals can contain \n characters
    stringLiteral = char ('"') >>
                    many stringChar >>= \s ->
                    char ('"') >>
                    whitespace >>
                    return (Val (VString s)) 

    stringChar = (char '\\' >> char 'n' >> return '\n') 
                 +++ sat (/= '"')

    ----------------------
    -- Parser utilities --
    ----------------------

    keywords = words "var if else while"
    isKeyword s = s `elem` keywords

    keyword s = 
      identifier >>= \s' ->
      if s' == s then return s else failure

    newtype Parser a = P (String -> [(a, String)])

    parse :: Parser a -> String -> [(a, String)]
    parse (P p) inp = p inp

    instance Functor Parser where
        fmap = liftM

    instance Applicative Parser where
        pure  = return
        (<*>) = ap

    instance Monad Parser where
        -- return :: a -> Parser a
        return v = P $ \inp -> [(v, inp)]

        -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        p >>= q = P $ \inp -> case parse p inp of
                                [] -> []
                                [(v, inp')] -> let q' = q v in parse q' inp'

    failure :: Parser a
    failure = P $ \_ -> []

    item :: Parser Char
    item = P $ \inp -> case inp of
                         (x:xs) -> [(x, xs)]
                         [] -> []

    -- Parse with p or q
    (+++) :: Parser a -> Parser a -> Parser a
    p +++ q = P $ \inp -> case parse p inp of
                              [] -> parse q inp
                              [(v, inp')] -> [(v, inp')]


    -- Simple helper parsers
    sat :: (Char -> Bool) -> Parser Char
    sat pred = item >>= \c ->
               if pred c then return c else failure

    digit, letter, alphanum :: Parser Char
    digit = sat isDigit
    letter = sat isAlpha
    alphanum = sat isAlphaNum

    char :: Char -> Parser Char
    char x = sat (== x)

    string = sequence . map char

    many1 :: Parser a -> Parser [a]
    many1 p = p >>= \v ->
              many p >>= \vs ->
              return (v:vs)

    many :: Parser a -> Parser [a]
    many p = many1 p +++ return []

    -- Useful building blocks
    nat :: Parser Int
    nat = many1 digit >>= \s ->
          whitespace >>
          return (read s)

    identifier :: Parser String
    identifier = letter >>= \s ->
                 many alphanum >>= \ss ->
                 whitespace >>
                 return (s:ss)

    whitespace :: Parser ()
    whitespace = many (sat isSpace) >> comment

    symbol s =
        string s >>= \s' ->
        whitespace >>
        return s'

    comment = ( string "//" >>
                many (sat (/= '\n')) >>
                whitespace ) +++ return ()

    parens p =
        symbol "(" >>
        p >>= \res ->
        symbol ")" >>
        return res

