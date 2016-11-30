module Scheme where
import Data.Char

operators = "+-*/#";
procedures=["double"]

type Context = Dictionary String Int
type Memory = Dictionary Int Ast

data Ast = Number Double | Block [Ast] | 
           If Ast Ast Ast | Let String Ast | Lambda String Ast | Boolean String |
           Procedure String
           
           deriving (Eq, Show, Ord)


tokenize::String -> [String]
tokenize [] = []
tokenize (x:xs) | isDigit x = [x:(takeWhile isDigit xs)] ++ (tokenize (dropWhile isDigit xs))
                | elem x operators  = [x:(takeWhile isAlpha xs)] ++ (tokenize (dropWhile isAlpha xs))
                | isAlpha x =  [x:(takeWhile isAlpha xs)] ++ (tokenize (dropWhile isAlpha xs))
                | x == '(' || x == ')' = [x]:tokenize xs
                | x == ';' = [x]:tokenize xs
                | otherwise = tokenize xs


eval::[String] -> Context -> Memory -> (Ast, [String], Context, Memory)
eval ("(":xs) con mem = eval xs con mem
eval (")":xs) con mem = eval xs con mem

-- Operators
eval ("+":xs) con mem = if (head leftOvers) /= ")"
                        then error "Expected closing parantheses"
                        else (Number (num1 + num2), tail leftOvers, con2, mem2) -- remove last parantheses with tail
                        where   
                            (Number num1, rest, con1, mem1) = eval xs con mem
                            (Number num2, leftOvers, con2, mem2) = eval rest con1 mem1

eval ("-":xs) con mem = if (head leftOvers) /= ")"
                        then error "Expected closing parantheses"
                        else (Number (num1 - num2), tail leftOvers, con2, mem2) -- remove last parantheses with tail
                        where   
                            (Number num1, rest, con1, mem1) = eval xs con mem
                            (Number num2, leftOvers, con2, mem2) = eval rest con1 mem1

eval ("/":xs) con mem = if (head leftOvers) /= ")"
                        then error "Expected closing parantheses"
                        else (Number (num1 / num2), tail leftOvers, con2, mem2) -- remove last parantheses with tail
                        where   
                            (Number num1, rest, con1, mem1) = eval xs con mem
                            (Number num2, leftOvers, con2, mem2) = eval rest con1 mem1

eval ("*":xs) con mem = if (head leftOvers) /= ")"
                then error "Expected closing parantheses"
                else (Number (num1 * num2), tail leftOvers, con2, mem2) -- remove last parantheses with tail
                where   
                    (Number num1, rest, con1, mem1) = eval xs con mem
                    (Number num2, leftOvers, con2, mem2) = eval rest con1 mem1

eval ("double":xs) con mem = let (Number num, leftOvers, con1, mem1) = eval xs con mem in 
                    if (head leftOvers /= ")")
                    then error "Expected closing parantheses" 
                    else (Number (2*num), tail leftOvers, con1, mem1)

eval ("square":xs) con mem = (Number (num*num), tail leftOvers, con1, mem1)
                    where   
                        (Number num, leftOvers, con1, mem1) = eval xs con mem


-- If implementation
eval ("if":xs) con mem =    if bool == "#t"
                    then (expr1, tail rest2, con2, mem2)
                    else (expr2, tail rest2, con3, mem3)
                    where 
                        (Boolean bool, rest, con1, mem1) = (eval xs con mem)
                        (expr1, rest1, con2, mem2) = eval rest con1 mem1
                        (expr2, rest2, con3, mem3) = eval rest1 con1 mem1
               
eval ("#t":xs) con mem = (Boolean "#t", xs, con, mem)
eval ("#f":xs) con mem = (Boolean "#f", xs, con, mem)

------

-- Let implementation
eval ("let":xs) con mem =   if (head expression == "(")
                            then error "There seems to be to many parantheses after let"
                            else if (head body == ")") && (head (tail body) /= ")") -- means there are more let expressions
                                 then eval ("let":body) con2 mem2
                                 else eval body con2 mem2
                            where 
                                expression = tail $ tail xs -- removed starting parantheses
                                (varName) = head expression
                                (value, body, con1, mem1) = evalExpr (tail expression) con mem
                                memoryIndex = length mem1
                                (con2, mem2) = (insert varName memoryIndex con1, insert memoryIndex value mem1)
                    

eval (x:xs) con mem | isDigit (head x) = (Number (read x), xs, con, mem)
                    | isAlpha (head x) && exist x con && not (isProcedure value) = (value, xs, con, mem)  
                    | isAlpha (head x) && exist x con && isProcedure value = let (Procedure name) = value in eval (name:xs) con mem            
                    | isAlpha (head x) && not (exist x con) = error $ "The variable " ++ x ++ " does not exist in scope"
                    | otherwise = eval xs con mem
                    where
                        memoryIndex = find x con
                        value = find memoryIndex mem

isProcedure::Ast -> Bool
isProcedure (Procedure _) = True
isProcedure _ = False

evalExpr::[String] -> Context -> Memory -> (Ast, [String], Context, Memory)
evalExpr (x:xs) con mem = if elem (head x) operators || elem x procedures -- TODO: senere sjekk om det generelt er en metode, ikke bare operator
                          then (Procedure x, xs, con, mem)
                          else eval (x:xs) con mem
                          where
                              memoryIndex = length mem
                              (updatedCon, updatedMem) = (insert x memoryIndex con, insert memoryIndex (Procedure x) mem)

                            



-- Takes scheme code as argument an runs it
run::String -> Ast
run program = first $ eval (tokenize program) [] []

first::(Ast, [String], Context, Memory) -> Ast
first (first, second, third, fourth) = first


--Dictionary type
type Dictionary n v = [(n,v)]

find :: Eq n => Show n => n -> Dictionary n v -> v
find n [] = error ("Variable " ++ show n ++ " is not defined.")
find n ((n',v'):xs) = if n == n' then v' else find n xs

exist::Eq n => n -> Dictionary n v -> Bool
exist n [] = False
exist n ((n',v'):xs) = if n == n' then True else exist n xs 

update :: Eq n => n -> v -> Dictionary n v -> Dictionary n v
update n v [] = []
update n v ((n',v'):xs) = if n == n' then (n,v):xs else (n',v'):update n v xs

insert :: Eq n => n -> v -> Dictionary n v -> Dictionary n v
insert n v xs = ((n,v):xs)