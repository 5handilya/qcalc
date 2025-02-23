module Main where

import Data.Char (isDigit)

--Expression datatype

data Exp
	= Num Int     -- number (terminal)
	| Add Exp Exp -- mapped to 'y'
	| Sub Exp Exp -- mapped to 'r'
	| Mul Exp Exp -- mapped to 'u'
	| Div Exp Exp -- mapped to 'e'

	deriving Show


--Eval function

eval :: Exp -> Int
eval (Num n) 	= n
eval (Add a b)	= eval a + eval b
eval (Sub a b)	= eval a - eval b
eval (Mul a b)	= eval a * eval b
eval (Div a b)	= eval a `div` eval b

-- PARSING

-- I. Parsing a Factor, which is a number or a paranthesized expression
-- Params : Input to parse (String)
-- Return :  

parseFactor :: String -> (Exp, String)
parseFactor [] = error "Unexpected end of input in factor"
parseFactor (x:xs)
	| isDigit x =					-- non-parantheses, number
		let (digits, rest) = span isDigit (x:xs)
		in  (Num (read digits), rest)
	| x == 'q' = 					-- parantheses, 'q' represents '('
		let (expr, rest) = parseExp xs
		in case rest of
			(c:cs) | c == 'o' -> (expr, cs) -- parantheses, 'o' represents ')'
			_ -> error "Expected closing parantheses 'o'"
	| otherwise = error $ "Unexpected character in factor: " ++ [x]

-- II. Parsing a Term, which handles multiplication and division
-- Params :
-- Return :

parseTerm :: String -> (Exp, String)
parseTerm input =
	let (factor, rest) = parseFactor input
	in parseTerm' factor rest

parseTerm' :: Exp -> String -> (Exp, String)
parseTerm' acc [] = (acc, [])
parseTerm' acc (x:xs)
	| x == 'u' =
		let (factor, rest) = parseFactor xs
		in parseTerm' (Mul acc factor) rest
	| x == 'e' =
		let (factor, rest) = parseFactor xs
		in parseTerm' (Div acc factor) rest
	| otherwise = (acc, x:xs)

-- III. Parsing an Expression, which is a term followed by zero or more + ('y') or - ('r')
-- Params :
-- Return :
parseExp :: String -> (Exp, String)
parseExp input =
	let (term, rest) = parseTerm input
	in parseExp' term rest

parseExp' :: Exp -> String -> (Exp, String)
parseExp' acc [] = (acc, [])
parseExp' acc (x:xs)
	| x == 'y' =
		let (term, rest) = parseTerm xs
		in parseExp' (Add acc term) rest
	| x == 'r' =
		let (term, rest) = parseTerm xs
		in parseExp' (Sub acc term) rest
	| otherwise = (acc, x:xs)


main :: IO ()
main = 	do
	putStrLn "Welcome to Qcalc"
	putStrLn "Enter expression:"
	line <- getLine
	-- removing whitespaces
	let input = filter (/= ' ') line
	-- parse
	let (exp, rest) = parseExp input
	case rest of
		"t" -> print(eval exp)
		_   -> putStrLn "input does not end with eval sym t"

