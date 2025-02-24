module Main where

import Data.Char (isDigit)
import System.Environment (getArgs)
import Control.Exception (catch, SomeException)
import System.IO(hFlush, stdout)

--Expression datatype

data Exp
	= Num Int     -- number (terminal)
	| Add Exp Exp -- mapped to 'y'
	| Sub Exp Exp -- mapped to 'r'
	| Mul Exp Exp -- mapped to 'u'
	| Div Exp Exp -- mapped to 'e'
	| Pow Exp Exp -- mapped to 'i'

	deriving Show

--Eval function

eval :: Exp -> Int
eval (Num n) 	= n
eval (Add a b)	= eval a + eval b
eval (Sub a b)	= eval a - eval b
eval (Mul a b)	= eval a * eval b
eval (Pow a b)	= eval a ^ eval b
eval (Div a b)	= eval a `div` eval b

-- PARSING

-- I. Parsing a Factor, which is a number or a paranthesized expression
-- Params : Input to parse (String)
-- Return :  

parseFactor :: String -> Either String (Exp, String)
parseFactor [] = Left "Unexpected end of input in factor"
parseFactor (x:xs)
	| isDigit x =					-- non-parantheses, number
		let (digits, rest) = span isDigit (x:xs)
		in  Right (Num (read digits), rest)
	| x == 'o' = 					-- parantheses, 'o' represents '('
		case parseExp xs of
			Left err -> Left err
			Right (expr, rest) ->
				case rest of
					(c:cs) | c == 'p' -> Right (expr, cs) -- parantheses, 'p' represents ')'
					_ -> Left "Expected closing parantheses 'p'"
	| otherwise = Left $ "Unexpected character in factor: " ++ [x]

-- II. Parsing a Term, which handles mul, div, exp
-- Params : Input Term String
-- Return : Error String or Expression + Remaining String
parseTerm :: String -> Either String (Exp, String)
parseTerm input = do
	(power, rest) <- parsePow input
	parseTerm' power rest
parseTerm' :: Exp -> String -> Either String (Exp, String)
parseTerm' acc [] = Right (acc, [])
parseTerm' acc (x:xs)
	| x == 'u' = do
		(factor, rest) <- parsePow xs
		parseTerm' (Mul acc factor) rest
	| x == 'e' = do
		(factor, rest) <- parsePow xs
		parseTerm' (Div acc factor) rest
	| otherwise = Right (acc, x:xs)

-- III. Parse exponentiation
-- Params :
-- Return :
parsePow :: String -> Either String (Exp, String)
parsePow input = do
	(fact, rest) <- parseFactor input
	case rest of
		('i':rest') ->	do
			(powExp, rest'') <- parsePow rest'
			return (Pow fact powExp, rest'')
		_ -> return (fact, rest)

-- IV. Parsing an Expression, which is a term followed by zero or more + ('y') or - ('r')
-- Params :
-- Return : String (Error message) or Expression, String (Remaining input)
parseExp :: String -> Either String (Exp, String)
parseExp input = do
	(term, rest) <- parseTerm input
	parseExp' term rest

parseExp' :: Exp -> String -> Either String (Exp, String)
parseExp' acc [] = Right (acc, [])
parseExp' acc (x:xs)
	| x == 'y' = do
		(term, rest) <- parseTerm xs
		parseExp' (Add acc term) rest
	| x == 'r' = do
		(term, rest) <- parseTerm xs
		parseExp' (Sub acc term) rest
	| otherwise = Right (acc, x:xs)

-- HELPERS

-- I. Input Cleaning, Parser Calling
calculate :: String -> Either String Int
calculate input =
    let filtered = filter (/= ' ') input
	in do
		(exp, rest) <- parseExp filtered
		if null rest
			then return (eval exp)
			else Left ("Unparsed input remaining: " ++ rest)

-- III. REPL Loop
-- Line by line eval
repl :: IO()
repl = do
	putStr "ℚ "
	hFlush stdout 
	line <- getLine
	case calculate line of
		Left err -> do
				putStrLn ("ℚ Error: " ++ err)
				repl
		Right res -> do
				putStrLn ("ℚ = " ++ show res)
				repl

------------------------------------------------------------------------
-- III. MAIN 
-- 		Two modes: 	
--		1. CLI with args "qcalc 6e4"
--		2. REPL 

main :: IO ()
main = 	do
	args <- getArgs
	if not (null args)
		then do
			let exp = concat args
			case calculate exp of
				Left err  -> putStrLn ("Error: " ++ err)
				Right res -> print res
		else do
			putStrLn "ℚ-CALC v.0.1"
			putStrLn "Enter the expression to be evaluated:"
			repl