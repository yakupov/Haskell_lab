-- REPL for untyped lambda calculus
module UnTyLambda.REPL where
import Data.List
import Monstupar
import UnTyLambda.Interpreter

-- Парсим строку в терм
parseLambda :: Monstupar Char Term
parseLambda = parseL1NotEmpty

--Implementation
parseDelim :: Monstupar Char Char
parseDelim = oneOf [' ', '\t']

parseManyDelim :: Monstupar Char [Char]
parseManyDelim = many parseDelim

parseDigit :: Monstupar Char Char
parseDigit = oneOf ['0'..'9']

parseLetter :: Monstupar Char Char
parseLetter = oneOf (['a'..'z'] ++ ['A'..'Z'])

parseNameTerm :: Monstupar Char Term
parseNameTerm = do
	name <- parseName
	return (Var name)

parseName :: Monstupar Char [Char]
parseName = do
	name <- many1 (parseLetter <|> parseDigit)
	return name

parseGroup :: Monstupar Char Term
parseGroup = do
	char '('
	parseManyDelim
	expr <- parseL1NotEmpty
	parseManyDelim
	char ')'
	return expr

parseL2 :: Monstupar Char Term
parseL2 = do
	char '\\'
	parseManyDelim
	name <- parseName
	parseManyDelim
	char '.'
	parseManyDelim
	expr <- parseL1NotEmpty
	return (Lam name expr)

parseGeneralL2 :: Monstupar Char Term
parseGeneralL2 = parseL2 <|> parseGroup <|> parseNameTerm

parseL1NotEmpty :: Monstupar Char Term
parseL1NotEmpty = do
	parseManyDelim
	expr1 <- parseGeneralL2
	parseManyDelim
	exprOther <- parseGeneralL1
	return (foldl App expr1 exprOther)

parseGeneralL1 :: Monstupar Char [Term]
parseGeneralL1 = (do
	parseManyDelim
	expr1 <- parseGeneralL2
	parseManyDelim
	exprOther <- parseGeneralL1
	return (expr1:exprOther)) <|> (return [])


--------------------------------------------------------------------------------
-- Заметье, что грамматика лямбда-выражений леворекурсивна.
-- Перед тем как бросаться кодить, сначала уберите леворекурсивность
-- (неопределённость тоже стоит убрать) на бумаге, а потом напишите
-- получившуюся грамматику в EBNF вот сюда:
-- 
--  L2 = \ [Name] . [SomeLambdaExpr]  OR  ([SomeLambdaExpr])  OR  [SomeLiteral]
--  L1 = L2  OR  App L2 L1
-- 
-- прямо сюда, да
--------------------------------------------------------------------------------

-- Красиво печатаем терм (можно с лишними скобками, можно без)
prettyPrint :: Term -> String
prettyPrint (Var x) = x
prettyPrint (Lam x xt) = "\\" ++ x ++ " . " ++ prettyPrint xt
prettyPrint (App t1 t2) = prettyPrint t1 ++ " " ++ prettyPrint t2

-- Собственно сам REPL. Первый аргумент — максимальное число итераций при
-- попытке нормализации стратегией из второго аргумента.
replLoop :: Integer -> (Integer -> Term -> Term) -> IO ()
replLoop patience strategy = do
	putStr "$ "
	expr <- getLine
	case runParser parseLambda expr of
		Left _ -> putStrLn "Incorrect expression"
		Right (_, term) -> putStrLn (prettyPrint (strategy patience term))
	replLoop patience strategy

-- Диалог с (replLoop 100 no) должен выглядеть так:
-- > \x . (\y . y) x x
-- \x . x x
