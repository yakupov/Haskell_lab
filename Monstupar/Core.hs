-- Extremely simple but monstrously stupid (and slow) monadic parser
-- combinator library
module Monstupar.Core
    ( ParseError(..)
    , Monstupar, runParser
    , ok, isnot, eof, (<|>), like
    ) where

--------------------------------------------------------------------------------
-- Определения

-- Тело этого определения можно заменить на всё, что захочется
data ParseError = ParseError
                deriving (Show) -- лишь бы show был

newtype Monstupar s a = Monstupar { runParser :: [s] -> Either ParseError ([s], a) }

instance Monad (Monstupar s) where
    return a = Monstupar $ \s -> Right (s , a)
    ma >>= f = Monstupar $ \s -> case (runParser ma s) of
					Left x -> Left x
					Right (s, a) -> runParser (f a) s
					

--------------------------------------------------------------------------------
-- Примитивные парсеры.
-- Имена и сигнатуры функций менять нельзя, тела можно

-- Всё хорошо
ok :: Monstupar s ()
ok = Monstupar $ \s -> Right (s , ())

-- Не должно парситься парсером p
isnot :: Monstupar s () -> Monstupar s ()
isnot p = Monstupar $ \s -> case runParser p s of
    Left e -> Right (s , ())
    Right _ -> Left ParseError

-- Конец ввода
eof :: Monstupar s ()
eof = Monstupar $ \s -> case s of
    [] -> Right (s , ())
    _  -> Left ParseError

infixr 2 <|>
-- Сначала первый парсер, если он фейлится, то второй
(<|>) :: Monstupar s a -> Monstupar s a -> Monstupar s a
a <|> b = Monstupar $ \s -> case runParser a s of
					Left x -> runParser b s
					Right x -> Right x

-- В голове ввода сейчас нечто, удовлетворяющее p
like :: (s -> Bool) -> Monstupar s s
like p = Monstupar $ \s -> case s of
					[] -> Left ParseError
					x -> if p (head x)
						then Right ((tail x), (head x))
						else Left ParseError



-- Сюда можно добавлять ещё какие-то примитивные парсеры
-- если они понадобятся

