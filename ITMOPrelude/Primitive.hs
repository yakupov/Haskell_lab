{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp (Succ x) (Succ y) = natCmp x y
natCmp (Succ x) Zero = GT
natCmp Zero (Succ y) = LT
natCmp Zero Zero = EQ

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
(Succ n) -. (Succ m) = n -. m
(Succ n) -. Zero = n
Zero -. m = Zero

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Division
(/.) :: Nat -> Nat -> Nat
(/.) Zero n = Zero
(/.) m Zero = Zero
(/.) m n = natOne +. ((m -. n) /. n)

-- Mod
natMod :: Nat -> Nat -> Nat
natMod m n = m -. ((m /. n) *. n)

-- Целое и остаток от деления n на m
--natDivMod :: Nat -> Nat -> Pair Nat Nat
--natDivMod n m = undefined

--natDiv n = fst . natDivMod n -- Целое
--natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd x y = gcd y (natMod x y)

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Plus Nat | Minus Nat deriving (Show,Read)

intZero   = Plus Zero
intOne    = Plus (Succ Zero)
intNegOne = Minus (Succ Zero)

-- n -> - n
intNeg :: Int -> Int
intNeg (Plus x) = Minus x
intNeg (Minus x) = Plus x

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp (Plus Zero) (Minus Zero) = EQ
intCmp (Minus Zero) (Plus Zero) = EQ
intCmp (Plus Zero) (Minus (Succ x)) = GT
intCmp (Minus Zero) (Plus (Succ x)) = LT
intCmp (Plus (Succ x)) (Minus Zero) = GT
intCmp (Minus (Succ x)) (Plus Zero) = LT
intCmp (Plus x) (Plus y) = natCmp x y
intCmp (Minus x) (Minus y) = natCmp y x


intEq :: Int -> Int -> Bool
intEq (Plus Zero) (Minus Zero) = True
intEq (Minus Zero) (Plus Zero) = True
intEq (Plus Zero) (Minus (Succ x)) = False
intEq (Minus Zero) (Plus (Succ x)) = False
intEq (Plus (Succ x)) (Minus Zero) = False
intEq (Minus (Succ x)) (Plus Zero) = False
intEq (Plus x) (Plus y) = natEq x y
intEq (Minus x) (Minus y) = natEq x y


intLt :: Int -> Int -> Bool
intLt (Plus Zero) (Minus Zero) = False
intLt (Minus Zero) (Plus Zero) = False
intLt (Plus Zero) (Minus (Succ x)) = False
intLt (Minus Zero) (Plus (Succ x)) = True
intLt (Plus (Succ x)) (Minus Zero) = False
intLt (Minus (Succ x)) (Plus Zero) = True
intLt (Plus x) (Plus y) = natLt x y
intLt (Minus x) (Minus y) = natLt y x


infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
(Plus m) .+. (Plus n) = Plus (m +. n)
(Minus m) .+. (Minus n) = Minus (m +. n)
(Plus (Succ m)) .+. (Minus (Succ n)) = (Plus m) .+. (Minus n)
(Minus (Succ m)) .+. (Plus (Succ n)) = (Plus m) .+. (Minus n)
x .+. (Plus Zero) = x
x .+. (Minus Zero) = x
(Plus Zero) .+. y = y
(Minus Zero) .+. y = y

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
(Plus m) .*. (Plus n) = Plus (m *. n)
(Minus m) .*. (Minus n) = Plus (m *. n)
(Plus m) .*. (Minus n) = Minus (m *. n)
(Minus m) .*. (Plus n) = Minus (m *. n)

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat (Plus x) y) = Rat (Plus y) x
ratInv (Rat (Minus x) y) = Rat (Minus y) x

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat a b) (Rat c d) = intCmp (a .*. (Plus d)) (c .*. (Plus b))

ratEq :: Rat -> Rat -> Bool
ratEq (Rat a b) (Rat c d) = intEq (a .*. (Plus d)) (c .*. (Plus b))

ratLt :: Rat -> Rat -> Bool
ratLt (Rat a b) (Rat c d) = intEq (a .*. (Plus d)) (c .*. (Plus b))


internalRatPlus :: Rat -> Rat -> Rat
internalRatPlus (Rat a b) (Rat c d) = Rat ((a .*. (Plus d)) .+. (c .*. (Plus b))) (b *. d)

internalRatShorten :: Rat -> Rat
internalRatShorten (Rat (Plus a) b) = Rat (Plus (a /. (gcd a b))) (b /. (gcd a b))
internalRatShorten (Rat (Minus a) b) = Rat (Minus (a /. (gcd a b))) (b /. (gcd a b))

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
n %+ m = internalRatShorten (internalRatPlus n m)

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat a b) %* (Rat c d) = Rat (a .*. c) (b *. d)

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
