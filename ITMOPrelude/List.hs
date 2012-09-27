{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length (Cons x xs) = natOne +. (length xs)

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
(Cons x xs) ++ y = Cons x (xs ++ y)
Nil ++ y = y

-- Список без первого элемента
tail :: List a -> List a
tail (Cons x xs) = xs

-- Список без последнего элемента
init :: List a -> List a
init (Cons x (Nil)) = Nil
init (Cons x xs) = (Cons x (Nil)) ++ (init xs)
--init x = reverse (tail (reverse x))

-- Первый элемент
head :: List a -> a
head (Cons x xs) = x

-- Последний элемент
last :: List a -> a
last (Cons x (Nil)) = x
last (Cons x xs) = last (xs)

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero xs = Nil
take (Succ n) (Cons x xs) = (Cons x (Nil)) ++ (take n xs)

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop Zero xs = xs
drop (Succ n) (Cons x xs) = (drop n xs)



-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter f (Cons x xs) = if' (f x) ((Cons x (Nil)) ++ (filter f xs)) (filter f xs)
filter f Nil = Nil


-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter f (Cons x xs) = myadd (gfilter f xs) (f x)
	where
	myadd :: List b -> Maybe b -> List b
	myadd xs Nothing = xs
	myadd xs (Just x) = (Cons x xs)


-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile f (Cons x xs) = if' (f x) ((Cons x (Nil)) ++ (takeWhile f xs)) Nil
takeWhile f Nil = Nil

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile f (Cons x xs) = if' (f x) (dropWhile f xs) (Cons x xs)
dropWhile f Nil = Nil

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
--span p xs = Pair (takeWhile p xs) (dropWhile p xs)
span f (Cons x xs) = if' (f x) (Pair ((Cons x (Nil)) ++ (fst (span f xs))) (snd (span f xs))) (Pair Nil (Cons x xs))
span f Nil = Pair Nil Nil

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break f (Cons x xs) = if' (not (f x)) (Pair ((Cons x (Nil)) ++ (fst (break f xs))) (snd (break f xs))) (Pair Nil (Cons x xs))
break f Nil = Pair Nil Nil

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons x xs) !! Zero = x
(Cons x xs) !! (Succ n) = xs !! n

-- Список задом на перёд
reverse :: List a -> List a
reverse (Cons x xs) = (reverse xs) ++ (Cons x (Nil))


-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences xs = (subseqtoend xs) ++ (subseqtoend (init xs))
	where
	subseqtoend :: List a -> List (List a)
	subseqtoend Nil = Nil
	subseqtoend (Cons x xs) = (Cons (Cons x xs) (subseqtoend(xs)))


-- insert the element at the i-th (or the last) position of the second list. Append the first list to the beginning
insert :: Nat -> a -> List a -> List a -> List a
insert Zero a xs ys = xs ++ (Cons a ys)
insert i a xs Nil = xs ++ (Cons a Nil)
insert i a xs (Cons y ys) = insert (i -. natOne) a (xs ++ (Cons y Nil)) ys


-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations Nil = Nil
permutations (Cons x Nil) = (Cons (Cons x Nil) Nil)
permutations (Cons x xs) = insertAtEveryPosForList (permutations xs) x
	where
	insertAtEveryPos :: List a -> a -> Nat -> List (List a)
	insertAtEveryPos str elem Zero = Cons (insert Zero elem Nil str) Nil
	insertAtEveryPos str elem pos = (insertAtEveryPos str elem (pos -. natOne)) ++ (Cons (insert pos elem Nil str) Nil)

	insertAtEveryPosForList :: List (List a) -> a -> List (List a)
	insertAtEveryPosForList (Cons x Nil) elem = insertAtEveryPos x elem (length x)
	insertAtEveryPosForList (Cons x xs) elem = (insertAtEveryPosForList xs elem) ++ (insertAtEveryPos x elem (length x))


-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' xs = permutations xs

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = (Cons a (Nil)) ++ (repeat a)

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f z (Cons x Nil) = f z x
foldl f z l = foldl f (f z (head l)) (tail l)

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f z Nil = Nil
scanl f z l = (Cons (f z (head l)) Nil) ++ (scanl f (f z (head l)) (tail l))

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z (Cons x Nil) = f x z
foldr f z l = foldr f (f (last l) z) (init l)

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr f z Nil = Nil
scanr f z l = (Cons (f (last l) z) Nil) ++ (scanr f (f (last l) z) (init l))


-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons x xs) = (Cons (f x) Nil) ++ (map f xs)

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons x xs) = x ++ (concat xs)

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap f Nil = Nil
concatMap f (Cons x xs) = (f x) ++ (concatMap f xs)

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip Nil ys = Nil
zip xs Nil = Nil
zip (Cons x xs) (Cons y ys) = (Cons (Pair x y) Nil) ++ (zip xs ys)

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f Nil ys = Nil
zipWith f xs Nil = Nil
zipWith f (Cons x xs) (Cons y ys) = (Cons (f x y) Nil) ++ (zipWith f xs ys)
