{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive
 
data Tree a = Nil | Node a (Tree a) (Tree a)

-- map
map :: (a -> b) -> Tree a -> Tree b
map f Nil = Nil
map f (Node v l r) = Node (f v) (map f l) (map f r)

-- addRoot
addRoot :: Tree a -> a -> Tree a
addRoot t r = Node r t Nil

-- addLeft
addLeft :: Tree a -> a -> Tree a
addLeft Nil v = Node v Nil Nil
addLeft (Node x Nil r) v = Node x (Node v Nil Nil) r

-- addRight
addRight :: Tree a -> a -> Tree a
addRight Nil v = Node v Nil Nil
addRight (Node x l Nil) v = Node x l (Node v Nil Nil)

-- turn left
turnLeft :: Tree a -> Tree a
turnLeft Nil = Nil
turnLeft (Node x l Nil) = Node x l Nil
turnLeft (Node x Nil r) = Node x Nil r
turnLeft (Node x l (Node xr rl rr)) = Node xr (Node x l rl) rr

-- turn right
turnRight :: Tree a -> Tree a
turnRight Nil = Nil
turnRight (Node x l Nil) = Node x l Nil
turnRight (Node x Nil r) = Node x Nil r
turnRight (Node x (Node xl ll lr) r) = Node xl ll (Node x lr r)

--foldl
foldl :: (a -> b -> a) -> a -> Tree b -> a
foldl f z Nil = z
foldl f z (Node x Nil Nil) = f z x
foldl f z (Node x l Nil) = f (foldl f z l) x
foldl f z (Node x Nil r) = foldl f (f z x) r
foldl f z (Node x l r) = foldl f (f (foldl f z l) x) r

--foldr
foldr :: (a -> b -> b) -> b -> Tree a -> b
foldr f z Nil = z
foldr f z (Node x Nil Nil) = f x z
foldr f z (Node x l Nil) = foldr f (f x z) l
foldr f z (Node x Nil r) = f x (foldr f z r)
foldr f z (Node x l r) =  f x (foldr f (foldr f (f x z) l) r)
