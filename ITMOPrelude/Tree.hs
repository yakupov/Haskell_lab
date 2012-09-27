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
-- what exaxtly do we need??
