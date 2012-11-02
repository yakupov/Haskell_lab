{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree
-- всевозможные инстансы для классов ниже

--------------------------------------------------------------------------------
-- Классы
class Category cat where
    id  :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

(>>) :: Monad m => m a -> m b -> m b
ma >> mb = ma >>= (\_ -> mb)

--------------------------------------------------------------------------------
-- Инстансы писать сюда

-- As long as nothing from itmo-prelude is suitable for category...
instance Category (->) where
	id f = f
	(.) f g x = f (g x)

instance Functor List where
	fmap = ITMOPrelude.List.map

instance Functor Tree where
	fmap = ITMOPrelude.Tree.map

instance Monad List where
	return x = Cons x ITMOPrelude.List.Nil
	(>>=) ITMOPrelude.List.Nil f = ITMOPrelude.List.Nil
	(>>=) (Cons x xs) f = (f x) ++ ((>>=) xs f)

instance Monad Tree where
	return x = Node x ITMOPrelude.Tree.Nil ITMOPrelude.Tree.Nil
	(>>=) ITMOPrelude.Tree.Nil f = ITMOPrelude.Tree.Nil
	(>>=) (Node x l r) f = Node (extractElement (f x)) ((>>=) l f) ((>>=) r f)
		where
		extractElement (Node x l r) = x

instance Monad Maybe where
	return x = Just x
	(Just x) >>= f = f x
	Nothing >>= f = Nothing

--------------------------------------------------------------------------------
-- Монада State
newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
    return a = State (\s -> (s, a))
    (>>=) someStateS f = State  (\s ->  let (s', a) = runState someStateS s
					in runState (f a) s'
				)
