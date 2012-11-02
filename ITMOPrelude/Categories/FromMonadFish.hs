{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonadFish where
import ITMOPrelude.Categories.MonadFish

-- Эти
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadJoin

--    (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

-- делаем из нас
instance MonadFish m => Monad m where
	return = returnFish
	(>>=) mx f = (>=>) id f mx
--	(>>=) mx f = (\s -> ((>=>) f (\x -> mx)) s)

instance MonadFish m => Functor m where
	fmap f x = ((>=>) id (returnFish . f)) x

instance MonadFish m => MonadJoin m where
	returnJoin = returnFish
--	join mx = ?

