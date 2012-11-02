{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonadJoin where
import ITMOPrelude.Categories.MonadJoin

-- Эти
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadFish

-- делаем из нас
instance MonadJoin m => Monad m where
	return = returnJoin
	(>>=) mx f = join (fmap f mx)
