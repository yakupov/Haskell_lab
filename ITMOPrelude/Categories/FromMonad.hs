{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories

-- Эти
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

-- делаем нас
instance Monad m => MonadFish m where
	returnFish = return
	(>=>) f1 f2 = \x -> (>>=) (f1 x) f2

