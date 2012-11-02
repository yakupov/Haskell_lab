{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPredule.Algebra where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
-- всевозможные инстансы для классов ниже 

-- Если не страшно, то реализуйте их и для
-- import ITMOPrelude.List
-- import ITMOPrelude.Tree
-- if there was a time...

-- Классы
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

class Monoid a => Group a where
    ginv :: a -> a

-- Natural
instance Monoid Nat where
	mempty = Zero
	mappend = (+.)

-- Integer
instance Monoid Int where
	mempty = Plus Zero
	mappend = (.+.)

instance Group Int where
	ginv = intNeg

-- Rational
instance Monoid Rat where
	mempty = Rat (Plus Zero) (Succ Zero)
	mappend = (%+)

instance Group Rat where
	ginv = ratNeg

