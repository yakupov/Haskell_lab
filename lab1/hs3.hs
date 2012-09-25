--
-- Data types
--
data Nat = One | Succ Nat
data Integers = Plus Nat | Minus Nat | Zero
data Rationals = Slash Integers Integers

--
-- Convertors from normal numbers to internal format and vice versa
--
toSuccNatInt 1 = One
toSuccNatInt n = Succ(toSuccNatInt (n - 1))

toSuccNat n = toSuccNatInt (abs n)

toSuccIntegerInt :: Int -> Int -> Integers
toSuccIntegerInt 0 m = Zero
toSuccIntegerInt 1 m = Plus (toSuccNat m)
toSuccIntegerInt (-1) m = Minus (toSuccNat (abs m))

toSuccInteger :: Int -> Integers
toSuccInteger n = toSuccIntegerInt (signum n) n

toSuccRat :: Int -> Int -> Rationals
toSuccRat m n = Slash (toSuccInteger (m * (signum n))) (toSuccInteger (abs n))

toIntn (One) = 1
toIntn (Succ n) = 1 + (toIntn n)

toInt (Zero) = 0
toInt (Plus n) = (toIntn n)
toInt (Minus n) = 0 - (toIntn n)

toIntR (Slash m n) = (toInt m) / (toInt n)

--
-- Operations (+-*/)
--
-- natural +
nplus :: Nat -> Nat -> Nat;
One `nplus` m = Succ m;
m `nplus` One = Succ m;
(Succ n) `nplus` m = Succ (n `nplus` m);

-- integer +
plus :: Integers -> Integers -> Integers;
Zero `plus` m = m;
m `plus` Zero = m;
(Plus m) `plus` (Plus n) = Plus (m `nplus` n);
(Minus m) `plus` (Minus n) = Minus (m `nplus` n);
(Plus m) `plus` (Minus n) = m `nminus` n;
(Minus m) `plus` (Plus n) = n `nminus` m;

-- internal - (semi-natural)
nminus :: Nat -> Nat -> Integers;
One `nminus` (Succ m) = Minus m;
(Succ m) `nminus` One = Plus m;
(Succ n) `nminus` (Succ m) = n `nminus` m;
One `nminus` One = Zero;

-- integer -
minus :: Integers -> Integers -> Integers;
Zero `minus` (Minus m) = Plus m;
Zero `minus` (Plus m) = Minus m;
m `minus` Zero = m;
(Plus m) `minus` (Plus n) = m `nminus` n;
(Minus m) `minus` (Minus n) = n `nminus` m;
(Plus m) `minus` (Minus n) = Plus (m `nplus` n);
(Minus m) `minus` (Plus n) = Minus (n `nplus` m);

--natural *
nmult :: Nat -> Nat -> Nat;
m `nmult` One = m;
m `nmult` (Succ n) = (m `nmult` n) `nplus` m;

-- integer *
mult :: Integers -> Integers -> Integers;
Zero `mult` m = Zero;
m `mult` Zero = Zero;
(Plus m) `mult` (Plus n) = Plus (m `nmult` n);
(Minus m) `mult` (Minus n) = Plus (m `nmult` n);
(Plus m) `mult` (Minus n) = Minus (m `nmult` n);
(Minus m) `mult` (Plus n) = Minus (m `nmult` n);

-- internal /
pdivide :: Integers -> Integers -> Nat;
(Minus m) `pdivide` n = One;
Zero `pdivide` m = (Succ One);
(Plus m) `pdivide` n = (One) `nplus` (((Plus m) `minus` n) `pdivide` n);

-- integer /
divide :: Integers -> Integers -> Integers;
Zero `divide` m = Zero;
--m `divide` Zero = Nan;
(Plus m) `divide` (Plus n) = Plus ((Plus m) `pdivide` (Plus n)) `minus` (Plus (Succ One));
(Minus m) `divide` (Minus n) = Plus ((Plus m) `pdivide` (Plus n)) `minus` (Plus (Succ One));
(Plus m) `divide` (Minus n) = Minus ((Plus m) `pdivide` (Plus n)) `plus` (Plus (Succ One));
(Minus m) `divide` (Plus n) = Minus ((Plus m) `pdivide` (Plus n)) `plus` (Plus (Succ One));

-- integer %
mymod :: Integers -> Integers -> Integers;
mymod m n = m `minus` ((m `divide` n) `mult` n)

-- integer abs
myabs (Plus m) = Plus m
myabs (Minus m) = Plus m

-- internal min
minInt :: Integers -> Integers -> Integers -> Integers;
minInt (Minus delta) a b = a
minInt (Plus delta) a b = b

-- integer min
minAbs :: Integers -> Integers -> Integers;
minAbs a b = minInt ((myabs a) `minus` (myabs b)) a b

-- internal gcd
mygcdInt :: Integers -> Integers -> Integers -> Integers -> Integers -> Integers;
mygcdInt (Plus amymodc) bmymodc a b c = mygcdInt (a `mymod` (c `minus` (Plus One))) (b `mymod` (c `minus` (Plus One))) a b (c `minus` (Plus One))
mygcdInt (Minus amymodc) bmymodc a b c = mygcdInt (a `mymod` (c `minus` (Plus One))) (b `mymod` (c `minus` (Plus One))) a b (c `minus` (Plus One))
mygcdInt Zero Zero a b c = c

-- integer gcd
mygcd :: Integers -> Integers -> Integers;
mygcd a b = mygcdInt (a `mymod` (minAbs a b)) (b `mymod` (minAbs a b)) a b (minAbs a b)

-- rational +
rplus :: Rationals -> Rationals -> Rationals
rplus (Slash a b) (Slash c d) = Slash ((a `mult` d) `plus` (c `mult` b)) (b `mult` d)

-- rational -
rminus :: Rationals -> Rationals -> Rationals
rminus (Slash a b) (Slash c d) = Slash ((a `mult` d) `minus` (c `mult` b)) (b `mult` d)

-- rational *
rmult :: Rationals -> Rationals -> Rationals
rmult (Slash a b) (Slash c d) = Slash (a `mult` c) (b `mult` d)

-- rational /
rdiv :: Rationals -> Rationals -> Rationals
rdiv (Slash a b) (Slash c d) = Slash (a `mult` d) (b `mult` c)

-- shorten the rational
shorten :: Rationals -> Rationals
shorten (Slash a b) = Slash (a `divide` (mygcd a b)) (b `divide` (mygcd a b))

--
-- Test
--
main :: IO ();
--main = print (toInt ( (toSuccInteger (1)) `plus` (toSuccInteger (-2)) ) )
main = print (toIntR (shorten ( (toSuccRat 1 2) `rdiv` (toSuccRat (-4) 16))))
--main = print (toInt ( (toSuccInteger (1)) `mygcd` (toSuccInteger (-2)) ) )

