data Nat = One | Succ Nat
data Integers = Plus Nat | Minus Nat | Zero

toSucc 1 = One
toSucc n = Succ(toSucc (n - 1))

toPos :: Nat -> Integers
toPos n = (Plus n)

toNeg :: Nat -> Integers;
toNeg n = (Minus n)

toIntn (One) = 1
toIntn (Succ n) = 1 + (toIntn n)

toInt (Zero) = 0
toInt (Plus n) = (toIntn n)
toInt (Minus n) = 0 - (toIntn n)

{-
Succ :: Integers -> Integers
Succ (Minus m) = Minus (Succ m)
Succ (Plus m) = Plus (Succ m)
Plus (Minus m) = Minus m
Minus (Plus m) = Minus m
Minus (Minus m) = Plus m
Plus (Plus m) = 	Plus m
Minus Zero = Zero
Plus Zero = Zero
-}

nplus :: Nat -> Nat -> Nat;
One `nplus` m = Succ m;
m `nplus` One = Succ m;
(Succ n) `nplus` m = Succ (n `nplus` m);

plus :: Integers -> Integers -> Integers;
Zero `plus` m = m;
m `plus` Zero = m;
(Plus m) `plus` (Plus n) = Plus (m `nplus` n);
(Minus m) `plus` (Minus n) = Minus (m `nplus` n);
(Plus m) `plus` (Minus n) = m `nminus` n;
(Minus m) `plus` (Plus n) = n `nminus` m;


minus :: Integers -> Integers -> Integers;
Zero `minus` (Minus m) = Plus m;
Zero `minus` (Plus m) = Minus m;
m `minus` Zero = m;
(Plus m) `minus` (Plus n) = m `nminus` n;
(Minus m) `minus` (Minus n) = n `nminus` m;
(Plus m) `minus` (Minus n) = Plus (m `nplus` n);
(Minus m) `minus` (Plus n) = Minus (n `nplus` m);

nminus :: Nat -> Nat -> Integers;
One `nminus` (Succ m) = Minus m;
(Succ m) `nminus` One = Plus m;
(Succ n) `nminus` (Succ m) = n `nminus` m;
One `nminus` One = Zero;


mult :: Integers -> Integers -> Integers;
Zero `mult` m = Zero;
m `mult` Zero = Zero;
(Plus m) `mult` (Plus n) = Plus (m `nmult` n);
(Minus m) `mult` (Minus n) = Plus (m `nmult` n);
(Plus m) `mult` (Minus n) = Minus (m `nmult` n);
(Minus m) `mult` (Plus n) = Minus (m `nmult` n);

nmult :: Nat -> Nat -> Nat;
m `nmult` One = m;
m `nmult` (Succ n) = (m `nmult` n) `nplus` m;


divide :: Integers -> Integers -> Integers;
Zero `divide` m = Zero;
--m `divide` Zero = Nan;
(Plus m) `divide` (Plus n) = Plus ((Plus m) `pdivide` (Plus n)) `minus` (Plus (Succ One));
(Minus m) `divide` (Minus n) = Plus ((Plus m) `pdivide` (Plus n)) `minus` (Plus (Succ One));
(Plus m) `divide` (Minus n) = Minus ((Plus m) `pdivide` (Plus n)) `plus` (Plus (Succ One));
(Minus m) `divide` (Plus n) = Minus ((Plus m) `pdivide` (Plus n)) `plus` (Plus (Succ One));

pdivide :: Integers -> Integers -> Nat;
(Minus m) `pdivide` n = One;
Zero `pdivide` m = (Succ One);
(Plus m) `pdivide` n = (One) `nplus` (((Plus m) `minus` n) `pdivide` n);


main :: IO ();
main = print (toInt ( (toPos (toSucc 21)) `divide` (toNeg (toSucc 212)) ) )








{-|



plus :: Integers -> Integers -> Integers;

(Plus m) `plus` (Plus n) = m `plus` n;
(Minus m) `plus` (Minus n) = Minus (m `plus` n);
(Plus m) `plus` (Minus n) = m `minus` n;
(Minus m) `plus` (Plus n) = n `plus` m;

(Succ m) `plus` (Plus n) = Succ (m `plus` n);
(Plus m) `plus` (Succ n) = Succ (m `plus` n);
(Succ m) `plus` (Minus n) = Succ (m `minus` n);
(Minus m) `plus` (Succ n) = Succ (m `plus` n);









Zero `plus` m = m;
m `plus` Zero = m;

One `plus` (Plus m) = (Succ m);
(Plus m) `plus` One = (Succ m);
One `plus` (Succ m) = (Succ (Succ m));
(Succ m) `plus` One = (Succ (Succ m));
--One `plus` (Minus (Succ m)) = (Minus m);
--(Minus (Succ m)) `plus` One = (Minus m);
One `plus` One = (Succ One);

m `plus` (Plus n) = (m `plus` n);
(Plus m) `plus` n = (m `plus` n);
m `plus` (Minus n) = (m `minus` n);
(Minus m) `plus` n = (n `minus` m);

(Succ n) `plus` (Succ m) = Succ (Succ (n `plus` m));



(Plus m) `plus` n = m `plus` n;
m `plus` (Plus n) = m `plus` n;
(Minus m) `plus` n = n `minus` m;
m `plus` (Minus n) = m `minus` n;
(Succ n) `plus` m = Succ (n `plus` m);


--plus :: Nat -> Nat -> Nat;
Zero `plus` m = m;
m `plus` Zero = m;
(Succ n) `plus` m = Succ (n `plus` m);
--(+) n m = case n of Zero -> m Succ nn -> Succ (nn + m)

Zero `minus` (Succ m) = (Pred Zero) `minus` m;
m `minus` (Pred n) = m `plus` (Succ n);
m `minus` Zero = m;
n `minus` (Succ m) = (Pred n) `minus` m;
--(Succ n) `minus` (Succ m) = n `minus` m;
--(Succ n) `minus` m = n `minus` (Pred m);
-}




