# Introduction to Agda

## You want to use Emacs, trust me

There is *agda2-mode* for Emacs. It allows to:

* input funny UNICODE symbols like ℕ or Σ,
* make *holes* in code to interactively build you proof/program. more on that below.

installation:

* install `emacs`,
* install everything with `agda` substring from your package manager or `Agda` and `Agda-executable` with `cabal`,
* run `agda-mode setup`.

Running:

* run `emacs`,
* press `C-x C-f FileName RET` (Control+x, Control+f, type "FileName", press Return/Enter key).

## Nearly Haskell

In Agda the module definition always goes first
\begin{code}
-- Date: December 2012
-- Author: Jan Malakhovski
-- WWW: http://twitter.com/oxij

-- Introduction to Agda. Early (but usable) draft
-- for Functional Programming and Proof Checking course at NRU ITMO.

module PrimitiveAgda12Part1 where
\end{code}

Datatypes are written in GADTs-style:
\begin{code}
-- ℕ is \bn
-- Naturals.
data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

-- ⊥ is \bot
-- Empty type. Absurd. False proposition.
data ⊥ : Set where
\end{code}

There is no `undefined`, all computations are guaranteed to end in a normal form.
Which means that `⊥` is really empty.

When you try to pattern match on the element of an empty type you might want to use *absurd pattern*:
\begin{code}
-- → is \to, but -> is fine too
-- Absurd implies anything.
⊥-elim : {A : Set} → ⊥ → A
⊥-elim ()
\end{code}
which allows you to skip right-hand side of the definition.

You can bind variables like that still:
\begin{code}
-- ‵ is \`
-- Absurd implies anything, take two.
⊥-elim‵ : {A : Set} → ⊥ → A
⊥-elim‵ x = ⊥-elim x
\end{code}

There are records, but more on them below.
\begin{code}
-- ⊤ is \top
-- One element type. Record without fields. True proposition.
record ⊤ : Set where
  constructor tt

test⊤ : ⊤
test⊤ = tt
\end{code}


Pattern matching looks as usual:
\begin{code}
data Bool : Set where
  true false : Bool -- Note this, constructors of a same type
                    -- can be listed by interspersing them with spaces.

not : Bool → Bool
not true  = false
not false = true
\end{code}

Except if you make an error in a constructor name:
\begin{code}
not‵ : Bool → Bool
not‵ true  = false
not‵ fals  = true
\end{code}
Agda will say nothing. This might be critical sometimes:
\begin{spec}
data Three : Set where
  COne CTwo CThree : Three

three2ℕ : Three → ℕ
three2ℕ COne = zero
three2ℕ Ctwo = succ zero
three2ℕ _    = succ (succ zero)
\end{spec}
the second and the third case intersect here.
`_` on the left-hand side of a clause means the same "I don't care about the name" thing as in Haskell.

Moreover, Agda uses oversimplified lexer that splits tokens by spaces, parentheses, and brackets only.
For instance (note the name of the variable binding):
\begin{code}
⊥-elim‵‵ : {A : Set} → ⊥ → A
⊥-elim‵‵ ∀x:⊥→-- = ⊥-elim ∀x:⊥→--
\end{code}
is totally fine. Also note this doesn't generate a comment.

## Dependent types

Let's define division by two:
\begin{code}
div2 : ℕ → ℕ
div2 zero = zero
div2 (succ zero) = zero
div2 (succ (succ n)) = succ (div2 n)
\end{code}

Note a hole marked with `{!check me!}` in the second clause. These are called *goals* and explained below.
For now let's forget about it. What are we supposed to write in its place?

Suppose that we know that in our program inputs to `div2` function are always even.
If that is the case we won't need to ponder how to extend `div2` for the `succ zero` case.
Having said that let's constrain `div2` to even naturals only.

With dependent types predicate `P` on `A` becomes a function from `A` to types, i.e. `A → Set`.
If `a : A` satisfies `P` then `P` returns a type with each element being a proof of `P a`, in the case when `a` doesn't satisfy `P`, it returns an empty type.

In our case we shall define `even` predicate:
\begin{code}
even : ℕ → Set
even zero = ⊤
even (succ zero) = ⊥
even (succ (succ n)) = even n
\end{code}
which returns `⊤` with with a trivial proof `tt` when argument is even and empty `⊥` then the argument is odd.

We are now ready to write a division function that takes an additional argument that proves that a number we want to divide is even:
\begin{code}
div2e : (n : ℕ) → even n → ℕ -- We have to give a name `n` to the first argument here
div2e zero p = zero
div2e (succ zero) ()
div2e (succ (succ y)) p = succ (div2e y p) -- Note that a proof of `even (succ (succ n))` translates to a proof of `even n`.
\end{code}

Each time we match on `n` the type of the second argument changes.
From the function side, if the first argument is odd then the second argument will become of type `⊥` sometime and we'll use absurd pattern then.
From the caller side, we can't call this function with an odd `n`, since we can't give it a value for the second argument in that case.

## Type families and unification

List datatype is just like Haskell's:
\begin{code}
data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A
\end{code}

Note a parameter `A`. `_∷_` is a funny way to write `cons` constructor. More on that below.
 
There is another way to define `even`. This time with a datatype with an *index*:
\begin{code}
data Even : ℕ → Set where
  ezero  : Even zero
  e2succ : {n : ℕ} → Even n → Even (succ (succ n))
\end{code}

But let's put indexes aside for a minute.
Note {n : ℕ}. This declares an implicit argument.
You don't have to explicitly apply implicit arguments, Agda tries to automatically infer their values from type-generated equations.
For instance, in the expression `e2succ ezero` the only implicit argument `n` is equal to `zero` because `ezero : Even zero`.

You can always apply implicit arguments explicitly and it's possible to pattern matched on them, e.g. `esucc {zero} ezero` or `esucc {n = zero} ezero`.
It is also possible to ask Agda to deduce a value of any non implicit subexpression with `_` effectively making it implicit:
\begin{code}
evenTest1 : Even (succ (succ zero))
evenTest1 = e2succ {n = _} ezero
\end{code}

It only uses equation solving. No proof search is ever done!
The code
\begin{code}
--evenTest1‵ : Even (succ (succ zero))
--evenTest1‵ = e2succ {n = _} _
\end{code}
generates an unsolved metavariable (more on them below), which is an error.

Implicit arguments are completely orthogonal to computational aspect of a program.
Being implicit doesn't imply anything except being implicit.
Implicit variables are not treated any way special, they are not erased by compilation.
It's just a kind of syntax sugar assisted by equation solving.

Parameters become constructors' implicit arguments.
For instance, `_∷_ : {A : Set} → A → List A → List A`.
Indexes don't.

For instance, you can write
\begin{code}
listTest1 : List ℕ
listTest1 = [] {ℕ}
\end{code}
or
\begin{code}
listTest2 : List ℕ
listTest2 = [] {A = ℕ}
\end{code}
and pattern match in a is similar fashion
\begin{code}
evenTest2 : ∀ {n} → Even n → ℕ
evenTest2 ezero      = zero
evenTest2 (e2succ {n = n} e) = n
\end{code}

Okay. Now, what is the point of indexes?
From a type theoretic point of view they allow us to declare a *family* of types.
For instance, `Even : ℕ → Set` is a family of types indexed by ℕ.
In the definition above type `Even zero` has only one element `ezero`.
For a given `n` type `Even (succ (succ n))` have one element if `Even n` is nonempty.
There are no other elements.

All this is very similar to a predicate functions explained above.
The difference is that `Even : ℕ → Set` *constructs* a type whereas `even : ℕ → Set` *returns* a type for each element of ℕ.

`Even` datatype allows us to define another non-extended division by two for ℕ:
\begin{code}
div2E : (n : ℕ) → Even n → ℕ
div2E zero ezero = zero
div2E (succ zero) ()
div2E (succ (succ n)) (e2succ stilleven) = succ (div2E n stilleven) -- Compare this case to div2e.
\end{code}

Note, there is no case for `div2E zero (e2succ x)`, since `e2succ x` has the wrong type.
For the `succ zero` case the type of the second argument is empty.
How do we know that? Unification!
Given two terms `M` and `N` unification tries to find a pair of substitutions `s₁` and `s₂` such that `M [s₁] = M [s₂]`.
Unification is the most important and easily forgotten aspect of dependently typed programming (with inductive datatypes).
In the code above `succ zero` doesn't unify with any of the `Even` constructors' indexes [`zero`, `succ (succ n)`] which makes this type empty.

We can combine parameters and indexes naturally, with the most famous example being `Vec`tor datatype, that is, a list with a length index:
\begin{code}
data Vec (A : Set) : ℕ → Set where
  []  : Vec A zero
  _∷_ : ∀ {n} → A → Vec A n → Vec A (succ n)
\end{code}

Note `∀` symbol.

* `∀ n` is a syntax sugar for `(n : _)`, i.e. "the argument of some type (deduced from equations) with a name `n`".
* `∀ {n}` is a sugar for `{n : _}`.
* `∀` extends to the right up to the first arrow. 
* `A → B` is a syntax sugar for `(_ : A) → B`.
* `(x y : A) → B` is a sugar for `(x : A) (y : A) → B`.
* `(x : A) (y : B) → C` is a sugar for `(x : A) → (y : B) → C`.

An example with all of these together: `∀ {n} (v₁ : Vec ℕ n) {m} (v₂ : Vec ℕ m) → (ℕ → ℕ) → ℕ` is a sugar for `{n : _} → (v₁ : Vec ℕ n) → {m : _} → (v₂ : Vec ℕ m) → (_ : ℕ → ℕ) → ℕ`.

Constructors of different types are allowed to have the same name. Compare `List` and `Vec`.

\begin{code}
head : ∀ {A n} → Vec A (succ n) → A
head (_∷_ a as) = a
\end{code}
Again, there's no case for an empty `Vec` since `[]` has the wrong type.

`_`s in a name mark arguments' places for `MixFix` parser.
For example, `if then else` construction can be defined the following way:
\begin{code}
if_then_else_ : {A : Set} → Bool → A → A → A
if true then a else _ = a
if false then _ else b = b
\end{code}

More infix definitions:
\begin{code}
-- Are two ℕs equal?
_=ℕ?_ : ℕ → ℕ → Bool
zero   =ℕ? zero   = true
zero   =ℕ? succ m = false
succ m =ℕ? zero   = false
succ n =ℕ? succ m = n =ℕ? m

-- Sum for ℕ.
infix 20 _+_
_+_ : ℕ → ℕ → ℕ
zero   + n = n
succ n + m = succ (n + m)
\end{code}
Note the fixity declaration `infix`. I didn't write `infixl` for a reason.
With declared associativity Agda won't print extra parentheses when asked to print something inferred.
That would somewhat complicate explanation of a several things below.

`if then else` usage:
\begin{code}
ifthenelseTest : ℕ
ifthenelseTest = if (zero + succ zero) =ℕ? zero
  then zero
  else succ (succ zero)
\end{code}

Same `head` function, but prettier:
\begin{code}
head‵ : ∀ {A n} → Vec A (succ n) → A
head‵ (a ∷ as) = a
\end{code}

By the way, the `Vec` type is famous for this:
\begin{code}
_++_ : ∀ {A n m} → Vec A n → Vec A m → Vec A (n + m)
[]       ++ bs = bs
(a ∷ as) ++ bs = a ∷ (as ++ bs)
\end{code}
i.e. the length of concatenation is available in types.

Why does this definition work? Because we defined `_+_` this way!
Compare `_+_` and `_++_` definitions.
In the first clause the type of `[]` gives `n = zero` by unification, `zero + m = m` by our definition.
Similarly in the second clause.

## Dotted patterns and unification

Let's define an extended substraction:
\begin{code}
infix 20 _-_
_-_ : ℕ → ℕ → ℕ
zero   - _      = zero
succ n - zero   = succ n
succ n - succ m = n - m
\end{code}
Note that `n - m = zero` for `m > n`.

Let's get rid of this ugly `(succ n) - zero` case with `_≤_` relation:
\begin{code}
data _≤_ : ℕ → ℕ → Set where
  z≤n : ∀ {n}           → zero ≤ n
  s≤s : ∀ {n m} → n ≤ m → succ n ≤ succ m
\end{code}

Now we are ready to write a substraction that is not extended for `m > n`.
\begin{code}
sub : (n m : ℕ) → m ≤ n → ℕ
sub n zero (z≤n .{n}) = n
sub .(succ n) .(succ m) (s≤s {m} {n} y) = sub n m y
\end{code}

Note the dots. These are called "dotted patterns".

Consider the case
`sub n zero (z≤n {k})`.
The type of third argument is `zero ≤ n`.
The type of `z≤n {k}` is `zero ≤ k`.
Unification of these two types gives the [`k = n`] equation.
We now have
`sub n zero (z≤n {n})`.
Which of the `n`s we want to bind/match on?
In the code above we say "on the first" and place the dot before the second to mark this intention.

The second clause is
`sub n m (s≤s {n'} {m'} y)`.
The type of the third argument is `m ≤ n`.
The type of `s≤s {n'} {m'} y` is `succ n' ≤ succ m'`.
This gives [`n = succ n'`, `m = succ m'`].
This time we decided to match on `n'` and `m'`.

Rewritten with a `case` construct from Haskell (Agda doesn't have `case`, see below for why) the code above becomes:
\begin{spec}
__IMPOSSIBLE__ = undefined

sub n m even = case even of
  z≤n {k}     -> case m of   -- here we have {`k = n`, `m = zero`} from unification
    zero   -> n
    succ m -> __IMPOSSIBLE__ -- since `m = zero`
  s≤s n' m' y -> sub m' n y  -- here we have {`n = succ n'`, `m = succ n'`}
\end{spec}

Note, that we have [`k = n`, `m = zero`] in the first case for even.
This means we can optimize the match on `m` away: 
\begin{code}
sub₁ : (n m : ℕ) → m ≤ n → ℕ
sub₁ n .zero (z≤n .{n}) = n
sub₁ .(succ n) .(succ m) (s≤s {m} {n} y) = sub₁ n m y
\end{code}

We can also rewrite `sub` to match on the first two arguments (which is a common sense):
\begin{code}
sub‵ : (n m : ℕ) → m ≤ n → ℕ
sub‵ n zero (z≤n .{n}) = n
sub‵ (succ n) (succ m) (s≤s .{m} .{n} y) = sub‵ n m y
\end{code}
which translates into the following:
\begin{spec}
sub‵ n m even = case m of
  zero   -> case even of
      z≤n {k}       -> n
      s≤s {k} {l} y -> __IMPOSSIBLE__ -- since `zero` (the value of `m`)
                                      -- can't be unified
                                      -- with `succ k`
  succ m' -> case n of
    zero   -> case even of
      z≤n {k}       -> __IMPOSSIBLE__ -- since `succ m'` (`m`) can't be unified
                                      -- with `zero`
      s≤s {k} {l} y -> __IMPOSSIBLE__ -- since `zero` (`n`) can't be unified
                                      -- with `succ l`
    succ n' -> case even of
      z≤n {k}       -> __IMPOSSIBLE__ -- since `succ n'` (`n`) can't be unified
                                      -- with `zero`
      s≤s {k} {l} y -> sub‵ n' m' y
\end{spec}

**Exercise.** Write out unification constraints for the definition above.

Note, that for `sub₂ n zero` the third argument is always `z≤n {n}`, so we would like to write
\begin{spec}
sub‵₂ : (n m : ℕ) → m ≤ n → ℕ
sub‵₂ n zero .(z≤n {n}) = n
sub‵₂ (succ n) (succ m) (s≤s .{m} .{n} y) = sub‵₂ n m y
\end{spec}
but Agda doesn't allow this. See below why.

We can write
\begin{code}
sub₃ : (n m : ℕ) → m ≤ n → ℕ
sub₃ n zero _ = n
sub₃ (succ n) (succ m) (s≤s .{m} .{n} y) = sub₃ n m y
\end{code}
still.

**Exercise.** Translate the following definition into Haskell:
\begin{code}
sub₁‵ : (n m : ℕ) → m ≤ n → ℕ
sub₁‵ n zero (z≤n .{n}) = n
sub₁‵ (succ .n) (succ .m) (s≤s {m} {n} y) = sub₁‵ n m y
\end{code}
Write out unification constraints for the each case.

So, what are the dotted patterns? They are inlined unification constraints!
This is why we couldn't dot `z≤n {n}` in the first clause of `sub‵₂`, Agda didn't generate such a constraint (it could, have it tried a bit harder, but it's lazy).

## Propositional equality

We now define the most useful type family, that is, Martin-Löf's equivalence (for values only):
\begin{code}
-- ≡ is \==
infix 10 _≡_
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x
\end{code}

For `x y : A` the type `x ≡ y` has exactly one constructor `refl` if `x` can be unified with/converted into `y`, i.e. there exist such `z` so that `z →β✴ x` and `z →β✴ y`, where `→β✴` is "β-reduces in zero or more steps".

Let's prove some of it's properties:
\begin{code}
-- ≡ is symmetric
sym : {A : Set}{a b : A} → a ≡ b → b ≡ a
sym refl = refl

-- and transitive.
trans : {A : Set}{a b c : A} → a ≡ b → b ≡ c → a ≡ c
trans refl refl = refl

-- A fun way to write transitivity. See examples below.
infixr 5 _~_
_~_ : {A : Set}{a b c : A} → a ≡ b → b ≡ c → a ≡ c
_~_ = trans

-- ≡ is congruent.
cong : {A B : Set} {a b : A} → (f : A → B) → a ≡ b → f a ≡ f b
cong f refl = refl
\end{code}

Consider the case `sym {A} {a} {b} (refl {x = a})`.
Matching on `refl` gives `b = a` equation, i.e. the clause actually is `sym {A} {a} .{a} (refl {x = a})` which allows to write `refl` on the right-hand side.
Other proofs are similar.

Note, we can prove `sym` the other way:
\begin{code}
sym‵ : {A : Set}{a b : A} → a ≡ b → b ≡ a
sym‵ {A} .{b} {b} refl = refl
\end{code}

`sym` packs `a` into `refl`. `sym‵` packs `b`. "Are these two definitions equal?" is an interesting philosophical question.
From the Agda's point of view, they are.

Since dotted patterns are just unification constraints, you don't have to dot implicit arguments when you don't bind or match on them.

`_≡_` type family is called *propositional equality*.
In Agda's standard library it has a bit more general definition, see below.

## Proving things

With `_≡_` we can finally prove something from arithmetic.

Let's do this interactively for associativity:
\begin{spec}
+-assoc : ∀ a b c → (a + b) + c ≡ a + (b + c)
+-assoc a b c = {!!}
\end{spec}
pressing `C-c C-l` loads and typechecks the buffer.

Note that you can load [this article in Literate Agda format](TODO.lagda) directly into Emacs.

Placing cursor in the *goal* above (the green hole in the text) and pressing `C-c C-c a RET` gives
\begin{spec}
+-assoc : ∀ a b c → (a + b) + c ≡ a + (b + c)
+-assoc zero b c = {!!}
+-assoc (succ a) b c = {!!}
\end{spec}
the first hole is obvious, lets write `refl` in there and press `C-c C-r` (refine), this gives
\begin{spec}
+-assoc : ∀ a b c → (a + b) + c ≡ a + (b + c)
+-assoc zero b c = refl
+-assoc (succ a) b c = {!!}
\end{spec}
`C-c C-f`, write `cong succ` there, `C-c C-r`:
\begin{spec}
+-assoc : ∀ a b c → (a + b) + c ≡ a + (b + c)
+-assoc zero b c = refl
+-assoc (succ a) b c = cong succ {!!}
\end{spec}
pressing `C-c C-,` in there prints the goal type and context pressing `C-c C-a` (auto proof search) gives
\begin{code}
+-assoc : ∀ a b c → (a + b) + c ≡ a + (b + c)
+-assoc zero b c = refl
+-assoc (succ a) b c = cong succ (+-assoc a b c)
\end{code}
Done.
(In Agda 2.3.2 you have to reload a buffer for proof search to work, it's a bug.)

Similarly, we prove
\begin{code}
lemma-+zero : ∀ a → a ≡ a + zero
lemma-+zero zero = refl
lemma-+zero (succ a) = cong succ (lemma-+zero a)

lemma-+succ : ∀ a b → succ a + b ≡ a + succ b
lemma-+succ zero b = refl
lemma-+succ (succ a) b = cong succ (lemma-+succ a b)
\end{code}

We are now ready to prove commutativity:
\begin{code}
+-comm : ∀ a b → a + b ≡ b + a
+-comm zero b = lemma-+zero b
+-comm (succ a) b = cong succ (+-comm a b) ~ lemma-+succ b a
\end{code}

Nice way to "step" through a proof is to wrap some subexpression with `{! !}`, e.g.:
\begin{spec}
+-comm (succ a) b = cong succ {!(+-comm a b)!} ~ lemma-+succ b a
\end{spec}
and ask for a type, context and inferred type of a goal with `C-c C-l` followed by `C-c C-.`, refine, wrap another subexpression, repeat.
I dream of a better interface for this.

The second case of `+-comm` is pretty fun example for infering implicit arguments by hand. Let's do that.
Algorithm is as follows.

* First, we expand all implicit arguments and explicit arguments applied with `_` into *metavariables* (special meta-level variables not bound in the program).
* Then the construct the system of equations.
* Solve it with the help from unification.
* Substitute the results back into their places.

For a term
\begin{spec}
_~_ (cong succ (+-comm1 a b)) (lemma-+succ b a)
\end{spec}
the first step gives:
\begin{spec}
_~_ {ma} {mb} {mc} {md} (cong {me} {mf} {mg} {mh} succ (+-comm a b)) (lemma-+succ b a)
\end{spec}

`a b : ℕ` since `_+_ : ℕ → ℕ → ℕ` in the type of `+comm`.
This gives the following system (with duplicated equations skipped):
\begin{spec}
_~_ (cong succ (+-comm a b)) (lemma-+succ b a) : _≡_ {ℕ} (succ a + b) (b + succ a)
_~_ (cong succ (+-comm a b)) (lemma-+succ b a) : _≡_ {ℕ} (succ (a + b)) (b + succ a) -- after normalization
ma = ℕ
mb = succ (a + b)
md = b + succ a
+-comm a b : _≡_ {ℕ} (a + b) (b + a)
mg = (a + b)
me = ℕ
mh = (b + a)
mf = ℕ
cong succ (+-comm a b) : _≡_ {ℕ} (succ (a + b)) (succ (b + a))
mc = succ (b + a)
lemma-+succ b a : _≡_ {ℕ} (succ b + a) (b + succ a)
lemma-+succ b a : _≡_ {ℕ} (succ (b + a)) (b + succ a) -- after normalization
_~_ (cong succ (+-comm a b)) (lemma-+succ b a) : _≡_ {ℕ} (succ a + b) (b + succ a)
\end{spec}

The most awesome thing here is that from Agda's point of view, a goal is just a metavariable of a special kind.
When you ask for a type of a goal with `C-c C-t` or `C-c C-,` Agda prints everything it has inferred for the corresponding metavariable.
Funny things like `?0`, `?1`, and etc in agda-mode outputs are references to these metavariables.
For instance, in the following:
\begin{code}
metaVarTest : Vec ℕ (div2 (succ zero)) → ℕ
metaVarTest = λ _ → zero
\end{code}
the type of the goal mentions the name of very first goal metavariable from this article.

## Termination checking

Work in progress.

## More things to prove

**Exercise.** Define multiplication by the induction on the first argument:
\begin{code}
infix 30 _*_
_*_ : ℕ → ℕ → ℕ
zero     * n = zero
(succ n) * m = m + (n * m)
\end{code}
so that the following proof works:
\begin{code}
-- Distributivity.
dist : ∀ a b c → (a + b) * c ≡ a * c + b * c
dist zero b c = refl
-- λ is \lambda
dist (succ a) b c = cong (λ x → c + x) (dist a b c) ~ sym (+-assoc c (a * c) (b * c))
\end{code}

Now, fill in the following goals:
\begin{code}
*-assoc : ∀ a b c → (a * b) * c ≡ a * (b * c)
*-assoc zero b c = refl
*-assoc (succ a) b c = dist b (a * b) c ~ cong (λ x → b * c + x) (*-assoc a b c)

lemma-*zero : ∀ a → zero ≡ a * zero
lemma-*zero zero = refl
lemma-*zero (succ a) = cong (λ x → x) (lemma-*zero a)

lemma-+swap : ∀ a b c → a + (b + c) ≡ b + (a + c)
lemma-+swap a b c = sym (+-assoc a b c) ~ cong (λ x → x + c) (+-comm a b) ~ +-assoc b a c

lemma-*succ : ∀ a b → a + a * b ≡ a * succ b 
lemma-*succ zero b = refl
lemma-*succ (succ a) b = cong succ (sym (sym (cong (λ x → x + b) (lemma-*succ a b) ~ +-comm (a * succ b) b) ~ +-comm (a + a * b) b ~ lemma-+swap b a (a * b)))

*-comm : ∀ a b → a * b ≡ b * a
*-comm a zero = sym (lemma-*zero a)
*-comm a (succ b) = sym(lemma-*succ a b) ~ cong (λ x → a + x) (*-comm a b)

\end{code}

## Logical proofs

There is a very dependent (and scary) way to type a function composition:
\begin{code}
-- ∘ is \o
_∘_ : {A : Set} {B : A → Set} {C : {x : A} → B x → Set}
    → (f : {x : A} → (y : B x) → C y)
    → (g : (x : A) → B x)
    → ((x : A) → C (g x))
f ∘ g = λ x → f (g x)
\end{code}
but you should be ready for this if you reached here.

If you have bottom you can derive negation in the following way:
\begin{code}
-- ¬ is \lnot
¬ : Set → Set
¬ P = P → ⊥
\end{code}

The idea is that `P` should be isomorphic to `⊥` when `¬ P` holds.
Since `⊥ → P` is always true there is only `P → ⊥` left for us to prove.

On the other hand, we can say that the idea about `P → ⊥` is that `¬ P` should be nonempty when `P` is empty and vice versa.
If `P` is empty then there is a function `P → ⊥` (`⊥-elim`-like), but you can't make a function from nonempty `P` to `⊥`, there is nothing to return.
This means `P → ⊥` meets our restrictions.

Contradiction implies anything:
\begin{code}
contradiction : {A B : Set} → A → ¬ A → B
contradiction a ¬a = ⊥-elim (¬a a)
\end{code}

Agda uses intuitionistic logic. There is no "double negation" rule since it has no simple computational explanation.
You can try to prove:
\begin{code}
--doubleneg : {A : Set} → ¬ (¬ A) → A
--doubleneg ¬¬a = {!!}
\end{code}
to understand why.

**Exercise.** We are now ready to prove something interesting:
\begin{code}
contra : {A B : Set} → (A → B) → (¬ B → ¬ A)
contra = λ f g x → (g ∘ f) x

¬³-red : {A : Set} → ¬ (¬ (¬ A)) → ¬ A
¬³-red = λ nnnx x → nnnx (λ f → f x)
\end{code}
Hint. Use `C-c C-,` to see normalized goal type.
Try to use `_∘_` in the proof of `contra`.

These two proofs amounted to a serious scientific paper at the start of 20th century.

More things to come.
