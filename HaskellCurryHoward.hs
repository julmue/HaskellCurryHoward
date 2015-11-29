{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CurryHoward where

import Prelude hiding (id, curry, uncurry, flip)

-- |  __The Absurd__ (aka Bottom, Empty, False)
--
--    * L.Ip.: the proposition without a proof  
--    * C.Ip.: the type without inhabitants, the empty type
--
data Absurd

-- | Elimination Rule for Absurd:
--
--   * L.Ip.: If the absurd can be proven, everything can be proven.
--   * C.Ip.: program termination

elimAbsurd :: Absurd -> a
elimAbsurd _ = undefined


-- | __Connective: Negation__
--
--   * L.Ip.: Negation  in intuitionistic logic is a proof from 
--            type 'a' to Absurd.
--   * C.Ip.:
--
--   In construcitve logic negation has no introduction rule. 
type Not a = a -> Absurd

 
-- | __Connective: Conjunction__ 
--
--   The conjunction is a product type paremeterized over two typevars; 
--   (a pair/tuple, isomorphic to "(a,b)" in prelude) 
--
--   Introduction Rule (And):
-- 
--   >     a : A    b : B
--   >    ---------------- (I And)
--   >      (a,b): (A,B)
-- 
--   To construct a conjunction 4 arguments are necessary: 
--
--   * 2 propositions (types) for the typeconstructor
--   * 2 witnesses (values) for the propositions
--
--   'And a b' is structurally isomorphic to the type of pairs '(a, b)'.
--
--   > type And a b = (a,b)
--
--   We use the Haskell pair @(a,b)@ form here on to denote conjunction.

data And :: * -> * -> *  where
    Conj :: forall a b. a -> b -> And a b -- I And 
      
-- this is the same as 'data And a b = Conj a b'
-- but with explicit kind signatures and type annotations.
 
-- | Conjunction: Elemination Rule 1 (Left Projection)
--
--   * L.Ip.:   
--   * C.Ip.: projection from dimension n + 1 
--
--   >     (a,b) : A * B               
--   >    ---------------(E1 And)    
--   >        left a : A               
--
--   'projectL' corresponds to fst.

projectL :: forall a b . (a, b) -> a
projectL (a, _) = a 

-- | Conjunction: Elemination Rule 1 (Right Projection)
-- 
--   >     (a,b) : A * B               
--   >    ---------------(E2 And)    
--   >        left a : A               
--
--   'projectR' corresponds to snd.

projectR :: forall a b . (a, b) -> b
projectR (_, b) = b 

-- | == Connective: Disjunction (OR)  
--
--   Disjunction 'OR' is a sum type parameterized over two 
--   typevariables.
--
--   * L.Ip.: Either A can be proven or B can be proven.
--   * C.Ip.: Container that wraps either a type a or a type b.  
--
--   'Or' is structurally isomorphic to 'Either'.
--
--   > type Or a b = Either a b
--
--   We use 'Either' form here on to denote conjunction.
--
--   === Introduction rules for 'Or'
-- 
--   To construct a disjunction 3 arguments are necessary: 
--
--   * 2 propositions (types) for the typeconstructor
--   * 1 witness (values) for either one of the propositions

data Or :: * -> * -> * where
    InjectL :: forall a b. a -> Or a b
    InjectR :: forall a b. b -> Or a b

-- | === Elimination Rule (Disjunction):
--   
--
-- 
elimOr :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
elimOr f1 f2 x = case x of
    Left a -> f1 a
    Right b -> f2 b 

{- 3 Implication 
-}
type Imp a b = a -> b

-- introduction rule: lambda abstraction
-- elimination rule: function application

data Iff a b = Iff { a2b :: a -> b, b2a :: b -> a }

-- introdurction rule
-- elimination rule:

-- | Theorem: Law Of Identity (Classic Law Of Reasoning I/III) 
--   Logical interpretation:
--   Computational interpretation: identity function
id :: forall a. a -> a 
id x = x 

-- | Theorem: Law Of Non-Contradiction (Classic Law Of Reasoning II/III) 
--   Logical interpretation:
--   Computational interpretation:
nc :: forall a. Not (a, (Not a))
nc p = (snd p) (fst p) 

-- | Theorem: Law Of The Excluded Middle (Classic Law Of Reasoning III/III) 
--   This does not hold in constructive logic
--   see below ...

-- | Theorem: modus ponens
--   Computational interpetation: modus ponens is function application
mp :: forall a b . a -> (a -> b) -> b
mp a f = f a

-- | Theorem: associativity of conjunction 
conAssoc :: forall a b c . ((a, b), c) -> (a, (b, c))
conAssoc x = (fst (fst x), (snd (fst x), snd x)) 

-- | Theorem: commutativity of conjunction
conComm :: forall a b . (a, b) -> (b, a)
conComm x = (snd x, fst x)

-- | Theorem: associativity of disjunction
disAssoc :: forall a b c . Either a (Either b c) -> Either (Either a b) c
disAssoc = elimOr (Left . Left) (elimOr (Left . Right) Right)

-- | Theorem: commutativity of disjunction
disCommu :: forall a b . Either a b -> Either b a
disCommu = elimOr Right Left

-- | Theorem: conjunction elimination in the first hypothesis
--   Computational interpretation: currying
curry :: forall a b c. ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

-- | Theorem: conjunction introduction with the first and second hypothesis
--
--   * L.Ip.: Explosion
--   * C.Ip.: uncurrying  
uncurry :: forall a b c. (a -> b -> c) -> ((a,b) -> c)
uncurry f (x,y) = f x y 

-- | Theorem: No Order In Premises
--
--   * L.Ip.: the order of premises doesnt matter for the conclusion
--   * C.Ip.: flipping the order of function arguments
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f b a = f a b

-- | Theorem: 
--
--   * L.Ip.: premisses don't deplete
--   * C.Ip.: 
double :: a -> a -> a
double = monotonicity id

-- | Theorem: not Absurd
--
--   * L.Ip.: Explosion
--   * C.Ip.: The Absurd is not provable  
--  
notAbsurd :: Not Absurd
notAbsurd = id

-- | Theorem: Ex Falso Quodlibet / Principal Of Explosion
--
--   * L.Ip.: Explosion
--   * C.Ip.: 
--
ex :: forall a b. (a, Not a) -> b
ex p = elimAbsurd (nc p)

-- | Theorem: Double Negation
--
dn1 :: forall a. a -> Not (Not a)
dn1 a f = f a

-- | Theorem: contraposition
-- 
cp :: forall a b. (a -> b) -> (Not b -> Not a)
cp f nb a = nb (f a)  

-- | Theorem: Monotonicity Of Entailment 
--   aka weakening in the antecedens aka thinning
--
--   * L.Ip.:   
--   * C.Ip.:  monotonicity can be seen as some kind of focusing 
--
--   > focus = monotonicity'
--   > length' :: forall a. [a] -> Int
--   >
--   > length' = foldr (focus succ) 0

monotonicity' :: forall a b c. (a -> b) -> ((c,a) -> b)
monotonicity' f = f . snd  

-- | alternative formulation of monotonicity. 
monotonicity :: forall a b c. (a -> b) -> c -> a -> b
monotonicity h0 _ = h0 

-- | Theorem: Absorption
abs1 :: forall a b . Either a (a,b) -> a
abs1 = elimOr id fst

abs2 :: forall a b . (a, Either a b) -> a
abs2 = fst

-- | Theorem: Frege's Theorem
--   * L.Ip.:   
--   * C.Ip.:  
frege :: forall p q r . (p -> (q -> r)) -> ((p -> q) -> (p -> r))
frege f1 f2 p = (f1 p) (f2 p)  

-- | Theorem: hypothetical syllogism: 
--
--   * L.Ip.:   
--   * C.Ip.: 
--
--   Same as function composition operator '.'.
hSyll :: forall a b c . (a -> b) -> (b -> c) -> (a -> c)
hSyll f1 f2 a = f2 (f1 a)

-- Theorem: Monotonicity of Entailment
--
--   * L.Ip.:   
--   * C.Ip.: 
--
mon :: forall a b c . (a -> b) -> ((a,c) -> b)
mon f x = f (fst x)

-- Theorem: Distributivity
dis1 :: (Either a b, c) -> Either (a,c) (b,c)
dis1 h0 = let d = fst h0; c = snd h0 in 
    elimOr (\a -> Left (a, c)) (\b -> Right (b, c)) d

dis2 ::  Either (a,c) (b,c) -> (Either a b, c)
dis2 = elimOr (\(a, c) -> (Left a, c)) (\(b, c) -> (Right b, c)) 


{-- Classical Propositional Calculus ----------------------------------} 

-- if we add one of the following (unproveable) combinators we get
-- from intuitionistic propositional calculus to 
-- classic propositional calculus

-- | Theorem: Tertium Non Datur / Law Of The Excluded Middle
tnd :: forall a . Either a (Not a)
tnd = undefined

-- | Theorem: Double Negation
classic :: forall a. (Not (Not a)) -> a
classic = undefined

-- | Theorem: Peirce's Theorem
peirce :: forall a b. ((a -> b) -> a) -> a
peirce = undefined

-- | Theorem: DeMorgan's Law
deMorgan :: forall a b. Not (Not a, Not b) -> Either a b
deMorgan = undefined  
 
 -- | Theorem: Implication as Conjunction 
implies2or :: forall a b. (a -> b) -> Either (Not a) b 
implies2or = undefined

-- Theorem: 
tnd2Classic :: 
    forall p. 
        (forall p. Either p (Not p))  -- ^ Hypothesis 1: LEM holds  
        -> Not (Not p)                -- ^ Hypothesis 2 
        -> p                          -- ^ Conculsion
-- Proof:
tnd2Classic tnd = case tnd of
    Left p -> \_ -> p
    Right np -> \nnp -> ex (np, nnp) 
    
-- Theorem:   
classic2Pierce :: 
   forall p q. 
       (forall r. Not (Not r) -> r)
       -> ((p -> q) -> p) 
       -> p
-- Proof:
classic2Pierce cls h0 = cls (l1 h0)
  where 
    l1 :: ((p -> q) -> p) -> Not (Not p)
    l1 h0 np = ex (l2 h0 np, np)                              
    l2 :: ((p -> q) -> p) -> Not p -> p
    l2 h0 np = h0 (\p -> ex (p, np))

-- Theorem:
lem2DeMorgan :: 
    forall p q. 
        (forall r. Either r (Not r))
        -> Not (Not p, Not q)
        -> Either p q
-- Proof:
lem2DeMorgan lem h0 = elimOr Left l1 lem
  where
    l1 :: Not p -> Either p q
    l1 np = elimOr Right (l2 np) lem
    l2 :: Not p -> Not q -> Either p q
    l2 np nq = ex ((np,nq), h0)

-- Theorem:
lem2Implies2or :: 
    forall p q.
        (forall r. Either r (Not r))
        -> (p -> q) 
        -> (Either (Not p) q)
-- Proof:        
lem2Implies2or lem h0 =
    elimOr (l1 h0) l2 lem
  where 
    l1 :: (p -> q) -> p -> Either (Not p) q
    l1 f p = Right (f p) 
    l2 :: Not p ->  Either (Not p) q
    l2 = Left      
