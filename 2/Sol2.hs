
module Sol2 where

import GS
import TAMO


-- Exercise 2.2
-- Truth table for XOR
-- P | Q | P ^ Q
-- t | t | f'
-- t | f | t'
-- f | t | t'
-- f | f | f'



-- Exercise 2.4
-- Truth table for "not (P <=> Q)"
-- P | Q | ¬ (P <=> Q)
-- t | t | f'    t
-- t | f | t'    f
-- f | t | t'    f
-- f | f | f'    t
--
-- We see that it is indeed equivalent to NOT <=>. Since <=> is defined as
-- simply (==), since NOT (<=>) = NOT (==) = (/=), it follows that
-- XOR = (/=) and the definition (<+>) = (/=)  is correct.



-- Exercise 2.9
-- Equivalence of P and (P ^ Q) ^ Q
-- P | Q | (P ^ Q) ^ Q
-- t | t |    f    t'
-- t | f |    t    t'
-- f | t |    t    f'
-- f | f |    f    f'
--
-- Clearly P and (P^Q)^Q have the same truth value in all cases.



-- Exercise 2.11
-- Proving theorems_2.10 by hand
-- 1
-- P == !!P
-- P | !  ! P
-- t | t' f
-- t | t' f
-- f | f' t
-- f | f' t
--
-- 2
-- P | P ∧ P | P ∨ P
-- t |   t'  |   t'
-- t |   t'  |   t'
-- f |   f'  |   f'
-- f |   f'  |   f'
--
-- 3
-- P ⇒  Q | ¬ P ∨  Q    ¬ (P ⇒ Q) | P ∧  ¬ Q
-- t t' t | f   t'      f'   t    |   f' f
-- t f' f | f   f'      t'   f    |   t' t
-- f t' t | t   t'      f'   t    |   f' f
-- f t' f | t   t'      f'   t    |   f' t
--
-- 4
--  ! P >  ! Q  | Q >  P     P >  ! Q) | Q >  ! P    ! P >  Q  | ! Q >  P
--  f t t' f t  |   t'         f' f    |   f' f      f   t'    | f   t'
--  f t t' t f  |   t'         t' t    |   t' f      f   t'    | t   t'
--  t f f' f t  |   f'         t' f    |   t' t      t   t'    | f   t'
--  t f t' t f  |   t'         t' t    |   t' t      t   f'    | t   f'
--
--
-- 5
-- P <=> Q | (P > Q) ∧ (Q > P) | (P ∧ Q) ∨  (! P ∧  ! Q)
-- t  t' t |    t    t'   t    |    t    t'  f   f  f
-- t  f' f |    f    f'   t    |    f    f'  f   f  t
-- f  f' t |    t    f'   f    |    f    f'  t   f  f
-- f  t' f |    t    t'   t    |    f    t'  t   t  t
--
--
-- 6
-- P ∧  Q | Q ∧  P     P ∨  Q | Q ∨  P
-- t t' t |   t'       t t' t |   t'
-- t f' f |   f'       t t' f |   t'
-- f f' t |   f'       f t' t |   t'
-- f f' f |   f'       f f' f |   f'
--
--
-- 7
-- ! (P ∨ Q) | ! P ∧  ! Q     ! (P ∧ Q) | ! P ∨   ! Q
-- f' t t t  | f   f' f       f'   t    | f   f'  f
-- f' t t f  | f   f' t       t'   f    | f   t'  t
-- f' f t t  | t   f' f       t'   f    | t   t'  f
-- t' f f f  | t   f' t       t'   f    | t   t'  t
--
--
-- 8
-- P ∧  (Q ∧ R) | (P ∧ Q) ∧   R         P ∨  (Q ∨ R) | (P ∨ Q) ∨   R
-- t t'  t t t  |    t    t'            t t'  t t t  |    t    t'
-- t f'  t f f  |    t    f'            t t'  t t f  |    t    t'
-- t f'  f f t  |    f    f'            t t'  f t t  |    t    t'
-- t f'  f f f  |    f    f'            t t'  f f f  |    t    t'
-- f f'  t t t  |    f    t'            f t'  t t t  |    t    t'
-- f f'  t f f  |    f    f'            f t'  t t f  |    t    t'
-- f f'  f f t  |    f    f'            f t'  f t t  |    f    t'
-- f f'  f f f  |    f    f'            f f'  f f f  |    f    f'
--
--
-- 9
-- P ∧  (Q ∨ R) | (P ∧ Q) ∨  (P ∧ R)            P ∨  (Q ∧ R) | (P ∨ Q) ∧  (P ∨ R)
-- t t'  t t t  |    t    t'    t               t t'  t t t  |    t    t'    t
-- t t'  t t f  |    t    t'    f               t t'  t f f  |    t    t'    t
-- t t'  f t t  |    f    t'    t               t t'  f f t  |    t    t'    t
-- t f'  f f f  |    f    f'    f               t t'  f f f  |    t    t'    t
-- f f'  t t t  |    f    f'    f               f t'  t t t  |    t    t'    t
-- f f'  t t f  |    f    f'    f               f f'  t f f  |    t    f'    f
-- f f'  f t t  |    f    f'    f               f f'  f f t  |    f    f'    t
-- f f'  f f f  |    f    f'    f               f f'  f f f  |    f    f'    f
--
--
--

-- Exercise 2.13
-- Defining the tests for theorem 2.12
e_213_1a = logEquiv1 (const (not True)) (const False)
e_213_1b = logEquiv1 (const (not False)) (const True)

e_213_2  = logEquiv1 (==> False) not

e_213_3a = logEquiv1 (|| True)  (const True)
e_213_3b = logEquiv1 (&& False) (const False)

e_213_4a = logEquiv1 (|| False) id
e_213_4b = logEquiv1 (&& True)  id

e_213_5  = logEquiv1 (\p -> p || (not p)) (const True)

e_213_6  = logEquiv1 (\p -> p && (not p)) (const False)

-- All tests
e_213 = and [e_213_1a,e_213_1b,e_213_2 ,e_213_3a,e_213_3b,e_213_4a,e_213_4b,e_213_5 ,e_213_6]




-- Exercise 2.15
-- First the easy way
contradiction1 :: (Bool -> Bool) -> Bool
contradiction1 = logEquiv1 (const False)

contradiction2 :: (Bool -> Bool -> Bool) -> Bool
contradiction2 = logEquiv2 (const . const $ False)

contradiction3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
contradiction3 = logEquiv3 (const . const . const $ False)

-- And now doing some actual work...
contradiction1' bf1 = bf1 True == False && bf1 False == False
contradiction2' bf1 = and [bf1 p q == False | p <- [True, False],
                                             q <- [True, False]]
contradiction3' bf1 = and [bf1 p q r == False | p <- [True, False]
                                             , q <- [True, False]
                                             , r <- [True, False]]

-- Exercise 2.16
-- 1. There is no solution to x² + 1 = 0.
-- 2. There exists a largest natrual number.
-- 3. The number 13 is not prime.
-- 4. The number n is not prime.  ("useful"?)
-- 5. There is a finite number of primes.


-- Exercise 2.17
-- P = x < y   Q = y < z
-- x < y < z = P ∧ Q

-- ¬ (P ∧ Q) = ¬ P ∨ ¬ Q
-- ¬ P = x >= y
-- ¬ Q = y >= z
-- ¬ P ∨ ¬ Q   =   x >= y ∨ y >= z

-- x >= y ∨ y >= z


-- Exercise 2.18
-- The approach is to treat Φ and Ψ as opaque statements which have value either False or True.
-- In that sense, we can just call them P and Q for now.

-- P <=> Q | (¬P <=> ¬Q)
-- t  t  t |  f   t  f
-- t  f  f |  f   f  t
-- f  f  t |  t   f  f
-- f  t  f |  t   t  t
e_218_1 = logEquiv2 (<=>) (\p q -> (not p) <=> (not q))

-- ¬ P <=>  Q | P <=>  ¬Q
-- f t  f'  t | t  f'  f
-- f t  t'  f | t  t'  t
-- t f  t'  t | f  t'  f
-- t f  f'  f | f  f'  t
e_218_2 =  logEquiv2 (\p q -> (not p) <=> q) (\p q -> p <=> (not q))

-- Exercise 2.19
-- If Φ <=> Ψ is a tautology, then Φ is true whenever Ψ is true and Φ is
-- false whenever Ψ is false. This is the definition of equivalent, thus
-- Φ ≡ Ψ

-- Exercise 2.20
e_220_1 = logEquiv2 (\p q   -> (not p) ==> q)   (\p q   -> p ==> (not q))
e_220_2 = logEquiv2 (\p q   -> (not p) ==> q)   (\p q   -> q ==> (not p))
e_220_3 = logEquiv2 (\p q   -> (not p) ==> q)   (\p q   -> (not q) ==> p)
e_220_4 = logEquiv3 (\p q r -> p ==> (q ==> r)) (\p q r -> q ==> (p ==> r))
e_220_5 = logEquiv3 (\p q r -> p ==> (q ==> r)) (\p q r -> (p ==> q) ==> r)
e_220_6 = logEquiv2 (\p q   -> (p ==> q) ==> p) const
e_220_7 = logEquiv3 (\p q r -> p || q ==> r) (\p q r -> (p ==> r) && (q ==> r))
e_220   = filter (not . snd) $ zip [1..] [e_220_1, e_220_2, e_220_3, e_220_4, e_220_5, e_220_6, e_220_7]
-- (1,False),(2,False),(5,False)


-- Exercise 2.22
-- Let a,b ∈ R such that a < b. Then c = (a+b)/2 ∈ R.

-- Exercise 2.51
-- Unique is True iff exactly ONE of (map p [a]) is True.
unique :: (a -> Bool) -> [a] -> Bool
unique p l = case (filter p l) of
                 [_] -> True
                 _   -> False


-- Exercise 2.52
-- parity xs is True iff an even number of xs's are True
parity :: [Bool] -> Bool
parity l = case (filter id l) of
               (_:_:zs) -> parity zs
               (_:_)    -> False
               _        -> True


-- Exercise 2.53
-- evenNR is True iff an even number of xs satisfy p
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p = parity . map p

