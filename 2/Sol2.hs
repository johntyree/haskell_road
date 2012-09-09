
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
e_213 = all id [e_213_1a,e_213_1b,e_213_2 ,e_213_3a,e_213_3b,e_213_4a,e_213_4b,e_213_5 ,e_213_6]
