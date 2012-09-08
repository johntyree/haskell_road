
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
-- Proving theorem 2.10 by hand
