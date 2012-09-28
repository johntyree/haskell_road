
module Relation where

import Data.List
import Debug.Trace
import System.Random
import Control.Monad

type Rel a = [(a,a)]

infixr 5 @@

relId :: Rel Integer
relId = let l = [1..4] in zip l l
relSucc :: Rel Integer
relSucc = let l = [1..4] in zip l (tail l)
relDiv :: Rel Integer
relDiv = let l = [1..4] in [(y, x) | y <- l, x <- l, x `rem` y == 0]

randomRel :: Int -> Int -> IO (Rel Integer)
randomRel maxElem size = do
    l0 <- liftM (map fromIntegral . randomRs (0,maxElem)) newStdGen
    l1 <- liftM (map fromIntegral . randomRs (0,maxElem)) newStdGen
    return . nub . sort . take size $ zip l0 l1

(==>) :: Bool -> Bool -> Bool
(==>) a b = not a || b
infix 2 ==>

-- This defines a new relation which is the composition of r and s.
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]

-- Fixed point combinator
fix :: (t -> t) -> t
fix f = let x = f x in x

-- Find the transitive closure by repeated union and composition.
-- 1 hour to figure out how "fix" works
-- 5 minutes to write the function.
-- R ∪ R² ∪ R³ ∪ ...
trClos :: Ord a => Rel a -> Rel a
trClos rel = fix compose rel
  where
    compose c r = let r' = nub $ r ++ (r @@ rel)
                  in if r' == r then r else c r'


-- Testing

prop_reflexive :: Eq t => [(t, t)] -> Bool
prop_reflexive l = and [(b,a) `elem` l | (a,b) <- l]

-- This test is not correct. We're only determing if we have R u R² so far.
prop_transitive :: Eq t => [(t, t)] -> Bool
prop_transitive l = and [(a,d) `elem` l | (a,b) <- l, (c,d) <- l, b == c]


