

module JoeJill where

import Data.Ratio
import Week2

jill' :: Ord a => a -> a -> Bool
jill' p x = p <= x

joe' :: (Fractional a, Ord a) =>  a -> (a, Bool -> a)
joe' cut = (cut, \p -> if p then 0 else 1/2)


testjill :: (Enum a, Fractional a, Ord a) => a -> Bool
testjill cutoff = and (map bestcake testvals)
    where
      testvals = cutoff : [0.5, 0.501 .. 1]
      bestcake x | jill' cutoff x = x >= (1-x) + 1/2
                 | otherwise      = x <  (1-x) + 1/2

testjoe :: (Enum a, Fractional a, Ord a) => (a -> Bool) -> a -> Bool
testjoe (fakejill) x = joescake x == maximum (map joescake testvals)
    where
       testvals = x : [0.5, 0.501 .. 1]
       joescake x | fakejill x = 2 - x
                  | otherwise  = x + 1/2


runTests = do
    print $ map testjill [0.5, 0.64, 0.75, 0.85, 1] -- Jill is optimal at 0.75
    print ""
    print $ map (\x -> testjoe (jill' x) (x*0.9 :: Ratio Integer)) [0.5, 0.64, 0.75, 0.85, 1] -- Jill always defers, optimal @ 1
    print ""
    print $ map (\x -> testjoe (jill' 0.64) (x :: Ratio Integer)) [0.5, 0.64, 0.75, 0.85, 1] -- Jill always accept @ threshold
    print ""
    print $ map (\x -> testjoe (jill' 0.75) (x :: Ratio Integer)) [0.5, 0.64, 0.75, 0.85, 1] -- Jill always accept @ threshold
    print ""
    print $ map (\x -> testjoe (jill' 0.85) (x :: Ratio Integer)) [0.5, 0.64, 0.75, 0.85, 1] -- Jill always actjoept @ threshold
    print ""
    print $ map (\x -> testjoe (jill' x) (x*1.1 :: Ratio Integer)) [0.5, 0.64, 0.75, 0.85, 1] -- Jill always accepts
-- 2.5 Hours without report
