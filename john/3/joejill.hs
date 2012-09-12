

module JoeJill where

import Week2

jill' :: Float -> Float -> Bool
jill' p x = p <= x

joe' :: Float -> (Float, Bool -> Float)
joe' cut = (cut, \p -> if p then 1/100000 else 1/2)


testjill :: Float -> Bool
testjill cutoff = and (map bestcake testvals)
    where
      testvals = cutoff : [0.5, 0.501 .. 1]
      bestcake x | jill' cutoff x = x >= (1-x) + 1/2
                 | otherwise      = x <  (1-x) + 1/2

testjoe :: (Enum a, Fractional a, Ord a) => (a -> Bool) -> a -> Bool
testjoe (fakejill) x = joescake x == maximum (map joescake testvals)
    where
       testvals = x : [0.5, 0.501 .. 1]
       joescake x | fakejill x = 1 - x + 1
                  | otherwise  = x + 1/2

-- 2.5 Hours without report
