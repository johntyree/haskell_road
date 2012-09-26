

module RandomFuncs where

import Week3
import Techniques
import System.Random
import Control.Monad

-- | Here we do the dirty work.
genIntList :: IO [Int]
genIntList = genIntListR (0, 15)

genIntListR :: Random a => (a, a) -> IO [a]
genIntListR range = liftM (randomRs range) newStdGen


-- | This is just a wrapper around the pure RNG code.
-- It probably looks the same as randomRs
makeRandoms :: (RandomGen g, Random a, Num a) => g -> [a]
makeRandoms g = let (x, g') = randomR (0, 5) g
                in x : makeRandoms g'


removeFst :: Eq a => a -> [a] -> [a]
removeFst _ [] = []
removeFst m (x:xs)
    | m == x    = xs
    | otherwise = x : removeFst m xs

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _  = False
isPermutation (x:xs) y
    | x `elem` y = isPermutation xs (removeFst x y)
    | otherwise  = False


prop_identity :: Eq a => [a] -> Bool
prop_identity l = isPermutation l l

-- I don't like this. Most randomly generated lists will not be
-- permutations of each other, so we need an enormous number of random
-- tests for this to actually tell us anything.
prop_transitivity :: Eq a => [a] -> [a] -> [a] -> Bool
prop_transitivity l m n = not (isPermutation l m && isPermutation l n) || isPermutation m n

prop_commutatitivity :: Eq a => [a] -> [a] -> Bool
prop_commutatitivity l r' = forward && backward || not forward && not backward
    where forward  = isPermutation l r'
          backward = isPermutation r' l


test1 :: ([Int] -> Bool) -> IO Bool
test1 prop = liftM (all prop) $ testLists 10000

test2 :: ([Int] -> [Int] -> Bool) -> IO Bool
test2 prop = do
    ls <- testLists 1000
    return $ and [prop l r | l <- ls, r <- ls]

test3 :: ([Int] -> [Int] -> [Int] -> Bool) -> IO Bool
test3 prop = do
    ls <- testLists 100
    return $ and [prop l m r | l <- ls, m <- ls, r <- ls]

splitAts :: [Int] -> [a] -> ([[a]], [a])
splitAts = go []
  where
    go acc []         l = (reverse acc, l)
    go acc (len:lens) l = let (sublist, rest) = splitAt len l
                          in go (sublist : acc) lens rest

testLists :: Int -> IO [[Int]]
testLists n = do
    g <- getStdGen
    let (g0, g1) = split g
    let lengths = take n (makeRandoms g0 :: [Int])
    let lists = fst . splitAts lengths $ (makeRandoms g1 :: [Int])
    return lists
