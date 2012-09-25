import System.Random
import Techniques

--exercise 2
genIntList2 :: IO [Int]
genIntList2 = do 
    g <- getStdGen
    return (randomRs (0,5) g)

--exercise 3
removeFst :: Eq a => a -> [a] -> [a]
removeFst m [] = []
removeFst m (x:xs) | m == x = xs
                   | otherwise = x : (removeFst m xs)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] y = False
isPermutation (x:xs) y | elem x y = isPermutation xs (removeFst x y)
                       | otherwise = False

--exercise 4
testIsPermutation :: IO Bool
testIsPermutation = do
    x <- fmap (take 5) genIntList2
    y <- fmap (take 5) genIntList2
    return (isPermutation x y)

