{-# LANGUAGE NoMonomorphismRestriction #-}

module RandomSetOrd where

import Techniques
import SetOrd
import RandomFuncs

import Control.Monad

-- Two to implement
-- Ten minutes to figure out that I need newStdGen instead of getStdGen in
-- genIntList
getRandomSetOrd :: Int -> IO (Set Int)
getRandomSetOrd maxsize = do
    l <- genIntList
    return $ foldr insertSet emptySet (take maxsize l)


-- Two minutes
setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection (Set l) (Set r) = list2set $ filter (`elem` r) l

-- Two minutes
setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set l) (Set r) = list2set (l ++ r)

-- Two minutes
setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference l (Set r) = foldr deleteSet l r


-- Tests
-- Time spent: 40 minutes


-- Single argument
prop_union_idempotence, prop_intersection_idempotence :: (Ord a) => Set a -> Bool
prop_union_identity, prop_difference_identity :: (Ord a) => Set a -> Bool
prop_intersection_zero, prop_difference_zero :: (Ord a) => Set a -> Bool

propertiesUnion1 = [prop_union_idempotence, prop_intersection_idempotence, prop_union_identity]
propertiesIntersection1 = [prop_intersection_idempotence, prop_intersection_zero]
propertiesDifference1 = [prop_difference_identity, prop_difference_zero]

-- Two args
prop_union_true, prop_intersection_true, prop_difference_true :: (Ord a) => Set a -> Set a -> Bool
prop_union_reflexivity, prop_intersection_reflexivity :: (Ord a) => Set a -> Set a -> Bool
prop_difference_idempotence :: (Ord a) => Set a -> Set a -> Bool

propertiesUnion2 = [prop_union_true, prop_union_reflexivity]
propertiesIntersection2 = [prop_intersection_true, prop_intersection_reflexivity]
propertiesDifference2 = [prop_difference_true, prop_difference_idempotence]

-- Three args
prop_intersection_associativity :: (Ord a) => Set a -> Set a -> Set a -> Bool
prop_union_associativity :: (Ord a) => Set a -> Set a -> Set a -> Bool
prop_compound_1, prop_compound_2, prop_compound_3, prop_compound_4, prop_compound_5 :: (Ord a) => Set a -> Set a -> Set a -> Bool

propertiesUnion3 = [prop_union_associativity]
propertiesDifference3 = []
propertiesIntersection3 = [prop_intersection_associativity]
propertiesCompound3 = [prop_compound_1, prop_compound_2, prop_compound_3, prop_compound_4, prop_compound_5]


-- Test implementations
prop_union_idempotence s       = setUnion s s == s
prop_union_reflexivity a b     = setUnion a b == setUnion b a
prop_union_identity s          = setUnion s emptySet == s && setUnion emptySet s == s
prop_union_associativity a b c = setUnion (setUnion a b) c == setUnion a (setUnion b c)
prop_union_true a b = let (Set l) = setIntersection a b
                      in  all (\x -> inSet x a || inSet x b) l

prop_intersection_reflexivity a b     = setIntersection a b == setIntersection b a
prop_intersection_idempotence s       = setIntersection s s == s
prop_intersection_zero s   = setIntersection emptySet s == emptySet && setIntersection s emptySet == emptySet
prop_intersection_associativity a b c = setIntersection (setIntersection a b) c == setIntersection a (setIntersection b c)
prop_intersection_true a b = let (Set l) = setIntersection a b
                             in  all (\x -> inSet x a && inSet x b) l


-- | A ∖ A  =  Ø ,   Ø ∖ A  =  Ø
prop_difference_zero s     = setDifference s s == emptySet && setDifference emptySet s == emptySet
-- | A ∖ Ø                 = A
prop_difference_identity s = setDifference s emptySet == s
-- | (A ∖ B) ∖ B = A ∖ B
prop_difference_idempotence a b = setDifference (setDifference a b) b == setDifference a b
prop_difference_true a b   = let (Set l) = setDifference a b
                             in  all (\x -> inSet x a && not (inSet x b)) l


-- | C ∖ (A ∩ B)  =  (C ∖ A)∪(C ∖ B)
prop_compound_1 a b c = setDifference c (setIntersection a b) == setUnion (setDifference c a) (setDifference c b)
-- | C ∖ (A ∪ B)  =  (C ∖ A)∩(C ∖ B)
prop_compound_2 a b c = setDifference c (setUnion a b) == setIntersection (setDifference c a) (setDifference c b)
-- | C ∖ (B ∖ A)  =  (A ∩ C)∪(C ∖ B)
prop_compound_3 a b c = setDifference c (setDifference b a) == setUnion (setIntersection a c) (setDifference c b)
-- | (B ∖ A) ∩ C  =  (B ∩ C) ∖ A  =  B∩(C ∖ A)
prop_compound_4 a b c = setIntersection (setDifference b a) c == setUnion b (setDifference c a)
-- | (B ∖ A) ∪ C  =  (B ∪ C) ∖ (A ∖ C)
prop_compound_5 a b c = setUnion (setDifference b a) c == setDifference (setUnion b c) (setDifference a c)


-- Test harness
-- 1 hour
setTest1 :: [Set Int -> Bool] -> [Set Int] -> Bool
setTest1 props sets = and [p s | p <- props, s <- sets]

setTest2 :: [Set Int -> Set Int -> Bool] -> [Set Int] -> Bool
setTest2 props sets = setTest1 [p s | s <- sets, p <- props] sets

setTest3 :: [Set Int -> Set Int -> Set Int -> Bool] -> [Set Int] -> Bool
setTest3 props sets = setTest2 [p l | l <- sets, p <- props] sets

getRandomSetOrds :: Int -> Int -> IO [Set Int]
getRandomSetOrds maxSetSize numSets = do
    stream <- genIntListR (0, maxSetSize)
    let (lengths, elems) = splitAt numSets stream
    let sets = fst . splitAts lengths $ elems
    return $ map list2set sets

runSetOrdTests maxSize n = do
    sets <- getRandomSetOrds maxSize n
    putStrLn "Intersection: "
    print $ setTest1 propertiesIntersection1 sets
    print $ setTest2 propertiesIntersection2 sets
    print $ setTest3 propertiesIntersection3 sets
    putStrLn "Union: "
    print $ setTest1 propertiesUnion1 sets
    print $ setTest2 propertiesUnion2 sets
    print $ setTest3 propertiesUnion3 sets
    putStrLn "Difference: "
    print $ setTest1 propertiesDifference1 sets
    print $ setTest2 propertiesDifference2 sets
    print $ setTest3 propertiesDifference3 sets

