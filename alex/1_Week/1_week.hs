-- Exercise 1.5
devides d n = rem n d == 0
ld n = ldf 2 n
ldf k n | devides k n = k
        | k^2 > n = n
        | otherwise = ldf (k + 1) n

prime0 n | n < 1 = error "not a positive int"
         | n == 1 = False
         | otherwise = ld n == n

-- Exercise 1.6
-- rem :: Integer -> Integer -> Integer

-- Exercise 1.9
new_max :: Int -> Int -> Int
new_max x y | x > y = x
            | otherwise = y

-- Exercise 1.10
removeFst :: Eq a => a -> [a] -> [a]
removeFst m [] = []
removeFst m (x:xs) | m == x = xs
                   | otherwise = x : (removeFst m xs)

-- Exercise 1.13
countChs :: Char -> String -> Int
countChs c [] = 0
countChs c (x:xs) | c == x = 1 + (countChs c xs)
                  | otherwise = countChs c xs

-- Exercise 1.14
blowup :: String -> String
blowup s = blowstr 1 s

blowstr :: Int -> [a] -> [a]
blowstr n [] = []
blowstr n (x:xs) = replicate' n x ++ blowstr (n + 1) xs

replicate' :: Int -> a -> [a]
replicate' n x | n <= 0 = []
               | otherwise = x : replicate' (n - 1) x

-- Exercise 1.15
srtString:: [String] -> [String]
srtString [] = []
srtString xs = m : (srtString (removeFst m xs)) where m = minStr xs

minStr :: [String] -> String
minStr [] = error "empty list"
minStr [x] = x
minStr (x:xs) = min x (minStr xs)

-- Exercise 1.17
prefix:: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

substring :: String -> String -> Bool
substring (_:xs) (y:ys) | prefix xs ys = True
                    | ys == y:ys && (substring xs ys) = True
                    | otherwise = False

-- Exercise 1.18
--1. :t ["one", "two", "three"]
--2. :t (True, "one")
--3. :t [(True, "one"), (False, "two")]
--4. :t ([True, False, True), "one"]

-- Exercise 1.19
-- 1. :t head = [a] -> [a], return the first element of an arbitary list
-- 2. :t last = [a] -> [a], return the last element of an arbitary list
-- 3. :t init = [a] -> [a], return all but the last element of an arbitary
-- list
-- 4. :t fst = (a,b) -> a, return the first element of a tuple
-- 5. (++) :t [a] -> [a] -> [a], concatenate to arbitary lists
-- 6. :t flip (a -> b -> c) -> b -> a -> c, take a function and apply the
-- arguments in flipped order
-- 7. :t flip (++) [a] -> [a] -> [a], concatenate two arbitary lists but
-- put the second list in the begining

-- Exercise 1.20
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

lengths :: [[a]] -> [Int]
lengths xs = map length' xs

-- Exercise 1.21
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

sumLengths :: [[a]] -> Int
sumLengths xs = sum' (map length' xs)

-- Exercise 2.2
(|||) :: Bool -> Bool -> Bool
False ||| x = x
True ||| x | x == True = False
           | otherwise = True

-- Exercise 2.4
-- same..

-- Exercise 2.9
check :: Bool -> Bool -> Bool
check p q = (p ||| q) ||| q
-- output same as whatever p is

-- Exercise 2.13
infix 1 ==>
(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y

logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 = (bf1 True == bf2 True) && (bf1 False == bf2 False)

test1a = not True == False
test1b = not False == True
test2 = logEquiv1 (\ p -> p ==> False) (\ p -> not p)
test3a = logEquiv1 (\ p -> p || True) (\ p -> True)
test3b = logEquiv1 (\ p -> p && False) (\ p -> False)
test4a = logEquiv1 (\p -> p || False) (\ p -> p)
test4b = logEquiv1 (\ p -> p && True) (\ p -> p)
test5 = logEquiv1 (\ p -> p || not p) (\ p -> True)
test6 = logEquiv1 (\ p -> p && not p) (\ p -> False)

--Exercise 2.15
prop1 :: ( Bool -> Bool ) -> Bool
prop1 bf1 = (bf1 True /= True) && (bf1 False /= True)
prop2 :: (Bool -> Bool -> Bool) -> Bool
prop2 bf1 = and [(bf1 p q) /= True | p <- [True, False], q <- [True, False]]
--prop3...

--Exercise 2.17
