

module Prime

where

import GS

-- Exercise 1.4
-- If we change k^2 > n to k^2 >= n, we do not affect the outcome of the
-- program. If k^2 == n, then n/k == k and "divides k n" will have been
-- true. We won't reach the second equation.


-- Exercise 1.5
-- prime0 Already exists in GS.hs...
prime0' :: Integer -> Bool
prime0' n | n < 1     = error "Not a positive integer."
          | n == 1    = False
          | otherwise = ld n == n

-- Exercise 1.6
-- rem takes an argument of type Integer and returns a function which takes
-- an Integer and returns an Integer, thus
-- rem :: Integral a => a -> a -> a

-- Exercise 1.7
--   *Prime> :t divides
--   divides :: Integral a => a -> a -> Bool
-- What we see is that divides is of type "function from integer to
-- function from an integer to a boolean value", or perhaps more
-- intuitively: "function taking two integers and producing a bool."
--
--   *Prime> :t divides 5
--   divides 5 :: Integral a => a -> Bool
-- Here we have applied divides to the argument 5, producing a value of
-- type "function from integer to bool."

--   *Prime> :t divides 5 7
--   divides 5 7 :: Bool
-- Here divides has been applied to two arguments, leaving only the Bool
-- value, which is known now to be False.... so we should be able to
-- replace this with the literal "False" everywhere and not change
-- anything.


-- Exercise 1.9
maxInt :: Integral a => [a] -> a
maxInt []     = error "Empty list."
maxInt [x]    = x
maxInt (x:xs) = max x (maxInt xs)

maxInt' :: Integral a => [a] -> a
maxInt' = foldr1 max


-- Exercise 1.10
-- In general we would want type
-- removeFst :: Eq a => [a] -> [a]
-- But in this case it says "list of integers" specifically
removeFst :: Integral a => a -> [a] -> [a]
removeFst = removeFst'
removeFst' :: Eq a => a -> [a] -> [a]
removeFst' m [] = []
removeFst' m (x:xs) | m == x = xs
                    | otherwise = x : removeFst' m xs



-- Exercise 1.13
-- Again we might like a general type
-- count :: (Eq a, Integral b) => a -> [a] -> b
-- But the exercise calls for Chars and Strings
-- Using integral because no one likes to be forced to put fromIntegral
-- everywhere...
count :: Integral a => Char -> String -> a
count _ "" = 0
count x (c:s) | x == c    = succ $ count x s
              | otherwise = count x s


-- Exercise 1.14
-- Without leaning on the functions we don't know yet
-- copy == replicate ...
blowup :: String -> String
blowup s = go 1 s
  where
    go _ []    = []
    go n (c:s) = copy n c ++ go (n+1) s
    copy 0 _   = []
    copy n x   = x : copy (n-1) x


blowup' :: String -> String
blowup' = concat . zipWith replicate [1..]



-- Exercise 1.15
-- At first I didn't realize you could compare on lists
listCmp :: Ord a => [a] -> [a] -> Ordering
listCmp [] [] = EQ
listCmp x []  = GT
listCmp [] y  = LT
listCmp (x:xs) (y:ys) | x > y     = GT
                      | x < y     = LT
                      | otherwise = listCmp xs ys

minListElem :: Ord a => [[a]] -> [a]
minListElem [] = error "Empty list."
minListElem [x] = x
minListElem (x:y:xs) | listCmp x y == LT = minListElem (x:xs)
                     | otherwise         = minListElem (y:xs)

listSort :: Ord a => [[a]] -> [[a]]
listSort [] = []
listSort xs = let m = minListElem xs in m : listSort (removeFst' m xs)

srtString :: [String] -> [String]
srtString = listSort


-- But, apparently you can compare them directly.
minElem :: Ord a => [a] -> a
minElem [] = error "Empty list."
minElem [x] = x
minElem (x:y:xs) | x < y     = minElem (x:xs)
                 | otherwise = minElem (y:xs)

sort :: Ord a => [a] -> [a]
sort [] = []
sort xs = let m = minElem xs in m : sort (removeFst' m xs)

srtString' :: [String] -> [String]
srtString' = sort

-- Mergesort is a more suitable algorithm
msort p l@(_:_:_) = merge (msort p left) (msort p right)
    where
      (left, right) = split l l
      split (x:xs) (_:_:zs) = let (vs,us) = split xs zs in (x:vs, us)
      split xs     _        = ([],xs)
      merge [] r = r
      merge l [] = l
      merge (l:ls) (r:rs) | p l r     = l : merge ls (r:rs)
                          | otherwise = r : merge (l:ls) rs
msort _ l = l



-- Exercise 1.17
-- We'll use a generalized prefix in our substring definition
prefix' :: Eq a => [a] -> [a] -> Bool
prefix' [] _          = True
prefix' _ []          = False
prefix' (x:xs) (y:ys) = (x == y) && prefix' xs ys

sublist :: Eq a => [a] -> [a] -> Bool
sublist xs ys@(_:ys') = prefix' xs ys || sublist xs ys'
sublist xs ys         = prefix' xs ys

substring :: String -> String -> Bool
substring = sublist

substring' xs [] = prefix' xs []
substring' xs ys | prefix' xs ys            = True
                 | substring' xs (tail ys) = True
                 | otherwise               = False



-- Exercise 1.18
e1_18_1 = [""] :: [String]
e1_18_2 = (True, "") :: (Bool, String)
e1_18_3 = [e1_18_2] :: [(Bool, String)]
e1_18_4 = ([True], "") :: ([Bool], String)
e1_18_5 = not :: Bool -> Bool



-- Exercise 1.19
e_19_1 = head      :: [a] -> a
-- head is the first elem of a list, error on empty
e_19_2 = last      :: [a] -> a
-- last is the last elem of the list, error on empty
e_19_3 = init      :: [a] -> [a]
-- init is everything but the last elem of the list, error on empty
e_19_4 = fst       :: (a, b) -> a
-- fst is the first element of a 2-tuple
e_19_5 = (++)      :: [a] -> [a] -> [a]
-- (++) concatenates its two argument lists together
e_19_6 = flip      :: (a -> b -> c) -> b -> a -> c
-- flip reverses the order of the first two arguments of a function
e_19_7 = flip (++) :: [a] -> [a] -> [a]
-- Thus, flip (++) concatenates it's arguments, but by appending the first one
-- to the second one.



-- Exercise 1.20
lengths :: [[a]] -> [Int]
lengths = map length

-- Exercise 1.21
sumLengths = sum . lengths




