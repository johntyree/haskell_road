module CNF

where

import Data.List hiding (nub)
import Data.Set (toAscList, fromList)
import Test.QuickCheck
import Control.Applicative
import Data.Ratio ((%))

import Debug.Trace

import Week2
import Form2Bool
import Text.Printf

-- Toggle debugging
debug :: Bool
-- debug = True
debug = False
-- tracer :: (Show a, Show a1) => (a1 -> a) -> [Char] -> a1 -> a
tracer func funcName check label arg =
    let result = func arg
        result' = show result
        funcName' = printf "%8s" funcName
        label' = printf "%-15s" label
        arg' = printf "%-14s" (show arg)
    in if not (check arg result)
       then error $ "Failed check: " ++ funcName' ++ " " ++ label' ++ ": " ++ arg' ++ " -> " ++ result'
       else traceStack (funcName' ++ " " ++ label' ++ ": " ++ arg' ++ " -> " ++ result') result

nubsort :: Ord a => [a] -> [a]
nubsort = toAscList . fromList

-- | Apply entire transformation chain, from generic formula to equivalent valid
-- CNF.
fullcnf :: Form -> Form
fullcnf = (!! 6) . iterate flatform . cnf "FullCNF" . nnf . arrowfree

-- | cnf converts to conjunctive normal form from nnf.
-- precondition: input is arrow-free and in NNF
cnf :: String -> Form -> Form
cnf = if debug then tracer cnf' "cnf" equiv else const cnf'

cnf' :: Form -> Form
cnf' (Cnj [])  = Cnj []
cnf' (Cnj [x]) = cnf "Single" $ x
cnf' (Cnj fs)  = case filter (/= Cnj []) $ map (cnf "Case 2") fs of
                   (x:xs) -> if Dsj [] `elem` (x:xs)  -- If there's a contradiction, all is lost
                             then Dsj []           -- Propagate the false up
                             else Cnj (x:xs)
                   x      -> cnf "Filtered Cnj" $ Cnj x

cnf' (Dsj [])  = Dsj []
cnf' (Dsj [x]) = cnf "Single" $ x
cnf' (Dsj fs)  = case filter (/= Dsj []) $ map (cnf "Case 3") fs of
                   (x:xs) -> if Cnj [] `elem` (x:xs)  -- If there's a tautology, all is won
                             then Cnj []              -- Propagate the true up
                             else dist "cnf Case 3" (cnf "Case 3 Left" x
                                                   , cnf "Case 3 Right" (Dsj xs))
                   x      -> cnf "Filtered Dsj" $ Dsj x


cnf' (Prop x) = Prop x
cnf' (Neg (Prop x)) = Neg (Prop x)
cnf' x   = error $ "Formula wasn't in nnf and arrowfree: " ++ show x

-- precondition: f1 f2 are in cnf
dist :: String -> (Form, Form) -> Form
dist = if debug then tracer dist' "dist" (const . const $ True) else const dist'
-- dist ((Cnj (x:y:[])), f2) = Cnj [dist (x, f2), dist (y, f2)]
-- dist (f1, (Cnj (x:y:[]))) = Cnj [dist (f1, x), dist (f1, y)]

-- dist ((Cnj (x:y:xs)), f2) = Cnj [dist (x, f2), dist (Cnj (y:xs), f2)]
-- dist (f1, ( Cnj (x:y:xs))) = Cnj [dist (f1, x), dist (f1, Cnj (y:xs))]

dist' (Cnj [x], f2)    = dist "Left Cnj Single" (x, f2) -- Cnj [x] ≡ x
dist' (f1, Cnj [x])    = dist "Right Cnj Single" (f1, x)

dist' (Dsj [x], f2)    = dist "Left Dsj Single" (x, f2) -- Dsj [x] ≡ x
dist' (f1, Dsj [x])    = dist "Right Dsj Single" (f1, x)

dist' (Cnj (x:xs), f2) = Cnj [dist "Case 1 Left" (x, f2), dist "Case 1 Right" (Cnj xs, f2)]
dist' (f1, Cnj (x:xs)) = Cnj [dist "Case 2 Left" (f1, x), dist "Case 2 Right" (f1, Cnj xs)]

dist' (Dsj f1, Dsj f2) = Dsj (f1 ++ f2)
dist' (f1, Dsj f2)     = Dsj (f1:f2)
dist' (Dsj f1, f2)     = Dsj (f2:f1)
dist' (f1, f2)         = Dsj [f1, f2]


-- | flatform beautifies our formula. Remove redundant parentheses and tree-like structures
-- from conjunctions and disjunctions. May need to be called twice... :(
flatform :: Form -> Form
flatform form = case form of
        Cnj [x] -> flatform x
        Cnj fs  -> let Cnj fs' = foldl' go (Cnj []) fs
                   in  Cnj . map flatform . nubsort $ fs'
        Dsj [x] -> flatform x
        Dsj fs  -> let Dsj fs' = foldl' go (Dsj []) (nubsort fs)
                   in  Dsj . map flatform . nubsort $ fs'
        f -> f
      where
        go (Cnj acc) (Cnj [])  = Cnj acc  -- Empty Cnj is identity
        go (Cnj acc) (Dsj [])  = Dsj []   -- Empty Dsj is false
        go (Cnj acc) (Dsj [x]) = Cnj $ x : acc
        go (Cnj acc) (Dsj fs)  = Cnj $ Dsj fs : acc
        go (Cnj acc) (Cnj fs)  = Cnj $ fs ++ acc -- Merge nested
        go (Cnj acc) f         = Cnj $ f : acc
        go (Dsj acc) (Dsj [])  = Dsj acc -- Empty Dsj is identity
        go (Dsj acc) (Cnj [])  = Cnj []  -- Empty Cnj is True
        go (Dsj acc) (Cnj [x]) = Dsj $ x : acc
        go (Dsj acc) (Cnj fs)  = Dsj $ Cnj fs : acc
        go (Dsj acc) (Dsj fs)  = Dsj $ fs ++ acc -- Merge nested
        go (Dsj acc) f         = Dsj $ f : acc
        go acc x                 = error $ "How did this happen: " ++ show acc ++ " " ++ show x

-- * Extra functions for testing

isCnf :: Form -> Bool
isCnf (Dsj fs) = (not . any containsCnj $ fs) || error (intercalate "\n" ("Had cnj under dsj: " : map show fs))
isCnf (Cnj fs) = all isCnf fs
isCnf f        = (not $ containsCnj f || containsDsj f) || error ("Had cnj or dsj under prop: " ++ show f)


containsCnj :: Form -> Bool
containsCnj (Cnj fs) = True
containsCnj (Dsj fs) = any containsCnj fs
containsCnj _        = False

isDnf :: Form -> Bool
isDnf (Dsj fs) = all isDnf fs
isDnf (Cnj fs) = not . any containsDsj $ fs
isDnf f        = not $ containsCnj f || containsDsj f


containsDsj :: Form -> Bool
containsDsj (Cnj fs) = any containsDsj fs
containsDsj (Dsj fs) = True
containsDsj _        = False

-- * QuickCheck tests and instances

-- | A metric for determining the "complexity" of a formula. Useful for
-- preventing QuickCheck from launching into orbit. It's much faster to
-- just keep generating formulas until they are "just right" in complexity.
complexity :: Form -> Int
complexity (Prop name)   = 1
complexity (Neg f)       = 1 + complexity f
complexity (Cnj fs)      = 1 + (sum $ map complexity fs)
complexity (Dsj fs)      = 1 + (sum $ map complexity fs)
complexity (Impl f1 f2)  = 1 + complexity f1 + complexity f2
complexity (Equiv f1 f2) = 1 + complexity f1 + complexity f2

depthBetween :: Int -> Int -> Form -> Bool
depthBetween low hi f = dB low hi f
    where
      dB l h (Prop name)   = l <= 0 && 0 <= h
      dB l h (Neg f)       = 0 <= h && dB (l-1) (h-1) f
      dB l h (Cnj fs)      = 0 <= h && all (dB (l-2) (h-2)) fs
      dB l h (Dsj fs)      = 0 <= h && all (dB (l-2) (h-2)) fs
      dB l h (Impl f1 f2)  = 0 <= h && all (dB (l-2) (h-2)) [f1,f2]
      dB l h (Equiv f1 f2) = 0 <= h && all (dB (l-2) (h-2)) [f1,f2]

prop_flat, prop_equiv, prop_valid :: Form -> Bool

-- | Flat formulas are flat
prop_flat form = let f = fullcnf $ form
                 in f == flatform f

-- | Formulas in CNF should be equivalent to their original form.
prop_equiv form = let cnfform = fullcnf form
                  in equiv form cnfform

-- | Formulas should always be in valid CNF after converting.
prop_valid = isCnf . fullcnf

properties = [ prop_equiv
             , prop_valid
             -- , prop_flat
             ]

-- runCnfTests n = mapM_ (verboseCheckWith stdArgs {maxSuccess = n}) properties
runCnfTests n = mapM_ (quickCheckWith stdArgs {maxSuccess = n}) properties


instance Arbitrary Form where
    arbitrary = complexEnough $ arb
      where
        arb = frequency [ (25, Prop <$> choose (0,3))
                        , (7, Neg   <$> arb)
                        , (2, Cnj   <$> resize 5 (listOf arb))
                        , (2, Dsj   <$> resize 5 (listOf arb))
                        , (1, Impl  <$> arb <*> arb)
                        , (1, Equiv <$> arb <*> arb)
                        ]
        complexEnough = flip suchThat (depthBetween 4 15)

instance CoArbitrary Form where
    coarbitrary c = case p of
        Prop  n   -> variant 0 . coarbitrary n
        Neg   f   -> variant 1 . coarbitrary f
        Cnj   fs  -> variant 2 . coarbitrary fs
        Dsj   fs  -> variant 3 . coarbitrary fs
        Impl  f g -> variant 4 . coarbitrary f . coarbitrary g
        Equiv f g -> variant 5 . coarbitrary f . coarbitrary g

