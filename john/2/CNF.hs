module CNF

where

import Data.List hiding (nub)
import Data.Set (toAscList, fromList)
import Test.QuickCheck
import Control.Applicative
import Data.Ratio ((%))

import Week2
import Form2Bool


nubsort :: Ord a => [a] -> [a]
nubsort = toAscList . fromList

-- | Apply entire transformation chain, from generic formula to equivalent valid
-- CNF.
fullcnf :: Form -> Form
fullcnf = cnf . nnf . arrowfree

-- | cnf converts to conjunctive normal form from nnf.
-- precondition: input is arrow-free and in NNF
cnf :: Form -> Form
cnf = flatform . cnf'

cnf' :: Form -> Form
cnf' (Cnj fs)     = Cnj (map cnf' fs)
cnf' (Dsj (x:fs)) = dist (cnf' x, cnf' (Dsj fs))
cnf' x = x

-- precondition: f1 p2 are in cnf
dist :: (Form, Form) -> Form
dist ((Cnj (x:y:[])), f2) = Cnj [dist (x, f2), dist (y, f2)]
dist (f1, (Cnj (x:y:[]))) = Cnj [dist (f1, x), dist (f1, y)]

dist ((Cnj (x:y:xs)), f2) = Cnj [dist (x, f2), dist (Cnj (y:xs), f2)]
dist (f1, (Cnj (x:y:xs))) = Cnj [dist (f1, x), dist (f1, Cnj (y:xs))]

dist (Dsj f1, Dsj f2)     = Dsj (f1 ++ f2)
dist (f1, Dsj f2)         = Dsj (f1:f2)
dist (Dsj f1, f2)         = Dsj (f2:f1)
dist (f1, f2)             = Dsj [f1, f2]


-- | flatform beautifies our formula. Remove redundant parentheses and tree-like structures
-- from conjunctions and disjunctions.
flatform :: Form -> Form
flatform (Cnj [x]) = x
flatform (Cnj fs) = foldl' go (Cnj []) (nubsort fs)
  where
    clean = map flatform
    go (Cnj acc) (Cnj []) = Cnj (clean acc)
    go (Cnj acc) (Cnj fs) = Cnj (fs ++ acc)
    go (Cnj acc) f = Cnj (f:acc)

flatform (Dsj (x:[])) = x
flatform (Dsj fs) = foldl' go (Dsj []) (nubsort fs)
  where
    clean = map flatform
    go (Dsj acc) (Dsj []) = Dsj (clean acc)
    go (Dsj acc) (Dsj fs) = Dsj (fs ++ acc)
    go (Dsj acc) f = Dsj (f:acc)

flatform f = f



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

prop_equiv, prop_valid :: Form -> Bool

-- | Formulas in CNF should be equivalent to their original form.
prop_equiv form = let cnfform = fullcnf form
                     in equiv form cnfform

-- | Formulas should always be in valid CNF after converting.
prop_valid = isCnf . fullcnf

properties = [ prop_equiv
             , prop_valid
             ]

runCnfTests n = mapM_ (verboseCheckWith stdArgs {maxSuccess = n}) properties


instance Arbitrary Form where
    arbitrary = complexEnough $ arb
      where
        arb = frequency [ (25, Prop <$> choose (0,3))
                        , (7, Neg   <$> arb)
                        , (2, Cnj   <$> longEnough (resize 5 (listOf arb)))
                        , (2, Dsj   <$> longEnough (resize 5 (listOf arb)))
                        , (1, Impl  <$> arb <*> arb)
                        , (1, Equiv <$> arb <*> arb)
                        ]
        longEnough g  = suchThat g (\l -> length l > 1)
        complexEnough = flip suchThat (depthBetween 4 15)

instance CoArbitrary Form where
    coarbitrary c = case p of
        Prop  n   -> variant 0 . coarbitrary n
        Neg   f   -> variant 1 . coarbitrary f
        Cnj   fs  -> variant 2 . coarbitrary fs
        Dsj   fs  -> variant 3 . coarbitrary fs
        Impl  f g -> variant 4 . coarbitrary f . coarbitrary g
        Equiv f g -> variant 5 . coarbitrary f . coarbitrary g

