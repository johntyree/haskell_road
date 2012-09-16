module CNF

where

import Data.List hiding (nub)
import Data.Set (toAscList, fromList)
import Test.QuickCheck
import Control.Applicative
import Test.Feat
import Test.Feat.Modifiers
import Test.Feat.Enumerate
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
isCnf (Dsj fs) = (not . any containsCnj $ fs) ||  error (intercalate "\n" ("Had cnj under dsj: " : map show fs))
isCnf (Cnj fs) = all isCnf fs
isCnf f        = not $ containsCnj f || containsDsj f || error ("Had cnj or dsj under prop: " ++ show f)


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

runTests n = mapM_ (verboseCheckWith stdArgs {maxSuccess = n}) properties


instance Arbitrary Form where
    arbitrary = sized $ \n -> uniformWith (shared $ map (head . snd) . drop 1 . boundedBoth 2 $ n) n
    -- arbitrary = complexEnough $ arb
      -- where
        -- arb = frequency [ (22, Prop <$> choose (0,3))
                        -- , (6, Neg   <$> arb)
                        -- , (2, Cnj   <$> longEnough (resize 5 (listOf arb)))
                        -- , (2, Dsj   <$> longEnough (resize 5 (listOf arb)))
                        -- , (1, Impl  <$> arb <*> arb)
                        -- , (1, Equiv <$> arb <*> arb)
                        -- ]
        -- longEnough g  = suchThat g (\l -> length l > 1)
        -- complexEnough = flip suchThat (depthBetween 4 15)

instance CoArbitrary Form where
    coarbitrary c = case p of
        Prop  n   -> variant 0 . coarbitrary n
        Neg   f   -> variant 1 . coarbitrary f
        Cnj   fs  -> variant 2 . coarbitrary fs
        Dsj   fs  -> variant 3 . coarbitrary fs
        Impl  f g -> variant 4 . coarbitrary f . coarbitrary g
        Equiv f g -> variant 5 . coarbitrary f . coarbitrary g


instance Enumerable Form
  where
    enumerate = consts $
        [ pay . pay $ Impl <$> shared <*> shared
        , pay . pay $ Equiv <$> shared <*> shared
        , pay $ unary $ Cnj . nonEmpty
        , pay $ unary $ Dsj . nonEmpty
        , unary Neg
        , fromParts [Finite 4 Prop]
        ]

-- +(-((-1) ==> (2)) 3 *(-2) 3 --((3) <=> (3)) *(3 3) -3 --0 1 1)
f1 = Neg (Dsj [Impl (Prop 0) (Neg (Neg (Neg (Prop 1)))),Impl (Prop 2) (Equiv (Equiv (Prop 3) (Prop 0)) (Prop 2)),Equiv (Cnj [Equiv (Prop 1) (Equiv (Neg (Prop 1)) (Prop 3))]) (Cnj [Neg (Impl (Prop 3) (Prop 0))]),Prop 2,Prop 0,Prop 1,Neg (Neg (Prop 3)),Impl (Prop 0) (Impl (Prop 1) (Neg (Prop 1))),Impl (Prop 2) (Prop 1),Neg (Neg (Prop 2))])

f2 = Equiv (Prop 1) (Equiv (Neg (Neg (Dsj [Prop 1]))) (Prop 2))

-- | Non class version of 'bounded'.
boundedBoth :: (Enumerable a, Integral b) => b -> b -> [(Integer,[a])]
boundedBoth l h = let (l':h':_) = map fromIntegral [l, h]
                  in  map (samplePart l' h') $ parts optimal

-- Specification: pick at most @m@ evenly distributed values from part @p@ of @e@
-- Return the list length together with the list of the selected values.
samplePart :: Integral b => Index -> Index -> Finite a -> (b,[a])
samplePart l h (Finite crd ix) =
  let  step  =  crd % h
  in if crd <= h
       then (crd,  map ix [l..crd - 1])
       else (h,    map ix [ round (k * step)
                                    | k <- map toRational [l..h-1]])
-- The first value is at index 0 and the last value is at index ~= crd - step
-- This is "fair" if we consider using samplePart on the next part as well.
-- An alternative would be to make the last index used |crd-1|.

-- | Non class version of 'uniform'.
uniformWith :: Enumerate a -> Int -> Gen a
uniformWith = uni . parts where
  uni :: [Finite a] -> Int -> Gen a
  uni  []  _     =  error "uniform: empty enumeration"
  uni  ps  maxp  =  let  (incl, rest)  = splitAt maxp ps
                         fin           = mconcat incl
    in  case fCard fin of
          0  -> uni rest 1
          _  -> do  i <- choose (0,fCard fin-1)
                    return (fIndex fin i)
