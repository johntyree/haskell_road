module CNF

where

import Data.List
import Test.QuickCheck
import Control.Applicative

import Week2
import Form2Bool

fullcnf :: Form -> Form
fullcnf = cnf . nnf . arrowfree

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

-- dist ((Cnj [x]), f2) = Cnj [dist (x, f2), dist (nj (y:xs), f2)]
-- dist (f1, (Cnj [x])) = Cnj [dist (f1, x), dist (f1, Cnj (y:xs))]
dist (Dsj f1, Dsj f2)     = Dsj (f1 ++ f2)
dist (f1, Dsj f2)         = Dsj (f1:f2)
dist (Dsj f1, f2)         = Dsj (f2:f1)
dist (f1, f2)             = Dsj [f1, f2]


form4 = Cnj [Cnj [p, q], Cnj [p, r], Dsj [Neg q, r],  Cnj [p, r]]
form5 = Equiv (Neg p) (Neg p)
form6 = Dsj [Neg (Prop 0)
            ,Cnj [Neg (Prop 1),Prop 1]
            ,Neg (Prop (2))
            ,Neg (Prop 0)
            ,Cnj [ Prop (2)
                 , Dsj [Neg (Neg (Prop 1)) ,Neg (Prop 1) ,Prop 1 ,Prop (2)]
                 , Prop 0
                 , Prop 1]
                 ]


check form = let f = id $ fullcnf form
             in (form, f, isCnf f, equiv form f)


isCnf :: Form -> Bool
isCnf (Dsj fs) = not . any containsCnj $ fs
isCnf (Cnj fs) = all isCnf fs
isCnf f        = not $ containsCnj f || containsDsj f


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


flatform (Cnj [x]) = x
flatform (Cnj fs) = foldr go (Cnj []) (nub fs)
  where
    clean = map flatform . nub
    go (Cnj fs) (Cnj acc) = Cnj (clean $ fs ++ acc)
    go f (Cnj acc) = Cnj (clean $ f:acc)

flatform (Dsj (x:[])) = x
flatform (Dsj fs) = foldr go (Dsj []) (nub fs)
  where
    clean = map flatform . nub
    go (Dsj fs) (Dsj acc) = Dsj (fs ++ acc)
    go f (Dsj acc) = Dsj (f:acc)

flatform f = f


complexity :: Form -> Int
complexity (Prop name)   = 1
complexity (Neg f)       = 1 + complexity f
complexity (Cnj fs)      = 1 + (sum $ map complexity fs)
complexity (Dsj fs)      = 1 + (sum $ map complexity fs)
complexity (Impl f1 f2)  = 1 + complexity f1 + complexity f2
complexity (Equiv f1 f2) = 1 + complexity f1 + complexity f2


prop_equiv form = let cnfform = (cnf . nnf . arrowfree $ form)
                     in equiv form cnfform
prop_valid = isCnf . fullcnf

properties = [ prop_equiv
             , prop_valid
             ]

runTests = mapM_ (verboseCheckWith stdArgs {maxSuccess = 500}) properties

instance Arbitrary Form where
    arbitrary = complexEnough [(> 2), (< 100)] $ arb
      where
        arb = frequency [ (25, Prop <$> choose (0,4))
                        , (6, Neg   <$> arb)
                        , (2, Cnj   <$> longEnough (resize 5 (listOf arb)))
                        , (2, Dsj   <$> longEnough (resize 5 (listOf arb)))
                        , (1, Impl  <$> arb <*> arb)
                        , (1, Equiv <$> arb <*> arb)
                        ]
        longEnough g  = suchThat g (\l -> length l > 1)
        complexEnough ps = flip suchThat (\f -> and . map ($ complexity f) $ ps)

instance CoArbitrary Form where
    coarbitrary c = case p of
        Prop  n   -> variant 0 . coarbitrary n
        Neg   f   -> variant 1 . coarbitrary f
        Cnj   fs  -> variant 2 . coarbitrary fs
        Dsj   fs  -> variant 3 . coarbitrary fs
        Impl  f g -> variant 4 . coarbitrary f . coarbitrary g
        Equiv f g -> variant 5 . coarbitrary f . coarbitrary g


-- http://www.youtube.com/watch?v=HbX7pxYXsHg&feature=youtu.be
