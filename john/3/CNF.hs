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
cnf (Cnj fs)     = flatform $ Cnj (map cnf fs)
cnf (Dsj (x:fs)) = flatform $ dist (cnf x, cnf (Dsj fs))
cnf x = x

cnf' :: Form -> Form
cnf' (Cnj fs)     = Cnj (map cnf' fs)
cnf' (Dsj (x:fs)) = dist (cnf' x, cnf' (Dsj fs))
cnf' x = x

-- precondition: f1 p2 are in cnf
dist :: (Form, Form) -> Form
dist ((Cnj (x:y:xs)), f2) = Cnj [dist (x, f2), dist (y, f2)]
dist (f1, (Cnj (x:y:xs))) = Cnj [dist (f1, x), dist (f1, y)]
dist (Dsj f1, Dsj f2)     = Dsj (f1 ++ f2)
dist (f1, Dsj f2)         = Dsj (f1:f2)
dist (Dsj f1, f2)         = Dsj (f2:f1)
dist (f1, f2)             = Dsj [f1, f2]


form4 = Cnj [Cnj [p, q], Cnj [p, r], Dsj [Neg q, r],  Cnj [p, r]]
form5 = Equiv (Neg p) (Neg p)
form6 = Dsj [Neg (Prop 0),Cnj [Neg (Prop 1),Prop 1],Neg (Prop (2)),Neg (Prop 0),Cnj [Prop (2),Dsj [Neg (Neg (Prop 1)),Neg (Prop 1),Prop 1,Prop (2)],Prop 0,Prop 1]]

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
flatform (Cnj fs) = let Cnj x = foldr go (Cnj []) (fs)
                    in  Cnj (clean x)
  where
    clean = map flatform . nub
    go (Cnj fs) (Cnj acc) = Cnj (clean $ fs ++ acc)
    go f (Cnj acc) = Cnj (clean $ f:acc)

flatform (Dsj (x:[])) = x
flatform (Dsj fs) = let Dsj x = foldr go (Dsj []) (fs)
                    in  Dsj (clean x)
  where
    clean = map flatform . nub
    go (Dsj fs) (Dsj acc) = Dsj (fs ++ acc)
    go f (Dsj acc) = Dsj (f:acc)

flatform f = f


instance Arbitrary Form where
    arbitrary =
       frequency [ (13, Prop <$> choose (0,1))
                 , (6, Neg   <$> arbitrary)
                 , (2, Cnj   <$> longEnough (listOf (resize 4 arbitrary)))
                 , (2, Dsj   <$> longEnough (listOf (resize 4 arbitrary)))
                 -- , (1, Impl  <$> arbitrary <*> arbitrary)
                 -- , (1, Equiv <$> arbitrary <*> arbitrary)
                 ]
      where
        longEnough g = suchThat g (\l -> length l > 1)


instance CoArbitrary Form where
    coarbitrary c = case p of
        Prop  n   -> variant 0 . coarbitrary n
        Neg   f   -> variant 1 . coarbitrary f
        Cnj   fs  -> variant 2 . coarbitrary fs
        Dsj   fs  -> variant 3 . coarbitrary fs
        Impl  f g -> variant 4 . coarbitrary f . coarbitrary g
        Equiv f g -> variant 5 . coarbitrary f . coarbitrary g


-- http://www.youtube.com/watch?v=HbX7pxYXsHg&feature=youtu.be
