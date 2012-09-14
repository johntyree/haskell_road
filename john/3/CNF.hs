module CNF

where

import Data.List

import Week2
import Form2Bool

fullcnf :: Form -> Form
fullcnf = cnf . nnf . arrowfree

-- precondition: input is arrow-free and in NNF
cnf :: Form -> Form
cnf (Cnj fs) = flatform $ Cnj (map cnf fs)
cnf (Dsj (x:fs)) = flatform $ dist (cnf x, cnf (Dsj fs))
cnf x = x

-- precondition: f1 p2 are in cnf
dist :: (Form, Form) -> Form
dist ((Cnj (x:y:xs)), f2) = Cnj [dist (x, f2), dist (y, f2)]
dist (f1, (Cnj (x:y:xs))) = Cnj [dist (f1, x), dist (f1, y)]
dist (Dsj f1, Dsj f2)     = Dsj (f1 ++ f2)
dist (f1, Dsj f2)         = Dsj (f1:f2)
dist (Dsj f1, f2)         = Dsj (f2:f1)
dist (f1, f2)             = Dsj [f1, f2]


form4 = Cnj [Cnj [p, q], Dsj [Neg q, r],  Cnj [p, r]]

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

containsDsj :: Form -> Bool
containsDsj (Cnj fs) = any containsDsj fs
containsDsj (Dsj fs) = True
containsDsj _        = False


flatform (Cnj fs) = foldr go (Cnj []) (map flatform fs)
  where
    go (Cnj fs) (Cnj acc) = Cnj (map flatform fs ++ acc)
    go f (Cnj acc) = Cnj (f:acc)

flatform (Dsj fs) = foldr go (Dsj []) (map flatform fs)
  where
    go (Dsj fs) (Dsj acc) = Dsj (map flatform fs ++ acc)
    go f (Dsj acc) = Dsj (f:acc)

flatform f = f

simplifyCnj (Cnj (x:xs)) = map f xs
    where f (Neg y) = x == y
          f _       = False



