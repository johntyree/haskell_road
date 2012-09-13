module CNF

where

import Data.List

import Week2
import Form2Bool

fullcnf :: Form -> Form
fullcnf = cnf . nnf. arrowfree

-- precondition: input is arrow-free and in NNF
cnf :: Form -> Form
cnf (Cnj fs) = flatform $ Cnj (map cnf fs)
cnf (Dsj (x:fs)) = flatform $ dist (cnf x, cnf (Dsj fs))
cnf x = x

-- precondition: f1 p2 are in cnf
dist :: (Form, Form) -> Form

dist ((Cnj (x:y:z:xs)), f2) = Cnj [dist (x, f2), dist (Cnj (y:z:xs), f2)]
dist (f1, (Cnj (x:y:z:xs))) = Cnj [dist (f1, x), dist (f1, Cnj (y:z:xs))]

dist ((Cnj (x:y:xs)), f2) = Cnj [dist (x, f2), dist (y, f2)]
dist (f1, (Cnj (x:y:xs))) = Cnj [dist (f1, x), dist (f1, y)]

dist (Cnj (x:xs), f2) = error $ "Conjunction of single elem L " ++ show (x, xs, f2)
dist (f1, Cnj (x:xs)) = error $ "Conjunction of single elem R " ++ show x

dist (Dsj f1, Dsj f2)   = Dsj (f1 ++ f2)
dist (f1, Dsj f2)       = Dsj (f1:f2)
dist (Dsj f1, f2)       = Dsj (f2:f1)
dist (f1, f2)           = Dsj [f1, f2]


flatform' :: [Form] -> [Form]
flatform' (Cnj x : Cnj y : xs) = nub . sort . flatform' $ Cnj (flatform' (x ++ y)) : xs
flatform' (Dsj x : Dsj y : xs) = nub . sort . flatform' $ Dsj (flatform' (x ++ y)) : xs
flatform' (x:xs)             = x : (nub . sort . flatform') xs
flatform' []                 = []

flatform :: Form -> Form
flatform (Cnj fs) = Cnj $ flatform' fs
flatform (Dsj fs) = Dsj $ flatform' fs
flatform x        = x

-- flatform (Cnj fs) = foldr go (Cnj []) (map flatform $ sort fs)
  -- where go (Cnj (Cnj f1 : Cnj f2 : fs)) (Cnj acc) = Cnj ((Cnj (f1 ++ f2)):fs ++ acc)
        -- go (Csj (Dsj f1 : Dsj f2 : fs)) (Dsj acc) = Dsj ((Dsj (f1 ++ f2)):fs ++ acc)
        -- go (Cnj fs) (Cnj acc)                     = Cnj (fs ++ acc)
        -- go (Dsj fs) (Dsj acc)                     = Dsj (fs ++ acc)
        -- go p        (Cnj acc)                     = Cnj (p:acc)

-- flatform (Dsj fs) = foldr go (Dsj []) (map flatform $ sort fs)
  -- where go (Cnj (Cnj f1 : fs)) (Dsj (Cnj f2 : acc)) = Dsj ((Cnj (f1 ++ f2)):fs ++ acc)
        -- go (Dsj (Dsj f1 : Dsj f2 : fs)) (Dsj (Cnj f2 : acc) = Dsj ((Dsj (f1 ++ f2)):fs ++ acc)
        -- go (Dsj fs) (Dsj acc)                     = Dsj (fs ++ acc)
        -- go (Cnj fs) (Cnj acc)                     = Cnj (fs ++ acc)
        -- go p                            (Dsj acc) = Dsj (p:acc)

-- flatform f = f
