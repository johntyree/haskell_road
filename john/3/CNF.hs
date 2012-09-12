module CNF

where

import Week2

-- precondition: input is arrow-free and in NNF
cnf :: Form -> Form
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj (x:fs)) = dist (cnf x, cnf (Dsj fs))
cnf x = x

-- precondition: p1 p2 are in cnf
dist :: (Form, Form) -> Form
dist ((Cnj (x:xs)), f2) = Cnj [dist (x, f2), dist (Cnj xs, f2)]
dist (f1, (Cnj (x:xs))) = Cnj [dist (f1, x), dist (f1, Cnj xs)]
dist (f1, f2) = Dsj [f1, f2]
