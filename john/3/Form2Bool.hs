
module Form2Bool

where

import Week2

none = and . map not

contradiction :: Form -> Bool
contradiction form = none . map (flip eval form) . allVals $ form


tautology :: Form -> Bool
tautology form = and . map (flip eval form) . allVals $ form


entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)


equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Cnj [(Impl f1 f2), (Impl f2 f1)])

-- Time: 1 hour to figure out what entails *really* means w.r.t.
-- implication. 5 minutes to write and check functions.
