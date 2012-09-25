

module CNFTests

where

import Control.Monad

import Week2
import Week3
import CNF
import Form2Bool
import Techniques
import RandomFuncs
import FormulaFuncs

runCnfTests2 complexity n  = do
    forms <- getRandomForms complexity n
    return $ filter (not . fst) [(prop form, form) | form <- forms, prop <- properties]
