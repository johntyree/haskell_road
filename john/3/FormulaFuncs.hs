

module FormulaFuncs where

import Control.Monad
import Data.Char

import Week2
import Week3
import CNF
import Form2Bool
import Techniques
import RandomFuncs

getRandomTerm d =
    let names = "xyz"
        functions = "zspt"
        choice l = liftM (l !!) $ getRandomInt (length l - 1)
    in do n <- getRandomInt 1
          case n of
              0 -> do a <- choice names
                      return $ V [a]
              1 -> do name <- choice functions
                      numTerms <- getRandomInt (d-1) -- Max F arity
                      terms <- getRandomTerms (d-1) numTerms
                      return $ F [name] terms
              _ -> undefined

getRandomTerms d n = replicateM n (getRandomTerm d)

getRandomFormulas d n = replicateM n (getRandomFormula d)


getRandomFormula :: Int -> IO Formula
getRandomFormula d =
    let terms = "xyz"
        relations = "PQR"
        choice l = liftM (l !!) $ getRandomInt (length l - 1)
    in do n <- getRandomInt 7
          case n of
               0 -> do a <- choice relations
                       l <- getRandomInt 2 -- Max relation arity
                       ts <- getRandomTerms (d-1) l
                       return (Atom [a] ts)
               1 -> do f <- getRandomFormula (d-1)
                       return (Week3.Neg f)
               2 -> do m  <- getRandomInt (d-1)
                       fs <- getRandomFormulas (d-1) m
                       return (Conj fs)
               3 -> do m  <- getRandomInt (d-1)
                       fs <- getRandomFormulas (d-1) m
                       return (Disj fs)
               4 -> do f0 <- getRandomFormula (d-1)
                       f1 <- getRandomFormula (d-1)
                       return (Week3.Impl f0 f1)
               5 -> do f0 <- getRandomFormula (d-1)
                       f1 <- getRandomFormula (d-1)
                       return (Equi f0 f1)
               6 -> do n  <- choice terms
                       f1 <- getRandomFormula (d-1)
                       return (Forall [n] f1)
               7 -> do n  <- choice terms
                       f1 <- getRandomFormula (d-1)
                       return (Exists [n] f1)
               _  -> undefined


-- Parser

data FolToken
      = FolName Week3.Name
      | FolEq
      | FolNeg
      | FolImpl
      | FolEquiv
      | FolConj
      | FolDisj
      | FolForall
      | FolExists
      | FolTrue
      | FolFalse
      | FolOP
      | FolCP
      | FolOB
      | FolCB
      | FolSep
 deriving (Show,Eq)

folLexer :: String -> [FolToken]
folLexer [] = []
folLexer ('(':cs) = FolOP : folLexer cs
folLexer (')':cs) = FolCP : folLexer cs
folLexer ('[':cs) = FolOB : folLexer cs
folLexer (']':cs) = FolCB : folLexer cs
folLexer ('c':'o':'n':'j':cs) = FolConj : folLexer cs
folLexer ('d':'i':'s':'j':cs) = FolDisj : folLexer cs
folLexer ('~':cs) = FolNeg : folLexer cs
folLexer ('=':'=':'>':cs) = FolImpl : folLexer cs
folLexer ('<':'=':'>':cs) = FolEquiv : folLexer cs
folLexer ('=':'=':cs) = FolEq : folLexer cs
folLexer ('A':' ':cs) = FolForall : folLexer cs
folLexer ('E':' ':cs) = FolExists : folLexer cs
folLexer (',':cs) = FolSep : folLexer cs
folLexer (c:cs) | isSpace c = folLexer cs
                | isAlpha c = folLexName (c:cs)
folLexer (x:_) = error ("unknown token: " ++ [x])

folLexName :: String -> [FolToken]
folLexName cs = FolName name : folLexer rest
     where (name,rest) = span isAlpha cs


-- type Parser a b = [a] -> [(b,[a])]

-- succeed :: b -> Parser a b
-- succeed x xs = [(x,xs)]

-- parseFormula :: Parser FolToken Formula

-- parseFormula (FolName x : tokens) = (Atom x terms, rest)
  -- where (terms, rest) = parseTerms tokens

-- parseTerms (FolOB : tokens) =
    -- case parseTerm tokens of
        -- ([], all) -> ([], all)
        -- (term, okens) -> let (next, kens) = parseTerms okens (], all)
                         -- in  (term

-- parseFormula (FolName x : tokens) = Atom x [] : parseFormula tokens

-- parseFormula (FolNeg : tokens) =
  -- [ (Week3.Neg f, rest) | (f,rest) <- parseFormula tokens ]

-- parseFormula (FolConj : FolOP : tokens) =
  -- [ (Cnj fs, rest) | (fs,rest) <- parseFormulas tokens ]

-- parseFormula (FolDisj : FolOP : tokens) =
  -- [ (Dsj fs, rest) | (fs,rest) <- parseFormulas tokens ]

-- parseFormula (FolOP : tokens) =
    -- [ (Week3.Impl f1 f2, rest) | (f1,ys) <- parseFormula tokens,
                           -- (f2,rest) <- FormulaFuncs.parseImpl ys ]
     -- ++
    -- [ (Equiv f1 f2, rest) | (f1,ys) <- parseFormula tokens,
                            -- (f2,rest) <- FormulaFuncs.parseEquiv ys ]
-- parseFormula _ = []

-- parseFormulas :: Parser FolToken [Formula]
-- parseFormulas (FolCP : tokens) = succeed [] tokens
-- parseFormulas tokens =
   -- [(f:fs, rest) | (f,ys) <- parseFormula tokens,
                   -- (fs,rest) <- parseFormulas ys ]

-- parseImpl :: Parser FolToken Formula
-- parseImpl (FolImpl : tokens) =
  -- [ (f,ys) | (f,y:ys) <- parseFormula tokens,
              -- y == FolCP ]
-- parseImpl _ = []

-- parseEquiv :: Parser FolToken Formula
-- parseEquiv (FolEquiv : tokens) =
  -- [ (f,ys) | (f,y:ys) <- parseFormula tokens,
              -- y == FolCP ]
-- parseEquiv _ = []

-- parse :: String -> [Formula]
-- parse s = [ f | (f,_) <- parseFormula (folLexer s) ]

