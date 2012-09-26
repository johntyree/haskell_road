module ParseFOL where 

import Data.List
import FOL
import ScanFOL

type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed x xs = [(x,xs)]

parseTerm :: Parser Token Term
parseTerm (TokenName name: TokenOB : tokens) = 
  [ (F name terms, rest) | (terms,rest) <- parseTerms tokens ]
parseTerm (TokenName name: tokens) = [(V name, tokens)]
parseTerm tokens = []

parseTerms :: Parser Token [Term] 
parseTerms (TokenCB : tokens) = succeed [] tokens
parseTerms tokens = 
  [(t:ts, rest) | (t,TokenComma:ys) <- parseTerm tokens, 
                     (ts,rest) <- parseTerms ys ]
    ++
  [([t], rest) | (t,TokenCB:rest) <- parseTerm tokens ]

parseName :: Parser Token Name
parseName (TokenName name: tokens) = [(name,tokens)] 
parseName tokens = []

parseNames  :: Parser Token [Name]
--parseNames (TokenColon : tokens) = succeed [] tokens
parseNames tokens = 
   [(n:ns, rest) | (n,ys) <- parseName tokens, 
                   (ns,rest) <- parseNames ys ]
     ++
   [([n], rest) | (n,TokenColon:rest) <- parseName tokens ]

parseForm :: Parser Token Formula 
parseForm (TokenPred name: TokenOB: tokens) = 
   [(Atom name ts, rest) | (ts,rest) <- parseTerms tokens ]
parseForm (TokenName name: tokens) = 
   [(Eq t1 t2, rest) | 
      (t1, TokenEqSign:ys) 
           <- parseTerm (TokenName name: tokens), 
      (t2, rest) <- parseTerm ys ]
parseForm (TokenNeg : tokens) =
  [ (Neg f, rest) | (f,rest) <- parseForm tokens ]
parseForm (TokenCnj : TokenOP : tokens) = 
  [ (Conj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenDsj : TokenOP : tokens) = 
  [ (Disj fs, rest) | (fs,rest) <- parseForms tokens ]

parseForm (TokenForall : tokens) = 
    [ (Forall vs f, rest) | (vs,ys) <- parseNames tokens, 
                            (f,rest) <- parseForm ys ]
parseForm (TokenExist : tokens) = 
    [ (Exist vs f, rest) | (vs,ys) <- parseNames tokens, 
                           (f,rest) <- parseForm ys ]
parseForm (TokenOP : tokens) = 
    [ (Impl f1 f2, rest) | 
         (f1,TokenImpl:ys) <- parseForm tokens,
         (f2,TokenCP:rest) <- parseForm ys ]
     ++
    [ (Equi f1 f2, rest) | 
         (f1,TokenEquiv:ys) <- parseForm tokens,
         (f2,TokenCP:rest) <- parseForm ys ] 
parseForm tokens = []

parseForms :: Parser Token [Formula] 
parseForms (TokenCP : tokens) = succeed [] tokens
parseForms tokens = 
   [(f:fs, rest) | (f,TokenComma:ys) <- parseForm tokens, 
                   (fs,rest) <- parseForms ys ]
     ++ 
   [([f],rest) | (f,TokenCP:rest) <- parseForm tokens ]

parse :: String -> [Formula]
parse s = [ f | (f,_) <- parseForm (lexer s) ]

