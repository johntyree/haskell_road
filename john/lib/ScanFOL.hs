module ScanFOL where 

import Data.List
import Data.Char

data Token 
      = TokenNeg
      | TokenEqSign
      | TokenCnj
      | TokenDsj
      | TokenImpl
      | TokenEquiv 
      | TokenForall
      | TokenExist
      | TokenName String 
      | TokenPred String 
      | TokenOP
      | TokenCP
      | TokenOB
      | TokenCB
      | TokenComma
      | TokenColon
 deriving (Show,Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
             | isAlpha c && isLower c = lexName(c:cs) 
             | isAlpha c && isUpper c = lexPred(c:cs) 
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('[':cs) = TokenOB : lexer cs
lexer (']':cs) = TokenCB : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer ('!':cs) = TokenForall : lexer cs
lexer ('?':cs) = TokenExist : lexer cs
lexer ('*':cs) = TokenCnj : lexer cs
lexer ('+':cs) = TokenDsj : lexer cs
lexer ('-':cs) = TokenNeg : lexer cs 
lexer ('=':'=':'>':cs) = TokenImpl : lexer cs
lexer ('<':'=':'>':cs) = TokenEquiv : lexer cs
lexer ('=':cs) = TokenEqSign : lexer cs
lexer (x:_) = error ("unknown token: " ++ [x])

lexName cs = TokenName name : lexer rest
     where (name,rest) = span isAlpha cs

lexPred cs = TokenPred name : lexer rest
     where (name,rest) = span isAlpha cs

