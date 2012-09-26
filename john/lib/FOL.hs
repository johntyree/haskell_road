module FOL where

import Data.List

type Name = String
data Term = V Name | F Name [Term] deriving (Eq,Ord)

instance Show Term where 
  show (V name)    = name
  show (F name ts) = name ++ show ts

showNL, showNLrest :: [Name] -> String
showNL [] = ""
showNL (f:fs) = f ++ showNLrest fs
showNLrest [] = ""
showNLrest (f:fs) = ' ': f ++ showNLrest fs

x, y, z :: Term 
x = V "x"
y = V "y"
z = V "z"

varsInTerm :: Term -> [Name]
varsInTerm (V name)       = [name]
varsInTerm (F _ ts) = varsInTerms ts where 
  varsInTerms :: [Term] -> [Name]
  varsInTerms = nub . concat . map varsInTerm

subst :: Name -> Term -> Term -> Term
subst name t (V name') = 
  if name == name' then t else (V name') 
subst name t (F name' ts) = 
      F name' (map (subst name t) ts) 

data Formula = Atom Name [Term]
               | Eq Term Term
               | Neg  Formula 
               | Impl Formula Formula
               | Equi Formula Formula
               | Conj [Formula]
               | Disj [Formula] 
               | Forall [Name] Formula
               | Exist  [Name] Formula
               deriving (Eq,Ord)

instance Show Formula where 
  show (Atom s xs)   = s ++ show xs 
  show (Eq t1 t2)    = show t1 ++ "=" ++ show t2
  show (Neg form)    = '-':(show form)
  show (Impl f1 f2)  = '(': show f1 ++"==>"++ show f2 ++")"
  show (Equi f1 f2)  = '(': show f1 ++"<=>"++ show f2 ++")"
  show (Conj fs)     =  "*(" ++ showLst fs ++ ")"
  show (Disj fs)     =  "+(" ++ showLst fs ++ ")"
  show (Forall vs f) = '!' : showNL vs 
                            ++ ":" ++ show f 
  show (Exist vs f)  =  '?' : showNL vs 
                            ++ ":" ++ show f 

showLst,showRest :: [Formula] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ',': show f ++ showRest fs

r = Atom "R" 

formula1 = Forall ["x"] (r [x,x])
formula2 = Forall ["x","y"]
              (Impl (r [x,y]) (r [y,x]))
formula3 = Forall ["x","y","z"]
              (Impl (Conj [r[x,y], r[y,z]]) (r [x,z]))

type Rint a = Name -> [a] -> Bool
type Fint a = Name -> [a] -> a

type Lookup a = Name -> a

termVal :: Lookup a -> Fint a -> Term -> a 
termVal g i (V name) = g name
termVal g i (F name ts) = 
   i name (map (termVal g i) ts) 

updates :: Eq a => (a -> b) -> [(a,b)] -> a -> b
updates g table = \ v -> maybe (g v) id (lookup v table)

cartProd :: Int -> [a] -> [[a]]
cartProd 0 xs = [[]]
cartProd n xs = [ x:ys | x <- xs, ys <- cartProd (n-1) xs ]

evalFOL :: Eq a => 
   [a] -> Lookup a -> Fint a -> Rint a -> Formula -> Bool
evalFOL domain g f i = evalFOL' g where 
  evalFOL' g (Atom name ts) = i name (map (termVal g f) ts)
  evalFOL' g (Eq t1 t2) = termVal g f t1 == termVal g f t2
  evalFOL' g (Neg form) = not (evalFOL' g form)
  evalFOL' g (Impl f1 f2) = not 
                    (evalFOL' g f1 && not (evalFOL' g f2))
  evalFOL' g (Equi f1 f2) = evalFOL' g f1 == evalFOL' g f2 
  evalFOL' g (Conj fs)    = and (map (evalFOL' g) fs)
  evalFOL' g (Disj fs)    = or  (map (evalFOL' g) fs)

  evalFOL' g (Forall vs form) = 
    let 
      n = length vs
      prod = cartProd n domain
    in 
      all (\ ds -> evalFOL' (updates g (zip vs ds)) form) prod
  evalFOL' g (Exist vs form) = 
    let 
      n = length vs
      prod = cartProd n domain
    in 
      any (\ ds -> evalFOL' (updates g (zip vs ds)) form) prod

f :: Fint Int
f "z" []    = 0 
f "s" [i]   = succ i 
f "p" [i,j] = i + j
f "t" [i,j] = i * j

i :: Rint Int 
i "R" [i,j] = i < j

zero = F "z" []

frm1 = Exist ["x"] (r [zero,x])
frm2 = Exist ["x","y"] (r [x,y])
frm3 = Forall ["x"] (Exist ["y"] (r [x,y]))

evalFOL1 = evalFOL [0..] (\ v -> 0) f i frm1
evalFOL2 = evalFOL [0..] (\ v -> 0) f i frm2
evalFOL3 = evalFOL [0..] (\ v -> 0) f i frm3
evalFOL4 = evalFOL [0..] (\ v -> 0) f i (Neg frm3)

