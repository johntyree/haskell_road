module RndFOL where

import Data.List
import System.Random
import FOL
import ScanFOL
import ParseFOL
import RndTest

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

predName :: IO String
predName = do d <- getRandomInt 5
              return [toEnum (d + 80)]

fctName :: IO String 
fctName = do d <- getRandomInt 5 
             return [toEnum (d + 102)]

varName :: IO String
varName = do d <- getRandomInt 5
             return [toEnum (d + 117)]

varNames :: IO [String]
varNames = do n <- getRandomInt 2
              getNames (n+1)

getNames :: Int -> IO [String]
getNames = getNames' 0 

getNames' :: Int -> Int -> IO [String]
getNames' k n = if k == n then return []
                   else do v <- varName
                           vs <- getNames' (k+1) n
                           return (v:vs)

getRandomT :: IO Term
getRandomT = do d <- getRandomInt 1 
                getRandomTerm d

getRandomTerm :: Int -> IO Term
getRandomTerm 0 = do name <- varName 
                     return (V name)
getRandomTerm 1 = do name <- fctName 
                     n <- getRandomInt 2
                     ts <- getRandomTs n 
                     return (F name ts)

getRandomTs :: Int -> IO [Term]
getRandomTs = getRandomTs' 0

getRandomTs' :: Int -> Int ->  IO [Term]
getRandomTs' k n = if k == n then return []
                   else do t  <- getRandomT
                           ts <- getRandomTs' (k+1) n
                           return (t:ts)

getRndmF :: IO Formula
getRndmF = do d <- getRandomInt 3
              getRndmForm d

getRndmForm :: Int -> IO Formula
getRndmForm d = do n <- getRandomInt 6
                   case n of 
                     0 -> do name <- predName 
                             m <- getRandomInt 2
                             ts <- getRandomTs m
                             return (Atom name ts)
                     1 -> do t1 <- getRandomT
                             t2 <- getRandomT
                             return (Eq t1 t2)
                     2 -> do f <- getRndmForm (d-1)
                             return (Neg f)
                     3 -> do m <- getRandomInt 3 
                             fs <- getRndmFs (d-1) m
                             return (Conj fs)
                     4 -> do m <- getRandomInt 3 
                             fs <- getRndmFs (d-1) m
                             return (Disj fs)

                     5 -> do names <- varNames 
                             f <- getRndmForm (d-1)
                             return (Forall names f)
                     6 -> do names <- varNames 
                             f <- getRndmForm (d-1)
                             return (Exist names f)

getRndmFs :: Int -> Int -> IO [Formula]
getRndmFs _ 0 = return []
getRndmFs d n = do
                f <- getRndmF
                fs <- getRndmFs d (n-1)
                return (f:fs)

testParser :: Int -> IO ()
testParser = tests getRndmF 
   (\ f -> [f] == parse (show f))

