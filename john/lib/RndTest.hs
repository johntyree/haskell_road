module RndTest where 

import System.Random

tests :: Show a => IO a -> (a -> Bool) -> Int -> IO ()
tests generator p n = tests' generator p n 0

tests' :: Show a => IO a -> (a -> Bool) -> Int -> Int -> IO ()
tests' generator p n k =
  if n == k then print (show n ++ " tests passed")
  else do x <- generator
          if not (p x) then print ("failed test on: " ++ show x)
             else do print ("pass on:" ++ show x)
                     tests' generator p n (k+1)

