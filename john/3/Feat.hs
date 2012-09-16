{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Feat where

import Test.Feat
import Test.Feat.Modifiers
import Test.Feat.Enumerate
import Data.Typeable
import Control.Applicative
import Data.Ratio ((%))

-- | Non class version of 'bounded'.
boundedBoth :: Enumerable a => Integer -> Integer -> [(Integer,[a])]
boundedBoth l h = map (samplePart l h) $ parts optimal

-- Specification: pick at most @m@ evenly distributed values from part @p@ of @e@
-- Return the list length together with the list of the selected values.
samplePart :: Index -> Index -> Finite a -> (Integer,[a])
samplePart l h (Finite crd ix) =
  let  step  =  crd % h
  in if crd <= h
       then (crd,  map ix [l..crd - 1])
       else (h,    map ix [ round (k * step)
                                    | k <- map toRational [l..h-1]])
-- The first value is at index 0 and the last value is at index ~= crd - step
-- This is "fair" if we consider using samplePart on the next part as well.
-- An alternative would be to make the last index used |crd-1|.

data Form = Prop Integer
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form
          | Equiv Form Form
          deriving (Eq,Ord,Typeable)

instance Enumerable Form
  where
    enumerate = consts $
        [ pay . pay $ Impl <$> shared <*> shared
        , pay . pay $ Equiv <$> shared <*> shared
        , pay . unary $ Cnj . nonEmpty
        , pay . unary $ Dsj . nonEmpty
        , unary Neg
        , fromParts [Finite 4 Prop]
        ]

instance Show Form where
  show (Prop x)   = show x
  show (Neg f)    = '-' : show f
  show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
  show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
  show (Impl f1 f2)  = "((" ++ show f1 ++ ") ==> ("
                           ++ show f2 ++ "))"
  show (Equiv f1 f2)  = "((" ++ show f1 ++ ") <=> ("
                           ++ show f2 ++ "))"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs


test n = take n $ boundedBoth 2 3 :: [(Integer, [Form])]
t = mapM_ print . map (head . snd) . drop 1 . test

