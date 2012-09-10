
module Exerc1

where

data Creature = Lady | Tiger
     deriving (Eq,Show)

every = flip all
some  = flip any

sign1 door1 door2 = door1 == Lady || door2 == Lady
sign2 door1 door2 = door1 == Tiger

solution2 = [(d1, d2) | d1 <- [Lady, Tiger]
                      , d2 <- [Lady, Tiger]
                      , sign1 d1 d2 == sign2 d1 d2
            ]
