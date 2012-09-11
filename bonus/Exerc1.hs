
module Exerc1

where

import BonusExerc1

every = flip all
some  = flip any

sign3, sign4 :: (Creature, Creature) -> Bool
sign3 (door1, door2) = some [door1, door2] (== Lady)
sign4 (door1, door2) = door1 == Tiger

solution2 = [(d1, d2) | d1 <- [Lady, Tiger]
                      , d2 <- [Lady, Tiger]
                      , sign3 (d1, d2) == sign4 (d1, d2)
            ]

john' :: (Islander, Islander) -> Bool

john' (Knave, Knave) = True
john' _              = False


solution3' = [(j, b) | j <- [Knight, Knave]
                     , b <- [Knight, Knave]
                     , case j of
                           Knight -> john' (j, b)
                           Knave  -> not $ john' (j, b)
             ]

bill'', john'' :: (Islander, Islander) -> Bool
john'' (j,b) = j == b
bill'' (j,b) = j /= b

solution4 = [(j, b) | j <- [Knight, Knave]
                    , b <- [Knight, Knave]
                    , john'' (j,b) == (j == Knight)
                    , bill'' (j,b) == (b == Knight)
            ]

-- Example puzzle
-- Door 1 says opening this door is better than opening that door.
-- Door 2 says "Door 1 is a liar. And has the tiger."

s1 (d1, d2) = d1 == Lady
s2 (d1, d2) = not (s1 (d1, d2)) && d1 == Tiger

solution5 = [(d1, d2) | d1 <- [Lady, Tiger]
                      , d2 <- [Lady, Tiger]
                      , s1 (d1,d2) || s2 (d1,d2)
                      , d1 /= d2
            ]
