module BonusExerc1

where 

data Creature = Lady | Tiger 
     deriving (Eq,Show)

sign1, sign2 :: (Creature,Creature) -> Bool
sign1 (x,y) = x == Lady && y == Tiger
sign2 (x,y) = x /= y

solution1 :: [(Creature,Creature)]
solution1 = 
 [ (x,y) | x <- [Lady,Tiger], 
           y <- [Lady,Tiger], 
           sign1 (x,y) /= sign2 (x,y) ]

data Islander = Knight | Knave deriving (Eq,Show)

john :: (Islander,Islander) -> Bool
john (x,y) = (x,y) == (Knave,Knave)

solution3 :: [(Islander,Islander)]
solution3 = [(x,y) | x <- [Knight,Knave], 
                     y <- [Knight,Knave], 
                     john (x,y) == (x == Knight) ]

main = putStrLn (s ++ show s) 
  where s = "main = putStrLn (s ++ show s) \n  where s = "

