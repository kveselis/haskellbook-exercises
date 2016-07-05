module BasicDataTypes where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah



greetIfCool :: String -> IO()
greetIfCool collness = if cool collness
                       then putStrLn "eyyyy. What's shakin'?"
                       else putStrLn "pshhhh."
  where
    cool x = x == "downright frosty yo"

awsome = ["Papucon", "curry", ":)"]
alsoAwsome = ["Quake", "The Simson"]
allAwsome = [awsome, alsoAwsome]

-- 1.
-- length :: [a] -> Int

-- 2.
-- a) 5
-- b) 3
-- c) 2
-- d) 5

-- 3.
-- 6 / 3 => 2.0
-- 6 / length [1, 2, 3] => gives an error

-- 4.
-- 6 `div` length [1, 2, 3]
-- or
-- div 6 length [1, 2, 3]

-- 5.
-- 2 + 3 == 5
-- type Bool, result True

-- 6.
-- let x = 5
-- x + 3 == 5
-- type Bool, retult False

-- 7.
-- length allAwsome == 2 => True
-- length [1, 'a', 3, 'b'] => Error (list element not of the same type)
-- length allAwsome + lenght awsome => 2 + 3 => 5
-- (8 == 8) && ('b' < 'a') => False
-- (8 == 8) && 9 => Error

-- 8.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9.
myAbs :: Integer -> Integer
myAbs x =
  if x >= 0
  then x
  else x * (-1)

-- 10.
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
