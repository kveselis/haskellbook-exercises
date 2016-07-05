module Strings where
import Data.List (findIndex)

myGreeting :: String
myGreeting = "hello" ++ " world!"
myGreeting' = (++) "hello" " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  putStrLn myGreeting'
  putStrLn secondGreeting'
    where secondGreeting = concat [hello, " ", world]
          secondGreeting' = (++) hello ((++) " " world)

area d = pi * (r * r)
  where r = d / 2

-- 1. ++ [1, 2, 3] [4, 5, 6] => (++) [1, 2, 3] [4, 5, 6]
-- 2. '<3' ++ ' Haskell' => "<3" ++ " Haskell"
-- 3. concat ["<3", " Haskell"] => correct

-- 1.
addExclamation x = x ++ "!"

-- 2.
get4th x = x !! 4

-- 3.
drop9 x = drop 9 x

thirdLetter :: String -> Char
thirdLetter x = x !! 2

-- 4.
letterIndex :: Int -> Maybe Char
letterIndex x | x <= 0   = Nothing
              | x > 0    = Just ("Curry is awsome!" !! (x - 1))


-- 5.
rvrs :: String -> String
rvrs x = awsome ++ iss ++ curry
  where awsome = drop 9 x
        iss    = take 4 (drop 5 x)
        curry  = take 5 x
-- usage: rvrs "Curry is awsome"
main2 :: IO()
main2 = print $ rvrs "Curry is awsome"

