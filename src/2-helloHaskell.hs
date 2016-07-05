module HelloHaskel where

{- 1. Given the following lines of code as they might appear in a
source file, how would you change them to use them directly in
the REPL?
-}

half x = x / 2
square x = x * x

-- in REPL will be:
-- let half x = x / 2
-- let square x = x * x

{- 2. Write one function that can accept one argument and work for
all the following expressions. Be sure to name the function.
3.14 * (5 * 5)
3.14 * (10 * 10)
3.14 * (2 * 2)
3.14 * (4 * 4)
-}

pix x = 3.14 * (x * x)


-- where and let
printInc n = print plusTwo
  where plusTwo = n + 2

printInc2 n = let plusTwo = n + 2
              in print plusTwo
-- using lambda
printInc2' n = (\plusTwo -> print plusTwo) (n + 2)

mult1     = x * y
  where x = 5
        y = 6

-- let x = 3; y = 1000 in x * 3 + y
-- x * 3 + y where x = 3; y = 1000
-- (\x y -> x * 3 +y) 3 1000

z = 7
x = y^2
waxOn = x * 5
y = z + 8

triple x = x * 3
waxOn' = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2

waxOff x = triple x
