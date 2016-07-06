module Types where

-- Intermission: Exercises

-- 1.
-- a) not :: Bool -> Bool
-- b) length :: [a] -> Int
-- c) concat :: [[a]] -> [a]
-- d) head :: [a] -> a
-- e) (<) :: Ord a => a -> a -> Bool

-- 2.
-- a) - d)
-- b) - c)
-- c) - a)
-- d) - b)
-- e) - e)

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

typicalCurriedFun :: Integer -> Bool -> Integer
typicalCurriedFun i b = i + (nonsense b)

uncurriedFun :: (Integer, Bool) -> Integer
uncurriedFun (i, b) = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonymousAndManualyNested :: Integer -> Bool -> Integer
anonymousAndManualyNested = \i -> \b -> i + (nonsense b)


curry' f a b = f (a, b)
-- curry' fst 1 3
uncurry' f (a, b) = f a b
-- uncurry' (+) (1, 3)

-- Intermission: Exercises

-- 1. - a)
-- 2. - d)
-- 3. - d) ?
-- 4. - c) ?
-- 5. - a)
-- 6. - e)
-- 7. - f) ?
-- 8. - d) ?
-- 9. - c)


-- type declaration
triple :: Integer -> Integer
triple x = x * 3
-- uncommon usage of type declaration
triple' x = tripleItYo x
  where tripleItYo :: Integer -> Integer
        tripleItYo y = y * 3



-- Chapter Exercises

-- Multiple choice
-- 1. c)
-- 2. a)
-- 3. b)
-- 4. c)

-- Determine the type
-- 1. a) Num a => a
--    b) Num a => (a, [Char])
--    c) (Integer, [Char])
--    d) Bool
--    e) Int
--    f) Bool
-- 2. w :: Num a => a
-- 3. z :: Num a => a -> a
-- 4. f :: Fractional a => a
-- 5. f :: [Char]

-- Does it compile?
-- 1.
bigNum = (^) 5
wahoo = bigNum 10

-- 2.
x = print
y = print "wooohoo!"
z = x "hello world"

-- 3.
a = (+)
b = 5
c = a 10
d = c 200

-- 4.
a' = 12 + b'
c' = 2
b' = 10000 * c'

-- Type variable or specific type constructor
-- 1. example
-- 2. fully poly -> concrete -> concrete
-- 3. fully poly -> constrained poly -> fully poly
-- 4. fully poly -> fully poly -> concrete

-- Write a type signature
-- 1.
functionH :: [a] -> a
functionH (x:_) = x

-- 2.
functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

-- 3.
functionS :: (a, b) -> b
functionS (x, y) = y


-- Given a type, write the function
-- 1.
i :: a -> a
i x = x

-- 2.
c1 :: a -> b -> a
c1 x y = x

-- 3.
c2 :: b -> a -> b
c2 x y = x

-- 4.
c3 :: a -> b -> b
c3 x y = y

-- 5.
r :: [a] -> [a]
r x = reverse x

-- 6.
co :: (b -> c) -> (a -> b) -> (a -> c)
-- takes (b -> c) and (a -> b) and returns (a -> c)

-- so, we can say f :: b -> c and g :: a -> b
-- we can use anonymouse function of type 'a' to provide type 'a' to the function 'g'
-- co f g = \a -> g a --compile error

-- it's not enough since function 'g' returns type 'b'.
-- But function 'f' takes 'b' and returns type 'c' and that is what we need.
-- co f g = \a -> f (g a)

-- we can move 'a' to the left side, because
-- (a -> c) is the same as a -> c
-- co f g a = f (g a)

-- we can use composition operator (.)
-- co f g a = (f . g) a

-- now 'a' is redundant so we can remove it
-- co f g = f . g

-- we can pull '.' operator in front using it's function form (.)
-- co f g = (.) f g

-- now 'f' and 'g' are redundand, so we can remove them.
-- so this type signature actualy is a functional composition
co = (.)

-- 7.
a1 :: (a -> c) -> a -> a
a1 f x = x

-- 8.
a2 :: (a -> b) -> a -> b
a2 f x = f x


-- Type-Kwon-Do

data Woot
data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g (x, y) = (x, f y)


-- 1.
ff :: Int -> String
ff = undefined

gg :: String -> Char
gg = undefined

hh :: Int -> Char
hh x = gg (ff x)

-- 2.
data A
data B
data C

qq :: A -> B
qq = undefined

ww :: B -> C
ww = undefined

ee :: A -> C
ee x = ww (qq x)

-- 3.
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xfrom :: (X, Y) -> (Z, Z)
xfrom (x, y) = (xz x, yz y)

-- 4.
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g x = fst (g (f x))
