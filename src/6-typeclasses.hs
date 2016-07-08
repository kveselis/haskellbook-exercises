module Typeclasses where

import Data.List (sort)


-- * Eq typeclass

someEqExamples = do
  print "Examples of == and /="
  print (132 == 132)
  print ((1, 2) /= (1, 2))
  print ((1, 'a') == (1, 'b'))
  print ("dood" == "Dood")


-- * Num typeclass

-- divideThenAdd :: Num a => a -> a -> a won't work because of
-- (/) usage which requires Fractional typeclass. Num is more general and
-- does not provide (/)
divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y) + 1
-- but if we use (-) instead of (/) we can use Num typeclass which provides (+) and (-)
substractThenAdd :: Num a => a -> a -> a
substractThenAdd x y = (x - y) + 1

numId = id :: Num a => a -> a
intId = numId :: Integer -> Integer
-- by we cant go back
-- altNumId = intId :: Num a => a -> a --compile error


-- * Ord typeclass

someOrdExamples = do
  putStrLn ("compare 8 10" ++ " : "  ++ show (compare 8 10))
  putStrLn ("compare 4 4" ++ " : " ++ show (compare 4 4))
  putStrLn ("compare \"abc\" \"cba\"" ++ " : " ++ show (compare "abc" "cba"))
  putStrLn ("compare True False" ++ " : " ++ show (compare True False))
  putStrLn ("min [2, 3, 4] [1, 2, 3]" ++ " : " ++ show (min [2, 3, 4] [1, 2, 3]))
  putStrLn ("max (3, 4) (2, 3)" ++ " : " ++ show (max (3, 4) (2, 3)))


-- * Enum typeclass

someEnumExamples = do
  print $ succ 10
  print $ pred 'B'
  print $ succ 2.14159265
  print $ enumFromTo 2 11
  print $ enumFromThenTo 3 1 (-10)
  print $ enumFromThenTo 'a' 'c' 'z'


-- * Show typeclass

data Mood = Blah

instance Show Mood where
  show _ = "Blah"

-- or we can derive typeclass instance anutomatically

data Mood' = Blah' deriving Show


-- * Read typeclass

-- Try to avoid it!!!
-- because
r1 = read "1234" :: Integer -- Ok.
r2 = read "ABCD" :: Integer -- Compiles, but gives runtime exception!!!
-- "That exception is a runtime error and means that read is a partial
-- function, a function that doesn't return a proper value as a result for
-- all possible inputs" (Haskell Programming from first principles, 2016, p. 195)


-- * Dispatch

-- Typeclasses are dispatched by type.
class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65

newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 1988

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where integerOfA      = toNumber a
        integerOfAPrime = toNumber a'
        summed = integerOfA + integerOfAPrime

-- sumNumberish (Age 10) (Age 10)


-- * Writing typeclass instances

-- ** Eq instance

--data Trivial = Trivial
data Trivial = Trivial'

instance Eq Trivial where
--  Trivial' == Trivial' = True
-- or
  (==) Trivial' Trivial' = True


data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving (Show)

data Date = Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon   = True
  (==) Tue Tue   = True
  (==) Weds Weds = True
  (==) Fri Fri   = True
  (==) Sat Sat   = True
  (==) Sun Sun   = True
  (==) _ _       = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'


-- Some exercises

-- 1.
data TisAnInteger = TisAnInteger Integer
instance Eq TisAnInteger where
  (==) (TisAnInteger int) (TisAnInteger int') = int == int'
-- 2.
data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two i1 i2) (Two j1 j2) =
    i1 == j1 && i2 == j2
-- 3.
data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt i1) (TisAnInt i2) = i1 == i2
  (==) (TisAString s1) (TisAString s2) = s1 == s2
  (==) (TisAString s) (TisAnInt i) = s == show i
  (==) (TisAnInt i) (TisAString s) = s == show i
-- 4.
data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
  (==) (Pair e1 e2) (Pair f1 f2) =
    e1 == f1 && e2 == f2
-- 5.
data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple e1 e2) (Tuple f1 f2) =
    e1 == f1 && e2 == f2
-- 6.
data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
  (==) (ThisOne v1) (ThisOne v2) = v1 == v2
  (==) (ThatOne v1) (ThatOne v2) = v1 == v2
  (==) (ThatOne v1) (ThisOne v2) = v1 == v2
  (==) (ThisOne v1) (ThatOne v2) = v1 == v2
-- 7.
data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a1) (Hello a2) = a1 == a2
  (==) (Goodbye b1) (Goodbye b2) = b1 == b2
  (==) _ _ = False


-- Ord

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _   = GT
  compare _ Fri   = LT
  compare _ _     = EQ


-- * More operations

add :: Num a => a -> a -> a
add x y = x + y

addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y =
  if x > 1 then x + y else x

-- check :: a -> a -> Bool --won't compile
-- check x y = x == y

check' :: Eq a => a -> a -> Bool
check' x y = x == y

-- this is also valid because that Ord implies Eq
-- (we can say that Eq is a superclass of Ord)
-- Normally we would use Eq as it sets less constrains on the function
check'' :: Ord a => a -> a -> Bool
check'' x y = x == y



-- * Exercises for Chapter 6

-- 1. c)
-- 2. d)
-- 3. a)
-- 4. c)
-- 5. b)

-- Does it typecheck?
-- 1. No. No implementation of the typeclass Show. Implement or derive
-- Fixed
data Person = Person Bool deriving (Show)
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2. No. Mood2 isn't the instance of Eq
-- Fixed by adding Eq
data Mood2 = Blah2 | Woot2 deriving (Show, Eq)
settleDown x = if x == Woot2
then Blah2
else x

-- 3.
-- a) Blah2 or Woot2
-- b) Mood2 has to be an instance of Num in order to accept numerical values
-- c) Ord is not implemented.

-- 4. yes.
type Subject = String
type Verb = String
type Object = String
data Sentence = Sentence Subject Verb Object deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- Given a datatype declaration, what can we do?
data Rocks = Rocks String deriving (Eq, Show, Ord)
data Yeah = Yeah Bool deriving (Eq, Show, Ord)
data Papu = Papu Rocks Yeah deriving (Eq, Show, Ord)
-- 1.
phew = Papu (Rocks "chases") (Yeah True)
-- 2.
truth = Papu (Rocks "chomskydoz") (Yeah True)
-- 3.
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'
-- 4.
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'


-- * Match the types

-- 1.
i :: Num a => a
i = 1

-- 2.
f :: Float
f = 1.0

-- 3.
j :: Fractional a => a
j = 1.0

-- 4.
k :: RealFrac a => a
k = 1.0

-- 5.
freud :: Ord a => a -> a
freud x = x

-- 6.
freud' :: Int -> Int
freud' x = x

-- 7.
myX = 1 :: Int
sigmund :: Int -> Int
sigmund x = myX

-- 8.
--sigmund' :: Num a => a -> a -- erron on this
sigmund' x = myX

-- 9.
-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10.
-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11.
mySort :: [Char] -> [Char]
mySort = sort
signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a -- error on this. 
signifier xs = head (mySort xs)


-- * Type-Kwon-Do

-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b

-- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = 10 * (f a)

