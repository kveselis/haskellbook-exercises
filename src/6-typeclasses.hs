module Typeclasses where


-- Eq typeclass

someEqExamples = do
  print "Examples of == and /="
  print (132 == 132)
  print ((1, 2) /= (1, 2))
  print ((1, 'a') == (1, 'b'))
  print ("dood" == "Dood")


-- Num typeclass

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


-- Ord typeclass

someOrdExamples = do
  putStrLn ("compare 8 10" ++ " : "  ++ show (compare 8 10))
  putStrLn ("compare 4 4" ++ " : " ++ show (compare 4 4))
  putStrLn ("compare \"abc\" \"cba\"" ++ " : " ++ show (compare "abc" "cba"))
  putStrLn ("compare True False" ++ " : " ++ show (compare True False))
  putStrLn ("min [2, 3, 4] [1, 2, 3]" ++ " : " ++ show (min [2, 3, 4] [1, 2, 3]))
  putStrLn ("max (3, 4) (2, 3)" ++ " : " ++ show (max (3, 4) (2, 3)))


-- Enum typeclass

someEnumExamples = do
  print $ succ 10
  print $ pred 'B'
  print $ succ 2.14159265
  print $ enumFromTo 2 11
  print $ enumFromThenTo 3 1 (-10)
  print $ enumFromThenTo 'a' 'c' 'z'


-- Show typeclass

data Mood = Blah

instance Show Mood where
  show _ = "Blah"

-- or we can derive typeclass instance anutomatically

data Mood' = Blah' deriving Show


-- Read typeclass
-- Try to avoid it!!!
-- because
r1 = read "1234" :: Integer -- Ok.
r2 = read "ABCD" :: Integer -- Compiles, but gives runtime exception!!!
-- "That exception is a runtime error and means that read is a partial
-- function, a function that doesn't return a proper value as a result for
-- all possible inputs" (Haskell Programming from first principles, 2016, p. 195)


-- Dispatch
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


