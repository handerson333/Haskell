module Basics where

-- Don't worry about this line. It's just hiding some functions that are
-- usually imported by default, but which I'm defining my own versions of
-- in this intro file.
import Prelude hiding (length,sum,product,map,foldr)


---------------------
-- Introduce Tools --
---------------------

-- * GHCi commands
--     :help, :load, :reload, :quit, :type, :info
-- * Hoogle
-- * doctest


---------------------
-- Getting Started --
---------------------

-- In GHCi:
--  * basic data types (Bool, Int, Float)
--  * numeric and boolean operators
--  * if-then-else expressions
--  * let-expressions


---------------------
-- Basic Functions --
---------------------

-- * defining and applying functions
-- * pattern matching
-- * partial application


-- | Add an integer to itself.
double :: Int -> Int
double x = x + x

-- | Is this integer zero?
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False
-- isZero x = False
-- isZero x = x==0

-- | Is this integer non-zero?
isNonZero :: Int -> Bool
isNonZero = not . isZero
-- isNonZero x = not (isZero x)
-- isNonZero 0 = False
-- isNonZero _ = True




-- | Computes the average of two floating point numbers.
avg :: Float -> Float -> Float
avg x y = (x+y)/2


-- | Using the uncurry function defined in Prelude to
--   get an uncurried function from a curried one with
--   the same functionality.
--
uncurriedAvg :: (Float, Float) -> Float
uncurriedAvg = uncurry avg

-- | Using the function curry defined in Prelude to
--   get a curried function from an uncurried onw with
--   the same functionality.
--
curriedAvg :: Float -> Float -> Float
curriedAvg = curry uncurriedAvg

-- | Uses avg to compute half of a floating point number.
half :: Float -> Float
half = avg 0
-- half x = avg x 0
-- half x = x/2



-- In GHCi:
--  * infix vs. prefix application: operators are just functions!
--    * (+) x y = x + y
--    * avg x y = x `avg` y
-- * anonymous functions

-- | Operator that computes average.
(*-*) :: Float -> Float -> Float
(*-*) = avg
-- (*-*) x y = (x+y)/2

----------------------
-- Basic Data Types --
----------------------

-- * a data type definition consists of:
--   * a new type name: Result
--   * a set of cases separated by bars, each with:
--     * a data constructor: OK, Error
--     * zero or more arguments: Int (for the case of OK) 
--                         and none for the case of Error
-- * more pattern matching
--   * top-level and case-expressions

-- | An example data type with two cases.
data Result = OK Int | Error
  deriving (Eq,Show)

-- | Instantiating type classes
--   "deriving" instatiates type classes automatically
--   but we can also do this ourselves and the way we like
--
{--
instance Show Result where
    show (OK i) = show i ++ ";-)"
    show Error  = ":-("

instance Eq Result where
    (==) (OK i) (OK j) = i==j
    (==) Error Error   = True
    (==) _ _           = False

--}


-- | Defining type synonyms by using the "type" keyword
--
-- type MyResult = Result
-- type Name = String


-- | Safely divide two integers.
safeDiv :: Int -> Int -> Result
safeDiv x 0 = Error
safeDiv x y = OK (x `div` y)

-- | Add two results.
addResults :: Result -> Result -> Result
addResults rx ry = case (rx, ry) of 
                     (OK x, OK y) -> OK (x+y)
                     (_, _)       -> Error
{-- 
addResults rx ry = case rx of
                     Error -> Error
                     (OK x) -> case ry of
                                 (OK y) -> OK (x+y)
                                 _      -> Error
--}
-- addResults (OK x) (OK y) = OK (x+y)
-- addResults _ _           = Error

-- | Get the integer from an OK result, or return 0 on an Error.
fromResult :: Result -> Int
fromResult (OK i) = i
fromResult Error  = 0



-- The definition of Bool in the Haskell Prelude looks like this:
--   
--   data Bool = False | True

-- Similar data type to Result predefined in Haskell?
--
--   data Maybe a = Just a | Nothing

type NewResult = Maybe Int

fromNewResult :: NewResult -> Int
fromNewResult (Just i) = i
fromNewResult Nothing  = 0


---------------
-- Recursion --
---------------

-- * recursive data type definitions
-- * recursive functions

-- | An example of a recursive data type.
data List = Nil
          | Cons Int List
  deriving (Eq,Show)

-- | Compute the length of a list.
listLength :: List -> Int
listLength Nil      = 0
listLength (Cons i l) = 1 + listLength l

-- | Compute the sum of the integers in a list.
listSum :: List -> Int
listSum Nil        = 0
listSum (Cons i l) = i + listSum l


-- | We can also generalize the List data type more
-- 
data GenList a = Nil'
               | Cons' a (GenList a)

-- | Reimplement listLength for the GenList data type
listLength' :: GenList a -> Int
listLength' = undefined

-- | Reimplement listSum for the GenList data type
listSum' :: GenList Int -> Int
listSum' = undefined

-- | Reimplement listSum in a more general way than listSum'
--   a must be a type that instantiates the type class Num
--   for the function listSum'' to work on it.
listSum'' :: Num a => GenList a -> a
listSum'' Nil'        = 0
listSum'' (Cons' h t) = h + listSum'' t


-- Example evaluation:
--
-- listSum (Cons 4 (Cons 5 Nil))
-- => 4 + listSum (Cons 5 Nil)
-- => 4 + (5 + listSum Nil)
-- => 4 + (5 + 0)
-- =>* 9

-------------------
-- Haskell Lists --
-------------------

-- * Haskell's built-in list and string types
--   * cons, nil, and syntactic sugar
--   * more recursive functions

-- data [a] = []         -- Nil'
--          | a : [a]    -- Cons'

-- The definition of String in the Haskell Prelude looks like this:
--
--   type String = [Char]


-- | Compute the length of a list.
length :: [a] -> Int
length []    = 0
length (_:t) = 1 + length t

-- | Compute the sum of an integer list.
sum :: [Int] -> Int
sum []    = 0
sum (h:t) = h + sum t

-- | Compute the product of the elements in a list.
product :: [Int] -> Int
product []    = 1
product (h:t) = h * product t

-- pattern!

-- | Double all the elements in an integer list.
doubleAll :: [Int] -> [Int]
doubleAll []    = []
doubleAll (h:t) = 2*h : doubleAll t

-- | Flip all of the boolean values in a boolean list.
notAll :: [Bool] -> [Bool]
notAll []    = []
notAll (h:t) = not h : notAll t


----------------------------
-- Higher-Order Functions --
----------------------------

-- * map and foldr


-- | Map a function over the elements in a list.
map :: (a -> b) -> [a] -> [b]
map f []    = []
map f (h:t) = f h : map f t

-- | Reimplement doubleAll using map.
doubleAll' :: [Int] -> [Int]
doubleAll' = map (*2)
-- doubleAll' = map (\x -> 2*x)
-- doubleAll' = map double 
-- doubleAll' xs = map double xs


-- | Reimplement notAll using map.
notAll' :: [Bool] -> [Bool]
notAll' = map not 


-- | Determine which element of the input list is even
--   Illustrates that the type of the input and output lists
--   don't have to be the same
--
isEvens :: [Int] -> [Bool]
isEvens = map (\x -> x `mod` 2 == 0)

-- | Fold a function over the elements of a list.
simpleFold :: (a -> a -> a) -> a -> [a] -> a
simpleFold f b []    = b
simpleFold f b (h:t) = f h (simpleFold f b t)

-- | Reimplement sum using foldr.
sum' :: [Int] -> Int
sum' = simpleFold (+) 0
-- sum' = simpleFold (\x y -> x+y) 0

-- | Reimplement product using foldr.
product' :: [Int] -> Int
product' = simpleFold (*) 1

-- | Fold a function over the elements in a list, allowing the type of the
--   accumulated value to differ from the elements in the list.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b []    = b
foldr f b (h:t) = f h (foldr f b t)


-- | Use foldr to count the True values in a list of Bools.
countTrues :: [Bool] -> Int
countTrues = foldr 
       (\b i -> if b==True then i+1 else i) 0


       
ex13 = ('a' == 'a')
ex14 = (16 /= 3)
ex15 = (5 > 3) && ('p' <= 'q')
ex16 = "Haskell" > "C++"

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0           = 0
  | n `mod` 17 == 2 = -43
  | otherwise       = n + 3
  
-- /show
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3 * n + 1
  
-- show
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (x:xs) = 1 + intListLength xs

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1