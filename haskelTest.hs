module Basics where

import Prelude hiding (length, sum, product,map,foldr, List)



avg :: Float -> Float -> Float
avg x y = (x+y)/2

half :: Float -> Float
half = avg 0


data Result = OK Int | Error
    deriving (Eq,Show)

type MyResult = Result
type Name = String

-- safeDiv :: Int -> Int -> Result
-- safeDiv x 0 = Error
-- safeDiv x y = OK ( x / y )


addResults :: Result -> Result -> Result
addResults (OK x) (OK y) = OK (x+y)
addResults _ _           = Error

fromResult :: Result -> Int
fromResult = undefined

data IntList = Empty | Cons Int IntList
  deriving Show
-- show
addOneToAll :: IntList -> IntList
addOneToAll Empty = Empty
addOneToAll (Cons x xs) = Cons (x + 1) (addOneToAll xs)

myIntList = Cons 2 (Cons (-3) (Cons 5 Empty))

list1 :: List Int
list1 = C 3 (C 4 (C 5(C 10 (C 0 (C 1 E)))))
list2 :: List String
list2 = C "what" (C " is " (C "happening?" E))
list3 :: List Bool
list3 = C True (C False (C True E))

-- addOneToList :: List -> List
-- addOneToList _ E = E
-- addOneToList (C x xs) = C (x+1) (addOneToList xs)


-- show
data List t = E | C t (List t)
  deriving Show
-- show
filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList a ( C x xs)
    | a x       = C x (filterList a xs)
    | otherwise = filterList a xs
  

mapList :: (a->b) -> List a -> List b
mapList a (C x xs) = C (a x) (mapList a xs)
mapList a E         = E
myList = C 2 (C (-3) (C 5 E))

double x = x * 2


emptyStringList :: [String]
emptyStringList = []

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe a
safeTail [] = Nothing
safeTail (x:xs) = if (null xs) 
                    then Just x
                    else safeTail xs 