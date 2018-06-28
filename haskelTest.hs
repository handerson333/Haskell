module Basics where

import Prelude hiding (length, sum, product,map,foldr)



avg :: Float -> Float -> Float
avg x y = (x+y)/2

half :: Float -> Float
half = avg 0


data Result = OK Int | Error
    deriving (Eq,Show)

type MyResult = Result
type Name = String

safeDiv :: Int -> Int -> Result
safeDiv x 0 = Error
safeDiv x y = OK(x/y)


addResults :: Result -> Result -> Result
addResults (OK x) (OK y) = OK (x+y)
addResults _ _           = Error

fromResult :: Result -> Int
fromResult = undefined