module StrJustified
    ( module Str
    , module StrJustified
    , hPutStr
    ) where

import Prelude hiding (replicate)
import Str.String as Str
import System.IO

justifyLeft :: Int -> Chr -> Str -> Str
justifyLeft n c s
    | diff <= 0 = s
    | otherwise = s ++ Str.replicate diff c
  where
    diff = n - Str.length s

justifyRight :: Int -> Chr -> Str -> Str
justifyRight n c s
    | diff <= 0 = s
    | otherwise = Str.replicate diff c ++ s
  where
    diff = n - Str.length s

chr :: Char -> Chr
chr = id

cons' :: Chr -> String -> String
cons' a b = a : b

infixr 5 `cons'`
