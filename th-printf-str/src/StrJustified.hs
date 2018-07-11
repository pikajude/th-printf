module StrJustified
    ( module Str
    , module StrJustified
    ) where

import Data.Char (intToDigit)
import Numeric (showIntAtBase)
import Prelude hiding (replicate)
import Str.String as Str

justifyLeft :: Int -> Chr -> Str -> Str
justifyLeft n c s
    | diff <= 0 = s
    | otherwise = s ++ Str.replicate diff ' '
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

showDecimal :: (Integral a, Show a) => a -> Str
showDecimal = show

type Output = String

fromOutput :: Output -> Str
fromOutput = id

finalize :: Str -> Output
finalize = id
