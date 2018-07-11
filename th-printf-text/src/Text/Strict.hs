module Text.Strict
    ( module Str.Text
    , chr
    , showDecimal
    ) where

import qualified Data.Text.Lazy as T
import Str.Text
import qualified Text.Lazy

chr :: Char -> Chr
chr = id

showDecimal :: (Integral a, Show a) => a -> Str
showDecimal = T.toStrict . Text.Lazy.showDecimal
