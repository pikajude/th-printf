module Text.Strict
    ( module Str.Text
    , chr
    , showDecimal
    , elem
    ) where

import qualified Data.Text.Lazy as T
import Prelude hiding (any, elem)
import Str.Text
import qualified Text.Lazy

elem c = any (== c)

chr :: Char -> Chr
chr = id

showDecimal :: (Integral a, Show a) => a -> Str
showDecimal = T.toStrict . Text.Lazy.showDecimal
