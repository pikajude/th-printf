module Text.Strict
    ( module Str.Text
    , chr
    , elem
    ) where

import Prelude hiding (any, elem)
import Str.Text

elem :: Chr -> Str -> Bool
elem c = any (== c)

chr :: Char -> Chr
chr = id
