module Text.Strict
    ( module Str.Text
    , chr
    , elem
    , hPutStr
    ) where

import Prelude hiding (any, elem)
import Str.Text
import Data.Text.IO

elem :: Chr -> Str -> Bool
elem c = any (== c)

chr :: Char -> Chr
chr = id
