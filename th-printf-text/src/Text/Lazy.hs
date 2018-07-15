{-# LANGUAGE TypeSynonymInstances #-}

module Text.Lazy
    ( module Str.Text.Lazy
    , chr
    , elem
    ) where

import Prelude hiding (any, elem)
import Str.Text.Lazy

elem :: Chr -> Str -> Bool
elem c = any (== c)

chr :: Char -> Chr
chr x = x
