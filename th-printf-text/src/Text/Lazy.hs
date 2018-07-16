{-# LANGUAGE TypeSynonymInstances #-}

module Text.Lazy
    ( module Str.Text.Lazy
    , chr
    , elem
    , hPutStr
    ) where

import Prelude hiding (any, elem)
import Str.Text.Lazy
import Data.Text.Lazy.IO

elem :: Chr -> Str -> Bool
elem c = any (== c)

chr :: Char -> Chr
chr x = x
