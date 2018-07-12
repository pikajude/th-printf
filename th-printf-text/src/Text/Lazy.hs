{-# LANGUAGE TypeSynonymInstances #-}

module Text.Lazy
    ( module Str.Text.Lazy
    , chr
    , showDecimal
    , elem
    ) where

import Prelude hiding (any, elem)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as B
import Data.Text.Lazy.Builder.Int
import Str.Text.Lazy

elem c = any (== c)

chr :: Char -> Chr
chr x = x

showDecimal :: (Integral a, Show a) => a -> Str
showDecimal = B.toLazyText . decimal
