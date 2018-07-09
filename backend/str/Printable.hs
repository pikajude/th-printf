module Printable where

import Prelude hiding (null)

type Str = String

cons :: Char -> Str -> Str
cons = (:)
null :: Str
null = []
