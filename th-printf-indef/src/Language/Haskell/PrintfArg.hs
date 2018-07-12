module Language.Haskell.PrintfArg where

import Parser.Types
import Str (Index)

data PrintfArg v = PrintfArg
    { flagSet :: FlagSet
    , width :: Maybe Index
    , prec :: Maybe Index
    , value :: v
    }
