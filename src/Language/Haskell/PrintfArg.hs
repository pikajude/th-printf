{-# LANGUAGE DeriveFunctor #-}

module Language.Haskell.PrintfArg where

import qualified Parser.Types as P

data PrintfArg v = PrintfArg
    { flagSet :: P.FlagSet
    , width :: Maybe Int
    , prec :: Maybe Int
    , lengthSpec :: Maybe P.LengthSpecifier
    , fieldSpec :: Char
    , value :: v
    } deriving (Show, Functor)

adjustment :: PrintfArg v -> Maybe P.Adjustment
adjustment = P.adjustment . flagSet

signed, spaced, prefixed :: PrintfArg v -> Bool
signed = P.signed . flagSet

spaced = P.spaced . flagSet

prefixed = P.prefixed . flagSet
