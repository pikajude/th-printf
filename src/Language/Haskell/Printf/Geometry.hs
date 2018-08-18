{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Printf.Geometry where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Maybe
import Data.Monoid (mempty)
import Data.Semigroup ((<>))
import Language.Haskell.PrintfArg
import Parser.Types (Adjustment(..))
import StrUtils

data Value = Value
    { valArg :: PrintfArg String
    , valPrefix :: Maybe String
    , valSign :: Maybe String
    } deriving (Show)

sign' :: (Num n, Ord n) => PrintfArg n -> Maybe String
sign' pf
    | value pf < 0 = Just "-"
    | spaced pf = Just " "
    | signed pf = Just "+"
    | otherwise = Nothing

padDecimal :: (Eq v, Num v) => PrintfArg v -> String -> String
padDecimal spec
    | prec spec == Just 0 && value spec == 0 = const ""
    | otherwise = maybe id (`justifyRight` '0') (prec spec)

prefix :: (Num n, Eq n) => String -> PrintfArg n -> Maybe String
prefix s pf = guard (prefixed pf && value pf /= 0) >> Just s

fromPrintfArg ::
       (n -> String)
    -> (PrintfArg n -> Maybe String)
    -> (PrintfArg n -> Maybe String)
    -> PrintfArg n
    -> Value
fromPrintfArg f b c a = Value (f <$> a) (b a) (c a)

formatOne :: Value -> String
formatOne Value {..}
    | Nothing <- width valArg = prefix' <> text
    | Just w <- width valArg =
        case adjustment valArg of
            Just ZeroPadded
                | isn'tDecimal || isNothing (prec valArg) ->
                    prefix' <> justifyRight (w - length prefix') ('0') text
            Just LeftJustified -> justifyLeft w ' ' (prefix' <> text)
            _ -> justify' w (prefix' <> text)
    | otherwise = error "unreachable"
  where
    isn'tDecimal = fieldSpec valArg `notElem` ("diouxX" :: String)
    justify' n
        | n < 0 = justifyLeft (abs n) ' '
        | otherwise = justifyRight n ' '
    prefix' = fromMaybe mempty valSign <> fromMaybe mempty valPrefix
    text = value valArg
