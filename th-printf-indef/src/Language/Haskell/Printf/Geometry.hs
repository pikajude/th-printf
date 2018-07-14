{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Printf.Geometry where

import Control.Monad
import Control.Monad.Fix
import Data.Maybe
import Data.String (IsString(..))
import Debug.Trace
import Language.Haskell.PrintfArg
import Parser.Types (Adjustment(..))
import Prelude hiding ((<>))
import qualified Str as S

data Value = Value
    { valArg :: PrintfArg S.Str
    , valPrefix :: Maybe S.Str
    , valSign :: Maybe S.Str
    } deriving (Show)

sign' :: (Num n, Ord n) => PrintfArg n -> Maybe S.Str
sign' pf
    | value pf < 0 = Just "-"
    | spaced pf = Just " "
    | signed pf = Just "+"
    | otherwise = Nothing

padDecimal spec
    | prec spec == Just 0 && value spec == 0 = const ""
    | otherwise = maybe id (`S.justifyRight` S.chr '0') (prec spec)

prefix :: (Num n, Eq n) => S.Str -> PrintfArg n -> Maybe S.Str
prefix s pf = guard (prefixed pf && value pf /= 0) >> Just s

fromPrintfArg ::
       (n -> S.Str)
    -> (PrintfArg n -> Maybe S.Str)
    -> (PrintfArg n -> Maybe S.Str)
    -> PrintfArg n
    -> Value
fromPrintfArg f b c a = Value (f <$> a) (b a) (c a)

formatOne Value {..}
    | Nothing <- width valArg = prefix' <> text
    | Just w <- width valArg =
        case adjustment valArg of
            Just ZeroPadded
                | isn'tDecimal || isNothing (prec valArg) ->
                    prefix' <> S.justifyRight (w - S.length prefix') (S.chr '0') text
            Just LeftJustified -> S.justifyLeft w (S.chr ' ') (prefix' <> text)
            _ -> justify' w (prefix' <> text)
  where
    isn'tDecimal = fieldSpec valArg `notElem` ("diouxX" :: String)
    justify' n
        | n < 0 = S.justifyLeft (abs n) (S.chr ' ')
        | otherwise = S.justifyRight n (S.chr ' ')
    prefix' = fromMaybe mempty valSign <> fromMaybe mempty valPrefix
    text = value valArg

(<>) = mappend
