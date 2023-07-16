{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.Printf.Geometry (
  sign',
  padDecimal,
  prefix,
  fromPrintfArg,
  formatOne,
  Value (..),
) where

import Control.Monad
import Data.Maybe
import Language.Haskell.PrintfArg
import Parser.Types (Adjustment (..))

import Buf
import StrUtils

data Value buf = Value
  { valArg :: PrintfArg buf
  , valPrefix :: Maybe buf
  , valSign :: Maybe buf
  }
  deriving (Show)

sign' :: (Num n, Ord n, Buf buf) => PrintfArg n -> Maybe buf
sign' pf
  | value pf < 0 = Just (singleton '-')
  | spaced pf = Just (singleton ' ')
  | signed pf = Just (singleton '+')
  | otherwise = Nothing

padDecimal :: (Buf buf, Eq v, Num v) => PrintfArg v -> buf -> buf
padDecimal spec
  | prec spec == Just 0 && value spec == 0 = const mempty
  | otherwise = maybe id (`justifyRight` '0') (prec spec)

prefix :: (Num n, Eq n, Buf buf) => buf -> PrintfArg n -> Maybe buf
prefix s pf = guard (prefixed pf && value pf /= 0) >> Just s

fromPrintfArg ::
  (Buf buf) =>
  (n -> buf) ->
  (PrintfArg n -> Maybe buf) ->
  (PrintfArg n -> Maybe buf) ->
  PrintfArg n ->
  Value buf
fromPrintfArg f b c a = Value (f <$> a) (b a) (c a)

formatOne :: (Buf buf) => Value buf -> buf
formatOne Value{..}
  | Nothing <- width valArg = prefix' <> text
  | Just w <- width valArg = case adjustment valArg of
    Just ZeroPadded
      | isn'tDecimal || isNothing (prec valArg) ->
        prefix' <> justifyRight (w - size prefix') '0' text
    Just LeftJustified -> justifyLeft w ' ' (prefix' <> text)
    _ -> justify' w (prefix' <> text)
 where
  isn'tDecimal = fieldSpec valArg `notElem` ("diouxX" :: String)
  justify' n
    | n < 0 = justifyLeft (abs n) ' '
    | otherwise = justifyRight n ' '
  prefix' = fromMaybe mempty valSign <> fromMaybe mempty valPrefix
  text = value valArg
