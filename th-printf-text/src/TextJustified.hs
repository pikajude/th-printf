{-# LANGUAGE TypeSynonymInstances #-}

module TextJustified where

import Data.Data
import Data.Int (Int64)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as B
import Data.Text.Lazy.Builder.Int
import Prelude hiding (length)

type Chr = Char

type Index = Int64

type Output = T.Text

chr :: Char -> Chr
chr x = x

type Str = B.Builder

instance Data Str

showDecimal :: (Integral a, Show a) => a -> Str
showDecimal = decimal

length = T.length . B.toLazyText

head = T.head . B.toLazyText

empty :: Str
empty = mempty

justifyLeft :: Index -> Chr -> Str -> Str
justifyLeft n c s
    | diff <= 0 = s
    | otherwise = s <> B.fromLazyText (T.replicate diff (T.singleton ' '))
  where
    diff = fromIntegral n - length s

justifyRight :: Index -> Chr -> Str -> Str
justifyRight n c s
    | diff <= 0 = s
    | otherwise = B.fromLazyText (T.replicate diff (T.singleton c)) <> s
  where
    diff = fromIntegral n - length s

fromOutput = B.fromLazyText

finalize = B.toLazyText
