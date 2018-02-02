{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language FlexibleContexts #-}
{-# Language RecordWildCards #-}
{-# Language TypeSynonymInstances #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleInstances #-}

module Text.Printf.TH.Printer.String where

import Data.Char (toUpper)
import Numeric hiding (showIntAtBase)
import NumericUtils
import Text.Printf.TH.Printer

instance Printer String where
    type Output String = String
    literal = id
    formatChar' = pure
    cons = (:)
    output _ = id
    rjust c n s
        | diff <= 0 = s
        | otherwise = replicate diff c ++ s
      where
        diff = fromIntegral n - length s
    ljust n s
        | diff <= 0 = s
        | otherwise = s ++ replicate diff ' '
      where
        diff = fromIntegral n - length s
    formatDec' = showIntAtBase 10 intToDigit
    formatOct' = showIntAtBase 8 intToDigit
    formatHex' = showIntAtBase 16 intToDigit
    formatHexUpper' = showIntAtBase 16 intToDigitUpper
    formatFloat' p =
        ( \n -> showFFloat (fromIntegral <$> p) n ""
        , \n -> showFFloatAlt (fromIntegral <$> p) n "")
    formatSci' p n = showEFloat (fromIntegral <$> p) n ""
    formatSciUpper' p = map toUpper . formatSci' p
    formatG' p =
        ( \n -> showGFloat (fromIntegral <$> p) n ""
        , \n -> showGFloatAlt (fromIntegral <$> p) n "")
    formatGUpper' p = both (map toUpper .) (formatG' p)
      where
        both f (x, y) = (f x, f y)
