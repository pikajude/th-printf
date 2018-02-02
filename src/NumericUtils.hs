{-# Language MagicHash #-}
module NumericUtils (module NumericUtils, intToDigit) where

import Data.Char
import GHC.Base
import Text.Printf.TH.Printer

intToDigitUpper :: Int -> Char
intToDigitUpper (I# i)
    | isTrue# (i >=# 0#)  && isTrue# (i <=#  9#) = unsafeChr (ord '0' + I# i)
    | isTrue# (i >=# 10#) && isTrue# (i <=# 15#) = unsafeChr (ord 'A' + I# i - 10)
    | otherwise =  error ("Char.intToDigit: not a digit " ++ show (I# i))

showIntAtBase :: (Integral i, Monoid b, Printer b) => i -> (Int -> Char) -> i -> b
showIntAtBase base toB n0 = showIt (quotRem n0 base) mempty
  where
    showIt (n, d) r =
        case n of
            0 -> r'
            _ -> showIt (quotRem n base) r'
      where
        r' = cons (toB $ fromIntegral d) r
