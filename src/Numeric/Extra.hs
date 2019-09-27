{-# LANGUAGE MagicHash #-}

module Numeric.Extra where

import           Data.Char
import           GHC.Exts
import           GHC.Base
import           Text.Printf.TH.Builder

showIntAtBase
  :: (Integral a, Show a, Builder buf) => a -> (Int -> Char) -> a -> buf
showIntAtBase base toChr n0
  | base <= 1 = error ("unsupported base " ++ show base)
  | n0 < 0    = error ("negative number " ++ show n0)
  | otherwise = showIt (quotRem n0 base) mempty
 where
  showIt (n, d) r = seq c $ case n of
    0 -> r'
    _ -> showIt (quotRem n base) r'
   where
    c  = toChr (fromIntegral d)
    r' = cons c r

intToDigitUpper (I# i)
  | isTrue# (i >=# 0#) && isTrue# (i <=# 9#) = unsafeChr (ord '0' + I# i)
  | isTrue# (i >=# 10#) && isTrue# (i <=# 15#) = unsafeChr (ord 'A' + I# i - 10)
  | otherwise = errorWithoutStackTrace
    ("Numeric.Extra.intToDigitUpper: not a digit " ++ show (I# i))
