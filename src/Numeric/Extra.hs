module Numeric.Extra where

import           Language.Haskell.Printf.Builder

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
