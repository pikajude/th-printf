module Text.Printf.TH.Print where

import           Data.Char
import qualified Data.Text                     as S
import qualified Data.Text.Lazy                as L

import           Numeric.Extra
import           Text.Printf.TH.Builder
import qualified Text.Printf.TH.Parse.Flags    as F
import           Text.Printf.TH.Print.Utils
import           Text.Printf.TH.Print.Floating

{- HLINT ignore "Use showHex" -}
{- HLINT ignore "Use showOct" -}

printString :: Builder a => Printer String a
printString flags width prec s = justify flags width $ str val
 where
  val = case prec of
    Just n  -> take n s
    Nothing -> s

printStrictText :: Builder a => Printer S.Text a
printStrictText flags width prec s = justify flags width $ stext val where
  val = case prec of
    Just n  -> S.take n s
    Nothing -> s

printLazyText :: Builder a => Printer L.Text a
printLazyText flags width prec s = justify flags width $ ltext val where
  val = case prec of
    Just n  -> L.take (fromIntegral n) s
    Nothing -> s

printChar :: Builder a => Printer Char a
printChar flags width _ c = justify flags width $ char c

printSigned :: (Show i, Integral i, Builder a) => Printer i a
printSigned flags width prec d =
  justifySign flags width d
    . mnull prec d
    . justifyRight prec '0'
    . showIntAtBase 10 intToDigit
    $ abs d

printUnsigned flags width prec d =
  justify flags width
    . mnull prec d
    . justifyRight prec '0'
    . showIntAtBase 10 intToDigit
    $ clamp d

printShow :: (Show i, Builder a) => Printer i a
printShow f w p = printString f w p . show

printHexUpper = printHex True
printHexLower = printHex False

printHex :: (Integral i, Bounded i, Builder a) => Bool -> Printer i a
printHex upper flags width prec d =
  addPrefix
    . mnull prec d
    . justifyRight prec '0'
    . showIntAtBase 16 toDig
    $ clamp d
 where
  addPrefix = if F.justify flags == Just F.ZeroFill
    then withPrefix . justify flags (fmap (subtract prefixWidth) width)
    else justify flags width . withPrefix
  shouldPrefix = F.prefix flags && d /= 0
  withPrefix   = if shouldPrefix then (str pref <>) else id
  prefixWidth  = if shouldPrefix then 2 else 0
  pref         = if upper then "0X" else "0x"
  toDig        = if upper then intToDigitUpper else intToDigit

printOctal flags width prec_ d =
  justify flags width
    . justifyRight prec_ '0'
    . withPrefix -- prefix applied *before* filling
    . mnull prec d
    . showIntAtBase 8 intToDigit
    $ clamp d
 where
  -- don't bother trying to remember why this is needed
  prec | F.prefix flags = fmap (max 0 . subtract 1) prec_
       | otherwise      = prec_
  withPrefix = if F.prefix flags && d /= 0 then (str "0" <>) else id

clamp :: (Integral a, Bounded a) => a -> Integer
clamp d | d < 0     = toInteger d + (-2 * toInteger (minBound `asTypeOf` d))
        | otherwise = toInteger d

-- Utility function for showing integrals.
--
-- "A precision of 0 means that no character is written for the value 0."
mnull (Just 0) 0 _ = mempty
mnull _        _ y = y

printAny :: (Show a, Builder b) => Printer a b
printAny _ _ _ s = error (show s)
