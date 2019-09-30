module Text.Printf.TH.Layouters.Float where

import           Data.Maybe
import           Data.Char                      ( intToDigit )
import           Numeric                        ( floatToDigits )
import           GHC.Float                      ( roundTo )
import           Prelude                 hiding ( exponent )
import           Data.Bits

import           Numeric.Extra
import qualified Text.Printf.TH.Parse.Flags    as F
import           Text.Printf.TH.Builder
import           Text.Printf.TH.Layout

type Layouter a buf = Maybe Int -> Maybe Int -> F.Flags -> a -> buf

fixed width prec_ flags f' =
  layout width (signedPrefix flags f') (F.justify flags)
    $ case splitAt exp1 digs1 of
        ([], []) -> str "0." <> times prec '0'
        (xs, []) -> build intToDigit xs
        ([], xs) -> str "0." <> build intToDigit xs
        (xs, ys) -> build intToDigit xs <> char '.' <> build intToDigit ys
 where
  f             = abs f'
  (digs0, exp0) = floatToDigits 10 f
  (digs1, exp1)
    | roundSize < 0
    = ([], exp0)
    | otherwise
    = let (overflow, digs2) = roundTo 10 roundSize digs0
      in  (digs2, exp0 + overflow)
  prec      = fromMaybe 6 prec_
  roundSize = exp0 + prec

sci width prec_ flags f' =
  layout width (signedPrefix flags f') (F.justify flags)
    $  char (intToDigit whole)
    <> char '.'
    <> build intToDigit part
    <> char 'e'
    <> char (if exp1 >= 0 then '+' else '-')
    <> justifyRight (Just 2) '0' (showIntAtBase 10 intToDigit (abs exp1))
 where
  f                 = abs f'
  (digs0   , exp0 ) = floatToDigits 10 f
  (overflow, digs1) = roundTo 10 (prec + 1) digs0
  exp1              = exp0 - 1 + overflow
  (whole : part)    = digs1
  prec              = fromMaybe 6 prec_

generic _width prec _flags f =
  build intToDigit whole <> char '.' <> build intToDigit part where
  (digs , e      ) = floatToDigits 10 f
  (whole, part   ) = splitAt e sigfigs
  (_    , sigfigs) = case prec of
    Just p | p < length digs -> roundTo 10 p digs
    _                        -> (0, digs)

hexUpper = hex True
hexLower = hex False

hex upper width prec flags f =
  layout width (Just $ str (if upper then "0X" else "0x")) (F.justify flags)
    $  char (intToDigit d1)
    <> (if not (null rounded) || F.prefix flags then char '.' else mempty)
    <> build (if upper then intToDigitUpper else intToDigit) rounded
    <> char (if upper then 'P' else 'p')
    <> char (if exp1 >= 0 then '+' else '-')
    <> showIntAtBase 10 intToDigit (abs exp1)
 where
  (d0 : digs0, exp0)   = floatToDigits 2 f
  d1                   = d0 + overflow
  rounded              = drop overflow rounded'
  (overflow, rounded') = case prec of
    Just n  -> roundTo 16 n hdigs0
    Nothing -> (0, hdigs0)
  exp1   = exp0 - 1
  hdigs0 = asHex digs0
  asHex (a : b : c : d : xs) =
    (a <<< 3 .|. b <<< 2 .|. c <<< 1 .|. d) : asHex xs
  asHex [a, b, c] = [a <<< 3 .|. b <<< 2 .|. c <<< 1]
  asHex [a, b]    = [a <<< 3 .|. b <<< 2]
  asHex [a]       = [a <<< 3]
  asHex []        = []

(<<<) = shiftL
infixl 8 <<<
