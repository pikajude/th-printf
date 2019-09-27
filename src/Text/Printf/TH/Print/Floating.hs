module Text.Printf.TH.Print.Floating where

import           Data.Char
import           Data.Maybe                     ( fromMaybe )
import           Numeric                        ( floatToDigits )
import           GHC.Float                      ( roundTo )

import qualified Text.Printf.TH.Parse.Flags    as F
import           Text.Printf.TH.Builder
import           Text.Printf.TH.Print.Utils
import           Numeric.Extra

type Printer a buf = F.Flags -> Maybe Int -> Maybe Int -> a -> buf

printFixed :: (RealFloat f, Builder a) => Printer f a
printFixed flags width maybePrec f =
  justifySign flags width f
    $  build intToDigit whole'
    <> (if null part' then id else cons '.') (build intToDigit part')
 where
  prec          = fromMaybe 6 maybePrec
  (digs , e   ) = floatToDigits 10 f
  (whole, part) = splitAt e fullDigits
  whole' | null whole = [overflow]
         | otherwise  = init whole ++ [last whole + overflow]
  (overflow, part') = case roundTo 10 prec part of
    (1, x : xs) -> (1, xs)
    (_, xs    ) -> (0, xs)
  fullDigits = if e < 1
    then replicate (-e) 0 ++ digs
    else digs ++ replicate (e - length digs) 0

printExp :: (RealFloat f, Builder a) => Printer f a
printExp flags width maybePrec f =
  justifySign flags width f
    $  cons (intToDigit fdig1) (cons '.' $ build intToDigit floatDigits)
    <> char 'e'
    <> char expSign
    <> justifyRight (Just 2) '0' (showIntAtBase 10 intToDigit $ abs exponent)
 where
  prec                         = fromMaybe 6 maybePrec
  (fdig1 : floatDigits', exp') = floatToDigits 10 $ abs f
  floatDigits = floatDigits' ++ replicate (prec - length floatDigits') 0
  exponent                     = exp' - 1
  expSign                      = if signum exponent == -1 then '-' else '+'

-- for Generic, precision argument = total number of sig figs
printGeneric :: (RealFloat f, Builder a) => Printer f a
printGeneric flags width maybePrec f =
  justifySign flags width f
    $  build intToDigit whole
    <> char '.'
    <> build intToDigit part
 where
  (digs , e      ) = floatToDigits 10 f
  (whole, part   ) = splitAt e sigfigs
  (_    , sigfigs) = case maybePrec of
    Just p | p < length digs -> roundTo 10 p digs
    _                        -> (0, digs)
