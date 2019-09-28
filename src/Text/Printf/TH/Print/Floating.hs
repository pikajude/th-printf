module Text.Printf.TH.Print.Floating where

import           Data.Char
import           Data.Maybe                     ( fromMaybe )
import           Numeric                        ( floatToDigits )
import           GHC.Float                      ( roundTo )
import           Data.Bits
import           Prelude                 hiding ( floatDigits
                                                , exponent
                                                )

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
    (1, _ : xs) -> (1, xs)
    (_, xs    ) -> (0 :: Int, xs)
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

printHexFloatUpper = printHexFloat True
printHexFloatLower = printHexFloat False

printHexFloat uppercase flags width maybePrec f =
  justifySign flags width f $ mconcat
    [ if uppercase then str "0X" else str "0x"
    , showIntAtBase 16 toDig finalWhole
    , if null rounded && not (F.prefix flags) then mempty else char '.'
    , build toDig rounded
    , char (if uppercase then 'P' else 'p')
    , expSign
    , showIntAtBase 10 toDig (abs finalExp)
    ]
 where
  (rounded, finalWhole) = case maybePrec of
    Just n -> case roundTo 16 n hexDigits of
      (0, xs    ) -> (xs, whole)
      (x, 1 : xs) -> (xs, whole + x)
      (overflow, _) ->
        error $ "roundTo produced strange result: " ++ show overflow
    Nothing -> (hexDigits, whole)
  hexDigits          = toHex digs
  toDig              = if uppercase then intToDigitUpper else intToDigit
  (whole : digs, e') = floatToDigits 2 f
  finalExp           = e' - 1
  expSign | finalExp < 0 = char '-'
          | otherwise    = char '+'
  toHex (a : b : c : d : xs) =
    (a <<< 3 .|. b <<< 2 .|. c <<< 1 .|. d) : toHex xs
  toHex []        = []
  toHex [a]       = [a <<< 3]
  toHex [a, b]    = [a <<< 3 .|. b <<< 2]
  toHex [a, b, c] = [a <<< 3 .|. b <<< 2 .|. c <<< 1]

(<<<) = shiftL
infixl 8 <<<
