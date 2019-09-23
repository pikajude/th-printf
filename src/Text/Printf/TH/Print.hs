module Text.Printf.TH.Print where

import           Data.Char
import           Data.Maybe                     ( fromMaybe )
import           Numeric                        ( floatToDigits )
import           GHC.Float                      ( roundTo )

import           Numeric.Extra
import           Text.Printf.TH.Builder
import qualified Text.Printf.TH.Parse.Flags    as F
import           Text.Printf.TH.Print.Utils

type Printer a buf = F.Flags -> Maybe Int -> Maybe Int -> a -> buf

printString :: Builder a => Printer String a
printString flags width prec s = justify flags width $ str val
 where
  val = case prec of
    Just n  -> take n s
    Nothing -> s

printChar :: Builder a => Printer Char a
printChar flags width prec c = justify flags width $ char c

printSigned :: (Show i, Integral i, Builder a) => Printer i a
printSigned flags width prec d =
  justify flags width . sign flags d $ justifyRight
    prec
    (showIntAtBase 10 intToDigit $ abs d)
    '0'

printFixed :: (RealFloat f, Builder a) => Printer f a
printFixed flags width maybePrec f =
  build intToDigit whole' <> char '.' <> build intToDigit part'
 where
  prec          = fromMaybe 6 maybePrec
  (digs , e   ) = floatToDigits 10 f
  (whole, part) = splitAt e fullDigits
  whole' | null whole = [0]
         | otherwise  = whole
  (_, part') = roundTo 10 prec part
  fullDigits = if e < 1
    then replicate (-e) 0 ++ digs
    else digs ++ replicate (e - length digs) 0

printShow :: (Show i, Builder a) => Printer i a
printShow f w p = printString f w p . show

printAny :: (Show a, Builder b) => Printer a b
printAny _ _ _ s = str (show s)
