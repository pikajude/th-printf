module Text.Printf.TH.Layouters where

import           Data.Char                      ( intToDigit )
import qualified Data.Text                     as S
import qualified Data.Text.Lazy                as L
import           Foreign.Ptr

import           Numeric.Extra
import           Text.Printf.TH.Builder
import qualified Text.Printf.TH.Parse.Flags    as F
import           Text.Printf.TH.Layout

type Layouter a buf = Maybe Int -> Maybe Int -> F.Flags -> a -> buf

string width prec flags s =
  layout width Nothing (F.justify flags) $ str $ case prec of
    Just n  -> take n s
    Nothing -> s

shown width prec flags s = string width prec flags (show s)

strictText width prec flags s =
  layout width Nothing (F.justify flags) $ stext $ case prec of
    Just n  -> S.take n s
    Nothing -> s

lazyText width prec flags s =
  layout width Nothing (F.justify flags) $ ltext $ case prec of
    Just n  -> L.take (fromIntegral (n :: Int)) s
    Nothing -> s

chr width _ flags c = layout width Nothing (F.justify flags) $ char c

signed width prec flags d =
  layout width (signedPrefix flags d) (F.justify flags)
    . justifyRight prec '0'
    . hideZero prec d
    . showIntAtBase 10 intToDigit
    $ abs d

unsigned width prec flags d =
  layout width Nothing (F.justify flags)
    . justifyRight prec '0'
    . hideZero prec d
    . showIntAtBase 10 intToDigit
    $ clamp d

hexUpper = hex_ True
hexLower = hex_ False

{-# ANN hex_ ("HLint: ignore Use showHex" :: String) #-}
hex_ upper width prec flags d =
  layout width (hexPrefix upper flags d) (F.justify flags)
    . justifyRight prec '0'
    . hideZero prec d
    . showIntAtBase 16 (if upper then intToDigitUpper else intToDigit)
    $ clamp d

{-# ANN octal ("HLint: ignore Use showOct" :: String) #-}
octal width prec flags d =
  layout width Nothing (F.justify flags)
    . justifyRight prec '0'
    . hideZero prec d
    . oPrefix
    . showIntAtBase 8 intToDigit
    $ clamp d
 where
  oPrefix | F.prefix flags && d /= 0 = cons '0'
          | otherwise                = id

{-# ANN ptr ("HLint: ignore Use showHex" :: String) #-}
ptr width _ flags p =
  layout width (signedPrefix flags p0 <> Just (str "0x")) (F.justify flags)
    $ showIntAtBase 16 intToDigit p0
  where p0 = p `minusPtr` nullPtr

clamp x | x < 0     = toInteger x + (-2 * toInteger (minBound `asTypeOf` x))
        | otherwise = toInteger x
