module Text.Printf.TH.Layouters.Float where

import           Data.Maybe
import           Data.Char                      ( intToDigit )
import           Numeric                        ( floatToDigits )
import           GHC.Float                      ( roundTo )
import           Prelude                 hiding ( exponent )

import qualified Text.Printf.TH.Parse.Flags    as F
import           Text.Printf.TH.Builder
import           Text.Printf.TH.Layout

type Layouter a buf = Maybe Int -> Maybe Int -> F.Flags -> a -> buf

fixed width prec_ flags f' = layout width
                                    (signedPrefix flags f')
                                    (F.justify flags)
                                    buffer
 where
  buffer
    | roundSize < 0
    = str "0." <> times prec '0'
    | otherwise
    = let (whole, part) = splitAt exp1 digs1
      in  half whole <> char '.' <> half part
  f = abs f'
  half [] = char '0'
  half xs = build intToDigit xs
  (digs0, exp0) = floatToDigits 10 f
  (digs1, exp1)
    | roundSize < 0
    = ([], exp0)
    | otherwise
    = let (overflow, digs2) = roundTo 10 roundSize digs0
      in  (digs2, exp0 + overflow)
  prec      = fromMaybe 6 prec_
  roundSize = exp0 + prec
