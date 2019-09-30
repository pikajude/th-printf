module Text.Printf.TH.Layouters where

import           Data.Char                      ( intToDigit )

import           Numeric.Extra
import           Text.Printf.TH.Builder
import qualified Text.Printf.TH.Parse.Flags    as F
import           Text.Printf.TH.Layout

type Layouter a buf = Maybe Int -> Maybe Int -> F.Flags -> a -> buf

decimal width prec flags d =
  layout width (signedPrefix flags d) (F.justify flags)
    $ justifyRight prec '0' shown
 where
  shown = if prec == Just 0 && d == 0
    then mempty
    else showIntAtBase 10 intToDigit (abs d)

string width prec flags s =
  layout width Nothing (F.noZero flags) $ str $ case prec of
    Just n  -> take n s
    Nothing -> s

chr width _ flags c = layout width Nothing (F.noZero flags) $ char c
