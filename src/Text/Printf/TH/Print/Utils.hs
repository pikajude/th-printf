module Text.Printf.TH.Print.Utils where

import           Text.Printf.TH.Builder
import qualified Text.Printf.TH.Parse.Flags    as F

justify :: Builder buf => F.Flags -> Maybe Int -> buf -> buf
justify _ Nothing b          = b
justify _ (Just n) b | n < 0 = justifyLeft (Just $ negate n) b
justify flags w b | Just F.LeftJustify <- F.justify flags = justifyLeft w b
                  | Just F.ZeroFill <- F.justify flags    = justifyRight w '0' b
                  | otherwise                             = justifyRight w ' ' b

sign :: (Num a, Eq a, Builder buf) => F.Flags -> a -> buf -> buf
sign flags numValue | signum numValue == -1 = cons '-'
                    | F.sign flags          = cons '+'
                    | F.space flags         = cons ' '
                    | otherwise             = id

signWidth flags val =
  if signum val == -1 || F.sign flags || F.space flags then 1 else 0

justifySign flags width value buf
  | Just F.LeftJustify <- F.justify flags = justifyLeft width
                                                        (sign flags value buf)
  | Just F.ZeroFill <- F.justify flags = sign flags value
  $ justifyRight (fmap (\x -> x - signWidth flags value) width) '0' buf
  | otherwise = justifyRight width ' ' $ sign flags value buf
