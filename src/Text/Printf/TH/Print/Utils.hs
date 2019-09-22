module Text.Printf.TH.Print.Utils where

import           Text.Printf.TH.Builder
import qualified Text.Printf.TH.Parse.Flags    as F

justify :: Builder buf => F.Flags -> Maybe Int -> buf -> buf
justify _ Nothing b = b
justify flags w b | Just F.LeftJustify <- F.justify flags = justifyLeft w b
                  | Just F.ZeroFill <- F.justify flags    = justifyRight w b '0'
                  | otherwise                             = justifyRight w b ' '

sign :: (Num a, Eq a, Builder buf) => F.Flags -> a -> buf -> buf
sign flags numValue | signum numValue == -1 = cons '-'
                    | F.sign flags          = cons '+'
                    | otherwise             = id
