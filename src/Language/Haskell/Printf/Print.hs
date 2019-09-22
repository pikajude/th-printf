module Language.Haskell.Printf.Print where

import           Language.Haskell.Printf.Builder
import           Numeric.Extra
import           Data.Char
import qualified Parse.Flags                   as F

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
printSigned flags width prec d = justify flags width $ sign $ justifyRight
  prec
  (showIntAtBase 10 intToDigit $ abs d)
  '0'
 where
  sign | d < 0        = cons '-'
       | F.sign flags = cons '+'
       | otherwise    = id

printShow f w p = printString f w p . show

printAny :: (Show a, Builder b) => Printer a b
printAny _ _ _ s = str (show s)

justify :: Builder buf => F.Flags -> Maybe Int -> buf -> buf
justify _ Nothing b = b -- shortcut, even though justify* already does this
justify flags w b | Just F.LeftJustify <- F.justify flags = justifyLeft w b
                  | Just F.ZeroFill <- F.justify flags    = justifyRight w b '0'
                  | otherwise                             = justifyRight w b ' '
