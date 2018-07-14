module Impl.Str where

import qualified Data.Char as C
import Data.Maybe
import Numeric
import qualified Prelude as P
import qualified Data.List
import Text.ParserCombinators.ReadP
import qualified Text.Read.Lex as R

type Str = P.String

type Index = P.Int

type Chr = P.Char

empty :: Str
empty = []

cons' :: Chr -> Str -> Str
cons' = (:)

infixr 5 `cons'`

elem :: Chr -> Str -> P.Bool
elem = P.elem

isPrefixOf :: Str -> Str -> P.Bool
isPrefixOf = Data.List.isPrefixOf

head :: Str -> Chr
head = P.head

take :: Index -> Str -> Str
take = P.take

chr :: Chr -> P.Char
chr x = x

singleton :: Chr -> Str
singleton c = [c]

showDecimal :: (P.Integral a, P.Show a) => a -> Str
showDecimal = P.show

showHex :: (P.Integral a, P.Show a) => P.Bool -> a -> Str
showHex upper s =
    showIntAtBase
        16
        (\d ->
             C.chr
                 (d P.+
                  (if d P.< 10
                       then 48
                       else if upper
                                then 55
                                else 87)))
        s
        ""

length :: Str -> Index
length = P.length

lexChar :: Str -> Maybe (Chr, Str)
lexChar s = listToMaybe (readP_to_S R.lexChar s)

justifyLeft :: Index -> Chr -> Str -> Str
justifyLeft n c s
    | diff P.<= 0 = s
    | P.otherwise = s P.++ P.replicate diff ' '
  where
    diff = n P.- P.length s

justifyRight :: Index -> Chr -> Str -> Str
justifyRight n c s
    | diff P.<= 0 = s
    | P.otherwise = P.replicate diff c P.++ s
  where
    diff = n P.- P.length s
