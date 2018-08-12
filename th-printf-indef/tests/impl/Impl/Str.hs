module Impl.Str where

import qualified Data.List
import qualified Prelude as P
import qualified System.IO

type Str = P.String

type Index = P.Int

type Chr = P.Char

take :: Index -> Str -> Str
take = P.take

singleton :: Chr -> Str
singleton c = [c]

length :: Str -> Index
length = P.length

justifyLeft :: Index -> Chr -> Str -> Str
justifyLeft n c s
    | diff P.<= 0 = s
    | P.otherwise = s P.++ P.replicate diff c
  where
    diff = n P.- P.length s

justifyRight :: Index -> Chr -> Str -> Str
justifyRight n c s
    | diff P.<= 0 = s
    | P.otherwise = P.replicate diff c P.++ s
  where
    diff = n P.- P.length s

isPrefixOf :: Str -> Str -> P.Bool
isPrefixOf = Data.List.isPrefixOf

head :: Str -> Chr
head = P.head

hPutStr :: System.IO.Handle -> P.String -> System.IO.IO ()
hPutStr = System.IO.hPutStr

elem :: Chr -> Str -> P.Bool
elem = P.elem

empty :: Str
empty = []

cons' :: Chr -> Str -> Str
cons' = (:)

infixr 5 `cons'`

chr :: Chr -> P.Char
chr x = x
