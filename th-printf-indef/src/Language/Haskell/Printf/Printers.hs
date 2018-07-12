{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Printf.Printers where

import Data.String (fromString)
import Language.Haskell.Printf.Geometry
import Language.Haskell.PrintfArg
import Numeric.Natural
import Parser.Types
import qualified Str as S

type Printer n = PrintfArg n -> Val

printfString :: Printer S.Str
printfString spec = adjust spec $ valOf $ value spec

printfChar :: Printer S.Chr
printfChar spec = adjust spec $ valOf $ S.singleton $ value spec

printfDecimal :: (Show a, Integral a) => Printer a
printfDecimal spec = adjustAndSign S.empty spec $ valOf $ S.showDecimal (abs $ value spec)

printfNatural :: Printer Natural
printfNatural = printfDecimal
