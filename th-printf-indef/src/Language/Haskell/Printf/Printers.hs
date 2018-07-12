{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Printf.Printers where

import Data.String (fromString)
import Language.Haskell.Printf.Geometry
import Language.Haskell.PrintfArg
import Numeric.Natural
import Parser.Types
import qualified Str as S

printfString spec = adjust spec $ valOf $ value spec

printfChar spec = adjust spec $ valOf $ S.singleton $ value spec

printfDecimal :: (Show a, Integral a) => PrintfArg a -> Val S.Str
printfDecimal spec = adjustAndSign S.empty spec $ valOf $ S.showDecimal (abs $ value spec)

printfNatural = printfDecimal @Natural

adjustAndSign :: (Num n, Eq n) => S.Str -> PrintfArg n -> Val S.Str -> Val S.Str
adjustAndSign pref (PrintfArg flags width _ num) =
    adj . setWidth' width . sign flags num . prefix pref flags num
  where
    adj =
        case adjustment flags of
            Nothing -> id
            Just LeftJustified -> setLeftAligned
            Just ZeroPadded -> setZero

sign flags n
    -- -1 as a "Natural" literal causes a runtime error
    | signum n `notElem` [0, 1] = setSign (1, "-")
    | spaced flags = setSign (1, " ")
    | signed flags = setSign (1, "+")
    | otherwise = id

prefix _ _ 0 = id
prefix s flags _
    | prefixed flags = setPrefix (S.length s, s)
    | otherwise = id
