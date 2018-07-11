{-# LANGUAGE OverloadedStrings #-}

module TH.Printf.Printers where

import Data.String (fromString)
import Parser.Types
import qualified Str as S
import TH.Printf.Geometry
import TH.PrintfArg

printfString spec = adjust spec $ valOf (value spec)

printfDecimal spec = adjustAndSign S.empty spec $ valOf $ S.showDecimal (abs $ value spec)

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
    | signum n == -1 = setSign (1, "-")
    | spaced flags = setSign (1, " ")
    | signed flags = setSign (1, "+")
    | otherwise = id

prefix _ _ 0 = id
prefix s flags _
    | prefixed flags = setPrefix (S.length s, s)
    | otherwise = id
