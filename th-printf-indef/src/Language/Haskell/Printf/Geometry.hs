{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Printf.Geometry where

import Control.Monad.Fix
import Data.Maybe
import Data.String (IsString(..))
import Language.Haskell.PrintfArg
import Parser.Types
import qualified Str as S

data Direction
    = Leftward
    | Rightward
    deriving (Show)

data Pad
    = Space
    | Zero
    deriving (Eq, Show)

data Val = Val
    { valLit :: S.Str
    , valWidth :: Maybe S.Index
    , valPrefix :: Maybe S.Str
    , valSign :: Maybe S.Chr
    , valPad :: Pad
    , valDirection :: Direction
    } deriving (Show)

valOf x =
    Val
        { valLit = x
        , valWidth = Nothing
        , valPrefix = Nothing
        , valSign = Nothing
        , valPad = Space
        , valDirection = Rightward
        }

setSign x v = v {valSign = Just (S.chr x)}

setPrefix x v = v {valPrefix = Just x}

setRightAligned v = v {valDirection = Rightward}

setLeftAligned v = v {valDirection = Leftward}

setWidth n v = v {valWidth = Just n}

setWidth' n v = v {valWidth = n}

setZero v = v {valPad = Zero}

adjust (PrintfArg flags width _ _) =
    setWidth' width .
    case adjustment flags of
        Nothing -> id
        Just LeftJustified -> setLeftAligned
        Just ZeroPadded -> setZero

valSign' = maybe mempty S.singleton . valSign

valPrefix' = fromMaybe mempty . valPrefix

formatOne v@(Val {valWidth = Nothing, ..}) = mconcat [valSign' v, valPrefix' v, valLit]
formatOne v@(Val {valWidth = Just n, valDirection = Rightward, ..}) =
    if valPad == Zero
        -- for zero-pads, the sign and prefix go first
        -- i.e. "-0x00000fcb"
        then zeroPrefix `mappend`
             S.justifyRight (n - S.length zeroPrefix) (S.chr '0') valLit
        -- but for space-padding, the sign and prefix are considered
        -- part of the token
        -- "     -0xfcb"
        else S.justifyRight n (S.chr ' ') $ formatOne (v {valWidth = Nothing})
  where
    zeroPrefix = valSign' v `mappend` valPrefix' v
formatOne v@(Val {valWidth = Just n, valDirection = Leftward, ..}) =
    S.justifyLeft n (S.chr ' ') $ formatOne (v {valWidth = Nothing})

adjustAndSign :: (Num n, Ord n) => S.Str -> PrintfArg n -> Val -> Val
adjustAndSign pref (PrintfArg flags width _ num) =
    adj . setWidth' width . sign flags num . prefix pref flags num
  where
    adj =
        case adjustment flags of
            Nothing -> id
            Just LeftJustified -> setLeftAligned
            Just ZeroPadded -> setZero

sign :: (Num n, Ord n) => FlagSet -> n -> Val -> Val
sign flags n
    | signum n < 0 = setSign '-'
    | spaced flags = setSign ' '
    | signed flags = setSign '+'
    | otherwise = id

prefix _ _ 0 = id
prefix s flags _
    | prefixed flags = setPrefix s
    | otherwise = id
