{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE NamedFieldPuns #-}

module Parser.Types where

import qualified Data.Set as S
import Data.Set (Set)
import Language.Haskell.TH.Syntax

data Atom
    = Arg FormatArg
    | Str String
    deriving (Show, Lift)

data LengthSpecifier
    = DoubleH
    | H
    | DoubleL
    | BigL
    | L
    | J
    | Z
    | T
    deriving (Show, Eq, Lift)

data Flag
    = FlagLJust
    | FlagSigned
    | FlagSpaced
    | FlagPrefixed
    | FlagZeroPadded
    deriving (Show, Eq, Ord)

adjustmentFlags = S.fromList [FlagLJust, FlagZeroPadded]

data Adjustment
    = LeftJustified
    | ZeroPadded
    deriving (Show, Lift)

data MaySpecify
    = Given Integer
    | Need
    deriving (Show, Lift)

data FormatArg = FormatArg
    { flags :: FlagSet
    , width :: Maybe MaySpecify
    , precision :: Maybe MaySpecify
    , spec :: Char
    , lengthSpec :: Maybe LengthSpecifier
    } deriving (Show, Lift)

type FormatStr = [Atom]

data FlagSet = FlagSet
    { adjustment :: Maybe Adjustment
    , signed :: Bool
    , spaced :: Bool
    , prefixed :: Bool
    } deriving (Show, Lift)

toFlagSet :: Set Flag -> FlagSet
toFlagSet fs = set
  where
    adjustment
        | FlagLJust `S.member` fs = Just LeftJustified
        | FlagZeroPadded `S.member` fs = Just ZeroPadded
        | otherwise = Nothing
    set =
        FlagSet
            { signed = FlagSigned `elem` fs
            , prefixed = FlagPrefixed `elem` fs
            , adjustment
            , spaced = FlagSpaced `elem` fs && FlagSigned `notElem` fs
            }
