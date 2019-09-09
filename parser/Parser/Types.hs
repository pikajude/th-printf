{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Parser.Types where

import           Data.Foldable                  ( elem
                                                , notElem
                                                )
import qualified Data.Set                      as S
import           Data.Set                       ( Set )
import           Language.Haskell.TH.Lift
import           Lens.Micro.Platform
import           Prelude                 hiding ( elem
                                                , notElem
                                                )

data Atom
    = Arg FormatArg
    | Str String
    deriving (Show)

data LengthSpecifier
    = DoubleH
    | H
    | DoubleL
    | BigL
    | L
    | J
    | Z
    | T
    deriving (Eq)

instance Show LengthSpecifier where
  show DoubleH = "hh"
  show H       = "h"
  show DoubleL = "ll"
  show BigL    = "L"
  show L       = "l"
  show J       = "j"
  show Z       = "z"
  show T       = "t"

data Flag
    = FlagLJust
    | FlagSigned
    | FlagSpaced
    | FlagPrefixed
    | FlagZeroPadded
    deriving (Show, Eq, Ord)

adjustmentFlags :: Set Flag
adjustmentFlags = S.fromList [FlagLJust, FlagZeroPadded]

data Adjustment
    = LeftJustified
    | ZeroPadded
    deriving (Show, Eq)

data MaySpecify
    = Given Integer
    | Need
    deriving (Show)

data FormatArg = FormatArg
    { flags :: FlagSet
    , width :: Maybe MaySpecify
    , precision :: Maybe MaySpecify
    , spec :: Char
    , lengthSpec :: Maybe LengthSpecifier
    } deriving (Show)

type FormatStr = [Atom]

data FlagSet = FlagSet
    { adjustment :: Maybe Adjustment
    , signed :: Bool
    , spaced :: Bool
    , prefixed :: Bool
    } deriving (Show)

emptyFlagSet :: FlagSet
emptyFlagSet = FlagSet Nothing False False False

toFlagSet :: Set Flag -> FlagSet
toFlagSet fs = set'
 where
  adjustment | FlagLJust `S.member` fs      = Just LeftJustified
             | FlagZeroPadded `S.member` fs = Just ZeroPadded
             | otherwise                    = Nothing
  set' = FlagSet { signed     = FlagSigned `elem` fs
                 , prefixed   = FlagPrefixed `elem` fs
                 , adjustment
                 , spaced     = FlagSpaced `elem` fs && FlagSigned `notElem` fs
                 }

makeLensesFor
    [ ("adjustment", "adjustment_")
    , ("signed", "signed_")
    , ("spaced", "spaced_")
    , ("prefixed", "prefixed_")
    ]
    ''FlagSet

makeLensesFor
    [ ("flags", "flags_")
    , ("width", "width_")
    , ("precision", "precision_")
    , ("spec", "spec_")
    , ("lengthSpec", "lengthSpec_")
    ]
    ''FormatArg

deriveLiftMany [''Adjustment, ''FlagSet, ''LengthSpecifier]
