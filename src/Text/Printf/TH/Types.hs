{-# Language TemplateHaskell #-}
{-# Language NamedFieldPuns #-}

module Text.Printf.TH.Types where

import qualified Data.Set as S
import Data.Set (Set)
import Language.Haskell.TH.Syntax

data Atom
    = Arg FormatArg
    | Str String
    deriving (Show)

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
    deriving (Show)

data MaySpecify
    = Given Integer
    | Need
    deriving (Show)

data FormatArg = FormatArg
    { flags :: FlagSet
    , width :: Maybe MaySpecify
    , precision :: Maybe MaySpecify
    , spec :: Char
    } deriving (Show)

type FormatStr = [Atom]

instance Lift FlagSet where
    lift (FlagSet a s b p) =
        [|FlagSet
              { adjustment = $(a')
              , signed = $(lift s)
              , spaced = $(lift b)
              , prefixed = $(lift p)
              }|]
      where
        a' =
            case a of
                Just LeftJustified -> [|Just LeftJustified|]
                Just ZeroPadded -> [|Just ZeroPadded|]
                Nothing -> [|Nothing|]

data FlagSet = FlagSet
    { adjustment :: Maybe Adjustment
    , signed :: Bool
    , spaced :: Bool
    , prefixed :: Bool
    } deriving (Show)

toFlagSet :: Set Flag -> FlagSet
toFlagSet fs
    | S.size (S.intersection fs adjustmentFlags) > 1 =
        error
            "Error: multiple adjustment flags specified; you can only have one of '-', '0', ' '"
    | spaced set && signed set = error "'+' and ' ' specifiers cannot be used together"
    | otherwise = set
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
            , spaced = FlagSpaced `elem` fs
            }
