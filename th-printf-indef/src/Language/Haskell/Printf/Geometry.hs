{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.Printf.Geometry where

import Data.String (IsString(..))
import Parser.Types
import qualified Str as S
import Language.Haskell.PrintfArg

data Direction
    = Leftward
    | Rightward
    deriving (Show)

data Pad
    = Space
    | Zero
    deriving (Eq, Show)

data Val v = Val
    { valLit :: v
    , valWidth :: Maybe S.Index
    , valPrefix :: Maybe (S.Index, v)
    , valSign :: Maybe (S.Index, v)
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

setSign x v = v {valSign = Just x}

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

valSign' = maybe mempty snd . valSign

valPrefix' = maybe mempty snd . valPrefix

formatOne v@(Val {valWidth = Nothing, ..}) = mconcat [valSign' v, valPrefix' v, valLit]
formatOne v@(Val {valWidth = Just n, valDirection = Rightward, ..}) =
    if valPad == Zero
        then mconcat
                 [ maybe mempty snd valSign
                 , maybe mempty snd valPrefix
                 , S.justifyRight (n - extra) (S.chr '0') valLit
                 ]
        else S.justifyRight n (S.chr ' ') $
             formatOne (v {valWidth = Nothing})
  where
    extra = maybe 0 fst valPrefix + maybe 0 fst valSign
formatOne v@(Val {valWidth = Just n, valDirection = Leftward, ..}) =
    S.justifyLeft n (S.chr ' ') $ formatOne (v {valWidth = Nothing})
