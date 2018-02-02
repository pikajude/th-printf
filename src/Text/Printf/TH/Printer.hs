{-# Language RecordWildCards #-}
{-# Language DefaultSignatures #-}
{-# Language OverloadedStrings #-}
{-# Language NamedFieldPuns #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}
{-# Language TypeFamilies #-}

module Text.Printf.TH.Printer where

import Data.Monoid
import Data.String
import Numeric.Natural
import Text.Printf.TH.Types

class (IsString a, Monoid a) =>
      Printer a
    where
    type Output a
    literal :: String -> a
    cons :: Char -> a -> a
    cons c = (formatChar' c <>)
    rjust :: Char -> Int -> a -> a
    ljust :: Int -> a -> a
    output :: proxy a -> a -> Output a
    formatChar' :: Char -> a
    formatDec' :: Integral i => i -> a
    formatOct' :: Integral i => i -> a
    formatHex' :: Integral i => i -> a
    formatHexUpper' :: Integral i => i -> a
    formatFloat' :: RealFloat f => Maybe Int -> (f -> a, f -> a)
    formatSci' :: RealFloat f => Maybe Int -> f -> a
    formatSciUpper' :: RealFloat f => Maybe Int -> f -> a
    formatG' :: RealFloat f => Maybe Int -> (f -> a, f -> a)
    formatGUpper' :: RealFloat f => Maybe Int -> (f -> a, f -> a)

data ArgSpec v = ArgSpec
    { flagSet :: FlagSet
    , width :: Maybe Int
    , prec :: Maybe Int
    , value :: v
    }

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
    , valWidth :: Maybe Int
    , valPrefix :: Maybe (Int, v)
    , valSign :: Maybe (Int, v)
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

valSign' = maybe mempty snd . valSign

valPrefix' = maybe mempty snd . valPrefix

setSign x v = v {valSign = Just $ fmap literal x}

setPrefix x v = v {valPrefix = Just $ fmap literal x}

setRightAligned v = v {valDirection = Rightward}

setLeftAligned v = v {valDirection = Leftward}

setWidth n v = v {valWidth = Just n}

setWidth' n v = v {valWidth = n}

setZero v = v {valPad = Zero}

instance Functor ArgSpec where
    fmap f a@(ArgSpec {value}) = a {value = f value}

adjust (ArgSpec flags width _ _) =
    setWidth' width .
    case adjustment flags of
        Nothing -> id
        Just LeftJustified -> setLeftAligned
        Just ZeroPadded -> setZero

helper :: (Num a, Eq a, Printer p) => (a -> p) -> String -> ArgSpec a -> Val p
helper f pref spec = adjustAndSign pref spec $ valOf $ f (abs $ value spec)

adjustAndSign :: (Num n, Printer a, Eq n) => String -> ArgSpec n -> Val a -> Val a
adjustAndSign pref (ArgSpec flags width _ num) =
    adj . setWidth' width . sign flags num . prefix pref flags num
  where
    adj =
        case adjustment flags of
            Nothing -> id
            Just LeftJustified -> setLeftAligned
            Just ZeroPadded -> setZero

prefix _ _ 0 = id
prefix s flags _
    | prefixed flags = setPrefix (length s, s)
    | otherwise = id

sign flags n
    | signum n == -1 = setSign (1, "-")
    | spaced flags = setSign (1, " ")
    | signed flags = setSign (1, "+")
    | otherwise = id

formatDec = helper formatDec' ""

formatOct = helper formatOct' "0"

formatHex = helper formatHex' "0x"

formatHexUpper = helper formatHexUpper' "0X"

helper' spec pair
    | prefixed (flagSet spec) = helper (snd pair) "" spec
    | otherwise = helper (fst pair) "" spec

formatFloat spec = helper' spec (formatFloat' (prec spec))

formatSci spec = helper (formatSci' (prec spec)) "" spec

formatSciUpper spec = helper (formatSciUpper' (prec spec)) "" spec

formatG spec = helper' spec (formatG' (prec spec))

formatGUpper spec = helper' spec (formatGUpper' (prec spec))

formatNat = formatDec . fmap (fromIntegral :: Natural -> Integer)

fOne v@(Val {valWidth = Nothing, ..}) = mconcat [valSign' v, valPrefix' v, valLit]
fOne v@(Val {valWidth = Just n, valDirection = Rightward, ..}) =
    if valPad == Zero
        then mconcat
                 [ maybe mempty snd valSign
                 , maybe mempty snd valPrefix
                 , rjust '0' (n - extra) valLit
                 ]
        else rjust ' ' n $ fOne (v {valWidth = Nothing})
  where
    extra = maybe 0 fst valPrefix + maybe 0 fst valSign
fOne v@(Val {valWidth = Just n, valDirection = Leftward, ..}) =
    ljust n $ fOne (v {valWidth = Nothing})

finalize p = foldMap (output p . fOne)

formatStr spec = adjust spec $ valOf $ literal (value spec)

formatChar spec = adjust spec $ valOf $ formatChar' (value spec)
