{-# LANGUAGE NoMonomorphismRestriction #-}

module NumUtils (showIntAtBase, formatRealFloatAlt, formatHexFloat) where

import Data.Bits
import Data.Char
import Data.Foldable
import Data.Ord
import Data.Semigroup ((<>))
import Data.Tuple
import GHC.Float (
  FFFormat (..),
  roundTo,
 )
import Numeric (floatToDigits)
import Prelude hiding (
  exp,
  foldr,
  (<>),
 )

import Buildable
import StrUtils

showIntAtBase ::
  (Buf buf, Show a, Integral a) => a -> (Int -> Char) -> a -> buf
showIntAtBase base toChr n0
  | base <= 1 = error "unsupported base"
  | n0 < 0 = error $ "negative number " ++ show n0
  | otherwise = showIt (quotRem n0 base) mempty
 where
  showIt (n, d) r = case n of
    0 -> r'
    _ -> showIt (quotRem n base) r'
   where
    r' = cons (toChr (fromIntegral d)) r

formatRealFloatAlt ::
  (Buf buf, RealFloat a) =>
  FFFormat ->
  Maybe Int ->
  Bool ->
  Bool ->
  a ->
  buf
formatRealFloatAlt fmt decs forceDot upper x
  | isNaN x = str "NaN"
  | isInfinite x = str $ if x < 0 then "-Infinity" else "Infinity"
  | x < 0 || isNegativeZero x =
      cons
        '-'
        (doFmt fmt (floatToDigits 10 (-x)) False)
  | otherwise = doFmt fmt (floatToDigits 10 x) False
 where
  eChar
    | upper = 'E'
    | otherwise = 'e'
  doFmt FFFixed (digs, exp) fullRounding
    | exp < 0 =
        doFmt FFFixed (replicate (negate exp) 0 ++ digs, 0) fullRounding
    | null part =
        fromDigits False whole <> (if forceDot then singleton '.' else mempty)
    | null whole =
        str "0." <> fromDigits False part
    | otherwise =
        fromDigits False whole <> singleton '.' <> fromDigits False part
   where
    (whole, part) =
      uncurry (flip splitAt) (toRoundedDigits decs (digs, exp) fullRounding)
  doFmt FFExponent ([0], _) _
    | forceDot = str "0.e+00"
    | otherwise = str "0e+00"
  doFmt FFExponent (digs, exp) fullRounding =
    shownDigs <> cons eChar shownExponent
   where
    shownDigs = case digs' of
      [] -> undefined
      [x'] ->
        cons (intToDigit x') (if forceDot then singleton '.' else mempty)
      (x' : xs) -> cons (intToDigit x') (cons '.' (fromDigits False xs))
    digs' = case decs of
      Just n ->
        case roundTo
          10
          (if fullRounding then min (length digs) n else n + 1)
          digs of
          (1, xs) -> 1 : xs
          (_, ys) -> ys
      Nothing -> digs
    exp' = exp - 1
    shownExponent =
      cons (if exp' < 0 then '-' else '+') $
        justifyRight 2 '0' $
          showIntAtBase 10 intToDigit $
            abs exp'
  doFmt FFGeneric d _ =
    minimumBy (comparing size) [doFmt FFFixed d True, doFmt FFExponent d True]

toRoundedDigits :: Maybe Int -> ([Int], Int) -> Bool -> ([Int], Int)
toRoundedDigits Nothing (digs, exp) _ = (digs, exp)
toRoundedDigits (Just prec) (digs, exp) fullRounding = (digs', exp + overflow)
 where
  (overflow, digs') =
    roundTo
      10
      (if fullRounding && prec > exp then min (length digs) prec else prec + exp)
      digs

fromDigits :: (Buf buf) => Bool -> [Int] -> buf
fromDigits upper =
  foldr (cons . (if upper then toUpper else id) . intToDigit) mempty

formatHexFloat ::
  (Buf buf, RealFloat a) => Maybe Int -> Bool -> Bool -> a -> buf
formatHexFloat decs alt upper x = doFmt (floatToDigits 2 x)
 where
  pChar
    | upper = 'P'
    | otherwise = 'p'
  doFmt ([], _) = undefined
  doFmt ([0], 0) = cons '0' (cons pChar (str "+0"))
  -- possible ghcjs bug - some floats are encoded as ([0,...], exp + 1)
  -- but the first digit should never be 0 unless the input is 0.0
  doFmt (0 : bits, exp) = doFmt (bits, exp - 1)
  doFmt (_ : bits, exp) =
    cons '1' $
      (if not (null hexDigits) || alt then singleton '.' else mempty)
        <> fromDigits upper hexDigits
        <> singleton pChar
        <> (if exp > 0 then singleton '+' else mempty)
        <> str (show (exp - 1 + overflow))
   where
    hexDigits' = go bits
    (overflow, hexDigits) = case decs of
      Just n -> case roundTo 16 n hexDigits' of
        (1, _ : digs) -> (1, digs)
        x' -> x'
      Nothing -> (0, hexDigits')
    go (a : b : c : d : xs) =
      ((a `shiftL` 3) .|. (b `shiftL` 2) .|. (c `shiftL` 1) .|. d) : go xs
    go [a, b, c] = go [a, b, c, 0]
    go [a, b] = go [a, b, 0, 0]
    go [a] = go [a, 0, 0, 0]
    go [] = []
