{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module NumUtils where

import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Ord
import           Data.Semigroup                 ( (<>) )
import           Data.Tuple
import           GHC.Float                      ( FFFormat(..)
                                                , roundTo
                                                )
import           Numeric                        ( floatToDigits )
import           Prelude                 hiding ( exp
                                                , foldr
                                                , (<>)
                                                )

import qualified Buildable                     as B
import           StrUtils

showIntAtBase
  :: (B.Buildable buf, Show a, Integral a) => a -> (Int -> Char) -> a -> buf
showIntAtBase base toChr n0 | base <= 1 = error "unsupported base"
                            | n0 < 0    = error $ "negative number " ++ show n0
                            | otherwise = showIt (quotRem n0 base) mempty
 where
  showIt (n, d) r = case n of
    0 -> r'
    _ -> showIt (quotRem n base) r'
    where r' = B.cons (toChr (fromIntegral d)) r

formatRealFloatAlt
  :: (B.Buildable buf, RealFloat a)
  => FFFormat
  -> Maybe Int
  -> Bool
  -> Bool
  -> a
  -> buf
formatRealFloatAlt fmt decs forceDot upper x
  | isNaN x = B.str "NaN"
  | isInfinite x = B.str $ if x < 0 then "-Infinity" else "Infinity"
  | x < 0 || isNegativeZero x = B.cons
    '-'
    (doFmt fmt (floatToDigits 10 (-x)) False)
  | otherwise = doFmt fmt (floatToDigits 10 x) False
 where
  eChar | upper     = 'E'
        | otherwise = 'e'
  doFmt FFFixed (digs, exp) fullRounding
    | exp < 0
    = doFmt FFFixed (replicate (negate exp) 0 ++ digs, 0) fullRounding
    | null part
    = fromDigits False whole <> (if forceDot then B.singleton '.' else mempty)
    | null whole
    = B.str "0." <> fromDigits False part
    | otherwise
    = fromDigits False whole <> B.singleton '.' <> fromDigits False part
   where
    (whole, part) =
      uncurry (flip splitAt) (toRoundedDigits decs (digs, exp) fullRounding)
  doFmt FFExponent ([0], _) _ | forceDot  = B.str "0.e+00"
                              | otherwise = B.str "0e+00"
  doFmt FFExponent (digs, exp) fullRounding =
    shownDigs <> B.cons eChar shownExponent
   where
    shownDigs = case digs' of
      [] -> undefined
      [x'] ->
        B.cons (intToDigit x') (if forceDot then B.singleton '.' else mempty)
      (x' : xs) -> B.cons (intToDigit x') (B.cons '.' (fromDigits False xs))
    digs' = case decs of
      Just n ->
        case
            roundTo 10
                    (if fullRounding then min (length digs) n else n + 1)
                    digs
          of
            (1, xs) -> 1 : xs
            (_, ys) -> ys
      Nothing -> digs
    exp' = exp - 1
    shownExponent =
      B.cons (if exp' < 0 then '-' else '+')
        $ justifyRight 2 '0'
        $ showIntAtBase 10 intToDigit
        $ abs exp'
  doFmt FFGeneric d _ =
    minimumBy (comparing B.size) [doFmt FFFixed d True, doFmt FFExponent d True]

toRoundedDigits :: Maybe Int -> ([Int], Int) -> Bool -> ([Int], Int)
toRoundedDigits Nothing     (digs, exp) _            = (digs, exp)
toRoundedDigits (Just prec) (digs, exp) fullRounding = (digs', exp + overflow)
 where
  (overflow, digs') = roundTo
    10
    (if fullRounding && prec > exp then min (length digs) prec else prec + exp)
    digs

fromDigits :: B.Buildable buf => Bool -> [Int] -> buf
fromDigits upper =
  foldr (B.cons . (if upper then toUpper else id) . intToDigit) mempty

formatHexFloat
  :: (B.Buildable buf, RealFloat a) => Maybe Int -> Bool -> Bool -> a -> buf
formatHexFloat decs alt upper x = doFmt (floatToDigits 2 x)
 where
  pChar | upper     = 'P'
        | otherwise = 'p'
  doFmt ([] , _) = undefined
  doFmt ([0], 0) = B.cons '0' (B.cons pChar (B.str "+0"))
  -- possible ghcjs bug - some floats are encoded as ([0,...], exp + 1)
  -- but the first digit should never be 0 unless the input is 0.0
  doFmt (0 : bits, exp) = doFmt (bits, exp - 1)
  doFmt (_ : bits, exp) =
    B.cons '1'
      $ (if not (null hexDigits) || alt then B.singleton '.' else mempty)
      <> fromDigits upper hexDigits
      <> B.singleton pChar
      <> (if exp > 0 then B.singleton '+' else mempty)
      <> B.str (show (exp - 1 + overflow))
   where
    hexDigits'            = go bits
    (overflow, hexDigits) = case decs of
      Just n -> case roundTo 16 n hexDigits' of
        (1, _ : digs) -> (1, digs)
        x'            -> x'
      Nothing -> (0, hexDigits')
    go (a : b : c : d : xs) =
      ((a `shiftL` 3) .|. (b `shiftL` 2) .|. (c `shiftL` 1) .|. d) : go xs
    go [a, b, c] = go [a, b, c, 0]
    go [a, b]    = go [a, b, 0, 0]
    go [a]       = go [a, 0, 0, 0]
    go []        = []
