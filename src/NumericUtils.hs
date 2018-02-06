{-# Language MagicHash #-}

module NumericUtils
    ( module NumericUtils
    , intToDigit
    , FFFormat(..)
    ) where

import Data.Char
import GHC.Base
import GHC.Float (FFFormat(..), roundTo)
import Numeric (floatToDigits)
import Text.Printf.TH.Printer

intToDigitUpper :: Int -> Char
intToDigitUpper (I# i)
    | isTrue# (i >=# 0#) && isTrue# (i <=# 9#) = unsafeChr (ord '0' + I# i)
    | isTrue# (i >=# 10#) && isTrue# (i <=# 15#) = unsafeChr (ord 'A' + I# i - 10)
    | otherwise = error ("Char.intToDigit: not a digit " ++ show (I# i))

showIntAtBase :: (Integral i, Monoid b, Printer b) => i -> (Int -> Char) -> i -> b
showIntAtBase base toB n0 = showIt (quotRem n0 base) mempty
  where
    showIt (n, d) r =
        case n of
            0 -> r'
            _ -> showIt (quotRem n base) r'
      where
        r' = cons (toB $ fromIntegral d) r

-- based on GHC.Float.formatRealFloatAlt, except this adds an uppercasing
-- flag and "fixes" (imo) the (Just 0) precision case
formatRealFloatAlt :: RealFloat a => FFFormat -> Maybe Int -> Bool -> Bool -> a -> String
formatRealFloatAlt fmt decs alt upper x
    | isNaN x = "NaN"
    | isInfinite x =
        if x < 0
            then "-Infinity"
            else "Infinity"
    | x < 0 || isNegativeZero x = '-' : doFmt fmt (floatToDigits (toInteger base) (-x))
    | otherwise = doFmt fmt (floatToDigits (toInteger base) x)
  where
    eChar
        | upper = 'E'
        | otherwise = 'e'
    base = 10
    doFmt format (is, e) =
        let ds = map intToDigit is
         in case format of
                FFGeneric ->
                    let alt1 = doFmt FFExponent (is, e)
                        alt2 = doFmt FFFixed (is, e)
                     in if length alt2 > length alt1
                            then alt1
                            else alt2
                FFExponent ->
                    case decs of
                        Nothing ->
                            let show_e' = show (e - 1)
                             in case ds of
                                    "0" -> "0.0" ++ (eChar : "0")
                                    [d] -> d : ".0" ++ [eChar] ++ show_e'
                                    (d:ds') -> d : '.' : ds' ++ [eChar] ++ show_e'
                                    [] -> error "formatRealFloat/doFmt/FFExponent: []"
                        Just dec ->
                            let dec' = max dec 0
                             in case is of
                                    [0] ->
                                        (if dec' == 0
                                             then "0"
                                             else "0.") ++
                                        take dec' (repeat '0') ++ (eChar : "0")
                                    _ ->
                                        let (ei, is') = roundTo base (dec' + 1) is
                                            (d:ds') =
                                                map
                                                    intToDigit
                                                    (if ei > 0
                                                         then init is'
                                                         else is')
                                         in (if null ds' && not alt
                                                 then [d]
                                                 else d : ".") ++
                                            ds' ++ eChar : show (e - 1 + ei)
                FFFixed ->
                    let mk0 ls =
                            case ls of
                                "" -> "0"
                                _ -> ls
                     in case decs of
                            Nothing
                                | e <= 0 -> "0." ++ replicate (-e) '0' ++ ds
                                | otherwise ->
                                    let f 0 s rs = mk0 (reverse s) ++ '.' : mk0 rs
                                        f n s "" = f (n - 1) ('0' : s) ""
                                        f n s (r:rs) = f (n - 1) (r : s) rs
                                     in f e "" ds
                            Just dec ->
                                let dec' = max dec 0
                                 in if e >= 0
                                        then let (ei, is') = roundTo base (dec' + e) is
                                                 (ls, rs) =
                                                     splitAt (e + ei) (map intToDigit is')
                                              in mk0 ls ++
                                                 (if null rs && not alt
                                                      then ""
                                                      else '.' : rs)
                                        else let (ei, is') =
                                                     roundTo
                                                         base
                                                         dec'
                                                         (replicate (-e) 0 ++ is)
                                                 d:ds' =
                                                     map
                                                         intToDigit
                                                         (if ei > 0
                                                              then is'
                                                              else 0 : is')
                                              in d :
                                                 (if null ds' && not alt
                                                      then ""
                                                      else '.' : ds')

-- we don't add 0x here because it's handled by the formatter
formatFloatHex :: RealFloat a => Maybe Int -> Bool -> Bool -> a -> String
formatFloatHex decs alt upper x = doFmt (floatToDigits 2 x)
  where
    pChar
        | upper = 'P'
        | otherwise = 'p'
    round' =
        case decs of
            Just d ->
                \x ->
                    let (a, b) = roundTo 16 d x
                     in ( intToDigit (a + 1)
                        , if a > 0
                              then tail b
                              else b)
            Nothing -> \x -> ('1', x)
    doFmt ([0], 0) = "0" ++ [pChar] ++ "+0"
    doFmt ((1:bits), exp) =
        first :
        (if null digs && not alt
             then ""
             else ".") ++
        map
            (if upper
                 then intToDigitUpper
                 else intToDigit)
            digs ++
        [pChar] ++
        (if exp >= 1
             then "+"
             else "") ++
        show (exp - 1)
      where
        (first, digs) = round' $ go bits
        go (a:b:c:d:xs) = foldl (\a b -> 2 * a + b) 0 [a, b, c, d] : go xs
        go [] = []
        go ys = go (take 4 $ ys ++ repeat 0)
    doFmt _ = error "nonsense"
