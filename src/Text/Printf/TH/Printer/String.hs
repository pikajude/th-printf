{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language FlexibleContexts #-}
{-# Language RecordWildCards #-}
{-# Language TypeSynonymInstances #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleInstances #-}

module Text.Printf.TH.Printer.String where

import NumericUtils
import Text.Printf.TH.Printer

instance Printer String where
    type Output String = String
    string = id
    formatChar' = pure
    cons = (:)
    output _ = id
    rjust c n s
        | diff <= 0 = s
        | otherwise = replicate diff c ++ s
      where
        diff = fromIntegral n - length s
    ljust n s
        | diff <= 0 = s
        | otherwise = s ++ replicate diff ' '
      where
        diff = fromIntegral n - length s
    formatDec' = showIntAtBase 10 intToDigit
    formatOct' = showIntAtBase 8 intToDigit
    formatHex' = showIntAtBase 16 intToDigit
    formatHexUpper' = showIntAtBase 16 intToDigitUpper
    formatFloat' p =
        ( formatRealFloatAlt FFFixed (fromIntegral <$> p) False False
        , formatRealFloatAlt FFFixed (fromIntegral <$> p) True False)
    formatHexFloat' p =
        ( formatFloatHex (fromIntegral <$> p) False False
        , formatFloatHex (fromIntegral <$> p) True False)
    formatHexFloatUpper' p =
        ( formatFloatHex (fromIntegral <$> p) False True
        , formatFloatHex (fromIntegral <$> p) True True)
    formatSci' p =
        ( formatRealFloatAlt FFExponent (fromIntegral <$> p) False False
        , formatRealFloatAlt FFExponent (fromIntegral <$> p) True False)
    formatSciUpper' p =
        ( formatRealFloatAlt FFExponent (fromIntegral <$> p) False True
        , formatRealFloatAlt FFExponent (fromIntegral <$> p) True True)
    formatG' p =
        ( formatRealFloatAlt FFGeneric (fromIntegral <$> p) False False
        , formatRealFloatAlt FFGeneric (fromIntegral <$> p) True False)
    formatGUpper' p =
        ( formatRealFloatAlt FFGeneric (fromIntegral <$> p) False True
        , formatRealFloatAlt FFGeneric (fromIntegral <$> p) True True)
