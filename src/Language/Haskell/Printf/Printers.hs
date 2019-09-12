{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Printf.Printers where

import           Control.Applicative            ( (<$>) )
import           Data.Char
import           Data.String                    ( fromString )
import           Data.Maybe                     ( fromMaybe )
import           Foreign.Ptr
import           GHC.Float                      ( FFFormat(..) )
import           Language.Haskell.Printf.Geometry
import           Language.Haskell.PrintfArg
import qualified Data.Text.Lazy                as L
import qualified Data.Text                     as S
import           Math.NumberTheory.Logarithms

import           NumUtils
import qualified Parser.Types                  as P
import qualified Buildable                     as B

type Printer n buf = PrintfArg n -> Value buf

printfString :: B.Buildable buf => Printer String buf
printfString spec = Value
  { valArg    = case prec spec of
                  Nothing -> B.str <$> spec
                  Just c  -> B.str . take c <$> spec
  , valPrefix = Nothing
  , valSign   = Nothing
  }

printfStrictText :: B.Buildable buf => Printer S.Text buf
printfStrictText spec = Value
  { valArg    = case prec spec of
                  Nothing -> B.sText <$> spec
                  Just c  -> B.sText . S.take c <$> spec
  , valPrefix = Nothing
  , valSign   = Nothing
  }

printfLazyText :: B.Buildable buf => Printer L.Text buf
printfLazyText spec = Value
  { valArg    = case prec spec of
                  Nothing -> B.lText <$> spec
                  Just c  -> B.lText . L.take (fromIntegral c) <$> spec
  , valPrefix = Nothing
  , valSign   = Nothing
  }

printfShow :: (B.Buildable buf, Show a) => Printer a buf
printfShow spec = printfString (fromString . show <$> spec)

printfChar :: B.Buildable buf => Printer Char buf
printfChar spec = Value { valArg    = B.singleton <$> spec
                        , valPrefix = Nothing
                        , valSign   = Nothing
                        }

{-# ANN printfPtr ("HLint: ignore Use showHex" :: String) #-}
printfPtr :: B.Buildable buf => Printer (Ptr a) buf
printfPtr spec = Value
  { valArg    = PrintfArg
                  { width      = width spec
                  , prec       = Nothing
                  , flagSet    = P.emptyFlagSet { P.prefixed = True }
                  , lengthSpec = Nothing
                  , fieldSpec  = 'p'
                  , value = showIntAtBase 16 intToDigit (ptrToWordPtr $ value spec)
                  }
  , valPrefix = Just (B.str "0x")
  , valSign   = Nothing
  }

printfDecimal spec = Value
  { valArg    = padDecimal spec . showIntAtBase 10 intToDigit . abs <$> spec
  , valPrefix = Nothing
  , valSign   = sign' spec
  }

fmtUnsigned
  :: (Bounded a, Integral a, B.Buildable buf)
  => (Integer -> buf)
  -> (PrintfArg a -> Maybe buf)
  -> Printer a buf
fmtUnsigned shower p spec = Value
  { valArg    = padDecimal spec . shower . clampUnsigned <$> spec
  , valPrefix = p spec
  , valSign   = Nothing
  }

printfHex b = fmtUnsigned showHex (prefix (if b then "0X" else "0x"))
  where showHex = showIntAtBase 16 ((if b then toUpper else id) . intToDigit)

printfUnsigned = fmtUnsigned (showIntAtBase 10 intToDigit) (const Nothing)

-- printing octal is really annoying.  consider
--
-- printf "%#-8.5x" 1234
--
-- "0x004d2 "
--  ^~~~~~~^ width (8)
--    ^~~~^  precision (5)
--  ^^       prefix (2)
--    ^^     padding (2)
--
-- printf "%#-8.5o" 1234
--
-- "02322   "
--  ^~~~~~~^ width (8)
--  ^~~~^    precision (5)
--  ^        prefix (1)
--  ^        padding (1, same character)
--
-- in octal, when combining prefix and padding, the prefix
-- must eat the first padding char
{-# ANN printfOctal ("HLint: ignore Use showOct" :: String) #-}
printfOctal spec = fmtUnsigned
  (showIntAtBase 8 intToDigit)
  (\y -> if shouldUnpad then Nothing else prefix "0" y)
  spec
 where
  expectedWidth = integerLogBase 8 (max 1 $ clampUnsigned $ value spec) + 1
  shouldUnpad   = prefixed spec && fromMaybe 0 (prec spec) > expectedWidth

printfFloating upperFlag spec = Value { valArg    = showFloat . abs <$> spec
                                      , valPrefix = Nothing
                                      , valSign   = sign' spec
                                      }
 where
  precision = case prec spec of
    Just n -> Just (fromIntegral n)
    Nothing | Just P.ZeroPadded <- adjustment spec -> Just 6
    _      -> Nothing
  showFloat = formatRealFloatAlt FFFixed precision (prefixed spec) upperFlag

printfScientific upperFlag spec = Value { valArg    = showSci . abs <$> spec
                                        , valPrefix = Nothing
                                        , valSign   = sign' spec
                                        }
 where
  showSci = formatRealFloatAlt FFExponent
                               (fromIntegral <$> prec spec)
                               (prefixed spec)
                               upperFlag

printfGeneric upperFlag spec = Value { valArg    = showSci . abs <$> spec
                                     , valPrefix = Nothing
                                     , valSign   = sign' spec
                                     }
 where
  showSci = formatRealFloatAlt FFGeneric
                               (fromIntegral <$> prec spec)
                               (prefixed spec)
                               upperFlag

printfFloatHex upperFlag spec = Value
  { valArg    = showHexFloat . abs <$> spec
  , valPrefix = Just (if upperFlag then "0X" else "0x")
  , valSign   = sign' spec
  }
 where
  showHexFloat =
    formatHexFloat (fromIntegral <$> prec spec) (prefixed spec) upperFlag

clampUnsigned :: (Bounded a, Integral a) => a -> Integer
clampUnsigned x | x < 0 = toInteger x + (-2 * toInteger (minBound `asTypeOf` x))
                | otherwise = toInteger x
