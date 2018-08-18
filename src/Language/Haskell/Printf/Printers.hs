{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Printf.Printers where

import Control.Applicative ((<$>), pure)
import Data.Bool
import Data.Char
import Data.List
import Data.String (fromString)
import Foreign.Ptr
import GHC.Float (FFFormat(..))
import Language.Haskell.Printf.Geometry
import Language.Haskell.PrintfArg
import NumUtils
import qualified Parser.Types as P

type Printer n = PrintfArg n -> Value

printfString :: Printer String
printfString spec =
    Value
        { valArg =
              case prec spec of
                  Nothing -> spec
                  Just c -> take c <$> spec
        , valPrefix = Nothing
        , valSign = Nothing
        }

printfShow :: Show a => Printer a
printfShow spec = printfString (fromString . show <$> spec)

printfChar :: Printer Char
printfChar spec = Value {valArg = pure <$> spec, valPrefix = Nothing, valSign = Nothing}

printfPtr :: Printer (Ptr a)
printfPtr spec =
    Value
        { valArg =
              PrintfArg
                  { width = width spec
                  , prec = Nothing
                  , flagSet = P.emptyFlagSet {P.prefixed = True}
                  , lengthSpec = Nothing
                  , fieldSpec = 'p'
                  , value = showIntAtBase 16 intToDigit (toInt $ value spec)
                  }
        , valPrefix = Just "0x"
        , valSign = Nothing
        }
  where
    toInt x = x `minusPtr` nullPtr

printfDecimal spec =
    Value
        { valArg = padDecimal spec . showIntAtBase 10 intToDigit . abs <$> spec
        , valPrefix = Nothing
        , valSign = sign' spec
        }

fmtUnsigned ::
       forall a. (Bounded a, Integral a)
    => (Integer -> String)
    -> (PrintfArg a -> Maybe String)
    -> Printer a
fmtUnsigned shower p spec =
    Value
        { valArg = padDecimal spec . shower . clamp <$> spec
        , valPrefix = p spec
        , valSign = Nothing
        }
  where
    lb = minBound :: a
    clamp :: a -> Integer
    clamp x
        | x < 0 = toInteger x + (-2 * toInteger lb)
        | otherwise = toInteger x

printfHex b = fmtUnsigned showHex (prefix (bool "0x" "0X" b))
  where
    showHex =
        showIntAtBase
            16
            ((if b
                  then toUpper
                  else id) .
             intToDigit)

printfUnsigned = fmtUnsigned (showIntAtBase 10 intToDigit) (const Nothing)

printfOctal spec
    | "0" `isPrefixOf` value valArg = v
    | otherwise = v {valPrefix = prefix "0" spec}
  where
    v@Value {..} = fmtUnsigned (showIntAtBase 8 intToDigit) (const Nothing) spec

printfFloating upperFlag spec =
    Value {valArg = showFloat . abs <$> spec, valPrefix = Nothing, valSign = sign' spec}
  where
    precision =
        case prec spec of
            Just n -> Just (fromIntegral n)
            Nothing
                | Just P.ZeroPadded <- adjustment spec -> Just 6
            _ -> Nothing
    showFloat = formatRealFloatAlt FFFixed precision (prefixed spec) upperFlag

printfScientific upperFlag spec =
    Value {valArg = showSci . abs <$> spec, valPrefix = Nothing, valSign = sign' spec}
  where
    showSci =
        formatRealFloatAlt
            FFExponent
            (fromIntegral <$> prec spec)
            (prefixed spec)
            upperFlag

printfGeneric upperFlag spec =
    Value {valArg = showSci . abs <$> spec, valPrefix = Nothing, valSign = sign' spec}
  where
    showSci =
        formatRealFloatAlt
            FFGeneric
            (fromIntegral <$> prec spec)
            (prefixed spec)
            upperFlag

printfFloatHex upperFlag spec =
    Value
        { valArg = showHexFloat . abs <$> spec
        , valPrefix = Just (bool "0x" "0X" upperFlag)
        , valSign = sign' spec
        }
  where
    showHexFloat = formatHexFloat (fromIntegral <$> prec spec) (prefixed spec) upperFlag
