{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Printf.Lib
  ( toSplices
  , OutputType (..)
  )
where

import           Control.Applicative            ( (<$>)
                                                , pure
                                                )
import           Data.Maybe
import           Data.Semigroup                 ( (<>) )
import           Data.String                    ( fromString )
import           Language.Haskell.Printf.Geometry
                                                ( formatOne )
import qualified Language.Haskell.Printf.Printers
                                               as Printers
import           Language.Haskell.PrintfArg
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           GHC.Generics                   ( Generic )
import           Parser                         ( parseStr )
import           Parser.Types            hiding ( lengthSpec
                                                , width
                                                )

import           Buildable                      ( finalize
                                                , SizedList
                                                , SizedBuilder
                                                )

data OutputType = OutputString | OutputText deriving (Show, Eq, Ord, Generic, Enum, Bounded)

-- | Takes a format string as input and produces a tuple @(args, outputExpr)@.
--
-- This function processes character escapes as they would appear in Haskell source code.
-- It will emit warnings (or throw an error, as appropriate) when given an invalid format
-- string.
--
-- Use if you wish to leverage @th-printf@ in conjunction with, for example, an existing
-- logging library.
toSplices :: String -> OutputType -> Q ([Pat], Exp)
toSplices s' ot = case parseStr s' of
  Left  x          -> fail $ show x
  Right (y, warns) -> do
    mapM_ (qReport False) (concat warns)
    (lhss, rhss) <- unzip <$> mapM extractExpr y
    rhss'        <- appE
      [|finalize|]
      (sigE (foldr1 (\x y' -> infixApp x [|(<>)|] y') rhss) otype)
    return (map VarP $ concat lhss, rhss')
  where
    otype = case ot of
      OutputString -> [t|SizedList Char|]
      OutputText -> [t|SizedBuilder|]

extractExpr :: Atom -> Q ([Name], ExpQ)
extractExpr (Str s') = return ([], [|fromString $(stringE s')|])
extractExpr (Arg (FormatArg flags' width' precision' spec' lengthSpec')) = do
  (warg, wexp) <- extractArgs width'
  (parg, pexp) <- extractArgs precision'
  varg         <- newName "arg"
  return
    ( catMaybes [warg, parg, Just varg]
    , appE
      [|formatOne|]
      (appE
        formatter
        [|PrintfArg { flagSet = $(lift flags')
                    , width = $(wexp)
                    , prec = $(pexp)
                    , value = $(varE varg)
                    , lengthSpec = $(lift lengthSpec')
                    , fieldSpec = $(lift spec') }|]
      )
    )
 where
  extractArgs n = case n of
    Just Need -> do
      a <- newName "arg"
      pure (Just a, [|Just (fromInteger (fromIntegral $(varE a)))|])
    Just (Given n') -> pure (Nothing, [|Just $(litE $ integerL n')|])
    Nothing         -> pure (Nothing, [|Nothing|])
  formatter = case spec' of
    's' -> [|Printers.printfString|]
    '?' -> [|Printers.printfShow|]
    'd' -> [|Printers.printfDecimal|]
    'i' -> [|Printers.printfDecimal|]
    'p' -> [|Printers.printfPtr|]
    'c' -> [|Printers.printfChar|]
    'u' -> [|Printers.printfUnsigned|]
    'x' -> [|Printers.printfHex False|]
    'X' -> [|Printers.printfHex True|]
    'o' -> [|Printers.printfOctal|]
    'f' -> [|Printers.printfFloating False|]
    'F' -> [|Printers.printfFloating True|]
    'e' -> [|Printers.printfScientific False|]
    'E' -> [|Printers.printfScientific True|]
    'g' -> [|Printers.printfGeneric False|]
    'G' -> [|Printers.printfGeneric True|]
    'a' -> [|Printers.printfFloatHex False|]
    'A' -> [|Printers.printfFloatHex True|]
    _   -> undefined
