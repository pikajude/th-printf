{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Haskell.Printf
    ( s
    ) where

import Data.Maybe
import Data.String (fromString)
import Language.Haskell.Printf.Geometry (formatOne)
import qualified Language.Haskell.Printf.Printers as Printers
import Language.Haskell.PrintfArg
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Parser (parseStr)
import Parser.Types hiding (width, lengthSpec)
import qualified Str as S

-- | Printf a string
s :: QuasiQuoter
s =
    QuasiQuoter
        { quoteExp =
              \s ->
                  case parseStr s of
                      Left x -> error $ show x
                      Right y -> do
                          (lhss, rhss) <- unzip <$> mapM extractExpr y
                          let rhss' = foldr1 (\x y -> infixApp x [|(<>)|] y) rhss
                          lamE (map varP $ concat lhss) rhss'
        , quotePat = error "this quoter cannot be used in a pattern context"
        , quoteType = error "this quoter cannot be used in a type context"
        , quoteDec = error "this quoter cannot be used in a declaration context"
        }

extractExpr (Str s) = return ([], [|fromString $(stringE s)|])
extractExpr (Arg (FormatArg flags width precision spec lengthSpec)) = do
    (warg, wexp) <- extractArgs width
    (parg, pexp) <- extractArgs precision
    varg <- newName "arg"
    return
        ( catMaybes [warg, parg, Just varg]
        , appE
              [|formatOne|]
              (appE
                   formatter
                   [|PrintfArg
                         { flagSet = $(lift flags)
                         , width = $(wexp)
                         , prec = $(pexp)
                         , value = $(varE varg)
                         , lengthSpec = $(lift lengthSpec)
                         , fieldSpec = $(lift spec)
                         }|]))
  where
    extractArgs n =
        case n of
            Just Need -> do
                a <- newName "arg"
                pure (Just a, [|Just $(varE a)|])
            Just (Given n) -> pure (Nothing, [|Just $(litE $ integerL n)|])
            Nothing -> pure (Nothing, [|Nothing|])
    formatter =
        case spec of
            's' -> [|Printers.printfString|]
            'd' -> [|Printers.printfDecimal|]
            'i' -> [|Printers.printfDecimal|]
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

errorInternal s = error $ "th-printf internal error: " ++ s
