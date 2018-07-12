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
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Parser (parseStr)
import Parser.Types hiding (width)
import qualified Str as S
import Language.Haskell.Printf.Geometry (formatOne)
import qualified Language.Haskell.Printf.Printers as Printers
import Language.Haskell.PrintfArg

-- | Printf a string
s :: QuasiQuoter
s =
    QuasiQuoter
        { quoteExp =
              \s ->
                  case parseStr "" s of
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
extractExpr (Arg (FormatArg flags width precision spec)) = do
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
