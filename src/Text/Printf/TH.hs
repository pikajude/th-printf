{-# Language DeriveDataTypeable #-}
{-# Language FlexibleInstances #-}
{-# Language QuasiQuotes #-}
{-# Language RecordWildCards #-}
{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# Language TupleSections #-}

module Text.Printf.TH
    ( s
    , st
    , sb
    ) where

import Control.Exception (Exception, throw)
import Data.ByteString.UTF8 (fromString)
import Data.Maybe
import Data.Proxy (Proxy(..))
import Data.Text (pack)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import System.IO
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding ((<$>), (<>), empty, line)
import Text.Trifecta

import Text.Printf.TH.Parser
import Text.Printf.TH.Printer
import Text.Printf.TH.Printer.String ()
import Text.Printf.TH.Types

data ParseError =
    ParseError
    deriving (Show)

instance Exception ParseError

s = quoter 'id

st = quoter 'pack

sb = quoter 'fromString

quoter f =
    QuasiQuoter
        { quoteExp = parse f
        , quotePat = error "printf cannot be used in a pattern context"
        , quoteType = error "printf cannot be used in a type context"
        , quoteDec = error "printf cannot be used in a decl context"
        }

parse f s =
    case parseString parseFmtStr mempty s of
        Success fmtStr -> build f ''String fmtStr
        Failure xs -> do
            runIO $ displayIO stderr $ renderPretty 0.8 80 $ _errDoc xs
            throw ParseError

build f outputType fmtStr = do
    pairs <- mapM mkExpr fmtStr
    let (args, exprs) = unzip pairs
    lamE (map varP $ concat args) $
        appE (varE f) $
        appsE [[|finalize|], [|Proxy :: Proxy $(conT outputType)|], listE exprs]

mkExpr (Str s) = pure ([] :: [Name], [|valOf (literal $(stringE s))|])
mkExpr (Arg (FormatArg flags width precision spec)) = do
    (warg, wexp) <- extractArgs width
    (parg, pexp) <- extractArgs precision
    varg <- newName "arg"
    pure
        ( catMaybes [warg, parg, Just varg]
        , appE
              formatter
              [|ArgSpec
                    { flagSet = $(lift flags)
                    , width = $(wexp)
                    , prec = $(pexp)
                    , value = $(varE varg)
                    }|])
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
            x
                | x `elem` "id" -> [|formatDec|]
            'u' -> [|formatNat|]
            'o' -> [|formatOct|]
            'x' -> [|formatHex|]
            'X' -> [|formatHexUpper|]
            's' -> [|formatStr|]
            'f' -> [|formatFloat|]
            'F' -> [|formatFloat|]
            'e' -> [|formatSci|]
            'E' -> [|formatSciUpper|]
            'g' -> [|formatG|]
            'G' -> [|formatGUpper|]
            'c' -> [|formatChar|]
            _ -> error "???"
