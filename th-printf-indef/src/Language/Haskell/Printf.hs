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
import Parser.Types hiding (lengthSpec, width)

-- * Formatting strings
-- | @
-- ['s'|Hello, %s! (%d people greeted)|] :: ... -> String
-- @
--
-- This formatter follows the guidelines listed
-- <http://www.cplusplus.com/reference/cstdio/printf/ here>, except for
-- @%n@ (store number of printed characters) for obvious
-- reasons.
--
-- @
-- %c     :: 'Char'
-- %s     :: 'String'
--
-- %?     :: 'Show' a => a
--
-- %d, %i :: 'Integral' i => i
--
-- %u     :: ('Bounded' i, 'Integral' i) => i
-- %o     :: ('Bounded' i, 'Integral' i) => i
-- %x, %X :: ('Bounded' i, 'Integral' i) => i
--
-- %a, %A :: 'RealFloat' f => f
-- %e, %E :: 'RealFloat' f => f
-- %f, %F :: 'RealFloat' f => f
-- %g, %G :: 'RealFloat' f => f
--
-- %p     :: 'Foreign.Ptr.Ptr' a
-- @
s :: QuasiQuoter
s =
    QuasiQuoter
        { quoteExp =
              \s' ->
                  case parseStr s' of
                      Left x -> error $ show x
                      Right (y, warns) -> do
                          mapM_ (qReport False) (concat warns)
                          (lhss, rhss) <- unzip <$> mapM extractExpr y
                          let rhss' = foldr1 (\x y' -> infixApp x [|(<>)|] y') rhss
                          lamE (map varP $ concat lhss) rhss'
        , quotePat = error "this quoter cannot be used in a pattern context"
        , quoteType = error "this quoter cannot be used in a type context"
        , quoteDec = error "this quoter cannot be used in a declaration context"
        }

extractExpr :: Atom -> Q ([Name], ExpQ)
extractExpr (Str s') = return ([], [|fromString $(stringE s')|])
extractExpr (Arg (FormatArg flags' width' precision' spec' lengthSpec')) = do
    (warg, wexp) <- extractArgs width'
    (parg, pexp) <- extractArgs precision'
    varg <- newName "arg"
    return
        ( catMaybes [warg, parg, Just varg]
        , appE
              [|formatOne|]
              (appE
                   formatter
                   [|PrintfArg
                         { flagSet = $(lift flags')
                         , width = fmap (fromInteger . fromIntegral) $(wexp)
                         , prec = fmap (fromInteger . fromIntegral) $(pexp)
                         , value = $(varE varg)
                         , lengthSpec = $(lift lengthSpec')
                         , fieldSpec = $(lift spec')
                         }|]))
  where
    extractArgs n =
        case n of
            Just Need -> do
                a <- newName "arg"
                pure (Just a, [|Just $(varE a)|])
            Just (Given n') -> pure (Nothing, [|Just $(litE $ integerL n')|])
            Nothing -> pure (Nothing, [|Nothing|])
    formatter =
        case spec' of
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
            _ -> undefined
