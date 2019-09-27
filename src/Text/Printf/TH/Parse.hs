{-# LANGUAGE FlexibleContexts #-}

module Text.Printf.TH.Parse
  ( module Text.Printf.TH.Parse
  , module Text.Printf.TH.Parse.Rules
  )
where

import           Text.ParserCombinators.Parsec.Char
import           Text.ParserCombinators.Parsec.Prim
import           Text.Parsec.Combinator
import           Control.Applicative            ( some )
import qualified Data.Set                      as S
import qualified Data.IntMap                   as I
import           Data.Char
import           Data.Functor                   ( void )

import           Text.Printf.TH.Parse.Charset
import           Text.Printf.TH.Parse.Rules

fmtString = many $ fmtSpec <|> Plain <$> some plainChar
  where plainChar = satisfy (/= '%')

fmtSpec = do
  _     <- char '%'
  flags <- many (oneOfSet flagSet) <?> "flags"
  width <- optionMaybe (var False) <?> "width specifier"
  prec  <-
    optionMaybe (char '.' *> (try (var True) <|> pure (Given 0)))
      <?> "precision specifier"
  optional (void (try $ string "hh" <|> string "ll") <|> oneOfSet lengthSet)
    <?> "length specifier"
  spec <- oneOfSet specSet <?> "format specifier"
  return FormatSpec { fSpec      = spec
                    , fFlags     = S.fromList flags
                    , fWidth     = width
                    , fPrecision = prec
                    }
 where
  var allowLeadingZero = Needed <$ char '*' <|> Given <$> nat allowLeadingZero

nat leadingZero = do
  chars <- if leadingZero
    then some (satisfy isDigit)
    else (:) <$> satisfy (\c -> c >= '1' && c <= '9') <*> many (satisfy isDigit)
  return $ valSimple 10 $ map digitToInt chars

-- stolen from base
valSimple :: (Num a, Integral d) => a -> [d] -> a
valSimple base = go 0 where
  go r []       = r
  go r (d : ds) = r' `seq` go r' ds where r' = r * base + fromIntegral d

specSet = I.fromAscList
  [ (ord x, y)
  | (x, y) <-
    [ ('%', Percent)
    , ('?', Showable)
    , ('A', HexFloat Upper)
    , ('E', Sci Upper)
    , ('F', Float Upper)
    , ('G', Generic Upper)
    , ('Q', StrictText)
    , ('X', Hex Upper)
    , ('a', HexFloat Lower)
    , ('c', Char)
    , ('d', Signed)
    , ('e', Sci Lower)
    , ('f', Float Lower)
    , ('g', Generic Lower)
    , ('i', Signed)
    , ('o', Octal)
    , ('p', Ptr)
    , ('q', LazyText)
    , ('s', String)
    , ('u', Unsigned)
    , ('x', Hex Lower)
    ]
  ]

flagSet = I.fromAscList
  [ (ord x, y)
  | (x, y) <-
    [ (' ', SpacePad)
    , ('#', Prefix)
    , ('+', AlwaysSign)
    , ('-', LeftJustify)
    , ('0', ZeroFill)
    ]
  ]

lengthSet = I.fromList [ (ord x, ()) | x <- "hljztL" ]
