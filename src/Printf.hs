{-# LANGUAGE FlexibleContexts #-}

module Printf where

import           Text.ParserCombinators.Parsec.Char
import           Text.ParserCombinators.Parsec.Prim
import           Control.Applicative            ( some )

data Atom = FormatSpec Char | Plain String deriving (Show, Eq)

fmtString = many ((FormatSpec <$> fmtSpec) <|> (Plain <$> some plainChar))
 where
  plainChar = satisfy (/= '%')
  fmtSpec   = do
    _ <- char '%'
    (char 's' <?> "string") <|> (char '%' <?> "escaped percent")
