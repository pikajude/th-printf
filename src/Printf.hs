{-# LANGUAGE FlexibleContexts #-}

module Printf where

import           Text.ParserCombinators.Parsec.Char
import           Text.ParserCombinators.Parsec.Prim
import           Text.Parsec.Combinator
import           Control.Applicative            ( some )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import qualified Data.IntMap                   as I
import qualified Data.IntSet                   as IS
import           Data.CharSet
import           Data.Char

data Atom
  = FormatSpec
    { fSpec :: Spec
    , fFlags :: Set Flag
    }
  | Plain String
  deriving (Show, Eq)

data Case
  = Lower
  | Upper
  deriving (Show, Eq, Ord, Bounded, Enum)

data Flag
  = LeftJustify
  | AlwaysSign
  | SpacePad
  | Prefix
  | ZeroFill
  deriving (Show, Eq, Ord, Bounded, Enum)

data Spec
  = Signed
  | Unsigned
  | Octal
  | Hex Case
  | Float Case
  | Sci Case
  | Generic Case
  | HexFloat Case
  | Char
  | String
  | Ptr
  | Percent
  -- our special format args
  | LazyText
  | StrictText
  | Showable
  deriving (Show, Eq, Ord)

fmtString = many (fmtSpec <|> (Plain <$> some plainChar))
 where
  plainChar = satisfy (/= '%')
  fmtSpec   = do
    _     <- char '%'
    flags <- many $ choice
      [ LeftJustify <$ char '-'
      , AlwaysSign <$ char '+'
      , SpacePad <$ char ' '
      , Prefix <$ char '#'
      , ZeroFill <$ char '0'
      ]
    specChar <- satisfy (\u -> IS.member (ord u) specChars)
    return FormatSpec { fSpec  = specMap I.! ord specChar
                      , fFlags = S.fromList flags
                      }

specChars = I.keysSet specMap

specMap = I.fromList
  [ (ord x, y)
  | (x, y) <-
    [ ('a', HexFloat Lower)
    , ('A', HexFloat Upper)
    , ('c', Char)
    , ('d', Signed)
    , ('e', Sci Lower)
    , ('E', Sci Upper)
    , ('f', Float Lower)
    , ('F', Float Upper)
    , ('g', Generic Lower)
    , ('G', Generic Upper)
    , ('i', Signed)
    , ('p', Ptr)
    , ('q', LazyText)
    , ('Q', StrictText)
    , ('s', String)
    , ('u', Unsigned)
    , ('x', Hex Lower)
    , ('X', Hex Upper)
    , ('?', Showable)
    , ('%', Percent)
    ]
  ]
