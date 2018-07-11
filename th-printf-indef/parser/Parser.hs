{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser where

import qualified Data.Set as S

import Data.CharSet
import Parser.Types
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token

parseStr = parse printfStr

flagSet = fromList "-+ #0"

specSet = fromList "diuoxXfFeEaAgGpcst?"

oneOfSet s = satisfy (`member` s)

printfStr = do
    atoms <-
        many $
        choice [Str "%" <$ try (string "%%"), Arg <$> fmtArg, Str . return <$> noneOf "%"]
    return $ go atoms
  where
    go (Str s:Str s1:as) = go (Str (s ++ s1) : as)
    go (a:as) = a : go as
    go [] = []

fmtArg = do
    char '%'
    flags <-
        do fs <-
               many $ do
                   c <- oneOfSet flagSet <?> "flag"
                   pure $
                       case c of
                           '-' -> FlagLJust
                           '+' -> FlagSigned
                           ' ' -> FlagSpaced
                           '#' -> FlagPrefixed
                           '0' -> FlagZeroPadded
                           _ -> error "???"
           let flagSet = S.fromList fs
           if S.size flagSet < length fs
               then fail "Duplicate flags specified"
               else pure $ toFlagSet flagSet
    width <- optionMaybe (choice [Given <$> nat, Need <$ char '*']) <?> "width"
    precision <-
        optionMaybe
            ((do char '.'
                 choice [Given <$> nat, Need <$ char '*'])) <?>
        "precision"
    spec <- oneOfSet specSet <?> "valid specifier"
    pure $ FormatArg flags width precision spec

TokenParser {natural = nat} = makeTokenParser emptyDef