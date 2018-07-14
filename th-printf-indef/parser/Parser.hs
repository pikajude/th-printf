{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser where

import qualified Data.Set as S

import Control.Monad.Fix
import Control.Monad.RWS
import Data.Char
import Data.CharSet hiding (map)
import Data.Coerce
import Data.Maybe
import Lens.Micro.Platform
import Parser.Types
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Read.Lex (lexChar)

type Warning = String

parseStr :: String -> Either ParseError ([Atom], [[Warning]])
parseStr = fmap (unzip . map normalizeAndWarn) . parse printfStr "" . lexChars
  where
    lexChars x =
        (`fix` x) $ \f s ->
            if Prelude.null s
                then []
                else case readP_to_S lexChar s of
                         ((c, rest):_) -> c : f rest
                         [] -> error "malformed input"

normalizeAndWarn s@Str {} = (s, [])
normalizeAndWarn (Arg f) = (Arg a, b)
  where
    (_, a, b) = runRWS @_ @[Warning] (go (spec f)) () f
    go c
        | c `elem` "aAeEfFgGxXo" = return ()
    go c
        | c `elem` "cs?" = warnSign >> warnPrefix >> warnZero
    go c
        | c `elem` "diu" = warnPrefix
    go 'p' = warnSign >> warnPrefix >> warnZero
    warnFlag ::
           (Eq a, MonadWriter [String] m, MonadState FormatArg m)
        => Lens' FlagSet a
        -> a
        -> a
        -> Char
        -> m ()
    warnFlag lens bad good flagName = do
        oldVal <- use (flags_ . lens)
        when (oldVal == bad) $ do
            c <- use spec_
            flags_ . lens .= good
            tell
                ["`" ++ [flagName] ++ "` flag has no effect on `" ++ [c] ++ "` specifier"]
    warnSign = warnFlag signed_ True False '+'
    warnPrefix = warnFlag prefixed_ True False '#'
    warnZero = warnFlag adjustment_ (Just ZeroPadded) Nothing '0'

flagSet = fromList "-+ #0"

specSet = fromList "diuoxXfFeEaAgGpcs?"

lengthSpecifiers =
    [ ("hh", DoubleH)
    , ("h", H)
    , ("ll", DoubleL)
    , ("l", L)
    , ("j", J)
    , ("z", Z)
    , ("t", T)
    , ("L", BigL)
    ]

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
                 optionMaybe $ choice [Given <$> nat, Need <$ char '*'])) <?>
        "precision"
    lengthSpec <-
        optionMaybe $ choice $ Prelude.map (\(a, b) -> b <$ string a) lengthSpecifiers
    spec <- oneOfSet specSet <?> "valid specifier"
    pure $ FormatArg flags width (fromMaybe (Given 0) <$> precision) spec lengthSpec

nat = do
    c <- many1 $ satisfy isDigit
    return $ read @Integer c
