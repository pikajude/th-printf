{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Fix
import Control.Monad.RWS
import Data.Char
import Data.CharSet hiding (map)
import Data.Maybe
import qualified Data.Set as S
import Lens.Micro.Platform
import Parser.Types
import Text.Parsec hiding (many)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Read.Lex (lexChar)

type Warning = String

parseStr :: String -> Either ParseError ([Atom], [[Warning]])
parseStr = fmap (unzip . map normalizeAndWarn) . parse printfStr "" . lexChars
 where
  lexChars x = (`fix` x) $ \f s ->
    if Prelude.null s
      then []
      else case readP_to_S lexChar s of
        ((c, rest) : _) -> c : f rest
        [] -> error "malformed input"

normalizeAndWarn :: Atom -> (Atom, [Warning])
normalizeAndWarn s@Str{} = (s, [])
normalizeAndWarn (Arg f) = (Arg a, b)
 where
  (_, a, b) = runRWS (warnLength f >> go (spec f)) () f
  go c | c `elem` "aAeEfFgGxXo" = return ()
  go c | c `elem` "csqQ?" = warnSign >> warnPrefix >> warnZero >> warnSpace
  go c | c `elem` "diu" = warnPrefix
  go 'p' = warnSign >> warnPrefix >> warnZero
  go _ = undefined
  warnFlag ::
    (Eq a, MonadWriter [String] m, MonadState FormatArg m) =>
    Lens' FlagSet a ->
    a ->
    a ->
    Char ->
    m ()
  warnFlag lens' bad good flagName = do
    oldVal <- use (flags_ . lens')
    when (oldVal == bad) $ do
      c <- use spec_
      flags_ . lens' .= good
      tell
        [ "`"
            ++ [flagName]
            ++ "` flag has no effect on `"
            ++ [c]
            ++ "` specifier"
        ]
  warnSign = warnFlag signed_ True False '+'
  warnPrefix = warnFlag prefixed_ True False '#'
  warnSpace = warnFlag spaced_ True False ' '
  warnZero = warnFlag adjustment_ (Just ZeroPadded) Nothing '0'
  phonyLengthSpec =
    S.fromList $
      [(x, y) | x <- "diuoxX", y <- ["L"]]
        ++ [ (x, y)
           | x <- "fFeEgGaA"
           , y <- ["hh", "h", "l", "ll", "j", "z", "t"]
           ]
        ++ [(x, y) | x <- "csqQ", y <- ["hh", "h", "ll", "j", "z", "t", "L"]]
        ++ map ('p',) ["hh", "h", "l", "ll", "j", "z", "t", "L"]
  warnLength FormatArg{spec, lengthSpec = Just l}
    | (spec, show l) `S.member` phonyLengthSpec =
        tell
          [ "`"
              ++ show l
              ++ "` length modifier has no effect when combined with `"
              ++ [spec]
              ++ "` specifier"
          ]
  warnLength _ = return ()

flagSet :: CharSet
flagSet = fromList "-+ #0"

specSet :: CharSet
specSet = fromList "diuoxXfFeEaAgGpcsQq?"

lengthSpecifiers :: [(String, LengthSpecifier)]
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

oneOfSet :: Stream s m Char => CharSet -> ParsecT s u m Char
oneOfSet s = satisfy (`member` s)

printfStr :: Stream s m Char => ParsecT s u m [Atom]
printfStr = do
  atoms <-
    many $
      choice
        [Str "%" <$ try (string "%%"), Arg <$> fmtArg, Str . return <$> noneOf "%"]
  return $ go atoms
 where
  go (Str s : Str s1 : as) = go (Str (s ++ s1) : as)
  go (a : as) = a : go as
  go [] = []

fmtArg :: Stream s m Char => ParsecT s u m FormatArg
fmtArg = do
  char '%'
  flags <- do
    fs <- many $ do
      c <- oneOfSet flagSet <?> "flag"
      pure $ case c of
        '-' -> FlagLJust
        '+' -> FlagSigned
        ' ' -> FlagSpaced
        '#' -> FlagPrefixed
        '0' -> FlagZeroPadded
        _ -> error "???"
    let flagSet' = S.fromList fs
    if S.size flagSet' < length fs
      then fail "Duplicate flags specified"
      else pure $ toFlagSet flagSet'
  width <- optionMaybe (choice [Given <$> nat, Need <$ char '*']) <?> "width"
  precision <-
    optionMaybe
      ( do
          char '.'
          optionMaybe $ choice [Given <$> nat, Need <$ char '*']
      )
      <?> "precision"
  lengthSpec <-
    optionMaybe $
      choice $
        Prelude.map
          (\(a, b) -> b <$ string a)
          lengthSpecifiers
  spec <- oneOfSet specSet <?> "valid specifier"
  pure $
    FormatArg
      flags
      width
      (fromMaybe (Given 0) <$> precision)
      spec
      lengthSpec
 where
  nat = do
    c <- many1 $ satisfy isDigit
    return (read c :: Integer)
