{-# Language NoMonomorphismRestriction #-}

module Text.Printf.TH.Parser where

import Control.Applicative
import Data.CharSet (fromList)
import Text.Trifecta
import qualified Data.Set as S

import Text.Printf.TH.Types

flagSet = fromList "-+ #0"

specSet = fromList "diuoxXfFeEgGcs"

parseFmtStr = do
    atoms <-
        many $
        choice [Str "%" <$ string "%%", Arg <$> fmtArg, Str . return <$> noneOf "%"]
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
    width <- optional $ choice [Given <$> natural, Need <$ char '*'] <?> "width"
    precision <-
        optional $
        (do char '.'
            choice [Given <$> natural, Need <$ char '*']) <?>
        "precision"
    spec <- oneOfSet specSet <?> "valid specifier"
    pure $ FormatArg flags width precision spec
