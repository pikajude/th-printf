{-# LANGUAGE TemplateHaskell #-}

module Text.Printf.TH where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Parsec                    ( parse )
import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Data.Semigroup                 ( (<>) )

import           Text.Printf.TH.Builder
import qualified Text.Printf.TH.Print          as P
import           Text.Printf.TH.Parse
import           Text.Printf.TH.Parse.Flags

data OutputType = OutputString | OutputText

s = QuasiQuoter
  { quoteExp  = \s -> do
                  (lhs, rhs) <- toSplices s OutputString
                  return $ LamE lhs rhs
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

toSplices s otype = case parse fmtString "" s of
  Left  x     -> fail $ show x
  Right atoms -> do
    (lhss, rhss) <- unzip <$> mapM extract atoms
    let parts = foldr1 (\x y' -> infixApp x [|(Data.Semigroup.<>)|] y') rhss
    rhss' <- [|finalize $(sigE parts output)|]
    return (map VarP $ concat lhss, rhss')
 where
  output = case otype of
    OutputString -> [t|Str|]
    OutputText   -> [t|()|]

extract (Plain s)                          = pure ([], [|str $(stringE s)|])
extract FormatSpec { fSpec = Percent }     = pure ([], [|char '%'|])
extract (FormatSpec sp flagSet width prec) = do
  (warg, wexp) <- extractArg width "width"
  (parg, pexp) <- extractArg prec "prec"
  varg         <- newName "var"
  let flags = mkFlags flagSet
  return
    ( catMaybes [warg, parg, Just varg]
    , [|$(varE formatter) flags $(wexp) $(pexp) $(varE varg)|]
    )
 where
  extractArg (Just Needed) name = do
    var <- newName name
    pure (Just var, [|Just (fromIntegral $(varE var))|])
  extractArg (Just (Given n)) _ = pure (Nothing, [|Just n|])
  extractArg Nothing          _ = pure (Nothing, [|Nothing|])
  formatter = case sp of
    String   -> 'P.printString
    Char     -> 'P.printChar
    Signed   -> 'P.printSigned
    Showable -> 'P.printShow
    Float _  -> 'P.printFixed
    _        -> 'P.printAny
