{-# LANGUAGE TemplateHaskell #-}

module Text.Printf.TH where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Parsec                    ( parse )
import           Data.Maybe
import           Data.Semigroup                 ( (<>) )

import           Text.Printf.TH.Builder
import           Text.Printf.TH.Parse
import           Text.Printf.TH.Parse.Rules     ( applyAll )
import           Text.Printf.TH.Parse.Flags
import qualified Text.Printf.TH.Layouters      as L
import qualified Text.Printf.TH.Layouters.Float
                                               as FL

data OutputType = OutputString | OutputText

s = QuasiQuoter
  { quoteExp  = \st -> do
                  (lhs, rhs) <- toSplices st OutputString
                  return $ LamE lhs rhs
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

toSplices input otype = case parse fmtString "" input of
  Left  x      -> fail $ show x
  Right atoms' -> do
    atoms        <- applyAll atoms'
    (lhss, rhss) <- unzip <$> mapM extract atoms
    let parts = foldr1 (\x y' -> infixApp x [|(Data.Semigroup.<>)|] y') rhss
    rhss' <- [|finalize $(sigE parts output)|]
    return (map VarP $ concat lhss, rhss')
 where
  output = case otype of
    OutputString -> [t|Str|]
    OutputText   -> [t|()|]

extract (Plain splain) = pure ([], [|str $(stringE splain)|])
extract (Spec FormatSpec { fSpec = Percent }) = pure ([], [|char '%'|])
extract (Spec (FormatSpec sp _ flagSet' width prec)) = do
  (warg, wexp) <- extractArg width "width"
  (parg, pexp) <- extractArg prec "prec"
  varg         <- newName "var"
  let flags = mkFlags flagSet'
  return
    ( catMaybes [warg, parg, Just varg]
    , [|$(varE formatter) $(wexp) $(pexp) flags $(varE varg)|]
    )
 where
  extractArg (Just Needed) name = do
    var <- newName name
    pure (Just var, [|Just (fromIntegral $(varE var))|])
  extractArg (Just (Given n)) _ = pure (Nothing, [|Just n|])
  extractArg Nothing          _ = pure (Nothing, [|Nothing|])
  formatter = case sp of
    Signed         -> 'L.signed
    Unsigned       -> 'L.unsigned
    Hex Upper      -> 'L.hexUpper
    Hex Lower      -> 'L.hexLower
    Octal          -> 'L.octal
    Float    _     -> 'FL.fixed
    Sci      _     -> 'FL.sci
    Generic  _     -> 'FL.generic
    HexFloat Upper -> 'FL.hexUpper
    HexFloat Lower -> 'FL.hexLower
    String         -> 'L.string
    Char           -> 'L.chr
    StrictText     -> 'L.strictText
    LazyText       -> 'L.lazyText
    Showable       -> 'L.shown
    Ptr            -> 'L.ptr
    Percent        -> undefined
