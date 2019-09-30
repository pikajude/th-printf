{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE MultiWayIf #-}

module Text.Printf.TH.Parse.Flags where

import           Control.Monad                  ( guard )
import           Language.Haskell.TH.Syntax
import           Data.Set                       ( Set )
import qualified Data.Set                      as S

import qualified Text.Printf.TH.Parse          as P

data Justify = LeftJustify | ZeroFill deriving (Show, Eq, Ord, Lift)

data Flags = Flags
  { justify :: Maybe Justify
  , sign :: Bool
  , space :: Bool
  , prefix :: Bool
  } deriving (Show, Eq, Ord, Lift)

noZero Flags { justify = j } = do
  just <- j
  guard (just /= ZeroFill)
  pure just

mkFlags :: Set P.Flag -> Flags
mkFlags fs = Flags
  { sign    = P.AlwaysSign ∈ fs
  , prefix  = P.Prefix ∈ fs
  , space   = P.SpacePad ∈ fs && P.AlwaysSign ∉ fs
  , justify = if
                | P.LeftJustify ∈ fs -> Just LeftJustify
                | P.ZeroFill ∈ fs    -> Just ZeroFill
                | otherwise          -> Nothing
  }

(∈) = S.member
infixr 8 ∈

(∉) = S.notMember
infixr 8 ∉
