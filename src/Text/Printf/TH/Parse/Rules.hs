{-# LANGUAGE FlexibleContexts #-}

module Text.Printf.TH.Parse.Rules where

import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Maybe

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

data Variable a
  = Given a
  | Needed
  deriving (Show, Eq, Ord)

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

data Atom
  = FormatSpec
    { fSpec :: Spec
    , fFlags :: Set Flag
    , fWidth :: Maybe (Variable Int)
    , fPrecision :: Maybe (Variable Int)
    }
  | Plain String
  deriving (Show, Eq)

isDecimal Signed   = True
isDecimal Unsigned = True
isDecimal Octal    = True
isDecimal (Hex _)  = True
isDecimal _        = False

applyRules = do
  s <- get
  when
      (isDecimal (fSpec s) && S.member ZeroFill (fFlags s) && isJust
        (fPrecision s)
      )
    $ do
        tell "That's wrong"
        modify (\st -> st { fFlags = S.delete ZeroFill (fFlags st) })
