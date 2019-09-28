{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Text.Printf.TH.Parse.Rules where

import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.RWS
import           Data.Maybe
import           Lens.Micro.Platform
import           Text.Printf                    ( printf )
import           Language.Haskell.TH            ( reportWarning )

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

data FormatSpec
  = FormatSpec
    { fSpec :: Spec
    , fSpecChar :: Char -- only for error reporting
    , fFlags :: Set Flag
    , fWidth :: Maybe (Variable Int)
    , fPrecision :: Maybe (Variable Int)
    } deriving (Show, Eq)

data Atom
  = Spec FormatSpec
  | Plain String
  deriving (Show, Eq)

makeLensesFor
  [ ("fSpec", "_spec")
  , ("fSpecChar", "_specChar")
  , ("fFlags", "_flags")
  , ("fWidth", "_width")
  , ("fPrecision", "_precision")
  ] ''FormatSpec

isDecimal Signed   = True
isDecimal Unsigned = True
isDecimal Octal    = True
isDecimal (Hex _)  = True
isDecimal _        = False

rules :: (MonadState FormatSpec m, MonadWriter [String] m) => m ()
rules = do
  FormatSpec { fSpec = sp, fSpecChar = sc, fFlags = fl, fPrecision = prec } <-
    get
  when (isDecimal sp) $ when (S.member ZeroFill fl && isJust prec) $ do
    tell
      [ printf
          "`0` flag has no effect when combined with precision on `%c` specifier"
          sc
      ]
    _flags %= S.delete ZeroFill

applyRules = execRWS rules ()

applyAll = mapM $ \case
  p@Plain{} -> pure p
  Spec f    -> do
    let (newSpec, warns) = applyRules f
    mapM_ reportWarning warns
    pure (Spec newSpec)
