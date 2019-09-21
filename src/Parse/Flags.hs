{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Parse.Flags where

import qualified Parse                         as P
import           Language.Haskell.TH.Lift
import           Data.Set                       ( Set )
import qualified Data.Set                      as S

data Justify = LeftJustify | ZeroFill deriving (Show, Eq, Ord)

data Flags = Flags
  { justify :: Maybe Justify
  , sign :: Bool
  , space :: Bool
  , prefix :: Bool
  } deriving (Show, Eq, Ord)

deriveLiftMany [''Justify, ''Flags]

mkFlags :: Set P.Flag -> Flags
mkFlags fs = Flags
  { sign    = P.AlwaysSign `S.member` fs
  , prefix  = P.Prefix `S.member` fs
  , space   = P.SpacePad `S.member` fs && P.AlwaysSign `S.notMember` fs
  , justify = if
                | P.LeftJustify `S.member` fs -> Just LeftJustify
                | P.ZeroFill `S.member` fs    -> Just ZeroFill
                | otherwise                   -> Nothing
  }
