{-# LANGUAGE FlexibleContexts #-}
-- helpers for when a variety of single characters map to different values
module Parse.Charset where

import qualified Data.IntSet                   as IS
import qualified Data.IntMap                   as I
import           Text.Parsec.Char               ( satisfy )
import           Data.Char                      ( ord )

oneOfSet c = do
  ch <- satisfy (\u -> IS.member (ord u) keyset)
  return $ c I.! ord ch
  where keyset = I.keysSet c
