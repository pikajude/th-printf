module Text.Printf.TH.Layout where

import           Data.Maybe                     ( fromMaybe )

import qualified Text.Printf.TH.Parse.Flags    as F
import           Text.Printf.TH.Builder

layout :: Builder buf => Maybe Int -> Maybe buf -> Maybe F.Justify -> buf -> buf
layout w p j buf = case j of
  Nothing -> case w of
    Just n | n < 0 -> justifyLeft (Just (-n)) alt0
    w_             -> justifyRight w_ ' ' alt0
  Just F.ZeroFill    -> b0 <> justifyRight (subtract s0 <$> w) '0' buf
  Just F.LeftJustify -> justifyLeft w alt0
 where
  alt0 = b0 <> buf
  b0   = fromMaybe mempty p
  s0   = size b0

signedPrefix flags d | d < 0         = Just (char '-')
                     | F.sign flags  = Just (char '+')
                     | F.space flags = Just (char ' ')
                     | otherwise     = Nothing
