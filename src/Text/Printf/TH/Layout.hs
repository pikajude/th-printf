module Text.Printf.TH.Layout where

import           Control.Monad                  ( guard )
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

hideZero (Just 0) 0 _ = mempty
hideZero _        _ b = b

signedPrefix flags d | d < 0         = Just (char '-')
                     | F.sign flags  = Just (char '+')
                     | F.space flags = Just (char ' ')
                     | otherwise     = Nothing

hexPrefix upper flags d = do
  guard (F.prefix flags && d /= 0)
  Just $ str (if upper then "0X" else "0x")
