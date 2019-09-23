{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Printf.TH.Builder where

import           Data.String
import qualified Data.DList                    as D

class Monoid a => Builder a where
  type Output a :: *

  finalize :: a -> Output a

  str :: String -> a
  default str :: IsString a => String -> a
  str = fromString

  char :: Char -> a
  default char :: IsString a => Char -> a
  char = str . return

  times :: Int -> Char -> a
  default times :: IsString a => Int -> Char -> a
  times n c = str (replicate n c)

  cons :: Char -> a -> a
  cons c b = char c <> b

  size :: a -> Int

newtype Str = Str { unStr :: (D.DList Char, Int) } deriving Show

instance Semigroup Str where
  Str (a, b) <> Str (c, d) = Str (a <> c, b + d)

instance Monoid Str where
  mempty = Str (mempty, 0)

instance Builder Str where
  type Output Str = String
  finalize (Str (l, _)) = D.toList l

  str s = Str (D.fromList s, length s)
  char c = Str (D.singleton c, 1)
  times n c = Str (D.replicate n c, n)
  cons c (Str (l, n)) = Str (D.cons c l, n + 1)

  size = snd . unStr

justifyLeft Nothing b = b
justifyLeft (Just len) b | size b >= len = b
                         | otherwise     = b <> times (len - size b) ' '

justifyRight Nothing b _ = b
justifyRight (Just len) b fill | size b >= len = b
                               | otherwise     = times (len - size b) fill <> b

build f = foldr (cons . f) mempty
