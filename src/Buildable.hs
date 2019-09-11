{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Buildable where

import           Data.String
import qualified Data.DList                    as D
import           Data.Char                      ( intToDigit )
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy.Builder        as T
import qualified Data.Text.Lazy.Builder.Int    as T

newtype Sized a = Sized { unSized :: (a, Int) } deriving (Show, Ord, Eq)

type SizedList a = Sized (D.DList a)
type SizedBuilder = Sized T.Builder

instance IsString a => IsString (Sized a) where
  fromString s = Sized (fromString s, length s)

instance Semigroup a => Semigroup (Sized a) where
  Sized (a, b) <> Sized (c, d) = Sized (a <> c, b + d)

instance Monoid a => Monoid (Sized a) where
  mempty = Sized (mempty, 0)

class Monoid a => Buildable a where
  type Output a :: *

  str :: String -> a

  singleton :: Char -> a
  digit :: Int -> a
  digit = singleton . intToDigit
  {-# INLINE digit #-}

  cons :: Char -> a -> a
  cons c s = singleton c <> s
  {-# INLINE cons #-}

  repeatN :: Int -> Char -> a
  repeatN n = str . replicate n

  size :: a -> Int

  finalize :: a -> Output a

instance Buildable (SizedList Char) where
  type Output (SizedList Char) = String
  str a = Sized (D.fromList a, length a)
  singleton c = Sized (D.singleton c, 1)
  finalize = D.toList . fst . unSized
  cons c (Sized (r, m)) = Sized (D.cons c r, m + 1)
  repeatN n c = Sized (D.replicate n c, n)
  size = snd . unSized

instance Buildable SizedBuilder where
  type Output SizedBuilder = Text
  str a = Sized (fromString a, length a)
  singleton c = Sized (T.singleton c, 1)
  digit c = Sized (T.hexadecimal c, 1)
  finalize = T.toLazyText . fst . unSized
  size     = snd . unSized
