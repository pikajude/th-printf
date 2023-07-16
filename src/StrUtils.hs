module StrUtils (justifyLeft, justifyRight) where

import Buildable

justifyLeft :: (Buf a) => Int -> Char -> a -> a
justifyLeft n c s
  | diff <= 0 = s
  | otherwise = s <> repeatN diff c
 where
  diff = n - size s

justifyRight :: (Buf a) => Int -> Char -> a -> a
justifyRight n c s
  | diff <= 0 = s
  | otherwise = repeatN diff c <> s
 where
  diff = n - size s
