module StrUtils where

justifyLeft :: Int -> Char -> String -> String
justifyLeft n c s
    | diff <= 0 = s
    | otherwise = s ++ replicate diff c
  where
    diff = n - length s

justifyRight :: Int -> Char -> String -> String
justifyRight n c s
    | diff <= 0 = s
    | otherwise = replicate diff c ++ s
  where
    diff = n - length s
