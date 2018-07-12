{-# LANGUAGE QuasiQuotes #-}

module Main where

import Language.Haskell.Printf
import Language.Haskell.TH.Quote
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

str :: Gen String
str = do
    n <- elements [7 .. 20]
    vectorOf n $ elements ['\0' .. '\65536']

main :: IO ()
main =
    hspec $
    describe "th-printf" $ do
        describe "string" $ do
            it "basic" $ do
                [s|\1234|] @?= "\1234"
                [s|\12\&34|] @?= "\12\&34"
                [s|%s|] "foo" @?= "foo"
                [s|%10s|] "foo" @?= "       foo"
                [s|%10s|] "fübar" @?= "     fübar"
                [s|%-10s|] "foo" @?= "foo       "
                [s|%010s|] "foo" @?= "0000000foo"
            it "precision" $ forAll str $ \n -> [s|%.10s|] n == [s|%s|] n
            it "prefix" $ forAll str $ \n -> [s|%#s|] n == [s|%#s|] n
        describe "char" $ do
            it "basic" $ do
                [s|%c|] 'U' @?= "U"
                [s|%c|] '\65210' @?= "\65210"
                [s|%5c|] '\65210' @?= "    \65210"
        describe "decimal" $ do
            it "basic" $ do
                [s|%d|] 20 @?= "20"
                [s|%d|] (negate 10) @?= "-10"
                [s|%10d|] 20 @?= "        20"
                [s|%10d|] (negate 10) @?= "       -10"
                [s|%*d|] 10 20 @?= "        20"
                [s|%*d|] 10 (negate 10) @?= "       -10"
                [s|%010d|] 20 @?= "0000000020"
                [s|%010d|] (negate 10) @?= "-000000010"
                [s|% d|] 20 @?= " 20"
                [s|% d|] (negate 10) @?= "-10"
                [s|%+d|] 20 @?= "+20"
                [s|%+10d|] 20 @?= "       +20"
                [s|%-10d|] 20 @?= "20        "
                [s|%-10d|] (negate 10) @?= "-10       "
                [s|%+-10d|] 20 @?= "+20       "
        describe "unsigned" $ it "basic" $ [s|%u|] 20 @?= "20"
