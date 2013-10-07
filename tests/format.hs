{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Text.Printf.TH

main :: IO ()
main = hspec $
    describe "string substitution" $ do
        it "inserts strings of different types" $
            [s|%s %s %s %s %s %s|] "foo"
                                   (T.pack "foo")
                                   (LT.pack "foo")
                                   (B.pack "foo")
                                   (LB.pack "foo")
                                   [102 :: Int, 111, 111]
              @?= ("foo foo foo foo foo foo" :: String)

        it "pads" $ do
            [s|%10s|] "foo"  @?= "       foo"
            [s|%010s|] "foo" @?= "0000000foo"
            [s|%*s|] 5 "foo" @?= "  foo"

        it "ignores magic hash" $
            forAll str $ \n -> [s|%#s|] n == [s|%s|] n

        it "ignores precision" $
            forAll str $ \n -> [s|%.10s|] n == [s|%s|] n

str :: Gen String
str = do
    n <- elements [7..20]
    vectorOf n (elements ['A'..'z'])
