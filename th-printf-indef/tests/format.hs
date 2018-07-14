{-# LANGUAGE QuasiQuotes #-}

module Main where

import GeneratedSpec
import Language.Haskell.Printf
import Language.Haskell.TH.Quote
import Test.HUnit
import Test.Hspec

main :: IO ()
main =
    hspec $
    describe "th-printf" $ do
        GeneratedSpec.spec
        it "hexadecimal float" $ do
            [s|%a|] 0.857421875 @?= "0x1.b7p-1"
            [s|%A|] 3.1415926 @?= "0X1.921FB4D12D84AP+1"
            [s|%.3a|] 1.999999999 @?= "0x2.000p+0"
            [s|%.0a|] 1.999999999 @?= "0x2p+0"
            [s|%#.0a|] 1.999999999 @?= "0x2.p+0"
            [s|%.3a|] 0.7576 @?= "0x1.83ep-1"
            [s|%015.3a|] 0.7576 @?= "0x000001.83ep-1"
            [s|% 15.3a|] 0.7576 @?= "     0x1.83ep-1"
