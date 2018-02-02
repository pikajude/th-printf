{-# Language TemplateHaskell #-}

module Codegen
    ( gen
    , str
    ) where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Name)
import Test.QuickCheck
import Test.Hspec
import Test.HUnit.Base

str :: Gen String
str = do
    n <- elements [7 .. 20]
    vectorOf n (elements ['A' .. 'z'])

gen :: String -> Name -> QuasiQuoter -> ExpQ
gen name f quoter =
    [|describe $(stringE name) $ do
          describe "string" $ do
              it "basic" $ do
                  $(q "%s") "foo" @?= "foo"
                  $(q "%10s") "foo" @?= "       foo"
                  $(q "%10s") "fübar" @?= $(varE f) "     fübar"
                  $(q "%-10s") "foo" @?= "foo       "
                  $(q "%010s") "foo" @?= "0000000foo"
              it "precision" $ forAll str $ \n -> $(q "%.10s") n == $(q "%s") n
              it "prefix" $ forAll str $ \n -> $(q "%#s") n == $(q "%s") n
          describe "char" $ do
              it "basic" $ do
                  $(q "%c") 'U' @?= "U"
                  $(q "%c") '\65210' @?= $(varE f) "\65210"
                  $(q "%5c") '\65210' @?= $(varE f) "    \65210"
          describe "decimal" $ do
              it "basic" $ do
                  $(q "%d") 20 @?= "20"
                  $(q "%d") (negate 10) @?= "-10"
                  $(q "%10d") 20 @?= "        20"
                  $(q "%10d") (negate 10) @?= "       -10"
                  $(q "%*d") 10 20 @?= "        20"
                  $(q "%*d") 10 (negate 10) @?= "       -10"
                  $(q "%010d") 20 @?= "0000000020"
                  $(q "%010d") (negate 10) @?= "-000000010"
                  $(q "% d") 20 @?= " 20"
                  $(q "% d") (negate 10) @?= "-10"
                  $(q "%+d") 20 @?= "+20"
                  $(q "%+10d") 20 @?= "       +20"
                  $(q "%-10d") 20 @?= "20        "
                  $(q "%-10d") (negate 10) @?= "-10       "
                  $(q "%+-10d") 20 @?= "+20       "
          describe "unsigned" $ do it "basic" $ do $(q "%u") 20 @?= "20"
          describe "octal" $ do
              it "basic" $ do
                  $(q "%o") 1500 @?= "2734"
                  $(q "%o") (negate 1500) @?= "-2734"
              it "prefix" $ do
                  $(q "%#o") 1500 @?= "02734"
                  $(q "%#o") 0 @?= "0"
                  $(q "%#o") (negate 1500) @?= "-02734"
                  $(q "%#010o") 1500 @?= "0000002734"
                  $(q "%#010o") (negate 1500) @?= "-000002734"
                  $(q "%#-10o") (negate 1500) @?= "-02734    "
                  $(q "%#-1o") (negate 1500) @?= "-02734"
          describe "hex" $ do
              it "basic" $ do
                  $(q "%x") 12513024 @?= "beef00"
                  $(q "%X") 12513024 @?= "BEEF00"
              it "prefix" $ do
                  $(q "%#x") 12513024 @?= "0xbeef00"
                  $(q "%#X") 12513024 @?= "0XBEEF00"
                  $(q "%#15x") 12513024 @?= "       0xbeef00"
                  $(q "%#015x") 12513024 @?= "0x0000000beef00"
          describe "float" $ do
              it "basic" $ do
                  $(q "%f") 1234.56 @?= "1234.56"
                  $(q "%f") (negate 1234.56) @?= "-1234.56"
              it "precision" $ do
                  $(q "%.5f") 1234.56 @?= "1234.56000"
                  $(q "%.1f") 1234.56 @?= "1234.6"
                  $(q "%.0f") 1234.56 @?= "1235"
              it "prefix flag" $ do $(q "%#.0f") 1234.56 @?= "1235."
          describe "scientific" $ do
              it "basic" $ do
                  $(q "%e") 1234.56 @?= "1.23456e3"
                  $(q "%e") (negate 1234.56) @?= "-1.23456e3"
                  $(q "%e") 0.00035 @?= "3.5e-4"
                  $(q "%E") 1234.56 @?= "1.23456E3"
              it "precision" $ do
                  $(q "%.5e") 12.34 @?= "1.23400e1"
                  -- GHC's implementation is wrong
                  -- $(q "%.0e") 12.34 @?= "1e1"
                  -- $(q "%#.0e") 12.34 @?= "1.e1"
                  $(q "%15.5e") 12.34 @?= "      1.23400e1"
                  $(q "%+015.5e") 12.34 @?= "+000001.23400e1"
                  $(q "%+-15.5e") 12.34 @?= "+1.23400e1     "
          describe "g-format" $ do
              it "basic" $ do
                  $(q "%g") 1234.56 @?= "1234.56"
                  $(q "%g") 123456789.876 @?= "1.23456789876e8"
                  $(q "%G") 123456789.876 @?= "1.23456789876E8"
                  $(q "%.3g") 1234.56 @?= "1234.560"
                  $(q "%.3g") 123456789.876 @?= "1.235e8"
                  $(q "%.3G") 123456789.876 @?= "1.235E8"
                  $(q "%#g") 1234.56 @?= "1234.56"|]
  where
    q = quoteExp quoter
