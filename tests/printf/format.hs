{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Foreign.Ptr
import           GeneratedSpec
import           Test.HUnit
import           Test.Hspec
import qualified Data.Text                     as S
import qualified Data.Text.Lazy                as L

import           Text.Printf.TH

main :: IO ()
main = hspec $ describe "th-printf" $ do
  GeneratedSpec.spec
  it "text" $ do
    -- sanity checking that text can be rendered
    -- all the actual string formatting is in GeneratedSpec
    [s|Hello, %Q!|] (S.pack "world") @?= "Hello, world!"
    [s|Hello, %q!|] (L.pack "world") @?= "Hello, world!"
  it "hexadecimal float" $ do
    [s|%a|] 0.857421875 @?= "0x1.b7p-1"
    [s|%A|] 3.1415926 @?= "0X1.921FB4D12D84AP+1"
    [s|%.3a|] 1.999999999 @?= "0x2.000p+0"
    [s|%.0a|] 1.999999999 @?= "0x2p+0"
    [s|%#.0a|] 1.999999999 @?= "0x2.p+0"
    [s|%.3a|] 0.7576 @?= "0x1.83ep-1"
    [s|%015.3a|] 0.7576 @?= "0x000001.83ep-1"
    [s|% 15.3a|] 0.7576 @?= "     0x1.83ep-1"
  it "Show instances" $ do
    [s|%?|] () @?= "()"
    [s|%10?|] () @?= "        ()"
  it "pointer" $ do
    [s|%p|] nullPtr @?= "0x0"
    [s|%15p|] fakePtr @?= "     0xdeadbeef"
    -- sign flag does nothing
    [s|%+p|] fakePtr @?= "0xdeadbeef"
    -- prefix flag does nothing
    [s|%#p|] fakePtr @?= "0xdeadbeef"
    -- zero flag does nothing
    [s|%015p|] fakePtr @?= "     0xdeadbeef"
    -- left-pad flag does nothing
    [s|%-15p|] fakePtr @?= "     0xdeadbeef"

fakePtr :: Ptr ()
fakePtr = nullPtr `plusPtr` 0xdeadbeef
