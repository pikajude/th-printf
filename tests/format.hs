{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP         #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import           Language.Haskell.TH.Quote
import           Test.Hspec
import           Test.HUnit
import           Test.QuickCheck
import           Text.Printf.TH

#if __GLASGOW_HASKELL__ <= 706
instance Eq ErrorCall where
    ErrorCall m == ErrorCall n = m == n
#endif

main :: IO ()
main = hspec $ do
    describe "quoter" $ do
        it "handles escapes properly" $
            [s|\n\STX\\|] @?= "\n\STX\\"

        it "accepts escaped format specifiers" $
            [s|\37\115|] "foo" @?= "foo"

        it "rejects unknown escape sequences" $
            assertException (\ x -> case x of ErrorCall _ -> True; _ -> False) . evaluate $ quoteExp s "\\UNKNOWN"

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

assertException :: (Exception e, Eq e) => (e -> Bool) -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        a <- action
        assertFailure $ a `seq` "Thrown exception did not match predicate"
    where isWanted = guard . ex
