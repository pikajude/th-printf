{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language CPP #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language QuasiQuotes #-}

module Main (main) where

import Codegen
import qualified Data.Text as T
import Test.Hspec
import Text.Printf.TH

main :: IO ()
main =
    hspec $ do
        $(gen "String quoter" 'id s)
        $(gen "Text quoter" 'T.pack st)
