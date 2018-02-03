{-# Language QuasiQuotes #-}

module Main
    ( main
    ) where

import Criterion
import Criterion.Main
import Text.Printf.TH

main :: IO ()
main =
    defaultMain
        [ env (pure ([s|test %50s|], [st|test %50s|])) $ \ ~(fs, ft) ->
              bgroup "string" [bench "s" $ nf fs "foobar", bench "st" $ nf ft "foobar"]
        , bgroup "int" [bench "s" $ nf [s|%010d|] 20, bench "st" $ nf [st|%010d|] 20]
        ]
