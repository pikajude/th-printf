[![Build Status](https://travis-ci.org/joelteon/th-printf.png)](https://travis-ci.org/joelteon/th-printf)
th-printf
=========
aims to make printf for people who like to use printf safe and reasonably efficient.

This package was inspired by `Text.Printf` and [http://hackage.haskell.org/package/printf-mauke](`printf-mauke`),
but the backend is generalized to support any String-like datastructure instead of only String. It also provides
quasiquoters rather than Exp splicers, which hopefully will be a little less noisy.

Usage
-----

There are five different formatters: `s`, `st`, `lt`, `sb`, and `lb`. They produce `String`, strict `Text`, lazy
`Text`, strict `ByteString`, and lazy `ByteString` respectively.

Quoters support any format type that C's printf supports (excluding hexdecimal floating point; if you would like to
submit a patch, please feel free):

``` haskell
-- String interpolation
[s|Hello, %s!|] "Jeff" -- "Hello, Jeff!"

-- Width specifiers
[s|%010d|] 1977 -- "0000001977"

-- Different radices
[s|%d, %x, %o, %#x, %#o|] 100 100 100 100 100 -- "100, 64, 144, 0x64, 0144"

-- Variable-width formatting
[s|%0*d|] 5 10 -- "00010"
```

There are five additional formatters: `sP`, `stP`, and so on; these print out the formatted string rather than returning
it. Their types are `MonadIO m => ... -> m ()`.
