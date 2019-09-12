# th-printf [![th-printf](https://travis-ci.org/pikajude/th-printf.svg)](https://travis-ci.org/pikajude/th-printf) ![th-printf](https://img.shields.io/hackage/v/th-printf)

printf QuasiQuoters

Usage
-----

``` haskell
import Language.Haskell.Printf

-- String interpolation
[s|Hello, %s!|] "Jeff" -- "Hello, Jeff!"

-- Text interpolation
[s|Hello, %q!|] (Data.Text.Lazy.pack "Jeff") -- "Hello, Jeff!"

-- Width specifiers
[s|%010d|] 1977 -- "0000001977"

-- Different radices
[s|%d, %x, %o, %#x, %#o|] 100 100 100 100 100 -- "100, 64, 144, 0x64, 0144"

-- Variable-width formatting
[s|%0*d|] 5 10 -- "00010"
```

There's also another format specifier `%?`, which accepts any `Show`able datatype.
