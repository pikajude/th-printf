# th-printf [![th-printf](https://travis-ci.org/pikajude/th-printf.svg?branch=bkp)](https://travis-ci.org/pikajude/th-printf)

printf QuasiQuoters

Usage
-----

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

There's also another format specifier `%?`, which accepts any `Show`able datatype.
