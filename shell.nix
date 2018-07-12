with import <nixpkgs> {}; stdenv.mkDerivation { name = "th-printf-env"; buildInputs = [ haskell.compiler.ghc843 ]; }
