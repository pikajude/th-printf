{ compiler ? "default", pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  hp = if compiler == "default"
    then haskellPackages
    else haskell.packages.${compiler};

  isGhcjs = hp.ghc.isGhcjs or false;

  inHie = builtins.getEnv "HIE" == "1";

in hp.developPackage {
  root = ./.;
  modifier = drv: if inHie
    # needed for cabal-helper, which isn't 3.0 ready
    then haskell.lib.addSetupDepend drv hp.Cabal_2_4_1_0
    else drv;
  overrides = self: super: with haskell.lib; {
    # don't auto-run hpack. it has a ton of dependencies and I already run it manually
    hpack = null;
    ${if isGhcjs then "QuickCheck" else null} = dontCheck super.QuickCheck;
  };
}
