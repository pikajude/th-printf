{ compiler ? "default", pkgs ? import <nixpkgs> {} }:

with pkgs;

let hp = if compiler == "default"
  then haskellPackages
  else haskell.packages.${compiler};

in hp.developPackage {
  root = ./.;
  # needed for cabal-helper, which isn't 3.0 ready
  modifier = drv: haskell.lib.addSetupDepend drv hp.Cabal_2_4_1_0;
  overrides = _: _: {
    hpack = null;
  };
}
