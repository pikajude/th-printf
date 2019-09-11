with import <nixpkgs> {};

haskellPackages.developPackage {
  root = ./.;
  # needed for cabal-helper, which isn't 3.0 ready
  modifier = drv: haskell.lib.addSetupDepend drv haskellPackages.Cabal_2_4_1_0;
}
