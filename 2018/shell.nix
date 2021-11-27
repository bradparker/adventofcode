let
  free-algebras-source = builtins.fetchTarball
    "https://github.com/mstksg/free-algebras/archive/547a7e80b0845ac1cff6052ad24d10a9665cffde.tar.gz";
in { nixpkgs ? import <nixpkgs> {
  overlays = [
    (self: super: {
      haskell = super.haskell // {
        packageOverrides = hself: hsuper: {
          recursion-schemes =
            hsuper.callCabal2nix "free-algebras" free-algebras-source { };
        };
      };
    })
  ];
} }:
let package = nixpkgs.callPackage ./. { };
in package.env
