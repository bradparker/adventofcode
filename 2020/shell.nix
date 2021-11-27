let
  nixpkgs = import <nixpkgs> {};
  ghcWithPackages = nixpkgs.haskellPackages.ghcWithPackages (p: with p; [
    criterion
  ]);
in
  nixpkgs.mkShell {
    buildInputs = [
      ghcWithPackages
    ];
  }
