nixpkgs:
self:
super:
let
  free-algebras-source = nixpkgs.fetchFromGitHub {
    owner = "mstksg";
    repo = "free-algebras";
    rev = nixpkgs.lib.fileContents ./rev;
    sha256 = nixpkgs.lib.fileContents  ./sha;
  };
in
  {
    free-algebras = self.callPackage "${free-algebras-source}/pkg.nix" {
      inherit nixpkgs;
    };
  }
