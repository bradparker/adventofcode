{ compiler ? "default"
}:
let
  nixpkgs = import ./nix/packages {
    inherit compiler;
  };
in
  (import ./. {
    inherit compiler;
  }).env
