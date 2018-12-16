{ mkDerivation, base, containers, free-algebras, groups, megaparsec
, mtl, stdenv
}:
mkDerivation {
  pname = "adventofcode2018";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers free-algebras groups megaparsec mtl
  ];
  license = stdenv.lib.licenses.bsd3;
}
