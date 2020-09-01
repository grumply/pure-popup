{ mkDerivation, base, pure, pure-elm, pure-prop, stdenv
}:
mkDerivation {
  pname = "pure-popup";
  version = "0.8.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base pure pure-elm pure-prop
  ];
  license = stdenv.lib.licenses.bsd3;
}
