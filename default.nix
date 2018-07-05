{ mkDerivation, base, pure, pure-cond, pure-prop, pure-styles, pure-txt, pure-theme, pure-portal, pure-css, stdenv
}:
mkDerivation {
  pname = "pure-popup";
  version = "0.7.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base pure pure-cond pure-prop pure-styles pure-txt pure-theme pure-portal pure-css
  ];
  license = stdenv.lib.licenses.bsd3;
}