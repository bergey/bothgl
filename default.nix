{ mkDerivation, base, bytestring, ghcjs-dom, mtl, stdenv, transformers
, vector
}:
mkDerivation {
  pname = "bothgl";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring ghcjs-dom mtl transformers vector
  ];
  homepage = "http://github.com/bergey/bothgl";
  description = "Abstract over WebGL and native OpenGL";
  license = stdenv.lib.licenses.bsd3;
}
