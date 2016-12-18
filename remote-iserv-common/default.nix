{ mkDerivation, base, binary, bytestring, stdenv }:
mkDerivation {
  pname = "remote-iserv-common";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base binary bytestring ];
  homepage = "https://github.com/shlevy/remote-iserv";
  description = "Common util library for implementing iserv over the wire";
  license = stdenv.lib.licenses.mit;
}
