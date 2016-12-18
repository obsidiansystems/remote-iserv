{ mkDerivation, base, binary, bytestring, ghci, remote-iserv-common
, stdenv, unix
}:
mkDerivation {
  pname = "remote-iserv-client";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring ghci remote-iserv-common unix
  ];
  homepage = "https://github.com/shlevy/remote-iserv";
  description = "Client library for implementing iserv over the wire";
  license = stdenv.lib.licenses.mit;
}
