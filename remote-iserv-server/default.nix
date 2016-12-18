{ mkDerivation, base, binary, bytestring, deepseq, filepath, ghci
, remote-iserv-common, stdenv, unix
}:
mkDerivation {
  pname = "remote-iserv-server";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring deepseq filepath ghci remote-iserv-common
    unix
  ];
  homepage = "https://github.com/obsidiansystems/remote-iserv";
  description = "Server library for implementing iserv over the wire";
  license = stdenv.lib.licenses.bsd3;
}
