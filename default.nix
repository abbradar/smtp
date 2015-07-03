{ mkDerivation, attoparsec, base, bytestring, conduit
, conduit-extra, netwire, network, stdenv, text, transformers
, vector
}:
mkDerivation {
  pname = "smtp";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    attoparsec base bytestring conduit conduit-extra netwire network
    text transformers vector
  ];
  doHaddock = false;
  license = stdenv.lib.licenses.bsd3;
}
