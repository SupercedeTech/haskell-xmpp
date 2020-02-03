{ mkDerivation, array, base, blaze-markup, HaXml, html, mtl
, network, polyparse, pretty, random, regex-compat, singletons
, stdenv, stm, text, time, utf8-string, uuid, xml-conduit
, xml-hamlet, unliftio
}:
mkDerivation {
  pname = "haskell-xmpp";
  version = "1.0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base blaze-markup HaXml html mtl network polyparse pretty
    random regex-compat singletons stm text time utf8-string uuid
    xml-conduit xml-hamlet unliftio
  ];
  homepage = "http://patch-tag.com/r/adept/haskell-xmpp/home";
  description = "Haskell XMPP (eXtensible Message Passing Protocol, a.k.a. Jabber) library";
  license = stdenv.lib.licenses.bsd3;
}
