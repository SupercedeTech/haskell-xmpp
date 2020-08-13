{ pkgs ? import ./nixpkgs.nix {}
, compiler ? "ghc883"
}:

let
  hpkgs = pkgs.haskell.packages.${compiler};
  ignore = import ./gitignoreSource.nix { inherit (pkgs) lib; };
  haskell-xmpp = hpkgs.callCabal2nix "haskell-xmpp" (ignore.gitignoreSource ./.) {};
in
pkgs.haskell.lib.overrideCabal haskell-xmpp (drv: {
    configureFlags = ["-f-library-only"];
    doCheck = false;
    testHaskellDepends = [];
    testToolDepends = [];
    doHaddock = false;
    enableLibraryProfiling = false;
    enableSeparateDataOutput = false;
    enableSharedExecutables = false;
    isLibrary = false;
    postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
  })
