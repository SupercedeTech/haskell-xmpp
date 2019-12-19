{ pkgs ? import ./nixpkgs.nix {}
, compiler ? "ghc865"
}:

let
  inherit (import ./gitignoreSource.nix { inherit (pkgs) lib; }) gitignoreSource;
in
  pkgs.haskell.lib.overrideCabal (pkgs.haskell.packages.${compiler}.callPackage ./haskell-xmpp.nix {}) (drv: {
    src = gitignoreSource ./.;
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

