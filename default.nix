let
  config = {
    packageOverrides = pkgs:
      let recursiveUpdate = pkgs.lib.recursiveUpdate;
      in
      {

    haskell = recursiveUpdate pkgs.haskell {
      packages = recursiveUpdate pkgs.haskell.packages {
        "${compiler}" = pkgs.haskell.packages."${compiler}".override {
          overrides = hpNew: hpOld: {
            network = pkgs.haskell.lib.dontCheck (hpOld.callHackageDirect {
              pkg = "network";
              ver = "2.8.0.0";
              sha256 = "1849pgyjcxxs580ihhg8hqiqssqv34kiv80fnlnsygkcvx9n7g0n";
            } {});
          };
        };
      };
    };
    };
  };
 compiler = "ghc883";
in
{ pkgs ? import ./nixpkgs.nix {inherit config;}
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

