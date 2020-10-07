{ pkgs
, ... }:

let
  riskbook = import ../backend/riskbook-web {inherit settings; };
  settings = pkgs.lib.recursiveUpdate (import ./settings.nix) {inherit pkgs;};
in
  {
    virtualisation = {
      diskSize = 2048;
      writableStoreUseTmpfs = true;
      graphics = false;
      memorySize = 4096;
    } ;
    services = {
        ejabberd = {
            enable = true;
        };
    };

  }
