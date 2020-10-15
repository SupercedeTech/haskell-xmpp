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

    environment = {
        etc."ejabberd.yml" = {
            user = "ejabberd";
            mode = "0600";
            text = builtins.readFile ./ejabberd.yml;
        };
    };

    services = {
        ejabberd = {
            enable = true;
            configFile = "/etc/ejabberd.yml";
        };
    };

  }
