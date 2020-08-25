let
  owner = "NixOS";
  repo = "nixpkgs";
  # release from 13.08.2020
  rev = "70b888b613f8b54d64958ac5c032af7baa88f8d0";
  url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
in
  import (builtins.fetchTarball url)
