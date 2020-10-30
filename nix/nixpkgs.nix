let
  # https://github.com/NixOS/nixpkgs/tree/nixos-20.09
  rev = "edb26126d98bc696f4f3e206583faa65d3d6e818";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
in
  import (builtins.fetchTarball url)
