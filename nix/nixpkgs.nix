let
  # release-21.11, committed on May 23 2022
  rev = "a790b646e0634695782876f45d98f93c38ceae1d";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
in
  import (builtins.fetchTarball url)
