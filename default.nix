{
    pkgs ? import nix/nixpkgs.nix {}, ...
}:
{
  haskell-xmpp-884  = import nix/haskell-xmpp.nix {compiler = "ghc884"; inherit pkgs;};
  haskell-xmpp-8107 = import nix/haskell-xmpp.nix {compiler = "ghc8107"; inherit pkgs;};
  haskell-xmpp-902  = import nix/haskell-xmpp.nix {compiler = "ghc902"; inherit pkgs;};
  # singlethongs currently doesn't build on ghc9.2,
  # we should probably move over to singletongs which is maintained.
  # haskell-xmpp-922  = import nix/haskell-xmpp.nix {compiler = "ghc922"; inherit pkgs;};
  test = pkgs.nixosTest nix/test.nix;
}
