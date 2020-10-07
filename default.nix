{
    pkgs ? import nix/nixpkgs.nix {}, ...
}:
{
  haskell-xmpp-865 = import nix/haskell-xmpp.nix {compiler = "ghc865"; inherit pkgs; };
  haskell-xmpp-883 = import nix/haskell-xmpp.nix {compiler = "ghc883"; inherit pkgs;};
  haskell-xmpp-810 = import nix/haskell-xmpp.nix {compiler = "ghc8101"; inherit pkgs;};
  test = pkgs.nixosTest nix/test.nix;
}
