{ pkgs, ... }:

let
  haskell-xmpp = import ./haskell-xmpp.nix {inherit pkgs;};
in
  {
    name = "riskbook-test";

    nodes.server = ./ci-server.nix;

    # Hint: the following get passed through a python linter and the
    # test will fail if the linter would change anything (the error
    # contains a diff of what should be changed)
    testScript = ''
      server.start()
      server.wait_for_unit("ejabberd.service")
      print(
          server.succeed(
              "${haskell-xmpp}/bin/haskell-xmpp-io-test"
          )
      )
      print("done")
    '';
  }
