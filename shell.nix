let srcs = import nix/sources.nix;
    thoralf-overlay =
      self: super: {
        haskellPackages = super.haskell.packages.ghc882.override {
          overrides = hself: hsuper: {
            thoralf-plugin =
              super.haskell.lib.dontCheck
              (hself.callCabal2nix "thoralf-plugin"
                srcs.the-thoralf-plugin {});
            };
          };
      };

    isl-bindings-overlay = import (builtins.fetchGit {
      name = "isl-bindings";
      url = "https://github.com/gdeest/isl-bindings";
      rev = "3c9738e52801b7672daad38d64b5f6a24144974d";
    });

    pkgs = (import srcs.nixpkgs {
      overlays = [ thoralf-overlay isl-bindings-overlay ];
    });
in
with pkgs;
mkShell {
  name = "dumb-shell";
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (p: with p;
      [ thoralf-plugin
        isl-bindings
        vector
      ]))
    haskellPackages.cabal-install
    z3
  ];
}
