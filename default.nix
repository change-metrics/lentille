{ hoogle ? false, nixpkgs ? import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/c0e881852006b132236cbf0301bd1939bb50867e.tar.gz")
  { } }:
let
  hsPkgs = nixpkgs.haskellPackages.override {
    overrides = self: super: {
      relude = nixpkgs.haskell.lib.overrideCabal super.relude {
        version = "1.0.0.1";
        sha256 = "0cw9a1gfvias4hr36ywdizhysnzbzxy20fb3jwmqmgjy40lzxp2g";
      };
      lentille = self.callCabal2nix "lentille" ./lentille/. { };
      lentille-doc = self.callCabal2nix "lentille-doc" ./doc/. { };
      lentille-mock = self.callCabal2nix "lentille-mock" ./lentille-mock/. { };
      lentille-bugzilla =
        self.callCabal2nix "lentille-bugzilla" ./lentille-bugzilla/. { };
      lentille-api = self.callCabal2nix "lentille-api" ./lentille-api/. { };
    };
  };
  drvs = with hsPkgs; [
    lentille
    lentille-doc
    lentille-mock
    lentille-bugzilla
    lentille-api
  ];
  shellDrv = hsPkgs.shellFor {
    withHoogle = hoogle;
    packages = p: drvs;
    buildInputs = with nixpkgs.haskellPackages; [
      hlint
      cabal-install
      haskell-language-server
    ];
  };
in if nixpkgs.lib.inNixShell then shellDrv else drvs
