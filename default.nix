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
      morpheus-graphql-core =
        nixpkgs.haskell.lib.overrideCabal super.morpheus-graphql-core {
          version = "0.17.0";
          sha256 = "0rj4g05365hp5c9b5y0v0v7s73jw3gkq3g0z3m6xrpxi3j2gp0p8";
        };
      morpheus-graphql-client =
        nixpkgs.haskell.lib.overrideCabal super.morpheus-graphql-client {
          version = "0.17.0";
          sha256 = "1djgxy59s98na1s182p5a06qjhw8n862zka96wwp8ckyx2jpjkq3";
        };
      bloodhound = nixpkgs.haskell.lib.overrideCabal super.bloodhound {
        src = nixpkgs.fetchFromGitHub {
          owner = "bitemyapp";
          repo = "bloodhound";
          rev = "4775ebb759fe1b7cb5f880e4a41044b2363d98af";
          sha256 = "00wzaj4slvdxanm0krbc6mfn96mi5c6hhd3sywd3gq5m2ff59ggn";
        };

        broken = false;
      };
      lentille = self.callCabal2nix "lentille" ./lentille/. { };
      lentille-bloodhound =
        self.callCabal2nix "lentille-bloodhound" ./lentille-bloodhound/. { };
      lentille-doc = self.callCabal2nix "lentille-doc" ./doc/. { };
      lentille-mock = self.callCabal2nix "lentille-mock" ./lentille-mock/. { };
      lentille-bugzilla =
        self.callCabal2nix "lentille-bugzilla" ./lentille-bugzilla/. { };
      lentille-api = self.callCabal2nix "lentille-api" ./lentille-api/. { };
    };
  };
  drvs = with hsPkgs; [
    lentille
    lentille-bloodhound
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
      ghcid
    ];
  };
in if nixpkgs.lib.inNixShell then shellDrv else drvs
