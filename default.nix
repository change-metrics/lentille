{ hoogle ? false, nixpkgs ? import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/c0e881852006b132236cbf0301bd1939bb50867e.tar.gz")
  { } }:
let
  haskell-grpc-src = nixpkgs.fetchFromGitHub {
    owner = "awakesecurity";
    repo = "grpc-haskell";
    rev = "1bdc3662db13686f5e4941f3cc506f6fdf11ce32";
    sha256 = "186g46b4zyjpqylikhfrwl7gq1vqa2b6jv40h786ayc8njgkrk6j";
  };
  notest = s: d:
    nixpkgs.haskell.lib.dontCheck (nixpkgs.haskell.lib.overrideCabal s d);
  hsPkgs = nixpkgs.haskellPackages.override {
    overrides = self: super: {
      relude = nixpkgs.haskell.lib.overrideCabal super.relude {
        version = "1.0.0.1";
        sha256 = "0cw9a1gfvias4hr36ywdizhysnzbzxy20fb3jwmqmgjy40lzxp2g";
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

      #############################
      # grpc-haskell requirements #
      #############################
      # disable broken tests:
      parameterized = notest super.parameterized { broken = false; };
      range-set-list = nixpkgs.haskell.lib.dontCheck super.range-set-list;
      proto3-wire =
        nixpkgs.haskell.lib.overrideCabal super.proto3-wire { broken = false; };
      # pull in fixes, see cabal.project
      proto3-suite = nixpkgs.haskell.lib.dontCheck
        (nixpkgs.haskell.lib.overrideCabal super.proto3-suite {
          version = "0.4.2.0";
          src = nixpkgs.fetchFromGitHub {
            owner = "awakesecurity";
            repo = "proto3-suite";
            rev = "0af901f9ef3b9719e08eae4fab8fd700d6c8047a";
            sha256 = "0mpy35r6qd1v5sixhy2lqcn5x81rfj4dc079g1kpa4fb1f23dbha";
          };
          broken = false;
        });
      grpc-haskell-core =
        nixpkgs.haskell.lib.overrideCabal super.grpc-haskell-core {
          broken = false;
          src = "${haskell-grpc-src}/core";
        };
      grpc-haskell = notest super.grpc-haskell {
        version = "0.1.0";
        src = haskell-grpc-src;
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
      lentille-grpc =
        self.callCabal2nixWithOptions "lentille-grpc" ./lentille-grpc/.
        "--flag=with-grpc" { };
    };
  };
  drvs = with hsPkgs; [
    lentille
    lentille-bloodhound
    lentille-doc
    lentille-mock
    lentille-bugzilla
    lentille-api
    # TODO: pin the grpc version
    # lentille-grpc
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
