{ hoogle ? false }:
let
  # pin the upstream nixpkgs
  nixpkgsSrc = (import (fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/8d0340aee5caac3807c58ad7fa4ebdbbdd9134d6.tar.gz";
    sha256 = "0r00azbz64fz8yylm8x37imnrsm5cdzshd5ma8gwfwjyw166n3r1";
  }));

  # create the main package set without options
  pkgs = nixpkgsSrc { };

  grpc-haskell-src = pkgs.fetchFromGitHub {
    owner = "awakesecurity";
    repo = "gRPC-haskell";
    rev = "d821e58c2b72f127ce5b74b69dac8cf3d7f558ad";
    sha256 = "1ms6v58rznkqk4807n9yr888lf0bbn7p7a9mjwmbdckc1pa1gxdv";
  };
  grpc-overlay = (import "${grpc-haskell-src}/release.nix").overlay;
  grpc-nixpkgs = (import "${grpc-haskell-src}/nixpkgs.nix");
  nixpkgs = grpc-nixpkgs {
    overlays = [ grpc-overlay ];
    config = { allowBroken = true; };
  };
  hsPkgs = nixpkgs.haskellPackages.extend (self: super: {
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
    monocle = self.callCabal2nix "monocle" ../monocle/haskell/. { };
    macroscrope = self.callCabal2nix "macroscope" ./macroscope/. { };
    lentille-doc = self.callCabal2nix "lentille-doc" ./doc/. { };
    lentille-bugzilla =
      self.callCabal2nix "lentille-bugzilla" ./lentille-bugzilla/. { };
    lentille-github =
      self.callCabal2nix "lentille-github" ./lentille-github/. { };
    lentille-gitlab =
      self.callCabal2nix "lentille-gitlab" ./lentille-gitlab/. { };
  });
  drvs = with hsPkgs; [
    lentille-doc
    lentille-bugzilla
    lentille-github
    lentille-gitlab
    macroscope
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
