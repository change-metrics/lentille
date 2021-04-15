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
    };
  };
  drv = hsPkgs.callCabal2nix "lentille" ./. { };
  drv-api = hsPkgs.callCabal2nix "lentille-api" ./lentille-api/. { };
  shellDrv = hsPkgs.shellFor {
    withHoogle = hoogle;
    packages = p: [ drv drv-api ];
    buildInputs = with nixpkgs.haskellPackages; [
      hlint
      cabal-install
      haskell-language-server
    ];
  };
in if nixpkgs.lib.inNixShell then shellDrv else drv
