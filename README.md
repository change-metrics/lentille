# lentille

[![Hackage](https://img.shields.io/hackage/v/lentille.svg?logo=haskell)](https://hackage.haskell.org/package/lentille)
[![AGPL-3.0-only license](https://img.shields.io/badge/license-AGPL--3.0--only-blue.svg)](LICENSE)

This repository contains Haskell package for change-metrics:

* [`lentille`](./lentille) - a prelude library
* [`lentille-api`](./lentille-api) - a WIP servant based api prototype
* [`lentille-bugzilla`](./lentille-bugzilla) - bugzilla task crawler

## Contributing

Contributions are very welcome!
To get started, run:

```ShellSession
# Install the toolchain, for example on fedora>=33:
dnf install -y ghc cabal-install zlib-devel git && cabal update

# Run the tests:
cabal test all
```
