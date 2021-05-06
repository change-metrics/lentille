# lentille

[![Hackage](https://img.shields.io/hackage/v/lentille.svg?logo=haskell)](https://hackage.haskell.org/package/lentille)
[![AGPL-3.0-only license](https://img.shields.io/badge/license-AGPL--3.0--only-blue.svg)](LICENSE)

This repository contains worker for change-metrics:

* [`lentille-bugzilla`](./lentille-bugzilla) - bugzilla task crawler
* [`lentille-github`](./lentille-bugzilla) - github task crawler


## Contributing

Contributions are very welcome!
To get started, run:

```ShellSession
# Install the toolchain, for example on fedora>=33:
dnf install -y ghc cabal-install zlib-devel git && cabal update

# Run the tests:
cabal test all
```
