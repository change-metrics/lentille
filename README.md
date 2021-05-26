# lentille

[![Hackage](https://img.shields.io/hackage/v/lentille.svg?logo=haskell)](https://hackage.haskell.org/package/lentille)
[![AGPL-3.0-only license](https://img.shields.io/badge/license-AGPL--3.0--only-blue.svg)](LICENSE)

This repository contains worker for change-metrics:

- [`lentille-bugzilla`](./lentille-bugzilla) - bugzilla task crawler
- [`lentille-github`](./lentille-bugzilla) - github task crawler

## Contributing

Contributions are very welcome!
To get started, run:

```ShellSession
# Install the toolchain, for example on fedora>=33:
dnf install -y ghc cabal-install zlib-devel git && cabal update

# Run the tests:
cabal test all
```

### Validate a change that depend on a PR on Monocle

Ensure to revert those changes before the merge of the Lentille PR.

#### In the GitHub CI

In the `.github/workflows/haskell.yml` ensure to set the Monocle checkout ref at the
right dependent PR.

#### Locally

Ensure to have a checkout of change-metrics/monocle in the lentille's parent directory.

In `cabal.project`, add `../monocle/haskell/` to the list of packages and comment
the `source-repository-package` of Monocle.

## Run a crawler

Prior to run a crawler, you need to ensure that a crawler entry is defined in the Monocle
[configuration](https://github.com/change-metrics/monocle#connect-a-tasks-tracker-crawler).

## Run the GitHub issue crawler

A CLI is available and the crawler can be run with the following command:

```ShellSession
GITHUB_GRAPH_TOKEN=<gh-token> MONOCLE_API_KEY=<monocle-api-key> cabal run lentille-github -- \
 --monocle-url <monocle-api-url> --index elastic --crawler-name gh-crawler --repo elastic/elasticsearch
```

Or using the container image:

```ShellSession
podman run -e GITHUB_GRAPH_TOKEN=<gh-token> -e MONOCLE_API_KEY=<monocle-api-key> -it --rm \
quay.io/change-metrics/lentille /bin/lentille-github -h
```

## Run the BugZilla crawler

Note that this crawler is based on the [redhat-bugzilla](https://hackage.haskell.org/package/bugzilla-redhat)
library and it might not work as expected with regular BugZilla.

A CLI is available and the crawler can be run with the following command:

```ShellSession
BZ_API_KEY=<bugzilla-api-key> MONOCLE_API_KEY=<monocle-api-key> cabal run lentille-bugzilla -- \
 --monocle-url <monocle-url> --index openstack --crawler-name rhbz-crawler \
 --bugzilla-product "Red Hat OpenStack"
```

Or using the container image:

```ShellSession
podman run -e BZ_API_KEY=<bugzilla-api-key> -e MONOCLE_API_KEY=<monocle-api-key> -it --rm \
quay.io/change-metrics/lentille /bin/lentille-bugzilla -h
```

## Build container image

```ShellSession
TMPDIR=/tmp podman build -f Containerfile -t quay.io/change-metrics/lentille .
```
