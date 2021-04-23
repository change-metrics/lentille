# WIP: lentille-grpc

> Note: this relies on the libgrpc.so version 1.34.1 (currently not available on f33).

Install development dependencies:

```ShellSession
sudo dnf install -y cmake make gcc
git clone -b v1.34.1 https://github.com/grpc/grpc
cd grpc
git submodule update --init
mkdir -p cmake/build
cd cmake/build
# TODO: make /usr/local/lib work and stop using /usr/lib here
cmake ../.. -DBUILD_SHARED_LIBS:BOOL=ON -DgRPC_BUILD_CODEGEN:BOOL=OFF -DgRPC_INSTALL:BOOL=ON -DgRPC_INSTALL_LIBDIR:PATH=/usr/lib64 -DgRPC_INSTALL_INCLUDEDIR:PATH=/usr/include -DINSTALL_BIN_DIR:PATH=/usr/bin -DINSTALL_INC_DIR:PATH=/usr/include -DINSTALL_LIB_DIR:PATH=/usr/lib64 -DINSTALL_PKGCONFIG_DIR:PATH=/usr/lib64/pkgconfig
make -j8
sudo make install
```

Generate the types:

```bash
cabal install proto3-suite
~/.cabal/bin/compile-proto-file --includeDir ../protos/ --proto monocle/search.proto --out src/
```

Run the api:

```bash
cabal run --flag=with-grpc
```

Run the client:

```ShellSession
$ (cd client;
   python3 -m pip install --user grpcio-tools
   # Generate binding
   python3 -m grpc_tools.protoc -I../../protos --python_out=. --grpc_python_out=. ../../protos/monocle/search.proto
   # Run client
   python3 python.py)
```
