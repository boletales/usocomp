#!/bin/sh

cabal install --installdir=./bin --with-compiler=wasm32-wasi-ghc --with-hc-pkg=wasm32-wasi-ghc-pkg --with-hsc2hs=wasm32-wasi-hsc2hs
cp -L bin/slangc.wasm ../docs/slangc.wasm