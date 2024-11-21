#!/bin/sh

#cabal build                                                --constraint "usocomp +web" --with-compiler=wasm32-wasi-ghc --with-hc-pkg=wasm32-wasi-ghc-pkg --with-hsc2hs=wasm32-wasi-hsc2hs
cabal install --installdir=./bin --overwrite-policy=always --constraint "usocomp +web" --with-compiler=wasm32-wasi-ghc --with-hc-pkg=wasm32-wasi-ghc-pkg --with-hsc2hs=wasm32-wasi-hsc2hs
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i bin/slangcweb.wasm -o bin/ghc_wasm_jsffi_raw.js
mv bin/slangcweb.wasm bin/slangcweb_orig.wasm
wizer bin/slangcweb_orig.wasm -o bin/slangcweb_initialized.wasm --wasm-bulk-memory true --allow-wasi --init-func _initialize
wasm-opt bin/slangcweb_initialized.wasm -Oz -o bin/slangcweb.wasm
wasm-strip bin/slangcweb.wasm
cp -L bin/slangcweb.wasm ../docs/slangcweb.wasm
#cp -L bin/slangcweb.js ../docs/slangcweb.js