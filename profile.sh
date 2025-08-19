#!/usr/bin/env sh


GHC_PROFILING_FLAGS="-fprof-auto -rtsopts"

cabal build --enable-profiling --ghc-options="$GHC_PROFILING_FLAGS"

time ./dist-newstyle/build/x86_64-linux/ghc-9.8.4/DraftGen-1.5.2.0/x/dg/build/dg/dg +RTS -p -RTS "$@"

ghc-prof-flamegraph dg.prof

firefox dg.svg
