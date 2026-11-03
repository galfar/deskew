#!/bin/bash
# Builds the CLI for WebAssembly (wasm32-wasip1) into Web/deskew.wasm for the web GUI.
# Needs FPC trunk (3.3.1) with the wasm32-wasip1 cross compiler and wasm-ld (LLVM lld),
# e.g. installed by FPCupDeluxe. FPC can be given in the FPC variable.
set -eu

ROOTDIR="$(dirname "$0")/.."
pushd $ROOTDIR

FPC="${FPC:-fpc}"
mkdir -p "./Dcu/wasm"

OUTPUT="-FE./Web -FU./Dcu/wasm"
IMGDIR="./Imaging"
UNITS="-Fu$IMGDIR -Fu$IMGDIR/Libs"
# Branchful exceptions work in every browser, the others need the wasm exception handling proposal
TARGET="-Twasip1 -Pwasm32 -CTbfexceptions"
OPTIONS="-B -O3 -Xs -Mdelphi -vn-"
INCLUDE="-Fi$IMGDIR -Fi."
DEFINES="-dIMAGING_USER_OPTIONS"

"$FPC" $TARGET $OPTIONS $OUTPUT $UNITS $INCLUDE $DEFINES deskew.lpr
