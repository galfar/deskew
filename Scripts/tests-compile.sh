#!/bin/bash
set -eu

ROOTDIR="$(dirname "$0")/.."
mkdir -p "$ROOTDIR/Dcu"

echo $ROOTDIR

OUTPUT="-FE$ROOTDIR/Bin -FU$ROOTDIR/Dcu"
IMGDIR="$ROOTDIR/Imaging"
UNITS="-Fu$IMGDIR -Fu$IMGDIR/ZLib -Fu$IMGDIR/LibTiff"
OPTIONS="-B -CirotR -O1 -Mdelphi -vn-h-"
INCLUDE="-Fi$IMGDIR"
LIBS="-Fl$IMGDIR/LibTiff/Compiled"
DEFINES="-dPASJPEG"

pushd ../Tests

fpc $OPTIONS $OUTPUT $UNITS $INCLUDE $LIBS $DEFINES tests.lpr

popd
