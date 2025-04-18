#!/bin/bash
set -eu

ROOTDIR="$(dirname "$0")/.."
pushd $ROOTDIR

mkdir -p "./Dcu"

OUTPUT="-FE./Bin -FU./Dcu"
IMGDIR="./Imaging"
UNITS="-Fu$IMGDIR -Fu$IMGDIR/ZLib -Fu$IMGDIR/LibTiff"
OPTIONS="-B -CirotR -O1 -Mdelphi -vn-h-"
INCLUDE="-Fi$IMGDIR"
LIBS="-Fl$IMGDIR/LibTiff/Compiled"
DEFINES="-dPASJPEG"

fpc $OPTIONS $OUTPUT $UNITS $INCLUDE $LIBS $DEFINES Tests/tests.lpr

