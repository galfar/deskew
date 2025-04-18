#!/bin/bash
set -eu

ROOTDIR="$(dirname "$0")/.."
pushd $ROOTDIR

mkdir -p "./Dcu"

OUTPUT="-FE./Bin -FU./Dcu"
IMGDIR="./Imaging"
UNITS="-Fu$IMGDIR -Fu$IMGDIR/ZLib -Fu$IMGDIR/JpegLib -Fu$IMGDIR/LibTiff"
# This is how you suppress -vn set in fpc.cfg
OPTIONS="-B -O3 -Xs -Mdelphi -vn-"
INCLUDE="-Fi$IMGDIR"
LIBS="-Fl$IMGDIR/LibTiff/Compiled"  

fpc $OPTIONS $OUTPUT $UNITS $INCLUDE $LIBS deskew.lpr
