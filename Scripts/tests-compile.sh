#!/bin/bash
set -eu

ROOTDIR="$(dirname "$0")/.."
pushd $ROOTDIR

mkdir -p "./Dcu"

OUTPUT="-FE./Bin -FU./Dcu"
IMGDIR="./Imaging"
UNITS="-Fu$IMGDIR -Fu$IMGDIR/Libs -Fu$IMGDIR/LibTiff"
OPTIONS="-B -CirotR -O1 -Mdelphi -vn-h-"
INCLUDE="-Fi$IMGDIR -Fi."
LIBS="-Fl$IMGDIR/LibTiff/Compiled"
DEFINES="-dFPCPASJPEG -dIMAGING_USER_OPTIONS"

fpc $OPTIONS $OUTPUT $UNITS $INCLUDE $LIBS $DEFINES Tests/tests.lpr

