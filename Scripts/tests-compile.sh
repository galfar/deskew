#!/bin/bash
set -eu

ROOTDIR="$(dirname "$0")/.."
pushd $ROOTDIR

mkdir -p "./Dcu"

OUTPUT="-FE./Bin -FU./Dcu"
IMGDIR="./Imaging"
UNITS="-Fu$IMGDIR -Fu$IMGDIR/Libs"
OPTIONS="-B -CirotR -O1 -Mdelphi -vn-h-"
INCLUDE="-Fi$IMGDIR -Fi."
DEFINES="-dIMAGING_USER_OPTIONS"

fpc $OPTIONS $OUTPUT $UNITS $INCLUDE $DEFINES Tests/tests.lpr

