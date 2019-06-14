#!/bin/bash

pushd ..
mkdir -p Dcu

# By default FPC ARM cross compiler is built with "-dFPC_ARMHF" option and 
# can only produce binaries with this ABI (https://wiki.freepascal.org/ARM_compiler_options).
# If you have ARM softfp target tou have to compile the cross compiler with "-dFPC_ARMEL".

/mnt/c/Programs/FPCup/fpc/bin/x86_64-win64/ppcrossarm.exe -TLinux \
-FuImaging -FuImaging/ZLib -FuImaging/JpegLib -FuImaging/LibTiff -FiImaging -FUDcu \
-O3 -B -Xs -XX -Mdelphi -FEBin -odeskew-arm deskew.lpr

popd