pushd ..
mkdir Dcu
fpc -FuImaging -FuImaging/Libs -FuImaging/LibTiff -FiImaging -Fi. -dIMAGING_USER_OPTIONS -FUDcu -FlImaging\LibTiff\Compiled -O3 -B -Xs -XX -Mdelphi -FEBin deskew.lpr
popd