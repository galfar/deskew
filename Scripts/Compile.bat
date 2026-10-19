pushd ..
mkdir Dcu
fpc -FuImaging -FuImaging/Libs -FiImaging -Fi. -dIMAGING_USER_OPTIONS -FUDcu -O3 -B -Xs -XX -Mdelphi -FEBin deskew.lpr
popd