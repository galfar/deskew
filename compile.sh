mkdir -p Dcu
fpc -FuImaging -FuImaging/ZLib -FuImaging/JpegLib -FuImaging/LibTiff -FiImaging -FUDcu -O3 -B -Xs -XX -Mdelphi -FEBin deskew.lpr