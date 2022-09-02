#!/bin/bash
set -eu

# If you want to run the test with another deskew executable just pass it as
# a parameter e.g. "runtests.sh deskew-arm"
DESKEW=deskew

# On some platforms deskew does not suport TIFFs,
# if you want to run the tests there pass "--no-tiff" as arg
NOTIFF=0
NOTIFF_FLAG="--no-tiff"

while (( "$#" )); do
  if [[ $1 == $NOTIFF_FLAG ]]; then
    NOTIFF=1
    echo "Not running tests with TIFFs"
  else
    DESKEW=${1-$DESKEW}
    echo "Using '$DESKEW' executable"
  fi
  shift
done

./$DESKEW ../TestImages/2.png
./$DESKEW -t a -a 10 -o TestOut/Out2.png ../TestImages/2.png
./$DESKEW -a 10 -o TestOut/Out3.png ../TestImages/3.png
./$DESKEW -o TestOut/Out4.png ../TestImages/4.png
./$DESKEW -q lanczos -a 10 -o TestOut/Out5.png ../TestImages/5.png
./$DESKEW -g c -o TestOut/OutF1550.jpg ../TestImages/F1550.jpg
./$DESKEW -t 128 -o TestOut/Oute2.png ../TestImages/2.png
./$DESKEW -t 180 -o TestOut/OuteF1550.jpg ../TestImages/F1550.jpg
./$DESKEW -q nearest -b 00FFFF -o TestOut/Outb5.png ../TestImages/5.png
./$DESKEW -r 214,266,933,1040 -o TestOut/Outr4.png ../TestImages/4.png
./$DESKEW -t 100 -a 11 -b aa55cc -r 314,366,833,940 -s sp -o TestOut/Outs4.png ../TestImages/4.png
./$DESKEW -f b1 -o TestOut/Outf2.png ../TestImages/2.png
./$DESKEW -f rgba32 -b 40ff00ff -o TestOut/Outa6.png ../TestImages/6.png
./$DESKEW -f g8 -b 77 -s t -o TestOut/Outg6.png ../TestImages/6.png
./$DESKEW -g d ../TestImages/5.png

if [[ $NOTIFF == 0 ]]; then
  ./$DESKEW -t a -a 5 -o TestOut/Out1.tif ../TestImages/1.tif
  ./$DESKEW -b DD -c j95,tjpeg -o TestOut/Out-tiff-jpeg.tif ../TestImages/tiff-jpeg.tif
  ./$DESKEW -t 128 -o TestOut/Oute1.tif ../TestImages/1.tif
  ./$DESKEW -b FF0000 -o TestOut/Outb1.tif ../TestImages/1.tif
  ./$DESKEW -f b1 -o TestOut/Outf1.tif ../TestImages/1.tif
  ./$DESKEW -a 5 -l 2 -o TestOut/Outl1.tif ../TestImages/1.tif
fi

echo
echo TESTS PASSED!