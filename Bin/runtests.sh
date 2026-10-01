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

# All defaults, output goes next to the input as deskewed-2.png
./$DESKEW ../TestImages/2.png
# Auto (Otsu) threshold and max skew angle
./$DESKEW -t a -a 10 -o TestOut/Out2.png ../TestImages/2.png
# Max skew angle only
./$DESKEW -a 10 -o TestOut/Out3.png ../TestImages/3.png
# Output file name only
./$DESKEW -o TestOut/Out4.png ../TestImages/4.png
# Lanczos resampling filter
./$DESKEW -q lanczos -a 10 -o TestOut/Out5.png ../TestImages/5.png
# JPEG in and out, crop the result to the input size
./$DESKEW -g c -o TestOut/OutF1550.jpg ../TestImages/F1550.jpg
# Explicit threshold level
./$DESKEW -t 128 -o TestOut/Oute2.png ../TestImages/2.png
# Explicit threshold level, JPEG
./$DESKEW -t 180 -o TestOut/OuteF1550.jpg ../TestImages/F1550.jpg
# Nearest neighbor filter and RGB background color
./$DESKEW -q nearest -b 00FFFF -o TestOut/Outb5.png ../TestImages/5.png
# Skew detection only inside a content rectangle (pixels)
./$DESKEW -r 214,266,933,1040 -o TestOut/Outr4.png ../TestImages/4.png
# Skew detection only outside page margins (horizontal and vertical, in % of page size)
./$DESKEW -m 15,10,% -o TestOut/Outm4.png ../TestImages/4.png
# Many options combined: threshold, max angle, ARGB background, content rectangle, stats and params dump
./$DESKEW -t 100 -a 11 -b aa55cc -r 314,366,833,940 -s sp -o TestOut/Outs4.png ../TestImages/4.png
# Forced 1-bit output format
./$DESKEW -f b1 -o TestOut/Outf2.png ../TestImages/2.png
# Forced 32-bit RGBA output with semi-transparent background
./$DESKEW -f rgba32 -b 40ff00ff -o TestOut/Outa6.png ../TestImages/6.png
# Forced 8-bit grayscale output with gray background, timings
./$DESKEW -f g8 -b 77 -s t -o TestOut/Outg6.png ../TestImages/6.png
# Detect only: prints skew angle, writes no output
./$DESKEW -g d ../TestImages/5.png
# Skew below the skip limit: deskewing is skipped and the input is copied as is
./$DESKEW -a 5 -l 2 -o TestOut/OutlF1550.jpg ../TestImages/F1550.jpg

if [[ $NOTIFF == 0 ]]; then
  # Binary LZW TIFF in and out, default compression (LZW)
  ./$DESKEW -t a -a 5 -o TestOut/Out1.tif ../TestImages/1-lzw.tif
  # JPEG compression requested explicitly for a non-JPEG input, output must be JPEG-compressed
  ./$DESKEW -b DD -c j95,tjpeg -o TestOut/Out1-lzw-to-jpeg.tif ../TestImages/1-lzw.tif
  # Compression "as input": CCITT G4 input stays G4 (bilevel output)
  ./$DESKEW -t 128 -c tinput -o TestOut/Out1-g4.tif ../TestImages/1-g4.tif
  # Compression "as input": JPEG input stays JPEG (lossy recompression is accepted here)
  ./$DESKEW -b DD -c tinput -o TestOut/Out-tiff-jpeg-also-jpeg.tif ../TestImages/tiff-jpeg.tif
  # Compression "as input, lossless only": JPEG input gets LZW instead
  ./$DESKEW -c tinput-lossless -o TestOut/Out-tiff-jpeg-to-lzw.tif ../TestImages/tiff-jpeg.tif
  # Explicit Deflate compression and red background
  ./$DESKEW -b FF0000 -c tdeflate -o TestOut/Out1-deflate.tif ../TestImages/1-lzw.tif
  # Forced 1-bit output, TIFF writer picks CCITT G4 for it
  ./$DESKEW -f b1 -o TestOut/Out1-b1.tif ../TestImages/1-lzw.tif
fi

echo
echo TESTS PASSED!