#!/bin/bash
# Runs Deskew on the example images, results go to "out" folder.
# See ../Readme.md and ../Docs/Usage.md for what the options mean.
cd "$(dirname "$0")"
DESKEW=../deskew
mkdir -p out

echo "--- Printed page scanned on a dark background"
$DESKEW -o out/book-page.png book-page.png
echo
echo "--- Handwritten notes: corners filled with white, size of the page kept, JPEG quality 85"
$DESKEW -b ffffff -g c -c j85 -o out/notes.jpg notes.jpg
echo
echo "--- Blank page: nothing to detect the skew from, it is left as it is"
$DESKEW -o out/blank-page.png blank-page.png
echo
echo "--- Skew angle only, no output file"
$DESKEW -g d book-page.png
echo
echo "--- Black and white fax in TIFF, output kept 1-bit (needs libtiff, see Readme)"
$DESKEW -f b1 -o out/fax-1bit.tif fax-1bit.tif || echo "TIFF example failed, is libtiff installed?"
echo
echo "Results are in $(pwd)/out"
