Deskew
------------------------
by Marek Mauder
http://galfar.vevb.net/deskew/
https://bitbucket.org/galfar/app-deskew

v0.95 (2010-12-28)

Overview
------------------------


Version History
------------------------
  0.95 2010-12-28: Added auto thresholding. Imaging library updated.
  0.90 2010-02-12: Initial version

Usage
------------------------

deskew [-a angle] [-t a|treshold] [-o output] input
  -a angle:      Maximal skew angle in degrees (default: 10)
  -t a|treshold: Auto threshold or value in 0..255 (default: a)
  -o output:     Output image file (default: out.png)
  input:         Input image file

  Supported file formats
    Input:  BMP, JPG, PNG, MNG, JNG, GIF, DDS, TGA, PBM, PGM, PPM, PAM, PFM
    Output: BMP, JPG, PNG, MNG, JNG, GIF, DDS, TGA, PGM, PPM, PAM, PFM



