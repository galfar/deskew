Deskew
=======================
by Marek Mauder
[http://galfar.vevb.net/deskew/](http://galfar.vevb.net/deskew/)
[https://bitbucket.org/galfar/app-deskew](https://bitbucket.org/galfar/app-deskew)

**v1.25 2018-05-19**

Overview
------------------------

Deskew is a command line tool for deskewing scanned text documents.
It uses Hough transform to detect "text lines" in the image. As an output, you get
an image rotated so that the lines are horizontal.

There are binaries built for these platforms (located in Bin folder):
Win32 (deskew.exe), Win64 (deskew64.exe), Linux 64bit (deskew), macOS (deskew-osx),
Linux ARMv7 (deskew-arm).

You can find some test images in TestImages folder and
scripts to run tests (`RunTests.bat` and `runtests.sh`) in Bin.
Note that scripts just call `deskew` command so you may need
to rename binary for your platform to just `deskew`.

License: MIT

Usage
------------------------

```
deskew [-o output] [-a angle] [-b color] [..] input
    input:         Input image file
  Options:
    -o output:     Output image file (default: out.png)
    -a angle:      Maximal skew angle in degrees (default: 10)
    -b color:      Background color in hex format RRGGBB|LL|AARRGGBB (default: black)
  Ext. options:
    -t a|treshold: Auto threshold or value in 0..255 (default: a)
    -r rect:       Skew detection only in content rectangle (pixels):
                   left,top,right,bottom (default: whole page)
    -f format:     Force output pixel format (values: b1|g8|rgb24|rgba32)
    -l angle:      Skip deskewing step if skew angle is smaller (default: 0.01)
    -s info:       Info dump (any combination of):
                   s - skew detection stats, p - program parameters, t - timings

  Supported file formats
    Input:  BMP, JPG, PNG, JNG, GIF, DDS, TGA, PBM, PGM, PPM, PAM, PFM, PSD, TIF
    Output: BMP, JPG, PNG, JNG, GIF, DDS, TGA, PGM, PPM, PAM, PFM, PSD, TIF
```

#### Notes

For TIFF support in Linux and macOS you need to have libtiff 4.x installed (package is usually called libtiff5).

Version History
------------------------
1.25 2018-05-19:

  - fix #6: Preserve DPI measurement system (TIFF)
  - fix #4: Output image not saved in requested format (when deskewing is skipped)
  - dynamic loading of libtiff library - adds TIFF support in macOS when libtiff is installed

1.21 2017-11-01:

  - fix #8: Cannot compile in Free Pascal 3.0+ (Windows) - Fails to link precompiled LibTiff library
  - fix #7: Windows FPC build fails with *Access violation exception* when loading certain TIFFs (especially those saved by Windows Photo Viewer etc.)

1.20 2016-09-01:

  - much faster rotation, especially when background color is set (>2x faster, 2x less memory)
  - can skip deskewing step if detected skew angle is lower than parameter
  - new option for timing of individual steps
  - fix: crash when last row of page is classified as text
  - misc: default back color is now opaque black, new forced output format "rgb24",  background color can define also alpha channel, nicer formatting of text output

1.10 2014-03-04:

  - TIFF support for Win64 and 32/64bit Linux
  - forced output formats
  - fix: output file names were always lowercase
  - fix: preserves resolution metadata (e.g. 300dpi) of input when writing output

1.00 2012-06-04:

  - background color
  - "area of interest" content rect
  - 64bit and Mac OSX support
  - PSD and TIFF (win32) support
  - show skew detection stats and program parameters

0.95 2010-12-28:

  - Added auto thresholding
  - Imaging library updated.

0.90 2010-02-12:

  - Initial version

Compiling Deskew
------------------------

Deskew is written in Object Pascal. You need
Free Pascal or Delphi to recompile it.

#### Tested Compilers
There are project files for these IDEs:

  1. Lazarus 1.8.2 (deskew.lpi)
  2. Delphi XE (deskew.dproj)

Additionaly, there is compile shell/batch scripts for FPC compiler `compile.sh` and
`Compile.bat`.

#### Target Platforms
Deskew is precompiled and was tested on these platforms:
Win32, Win64, Linux 64b, macOS 64b, Linux ARMv7

#### Source Code
Latest source code can be found here:
https://bitbucket.org/galfar/app-deskew

#### Dependencies
Vampyre Imaging Library is needed for compilation.
You can find it included in Imaging folder or get it at
http://imaginglib.sourceforge.net