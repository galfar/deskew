Deskew
=======================

by Marek Mauder \
<http://galfar.vevb.net/deskew> \
<https://bitbucket.org/galfar/app-deskew> \
<https://github.com/galfar/deskew>

**v1.30 2018-06-07**


Overview
------------------------

Deskew is a command line tool for deskewing scanned text documents.
It uses Hough transform to detect "text lines" in the image. As an output, you
get an image rotated so that the lines are horizontal.

There are binaries built for these platforms (located in Bin folder):
Win64 (`deskew.exe`), Win32 (`deskew32.exe`), Linux 64bit (`deskew`), macOS (`deskew-mac`), Linux ARMv7 (`deskew-arm`).

GUI frontend for this CLI tool is available as well (Windows, Linux, and macOS).

License: MIT

### Downloads And Releases

<https://github.com/galfar/deskew/releases> \
<https://bitbucket.org/galfar/app-deskew/downloads/>

Usage
------------------------

```console
Usage:
deskew [-o output] [-a angle] [-b color] [..] input
    input:         Input image file
  Options:
    -o output:     Output image file (default: out.png)
    -a angle:      Maximal expected skew angle (both directions) in degrees (default: 10)
    -b color:      Background color in hex format RRGGBB|LL|AARRGGBB (default: black)
  Ext. options:
    -q filter:     Resampling filter used for rotations (default: linear,
                   values: nearest|linear|cubic|lanczos)
    -t a|treshold: Auto threshold or value in 0..255 (default: a)
    -r rect:       Skew detection only in content rectangle (pixels):
                   left,top,right,bottom (default: whole page)
    -f format:     Force output pixel format (values: b1|g8|rgb24|rgba32)
    -l angle:      Skip deskewing step if skew angle is smaller (default: 0.01)
    -g flags:      Operational flags (any combination of):
                   c - auto crop, d - detect only (no output to file)
    -s info:       Info dump (any combination of):
                   s - skew detection stats, p - program parameters, t - timings
    -c specs:      Output compression specs for some file formats. Several specs
                   can be defined - delimited by commas. Supported specs:
                   jXX - JPEG compression quality, XX is in range [1,100(best)]
                   tSCHEME - TIFF compression scheme: none|lzw|rle|deflate|jpeg|g4

  Supported file formats
    Input:  BMP, JPG, PNG, JNG, GIF, DDS, TGA, PBM, PGM, PPM, PAM, PFM, TIF, PSD
    Output: BMP, JPG, PNG, JNG, GIF, DDS, TGA, PGM, PPM, PAM, PFM, TIF, PSD
```

### Notes

For TIFF support in Linux and macOS you need to have libtiff 4.x installed (package is usually called libtiff5).

For macOS you can download prebuilt libtiff binaries here: <https://bitbucket.org/galfar/app-deskew/downloads/TiffLibBins-macOS.zip>. Just put the files inside the archive to the same folder as `deskew-mac` executable.

You can find some test images in TestImages folder and
scripts to run tests (`RunTests.bat` and `runtests.sh`) in Bin.
By default scripts just call `deskew` command but you can pass a different one as a parameter
(e.g. `runtests.sh deskew-arm`).

### Bugs, Issues, Proposals 

File them here: \
<https://bitbucket.org/galfar/app-deskew/issues> \
<https://github.com/galfar/deskew/issues>


Version History
------------------------

v1.30 2019-06-07:

- fix #15: Better image quality after rotation - better default and also selectable nearest|linear|cubic|lanczos filtering
- fix #5: Detect skew angle only (no rotation done) - optionally only skew detection
- fix #17: Optional auto-crop after rotation
- fix #3: Command line option to set output compression - now for TIFF and JPEG
- fix #12: Bad behavior when an output is given and no deskewing is needed
- libtiff in macOS is now picked up also when binaries are put directly in the directory with deskew
- text output is flushed after every write (Linux/Unix): it used to be flushed only when writing to device but not file/pipe.

v1.25 2018-05-19:

- fix #6: Preserve DPI measurement system (TIFF)
- fix #4: Output image not saved in requested format (when deskewing is skipped)
- dynamic loading of libtiff library - adds TIFF support in macOS when libtiff is installed

v1.21 2017-11-01:

- fix #8: Cannot compile in Free Pascal 3.0+ (Windows) - Fails to link precompiled LibTiff library
- fix #7: Windows FPC build fails with *Access violation exception* when loading certain TIFFs (especially those saved by Windows Photo Viewer etc.)

v1.20 2016-09-01:

- much faster rotation, especially when background color is set (>2x faster, 2x less memory)
- can skip deskewing step if detected skew angle is lower than parameter
- new option for timing of individual steps
- fix: crash when last row of page is classified as text
- misc: default back color is now opaque black, new forced output format  "rgb24",  background color can define also alpha channel, nicer formatting of text output

v1.10 2014-03-04:

- TIFF support for Win64 and 32/64bit Linux
- forced output formats
- fix: output file names were always lowercase
- fix: preserves resolution metadata (e.g. 300dpi) of input when writing output

v1.00 2012-06-04:

- background color
- "area of interest" content rectangle
- 64bit and Mac OSX support
- PSD and TIFF (win32) support
- show skew detection stats and program parameters

v0.95 2010-12-28:

- Added auto thresholding

v0.90 2010-02-12:

- Initial version


Compiling Deskew
------------------------

Deskew is written in Object Pascal. You need Free Pascal or Delphi to recompile it.

### Tested Compilers

There are project files for these IDEs:

  1. Lazarus 2.0.2 (deskew.lpi)
  2. Delphi XE + 10.3 (deskew.dproj)

Additionally, there are compile shell/batch scripts for standalone FPC compiler in `Scripts` folder.

### Supported/Tested Platforms

Deskew is precompiled and was tested on these platforms:
Win32, Win64, Linux 64bit, macOS 64bit, Linux ARMv7

### Source Code

Latest source code can be found here: \
<https://bitbucket.org/galfar/app-deskew> \
<https://github.com/galfar/deskew>

### Dependencies

Vampyre Imaging Library is needed for compilation and it's included in Deskew's repo in Imaging folder.
