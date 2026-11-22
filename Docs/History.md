# Deskew Version History

Changes of the command line tool in each version. Versions of the GUI are in
[GUI's Readme](../Gui/Readme.md#version-history).

v1.35 2026-10-05:

- skew detection tells when it cannot be trusted: blank pages and pages without text lines are not rotated anymore (#71), detection confidence is printed, `-g f` forces the rotation
- more robust skew angle: a few long lines (edges of pages in book scans, borders) cannot outvote the text anymore
- new `-r auto`: skew detection only in automatically found content area (#43)
- new `-m`: content area defined by page margins (#31)
- content rectangle and margins in pixels, percents, millimeters, centimeters or inches
- new `-d`: angle step of the detection (#58)
- new `-p`: print resolution (DPI) override (#11)
- new `-c tinput` and `tinput-lossless`: TIFF output compression same as of the input (#30)
- new `-s j` and `-s r`: machine readable JSON report on standard output or in a file
- skew detection is several times faster on large and dense pages (precomputed SinCos)
- new `-h`, invalid values of options and unknown options are refused with a clear message (#68)
- fix: wrong automatic threshold for scans with more than 16.7 million pixels of one gray level (e.g. A4 at 600 DPI)
- fix: rotation with nearest filter read past the end of the image
- fix #44: impossible to set background color to fully transparent
- fix #32: auto thresholding honors the content rectangle
- fix #20: saving output file in unsupported format silently failed
- fix #54: TIFF G4 compression forces 1-bit pixel format
- default output file name is `deskewed-<input name>.png` next to the input file (#28)
- `-o` can be a folder, the output gets the name of the input file there
- output that would overwrite the input file is refused
- new `-s l`: shows the libraries used for zlib, JPEG, TIFF and WebP
- work image of `-s w` is named after the output file (`<output file name>.work.png`), has the detection area and the found content marked
- "auto crop" flag is now "crop to input size"
- TIFF is read and written without libtiff on all platforms, zlib, libjpeg-turbo, libtiff and libwebp are used when the system has them
- BigTIFF, QOI and WebP file formats, images with over 4 GiB of pixel data and over 2^31 pixels (#73)
- **DDS, MNG, JNG and PSD file formats are not supported anymore**
- less memory needed: about 2 times the size of the image for deskewing, 1.4 times for detect only
- macOS and Linux ARM64 builds
- WebUI: Deskew running in the web browser (WebAssembly)
- releases have one archive for each platform, with CLI and GUI together

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
