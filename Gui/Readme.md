GUI Frontend for Deskew Command Line Tool
===========================================

by Marek Mauder
<https://galfar.vevb.net/deskew>
<https://github.com/galfar/deskew>


**v1.00 2026-10-05**


Overview
------------------------

With Deskew GUI it’s easy to deskew many files without writing shell scripts.

It needs the command line tool which is called for each input file. Both come together
in the release archives, so there is nothing to set up:

- Windows and Linux: `deskewgui` and `deskew` executables are in the same folder.
- macOS: `DeskewGui.app` is a self-contained application bundle with the command line tool inside.

When the command line tool is not next to the GUI, it is looked for in `PATH` (on macOS also in
the Homebrew folders), so `deskew` installed by a package manager works too. You can also set
an explicit path, or just a name found in `PATH`, in Advanced Options. When the tool is not found,
the GUI says where it looked for it.

All the controls have tooltips explaining what they do. You can set the basic and most of the
advanced options of the command line tool:

- in the main window: output folder and file format, content area for the skew detection (found
  automatically or given by page margins), background color, cropping to the size of the input
- in Advanced Options: resampling filter, output pixel format, JPEG quality and TIFF compression,
  thresholding, max angle, angle step, skip angle, print resolution (DPI), detect only,
  forced rotation, printing of the parameters, saving of the work image

Anything else can be passed in "Extra command line parameters" (for example content rectangle
`-r 100,150,900,1200`). See [Usage.md](../Docs/Usage.md) for the description of all
the command line options.

Output of the command line tool for each file is shown in the output log, including the detected
skew angle and the detection confidence. Pages where no reliable angle was found (blank pages etc.)
are not rotated, the log says why.

Settings are stored in `deskewgui.ini` file in the folder of the application if it can be written to,
in the configuration folder of the user otherwise.

License: MPL 2.0

### Downloads And Releases

<https://github.com/galfar/deskew/releases>
<https://galfar.vevb.net/deskew#downloads>

### Bugs, Issues, Proposals

File them here, mark as "GUI":
<https://github.com/galfar/deskew/issues>

Version History
------------------------

v1.00 2026-10-05:

- needs Deskew command line tool v1.35 or newer
- tooltips for all the controls
- new options in the main window: automatic detection of the content area, page margins
- new advanced option: forced rotation
- new advanced options: angle step, DPI override, and "detect only" (just report the skew angle, no output files)
- new TIFF compression choices "As input" and "As input (lossless)": keep the compression scheme of the input TIFF; the lossless variant uses LZW instead of JPEG so JPEG-compressed TIFFs are never recompressed lossily
- extra command line parameters now support quoted values and repeated spaces
- command line tool is also looked for in `PATH` (and Homebrew folders on macOS), a clear message when it is not found
- failed files are easy to spot: number of failures while running, red summary and a list of the failed files at the end of the output log
- output log uses a monospaced font with all the widget sets and on macOS
- fixed: skip angle 0 and max angle just next to the default value were ignored
- fixed behavior with empty output folder
- macOS ARM64 build

v0.95 2021-06-01:

- added many missing options/parameters for CLI (resampling, compression, auto crop, ...)
- allows passing extra parameters to CLI so all features are usable at least in this form (e.g. when pairing with newer version of CLI tool)
- fixed default window is too big for some common screens
- fixed crash on macOS when opening "About" from main menu

v0.90 2019-01-04:

- Initial version

Compiling Deskew GUI
------------------------

Deskew GUI is written in Object Pascal using Lazarus and its LCL framework. Any recent Lazarus version should work
(releases are built with Lazarus 4.x): open `deskewgui.lpi` or run `lazbuild --build-mode=Release Gui/deskewgui.lpi`.
