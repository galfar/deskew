GUI Frontend for Deskew Command Line Tool
===========================================

by Marek Mauder  
<https://galfar.vevb.net/deskew>  
<https://github.com/galfar/deskew>


**v1.00 2021-06-01**


Overview
------------------------

With Deskew GUI it’s easy to deskew many files without writing shell scripts.

It needs the command line tool which is called for the each input file. You can also set the explicit path to the command line tool in the program itself (default is the same folder as GUI).

Prebuilt executables for Windows and Linux are available in the download – you just place them to the same folder as the command line tool. Version for macOS is a bit more convenient – it’s a self-contained app bundle with CLI tool already inside.

You can set the basic and most of the advanced options for deskewing in the GUI.

License: MPL 2.0

### Downloads And Releases

<https://github.com/galfar/deskew/releases>  
<https://galfar.vevb.net/deskew#downloads>

### Bugs, Issues, Proposals 

File them here, mark as "GUI":  
<https://github.com/galfar/deskew/issues>  

Version History
------------------------

v1.00 2021-06-01:

- added many missing options/parameters for CLI (resampling, compression, auto crop, ...)
- allows passing extra parameters to CLI so all features are usable at least in this form (e.g. when pairing with newer version of CLI tool)
- fixed default window is too big for some common screens
- fixed crash on macOS when opening "About" from main menu

v0.90 2019-01-04:

- Initial version

Compiling Deskew
------------------------

Deskew GUI is written in Object Pascal using Lazarus and its LCL framework. Any recent Lazarus version should work.
