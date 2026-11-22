# Building Deskew

Deskew is written in Object Pascal. The command line tool compiles with Free Pascal (with or without
Lazarus) and with Delphi, the GUI needs Lazarus.

- [Projects](#projects)
- [Building from the command line](#building-from-the-command-line)
- [Imaging library and its options](#imaging-library-and-its-options)
- [Tests](#tests)
- [Supported platforms](#supported-platforms)
- [Release archives](#release-archives)

## Projects

| Project | For | |
|---|---|---|
| `deskew.lpi` | Lazarus 4.x with Free Pascal 3.2.2 | command line tool (`deskew.lpr`) |
| `deskew.dproj` | Delphi XE3 and newer | command line tool (`deskew.dpr`) |
| `Gui/deskewgui.lpi` | Lazarus 4.x | GUI (LCL, Free Pascal only) |
| `Tests/tests.lpi`, `Tests/tests.dproj` | Lazarus, Delphi | unit tests |
| `Tests/gui/guitests.lpi` | Lazarus 4.x | unit tests of the GUI (Free Pascal only) |
| `deskew-group.lpg` | Lazarus | project group: all the Lazarus projects above |
| `deskew-group.groupproj` | Delphi | project group: command line tool and unit tests |

The command line tool and its unit tests have an `.lpr` file for Free Pascal and a `.dpr` file
for Delphi, the units are shared by both compilers. The GUI and its unit tests are Free Pascal
only and have just the `.lpr` file. Binaries go to `Bin/` and have the same names on all the
platforms (`deskew`, `deskewgui`), other targets are selected with `--cpu` and `--os` of
lazbuild. The minimal macOS version the GUI needs for linking is set in the project.

## Building from the command line

```bash
# Command line tool with Lazarus (build modes: Release [default], Debug)
lazbuild --build-mode=Release deskew.lpi

# Command line tool with plain Free Pascal, no Lazarus needed (Scripts/Compile.bat on Windows)
./Scripts/compile.sh

# GUI (build modes: Debug [default], Release); on Linux --ws=qt6 or --ws=gtk2 selects the widget set
lazbuild --build-mode=Release Gui/deskewgui.lpi

# Unit tests, Bin/tests
lazbuild Tests/tests.lpi          # or ./Scripts/tests-compile.sh
```

`Scripts/compile-wasm.sh` builds the command line tool for WebAssembly (`wasm32-wasip1`) into
`Web/deskew.wasm` for the WebUI. It needs FPC trunk with the cross compiler for the target,
see [WebUI's Readme](../Web/Readme.md).

## Imaging library and its options

Image files are read and written by [Vampyre Imaging Library](https://github.com/galfar/imaginglib). A copy of it is in `Imaging/`, made by
`Scripts/sync_imaging.sh` from the upstream repository, so do not change anything there.
No other libraries are needed for compiling.

Deskew's own settings of the library are in `ImagingUserOptions.inc` in the root folder. It is used
when `IMAGING_USER_OPTIONS` is defined, all the projects and scripts do that (the root folder and
`Imaging/` must be on the include path). Our options:

- leaves out the file formats Deskew does not need (`DONT_LINK_...` defines),
- makes Imaging use zlib, libjpeg-turbo (2.0+) and libtiff shared libraries when they are found at
  run time (`ZLIB_USE_DYNLIB`, `JPEG_USE_TURBO_DYNLIB`, `TIFF_USE_LIBTIFF_DYNLIB`). libwebp is looked
  for without a define. Pascal codecs compiled in are used when the libraries are not there.

`-dDESKEW_PASCAL_CODECS_ONLY` given to the compiler makes a build that never looks for the shared
libraries. Do not define `TIFF_NO_BUNDLED` or `WEBP_NO_BUNDLED`, they leave out the Pascal codecs.
All the options of the library are listed in `Imaging/ImagingOptions.inc`.
`deskew -s l` shows which libraries a build uses.

## Tests

Unit tests are in `Tests/` (FPCUnit with Free Pascal, DUnit with Delphi). Test images are in
`TestImages/` and the scripts in `Bin/` run Deskew with various options on them. They only check that
every run ends with success, the output goes to `Bin/TestOut/`.

`Tests/cli/check_cli.py` (Python 3, standard library only) checks the results of the command line
tool through its JSON report: the skew angle, outcome and threshold found for every test image must
be the same as in `Tests/expected-detection.json`, and it checks output formats and sizes, copying of
the input when nothing changes, TIFF compression and resolution of the output, and that errors end
with a non-zero exit code. After an intended change of the detection, `--update` rewrites the
expected results (check them in `git diff`).

GUI parts that work without forms (settings, the command line parameters made from them, helpers)
have their own unit tests in `Tests/gui/` (FPCUnit, built with the LCL "nogui" widget set, no display
needed). They also run the command line tool with the parameters the GUI makes and compare the
parameters it reports, so the GUI and the tool cannot drift apart unnoticed.

```bash
lazbuild Tests/tests.lpi                      # unit tests -> Bin/tests
lazbuild Tests/gui/guitests.lpi               # GUI unit tests -> Bin/guitests (needs Bin/deskew)
cd Bin
./tests -a -p --format=plain --no-addresses   # unit tests (Delphi build: tests.exe -text)
./guitests -a -p --format=plain               # GUI unit tests
./runtests.sh                                 # smoke tests, Linux and macOS
RunTests.bat                                  # smoke tests, Windows
python3 ../Tests/cli/check_cli.py ./deskew    # results of the command line tool
```

By default the scripts call `deskew` but you can pass a different executable as a parameter
(e.g. `./runtests.sh pascal/deskew`).

The WebAssembly build is tested in Node (`node Tests/web/test-wasi.mjs`, needs `Web/deskew.wasm`,
it compares the detection with the same expected results) and the WebUI in headless Chromium
(`Tests/web/test-page.mjs`, Playwright).

CI (`.github/workflows/ci.yml`) runs all of them on Windows, Linux and macOS on every push to `master`.
On Linux the command line tool is tested with the shared libraries and once more as a build with
the Pascal codecs only. CI also starts the GUI builds (`Scripts/ci/gui_smoke.sh`, `gui_smoke.ps1`)
and checks they show their main window, screenshots are in `Bin/gui-smoke` (artifacts of manual runs).
"Build on other architectures" workflow (started manually) builds and tests on CPUs emulated with QEMU.

## Supported platforms

Deskew is compiled and tested on these platforms: Win32, Win64, Linux x86_64, Linux ARM aarch64 and v7,
macOS ARM64 and x86_64, and WebAssembly (`wasm32-wasip1`, for the WebUI).

It also compiles for PowerPC64LE and RISC-V 64. These platforms are not officially supported.

## Release archives

Release archives are made by `Scripts/build_release.sh` and `Scripts/package_release.sh`,
see [Releasing.md](Releasing.md).
