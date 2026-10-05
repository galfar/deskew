# Making a Release

CLI and GUI are released together, one archive per platform:
`deskew-<CLI version>-<target>.zip`.

Targets: `win-x64`, `win-x86`, `linux-x64`, `linux-arm64`, `linux-armv7` (CLI only),
`macos-arm64`, `macos-x64`.

## Scripts

| Script | What it does |
|---|---|
| `Scripts/get_versions.sh` | prints CLI version (from `SAppVersion` in `MainUnit.pas`) and GUI version (from `Gui/deskewgui.lpi`) |
| `Scripts/build_release.sh <target>` | builds CLI and GUI in release mode with lazbuild into `Bin/` (Linux GUI with Qt6 and GTK2) |
| `Scripts/package_release.sh <target>` | makes `Releases/deskew-<version>-<target>` folder and archive from the binaries in `Bin/`: executables, readmes, license, `Docs/`, `Examples/` |
| `Scripts/make_macos_app.sh` | called by the packaging on macOS: `DeskewGui.app` with GUI and CLI, signed ad hoc |

They run locally (Git Bash on Windows) and in CI. On Windows, where Lazarus is not in `PATH`:

```bash
LAZBUILD=/c/Programs/Lazarus/lazbuild.exe LAZARUSDIR='C:\Programs\Lazarus' ./Scripts/build_release.sh win-x64
./Scripts/package_release.sh win-x64
```

Each platform is built on that platform. The CI workflow (`.github/workflows/ci.yml`) does it for all
of them on every push to `master`, on `v*` tags and on manual runs, and uploads the archives as artifacts
(`deskew-<target>`). `linux-armv7` comes from the manually started "Build on other architectures" workflow.

## Checklist

1. Versions and dates:
   - CLI: `SAppVersion` in `MainUnit.pas`, and the release date in `SAppTitle` right below it
   - GUI: version info in `Gui/deskewgui.lpi`
   - `Readme.md`: version line, table of archives, Version History with the changes of the new version
     only; the same entry is added to `Docs/History.md`
   - `Gui/Readme.md`: version line and history
   - `Scripts/get_versions.sh` should print what you expect
2. Usage block in `Readme.md` matches `deskew -h`, `Docs/Usage.md` knows about new options,
   GUI passes them (`Gui/options.pas`).
3. Tests pass locally: `Bin/tests -a -p --format=plain`, `Bin/guitests -a -p --format=plain`,
   `Bin/RunTests.bat` or `Bin/runtests.sh`, `python3 Tests/cli/check_cli.py Bin/deskew`,
   Delphi build still compiles (see [Building.md](Building.md#tests)).
4. Push, wait for green CI on all the platforms. Look into the log of the packaging step:
   - Linux: required glibc version is printed for the binaries, and "deskewgui loads libQt6Pas.so.6
     from the archive folder" must be there
   - all: libraries the packaged command line tool uses on the runner (`deskew -s l`)
   - GUI smoke test steps started the GUIs of the archives (screenshots in `Bin/gui-smoke` of the
     `deskew-artifacts-*` of a manual run)
5. Tag `vX.YY`, push the tag, download the archives from the CI run of the tag
   (plus `linux-armv7` from the other workflow).
6. Try the archives: extract, run `Examples/run-examples` script, start the GUI and deskew a file with it
   (Linux: both `deskewgui` and `deskewgui-gtk2`).
7. Create the release as a draft, check it and publish:
   `gh release create vX.YY --draft --title "Deskew X.YY" --notes-file notes.md deskew-*.zip`
8. After the release: Homebrew formula (`Scripts/homebrew/deskew.rb`: tag URL and its sha256),
   close the fixed issues, update the home page.

## Known Limits

- macOS binaries are signed ad hoc only, not notarized. Users have to remove the quarantine flag.
- No third party libraries for the file formats are in the archives. zlib, libjpeg-turbo (2.0+),
  libtiff and libwebp are used when the user's system has them, the Pascal codecs built in otherwise.
- Linux archives have `libQt6Pas.so.6`, the Qt6 binding of LCL (LGPL), taken from the libqt6pas
  package the GUI was built with on the CI runner (newer than what Ubuntu has, from
  <https://github.com/davidbannon/libqt6pas/releases>). Qt6 itself must be on the user's system,
  `deskewgui-gtk2` is there for systems without it.
