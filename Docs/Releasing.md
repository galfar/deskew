# Making a Release

CLI and GUI are released together, one archive per platform:
`deskew-<CLI version>-<target>.zip`.

Targets: `win-x64`, `win-x86`, `linux-x64`, `linux-arm64`, `linux-armv7` (CLI only),
`macos-arm64`, `macos-x64`.

## Scripts

| Script | What it does |
|---|---|
| `Scripts/get_versions.sh` | prints CLI version (from `SAppVersion` in `MainUnit.pas`) and GUI version (from `Gui/deskewgui.lpi`) |
| `Scripts/ci/publish_snapshot.sh` | CI: publishes the dev archives as the `snapshot` pre-release |
| `Scripts/ci/release_suffix.sh` | CI: `dev-<date>` suffix of the archive names for runs that are not `v*` tags, checks that the tag matches the version |
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
(`deskew-<version>-<target>.zip`). `linux-armv7` comes from the manually
started "Build on other architectures" workflow.

After every successful build of `master` the `snapshot` job of the CI workflow puts its archives
(dev names, all platforms of `ci.yml` and the WebUI) into the pre-release
[`snapshot`](https://github.com/galfar/deskew/releases/tag/snapshot)
(`Scripts/ci/publish_snapshot.sh`): the tag is moved to the built commit, the release text is
rewritten and the old archives replaced. Being a pre-release it is never the "Latest" release, so
it needs no care when releasing. A manual run of the CI workflow with the `snapshot_tag` input
publishes the same way under another tag, for testing the job (delete that release and tag
afterwards: `gh release delete <tag> --cleanup-tag`). Only the runs of `v*` tags have these names, and
they fail when the tag is not the version in the sources; other runs add the date of the commit:
`deskew-<version>-dev-<YYYYMMDD>-<target>.zip` (`Scripts/ci/release_suffix.sh`). The WebUI (static site, `Web/`) is packed by the
web job of the same workflow into `deskew-<version>-web.zip`.

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
   (Linux: both `deskewgui` and `deskewgui-gtk2`). WebUI: serve the extracted folder as its Readme
   says, footer shows the build date and commit of the tag.
7. Create the release as a draft, check it and publish:
   `gh release create vX.YY --draft --title "Deskew X.YY" --notes-file notes.md deskew-*.zip`
8. After the release:
   - Homebrew: update the tag URL and its sha256 in `Scripts/homebrew/deskew.rb` and commit it,
     copy it unchanged to `Formula/deskew.rb` of the tap <https://gitlab.com/deskew/homebrew>,
     push, and check with `brew update`, `brew upgrade deskew` and `brew test deskew`.
   - Scoop: in a clone of the bucket <https://gitlab.com/deskew/scoop> run
     `& "$env:USERPROFILE\scoop\apps\scoop\current\bin\checkver.ps1" -App deskew -Dir .\bucket -Update`
     (new version, URLs and hashes), commit, push, and check with `scoop update deskew`.
   - Docker: publishing the release starts the "Docker image" workflow (`.github/workflows/docker.yml`),
     which pushes `ghcr.io/galfar/deskew:X.YY` and `:latest` from the Linux archives. Check that it
     passed and that `docker run --rm ghcr.io/galfar/deskew:X.YY` prints the new version.
   - Close the fixed issues, update the home page, upload WebUI to live version hosting.

## Known Limits

- macOS binaries are signed ad hoc only, not notarized. Users have to remove the quarantine flag.
- No third party libraries for the file formats are in the archives. zlib, libjpeg-turbo (2.0+),
  libtiff and libwebp are used when the user's system has them, the Pascal codecs built in otherwise.
- Linux archives have `libQt6Pas.so.6`, the Qt6 binding of LCL (LGPL), taken from the libqt6pas
  package the GUI was built with on the CI runner (newer than what Ubuntu has, from
  <https://github.com/davidbannon/libqt6pas/releases>). Qt6 itself must be on the user's system,
  `deskewgui-gtk2` is there for systems without it.
