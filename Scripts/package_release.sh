#!/bin/bash
# Makes the release archive of Deskew for one platform out of the binaries
# built by Scripts/build_release.sh: Releases/deskew-<version>-<target>.zip
#
# Usage: Scripts/package_release.sh <target> [--cli-only]
#
# Targets: win-x64, win-x86, linux-x64, linux-arm64, linux-armv7, macos-arm64, macos-x64
#
# Contents of the archive (in deskew-<version>-<target> folder):
#   deskew[.exe], deskewgui[.exe]   command line tool and GUI
#                                   (macOS: DeskewGui.app with both inside and "deskew" launcher)
#   Linux: deskewgui is the Qt6 build with libQt6Pas.so.6 next to it,
#          deskewgui-gtk2 the GTK2 build for systems without Qt6
#   Readme.md, Readme-GUI.md, LICENSE, Docs/ (Usage.md, History.md)
#   Examples/                       a few images and a script running Deskew on them
#
# No sources and no binaries for other platforms. With --cli-only (always for
# linux-armv7) there is no GUI inside.
#
# Must run on the platform of the target: the packaged command line tool is started
# to check it is really the current version (Bin/ can have old binaries of other
# platforms lying around), and the macOS bundle is made and signed with macOS tools.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TARGET="${1:-}"
CLI_ONLY=0
[ "${2:-}" = "--cli-only" ] && CLI_ONLY=1

case "$TARGET" in
  win-*)       CLI_BIN=deskew.exe; GUI_BIN=deskewgui.exe ;;
  linux-x64|linux-arm64) CLI_BIN=deskew; GUI_BIN="deskewgui deskewgui-gtk2" ;;
  linux-armv7) CLI_BIN=deskew;     GUI_BIN=; CLI_ONLY=1 ;;
  macos-*)     CLI_BIN=deskew;     GUI_BIN=deskewgui ;;
  *)
    echo "Usage: $0 <target> [--cli-only]" >&2
    echo "Targets: win-x64, win-x86, linux-x64, linux-arm64, linux-armv7, macos-arm64, macos-x64" >&2
    exit 1 ;;
esac

HOST_OS="$(uname -s)"
case "$TARGET:$HOST_OS" in
  win-*:MINGW*|win-*:MSYS*|win-*:CYGWIN*|linux-*:Linux|macos-*:Darwin) ;;
  *)
    echo "Target $TARGET cannot be packaged on this system ($HOST_OS)" >&2
    exit 1 ;;
esac

cd "$ROOT"
. Scripts/get_versions.sh

NAME="deskew-$CLI_VERSION-$TARGET"
STAGE="Releases/$NAME"

for bin in "$CLI_BIN" $([ "$CLI_ONLY" = 0 ] && echo $GUI_BIN); do
  if [ ! -f "Bin/$bin" ]; then
    echo "Bin/$bin not found, run Scripts/build_release.sh $TARGET first" >&2
    exit 1
  fi
done

echo "=== Packaging $NAME (CLI $CLI_VERSION, GUI $GUI_VERSION)"
rm -rf "$STAGE" "Releases/$NAME.zip"
mkdir -p "$STAGE/Docs/images" "$STAGE/Examples"

# --- Binaries, the same names on all platforms
case "$TARGET" in
  win-*)
    cp "Bin/$CLI_BIN" "$STAGE/deskew.exe"
    [ "$CLI_ONLY" = 0 ] && cp "Bin/$GUI_BIN" "$STAGE/deskewgui.exe"
    ;;
  linux-*)
    cp "Bin/$CLI_BIN" "$STAGE/deskew"
    if [ "$CLI_ONLY" = 0 ]; then
      cp Bin/deskewgui Bin/deskewgui-gtk2 "$STAGE/"
      # Qt6 binding of LCL the GUI was linked with, Qt6 itself comes from the system
      QT6PAS="$(ldd Bin/deskewgui | awk '/libQt6Pas\.so/ { print $3 }')"
      if [ ! -f "$QT6PAS" ]; then
        echo "ERROR: libQt6Pas used by Bin/deskewgui not found (is it the Qt6 build?)" >&2
        exit 1
      fi
      cp -L "$QT6PAS" "$STAGE/libQt6Pas.so.6"
      chmod 644 "$STAGE/libQt6Pas.so.6"
    fi
    chmod 755 "$STAGE"/deskew*
    ;;
  macos-*)
    if [ "$CLI_ONLY" = 0 ]; then
      ./Scripts/make_macos_app.sh "Bin/$GUI_BIN" "Bin/$CLI_BIN" "$STAGE"
      # Command line tool lives inside the bundle, this makes it easy to reach
      printf '%s\n' '#!/bin/sh' \
        '# Runs Deskew command line tool from the application bundle' \
        'exec "$(dirname "$0")/DeskewGui.app/Contents/MacOS/deskew" "$@"' > "$STAGE/deskew"
    else
      cp "Bin/$CLI_BIN" "$STAGE/deskew"
    fi
    chmod 755 "$STAGE/deskew"
    ;;
esac

# --- Documentation, links between the files are adjusted to the layout of the archive
# (docs for building Deskew are not in the archive, the Readme links to them on GitHub)
sed -e 's|(Gui/Readme\.md)|(Readme-GUI.md)|g' \
    -e 's|(Docs/Building\.md)|(https://github.com/galfar/deskew/blob/master/Docs/Building.md)|g' \
    Readme.md > "$STAGE/Readme.md"
if [ "$CLI_ONLY" = 0 ]; then
  sed -e 's|(\.\./Docs/|(Docs/|g' -e 's|(\.\./Readme\.md|(Readme.md|g' Gui/Readme.md > "$STAGE/Readme-GUI.md"
fi
cp LICENSE "$STAGE/LICENSE"
cp Docs/Usage.md "$STAGE/Docs/Usage.md"
sed -e 's|(\.\./Gui/Readme\.md|(../Readme-GUI.md|g' Docs/History.md > "$STAGE/Docs/History.md"
cp Docs/images/*.png "$STAGE/Docs/images/"

# --- Examples
cp TestImages/4.png      "$STAGE/Examples/book-page.png"
cp TestImages/F1550.jpg  "$STAGE/Examples/notes.jpg"
cp TestImages/1-g4.tif   "$STAGE/Examples/fax-1bit.tif"
cp TestImages/blank.png  "$STAGE/Examples/blank-page.png"
case "$TARGET" in
  win-*) cp Scripts/release/run-examples.bat "$STAGE/Examples/run-examples.bat" ;;
  *)     cp Scripts/release/run-examples.sh "$STAGE/Examples/run-examples.sh"
         chmod 755 "$STAGE/Examples/run-examples.sh" ;;
esac

# --- Checks
echo "Checking the packaged command line tool:"
# Whole output is read (closing the pipe early makes the tool complain), first line is the banner
BANNER="$("$STAGE/deskew" -h | tr -d '\r')"
BANNER="${BANNER%%$'\n'*}"
echo "$BANNER"
case "$BANNER" in
  "Deskew $CLI_VERSION "*) ;;
  *)
    echo "ERROR: packaged command line tool is not version $CLI_VERSION, is Bin/$CLI_BIN up to date?" >&2
    exit 1 ;;
esac

if [ "$HOST_OS" = Linux ]; then
  # The newest glibc symbol version tells the oldest distribution the binaries run on
  for bin in "$STAGE"/deskew* "$STAGE"/libQt6Pas.so.6; do
    [ -f "$bin" ] || continue
    echo "$(basename "$bin") needs glibc $(objdump -T "$bin" | grep -o 'GLIBC_[0-9.]*' | sort -V | tail -1 | cut -d_ -f2) or newer"
  done
  if [ "$CLI_ONLY" = 0 ]; then
    # The Qt6 GUI must load libQt6Pas from the archive, not from the system
    if ldd "$STAGE/deskewgui" | grep -q "$NAME/libQt6Pas.so.6"; then
      echo "deskewgui loads libQt6Pas.so.6 from the archive folder"
    else
      ldd "$STAGE/deskewgui" | grep -i qt6 >&2 || true
      echo "ERROR: deskewgui does not load libQt6Pas.so.6 next to it (run path \$ORIGIN missing?)" >&2
      exit 1
    fi
  fi
fi

# Shared libraries found on this machine or the Pascal codecs built in (only informative,
# the user's machine can have other libraries)
"$STAGE/deskew" -s l -g d TestImages/1-lzw.tif | tr -d '\r' | sed -n '/^Imaging libraries/,/^$/p'

# --- Archive: ZIP for all the platforms
cd Releases
ARCHIVE="$NAME.zip"
case "$TARGET" in
  win-*)
    if command -v zip > /dev/null; then
      zip -r -9 -q "$ARCHIVE" "$NAME"
    elif command -v 7z > /dev/null; then
      7z a -tzip -mx=9 -bso0 "$ARCHIVE" "$NAME"
    else
      # tar that comes with Windows can write ZIP files
      "$(cygpath "$SYSTEMROOT")/System32/tar.exe" -a -c -f "$ARCHIVE" "$NAME"
    fi
    ;;
  linux-*)
    # zip stores the Unix permissions and unzip restores them, so the binaries
    # and the example script stay executable
    if ! command -v zip > /dev/null; then
      echo "ERROR: zip is needed to make the archive (e.g. apt-get install zip)" >&2
      exit 1
    fi
    zip -r -9 -q "$ARCHIVE" "$NAME"
    ;;
  macos-*)
    # ditto keeps permissions, symlinks and signatures of the bundle
    ditto -c -k --keepParent "$NAME" "$ARCHIVE"
    ;;
esac

echo "=== Created Releases/$ARCHIVE ($(wc -c < "$ARCHIVE" | tr -d ' ') bytes)"
