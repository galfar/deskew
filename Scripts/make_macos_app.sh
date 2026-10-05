#!/bin/bash
# Makes DeskewGui.app application bundle for macOS with GUI and CLI executables.
#
# Usage: Scripts/make_macos_app.sh <gui executable> <cli executable> <output folder>
#
# No libraries are bundled: all the file formats are read and written by Pascal
# code compiled into the CLI. Shared libraries the user has installed (libtiff,
# libjpeg-turbo, libwebp from Homebrew) are used instead when they are found.
#
# The bundle is signed ad hoc (needed on Apple Silicon), it is not notarized.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GUI_EXE="$1"
CLI_EXE="$2"
OUT_DIR="$3"

. "$ROOT/Scripts/get_versions.sh"

APP="$OUT_DIR/DeskewGui.app"
MACOS_DIR="$APP/Contents/MacOS"

rm -rf "$APP"
mkdir -p "$MACOS_DIR" "$APP/Contents/Resources"

cp "$GUI_EXE" "$MACOS_DIR/deskewgui"
cp "$CLI_EXE" "$MACOS_DIR/deskew"
chmod 755 "$MACOS_DIR/deskewgui" "$MACOS_DIR/deskew"
cp "$ROOT/Gui/deskewgui.icns" "$APP/Contents/Resources/deskewgui.icns"
printf 'APPL????' > "$APP/Contents/PkgInfo"

cat > "$APP/Contents/Info.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleDevelopmentRegion</key>
  <string>English</string>
  <key>CFBundleExecutable</key>
  <string>deskewgui</string>
  <key>CFBundleName</key>
  <string>Deskew GUI</string>
  <key>CFBundleDisplayName</key>
  <string>Deskew GUI</string>
  <key>CFBundleIdentifier</key>
  <string>net.galfarslair.deskewgui</string>
  <key>CFBundleIconFile</key>
  <string>deskewgui</string>
  <key>CFBundleInfoDictionaryVersion</key>
  <string>6.0</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
  <key>CFBundleSignature</key>
  <string>????</string>
  <key>CFBundleShortVersionString</key>
  <string>$GUI_VERSION</string>
  <key>CFBundleVersion</key>
  <string>$GUI_VERSION</string>
  <key>LSMinimumSystemVersion</key>
  <string>10.14</string>
  <key>NSHighResolutionCapable</key>
  <true/>
  <key>NSHumanReadableCopyright</key>
  <string>Marek Mauder, MPL 2.0</string>
  <key>CSResourcesFileMapped</key>
  <true/>
</dict>
</plist>
EOF

# Everything has to be signed on Apple Silicon
codesign --force --sign - "$MACOS_DIR/deskew"
codesign --force --sign - "$APP"

echo "Created $APP (GUI $GUI_VERSION, CLI $CLI_VERSION)"
