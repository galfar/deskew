#!/bin/bash
# Builds release binaries of Deskew CLI and GUI for one platform with lazbuild.
# Binaries end up in Bin/ (deskew, deskewgui, on Linux also deskewgui-gtk2),
# Scripts/package_release.sh then makes the release archive out of them.
#
# Linux GUI is built with Qt6 (deskewgui, needs libQt6Pas, which the archive has
# next to it) and with GTK2 (deskewgui-gtk2, fallback for systems without Qt6).
# libqt6pas development files must be installed for the Qt6 build.
#
# Usage: Scripts/build_release.sh <target> [--cli-only]
#
# Targets: win-x64, win-x86, linux-x64, linux-arm64, linux-armv7, macos-arm64, macos-x64
#
# The target has to match the machine the script runs on, unless cross compilers
# (and LCL for the target) are installed, like i386-win32 on 64-bit Windows.
# With --cli-only only the command line tool is built, and with plain FPC
# (Scripts/compile.sh) so Lazarus is not needed. linux-armv7 is always built this way.
#
# Environment:
#   LAZBUILD     lazbuild executable (default: lazbuild in PATH)
#   LAZARUSDIR   passed to lazbuild as --lazarusdir when set (needed on Windows
#                when Lazarus IDE has rewritten its configuration)
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TARGET="${1:-}"
CLI_ONLY=0
[ "${2:-}" = "--cli-only" ] && CLI_ONLY=1

case "$TARGET" in
  win-x64)     CPU=x86_64;  OS=win64 ;;
  win-x86)     CPU=i386;    OS=win32 ;;
  linux-x64)   CPU=x86_64;  OS=linux ;;
  linux-arm64) CPU=aarch64; OS=linux ;;
  linux-armv7) CPU=arm;     OS=linux; CLI_ONLY=1 ;;
  macos-arm64) CPU=aarch64; OS=darwin ;;
  macos-x64)   CPU=x86_64;  OS=darwin ;;
  *)
    echo "Usage: $0 <target> [--cli-only]" >&2
    echo "Targets: win-x64, win-x86, linux-x64, linux-arm64, linux-armv7, macos-arm64, macos-x64" >&2
    exit 1 ;;
esac

cd "$ROOT"

if [ "$CLI_ONLY" = 1 ]; then
  echo "=== Building Deskew CLI for $TARGET with FPC"
  ./Scripts/compile.sh
  exit 0
fi

LAZBUILD="${LAZBUILD:-lazbuild}"
LAZ_ARGS=(-B --build-mode=Release --cpu=$CPU --os=$OS)
[ -n "${LAZARUSDIR:-}" ] && LAZ_ARGS+=("--lazarusdir=$LAZARUSDIR")

echo "=== Building Deskew CLI for $TARGET"
"$LAZBUILD" "${LAZ_ARGS[@]}" deskew.lpi

if [ "$OS" = linux ]; then
  echo "=== Building Deskew GUI for $TARGET with GTK2"
  "$LAZBUILD" "${LAZ_ARGS[@]}" --ws=gtk2 Gui/deskewgui.lpi
  mv -f Bin/deskewgui Bin/deskewgui-gtk2
  echo "=== Building Deskew GUI for $TARGET with Qt6"
  # Run path $ORIGIN: libQt6Pas.so.6 next to the executable is found first
  "$LAZBUILD" "${LAZ_ARGS[@]}" --ws=qt6 --opt='-k-rpath=$ORIGIN' Gui/deskewgui.lpi
else
  echo "=== Building Deskew GUI for $TARGET"
  "$LAZBUILD" "${LAZ_ARGS[@]}" Gui/deskewgui.lpi
fi

echo "=== Done, binaries are in Bin/"
