#!/bin/bash
# Versions of Deskew CLI and GUI taken from the sources:
#   CLI_VERSION from SAppVersion constant in MainUnit.pas, e.g. 1.35
#   GUI_VERSION from version info in Gui/deskewgui.lpi, e.g. 1.00
#
# Source it to get the variables, or run it to have them printed:
#   . Scripts/get_versions.sh
#   Scripts/get_versions.sh

_VERSIONS_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

CLI_VERSION=$(sed -n "s/.*SAppVersion = '\([0-9][0-9.]*\)'.*/\1/p" "$_VERSIONS_ROOT/MainUnit.pas" | head -1)

# Zero parts of the version are not stored in the project file
_GUI_MAJOR=$(sed -n 's/.*<MajorVersionNr Value="\([0-9]*\)".*/\1/p' "$_VERSIONS_ROOT/Gui/deskewgui.lpi" | head -1)
_GUI_MINOR=$(sed -n 's/.*<MinorVersionNr Value="\([0-9]*\)".*/\1/p' "$_VERSIONS_ROOT/Gui/deskewgui.lpi" | head -1)
GUI_VERSION=$(printf '%d.%02d' "${_GUI_MAJOR:-0}" "${_GUI_MINOR:-0}")

if [ -z "$CLI_VERSION" ]; then
  echo "Could not find CLI version in MainUnit.pas" >&2
  return 1 2>/dev/null || exit 1
fi

# Executed, not sourced
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  echo "CLI_VERSION=$CLI_VERSION"
  echo "GUI_VERSION=$GUI_VERSION"
fi
