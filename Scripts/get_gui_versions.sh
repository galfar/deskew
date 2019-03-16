set -e

# get version from Lazarus project file
MAJOR_VER=$(grep 'MajorVersionNr' ../Gui/deskewgui.lpi | grep -oE '[0-9]+' || true)
MAJOR_VER=${MAJOR_VER:-0} # if major=0 it's not included in project file
MINOR_VER=$(grep 'MinorVersionNr' ../Gui/deskewgui.lpi | grep -oE '[0-9]+')

