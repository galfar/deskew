set -e

RELEASE_DIR=../_internal/Release
rm -f $RELEASE_DIR/*.zip

# get version from Lazarus project file
source ./get_gui_versions.sh
VERSION_STR=$MAJOR_VER.$MINOR_VER

echo "Building RELEASE zip for v$VERSION_STR"

# create the zip in Linux/WSL so the permissions are presserved 
chmod 755 $RELEASE_DIR/deskewgui
chmod 644 $RELEASE_DIR/DeskewGui-$VERSION_STR.dmg

ZIP_CMD="zip -j -9 $RELEASE_DIR/DeskewGui-$VERSION_STR.zip -@"  

echo $RELEASE_DIR/deskewgui | $ZIP_CMD
echo $RELEASE_DIR/deskewgui.exe | $ZIP_CMD
echo $RELEASE_DIR/DeskewGui-$VERSION_STR.dmg | $ZIP_CMD

# check expected contents 
OUTPUT=$(unzip -Z $RELEASE_DIR/DeskewGui-$VERSION_STR.zip)

echo "$OUTPUT" | grep -q "^-r.xr.xr.x.*unx.*deskewgui$"
echo "$OUTPUT" | grep -q "^-.*unx.*deskewgui.exe$"
echo "$OUTPUT" | grep -q "^-r..r..r...*unx.*DeskewGui-$VERSION_STR.dmg$"

echo "Finished OK with $RELEASE_DIR/DeskewGui-$VERSION_STR.zip!" 