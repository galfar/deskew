set -e

APP_NAME=${1:-DeskewGui}
APP_TITLE=${2:-Deskew GUI}
BUNDLE_ID=${3:-net.galfarslair.deskewgui}
COPYRIGHT=${4:-©2018, Marek Mauder}
ICON_PATH=${5:-"../Gui/deskewgui.icns"}
ICON_NAME=$(basename $ICON_PATH .icns)

RELEASE_DIR=../_internal/MacRelease
CONTENT_DIR=$RELEASE_DIR/$APP_NAME.app/Contents

mkdir -p $RELEASE_DIR
rm -rf $RELEASE_DIR/*

# build executables
rm -f ../Bin/deskew-mac
rm -f ../Gui/deskewgui
lazbuild --build-mode=Release-macOS -q --no-write-project ../deskew.lpi
lazbuild --build-mode=Release-macOS -q --no-write-project ../Gui/deskewgui.lpi

# build app bundle
mkdir -p $CONTENT_DIR/MacOS
mkdir $CONTENT_DIR/Resources

cp ../Bin/deskew-mac $CONTENT_DIR/MacOS/
chmod 755 $CONTENT_DIR/MacOS/deskew-mac
cp ../Gui/deskewgui $CONTENT_DIR/MacOS/
chmod 755 $CONTENT_DIR/MacOS/deskewgui
cp $ICON_PATH $CONTENT_DIR/Resources/
chmod 644 $CONTENT_DIR/Resources/$ICON_NAME.icns

echo "APPL????" > $CONTENT_DIR/PkgInfo

cat <<EOT >> $CONTENT_DIR/Info.plist
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleDevelopmentRegion</key>
  <string>English</string>
  <key>CFBundleExecutable</key>
  <string>deskewgui</string>
  <key>CFBundleDisplayName</key>
  <string>$APP_TITLE</string>
  <key>CFBundleName</key>
  <string>$APP_TITLE</string>
  <key>CFBundleIdentifier</key>
  <string>$BUNDLE_ID</string>
  <key>CFBundleIconFile</key>
  <string>$ICON_NAME</string>
  <key>CFBundleInfoDictionaryVersion</key>
  <string>6.0</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
  <key>CFBundleSignature</key>
  <string>desk</string>
  <key>CSResourcesFileMapped</key>
  <true/>
  <key>NSHumanReadableCopyright</key>
  <string>$COPYRIGHT</string>
  <key>NSHighResolutionCapable</key>
  <true/>
</dict>
</plist>
EOT

# update version from Lazarus project file
MAJOR_VER=$(grep 'MajorVersionNr' ../Gui/deskewgui.lpi | grep -oE '[0-9]+' || true)
MAJOR_VER=${MAJOR_VER:-0} # if major=0 it's not included in project file
MINOR_VER=$(grep 'MinorVersionNr' ../Gui/deskewgui.lpi | grep -oE '[0-9]+')

plutil -insert CFBundleShortVersionString -string $MAJOR_VER.$MINOR_VER $CONTENT_DIR/Info.plist
plutil -insert CFBundleVersion -string $MAJOR_VER.$MINOR_VER $CONTENT_DIR/Info.plist

# create DMG from folder
DMG_NAME=$APP_NAME-$MAJOR_VER.$MINOR_VER.dmg

hdiutil create -srcfolder $RELEASE_DIR -volname $APP_NAME -format UDZO -ov $RELEASE_DIR/$DMG_NAME


echo "Finished OK!"