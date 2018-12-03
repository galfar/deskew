RELEASE_DIR=../_internal/MacRelease
CONTENT_DIR=$RELEASE_DIR/DeskewGui.app/Contents

mkdir -p $RELEASE_DIR
rm -rf $RELEASE_DIR/*

# build executable
lazbuild --build-mode=Release-macOS --no-write-project ../Gui/deskewgui.lpi

# build app bundle
mkdir -p $CONTENT_DIR/MacOS
mkdir $CONTENT_DIR/Resources

cp ../Gui/deskewgui $CONTENT_DIR/MacOS/
chmod 755 $CONTENT_DIR/MacOS/deskewgui

cp ../Gui/deskewgui.icns $CONTENT_DIR/Resources/
chmod 644 $CONTENT_DIR/Resources/deskewgui.icns

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
  <string>Deskew GUI</string>
  <key>CFBundleName</key>
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
  <string>desk</string>
  <key>CSResourcesFileMapped</key>
  <true/>
  <key>NSHumanReadableCopyright</key>
  <string>©2018, Marek Mauder</string>
  <key>NSHighResolutionCapable</key>
  <true/>
</dict>
</plist>
EOT

# update version from Lazarus project file
MAJOR_VER=$(grep 'MajorVersionNr' ../Gui/deskewgui.lpi | grep -oE '[0-9]+')
MAJOR_VER=${MAJOR_VER:-0} # if major=0 it's not included in project file
MINOR_VER=$(grep 'MinorVersionNr' ../Gui/deskewgui.lpi | grep -oE '[0-9]+')

plutil -insert CFBundleShortVersionString -string $MAJOR_VER.$MINOR_VER $CONTENT_DIR/Info.plist
plutil -insert CFBundleVersion -string $MAJOR_VER.$MINOR_VER $CONTENT_DIR/Info.plist

# create DMG from folder
DMG_NAME=DeskewGui-$MAJOR_VER.$MINOR_VER.dmg

hdiutil create -srcfolder $RELEASE_DIR -volname DeskewGui -format UDZO -ov $RELEASE_DIR/$DMG_NAME