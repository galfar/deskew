set -e 

cd ..

# extract version number from main unit
VERSION_STR=$(grep -oP "(?<=SAppTitle = 'Deskew )([0-9\.]+)" MainUnit.pas)

ZIP_NAME=Deskew-$VERSION_STR.zip
RELEASE_DIR=_internal/Release-CLI
CONTENTS_DIR=$RELEASE_DIR/Deskew 
rm -rf $RELEASE_DIR/*
mkdir -p $CONTENTS_DIR

# clone sources (without .hg folder)
hg archive -t files $CONTENTS_DIR
rm $CONTENTS_DIR/.hg* 

# copy binaries to release folder (must be prebuilt - no platfrom where we could cross-compile all atm)
cp Bin/deskew $CONTENTS_DIR/Bin
cp Bin/deskew.exe $CONTENTS_DIR/Bin
cp Bin/deskew32.exe $CONTENTS_DIR/Bin
cp Bin/deskew-mac $CONTENTS_DIR/Bin
cp Bin/deskew-arm $CONTENTS_DIR/Bin
chmod 755 $CONTENTS_DIR/Bin/deskew*

# build the ZIP (in Linux/WSL so the exe permissions are presserved)
cd $RELEASE_DIR
zip -r -9 -q $ZIP_NAME Deskew 

# check expected contents 
OUTPUT=$(unzip -Z $ZIP_NAME)

echo "$OUTPUT" | grep -q "^-r.xr.xr.x.*unx.*deskew$"
echo "$OUTPUT" | grep -q "^-r.xr.xr.x.*unx.*deskew-mac$"
echo "$OUTPUT" | grep -q "^-r.xr.xr.x.*unx.*deskew-arm$"

echo "Finished OK with $RELEASE_DIR/$ZIP_NAME!" 



