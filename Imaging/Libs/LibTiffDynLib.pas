{
  Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  https://imaginglib.sourceforge.io
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
}

{ Bindings for libtiff 4.0+ shared library (DLL/SO/dylib) loaded at run time.
  Only the functions used by the libtiff backend of ImagingTiffBackend are bound.
  Nothing is loaded automatically, call LoadLibTiff (or LoadLibTiffFromFile)
  and check the result. Older libtiff 3.x (no BigTIFF) is refused. }
unit LibTiffDynLib;

{$I ImagingOptions.inc}

{$IFNDEF HAS_DYNLIBS}
  {$MESSAGE FATAL 'dynamic libraries are not supported on this target'}
{$ENDIF}

interface

uses
  ImagingTypes; // Int32, UInt32... for old Delphi

type
  va_list = Pointer;

  tmsize_t  = NativeInt;  // C ssize_t
  toff_t    = Int64;    // C uint64_t, Int64 for Delphi 7 (no UInt64 there)
  poff_t    = ^toff_t;
  tsample_t = UInt16;
  thandle_t = Pointer;
  tdata_t   = Pointer;
  { Directory numbers: tdir_t is 16-bit before libtiff 4.5 and 32-bit since.
    Declared 32-bit here; parameters work with both (the callee reads the low
    16 bits on little endian CPUs), the result of TIFFNumberOfDirectories is
    masked by its wrapper below for old versions. }
  tdir_t    = UInt32;

const
  TIFF_NOTYPE                           = 0;
  TIFF_BYTE                             = 1;       { 8-bit unsigned integer }
  TIFF_ASCII                            = 2;       { 8-bit bytes w/ last byte null }
  TIFF_SHORT                            = 3;       { 16-bit unsigned integer }
  TIFF_LONG                             = 4;       { 32-bit unsigned integer }
  TIFF_RATIONAL                         = 5;       { 64-bit unsigned fraction }
  TIFF_SBYTE                            = 6;       { !8-bit signed integer }
  TIFF_UNDEFINED                        = 7;       { !8-bit untyped data }
  TIFF_SSHORT                           = 8;       { !16-bit signed integer }
  TIFF_SLONG                            = 9;       { !32-bit signed integer }
  TIFF_SRATIONAL                        = 10;      { !64-bit signed fraction }
  TIFF_FLOAT                            = 11;      { !32-bit IEEE floating point }
  TIFF_DOUBLE                           = 12;      { !64-bit IEEE floating point }
  TIFF_IFD                              = 13;      { %32-bit unsigned integer (offset) }
  TIFF_UNICODE                          = 14;
  TIFF_COMPLEX                          = 15;
  TIFF_LONG8                            = 16;
  TIFF_SLONG8                           = 17;
  TIFF_IFD8                             = 18;

  TIFFTAG_SUBFILETYPE                   = 254;     { subfile data descriptor }
    FILETYPE_REDUCEDIMAGE               = $1;      { reduced resolution version }
    FILETYPE_PAGE                       = $2;      { one page of many }
    FILETYPE_MASK                       = $4;      { transparency mask }
  TIFFTAG_OSUBFILETYPE                  = 255;     { kind of data in subfile }
    OFILETYPE_IMAGE                     = 1;       { full resolution image data }
    OFILETYPE_REDUCEDIMAGE              = 2;       { reduced size image data }
    OFILETYPE_PAGE                      = 3;       { one page of many }
  TIFFTAG_IMAGEWIDTH                    = 256;     { image width in pixels }
  TIFFTAG_IMAGELENGTH                   = 257;     { image height in pixels }
  TIFFTAG_BITSPERSAMPLE                 = 258;     { bits per channel (sample) }
  TIFFTAG_COMPRESSION                   = 259;   { data compression technique }
    COMPRESSION_NONE                    = 1;     { dump mode }
    COMPRESSION_CCITTRLE                = 2;     { CCITT modified Huffman RLE }
    COMPRESSION_CCITTFAX3               = 3;     { CCITT Group 3 fax encoding }
    COMPRESSION_CCITT_T4                = 3;       { CCITT T.4 (TIFF 6 name) }
    COMPRESSION_CCITTFAX4	              = 4;	   { CCITT Group 4 fax encoding }
    COMPRESSION_CCITT_T6                = 4;       { CCITT T.6 (TIFF 6 name) }
    COMPRESSION_LZW		                  = 5;       { Lempel-Ziv  & Welch }
    COMPRESSION_OJPEG		                = 6;	   { !6.0 JPEG }
    COMPRESSION_JPEG		                = 7;	   { %JPEG DCT compression }
    COMPRESSION_NEXT		                = 32766;   { NeXT 2-bit RLE }
    COMPRESSION_CCITTRLEW	              = 32771;   { #1 w/ word alignment }
    COMPRESSION_PACKBITS	              = 32773;   { Macintosh RLE }
    COMPRESSION_THUNDERSCAN	            = 32809;   { ThunderScan RLE }
    { codes 32895-32898 are reserved for ANSI IT8 TIFF/IT <dkelly@apago.com) }
    COMPRESSION_IT8CTPAD	              = 32895;   { IT8 CT w/padding }
    COMPRESSION_IT8LW		                = 32896;   { IT8 Linework RLE }
    COMPRESSION_IT8MP		                = 32897;   { IT8 Monochrome picture }
    COMPRESSION_IT8BL		                = 32898;   { IT8 Binary line art }
    { compression codes 32908-32911 are reserved for Pixar }
    COMPRESSION_PIXARFILM	              = 32908;   { Pixar companded 10bit LZW }
    COMPRESSION_PIXARLOG	              = 32909;   { Pixar companded 11bit ZIP }
    COMPRESSION_DEFLATE		              = 32946;   { Deflate compression }
    COMPRESSION_ADOBE_DEFLATE           = 8;       { Deflate compression, as recognized by Adobe }
    { compression code 32947 is reserved for Oceana Matrix <dev@oceana.com> }
    COMPRESSION_DCS                     = 32947;   { Kodak DCS encoding }
    COMPRESSION_JBIG		                = 34661;   { ISO JBIG }
    COMPRESSION_SGILOG		              = 34676;   { SGI Log Luminance RLE }
    COMPRESSION_SGILOG24	              = 34677;   { SGI Log 24-bit packed }
    COMPRESSION_JP2000                  = 34712;   { Leadtools JPEG2000 }
  TIFFTAG_PHOTOMETRIC                   = 262;     { photometric interpretation }
    PHOTOMETRIC_MINISWHITE              = 0;       { min value is white }
    PHOTOMETRIC_MINISBLACK              = 1;       { min value is black }
    PHOTOMETRIC_RGB                     = 2;       { RGB color model }
    PHOTOMETRIC_PALETTE                 = 3;       { color map indexed }
    PHOTOMETRIC_MASK                    = 4;       { $holdout mask }
    PHOTOMETRIC_SEPARATED               = 5;       { !color separations }
    PHOTOMETRIC_YCBCR                   = 6;       { !CCIR 601 }
    PHOTOMETRIC_CIELAB                  = 8;       { !1976 CIE L*a*b* }
    PHOTOMETRIC_ICCLAB                  = 9;       { ICC L*a*b* [Adobe TIFF Technote 4] }
    PHOTOMETRIC_ITULAB                  = 10;      { ITU L*a*b* }
    PHOTOMETRIC_LOGL                    = 32844;   { CIE Log2(L) }
    PHOTOMETRIC_LOGLUV                  = 32845;   { CIE Log2(L) (u',v') }
  TIFFTAG_THRESHHOLDING                 = 263;     { thresholding used on data }
    THRESHHOLD_BILEVEL                  = 1;       { b&w art scan }
    THRESHHOLD_HALFTONE                 = 2;       { or dithered scan }
    THRESHHOLD_ERRORDIFFUSE             = 3;       { usually floyd-steinberg }
  TIFFTAG_CELLWIDTH                     = 264;     { +dithering matrix width }
  TIFFTAG_CELLLENGTH                    = 265;     { +dithering matrix height }
  TIFFTAG_FILLORDER                     = 266;     { data order within a byte }
    FILLORDER_MSB2LSB                   = 1;       { most significant -> least }
    FILLORDER_LSB2MSB                   = 2;       { least significant -> most }
  TIFFTAG_DOCUMENTNAME                  = 269;     { name of doc. image is from }
  TIFFTAG_IMAGEDESCRIPTION              = 270;     { info about image }
  TIFFTAG_MAKE                          = 271;     { scanner manufacturer name }
  TIFFTAG_MODEL                         = 272;     { scanner model name/number }
  TIFFTAG_STRIPOFFSETS                  = 273;     { offsets to data strips }
  TIFFTAG_ORIENTATION                   = 274;     { +image orientation }
    ORIENTATION_TOPLEFT                 = 1;       { row 0 top, col 0 lhs }
    ORIENTATION_TOPRIGHT                = 2;       { row 0 top, col 0 rhs }
    ORIENTATION_BOTRIGHT                = 3;       { row 0 bottom, col 0 rhs }
    ORIENTATION_BOTLEFT                 = 4;       { row 0 bottom, col 0 lhs }
    ORIENTATION_LEFTTOP                 = 5;       { row 0 lhs, col 0 top }
    ORIENTATION_RIGHTTOP                = 6;       { row 0 rhs, col 0 top }
    ORIENTATION_RIGHTBOT                = 7;       { row 0 rhs, col 0 bottom }
    ORIENTATION_LEFTBOT                 = 8;       { row 0 lhs, col 0 bottom }
  TIFFTAG_SAMPLESPERPIXEL               = 277;     { samples per pixel }
  TIFFTAG_ROWSPERSTRIP                  = 278;     { rows per strip of data }
  TIFFTAG_STRIPBYTECOUNTS               = 279;     { bytes counts for strips }
  TIFFTAG_MINSAMPLEVALUE                = 280;     { +minimum sample value }
  TIFFTAG_MAXSAMPLEVALUE                = 281;     { +maximum sample value }
  TIFFTAG_XRESOLUTION                   = 282;     { pixels/resolution in x }
  TIFFTAG_YRESOLUTION                   = 283;     { pixels/resolution in y }
  TIFFTAG_PLANARCONFIG                  = 284;     { storage organization }
    PLANARCONFIG_CONTIG                 = 1;       { single image plane }
    PLANARCONFIG_SEPARATE               = 2;       { separate planes of data }
  TIFFTAG_PAGENAME                      = 285;     { page name image is from }
  TIFFTAG_XPOSITION                     = 286;     { x page offset of image lhs }
  TIFFTAG_YPOSITION                     = 287;     { y page offset of image lhs }
  TIFFTAG_FREEOFFSETS                   = 288;     { +byte offset to free block }
  TIFFTAG_FREEBYTECOUNTS                = 289;     { +sizes of free blocks }

  {matched with tag reference up to this point}

  TIFFTAG_GRAYRESPONSEUNIT              = 290;     { $gray scale curve accuracy }
    GRAYRESPONSEUNIT_10S                = 1;       { tenths of a unit }
    GRAYRESPONSEUNIT_100S               = 2;       { hundredths of a unit }
    GRAYRESPONSEUNIT_1000S              = 3;       { thousandths of a unit }
    GRAYRESPONSEUNIT_10000S             = 4;       { ten-thousandths of a unit }
    GRAYRESPONSEUNIT_100000S            = 5;       { hundred-thousandths }
  TIFFTAG_GRAYRESPONSECURVE             = 291;     { $gray scale response curve }
  TIFFTAG_GROUP3OPTIONS                 = 292;     { 32 flag bits }
  TIFFTAG_T4OPTIONS                     = 292;     { TIFF 6.0 proper name alias }
    GROUP3OPT_2DENCODING                = $1;      { 2-dimensional coding }
    GROUP3OPT_UNCOMPRESSED              = $2;      { data not compressed }
    GROUP3OPT_FILLBITS                  = $4;      { fill to byte boundary }
  TIFFTAG_GROUP4OPTIONS                 = 293;     { 32 flag bits }
  TIFFTAG_T6OPTIONS                     = 293;     { TIFF 6.0 proper name }
    GROUP4OPT_UNCOMPRESSED              = $2;      { data not compressed }
  TIFFTAG_RESOLUTIONUNIT                = 296;     { units of resolutions }
    RESUNIT_NONE                        = 1;       { no meaningful units }
    RESUNIT_INCH                        = 2;       { english }
    RESUNIT_CENTIMETER                  = 3;       { metric }
  TIFFTAG_PAGENUMBER                    = 297;     { page numbers of multi-page }
  TIFFTAG_COLORRESPONSEUNIT             = 300;     { $color curve accuracy }
    COLORRESPONSEUNIT_10S               = 1;       { tenths of a unit }
    COLORRESPONSEUNIT_100S              = 2;       { hundredths of a unit }
    COLORRESPONSEUNIT_1000S             = 3;       { thousandths of a unit }
    COLORRESPONSEUNIT_10000S            = 4;       { ten-thousandths of a unit }
    COLORRESPONSEUNIT_100000S           = 5;       { hundred-thousandths }
  TIFFTAG_TRANSFERFUNCTION              = 301;     { !colorimetry info }
  TIFFTAG_SOFTWARE                      = 305;     { name & release }
  TIFFTAG_DATETIME                      = 306;     { creation date and time }
  TIFFTAG_ARTIST                        = 315;     { creator of image }
  TIFFTAG_HOSTCOMPUTER                  = 316;     { machine where created }
  TIFFTAG_PREDICTOR                     = 317;     { prediction scheme w/ LZW }
  TIFFTAG_WHITEPOINT                    = 318;     { image white point }
  TIFFTAG_PRIMARYCHROMATICITIES         = 319;     { !primary chromaticities }
  TIFFTAG_COLORMAP                      = 320;     { RGB map for pallette image }
  TIFFTAG_HALFTONEHINTS                 = 321;     { !highlight+shadow info }
  TIFFTAG_TILEWIDTH                     = 322;     { !rows/data tile }
  TIFFTAG_TILELENGTH                    = 323;     { !cols/data tile }
  TIFFTAG_TILEOFFSETS                   = 324;     { !offsets to data tiles }
  TIFFTAG_TILEBYTECOUNTS                = 325;     { !byte counts for tiles }
  TIFFTAG_BADFAXLINES                   = 326;     { lines w/ wrong pixel count }
  TIFFTAG_CLEANFAXDATA                  = 327;     { regenerated line info }
    CLEANFAXDATA_CLEAN                  = 0;       { no errors detected }
    CLEANFAXDATA_REGENERATED            = 1;       { receiver regenerated lines }
    CLEANFAXDATA_UNCLEAN                = 2;       { uncorrected errors exist }
  TIFFTAG_CONSECUTIVEBADFAXLINES        = 328;     { max consecutive bad lines }
  TIFFTAG_SUBIFD                        = 330;     { subimage descriptors }
  TIFFTAG_INKSET                        = 332;     { !inks in separated image }
    INKSET_CMYK                         = 1;       { !cyan-magenta-yellow-black color }
    INKSET_MULTIINK                     = 2;       { !multi-ink or hi-fi color }
  TIFFTAG_INKNAMES                      = 333;     { !ascii names of inks }
  TIFFTAG_NUMBEROFINKS                  = 334;     { !number of inks }
  TIFFTAG_DOTRANGE                      = 336;     { !0% and 100% dot codes }
  TIFFTAG_TARGETPRINTER                 = 337;     { !separation target }
  TIFFTAG_EXTRASAMPLES                  = 338;     { !info about extra samples }
    EXTRASAMPLE_UNSPECIFIED             = 0;       { !unspecified data }
    EXTRASAMPLE_ASSOCALPHA              = 1;       { !associated alpha data }
    EXTRASAMPLE_UNASSALPHA              = 2;       { !unassociated alpha data }
  TIFFTAG_SAMPLEFORMAT                  = 339;     { !data sample format }
    SAMPLEFORMAT_UINT                   = 1;       { !unsigned integer data }
    SAMPLEFORMAT_INT                    = 2;       { !signed integer data }
    SAMPLEFORMAT_IEEEFP                 = 3;       { !IEEE floating point data }
    SAMPLEFORMAT_VOID                   = 4;       { !untyped data }
    SAMPLEFORMAT_COMPLEXINT             = 5;       { !complex signed int }
    SAMPLEFORMAT_COMPLEXIEEEFP          = 6;       { !complex ieee floating }
  TIFFTAG_SMINSAMPLEVALUE               = 340;     { !variable MinSampleValue }
  TIFFTAG_SMAXSAMPLEVALUE               = 341;     { !variable MaxSampleValue }
  TIFFTAG_CLIPPATH                      = 343;     { %ClipPath [Adobe TIFF technote 2] }
  TIFFTAG_XCLIPPATHUNITS                = 344;     { %XClipPathUnits [Adobe TIFF technote 2] }
  TIFFTAG_YCLIPPATHUNITS                = 345;     { %YClipPathUnits [Adobe TIFF technote 2] }
  TIFFTAG_INDEXED                       = 346;     { %Indexed [Adobe TIFF Technote 3] }
  TIFFTAG_JPEGTABLES                    = 347;     { %JPEG table stream }
  TIFFTAG_OPIPROXY                      = 351;     { %OPI Proxy [Adobe TIFF technote] }
  { Tags 512-521 are obsoleted by Technical Note #2
  which specifies a revised JPEG-in-TIFF scheme. }
  TIFFTAG_JPEGPROC                      = 512;     { !JPEG processing algorithm }
    JPEGPROC_BASELINE                   = 1;       { !baseline sequential }
    JPEGPROC_LOSSLESS                   = 14;      { !Huffman coded lossless }
  TIFFTAG_JPEGIFOFFSET                  = 513;     { !pointer to SOI marker }
  TIFFTAG_JPEGIFBYTECOUNT               = 514;     { !JFIF stream length }
  TIFFTAG_JPEGRESTARTINTERVAL           = 515;     { !restart interval length }
  TIFFTAG_JPEGLOSSLESSPREDICTORS        = 517;     { !lossless proc predictor }
  TIFFTAG_JPEGPOINTTRANSFORM            = 518;     { !lossless point transform }
  TIFFTAG_JPEGQTABLES                   = 519;     { !Q matrice offsets }
  TIFFTAG_JPEGDCTABLES                  = 520;     { !DCT table offsets }
  TIFFTAG_JPEGACTABLES                  = 521;     { !AC coefficient offsets }
  TIFFTAG_YCBCRCOEFFICIENTS             = 529;     { !RGB -> YCbCr transform }
  TIFFTAG_YCBCRSUBSAMPLING              = 530;     { !YCbCr subsampling factors }
  TIFFTAG_YCBCRPOSITIONING              = 531;     { !subsample positioning }
    YCBCRPOSITION_CENTERED              = 1;       { !as in PostScript Level 2 }
    YCBCRPOSITION_COSITED               = 2;       { !as in CCIR 601-1 }
  TIFFTAG_REFERENCEBLACKWHITE           = 532;     { !colorimetry info }
  TIFFTAG_XMLPACKET                     = 700;     { %XML packet [Adobe XMP technote 9-14-02] (dkelly@apago.com) }
  TIFFTAG_OPIIMAGEID                    = 32781;   { %OPI ImageID [Adobe TIFF technote] }
  { tags 32952-32956 are private tags registered to Island Graphics }
  TIFFTAG_REFPTS                        = 32953;   { image reference points }
  TIFFTAG_REGIONTACKPOINT               = 32954;   { region-xform tack point }
  TIFFTAG_REGIONWARPCORNERS             = 32955;   { warp quadrilateral }
  TIFFTAG_REGIONAFFINE                  = 32956;   { affine transformation mat }
  { tags 32995-32999 are private tags registered to SGI }
  TIFFTAG_MATTEING                      = 32995;   { $use ExtraSamples }
  TIFFTAG_DATATYPE                      = 32996;   { $use SampleFormat }
  TIFFTAG_IMAGEDEPTH                    = 32997;   { z depth of image }
  TIFFTAG_TILEDEPTH                     = 32998;   { z depth/data tile }
  { tags 33300-33309 are private tags registered to Pixar }
  { TIFFTAG_PIXAR_IMAGEFULLWIDTH and TIFFTAG_PIXAR_IMAGEFULLLENGTH are set when an image has been cropped out of a larger image.
    They reflect the size of the original uncropped image. The TIFFTAG_XPOSITION and TIFFTAG_YPOSITION can be used to determine the
    position of the smaller image in the larger one. }
  TIFFTAG_PIXAR_IMAGEFULLWIDTH          = 33300;   { full image size in x }
  TIFFTAG_PIXAR_IMAGEFULLLENGTH         = 33301;   { full image size in y }
  { Tags 33302-33306 are used to identify special image modes and data used by Pixar's texture formats. }
  TIFFTAG_PIXAR_TEXTUREFORMAT           = 33302;   { texture map format }
  TIFFTAG_PIXAR_WRAPMODES               = 33303;   { s & t wrap modes }
  TIFFTAG_PIXAR_FOVCOT                  = 33304;   { cotan(fov) for env. maps }
  TIFFTAG_PIXAR_MATRIX_WORLDTOSCREEN    = 33305;
  TIFFTAG_PIXAR_MATRIX_WORLDTOCAMERA    = 33306;
  { tag 33405 is a private tag registered to Eastman Kodak }
  TIFFTAG_WRITERSERIALNUMBER            = 33405;   { device serial number }
  { tag 33432 is listed in the 6.0 spec w/ unknown ownership }
  TIFFTAG_COPYRIGHT                     = 33432;   { copyright string }
  { IPTC TAG from RichTIFF specifications }
  TIFFTAG_RICHTIFFIPTC                  = 33723;
  { 34016-34029 are reserved for ANSI IT8 TIFF/IT <dkelly@apago.com) }
  TIFFTAG_IT8SITE                       = 34016;   { site name }
  TIFFTAG_IT8COLORSEQUENCE              = 34017;   { color seq. [RGB,CMYK,etc] }
  TIFFTAG_IT8HEADER                     = 34018;   { DDES Header }
  TIFFTAG_IT8RASTERPADDING              = 34019;   { raster scanline padding }
  TIFFTAG_IT8BITSPERRUNLENGTH           = 34020;   { # of bits in short run }
  TIFFTAG_IT8BITSPEREXTENDEDRUNLENGTH   = 34021;   { # of bits in long run }
  TIFFTAG_IT8COLORTABLE                 = 34022;   { LW colortable }
  TIFFTAG_IT8IMAGECOLORINDICATOR        = 34023;   { BP/BL image color switch }
  TIFFTAG_IT8BKGCOLORINDICATOR          = 34024;   { BP/BL bg color switch }
  TIFFTAG_IT8IMAGECOLORVALUE            = 34025;   { BP/BL image color value }
  TIFFTAG_IT8BKGCOLORVALUE              = 34026;   { BP/BL bg color value }
  TIFFTAG_IT8PIXELINTENSITYRANGE        = 34027;   { MP pixel intensity value }
  TIFFTAG_IT8TRANSPARENCYINDICATOR      = 34028;   { HC transparency switch }
  TIFFTAG_IT8COLORCHARACTERIZATION      = 34029;   { color character. table }
  TIFFTAG_IT8HCUSAGE                    = 34030;   { HC usage indicator }
  TIFFTAG_IT8TRAPINDICATOR              = 34031;   { Trapping indicator (untrapped=0, trapped=1) }
  TIFFTAG_IT8CMYKEQUIVALENT             = 34032;   { CMYK color equivalents }
  { tags 34232-34236 are private tags registered to Texas Instruments }
  TIFFTAG_FRAMECOUNT                    = 34232;   { Sequence Frame Count }
  { tag 34750 is a private tag registered to Adobe? }
  TIFFTAG_ICCPROFILE                    = 34675;   { ICC profile data }
  { tag 34377 is private tag registered to Adobe for PhotoShop }
  TIFFTAG_PHOTOSHOP                     = 34377;
  { tag 34750 is a private tag registered to Pixel Magic }
  TIFFTAG_JBIGOPTIONS                   = 34750;   { JBIG options }
  { tags 34908-34914 are private tags registered to SGI }
  TIFFTAG_FAXRECVPARAMS                 = 34908;   { encoded Class 2 ses. parms }
  TIFFTAG_FAXSUBADDRESS                 = 34909;   { received SubAddr string }
  TIFFTAG_FAXRECVTIME                   = 34910;   { receive time (secs) }
  { tags 37439-37443 are registered to SGI <gregl@sgi.com> }
  TIFFTAG_STONITS                       = 37439;   { Sample value to Nits }
  { tag 34929 is a private tag registered to FedEx }
  TIFFTAG_FEDEX_EDR                     = 34929;   { unknown use }
  { tag 65535 is an undefined tag used by Eastman Kodak }
  TIFFTAG_DCSHUESHIFTVALUES             = 65535;   { hue shift correction data }
  { The following are ``pseudo tags'' that can be used to control codec-specific functionality. These tags are not written to file.
    Note that these values start at 0xffff+1 so that they'll never collide with Aldus-assigned tags. }
  TIFFTAG_FAXMODE                       = 65536;   { Group 3/4 format control }
    FAXMODE_CLASSIC                     = $0;      { default, include RTC }
    FAXMODE_NORTC                       = $1;      { no RTC at end of data }
    FAXMODE_NOEOL                       = $2;      { no EOL code at end of row }
    FAXMODE_BYTEALIGN                   = $4;      { byte align row }
    FAXMODE_WORDALIGN                   = $8;      { word align row }
    FAXMODE_CLASSF                      = FAXMODE_NORTC;        { TIFF Class F }
  TIFFTAG_JPEGQUALITY                   = 65537;   { Compression quality level }
  { Note: quality level is on the IJG 0-100 scale.  Default value is 75 }
  TIFFTAG_JPEGCOLORMODE                 = 65538;   { Auto RGB<=>YCbCr convert? }
    JPEGCOLORMODE_RAW                   = $0;      { no conversion (default) }
    JPEGCOLORMODE_RGB                   = $1;      { do auto conversion }
  TIFFTAG_JPEGTABLESMODE                = 65539;   { What to put in JPEGTables }
    JPEGTABLESMODE_QUANT                = $1;      { include quantization tbls }
    JPEGTABLESMODE_HUFF                 = $2;      { include Huffman tbls }
  { Note: default is JPEGTABLESMODE_QUANT | JPEGTABLESMODE_HUFF }
  TIFFTAG_FAXFILLFUNC                   = 65540;   { G3/G4 fill function }
  TIFFTAG_PIXARLOGDATAFMT               = 65549;   { PixarLogCodec I/O data sz }
    PIXARLOGDATAFMT_8BIT                = 0;       { regular u_char samples }
    PIXARLOGDATAFMT_8BITABGR            = 1;       { ABGR-order u_chars }
    PIXARLOGDATAFMT_11BITLOG            = 2;       { 11-bit log-encoded (raw) }
    PIXARLOGDATAFMT_12BITPICIO          = 3;       { as per PICIO (1.0==2048) }
    PIXARLOGDATAFMT_16BIT               = 4;       { signed short samples }
    PIXARLOGDATAFMT_FLOAT               = 5;       { IEEE float samples }
  { 65550-65556 are allocated to Oceana Matrix <dev@oceana.com> }
  TIFFTAG_DCSIMAGERTYPE                 = 65550;   { imager model & filter }
    DCSIMAGERMODEL_M3                   = 0;       { M3 chip (1280 x 1024) }
    DCSIMAGERMODEL_M5                   = 1;       { M5 chip (1536 x 1024) }
    DCSIMAGERMODEL_M6                   = 2;       { M6 chip (3072 x 2048) }
    DCSIMAGERFILTER_IR                  = 0;       { infrared filter }
    DCSIMAGERFILTER_MONO                = 1;       { monochrome filter }
    DCSIMAGERFILTER_CFA                 = 2;       { color filter array }
    DCSIMAGERFILTER_OTHER               = 3;       { other filter }
  TIFFTAG_DCSINTERPMODE                 = 65551;   { interpolation mode }
    DCSINTERPMODE_NORMAL                = 0;       { whole image, default }
    DCSINTERPMODE_PREVIEW               = 1;       { preview of image (384x256) }
  TIFFTAG_DCSBALANCEARRAY               = 65552;   { color balance values }
  TIFFTAG_DCSCORRECTMATRIX              = 65553;   { color correction values }
  TIFFTAG_DCSGAMMA                      = 65554;   { gamma value }
  TIFFTAG_DCSTOESHOULDERPTS             = 65555;   { toe & shoulder points }
  TIFFTAG_DCSCALIBRATIONFD              = 65556;   { calibration file desc }
  { Note: quality level is on the ZLIB 1-9 scale. Default value is -1 }
  TIFFTAG_ZIPQUALITY                    = 65557;   { compression quality level }
  TIFFTAG_PIXARLOGQUALITY               = 65558;   { PixarLog uses same scale }
  { 65559 is allocated to Oceana Matrix <dev@oceana.com> }
  TIFFTAG_DCSCLIPRECTANGLE              = 65559;   { area of image to acquire }
  TIFFTAG_SGILOGDATAFMT                 = 65560;   { SGILog user data format }
    SGILOGDATAFMT_FLOAT                 = 0;       { IEEE float samples }
    SGILOGDATAFMT_16BIT                 = 1;       { 16-bit samples }
    SGILOGDATAFMT_RAW                   = 2;       { uninterpreted data }
    SGILOGDATAFMT_8BIT                  = 3;       { 8-bit RGB monitor values }
  TIFFTAG_SGILOGENCODE                  = 65561;   { SGILog data encoding control }
    SGILOGENCODE_NODITHER               = 0;       { do not dither encoded values }
    SGILOGENCODE_RANDITHER              = 1;       { randomly dither encd values }


  { Flags to pass to TIFFPrintDirectory to control printing of data structures that are potentially very large. Bit-or these flags to
    enable printing multiple items. }
  TIFFPRINT_NONE                        = $0;      { no extra info }
  TIFFPRINT_STRIPS                      = $1;      { strips/tiles info }
  TIFFPRINT_CURVES                      = $2;      { color/gray response curves }
  TIFFPRINT_COLORMAP                    = $4;      { colormap }
  TIFFPRINT_JPEGQTABLES                 = $100;    { JPEG Q matrices }
  TIFFPRINT_JPEGACTABLES                = $200;    { JPEG AC tables }
  TIFFPRINT_JPEGDCTABLES                = $200;    { JPEG DC tables }


  TIFF_ANY                              = TIFF_NOTYPE;   { for field descriptor searching }
  TIFF_VARIABLE                         = -1;      { marker for variable length tags }
  TIFF_SPP                              = -2;      { marker for SamplesPerPixel tags }
  TIFF_VARIABLE2                        = -3;      { marker for uint32 var-length tags }

  FIELD_CUSTOM                          = 65;

 {added for LibTiff 3.9.4 by Alex (leontyyy@gmail.com) Dec.2011}
  TIFFTAG_EXIFIFD                       = 34665;   { pointer to the Exif IFD }
  EXIFTAG_FOCALLENGTH                   = 37386;   { focal length }
  EXIFTAG_FOCALLENGTHIN35MMFILM         = 41989;   { indicates the equivalent focal length assuming a 35mm film camera, in mm }
  EXIFTAG_EXIFVERSION                   = 36864;   { version of exif format }
  EXIFTAG_DATETIMEDIGITIZED             = 36868;   { date and time when the image was stored as digital data }
  EXIFTAG_DATETIMEORIGINAL              = 36867;   { date and time when the original image data was generated }
  EXIFTAG_EXPOSURETIME                  = 33434;   { exposure time, given in seconds }
  EXIFTAG_FNUMBER                       = 33437;   { F number }
  EXIFTAG_EXPOSUREPROGRAM               = 34850;   { class of the program used by the camera to set exposure }
  EXIFTAG_SPECTRALSENSITIVITY           = 34852;   { spectral sensitivity of each channel of the camera used }
  EXIFTAG_ISOSPEEDRATINGS               = 34855;   { ISO Speed and ISO Latitude }
  EXIFTAG_OECF                          = 34856;   { Opto-Electric Conversion Function }
  EXIFTAG_COMPONENTSCONFIGURATION       = 37121;   { meaning of each component }
  EXIFTAG_COMPRESSEDBITSPERPIXEL        = 37122;   { compression mode }
  EXIFTAG_SHUTTERSPEEDVALUE             = 37377;   { shutter speed }
  EXIFTAG_APERTUREVALUE                 = 37378;   { lens aperture }
  EXIFTAG_BRIGHTNESSVALUE               = 37379;   { brightness }
  EXIFTAG_EXPOSUREBIASVALUE             = 37380;   { exposure bias }
  EXIFTAG_MAXAPERTUREVALUE              = 37381;   { maximum lens aperture }
  EXIFTAG_SUBJECTDISTANCE               = 37382;   { distance to the subject in meters }
  EXIFTAG_METERINGMODE                  = 37383;   { metering mode }
  EXIFTAG_LIGHTSOURCE                   = 37384;   { light source }
  EXIFTAG_FLASH                         = 37385;   { flash }
  EXIFTAG_SUBJECTAREA                   = 37396;   { subject area (in exif ver.2.2) }
  EXIFTAG_MAKERNOTE                     = 37500;   { manufacturer notes }
  EXIFTAG_USERCOMMENT                   = 37510;   { user comments }
  EXIFTAG_SUBSECTIME                    = 37520;   { DateTime subseconds }
  EXIFTAG_SUBSECTIMEORIGINAL            = 37521;   { DateTimeOriginal subseconds }
  EXIFTAG_SUBSECTIMEDIGITIZED           = 37522;   { DateTimeDigitized subseconds }
  EXIFTAG_FLASHPIXVERSION               = 40960;   { FlashPix format version }
  EXIFTAG_COLORSPACE                    = 40961;   { color space information }
  EXIFTAG_PIXELXDIMENSION               = 40962;   { valid image width }
  EXIFTAG_PIXELYDIMENSION               = 40963;   { valid image height }
  EXIFTAG_RELATEDSOUNDFILE              = 40964;   { related audio file }
  EXIFTAG_FLASHENERGY                   = 41483;   { flash energy }
  EXIFTAG_SPATIALFREQUENCYRESPONSE      = 41484;   { spatial frequency response }
  EXIFTAG_FOCALPLANEXRESOLUTION         = 41486;   { focal plane X resolution }
  EXIFTAG_FOCALPLANEYRESOLUTION         = 41487;   { focal plane Y resolution }
  EXIFTAG_FOCALPLANERESOLUTIONUNIT      = 41488;   { focal plane resolution unit }
  EXIFTAG_SUBJECTLOCATION               = 41492;   { subject location }
  EXIFTAG_EXPOSUREINDEX                 = 41493;   { exposure index }
  EXIFTAG_SENSINGMETHOD                 = 41495;   { sensing method }
  EXIFTAG_FILESOURCE                    = 41728;   { file source }
  EXIFTAG_SCENETYPE                     = 41729;   { scene type }
  EXIFTAG_CFAPATTERN                    = 41730;   { CFA pattern }
  EXIFTAG_CUSTOMRENDERED                = 41985;   { custom image processing (in exif ver.2.2) }
  EXIFTAG_EXPOSUREMODE                  = 41986;   { exposure mode (in exif ver.2.2) }
  EXIFTAG_WHITEBALANCE                  = 41987;   { white balance (in exif ver.2.2) }
  EXIFTAG_DIGITALZOOMRATIO              = 41988;   { digital zoom ratio (in exif ver.2.2) }
  EXIFTAG_SCENECAPTURETYPE              = 41990;   { scene capture type (in exif ver.2.2) }
  EXIFTAG_GAINCONTROL                   = 41991;   { gain control (in exif ver.2.2) }
  EXIFTAG_CONTRAST                      = 41992;   { contrast (in exif ver.2.2) }
  EXIFTAG_SATURATION                    = 41993;   { saturation (in exif ver.2.2) }
  EXIFTAG_SHARPNESS                     = 41994;   { sharpness (in exif ver.2.2) }
  EXIFTAG_DEVICESETTINGDESCRIPTION      = 41995;   { device settings description (in exif ver.2.2) }
  EXIFTAG_SUBJECTDISTANCERANGE          = 41996;   { subject distance range (in exif ver.2.2) }
  EXIFTAG_IMAGEUNIQUEID                 = 42016;   { Unique image ID (in exif ver.2.2) }

type
  PTIFF = Pointer;
  PTIFFRGBAImage = Pointer;

  TIFFReadWriteProc = function(handle: thandle_t; buf: tdata_t; size: tmsize_t): tmsize_t; cdecl;
  TIFFSeekProc = function(handle: thandle_t; off: toff_t; whence: Integer): toff_t; cdecl;
  TIFFCloseProc = function(handle: thandle_t): Integer; cdecl;
  TIFFSizeProc = function(handle: thandle_t): toff_t; cdecl;
  TIFFMapFileProc = function(handle: thandle_t; var pbase: tdata_t; var psize: toff_t): Integer; cdecl;
  TIFFUnmapFileProc = procedure(handle: thandle_t; base: tdata_t; size: toff_t); cdecl;
  TIFFExtendProc = procedure(Handle: PTIFF); cdecl;
  TIFFErrorHandler = procedure(Module: PAnsiChar; Format: PAnsiChar; Params: va_list); cdecl;
  TIFFInitMethod = function(Handle: PTIFF; Scheme: Integer): Integer; cdecl;

  PTIFFCodec = ^TIFFCodec;
  TIFFCodec = record
    Name: PAnsiChar;
    Scheme: Word;
    Init: TIFFInitMethod;
  end;

  PTIFFFieldInfo = ^TIFFFieldInfo;
  TIFFFieldInfo = record
    FieldTag: Cardinal;              { field's tag }
    FieldReadCount: Smallint;        { read count/TIFF_VARIABLE/TIFF_SPP }
    FieldWriteCount: Smallint;       { write count/TIFF_VARIABLE }
    FieldType: Integer;              { type of associated data }
    FieldBit: Word;                  { bit in fieldsset bit vector }
    FieldOkToChange: Byte;           { if true, can change while writing }
    FieldPassCount: Byte;            { if true, pass dir count on set }
    FieldName: PAnsiChar;            { ASCII name }
  end;

  PTIFFTagValue = ^TIFFTagValue;
  TIFFTagValue = record
    Info: PTIFFFieldInfo;
    Count: Integer;
    Value: Pointer;
  end;

var
  TIFFGetVersion: function: PAnsiChar; cdecl;
  TIFFClientOpen: function(Name, Mode: PAnsiChar; ClientData: thandle_t;
    ReadProc, WriteProc: TIFFReadWriteProc; SeekProc: TIFFSeekProc;
    CloseProc: TIFFCloseProc; SizeProc: TIFFSizeProc;
    MapProc: TIFFMapFileProc; UnmapProc: TIFFUnmapFileProc): PTIFF; cdecl;
  TIFFClose: procedure(Handle: PTIFF); cdecl;
  TIFFSetField: function(Handle: PTIFF; Tag: UInt32): Int32; cdecl varargs;
  TIFFGetField: function(Handle: PTIFF; Tag: UInt32): Int32; cdecl varargs;
  TIFFGetFieldDefaulted: function(Handle: PTIFF; Tag: UInt32): Int32; cdecl varargs;
  TIFFSetDirectory: function(Handle: PTIFF; Dirn: tdir_t): Int32; cdecl;
  TIFFWriteDirectory: function(Handle: PTIFF): Int32; cdecl;
  TIFFIsTiled: function(Handle: PTIFF): Int32; cdecl;
  TIFFNumberOfStrips: function(Handle: PTIFF): UInt32; cdecl;
  TIFFNumberOfTiles: function(Handle: PTIFF): UInt32; cdecl;
  TIFFStripSize: function(Handle: PTIFF): tmsize_t; cdecl;
  TIFFTileSize: function(Handle: PTIFF): tmsize_t; cdecl;
  TIFFScanlineSize: function(Handle: PTIFF): tmsize_t; cdecl;
  TIFFDefaultStripSize: function(Handle: PTIFF; Request: UInt32): UInt32; cdecl;
  TIFFReadEncodedStrip: function(Handle: PTIFF; Strip: UInt32; Buf: Pointer; Size: tmsize_t): tmsize_t; cdecl;
  TIFFReadEncodedTile: function(Handle: PTIFF; Tile: UInt32; Buf: Pointer; Size: tmsize_t): tmsize_t; cdecl;
  TIFFReadScanline: function(Handle: PTIFF; Buf: Pointer; Row: UInt32; Sample: tsample_t): Int32; cdecl;
  TIFFWriteScanline: function(Handle: PTIFF; Buf: Pointer; Row: UInt32; Sample: tsample_t): Int32; cdecl;
  TIFFReadRGBAImageOriented: function(Handle: PTIFF; RWidth, RHeight: UInt32; Raster: Pointer;
    Orientation: Int32; Stop: Int32): Int32; cdecl;

{ Number of directories (pages) of the file, for any libtiff 4 version. }
function TIFFNumberOfDirectories(Handle: PTIFF): Integer;

{ Tries to load libtiff using the usual library names for the current platform.
  Returns True if the library with all needed functions is loaded (also
  when it was already loaded before). The search is done only once. }
function LoadLibTiff: Boolean;
{ Loads libtiff from the given file (replaces already loaded library).
  On Windows the libraries libtiff depends on (zlib, libjpeg...) are looked
  for in the folder of FileName first. }
function LoadLibTiffFromFile(const FileName: string): Boolean;
{ Returns True if libtiff is currently loaded. }
function IsLibTiffLoaded: Boolean;
{ Version of loaded libtiff as "major.minor.revision" or empty string. }
function GetLibTiffVersion: string;

type
  TLibTiffMessageHandler = procedure(const Module, Message: AnsiString);

{ Sets handlers that receive libtiff's errors and warnings, formatted. With
  nil handlers the messages are dropped (libtiff would print them to stderr). }
procedure SetLibTiffMessageHandlers(ErrorHandler, WarningHandler: TLibTiffMessageHandler);

implementation

uses
{$IFDEF FPC}
  dynlibs,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils;

const
{$IF Defined(MSWINDOWS)}
  // libtiff-6.dll is the name of MSYS2/MinGW builds
  LibNames: array[0..1] of string = ('libtiff.dll', 'libtiff-6.dll');
{$ELSEIF Defined(DARWIN) or Defined(MACOS)}
  LibNames: array[0..7] of string = ('libtiff.6.dylib', 'libtiff.dylib',
    '@executable_path/libtiff.6.dylib', '@executable_path/libtiff.dylib',
    '@executable_path/../Frameworks/libtiff.6.dylib',  // in MyApp.app/Contents/Frameworks
    '/opt/homebrew/opt/libtiff/lib/libtiff.6.dylib',   // Homebrew on ARM
    '/usr/local/opt/libtiff/lib/libtiff.6.dylib',      // Homebrew on Intel
    '/opt/local/lib/libtiff.6.dylib');                 // MacPorts
{$ELSE}
  // SONAME is libtiff.so.6 for libtiff 4.5+ and libtiff.so.5 for 4.0-4.4;
  // plain libtiff.so is often only in -dev packages
  LibNames: array[0..2] of string = ('libtiff.so.6', 'libtiff.so.5', 'libtiff.so');
{$IFEND}

{$IFDEF MSWINDOWS}
  SRuntimeLib = 'msvcrt.dll';
{$ELSE}
  SRuntimeLib = 'libc.so';
{$ENDIF}

type
  TIFFSetHandlerFunc = function(Handler: TIFFErrorHandler): TIFFErrorHandler; cdecl;
  TIFFDirCountFunc = function(Handle: PTIFF): tdir_t; cdecl;

var
  LibHandle: {$IFDEF FPC}TLibHandle{$ELSE}THandle{$ENDIF} = 0;
  SearchDone: Boolean = False;
  VersionMajor, VersionMinor, VersionRevision: Integer;
  NumberOfDirectoriesFunc: TIFFDirCountFunc;
  SetErrorHandler, SetWarningHandler: TIFFSetHandlerFunc;
  UserErrorHandler, UserWarningHandler: TLibTiffMessageHandler;

{ libtiff hands its message handler a format string and a va_list.
  vsnprintf is the entry point that takes a va_list, do not use snprintf,
  which is variadic. }
function vsnprintf(S: PAnsiChar; N: NativeInt; const Format: PAnsiChar;
  Args: va_list): Int32; cdecl;
  external SRuntimeLib name {$IFDEF MSWINDOWS}'_vsnprintf'{$ELSE}'vsnprintf'{$ENDIF};

procedure FormatAndCallHandler(Handler: TLibTiffMessageHandler; Module, Format: PAnsiChar;
  Params: va_list);
var
  Len: Integer;
  Buffer: array[0..511] of AnsiChar;
  Msg: AnsiString;
begin
  { msvcrt's _vsnprintf returns -1 when the text did not fit and does not
    terminate the buffer; the C99 version returns the length it WANTED.
    The last char is kept out of its reach as a terminator and the result
    is clamped before SetString is handed a count. }
  FillChar(Buffer, SizeOf(Buffer), 0);
  Len := vsnprintf(@Buffer, SizeOf(Buffer) - 1, Format, Params);
  if (Len < 0) or (Len > SizeOf(Buffer) - 1) then
    Len := StrLen(PAnsiChar(@Buffer));
  SetString(Msg, PAnsiChar(@Buffer), Len);
  Handler(AnsiString(Module), Msg);
end;

procedure InternalError(Module, Format: PAnsiChar; Params: va_list); cdecl;
begin
  if Assigned(UserErrorHandler) then
    FormatAndCallHandler(UserErrorHandler, Module, Format, Params);
end;

procedure InternalWarning(Module, Format: PAnsiChar; Params: va_list); cdecl;
begin
  if Assigned(UserWarningHandler) then
    FormatAndCallHandler(UserWarningHandler, Module, Format, Params);
end;

procedure SetLibTiffMessageHandlers(ErrorHandler, WarningHandler: TLibTiffMessageHandler);
begin
  UserErrorHandler := ErrorHandler;
  UserWarningHandler := WarningHandler;
end;

function TIFFNumberOfDirectories(Handle: PTIFF): Integer;
begin
  Result := Integer(NumberOfDirectoriesFunc(Handle));
  // Before 4.5 the result is 16-bit and the upper half of the register is
  // whatever was left there
  if VersionMinor < 5 then
    Result := Result and $FFFF;
end;

{ Parses the version out of "LIBTIFF, Version 4.7.0<LF>Copyright...". }
function ParseVersion(Text: PAnsiChar): Boolean;
var
  Version: string;
  Position, Part: Integer;
  Parts: array[0..2] of Integer;
begin
  Version := string(AnsiString(Text));
  Position := Pos('Version ', Version);
  Result := Position > 0;
  if not Result then
    Exit;

  Position := Position + Length('Version ');
  FillChar(Parts, SizeOf(Parts), 0);
  Part := 0;
  while (Position <= Length(Version)) and (Part <= High(Parts)) do
  begin
    if (Version[Position] >= '0') and (Version[Position] <= '9') then
      Parts[Part] := Parts[Part] * 10 + Ord(Version[Position]) - Ord('0')
    else if Version[Position] = '.' then
      Inc(Part)
    else
      Break;
    Inc(Position);
  end;

  VersionMajor := Parts[0];
  VersionMinor := Parts[1];
  VersionRevision := Parts[2];
end;

procedure UnloadLib;
begin
  if LibHandle <> 0 then
  begin
    FreeLibrary(LibHandle);
    LibHandle := 0;
  end;

  TIFFGetVersion := nil;
  TIFFClientOpen := nil;
  TIFFClose := nil;
  TIFFSetField := nil;
  TIFFGetField := nil;
  TIFFGetFieldDefaulted := nil;
  TIFFSetDirectory := nil;
  TIFFWriteDirectory := nil;
  TIFFIsTiled := nil;
  TIFFNumberOfStrips := nil;
  TIFFNumberOfTiles := nil;
  TIFFStripSize := nil;
  TIFFTileSize := nil;
  TIFFScanlineSize := nil;
  TIFFDefaultStripSize := nil;
  TIFFReadEncodedStrip := nil;
  TIFFReadEncodedTile := nil;
  TIFFReadScanline := nil;
  TIFFWriteScanline := nil;
  TIFFReadRGBAImageOriented := nil;
  NumberOfDirectoriesFunc := nil;
  SetErrorHandler := nil;
  SetWarningHandler := nil;
  VersionMajor := 0;
  VersionMinor := 0;
  VersionRevision := 0;
end;

function OpenLibrary(const FileName: string): Boolean;
{$IFDEF MSWINDOWS}
const
  LOAD_WITH_ALTERED_SEARCH_PATH = 8;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  // For a full path the altered search path makes Windows look for the
  // libraries libtiff depends on (zlib, libjpeg, and for MSYS2 builds also
  // libdeflate, libjbig, libLerc, liblzma, libwebp, libzstd and the GCC
  // runtime) in the folder of libtiff, not of the program
  if ExtractFilePath(FileName) <> '' then
    LibHandle := LoadLibraryEx(PChar(FileName), 0, LOAD_WITH_ALTERED_SEARCH_PATH)
  else
{$ENDIF}
    LibHandle := LoadLibrary(PChar(FileName));

  Result := LibHandle <> 0;
end;

function TryLoad(const FileName: string): Boolean;
var
  Missing: Boolean;

  function GetProc(const ProcName: string): Pointer;
  begin
    Result := GetProcAddress(LibHandle, PChar(ProcName));
    if Result = nil then
      Missing := True;
  end;

begin
  Result := OpenLibrary(FileName);
  if not Result then
    Exit;

  Missing := False;
  TIFFGetVersion := GetProc('TIFFGetVersion');
  TIFFClientOpen := GetProc('TIFFClientOpen');
  TIFFClose := GetProc('TIFFClose');
  TIFFSetField := GetProc('TIFFSetField');
  TIFFGetField := GetProc('TIFFGetField');
  TIFFGetFieldDefaulted := GetProc('TIFFGetFieldDefaulted');
  TIFFSetDirectory := GetProc('TIFFSetDirectory');
  TIFFWriteDirectory := GetProc('TIFFWriteDirectory');
  TIFFIsTiled := GetProc('TIFFIsTiled');
  TIFFNumberOfStrips := GetProc('TIFFNumberOfStrips');
  TIFFNumberOfTiles := GetProc('TIFFNumberOfTiles');
  TIFFStripSize := GetProc('TIFFStripSize');
  TIFFTileSize := GetProc('TIFFTileSize');
  TIFFScanlineSize := GetProc('TIFFScanlineSize');
  TIFFDefaultStripSize := GetProc('TIFFDefaultStripSize');
  TIFFReadEncodedStrip := GetProc('TIFFReadEncodedStrip');
  TIFFReadEncodedTile := GetProc('TIFFReadEncodedTile');
  TIFFReadScanline := GetProc('TIFFReadScanline');
  TIFFWriteScanline := GetProc('TIFFWriteScanline');
  TIFFReadRGBAImageOriented := GetProc('TIFFReadRGBAImageOriented');
  NumberOfDirectoriesFunc := GetProc('TIFFNumberOfDirectories');
  SetErrorHandler := GetProc('TIFFSetErrorHandler');
  SetWarningHandler := GetProc('TIFFSetWarningHandler');

  // BigTIFF and 64-bit offsets in the IO callbacks need libtiff 4
  Result := not Missing and ParseVersion(TIFFGetVersion) and (VersionMajor = 4);

  if Result then
  begin
    SetErrorHandler(@InternalError);
    SetWarningHandler(@InternalWarning);
  end
  else
    UnloadLib;
end;

function LoadLibTiff: Boolean;
var
  I: Integer;
begin
  if (LibHandle = 0) and not SearchDone then
  begin
    SearchDone := True;

    for I := Low(LibNames) to High(LibNames) do
    begin
      if TryLoad(LibNames[I]) then
        Break;
    end;
  end;

  Result := LibHandle <> 0;
end;

function LoadLibTiffFromFile(const FileName: string): Boolean;
begin
  UnloadLib;
  SearchDone := True;
  Result := TryLoad(FileName);
end;

function IsLibTiffLoaded: Boolean;
begin
  Result := LibHandle <> 0;
end;

function GetLibTiffVersion: string;
begin
  Result := '';
  if LibHandle <> 0 then
    Result := Format('%d.%d.%d', [VersionMajor, VersionMinor, VersionRevision]);
end;

initialization

finalization
  UnloadLib;
end.
