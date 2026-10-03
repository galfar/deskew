unit ImPasJpeg;

{ JPEG image compression and decompression library.

  Single-unit version of PasJPEG 1.1 by Jacques Nomssi Nzali, a Pascal
  translation of the Independent JPEG Group's JPEG software release 6b.
  Modified and merged into one unit for Vampyre Imaging Library by Marek Mauder
  (so this is an altered source version, not the original software).

  Original units are kept as sections delimited by comments
    == imjxxx interface ==  and  == imjxxx implementation ==
  Identifiers that were private to one of the original units and
  clashed with another unit are prefixed with the name of the original IJG
  module (for example jccoefct_start_iMCU_row).

  ---------------------------------------------------------------------------
  This software is based in part on the work of the Independent JPEG Group.
  The IJG code is copyright (C) 1991-1998, Thomas G. Lane.

  PASJPEG 1.1, May 29th, 1999
  Copyright (C) 1996,1998,1999 by NOMSSI NZALI Jacques H. C.

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
  --------------------------------------------------------------------------- }

{$I ImagingOptions.inc}

{ ----------------------- JPEG_INTERNAL_OPTIONS ---------------------- }

{ These defines indicate whether to include various optional functions.
  Undefining some of these symbols will produce a smaller but less capable
  library. }

{ Arithmetic coding is unsupported for legal reasons.  Complaints to IBM. }

{ Capability options common to encoder and decoder: }

{$define DCT_ISLOW_SUPPORTED}     { slow but accurate integer algorithm }
{$define DCT_IFAST_SUPPORTED}     { faster, less accurate integer method }
{$define DCT_FLOAT_SUPPORTED}     { floating-point: accurate, fast on fast HW }

{ Encoder capability options: }

{$undef C_ARITH_CODING_SUPPORTED}    { Arithmetic coding back end? }
{$define C_MULTISCAN_FILES_SUPPORTED} { Multiple-scan JPEG files? }
{$define C_PROGRESSIVE_SUPPORTED}     { Progressive JPEG? (Requires MULTISCAN)}
{$define ENTROPY_OPT_SUPPORTED}       { Optimization of entropy coding parms? }
{ Note: if you selected 12-bit data precision, it is dangerous to turn off
  ENTROPY_OPT_SUPPORTED.  The standard Huffman tables are only good for 8-bit
  precision, so jchuff.c normally uses entropy optimization to compute
  usable tables for higher precision.  If you don't want to do optimization,
  you'll have to supply different default Huffman tables.
  The exact same statements apply for progressive JPEG: the default tables
  don't work for progressive mode.  (This may get fixed, however.) }

{$define INPUT_SMOOTHING_SUPPORTED}   { Input image smoothing option? }

{ Decoder capability options: }

{$undef  D_ARITH_CODING_SUPPORTED}    { Arithmetic coding back end? }
{$define D_MULTISCAN_FILES_SUPPORTED} { Multiple-scan JPEG files? }
{$define D_PROGRESSIVE_SUPPORTED}     { Progressive JPEG? (Requires MULTISCAN)}
{$define SAVE_MARKERS_SUPPORTED}      { jpeg_save_markers() needed? }
{$define BLOCK_SMOOTHING_SUPPORTED}   { Block smoothing? (Progressive only) }
{$define IDCT_SCALING_SUPPORTED}      { Output rescaling via IDCT? }
{$undef  UPSAMPLE_SCALING_SUPPORTED}  { Output rescaling at upsample stage? }
{$define UPSAMPLE_MERGING_SUPPORTED}  { Fast path for sloppy upsampling? }
{$define QUANT_1PASS_SUPPORTED}       { 1-pass color quantization? }
{$define QUANT_2PASS_SUPPORTED}       { 2-pass color quantization? }

{ ------------------------------------------------------------------------ }

{$define DCTSIZE_IS_8}          { e.g. unroll the inner loop }
{$define RIGHT_SHIFT_IS_UNSIGNED}
{$undef AVOID_TABLES}
{$undef FAST_DIVIDE}

{$define BITS_IN_JSAMPLE_IS_8}

{----------------------------------------------------------------}
{ for test of 12 bit JPEG code only. !! }
{-- $undef  BITS_IN_JSAMPLE_IS_8}
{----------------------------------------------------------------}

{ RGB byte order: RGB_RED_IS_0 must not be defined for Delphi/FPC (BGR order) }
{$undef RGB_RED_IS_0}

{$define RGB_PIXELSIZE_IS_3}
{$define SLOW_SHIFT_32}
{$undef NO_ZERO_ROW_TEST}

{$undef FULL_MAIN_BUFFER_SUPPORTED}

{ 32bit x86 assembler version of the accurate integer IDCT (Delphi only),
  enabled by USE_ASM in ImagingOptions.inc. }
{$IF Defined(USE_ASM) and Defined(DCC) and Defined(CPUX86) and Defined(MSWINDOWS)}
  {$DEFINE JPEG_IDCT_ASM}
{$IFEND}

{$Q-}
{$MINENUMSIZE 4}
{$ALIGN 8}

interface

{== imjmorecfg interface ==}

{ This file contains additional configuration options that customize the
  JPEG software for special applications or support machine-dependent
  optimizations.  Most users will not need to touch this file. }

{ Source: jmorecfg.h; Copyright (C) 1991-1996, Thomas G. Lane. }


type
  int = Integer;
  uInt = Cardinal;
  short = SmallInt;
  ushort = Word;
  long = LongInt;

type
  voidp = pointer;

type
  int_ptr = ^int;
  size_t = int;

{ Define BITS_IN_JSAMPLE as either
    8   for 8-bit sample values (the usual setting)
    12  for 12-bit sample values
  Only 8 and 12 are legal data precisions for lossy JPEG according to the
  JPEG standard, and the IJG code does not support anything else!
  We do not support run-time selection of data precision, sorry. }


{$ifdef BITS_IN_JSAMPLE_IS_8}   { use 8 or 12 }
const
  BITS_IN_JSAMPLE = 8;
{$else}
const
  BITS_IN_JSAMPLE = 12;
{$endif}


{ Maximum number of components (color channels) allowed in JPEG image.
  To meet the letter of the JPEG spec, set this to 255.  However, darn
  few applications need more than 4 channels (maybe 5 for CMYK + alpha
  mask).  We recommend 10 as a reasonable compromise; use 4 if you are
  really short on memory.  (Each allowed component costs a hundred or so
  bytes of storage, whether actually used in an image or not.) }


const
  MAX_COMPONENTS = 10;          { maximum number of image components }


{ Basic data types.
  You may need to change these if you have a machine with unusual data
  type sizes; for example, "char" not 8 bits, "short" not 16 bits,
  or "long" not 32 bits.  We don't care whether "int" is 16 or 32 bits,
  but it had better be at least 16. }


{ Representation of a single sample (pixel element value).
  We frequently allocate large arrays of these, so it's important to keep
  them small.  But if you have memory to burn and access to char or short
  arrays is very slow on your hardware, you might want to change these. }


{$ifdef BITS_IN_JSAMPLE_IS_8}
{ JSAMPLE should be the smallest type that will hold the values 0..255.
  You can use a signed char by having GETJSAMPLE mask it with $FF. }

{ CHAR_IS_UNSIGNED }
type
  JSAMPLE = byte; { Pascal unsigned char }
  GETJSAMPLE = int;

const
  MAXJSAMPLE = 255;
  CENTERJSAMPLE = 128;

{$endif}

{$ifndef BITS_IN_JSAMPLE_IS_8}
{ JSAMPLE should be the smallest type that will hold the values 0..4095.
  On nearly all machines "short" will do nicely. }

type
  JSAMPLE = short;
  GETJSAMPLE = int;

const
  MAXJSAMPLE = 4095;
  CENTERJSAMPLE = 2048;

{$endif} { BITS_IN_JSAMPLE = 12 }


{ Representation of a DCT frequency coefficient.
  This should be a signed value of at least 16 bits; "short" is usually OK.
  Again, we allocate large arrays of these, but you can change to int
  if you have memory to burn and "short" is really slow. }
type
  JCOEF = int;
  JCOEF_PTR = ^JCOEF;


{ Compressed datastreams are represented as arrays of JOCTET.
  These must be EXACTLY 8 bits wide, at least once they are written to
  external storage.  Note that when using the stdio data source/destination
  managers, this is also the data type passed to fread/fwrite. }


type
  JOCTET = Byte;
  jTOctet = 0..(MaxInt div SizeOf(JOCTET))-1;
  JOCTET_FIELD = array[jTOctet] of JOCTET;
  JOCTET_FIELD_PTR = ^JOCTET_FIELD;
  JOCTETPTR = ^JOCTET;

  GETJOCTET = JOCTET; { A work around }


{ These typedefs are used for various table entries and so forth.
  They must be at least as wide as specified; but making them too big
  won't cost a huge amount of memory, so we don't provide special
  extraction code like we did for JSAMPLE.  (In other words, these
  typedefs live at a different point on the speed/space tradeoff curve.) }


{ UINT8 must hold at least the values 0..255. }

type
  UINT8 = Byte;

{ UINT16 must hold at least the values 0..65535. }

  UINT16 = Word;

{ INT16 must hold at least the values -32768..32767. }

  INT16 = SmallInt;

{ INT32 must hold at least signed 32-bit values. }

  INT32 = LongInt;
type
  INT32PTR = ^INT32;

{ Datatype used for image dimensions.  The JPEG standard only supports
  images up to 64K*64K due to 16-bit fields in SOF markers.  Therefore
  "unsigned int" is sufficient on all machines.  However, if you need to
  handle larger images and you don't mind deviating from the spec, you
  can change this datatype. }

type
  JDIMENSION = uInt;

const
  JPEG_MAX_DIMENSION = 65500;  { a tad under 64K to prevent overflows }


{ Ordering of RGB data in scanlines passed to or from the application.
  If your application wants to deal with data in the order B,G,R, just
  change these macros.  You can also deal with formats such as R,G,B,X
  (one extra byte per pixel) by changing RGB_PIXELSIZE.  Note that changing
  the offsets will also change the order in which colormap data is organized.
  RESTRICTIONS:
  1. The sample applications cjpeg,djpeg do NOT support modified RGB formats.
  2. These macros only affect RGB<=>YCbCr color conversion, so they are not
     useful if you are using JPEG color spaces other than YCbCr or grayscale.
  3. The color quantizer modules will not behave desirably if RGB_PIXELSIZE
     is not 3 (they don't understand about dummy color components!).  So you
     can't use color quantization if you change that value. }

{$ifdef RGB_RED_IS_0}
const
  RGB_RED       = 0;    { Offset of Red in an RGB scanline element }
  RGB_GREEN     = 1;    { Offset of Green }
  RGB_BLUE      = 2;    { Offset of Blue }
{$else}
const
  RGB_RED       = 2;    { Offset of Red in an RGB scanline element }
  RGB_GREEN     = 1;    { Offset of Green }
  RGB_BLUE      = 0;    { Offset of Blue }
{$endif}

{$ifdef RGB_PIXELSIZE_IS_3}
const
  RGB_PIXELSIZE = 3;    { JSAMPLEs per RGB scanline element }
{$else}
const
  RGB_PIXELSIZE = ??;   { Nomssi: deliberate syntax error. Set this value }
{$endif}

{ Definitions for speed-related optimizations. }

{ On some machines (notably 68000 series) "int" is 32 bits, but multiplying
  two 16-bit shorts is faster than multiplying two ints.  Define MULTIPLIER
  as short on such a machine.  MULTIPLIER must be at least 16 bits wide. }
type
  MULTIPLIER = int;     { type for fastest integer multiply }


{ FAST_FLOAT should be either float or double, whichever is done faster
  by your compiler.  (Note that this type is only used in the floating point
  DCT routines, so it only matters if you've defined DCT_FLOAT_SUPPORTED.)
  Typically, float is faster in ANSI C compilers, while double is faster in
  pre-ANSI compilers (because they insist on converting to double anyway).
  The code below therefore chooses float if we have ANSI-style prototypes. }

type
  FAST_FLOAT = double; {float}

{== imjdeferr interface ==}

{ This file defines the error and message codes for the cjpeg/djpeg
  applications.  These strings are not needed as part of the JPEG library
  proper.
  Edit this file to add new codes, or to translate the message strings to
  some other language. }

{ Original cderror.h  ; Copyright (C) 1994, Thomas G. Lane.  }


{ To define the enum list of message codes, include this file without
  defining macro JMESSAGE.  To create a message string table, include it
  again with a suitable JMESSAGE definition (see jerror.c for an example). }


{ Original: jversion.h ; Copyright (C) 1991-1996, Thomas G. Lane. }
{ This file contains software version identification. }

const
  JVERSION   = '6a  7-Feb-96';

  JCOPYRIGHT = 'Copyright (C) 1996, Thomas G. Lane';

  JNOTICE = 'Pascal Translation, Copyright (C) 1996, Jacques Nomssi Nzali';

{ Create the message string table.
  We do this from the master message list in jerror.h by re-reading
  jerror.h with a suitable definition for macro JMESSAGE.
  The message table is made an external symbol just in case any applications
  want to refer to it directly. }

type
  J_MESSAGE_CODE  =(
    JMSG_NOMESSAGE,
    JERR_ARITH_NOTIMPL,
    JERR_BAD_ALIGN_TYPE,
    JERR_BAD_ALLOC_CHUNK,
    JERR_BAD_BUFFER_MODE,
    JERR_BAD_COMPONENT_ID,
    JERR_BAD_DCT_COEF,
    JERR_BAD_DCTSIZE,
    JERR_BAD_HUFF_TABLE,
    JERR_BAD_IN_COLORSPACE,
    JERR_BAD_J_COLORSPACE,
    JERR_BAD_LENGTH,
    JERR_BAD_LIB_VERSION,
    JERR_BAD_MCU_SIZE,
    JERR_BAD_POOL_ID,
    JERR_BAD_PRECISION,
    JERR_BAD_PROGRESSION,
    JERR_BAD_PROG_SCRIPT,
    JERR_BAD_SAMPLING,
    JERR_BAD_SCAN_SCRIPT,
    JERR_BAD_STATE,
    JERR_BAD_STRUCT_SIZE,
    JERR_BAD_VIRTUAL_ACCESS,
    JERR_BUFFER_SIZE,
    JERR_CANT_SUSPEND,
    JERR_CCIR601_NOTIMPL,
    JERR_COMPONENT_COUNT,
    JERR_CONVERSION_NOTIMPL,
    JERR_DAC_INDEX,
    JERR_DAC_VALUE,
    JERR_DHT_COUNTS,
    JERR_DHT_INDEX,
    JERR_DQT_INDEX,
    JERR_EMPTY_IMAGE,
    JERR_EMS_READ,
    JERR_EMS_WRITE,
    JERR_EOI_EXPECTED,
    JERR_FILE_READ,
    JERR_FILE_WRITE,
    JERR_FRACT_SAMPLE_NOTIMPL,
    JERR_HUFF_CLEN_OVERFLOW,
    JERR_HUFF_MISSING_CODE,
    JERR_IMAGE_TOO_BIG,
    JERR_INPUT_EMPTY,
    JERR_INPUT_EOF,
    JERR_MISMATCHED_QUANT_TABLE,
    JERR_MISSING_DATA,
    JERR_MODE_CHANGE,
    JERR_NOTIMPL,
    JERR_NOT_COMPILED,
    JERR_NO_BACKING_STORE,
    JERR_NO_HUFF_TABLE,
    JERR_NO_IMAGE,
    JERR_NO_QUANT_TABLE,
    JERR_NO_SOI,
    JERR_OUT_OF_MEMORY,
    JERR_QUANT_COMPONENTS,
    JERR_QUANT_FEW_COLORS,
    JERR_QUANT_MANY_COLORS,
    JERR_SOF_DUPLICATE,
    JERR_SOF_NO_SOS,
    JERR_SOF_UNSUPPORTED,
    JERR_SOI_DUPLICATE,
    JERR_SOS_NO_SOF,
    JERR_TFILE_CREATE,
    JERR_TFILE_READ,
    JERR_TFILE_SEEK,
    JERR_TFILE_WRITE,
    JERR_TOO_LITTLE_DATA,
    JERR_UNKNOWN_MARKER,
    JERR_VIRTUAL_BUG,
    JERR_WIDTH_OVERFLOW,
    JERR_XMS_READ,
    JERR_XMS_WRITE,
    JMSG_COPYRIGHT,
    JMSG_VERSION,
    JTRC_16BIT_TABLES,
    JTRC_ADOBE,
    JTRC_APP0,
    JTRC_APP14,
    JTRC_DAC,
    JTRC_DHT,
    JTRC_DQT,
    JTRC_DRI,
    JTRC_EMS_CLOSE,
    JTRC_EMS_OPEN,
    JTRC_EOI,
    JTRC_HUFFBITS,
    JTRC_JFIF,
    JTRC_JFIF_BADTHUMBNAILSIZE,
    JTRC_JFIF_EXTENSION,
    JTRC_JFIF_THUMBNAIL,
    JTRC_MISC_MARKER,
    JTRC_PARMLESS_MARKER,
    JTRC_QUANTVALS,
    JTRC_QUANT_3_NCOLORS,
    JTRC_QUANT_NCOLORS,
    JTRC_QUANT_SELECTED,
    JTRC_RECOVERY_ACTION,
    JTRC_RST,
    JTRC_SMOOTH_NOTIMPL,
    JTRC_SOF,
    JTRC_SOF_COMPONENT,
    JTRC_SOI,
    JTRC_SOS,
    JTRC_SOS_COMPONENT,
    JTRC_SOS_PARAMS,
    JTRC_TFILE_CLOSE,
    JTRC_TFILE_OPEN,
    JTRC_THUMB_JPEG,
    JTRC_THUMB_PALETTE,
    JTRC_THUMB_RGB,
    JTRC_UNKNOWN_IDS,
    JTRC_XMS_CLOSE,
    JTRC_XMS_OPEN,
    JWRN_ADOBE_XFORM,
    JWRN_BOGUS_PROGRESSION,
    JWRN_EXTRANEOUS_DATA,
    JWRN_HIT_MARKER,
    JWRN_HUFF_BAD_CODE,
    JWRN_JFIF_MAJOR,
    JWRN_JPEG_EOF,
    JWRN_MUST_RESYNC,
    JWRN_NOT_SEQUENTIAL,
    JWRN_TOO_MUCH_DATA,


     JMSG_FIRSTADDONCODE,  { Must be first entry! }


     JERR_TGA_NOTCOMP,  { Targa support was not compiled }

     JERR_BAD_CMAP_FILE,
	    { Color map file is invalid or of unsupported format }
     JERR_TOO_MANY_COLORS,
	    { Output file format cannot handle %d colormap entries }
     JERR_UNGETC_FAILED,  { ungetc failed }
     JERR_UNKNOWN_FORMAT,  { Unrecognized input file format }
     JERR_UNSUPPORTED_FORMAT,  { Unsupported output file format }

     JMSG_LASTADDONCODE
   );


const
  JMSG_LASTMSGCODE : J_MESSAGE_CODE = JMSG_LASTADDONCODE;

type
  msg_table = Array[J_MESSAGE_CODE] of string[80];
const
  jpeg_std_message_table : msg_table = (

  { JMSG_NOMESSAGE } 'Bogus message code %d', { Must be first entry! }

{ For maintenance convenience, list is alphabetical by message code name }
  { JERR_ARITH_NOTIMPL }
	 'Sorry, there are legal restrictions on arithmetic coding',
  { JERR_BAD_ALIGN_TYPE } 'ALIGN_TYPE is wrong, please fix',
  { JERR_BAD_ALLOC_CHUNK } 'MAX_ALLOC_CHUNK is wrong, please fix',
  { JERR_BAD_BUFFER_MODE } 'Bogus buffer control mode',
  { JERR_BAD_COMPONENT_ID } 'Invalid component ID %d in SOS',
  { JERR_BAD_DCT_COEF } 'DCT coefficient out of range',
  { JERR_BAD_DCTSIZE } 'IDCT output block size %d not supported',
  { JERR_BAD_HUFF_TABLE } 'Bogus Huffman table definition',
  { JERR_BAD_IN_COLORSPACE } 'Bogus input colorspace',
  { JERR_BAD_J_COLORSPACE } 'Bogus JPEG colorspace',
  { JERR_BAD_LENGTH } 'Bogus marker length',
  { JERR_BAD_LIB_VERSION }
	 'Wrong JPEG library version: library is %d, caller expects %d',
  { JERR_BAD_MCU_SIZE } 'Sampling factors too large for interleaved scan',
  { JERR_BAD_POOL_ID } 'Invalid memory pool code %d',
  { JERR_BAD_PRECISION } 'Unsupported JPEG data precision %d',
  { JERR_BAD_PROGRESSION }
	 'Invalid progressive parameters Ss=%d Se=%d Ah=%d Al=%d',
  { JERR_BAD_PROG_SCRIPT }
	 'Invalid progressive parameters at scan script entry %d',
  { JERR_BAD_SAMPLING } 'Bogus sampling factors',
  { JERR_BAD_SCAN_SCRIPT } 'Invalid scan script at entry %d',
  { JERR_BAD_STATE } 'Improper call to JPEG library in state %d',
  { JERR_BAD_STRUCT_SIZE }
	 'JPEG parameter struct mismatch: library thinks size is %d, caller expects %d',
  { JERR_BAD_VIRTUAL_ACCESS } 'Bogus virtual array access',
  { JERR_BUFFER_SIZE } 'Buffer passed to JPEG library is too small',
  { JERR_CANT_SUSPEND } 'Suspension not allowed here',
  { JERR_CCIR601_NOTIMPL } 'CCIR601 sampling not implemented yet',
  { JERR_COMPONENT_COUNT } 'Too many color components: %d, max %d',
  { JERR_CONVERSION_NOTIMPL } 'Unsupported color conversion request',
  { JERR_DAC_INDEX } 'Bogus DAC index %d',
  { JERR_DAC_VALUE } 'Bogus DAC value $%x',
  { JERR_DHT_COUNTS } 'Bogus DHT counts',
  { JERR_DHT_INDEX } 'Bogus DHT index %d',
  { JERR_DQT_INDEX } 'Bogus DQT index %d',
  { JERR_EMPTY_IMAGE } 'Empty JPEG image (DNL not supported)',
  { JERR_EMS_READ } 'Read from EMS failed',
  { JERR_EMS_WRITE } 'Write to EMS failed',
  { JERR_EOI_EXPECTED } 'Didn''t expect more than one scan',
  { JERR_FILE_READ } 'Input file read error',
  { JERR_FILE_WRITE } 'Output file write error --- out of disk space?',
  { JERR_FRACT_SAMPLE_NOTIMPL } 'Fractional sampling not implemented yet',
  { JERR_HUFF_CLEN_OVERFLOW } 'Huffman code size table overflow',
  { JERR_HUFF_MISSING_CODE } 'Missing Huffman code table entry',
  { JERR_IMAGE_TOO_BIG } 'Maximum supported image dimension is %d pixels',
  { JERR_INPUT_EMPTY } 'Empty input file',
  { JERR_INPUT_EOF } 'Premature end of input file',
  { JERR_MISMATCHED_QUANT_TABLE }
	 'Cannot transcode due to multiple use of quantization table %d',
  { JERR_MISSING_DATA } 'Scan script does not transmit all data',
  { JERR_MODE_CHANGE } 'Invalid color quantization mode change',
  { JERR_NOTIMPL } 'Not implemented yet',
  { JERR_NOT_COMPILED } 'Requested feature was omitted at compile time',
  { JERR_NO_BACKING_STORE } 'Backing store not supported',
  { JERR_NO_HUFF_TABLE } 'Huffman table $%02x was not defined',
  { JERR_NO_IMAGE } 'JPEG datastream contains no image',
  { JERR_NO_QUANT_TABLE } 'Quantization table $%02x was not defined',
  { JERR_NO_SOI } 'Not a JPEG file: starts with $%02x $%02x',
  { JERR_OUT_OF_MEMORY } 'Insufficient memory (case %d)',
  { JERR_QUANT_COMPONENTS }
	 'Cannot quantize more than %d color components',
  { JERR_QUANT_FEW_COLORS } 'Cannot quantize to fewer than %d colors',
  { JERR_QUANT_MANY_COLORS } 'Cannot quantize to more than %d colors',
  { JERR_SOF_DUPLICATE } 'Invalid JPEG file structure: two SOF markers',
  { JERR_SOF_NO_SOS } 'Invalid JPEG file structure: missing SOS marker',
  { JERR_SOF_UNSUPPORTED } 'Unsupported JPEG process: SOF type $%02x',
  { JERR_SOI_DUPLICATE } 'Invalid JPEG file structure: two SOI markers',
  { JERR_SOS_NO_SOF } 'Invalid JPEG file structure: SOS before SOF',
  { JERR_TFILE_CREATE } 'Failed to create temporary file %s',
  { JERR_TFILE_READ } 'Read failed on temporary file',
  { JERR_TFILE_SEEK } 'Seek failed on temporary file',
  { JERR_TFILE_WRITE }
	 'Write failed on temporary file --- out of disk space?',
  { JERR_TOO_LITTLE_DATA } 'Application transferred too few scanlines',
  { JERR_UNKNOWN_MARKER } 'Unsupported marker type $%02x',
  { JERR_VIRTUAL_BUG } 'Virtual array controller messed up',
  { JERR_WIDTH_OVERFLOW } 'Image too wide for this implementation',
  { JERR_XMS_READ } 'Read from XMS failed',
  { JERR_XMS_WRITE } 'Write to XMS failed',
  { JMSG_COPYRIGHT }  JCOPYRIGHT,
  { JMSG_VERSION } JVERSION,
  { JTRC_16BIT_TABLES }
	 'Caution: quantization tables are too coarse for baseline JPEG',
  { JTRC_ADOBE }
	 'Adobe APP14 marker: version %d, flags $%04x $%04x, transform %d',
  { JTRC_APP0 } 'Unknown APP0 marker (not JFIF), length %d',
  { JTRC_APP14 } 'Unknown APP14 marker (not Adobe), length %d',
  { JTRC_DAC } 'Define Arithmetic Table $%02x: $%02x',
  { JTRC_DHT } 'Define Huffman Table $%02x',
  { JTRC_DQT } 'Define Quantization Table %d  precision %d',
  { JTRC_DRI } 'Define Restart Interval %d',
  { JTRC_EMS_CLOSE } 'Freed EMS handle %d',
  { JTRC_EMS_OPEN } 'Obtained EMS handle %d',
  { JTRC_EOI } 'End Of Image',
  { JTRC_HUFFBITS } '        %3d %3d %3d %3d %3d %3d %3d %3d',
  { JTRC_JFIF } 'JFIF APP0 marker, density %dx%d  %d',
  { JTRC_JFIF_BADTHUMBNAILSIZE }
	 'Warning: thumbnail image size does not match data length %d',
  { JTRC_JFIF_EXTENSION } 'JFIF extension marker: type 0x%02x, length %u',
  { JTRC_JFIF_THUMBNAIL } '    with %d x %d thumbnail image',
  { JTRC_MISC_MARKER } 'Skipping marker $%02x, length %d',
  { JTRC_PARMLESS_MARKER } 'Unexpected marker $%02x',
  { JTRC_QUANTVALS } '        %4d %4d %4d %4d %4d %4d %4d %4d',
  { JTRC_QUANT_3_NCOLORS } 'Quantizing to %d = %d*%d*%d colors',
  { JTRC_QUANT_NCOLORS } 'Quantizing to %d colors',
  { JTRC_QUANT_SELECTED } 'Selected %d colors for quantization',
  { JTRC_RECOVERY_ACTION } 'At marker $%02x, recovery action %d',
  { JTRC_RST } 'RST%d',
  { JTRC_SMOOTH_NOTIMPL }
	 'Smoothing not supported with nonstandard sampling ratios',
  { JTRC_SOF } 'Start Of Frame $%02x: width=%d, height=%d, components=%d',
  { JTRC_SOF_COMPONENT } '    Component %d: %dhx%dv q=%d',
  { JTRC_SOI } 'Start of Image',
  { JTRC_SOS } 'Start Of Scan: %d components',
  { JTRC_SOS_COMPONENT } '    Component %d: dc=%d ac=%d',
  { JTRC_SOS_PARAMS } '  Ss=%d, Se=%d, Ah=%d, Al=%d',
  { JTRC_TFILE_CLOSE } 'Closed temporary file %s',
  { JTRC_TFILE_OPEN } 'Opened temporary file %s',
  { JTRC_THUMB_JPEG }
	 'JFIF extension marker: JPEG-compressed thumbnail image, length %u',
  { JMESSAGE(JTRC_THUMB_PALETTE }
	 'JFIF extension marker: palette thumbnail image, length %u',
  { JMESSAGE(JTRC_THUMB_RGB }
	 'JFIF extension marker: RGB thumbnail image, length %u',
  { JTRC_UNKNOWN_IDS }
	 'Unrecognized component IDs %d %d %d, assuming YCbCr',
  { JTRC_XMS_CLOSE } 'Freed XMS handle %d',
  { JTRC_XMS_OPEN } 'Obtained XMS handle %d',
  { JWRN_ADOBE_XFORM } 'Unknown Adobe color transform code %d',
  { JWRN_BOGUS_PROGRESSION }
	 'Inconsistent progression sequence for component %d coefficient %d',
  { JWRN_EXTRANEOUS_DATA }
	 'Corrupt JPEG data: %d extraneous bytes before marker $%02x',
  { JWRN_HIT_MARKER } 'Corrupt JPEG data: premature end of data segment',
  { JWRN_HUFF_BAD_CODE } 'Corrupt JPEG data: bad Huffman code',
  { JWRN_JFIF_MAJOR } 'Warning: unknown JFIF revision number %d.%02d',
  { JWRN_JPEG_EOF } 'Premature end of JPEG file',
  { JWRN_MUST_RESYNC }
	 'Corrupt JPEG data: found marker $%02x instead of RST%d',
  { JWRN_NOT_SEQUENTIAL } 'Invalid SOS parameters for sequential JPEG',
  { JWRN_TOO_MUCH_DATA } 'Application transferred too many scanlines',

  { JMSG_FIRSTADDONCODE }  '', { Must be first entry! }


  { JERR_TGA_NOTCOMP } 'Targa support was not compiled',

  { JERR_BAD_CMAP_FILE }
	 'Color map file is invalid or of unsupported format',
  { JERR_TOO_MANY_COLORS }
	 'Output file format cannot handle %d colormap entries',
  { JERR_UNGETC_FAILED } 'ungetc failed',
  { JERR_UNKNOWN_FORMAT } 'Unrecognized input file format',
  { JERR_UNSUPPORTED_FORMAT } 'Unsupported output file format',


  { JMSG_LASTADDONCODE } '');

{== imjpeglib interface ==}

{ This file defines the application interface for the JPEG library.
  Most applications using the library need only include this file,
  and perhaps jerror.h if they want to know the exact error codes. }

{ Source:jpeglib.h+jpegint.h; Copyright (C) 1991-1998, Thomas G. Lane. }


{ First we include the configuration files that record how this
  installation of the JPEG library is set up.  jconfig.h can be
  generated automatically for many systems.  jmorecfg.h contains
  manual configuration options that most people need not worry about. }


{ Version ID for the JPEG library.
  Might be useful for tests like "#if JPEG_LIB_VERSION >= 60". }


Const
  JPEG_LIB_VERSION = 62;        { Version 6b }


{ These marker codes are exported since applications and data source modules
  are likely to want to use them. }

const
  JPEG_RST0     = $D0;  { RST0 marker code }
  JPEG_EOI      = $D9;  { EOI marker code }
  JPEG_APP0     = $E0;  { APP0 marker code }
  JPEG_COM      = $FE;  { COM marker code }


{ Various constants determining the sizes of things.
  All of these are specified by the JPEG standard, so don't change them
  if you want to be compatible. }

const
  DCTSIZE             = 8;      { The basic DCT block is 8x8 samples }
  DCTSIZE2            = 64;     { DCTSIZE squared; # of elements in a block }
  NUM_QUANT_TBLS      = 4;      { Quantization tables are numbered 0..3 }
  NUM_HUFF_TBLS       = 4;      { Huffman tables are numbered 0..3 }
  NUM_ARITH_TBLS      = 16;     { Arith-coding tables are numbered 0..15 }
  MAX_COMPS_IN_SCAN   = 4;      { JPEG limit on # of components in one scan }
  MAX_SAMP_FACTOR     = 4;      { JPEG limit on sampling factors }
{ Unfortunately, some bozo at Adobe saw no reason to be bound by the standard;
  the PostScript DCT filter can emit files with many more than 10 blocks/MCU.
  If you happen to run across such a file, you can up D_MAX_BLOCKS_IN_MCU
  to handle it.  We even let you do this from the jconfig.h file.  However,
  we strongly discourage changing C_MAX_BLOCKS_IN_MCU; just because Adobe
  sometimes emits noncompliant files doesn't mean you should too. }
  C_MAX_BLOCKS_IN_MCU = 10;     { compressor's limit on blocks per MCU }
  D_MAX_BLOCKS_IN_MCU = 10;     { decompressor's limit on blocks per MCU }


{ Data structures for images (arrays of samples and of DCT coefficients).
  On 80x86 machines, the image arrays are too big for near pointers,
  but the pointer arrays can fit in near memory. }

type
{ for typecasting }
  JSAMPLE_PTR = ^JSAMPLE;
  JSAMPROW_PTR = ^JSAMPROW;
  JBLOCKROW_PTR = ^JBLOCKROW;

  jTSample = 0..(MaxInt div SIZEOF(JSAMPLE))-1;
  JSAMPLE_ARRAY = Array[jTSample] of JSAMPLE;  {far}
  JSAMPROW = ^JSAMPLE_ARRAY;  { ptr to one image row of pixel samples. }

  jTRow = 0..(MaxInt div SIZEOF(JSAMPROW))-1;
  JSAMPROW_ARRAY = Array[jTRow] of JSAMPROW;
  JSAMPARRAY = ^JSAMPROW_ARRAY;  { ptr to some rows (a 2-D sample array) }

  jTArray = 0..(MaxInt div SIZEOF(JSAMPARRAY))-1;
  JSAMP_ARRAY = Array[jTArray] of JSAMPARRAY;
  JSAMPIMAGE = ^JSAMP_ARRAY;  { a 3-D sample array: top index is color }

  JBLOCK = Array[0..DCTSIZE2-1] of JCOEF;   { one block of coefficients }
  JBLOCK_PTR = ^JBLOCK;

  jTBlockRow = 0..(MaxInt div SIZEOF(JBLOCK))-1;
  JBLOCK_ROWS = Array[jTBlockRow] of JBLOCK;
  JBLOCKROW = ^JBLOCK_ROWS; {far} { pointer to one row of coefficient blocks }


  jTBlockArray = 0..(MaxInt div SIZEOF(JBLOCKROW))-1;
  JBLOCK_ARRAY = Array[jTBlockArray] of JBLOCKROW;
  JBLOCKARRAY = ^JBLOCK_ARRAY;    { a 2-D array of coefficient blocks }

  jTBlockImage = 0..(MaxInt div SIZEOF(JBLOCKARRAY))-1;
  JBLOCK_IMAGE = Array[jTBlockImage] of JBLOCKARRAY;
  JBLOCKIMAGE = ^JBLOCK_IMAGE;   { a 3-D array of coefficient blocks }

  jTCoef = 0..(MaxInt div SIZEOF(JCOEF))-1;
  JCOEF_ROW = Array[jTCoef] of JCOEF;
  JCOEFPTR = ^JCOEF_ROW; {far}   { useful in a couple of places }


type
  jTByte = 0..(MaxInt div SIZEOF(byte))-1;
  JByteArray = Array[jTByte] of byte;
  JBytePtr = ^JByteArray;
type
  byteptr = ^byte;

{ Types for JPEG compression parameters and working tables. }


{ DCT coefficient quantization tables. }

type
  JQUANT_TBL_PTR = ^JQUANT_TBL;
  JQUANT_TBL = record
  { This array gives the coefficient quantizers in natural array order
    (not the zigzag order in which they are stored in a JPEG DQT marker).
    CAUTION: IJG versions prior to v6a kept this array in zigzag order. }
    quantval : Array[0..DCTSIZE2-1] of UINT16;
                               { quantization step for each coefficient }
  { This field is used only during compression.  It's initialized FALSE when
    the table is created, and set TRUE when it's been output to the file.
    You could suppress output of a table by setting this to TRUE.
    (See jpeg_suppress_tables for an example.) }
    sent_table : boolean;      { TRUE when table has been output }
  end;
  JQUANT_TBL_FIELD = Array[0..(MaxInt div SizeOf(JQUANT_TBL))-1] of JQUANT_TBL;

{ Huffman coding tables. }

type
  JHUFF_TBL_PTR = ^JHUFF_TBL;
  JHUFF_TBL = record
  { These two fields directly represent the contents of a JPEG DHT marker }
    bits : Array[0..17-1] of UINT8; { bits[k] = # of symbols with codes of }
                                    { length k bits; bits[0] is unused }
    huffval : Array[0..256-1] of UINT8;
                                    { The symbols, in order of incr code length }
  { This field is used only during compression.  It's initialized FALSE when
    the table is created, and set TRUE when it's been output to the file.
    You could suppress output of a table by setting this to TRUE.
    (See jpeg_suppress_tables for an example.) }
    sent_table : boolean;           { TRUE when table has been output }
  end;
  JHUFF_TBL_FIELD = Array[0..(MaxInt div SizeOf(JHUFF_TBL))-1] of JHUFF_TBL;

{ Declarations for both compression & decompression }

type
  J_BUF_MODE = (		{ Operating modes for buffer controllers }
	JBUF_PASS_THRU,		{ Plain stripwise operation }
	{ Remaining modes require a full-image buffer to have been created }
	JBUF_SAVE_SOURCE,	{ Run source subobject only, save output }
	JBUF_CRANK_DEST,	{ Run dest subobject only, using saved data }
	JBUF_SAVE_AND_PASS	{ Run both subobjects, save output }
               );

{ Values of global_state field (jdapi.c has some dependencies on ordering!) }
const
  CSTATE_START        = 100;    { after create_compress }
  CSTATE_SCANNING     = 101;    { start_compress done, write_scanlines OK }
  CSTATE_RAW_OK       = 102;    { start_compress done, write_raw_data OK }
  CSTATE_WRCOEFS      = 103;    { jpeg_write_coefficients done }
  DSTATE_START        = 200;    { after create_decompress }
  DSTATE_INHEADER     = 201;    { reading header markers, no SOS yet }
  DSTATE_READY        = 202;    { found SOS, ready for start_decompress }
  DSTATE_PRELOAD      = 203;    { reading multiscan file in start_decompress}
  DSTATE_PRESCAN      = 204;    { performing dummy pass for 2-pass quant }
  DSTATE_SCANNING     = 205;    { start_decompress done, read_scanlines OK }
  DSTATE_RAW_OK       = 206;    { start_decompress done, read_raw_data OK }
  DSTATE_BUFIMAGE     = 207;    { expecting jpeg_start_output }
  DSTATE_BUFPOST      = 208;    { looking for SOS/EOI in jpeg_finish_output }
  DSTATE_RDCOEFS      = 209;    { reading file in jpeg_read_coefficients }
  DSTATE_STOPPING     = 210;    { looking for EOI in jpeg_finish_decompress }


{ Basic info about one component (color channel). }

type
  jpeg_component_info_ptr = ^jpeg_component_info;
  jpeg_component_info = record
    { These values are fixed over the whole image. }
    { For compression, they must be supplied by parameter setup; }
    { for decompression, they are read from the SOF marker. }
    component_id : int;           { identifier for this component (0..255) }
    component_index : int;        { its index in SOF or cinfo^.comp_info[] }
    h_samp_factor : int;          { horizontal sampling factor (1..4) }
    v_samp_factor : int;          { vertical sampling factor (1..4) }
    quant_tbl_no : int;           { quantization table selector (0..3) }
    { These values may vary between scans. }
    { For compression, they must be supplied by parameter setup; }
    { for decompression, they are read from the SOS marker. }
    { The decompressor output side may not use these variables. }
    dc_tbl_no : int;              { DC entropy table selector (0..3) }
    ac_tbl_no : int;              { AC entropy table selector (0..3) }

    { Remaining fields should be treated as private by applications. }

    { These values are computed during compression or decompression startup: }
    { Component's size in DCT blocks.
      Any dummy blocks added to complete an MCU are not counted; therefore
      these values do not depend on whether a scan is interleaved or not. }
    width_in_blocks : JDIMENSION;
    height_in_blocks : JDIMENSION;
    { Size of a DCT block in samples.  Always DCTSIZE for compression.
      For decompression this is the size of the output from one DCT block,
      reflecting any scaling we choose to apply during the IDCT step.
      Values of 1,2,4,8 are likely to be supported.  Note that different
      components may receive different IDCT scalings. }

    DCT_scaled_size : int;
    { The downsampled dimensions are the component's actual, unpadded number
      of samples at the main buffer (preprocessing/compression interface), thus
      downsampled_width = ceil(image_width * Hi/Hmax)
      and similarly for height.  For decompression, IDCT scaling is included, so
      downsampled_width = ceil(image_width * Hi/Hmax * DCT_scaled_size/DCTSIZE)}

    downsampled_width : JDIMENSION;        { actual width in samples }
    downsampled_height : JDIMENSION;       { actual height in samples }
    { This flag is used only for decompression.  In cases where some of the
      components will be ignored (eg grayscale output from YCbCr image),
      we can skip most computations for the unused components. }

    component_needed : boolean;     { do we need the value of this component? }

    { These values are computed before starting a scan of the component. }
    { The decompressor output side may not use these variables. }
    MCU_width : int;      { number of blocks per MCU, horizontally }
    MCU_height : int;     { number of blocks per MCU, vertically }
    MCU_blocks : int;     { MCU_width * MCU_height }
    MCU_sample_width : int;       { MCU width in samples, MCU_width*DCT_scaled_size }
    last_col_width : int;         { # of non-dummy blocks across in last MCU }
    last_row_height : int;        { # of non-dummy blocks down in last MCU }

    { Saved quantization table for component; NIL if none yet saved.
      See jdinput.c comments about the need for this information.
      This field is currently used only for decompression. }

    quant_table : JQUANT_TBL_PTR;

    { Private per-component storage for DCT or IDCT subsystem. }
    dct_table : pointer;
  end; { record jpeg_component_info }

  jTCinfo = 0..(MaxInt div SizeOf(jpeg_component_info))-1;
  jpeg_component_info_array = array[jTCinfo] of jpeg_component_info;
  jpeg_component_info_list_ptr = ^jpeg_component_info_array;


{ The script for encoding a multiple-scan file is an array of these: }

type
  jpeg_scan_info_ptr = ^jpeg_scan_info;
  jpeg_scan_info = record
    comps_in_scan : int;                { number of components encoded in this scan }
    component_index : Array[0..MAX_COMPS_IN_SCAN-1] of int;
                                        { their SOF/comp_info[] indexes }
    Ss, Se : int;                       { progressive JPEG spectral selection parms }
    Ah, Al : int;                       { progressive JPEG successive approx. parms }
  end;

{ The decompressor can save APPn and COM markers in a list of these: }

type
  jpeg_saved_marker_ptr = ^jpeg_marker_struct;
  jpeg_marker_struct = record
    next : jpeg_saved_marker_ptr;    { next in list, or NULL }
    marker : UINT8;                  { marker code: JPEG_COM, or JPEG_APP0+n }
    original_length : uint;          { # bytes of data in the file }
    data_length : uint;              { # bytes of data saved at data[] }
    data : JOCTET_FIELD_PTR;         { the data contained in the marker }
   { the marker length word is not counted in data_length or original_length }
  end;

{ Known color spaces. }

type
  J_COLOR_SPACE = (
	JCS_UNKNOWN,            { error/unspecified }
	JCS_GRAYSCALE,          { monochrome }
	JCS_RGB,                { red/green/blue }
	JCS_YCbCr,              { Y/Cb/Cr (also known as YUV) }
	JCS_CMYK,               { C/M/Y/K }
	JCS_YCCK                { Y/Cb/Cr/K }
                  );

{ DCT/IDCT algorithm options. }

type
  J_DCT_METHOD = (
	JDCT_ISLOW,		{ slow but accurate integer algorithm }
	JDCT_IFAST,		{ faster, less accurate integer method }
	JDCT_FLOAT		{ floating-point: accurate, fast on fast HW }
                 );

const
  JDCT_DEFAULT = JDCT_ISLOW;
  JDCT_FASTEST = JDCT_IFAST;

{ Dithering options for decompression. }

type
  J_DITHER_MODE = (
    JDITHER_NONE,               { no dithering }
    JDITHER_ORDERED,            { simple ordered dither }
    JDITHER_FS                  { Floyd-Steinberg error diffusion dither }
                  );


const
  JPOOL_PERMANENT  = 0; { lasts until master record is destroyed }
  JPOOL_IMAGE      = 1; { lasts until done with image/datastream }
  JPOOL_NUMPOOLS   = 2;


{ "Object" declarations for JPEG modules that may be supplied or called
  directly by the surrounding application.
  As with all objects in the JPEG library, these structs only define the
  publicly visible methods and state variables of a module.  Additional
  private fields may exist after the public ones. }


{ Error handler object }

const
  JMSG_LENGTH_MAX  = 200;  { recommended size of format_message buffer }
  JMSG_STR_PARM_MAX = 80;


type
  jpeg_error_mgr_ptr = ^jpeg_error_mgr;
  jpeg_memory_mgr_ptr = ^jpeg_memory_mgr;
  jpeg_progress_mgr_ptr = ^jpeg_progress_mgr;


  j_common_ptr = ^jpeg_common_struct;
  j_compress_ptr = ^jpeg_compress_struct;
  j_decompress_ptr = ^jpeg_decompress_struct;


{ This structure holds whatever state is needed to access a single
  backing-store object.  The read/write/close method pointers are called
  by jmemmgr.c to manipulate the backing-store object; all other fields
  are private to the system-dependent backing store routines. }


  backing_store_ptr = ^backing_store_info;
  backing_store_info = record
  { Methods for reading/writing/closing this backing-store object }
    read_backing_store : procedure (cinfo : j_common_ptr;
				    info : backing_store_ptr;
				    buffer_address : pointer; {far}
				    file_offset : long;
                                    byte_count : long);
    write_backing_store : procedure (cinfo : j_common_ptr;
				     info : backing_store_ptr;
				     buffer_address : pointer;  {far}
				     file_offset : long;
                                     byte_count : long);

    close_backing_store : procedure (cinfo : j_common_ptr;
				     info : backing_store_ptr);

 end;


{ The control blocks for virtual arrays.
  Note that these blocks are allocated in the "small" pool area.
  System-dependent info for the associated backing store (if any) is hidden
  inside the backing_store_info struct. }

  jvirt_sarray_ptr = ^jvirt_sarray_control;
  jvirt_sarray_control = record
    mem_buffer : JSAMPARRAY;    { => the in-memory buffer }
    rows_in_array : JDIMENSION; { total virtual array height }
    samplesperrow : JDIMENSION; { width of array (and of memory buffer) }
    maxaccess : JDIMENSION;     { max rows accessed by access_virt_sarray }
    rows_in_mem : JDIMENSION;   { height of memory buffer }
    rowsperchunk : JDIMENSION;  { allocation chunk size in mem_buffer }
    cur_start_row : JDIMENSION; { first logical row # in the buffer }
    first_undef_row : JDIMENSION; { row # of first uninitialized row }
    pre_zero : boolean;         { pre-zero mode requested? }
    dirty : boolean;            { do current buffer contents need written? }
    b_s_open : boolean;         { is backing-store data valid? }
    next : jvirt_sarray_ptr;    { link to next virtual sarray control block }
    b_s_info : backing_store_info; { System-dependent control info }
  end;

  jvirt_barray_ptr = ^jvirt_barray_control;
  jvirt_barray_control = record
    mem_buffer : JBLOCKARRAY;   { => the in-memory buffer }
    rows_in_array : JDIMENSION; { total virtual array height }
    blocksperrow : JDIMENSION;	{ width of array (and of memory buffer) }
    maxaccess : JDIMENSION;     { max rows accessed by access_virt_barray }
    rows_in_mem : JDIMENSION;   { height of memory buffer }
    rowsperchunk : JDIMENSION;  { allocation chunk size in mem_buffer }
    cur_start_row : JDIMENSION; { first logical row # in the buffer }
    first_undef_row : JDIMENSION; { row # of first uninitialized row }
    pre_zero : boolean;         { pre-zero mode requested? }
    dirty : boolean;            { do current buffer contents need written? }
    b_s_open : boolean;         { is backing-store data valid? }
    next : jvirt_barray_ptr;    { link to next virtual barray control block }
    b_s_info : backing_store_info;  { System-dependent control info }
  end;


{ Declarations for compression modules }

{ Master control module }
  jpeg_comp_master_ptr = ^jpeg_comp_master;
  jpeg_comp_master = record
    prepare_for_pass : procedure(cinfo : j_compress_ptr);
    pass_startup : procedure(cinfo : j_compress_ptr);
    finish_pass : procedure(cinfo : j_compress_ptr);

    { State variables made visible to other modules }
    call_pass_startup : Boolean;   { True if pass_startup must be called }
    is_last_pass : Boolean;        { True during last pass }
  end;

{ Main buffer control (downsampled-data buffer) }
  jpeg_c_main_controller_ptr = ^jpeg_c_main_controller;
  jpeg_c_main_controller = record
    start_pass : procedure(cinfo : j_compress_ptr; pass_mode : J_BUF_MODE);
    process_data : procedure(cinfo : j_compress_ptr;
                             input_buf : JSAMPARRAY;
                             var in_row_ctr : JDIMENSION;
			     in_rows_avail : JDIMENSION);
  end;

{ Compression preprocessing (downsampling input buffer control) }
  jpeg_c_prep_controller_ptr = ^jpeg_c_prep_controller;
  jpeg_c_prep_controller = record
    start_pass : procedure(cinfo : j_compress_ptr; pass_mode : J_BUF_MODE);
    pre_process_data : procedure(cinfo : j_compress_ptr;
				 input_buf : JSAMPARRAY;
				 var in_row_ctr : JDIMENSION;
				 in_rows_avail : JDIMENSION;
				 output_buf : JSAMPIMAGE;
				 var out_row_group_ctr : JDIMENSION;
				 out_row_groups_avail : JDIMENSION);
  end;

{ Coefficient buffer control }
  jpeg_c_coef_controller_ptr = ^jpeg_c_coef_controller;
  jpeg_c_coef_controller = record
    start_pass : procedure(cinfo : j_compress_ptr; pass_mode : J_BUF_MODE);
    compress_data : function(cinfo : j_compress_ptr;
                             input_buf : JSAMPIMAGE) : boolean;
  end;

{ Colorspace conversion }
  jpeg_color_converter_ptr = ^jpeg_color_converter;
  jpeg_color_converter = record
    start_pass : procedure(cinfo : j_compress_ptr);
    color_convert : procedure(cinfo : j_compress_ptr;
                              input_buf : JSAMPARRAY;
                              output_buf : JSAMPIMAGE;
			      output_row : JDIMENSION;
                              num_rows : int);
  end;

{ Downsampling }
  jpeg_downsampler_ptr = ^jpeg_downsampler;
  jpeg_downsampler = record
    start_pass : procedure(cinfo : j_compress_ptr);
    downsample : procedure(cinfo : j_compress_ptr;
			   input_buf : JSAMPIMAGE;
                           in_row_index :  JDIMENSION;
			   output_buf : JSAMPIMAGE;
			   out_row_group_index: JDIMENSION);
    need_context_rows : Boolean;  { TRUE if need rows above & below }
  end;

{ Forward DCT (also controls coefficient quantization) }
  jpeg_forward_dct_ptr = ^jpeg_forward_dct;
  jpeg_forward_dct = record
    start_pass : procedure(cinfo : j_compress_ptr);
    { perhaps this should be an array??? }
    forward_DCT : procedure(cinfo : j_compress_ptr;
			    compptr : jpeg_component_info_ptr;
			    sample_data : JSAMPARRAY;
                            coef_blocks : JBLOCKROW;
			    start_row : JDIMENSION;
                            start_col : JDIMENSION;
			    num_blocks : JDIMENSION);
  end;

{ Entropy encoding }

  jpeg_entropy_encoder_ptr = ^jpeg_entropy_encoder;
  jpeg_entropy_encoder = record
    start_pass : procedure(cinfo : j_compress_ptr; gather_statistics : boolean);
    encode_mcu : function(cinfo : j_compress_ptr;
                          const MCU_data: array of JBLOCKROW) : boolean;
    finish_pass : procedure(cinfo : j_compress_ptr);
  end;

{ Marker writing }
  jpeg_marker_writer_ptr = ^jpeg_marker_writer;
  jpeg_marker_writer = record
    write_file_header : procedure(cinfo : j_compress_ptr);
    write_frame_header : procedure(cinfo : j_compress_ptr);
    write_scan_header : procedure(cinfo : j_compress_ptr);
    write_file_trailer : procedure(cinfo : j_compress_ptr);
    write_tables_only : procedure(cinfo : j_compress_ptr);
   { These routines are exported to allow insertion of extra markers }
   { Probably only COM and APPn markers should be written this way }
    write_marker_header : procedure (cinfo : j_compress_ptr;
                                     marker : int;
				     datalen : uint);
    write_marker_byte : procedure (cinfo : j_compress_ptr; val : int);
  end;

{ Declarations for decompression modules }

{ Master control module }
  jpeg_decomp_master_ptr = ^jpeg_decomp_master;
  jpeg_decomp_master = record
    prepare_for_output_pass : procedure( cinfo : j_decompress_ptr);
    finish_output_pass : procedure(cinfo : j_decompress_ptr);

    { State variables made visible to other modules }
    is_dummy_pass : Boolean;	{ True during 1st pass for 2-pass quant }
  end;

{ Input control module }
  jpeg_input_controller_ptr = ^jpeg_input_controller;
  jpeg_input_controller = record
    consume_input : function (cinfo : j_decompress_ptr) : int;
    reset_input_controller : procedure(cinfo : j_decompress_ptr);
    start_input_pass : procedure(cinfo : j_decompress_ptr);
    finish_input_pass : procedure(cinfo : j_decompress_ptr);

    { State variables made visible to other modules }
    has_multiple_scans : Boolean;  { True if file has multiple scans }
    eoi_reached : Boolean;         { True when EOI has been consumed }
  end;

{ Main buffer control (downsampled-data buffer) }

  jpeg_d_main_controller_ptr = ^jpeg_d_main_controller;
  jpeg_d_main_controller = record
    start_pass : procedure(cinfo : j_decompress_ptr; pass_mode : J_BUF_MODE);
    process_data : procedure(cinfo : j_decompress_ptr;
                             output_buf : JSAMPARRAY;
                             var out_row_ctr : JDIMENSION;
	                     out_rows_avail : JDIMENSION);
  end;

{ Coefficient buffer control }
  jvirt_barray_tbl = array[0..MAX_COMPONENTS-1] of jvirt_barray_ptr;
  jvirt_barray_tbl_ptr = ^jvirt_barray_tbl;
  jpeg_d_coef_controller_ptr = ^jpeg_d_coef_controller;
  jpeg_d_coef_controller = record
    start_input_pass : procedure(cinfo : j_decompress_ptr);
    consume_data : function (cinfo : j_decompress_ptr) : int;
    start_output_pass : procedure(cinfo : j_decompress_ptr);
    decompress_data : function (cinfo : j_decompress_ptr;
                                output_buf : JSAMPIMAGE) : int;
  { Pointer to array of coefficient virtual arrays, or NIL if none }
    coef_arrays : jvirt_barray_tbl_ptr;
  end;

{ Decompression postprocessing (color quantization buffer control) }
  jpeg_d_post_controller_ptr = ^jpeg_d_post_controller;
  jpeg_d_post_controller = record
    start_pass : procedure(cinfo : j_decompress_ptr;
                           pass_mode : J_BUF_MODE);
    post_process_data : procedure(cinfo : j_decompress_ptr;
				  input_buf : JSAMPIMAGE;
				  var in_row_group_ctr : JDIMENSION;
				  in_row_groups_avail : JDIMENSION;
				  output_buf : JSAMPARRAY;
				  var out_row_ctr : JDIMENSION;
				  out_rows_avail : JDIMENSION);
  end;


{ Routine signature for application-supplied marker processing methods.
  Need not pass marker code since it is stored in cinfo^.unread_marker. }

  jpeg_marker_parser_method = function(cinfo : j_decompress_ptr) : boolean;

{ Marker reading & parsing }
  jpeg_marker_reader_ptr = ^jpeg_marker_reader;
  jpeg_marker_reader = record
    reset_marker_reader : procedure(cinfo : j_decompress_ptr);
    { Read markers until SOS or EOI.
      Returns same codes as are defined for jpeg_consume_input:
      JPEG_SUSPENDED, JPEG_REACHED_SOS, or JPEG_REACHED_EOI. }

    read_markers : function (cinfo : j_decompress_ptr) : int;
    { Read a restart marker --- exported for use by entropy decoder only }
    read_restart_marker : jpeg_marker_parser_method;

    { State of marker reader --- nominally internal, but applications
      supplying COM or APPn handlers might like to know the state. }

    saw_SOI : boolean;            { found SOI? }
    saw_SOF : boolean;            { found SOF? }
    next_restart_num : int;       { next restart number expected (0-7) }
    discarded_bytes : uint;       { # of bytes skipped looking for a marker }
  end;

{ Entropy decoding }
  jpeg_entropy_decoder_ptr = ^jpeg_entropy_decoder;
  jpeg_entropy_decoder = record
    start_pass : procedure(cinfo : j_decompress_ptr);
    decode_mcu : function(cinfo : j_decompress_ptr;
                          var MCU_data : array of JBLOCKROW) : boolean;
  { This is here to share code between baseline and progressive decoders; }
  { other modules probably should not use it }
    insufficient_data : BOOLEAN;  { set TRUE after emitting warning }
  end;

{ Inverse DCT (also performs dequantization) }
  inverse_DCT_method_ptr = procedure(cinfo : j_decompress_ptr;
                 compptr : jpeg_component_info_ptr;
		 coef_block : JCOEFPTR;
		 output_buf : JSAMPARRAY; output_col : JDIMENSION);

  jpeg_inverse_dct_ptr = ^jpeg_inverse_dct;
  jpeg_inverse_dct = record
    start_pass : procedure(cinfo : j_decompress_ptr);
    { It is useful to allow each component to have a separate IDCT method. }
    inverse_DCT : Array[0..MAX_COMPONENTS-1] of inverse_DCT_method_ptr;
  end;

{ Upsampling (note that upsampler must also call color converter) }
  jpeg_upsampler_ptr = ^jpeg_upsampler;
  jpeg_upsampler = record
    start_pass : procedure(cinfo : j_decompress_ptr);
    upsample : procedure(cinfo : j_decompress_ptr;
                   input_buf : JSAMPIMAGE;
		   var in_row_group_ctr : JDIMENSION;  { array of }
		   in_row_groups_avail : JDIMENSION;
		   output_buf : JSAMPARRAY;
		   var out_row_ctr : JDIMENSION;
		   out_rows_avail : JDIMENSION);

    need_context_rows : boolean;  { TRUE if need rows above & below }
  end;

{ Colorspace conversion }
  jpeg_color_deconverter_ptr = ^jpeg_color_deconverter;
  jpeg_color_deconverter = record
    start_pass : procedure(cinfo: j_decompress_ptr);
    color_convert : procedure(cinfo : j_decompress_ptr;
                              input_buf : JSAMPIMAGE;
                              input_row : JDIMENSION;
                              output_buf : JSAMPARRAY;
                              num_rows : int);
  end;

{ Color quantization or color precision reduction }
  jpeg_color_quantizer_ptr = ^jpeg_color_quantizer;
  jpeg_color_quantizer = record
    start_pass : procedure(cinfo : j_decompress_ptr; is_pre_scan : boolean);
    color_quantize : procedure(cinfo : j_decompress_ptr;
                               input_buf : JSAMPARRAY;
                               output_buf : JSAMPARRAY;
                               num_rows : int);

    finish_pass : procedure(cinfo : j_decompress_ptr);
    new_color_map : procedure(cinfo : j_decompress_ptr);
  end;

  {int8array = Array[0..8-1] of int;}
  int8array = Array[0..8-1] of longint; { for TP FormatStr }

  jpeg_error_mgr = record
    { Error exit handler: does not return to caller }
    error_exit : procedure  (cinfo : j_common_ptr);
    { Conditionally emit a trace or warning message }
    emit_message : procedure (cinfo : j_common_ptr; msg_level : int);
    { Routine that actually outputs a trace or error message }
    output_message : procedure (cinfo : j_common_ptr);
    { Format a message string for the most recent JPEG error or message }
    format_message : procedure  (cinfo : j_common_ptr; var buffer : AnsiString);

    { Reset error state variables at start of a new image }
    reset_error_mgr : procedure (cinfo : j_common_ptr);

    { The message ID code and any parameters are saved here.
      A message can have one string parameter or up to 8 int parameters. }

    msg_code : int;

    msg_parm : record
      case byte of
      0:(i : int8array);
      1:(s : string[JMSG_STR_PARM_MAX]);
    end;

    { Standard state variables for error facility }

    trace_level : int;         { max msg_level that will be displayed }

    { For recoverable corrupt-data errors, we emit a warning message,
      but keep going unless emit_message chooses to abort.  emit_message
      should count warnings in num_warnings.  The surrounding application
      can check for bad data by seeing if num_warnings is nonzero at the
      end of processing. }

    num_warnings : long;       { number of corrupt-data warnings }

    { These fields point to the table(s) of error message strings.
      An application can change the table pointer to switch to a different
      message list (typically, to change the language in which errors are
      reported).  Some applications may wish to add additional error codes
      that will be handled by the JPEG library error mechanism; the second
      table pointer is used for this purpose.

      First table includes all errors generated by JPEG library itself.
      Error code 0 is reserved for a "no such error string" message. }

    {const char * const * jpeg_message_table; }
    jpeg_message_table : ^msg_table; { Library errors }

    last_jpeg_message : J_MESSAGE_CODE;
      { Table contains strings 0..last_jpeg_message }
    { Second table can be added by application (see cjpeg/djpeg for example).
      It contains strings numbered first_addon_message..last_addon_message. }

    {const char * const * addon_message_table; }
    addon_message_table : ^msg_table; { Non-library errors }

    first_addon_message : J_MESSAGE_CODE;  { code for first string in addon table }
    last_addon_message : J_MESSAGE_CODE;   { code for last string in addon table }
  end;


{ Progress monitor object }

  jpeg_progress_mgr = record
    progress_monitor : procedure(cinfo : j_common_ptr);

    pass_counter : long;        { work units completed in this pass }
    pass_limit : long;          { total number of work units in this pass }
    completed_passes : int;	{ passes completed so far }
    total_passes : int;         { total number of passes expected }
  end;


{ Data destination object for compression }
  jpeg_destination_mgr_ptr = ^jpeg_destination_mgr;
  jpeg_destination_mgr = record
    next_output_byte : JOCTETptr;  { => next byte to write in buffer }
    free_in_buffer : size_t;    { # of byte spaces remaining in buffer }

    init_destination : procedure (cinfo : j_compress_ptr);
    empty_output_buffer : function (cinfo : j_compress_ptr) : boolean;
    term_destination : procedure (cinfo : j_compress_ptr);
  end;


{ Data source object for decompression }

  jpeg_source_mgr_ptr = ^jpeg_source_mgr;
  jpeg_source_mgr = record
    {const JOCTET * next_input_byte;}
    next_input_byte : JOCTETptr;      { => next byte to read from buffer }
    bytes_in_buffer : size_t;       { # of bytes remaining in buffer }

    init_source : procedure  (cinfo : j_decompress_ptr);
    fill_input_buffer : function (cinfo : j_decompress_ptr) : boolean;
    skip_input_data : procedure (cinfo : j_decompress_ptr; num_bytes : long);
    resync_to_restart : function (cinfo : j_decompress_ptr;
                                  desired : int) : boolean;
    term_source : procedure (cinfo : j_decompress_ptr);
  end;


{ Memory manager object.
  Allocates "small" objects (a few K total), "large" objects (tens of K),
  and "really big" objects (virtual arrays with backing store if needed).
  The memory manager does not allow individual objects to be freed; rather,
  each created object is assigned to a pool, and whole pools can be freed
  at once.  This is faster and more convenient than remembering exactly what
  to free, especially where malloc()/free() are not too speedy.
  NB: alloc routines never return NIL.  They exit to error_exit if not
  successful. }


  jpeg_memory_mgr = record
    { Method pointers }
    alloc_small : function (cinfo : j_common_ptr; pool_id : int;
				  sizeofobject : size_t) : pointer;
    alloc_large : function (cinfo : j_common_ptr; pool_id : int;
				  sizeofobject : size_t) : pointer; {far}
    alloc_sarray : function (cinfo : j_common_ptr; pool_id : int;
                             samplesperrow : JDIMENSION;
                             numrows : JDIMENSION) : JSAMPARRAY;

    alloc_barray : function (cinfo : j_common_ptr; pool_id : int;
                             blocksperrow : JDIMENSION;
                             numrows : JDIMENSION) : JBLOCKARRAY;

    request_virt_sarray : function(cinfo : j_common_ptr;
                                   pool_id : int;
                                   pre_zero : boolean;
                                   samplesperrow : JDIMENSION;
                                   numrows : JDIMENSION;
                                   maxaccess : JDIMENSION) : jvirt_sarray_ptr;

    request_virt_barray : function(cinfo : j_common_ptr;
                                   pool_id : int;
                                   pre_zero : boolean;
                                   blocksperrow : JDIMENSION;
                                   numrows : JDIMENSION;
                                   maxaccess : JDIMENSION) : jvirt_barray_ptr;

    realize_virt_arrays : procedure (cinfo : j_common_ptr);

    access_virt_sarray : function (cinfo : j_common_ptr;
                                   ptr : jvirt_sarray_ptr;
                                   start_row : JDIMENSION;
                                   num_rows : JDIMENSION;
				   writable : boolean) : JSAMPARRAY;

    access_virt_barray : function (cinfo : j_common_ptr;
                                   ptr : jvirt_barray_ptr;
                                   start_row : JDIMENSION;
                                   num_rows : JDIMENSION;
                                   writable : boolean) : JBLOCKARRAY;

    free_pool : procedure  (cinfo : j_common_ptr; pool_id : int);
    self_destruct : procedure (cinfo : j_common_ptr);

    { Limit on memory allocation for this JPEG object.  (Note that this is
      merely advisory, not a guaranteed maximum; it only affects the space
      used for virtual-array buffers.)  May be changed by outer application
      after creating the JPEG object. }
    max_memory_to_use : long;

    { Maximum allocation request accepted by alloc_large. }
    max_alloc_chunk : long;
  end;

{ Routines that are to be used by both halves of the library are declared
  to receive a pointer to this structure.  There are no actual instances of
  jpeg_common_struct, only of jpeg_compress_struct and jpeg_decompress_struct.}
  jpeg_common_struct = record
  { Fields common to both master struct types }
    err : jpeg_error_mgr_ptr;           { Error handler module }
    mem : jpeg_memory_mgr_ptr;          { Memory manager module }
    progress : jpeg_progress_mgr_ptr;   { Progress monitor, or NIL if none }
    client_data : voidp;                { Available for use by application }
    is_decompressor : boolean;     { so common code can tell which is which }
    global_state : int;            { for checking call sequence validity }

  { Additional fields follow in an actual jpeg_compress_struct or
    jpeg_decompress_struct.  All three structs must agree on these
    initial fields!  (This would be a lot cleaner in C++.) }
  end;


{ Master record for a compression instance }

  jpeg_compress_struct = record
    { Fields shared with jpeg_decompress_struct }
    err : jpeg_error_mgr_ptr;          { Error handler module }
    mem : jpeg_memory_mgr_ptr;         { Memory manager module }
    progress : jpeg_progress_mgr_ptr;  { Progress monitor, or NIL if none }
    client_data : voidp;               { Available for use by application }
    is_decompressor : boolean;      { so common code can tell which is which }
    global_state : int;             { for checking call sequence validity }

  { Destination for compressed data }
    dest : jpeg_destination_mgr_ptr;

  { Description of source image --- these fields must be filled in by
    outer application before starting compression.  in_color_space must
    be correct before you can even call jpeg_set_defaults(). }


    image_width : JDIMENSION;         { input image width }
    image_height : JDIMENSION;        { input image height }
    input_components : int;           { # of color components in input image }
    in_color_space : J_COLOR_SPACE;   { colorspace of input image }

    input_gamma : double;             { image gamma of input image }

    { Compression parameters --- these fields must be set before calling
      jpeg_start_compress().  We recommend calling jpeg_set_defaults() to
      initialize everything to reasonable defaults, then changing anything
      the application specifically wants to change.  That way you won't get
      burnt when new parameters are added.  Also note that there are several
      helper routines to simplify changing parameters. }

    data_precision : int;             { bits of precision in image data }

    num_components : int;             { # of color components in JPEG image }
    jpeg_color_space : J_COLOR_SPACE; { colorspace of JPEG image }

    comp_info : jpeg_component_info_list_ptr;
    { comp_info^[i] describes component that appears i'th in SOF }

    quant_tbl_ptrs: Array[0..NUM_QUANT_TBLS-1] of JQUANT_TBL_PTR;
    { ptrs to coefficient quantization tables, or NIL if not defined }

    dc_huff_tbl_ptrs : Array[0..NUM_HUFF_TBLS-1] of JHUFF_TBL_PTR;
    ac_huff_tbl_ptrs : Array[0..NUM_HUFF_TBLS-1] of JHUFF_TBL_PTR;
    { ptrs to Huffman coding tables, or NIL if not defined }

    arith_dc_L : Array[0..NUM_ARITH_TBLS-1] of UINT8; { L values for DC arith-coding tables }
    arith_dc_U : Array[0..NUM_ARITH_TBLS-1] of UINT8; { U values for DC arith-coding tables }
    arith_ac_K : Array[0..NUM_ARITH_TBLS-1] of UINT8; { Kx values for AC arith-coding tables }

    num_scans : int;		 { # of entries in scan_info array }
    scan_info : jpeg_scan_info_ptr; { script for multi-scan file, or NIL }
    { The default value of scan_info is NIL, which causes a single-scan
      sequential JPEG file to be emitted.  To create a multi-scan file,
      set num_scans and scan_info to point to an array of scan definitions. }

    raw_data_in : boolean;        { TRUE=caller supplies downsampled data }
    arith_code : boolean;         { TRUE=arithmetic coding, FALSE=Huffman }
    optimize_coding : boolean;    { TRUE=optimize entropy encoding parms }
    CCIR601_sampling : boolean;   { TRUE=first samples are cosited }
    smoothing_factor : int;       { 1..100, or 0 for no input smoothing }
    dct_method : J_DCT_METHOD;    { DCT algorithm selector }

    { The restart interval can be specified in absolute MCUs by setting
      restart_interval, or in MCU rows by setting restart_in_rows
      (in which case the correct restart_interval will be figured
      for each scan). }

    restart_interval : uint;      { MCUs per restart, or 0 for no restart }
    restart_in_rows : int;        { if > 0, MCU rows per restart interval }

    { Parameters controlling emission of special markers. }

    write_JFIF_header : boolean; { should a JFIF marker be written? }
    JFIF_major_version : UINT8;  { What to write for the JFIF version number }
    JFIF_minor_version : UINT8;
    { These three values are not used by the JPEG code, merely copied }
    { into the JFIF APP0 marker.  density_unit can be 0 for unknown, }
    { 1 for dots/inch, or 2 for dots/cm.  Note that the pixel aspect }
    { ratio is defined by X_density/Y_density even when density_unit=0. }
    density_unit : UINT8;         { JFIF code for pixel size units }
    X_density : UINT16;           { Horizontal pixel density }
    Y_density : UINT16;           { Vertical pixel density }
    write_Adobe_marker : boolean; { should an Adobe marker be written? }

    { State variable: index of next scanline to be written to
      jpeg_write_scanlines().  Application may use this to control its
      processing loop, e.g., "while (next_scanline < image_height)". }

    next_scanline : JDIMENSION;   { 0 .. image_height-1  }

    { Remaining fields are known throughout compressor, but generally
      should not be touched by a surrounding application. }

    { These fields are computed during compression startup }
    progressive_mode : boolean;   { TRUE if scan script uses progressive mode }
    max_h_samp_factor : int;      { largest h_samp_factor }
    max_v_samp_factor : int;      { largest v_samp_factor }

    total_iMCU_rows : JDIMENSION; { # of iMCU rows to be input to coef ctlr }
    { The coefficient controller receives data in units of MCU rows as defined
      for fully interleaved scans (whether the JPEG file is interleaved or not).
      There are v_samp_factor * DCTSIZE sample rows of each component in an
      "iMCU" (interleaved MCU) row. }

    { These fields are valid during any one scan.
      They describe the components and MCUs actually appearing in the scan. }

    comps_in_scan : int;          { # of JPEG components in this scan }
    cur_comp_info : Array[0..MAX_COMPS_IN_SCAN-1] of jpeg_component_info_ptr;
    { cur_comp_info[i]^ describes component that appears i'th in SOS }

    MCUs_per_row : JDIMENSION;    { # of MCUs across the image }
    MCU_rows_in_scan : JDIMENSION;{ # of MCU rows in the image }

    blocks_in_MCU : int;          { # of DCT blocks per MCU }
    MCU_membership : Array[0..C_MAX_BLOCKS_IN_MCU-1] of int;
    { MCU_membership[i] is index in cur_comp_info of component owning }
    { i'th block in an MCU }

    Ss, Se, Ah, Al : int;         { progressive JPEG parameters for scan }

    { Links to compression subobjects (methods and private variables of modules) }
    master : jpeg_comp_master_ptr;
    main : jpeg_c_main_controller_ptr;
    prep : jpeg_c_prep_controller_ptr;
    coef : jpeg_c_coef_controller_ptr;
    marker : jpeg_marker_writer_ptr;
    cconvert : jpeg_color_converter_ptr;
    downsample : jpeg_downsampler_ptr;
    fdct : jpeg_forward_dct_ptr;
    entropy : jpeg_entropy_encoder_ptr;
    script_space : jpeg_scan_info_ptr; { workspace for jpeg_simple_progression }
    script_space_size : int;
  end;


{ Master record for a decompression instance }

  coef_bits_field = Array[0..DCTSIZE2-1] of int;
  coef_bits_ptr = ^coef_bits_field;
  coef_bits_ptrfield =  Array[0..MAX_COMPS_IN_SCAN-1] of coef_bits_field;
  coef_bits_ptrrow = ^coef_bits_ptrfield;

  range_limit_table = array[-(MAXJSAMPLE+1)..4*(MAXJSAMPLE+1)
                            + CENTERJSAMPLE -1] of JSAMPLE;
  range_limit_table_ptr = ^range_limit_table;

  jpeg_decompress_struct = record
  { Fields shared with jpeg_compress_struct }
    err : jpeg_error_mgr_ptr;	   { Error handler module }
    mem : jpeg_memory_mgr_ptr;        { Memory manager module }
    progress : jpeg_progress_mgr_ptr; { Progress monitor, or NIL if none }
    client_data : voidp;              { Available for use by application }
    is_decompressor : boolean;     { so common code can tell which is which }
    global_state : int;            { for checking call sequence validity }

    { Source of compressed data }
    src : jpeg_source_mgr_ptr;

    { Basic description of image --- filled in by jpeg_read_header(). }
    { Application may inspect these values to decide how to process image. }

    image_width : JDIMENSION;      { nominal image width (from SOF marker) }
    image_height : JDIMENSION;     { nominal image height }
    num_components : int;          { # of color components in JPEG image }
    jpeg_color_space : J_COLOR_SPACE; { colorspace of JPEG image }

    { Decompression processing parameters --- these fields must be set before
      calling jpeg_start_decompress().  Note that jpeg_read_header()
      initializes them to default values. }

    out_color_space : J_COLOR_SPACE; { colorspace for output }

    scale_num, scale_denom : uint ;  { fraction by which to scale image }

    output_gamma : double;           { image gamma wanted in output }

    buffered_image : boolean;        { TRUE=multiple output passes }
    raw_data_out : boolean;          { TRUE=downsampled data wanted }

    dct_method : J_DCT_METHOD;       { IDCT algorithm selector }
    do_fancy_upsampling : boolean;   { TRUE=apply fancy upsampling }
    do_block_smoothing : boolean;    { TRUE=apply interblock smoothing }

    quantize_colors : boolean;       { TRUE=colormapped output wanted }
    { the following are ignored if not quantize_colors: }
    dither_mode : J_DITHER_MODE;     { type of color dithering to use }
    two_pass_quantize : boolean;     { TRUE=use two-pass color quantization }
    desired_number_of_colors : int;  { max # colors to use in created colormap }
    { these are significant only in buffered-image mode: }
    enable_1pass_quant : boolean;    { enable future use of 1-pass quantizer }
    enable_external_quant : boolean; { enable future use of external colormap }
    enable_2pass_quant : boolean;    { enable future use of 2-pass quantizer }

    { Description of actual output image that will be returned to application.
      These fields are computed by jpeg_start_decompress().
      You can also use jpeg_calc_output_dimensions() to determine these values
      in advance of calling jpeg_start_decompress(). }

    output_width : JDIMENSION;       { scaled image width }
    output_height: JDIMENSION;       { scaled image height }
    out_color_components : int;  { # of color components in out_color_space }
    output_components : int;     { # of color components returned }
    { output_components is 1 (a colormap index) when quantizing colors;
      otherwise it equals out_color_components. }

    rec_outbuf_height : int;     { min recommended height of scanline buffer }
    { If the buffer passed to jpeg_read_scanlines() is less than this many
      rows high, space and time will be wasted due to unnecessary data
      copying. Usually rec_outbuf_height will be 1 or 2, at most 4. }

    { When quantizing colors, the output colormap is described by these
      fields. The application can supply a colormap by setting colormap
      non-NIL before calling jpeg_start_decompress; otherwise a colormap
      is created during jpeg_start_decompress or jpeg_start_output. The map
      has out_color_components rows and actual_number_of_colors columns. }

    actual_number_of_colors : int;      { number of entries in use }
    colormap : JSAMPARRAY;              { The color map as a 2-D pixel array }

    { State variables: these variables indicate the progress of decompression.
      The application may examine these but must not modify them. }

    { Row index of next scanline to be read from jpeg_read_scanlines().
      Application may use this to control its processing loop, e.g.,
      "while (output_scanline < output_height)". }

    output_scanline : JDIMENSION; { 0 .. output_height-1  }

    { Current input scan number and number of iMCU rows completed in scan.
      These indicate the progress of the decompressor input side. }

    input_scan_number : int;      { Number of SOS markers seen so far }
    input_iMCU_row : JDIMENSION;  { Number of iMCU rows completed }

    { The "output scan number" is the notional scan being displayed by the
      output side.  The decompressor will not allow output scan/row number
      to get ahead of input scan/row, but it can fall arbitrarily far behind.}

    output_scan_number : int;     { Nominal scan number being displayed }
    output_iMCU_row : int;        { Number of iMCU rows read }

    { Current progression status.  coef_bits[c][i] indicates the precision
      with which component c's DCT coefficient i (in zigzag order) is known.
      It is -1 when no data has yet been received, otherwise it is the point
      transform (shift) value for the most recent scan of the coefficient
      (thus, 0 at completion of the progression).
      This pointer is NIL when reading a non-progressive file. }

    coef_bits : coef_bits_ptrrow;
                 { -1 or current Al value for each coef }

    { Internal JPEG parameters --- the application usually need not look at
      these fields.  Note that the decompressor output side may not use
      any parameters that can change between scans. }

    { Quantization and Huffman tables are carried forward across input
      datastreams when processing abbreviated JPEG datastreams. }

    quant_tbl_ptrs : Array[0..NUM_QUANT_TBLS-1] of JQUANT_TBL_PTR;
    { ptrs to coefficient quantization tables, or NIL if not defined }

    dc_huff_tbl_ptrs : Array[0..NUM_HUFF_TBLS-1] of JHUFF_TBL_PTR;
    ac_huff_tbl_ptrs : Array[0..NUM_HUFF_TBLS-1] of JHUFF_TBL_PTR;
    { ptrs to Huffman coding tables, or NIL if not defined }

    { These parameters are never carried across datastreams, since they
      are given in SOF/SOS markers or defined to be reset by SOI. }

    data_precision : int;          { bits of precision in image data }

    comp_info : jpeg_component_info_list_ptr;
    { comp_info^[i] describes component that appears i'th in SOF }

    progressive_mode : boolean;    { TRUE if SOFn specifies progressive mode }
    arith_code : boolean;          { TRUE=arithmetic coding, FALSE=Huffman }

    arith_dc_L : Array[0..NUM_ARITH_TBLS-1] of UINT8; { L values for DC arith-coding tables }
    arith_dc_U : Array[0..NUM_ARITH_TBLS-1] of UINT8; { U values for DC arith-coding tables }
    arith_ac_K : Array[0..NUM_ARITH_TBLS-1] of UINT8; { Kx values for AC arith-coding tables }

    restart_interval : uint; { MCUs per restart interval, or 0 for no restart }

    { These fields record data obtained from optional markers recognized by
      the JPEG library. }

    saw_JFIF_marker : boolean;  { TRUE iff a JFIF APP0 marker was found }
    { Data copied from JFIF marker; only valid if saw_JFIF_marker is TRUE: }
    JFIF_major_version : UINT8;	{ JFIF version number }
    JFIF_minor_version : UINT8;
    density_unit : UINT8;       { JFIF code for pixel size units }
    X_density : UINT16;         { Horizontal pixel density }
    Y_density : UINT16;         { Vertical pixel density }
    saw_Adobe_marker : boolean; { TRUE iff an Adobe APP14 marker was found }
    Adobe_transform : UINT8;    { Color transform code from Adobe marker }

    CCIR601_sampling : boolean; { TRUE=first samples are cosited }

    { Aside from the specific data retained from APPn markers known to the
      library, the uninterpreted contents of any or all APPn and COM markers
      can be saved in a list for examination by the application. }

    marker_list : jpeg_saved_marker_ptr; { Head of list of saved markers }

    { Remaining fields are known throughout decompressor, but generally
      should not be touched by a surrounding application. }


    { These fields are computed during decompression startup }

    max_h_samp_factor : int;    { largest h_samp_factor }
    max_v_samp_factor : int;    { largest v_samp_factor }

    min_DCT_scaled_size : int;  { smallest DCT_scaled_size of any component }

    total_iMCU_rows : JDIMENSION; { # of iMCU rows in image }
    { The coefficient controller's input and output progress is measured in
      units of "iMCU" (interleaved MCU) rows.  These are the same as MCU rows
      in fully interleaved JPEG scans, but are used whether the scan is
      interleaved or not.  We define an iMCU row as v_samp_factor DCT block
      rows of each component.  Therefore, the IDCT output contains
      v_samp_factor*DCT_scaled_size sample rows of a component per iMCU row.}

    sample_range_limit : range_limit_table_ptr; { table for fast range-limiting }


    { These fields are valid during any one scan.
      They describe the components and MCUs actually appearing in the scan.
      Note that the decompressor output side must not use these fields. }

    comps_in_scan : int;           { # of JPEG components in this scan }
    cur_comp_info : Array[0..MAX_COMPS_IN_SCAN-1] of jpeg_component_info_ptr;
    { cur_comp_info[i]^ describes component that appears i'th in SOS }

    MCUs_per_row : JDIMENSION;     { # of MCUs across the image }
    MCU_rows_in_scan : JDIMENSION; { # of MCU rows in the image }

    blocks_in_MCU : JDIMENSION;    { # of DCT blocks per MCU }
    MCU_membership : Array[0..D_MAX_BLOCKS_IN_MCU-1] of int;
    { MCU_membership[i] is index in cur_comp_info of component owning }
    { i'th block in an MCU }

    Ss, Se, Ah, Al : int;          { progressive JPEG parameters for scan }

    { This field is shared between entropy decoder and marker parser.
      It is either zero or the code of a JPEG marker that has been
      read from the data source, but has not yet been processed. }

    unread_marker : int;

    { Links to decompression subobjects
      (methods, private variables of modules) }

    master : jpeg_decomp_master_ptr;
    main : jpeg_d_main_controller_ptr;
    coef : jpeg_d_coef_controller_ptr;
    post : jpeg_d_post_controller_ptr;
    inputctl : jpeg_input_controller_ptr;
    marker : jpeg_marker_reader_ptr;
    entropy : jpeg_entropy_decoder_ptr;
    idct : jpeg_inverse_dct_ptr;
    upsample : jpeg_upsampler_ptr;
    cconvert : jpeg_color_deconverter_ptr;
    cquantize : jpeg_color_quantizer_ptr;
  end;

{ Decompression startup: read start of JPEG datastream to see what's there
   function jpeg_read_header (cinfo : j_decompress_ptr;
                              require_image : boolean) : int;
  Return value is one of: }
const
  JPEG_SUSPENDED              = 0; { Suspended due to lack of input data }
  JPEG_HEADER_OK              = 1; { Found valid image datastream }
  JPEG_HEADER_TABLES_ONLY     = 2; { Found valid table-specs-only datastream }
{ If you pass require_image = TRUE (normal case), you need not check for
  a TABLES_ONLY return code; an abbreviated file will cause an error exit.
  JPEG_SUSPENDED is only possible if you use a data source module that can
  give a suspension return (the stdio source module doesn't). }


{ function jpeg_consume_input (cinfo : j_decompress_ptr) : int;
  Return value is one of: }

  JPEG_REACHED_SOS            = 1; { Reached start of new scan }
  JPEG_REACHED_EOI            = 2; { Reached end of image }
  JPEG_ROW_COMPLETED          = 3; { Completed one iMCU row }
  JPEG_SCAN_COMPLETED         = 4; { Completed last iMCU row of a scan }

{== imjinclude interface ==}

{ This file exists to provide a single place to fix any problems with
  including the wrong system include files.  (Common problems are taken
  care of by the standard jconfig symbols, but on really weird systems
  you may have to edit this file.)

  NOTE: this file is NOT intended to be included by applications using the
  JPEG library.  Most applications need only include jpeglib.h. }

{ Original: jinclude.h Copyright (C) 1991-1994, Thomas G. Lane. }


{ Include auto-config file to find out which system include files we need. }


{ We need the NULL macro and size_t typedef.
  On an ANSI-conforming system it is sufficient to include <stddef.h>.
  Otherwise, we get them from <stdlib.h> or <stdio.h>; we may have to
  pull in <sys/types.h> as well.
  Note that the core JPEG library does not require <stdio.h>;
  only the default error handler and data source/destination modules do.
  But we must pull it in because of the references to FILE in jpeglib.h.
  You can remove those references if you want to compile without <stdio.h>.}


{ We need memory copying and zeroing functions, plus strncpy().
  ANSI and System V implementations declare these in <string.h>.
  BSD doesn't have the mem() functions, but it does have bcopy()/bzero().
  Some systems may declare memset and memcpy in <memory.h>.

  NOTE: we assume the size parameters to these functions are of type size_t.
  Change the casts in these macros if not! }

procedure MEMZERO(target : pointer; size : size_t);

procedure MEMCOPY(dest, src : pointer; size : size_t);

{function SIZEOF(object) : size_t;}

{== imjutils interface ==}

{ This file contains tables and miscellaneous utility routines needed
  for both compression and decompression.
  Note we prefix all global names with "j" to minimize conflicts with
  a surrounding application. }

{ Source: jutils.c; Copyright (C) 1991-1996, Thomas G. Lane. }


{ jpeg_zigzag_order[i] is the zigzag-order position of the i'th element
  of a DCT block read in natural order (left to right, top to bottom). }


{$ifdef FALSE}      { This table is not actually needed in v6a }

const
  jpeg_zigzag_order : array[0..DCTSIZE2] of int =
  (0,  1,  5,  6, 14, 15, 27, 28,
   2,  4,  7, 13, 16, 26, 29, 42,
   3,  8, 12, 17, 25, 30, 41, 43,
   9, 11, 18, 24, 31, 40, 44, 53,
  10, 19, 23, 32, 39, 45, 52, 54,
  20, 22, 33, 38, 46, 51, 55, 60,
  21, 34, 37, 47, 50, 56, 59, 61,
  35, 36, 48, 49, 57, 58, 62, 63);

{$endif}


{ jpeg_natural_order[i] is the natural-order position of the i'th element
  of zigzag order.

  When reading corrupted data, the Huffman decoders could attempt
  to reference an entry beyond the end of this array (if the decoded
  zero run length reaches past the end of the block).  To prevent
  wild stores without adding an inner-loop test, we put some extra
  "63"s after the real entries.  This will cause the extra coefficient
  to be stored in location 63 of the block, not somewhere random.
  The worst case would be a run-length of 15, which means we need 16
  fake entries. }


const
  jpeg_natural_order : array[0..DCTSIZE2+16-1] of int =
 (0,  1,  8, 16,  9,  2,  3, 10,
 17, 24, 32, 25, 18, 11,  4,  5,
 12, 19, 26, 33, 40, 48, 41, 34,
 27, 20, 13,  6,  7, 14, 21, 28,
 35, 42, 49, 56, 57, 50, 43, 36,
 29, 22, 15, 23, 30, 37, 44, 51,
 58, 59, 52, 45, 38, 31, 39, 46,
 53, 60, 61, 54, 47, 55, 62, 63,
 63, 63, 63, 63, 63, 63, 63, 63, { extra entries for safety in decoder }
 63, 63, 63, 63, 63, 63, 63, 63);


{ Arithmetic utilities }

{GLOBAL}
function jdiv_round_up (a : long; b : long) : long;

{GLOBAL}
function jround_up (a : long; b : long) : long;

{GLOBAL}
procedure jcopy_sample_rows (input_array : JSAMPARRAY;
                             source_row : int;
                             output_array : JSAMPARRAY; dest_row : int;
		             num_rows : int; num_cols : JDIMENSION);

{GLOBAL}
procedure jcopy_block_row (input_row : JBLOCKROW;
                           output_row : JBLOCKROW;
                           num_blocks : JDIMENSION);

{GLOBAL}
procedure jzero_far (target : pointer;{far} bytestozero : size_t);

procedure FMEMZERO(target : pointer; size : size_t);

procedure FMEMCOPY(dest,src : pointer; size : size_t);

{== imjcomapi interface ==}

{ This file contains application interface routines that are used for both
  compression and decompression. }

{ Original: jcomapi.c;  Copyright (C) 1994-1997, Thomas G. Lane. }


{ Abort processing of a JPEG compression or decompression operation,
  but don't destroy the object itself. }

{GLOBAL}
procedure jpeg_abort (cinfo : j_common_ptr);


{ Destruction of a JPEG object. }

{GLOBAL}
procedure jpeg_destroy (cinfo : j_common_ptr);

{GLOBAL}
function jpeg_alloc_quant_table (cinfo : j_common_ptr) : JQUANT_TBL_PTR;

{GLOBAL}
function jpeg_alloc_huff_table (cinfo : j_common_ptr) : JHUFF_TBL_PTR;

{== imjerror interface ==}

{ This file contains simple error-reporting and trace-message routines.
  These are suitable for Unix-like systems and others where writing to
  stderr is the right thing to do.  Many applications will want to replace
  some or all of these routines.

  These routines are used by both the compression and decompression code. }

{ Source: jerror.c;  Copyright (C) 1991-1996, Thomas G. Lane. }
{ note: format_message still contains a hack }


{
  jversion;
}

const
  EXIT_FAILURE  = 1;   { define halt() codes if not provided }

{GLOBAL}
function jpeg_std_error (var err : jpeg_error_mgr) : jpeg_error_mgr_ptr;


procedure ERREXIT(cinfo : j_common_ptr; code : J_MESSAGE_CODE);

procedure ERREXIT1(cinfo : j_common_ptr; code : J_MESSAGE_CODE; p1 : uInt);

procedure ERREXIT2(cinfo : j_common_ptr; code : J_MESSAGE_CODE; p1 : int; p2 : int);

procedure ERREXIT3(cinfo : j_common_ptr; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int);

procedure ERREXIT4(cinfo : j_common_ptr; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int; p4 : int);

procedure ERREXITS(cinfo : j_common_ptr;code : J_MESSAGE_CODE;
                   str : AnsiString);
{ Nonfatal errors (we can keep going, but the data is probably corrupt) }

procedure WARNMS(cinfo : j_common_ptr; code : J_MESSAGE_CODE);

procedure WARNMS1(cinfo : j_common_ptr;code : J_MESSAGE_CODE; p1 : int);

procedure WARNMS2(cinfo : j_common_ptr; code : J_MESSAGE_CODE;
                  p1 : int; p2 : int);

{ Informational/debugging messages }
procedure TRACEMS(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE);

procedure TRACEMS1(cinfo : j_common_ptr; lvl : int;
                   code : J_MESSAGE_CODE; p1 : long);

procedure TRACEMS2(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                   p1 : int;
                   p2 : int);

procedure TRACEMS3(cinfo : j_common_ptr;
                   lvl : int;
                   code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int);

procedure TRACEMS4(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int; p4 : int);

procedure TRACEMS5(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int; p4 : int; p5 : int);

procedure TRACEMS8(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                  p1 : int; p2 : int; p3 : int; p4 : int;
                  p5 : int; p6 : int; p7 : int; p8 : int);

procedure TRACEMSS(cinfo : j_common_ptr; lvl : int;
                   code : J_MESSAGE_CODE; str : AnsiString);

{== imjmem interface ==}

{ Delphi3 -- > jmemnobs from jmemwin }
{ This file provides an Win32-compatible implementation of the system-
  dependent portion of the JPEG memory manager. }

{ Check jmemnobs.c }
{ Copyright (C) 1996, Jacques Nomssi Nzali }


{ The macro MAX_ALLOC_CHUNK designates the maximum number of bytes that may
  be requested in a single call to jpeg_get_large (and jpeg_get_small for that
  matter, but that case should never come into play).  This macro is needed
  to model the 64Kb-segment-size limit of far addressing on 80x86 machines.
  On those machines, we expect that jconfig.h will provide a proper value.
  On machines with 32-bit flat address spaces, any large constant may be used.

  NB: jmemmgr.c expects that MAX_ALLOC_CHUNK will be representable as type
  size_t and will be a multiple of sizeof(align_type). }

const
  MAX_ALLOC_CHUNK = long(1000000000);

{GLOBAL}
procedure jpeg_open_backing_store (cinfo : j_common_ptr;
                                   info : backing_store_ptr;
                                   total_bytes_needed : long);

{ These routines take care of any system-dependent initialization and
  cleanup required. }

{GLOBAL}
function jpeg_mem_init (cinfo : j_common_ptr) : long;

{GLOBAL}
procedure jpeg_mem_term (cinfo : j_common_ptr);

{ These two functions are used to allocate and release small chunks of
  memory.  (Typically the total amount requested through jpeg_get_small is
  no more than 20K or so; this will be requested in chunks of a few K each.)
  Behavior should be the same as for the standard library functions malloc
  and free; in particular, jpeg_get_small must return NIL on failure.
  On most systems, these ARE malloc and free.  jpeg_free_small is passed the
  size of the object being freed, just in case it's needed.
  On an 80x86 machine using small-data memory model, these manage near heap. }


{ Near-memory allocation and freeing are controlled by the regular library
  routines malloc() and free(). }

{GLOBAL}
function jpeg_get_small (cinfo : j_common_ptr;
                         sizeofobject : size_t) : pointer;

{GLOBAL}
{object is a reserved word in Borland Pascal }
procedure jpeg_free_small (cinfo : j_common_ptr;
                           an_object : pointer;
                           sizeofobject : size_t);

{ These two functions are used to allocate and release large chunks of
  memory (up to the total free space designated by jpeg_mem_available).
  The interface is the same as above, except that on an 80x86 machine,
  far pointers are used.  On most other machines these are identical to
  the jpeg_get/free_small routines; but we keep them separate anyway,
  in case a different allocation strategy is desirable for large chunks. }


{ "Large" objects are allocated in far memory, if possible }


{GLOBAL}
function jpeg_get_large (cinfo : j_common_ptr;
                         sizeofobject : size_t) : voidp; {far}

{GLOBAL}
procedure jpeg_free_large (cinfo : j_common_ptr;
                          {var?} an_object : voidp; {FAR}
                          sizeofobject : size_t);

{ This routine computes the total memory space available for allocation.
  It's impossible to do this in a portable way; our current solution is
  to make the user tell us (with a default value set at compile time).
  If you can actually get the available space, it's a good idea to subtract
  a slop factor of 5% or so. }

{GLOBAL}
function jpeg_mem_available (cinfo : j_common_ptr;
                             min_bytes_needed : long;
                             max_bytes_needed : long;
                             already_allocated : long) : long;

{== imjmemmgr interface ==}

{ This file contains the JPEG system-independent memory management
  routines.  This code is usable across a wide variety of machines; most
  of the system dependencies have been isolated in a separate file.
  The major functions provided here are:
    * pool-based allocation and freeing of memory;
    * policy decisions about how to divide available memory among the
      virtual arrays;
    * control logic for swapping virtual arrays between main memory and
      backing storage.
  The separate system-dependent file provides the actual backing-storage
  access code, and it contains the policy decision about how much total
  main memory to use.
  This file is system-dependent in the sense that some of its functions
  are unnecessary in some systems.  For example, if there is enough virtual
  memory so that backing storage will never be used, much of the virtual
  array control logic could be removed.  (Of course, if you have that much
  memory then you shouldn't care about a little bit of unused code...) }

{ Original : jmemmgr.c ; Copyright (C) 1991-1997, Thomas G. Lane. }


{ Memory manager initialization.
  When this is called, only the error manager pointer is valid in cinfo! }

{GLOBAL}
procedure jinit_memory_mgr (cinfo : j_common_ptr);

{== imjdct interface ==}

{ Orignal: jdct.h; Copyright (C) 1994-1996, Thomas G. Lane. }

{ This include file contains common declarations for the forward and
  inverse DCT modules.  These declarations are private to the DCT managers
  (jcdctmgr.c, jddctmgr.c) and the individual DCT algorithms.
  The individual DCT algorithms are kept in separate files to ease
  machine-dependent tuning (e.g., assembly coding). }


{ A forward DCT routine is given a pointer to a work area of type DCTELEM[];
  the DCT is to be performed in-place in that buffer.  Type DCTELEM is int
  for 8-bit samples, INT32 for 12-bit samples.  (NOTE: Floating-point DCT
  implementations use an array of type FAST_FLOAT, instead.)
  The DCT inputs are expected to be signed (range +-CENTERJSAMPLE).
  The DCT outputs are returned scaled up by a factor of 8; they therefore
  have a range of +-8K for 8-bit data, +-128K for 12-bit data.  This
  convention improves accuracy in integer implementations and saves some
  work in floating-point ones.
  Quantization of the output coefficients is done by jcdctmgr.c. }


{$ifdef BITS_IN_JSAMPLE_IS_8}
type
  DCTELEM = int;                { 16 or 32 bits is fine }
{$else}
type                            { must have 32 bits }
  DCTELEM = INT32;
{$endif}
type
  jTDctElem = 0..(MaxInt div SizeOf(DCTELEM))-1;
  DCTELEM_FIELD = array[jTDctElem] of DCTELEM;
  DCTELEM_FIELD_PTR = ^DCTELEM_FIELD;
  DCTELEMPTR = ^DCTELEM;

type
  forward_DCT_method_ptr = procedure(var data : array of DCTELEM);
  float_DCT_method_ptr = procedure(var data : array of FAST_FLOAT);


{ An inverse DCT routine is given a pointer to the input JBLOCK and a pointer
  to an output sample array.  The routine must dequantize the input data as
  well as perform the IDCT; for dequantization, it uses the multiplier table
  pointed to by compptr->dct_table.  The output data is to be placed into the
  sample array starting at a specified column.  (Any row offset needed will
  be applied to the array pointer before it is passed to the IDCT code.)
  Note that the number of samples emitted by the IDCT routine is
  DCT_scaled_size * DCT_scaled_size. }


{ typedef inverse_DCT_method_ptr is declared in jpegint.h }


{ Each IDCT routine has its own ideas about the best dct_table element type. }


type
  ISLOW_MULT_TYPE = MULTIPLIER;  { short or int, whichever is faster }

{$ifdef BITS_IN_JSAMPLE_IS_8}
type
  IFAST_MULT_TYPE = MULTIPLIER;  { 16 bits is OK, use short if faster }
const
  IFAST_SCALE_BITS = 2;         { fractional bits in scale factors }
{$else}
type
  IFAST_MULT_TYPE = INT32;      {  need 32 bits for scaled quantizers }
const
  IFAST_SCALE_BITS = 13;        { fractional bits in scale factors }
{$endif}
type
  FLOAT_MULT_TYPE = FAST_FLOAT; { preferred floating type }

const
  RANGE_MASK = (MAXJSAMPLE * 4 + 3); { 2 bits wider than legal samples }

type
  jTMultType = 0..(MaxInt div SizeOf(ISLOW_MULT_TYPE))-1;
  ISLOW_MULT_TYPE_FIELD = array[jTMultType] of ISLOW_MULT_TYPE;
  ISLOW_MULT_TYPE_FIELD_PTR = ^ISLOW_MULT_TYPE_FIELD;
  ISLOW_MULT_TYPE_PTR = ^ISLOW_MULT_TYPE;

  jTFloatType = 0..(MaxInt div SizeOf(FLOAT_MULT_TYPE))-1;
  FLOAT_MULT_TYPE_FIELD = array[jTFloatType] of FLOAT_MULT_TYPE;
  FLOAT_MULT_TYPE_FIELD_PTR = ^FLOAT_MULT_TYPE_FIELD;
  FLOAT_MULT_TYPE_PTR = ^FLOAT_MULT_TYPE;

  jTFastType = 0..(MaxInt div SizeOf(IFAST_MULT_TYPE))-1;
  IFAST_MULT_TYPE_FIELD = array[jTFastType] of IFAST_MULT_TYPE;
  IFAST_MULT_TYPE_FIELD_PTR = ^IFAST_MULT_TYPE_FIELD;
  IFAST_MULT_TYPE_PTR = ^IFAST_MULT_TYPE;

type
  jTFastFloat = 0..(MaxInt div SizeOf(FAST_FLOAT))-1;
  FAST_FLOAT_FIELD = array[jTFastFloat] of FAST_FLOAT;
  FAST_FLOAT_FIELD_PTR = ^FAST_FLOAT_FIELD;
  FAST_FLOAT_PTR = ^FAST_FLOAT;

{== imjfdctint interface ==}

{ This file contains a slow-but-accurate integer implementation of the
  forward DCT (Discrete Cosine Transform).

  A 2-D DCT can be done by 1-D DCT on each row followed by 1-D DCT
  on each column.  Direct algorithms are also available, but they are
  much more complex and seem not to be any faster when reduced to code.

  This implementation is based on an algorithm described in
    C. Loeffler, A. Ligtenberg and G. Moschytz, "Practical Fast 1-D DCT
    Algorithms with 11 Multiplications", Proc. Int'l. Conf. on Acoustics,
    Speech, and Signal Processing 1989 (ICASSP '89), pp. 988-991.
  The primary algorithm described there uses 11 multiplies and 29 adds.
  We use their alternate method with 12 multiplies and 32 adds.
  The advantage of this method is that no data path contains more than one
  multiplication; this allows a very simple and accurate implementation in
  scaled fixed-point arithmetic, with a minimal number of shifts. }

{ Original : jfdctint.c ; Copyright (C) 1991-1996, Thomas G. Lane. }


{ Perform the forward DCT on one block of samples. }

{GLOBAL}
procedure jpeg_fdct_islow (var data : array of DCTELEM);

{== imjfdctfst interface ==}

{ This file contains a fast, not so accurate integer implementation of the
  forward DCT (Discrete Cosine Transform).

  A 2-D DCT can be done by 1-D DCT on each row followed by 1-D DCT
  on each column.  Direct algorithms are also available, but they are
  much more complex and seem not to be any faster when reduced to code.

  This implementation is based on Arai, Agui, and Nakajima's algorithm for
  scaled DCT.  Their original paper (Trans. IEICE E-71(11):1095) is in
  Japanese, but the algorithm is described in the Pennebaker & Mitchell
  JPEG textbook (see REFERENCES section in file README).  The following code
  is based directly on figure 4-8 in P&M.
  While an 8-point DCT cannot be done in less than 11 multiplies, it is
  possible to arrange the computation so that many of the multiplies are
  simple scalings of the final outputs.  These multiplies can then be
  folded into the multiplications or divisions by the JPEG quantization
  table entries.  The AA&N method leaves only 5 multiplies and 29 adds
  to be done in the DCT itself.
  The primary disadvantage of this method is that with fixed-point math,
  accuracy is lost due to imprecise representation of the scaled
  quantization values.  The smaller the quantization table entry, the less
  precise the scaled value, so this implementation does worse with high-
  quality-setting files than with low-quality ones. }

{ Original: jfdctfst.c ; Copyright (C) 1994-1996, Thomas G. Lane. }


{ Perform the forward DCT on one block of samples. }

{GLOBAL}
procedure jpeg_fdct_ifast (var data : array of DCTELEM);

{== imjfdctflt interface ==}

{ This file contains a floating-point implementation of the
  forward DCT (Discrete Cosine Transform).

  This implementation should be more accurate than either of the integer
  DCT implementations.  However, it may not give the same results on all
  machines because of differences in roundoff behavior.  Speed will depend
  on the hardware's floating point capacity.

  A 2-D DCT can be done by 1-D DCT on each row followed by 1-D DCT
  on each column.  Direct algorithms are also available, but they are
  much more complex and seem not to be any faster when reduced to code.

  This implementation is based on Arai, Agui, and Nakajima's algorithm for
  scaled DCT.  Their original paper (Trans. IEICE E-71(11):1095) is in
  Japanese, but the algorithm is described in the Pennebaker & Mitchell
  JPEG textbook (see REFERENCES section in file README).  The following code
  is based directly on figure 4-8 in P&M.
  While an 8-point DCT cannot be done in less than 11 multiplies, it is
  possible to arrange the computation so that many of the multiplies are
  simple scalings of the final outputs.  These multiplies can then be
  folded into the multiplications or divisions by the JPEG quantization
  table entries.  The AA&N method leaves only 5 multiplies and 29 adds
  to be done in the DCT itself.
  The primary disadvantage of this method is that with a fixed-point
  implementation, accuracy is lost due to imprecise representation of the
  scaled quantization values.  However, that problem does not arise if
  we use floating point arithmetic. }

{ Original : jfdctflt.c ; Copyright (C) 1994-1996, Thomas G. Lane. }


{ Perform the forward DCT on one block of samples.}

{GLOBAL}
procedure jpeg_fdct_float (var data : array of FAST_FLOAT);

{== imjidctint interface ==}

{$IFNDEF JPEG_IDCT_ASM}

{ This file contains a slow-but-accurate integer implementation of the
  inverse DCT (Discrete Cosine Transform).  In the IJG code, this routine
  must also perform dequantization of the input coefficients.

  A 2-D IDCT can be done by 1-D IDCT on each column followed by 1-D IDCT
  on each row (or vice versa, but it's more convenient to emit a row at
  a time).  Direct algorithms are also available, but they are much more
  complex and seem not to be any faster when reduced to code.

  This implementation is based on an algorithm described in
    C. Loeffler, A. Ligtenberg and G. Moschytz, "Practical Fast 1-D DCT
    Algorithms with 11 Multiplications", Proc. Int'l. Conf. on Acoustics,
    Speech, and Signal Processing 1989 (ICASSP '89), pp. 988-991.
  The primary algorithm described there uses 11 multiplies and 29 adds.
  We use their alternate method with 12 multiplies and 32 adds.
  The advantage of this method is that no data path contains more than one
  multiplication; this allows a very simple and accurate implementation in
  scaled fixed-point arithmetic, with a minimal number of shifts. }

{ Original : jidctint.c ;  Copyright (C) 1991-1998, Thomas G. Lane. }


{ Perform dequantization and inverse DCT on one block of coefficients. }

{GLOBAL}
procedure jpeg_idct_islow (cinfo : j_decompress_ptr;
                           compptr : jpeg_component_info_ptr;
		           coef_block : JCOEFPTR;
		           output_buf : JSAMPARRAY;
                           output_col : JDIMENSION);

{$ENDIF JPEG_IDCT_ASM}

{== imjidctasm interface ==}

{$IFDEF JPEG_IDCT_ASM}

{ This file contains a slow-but-accurate integer implementation of the
  inverse DCT (Discrete Cosine Transform).  In the IJG code, this routine
  must also perform dequantization of the input coefficients.

  A 2-D IDCT can be done by 1-D IDCT on each column followed by 1-D IDCT
  on each row (or vice versa, but it's more convenient to emit a row at
  a time).  Direct algorithms are also available, but they are much more
  complex and seem not to be any faster when reduced to code.

  This implementation is based on an algorithm described in
    C. Loeffler, A. Ligtenberg and G. Moschytz, "Practical Fast 1-D DCT
    Algorithms with 11 Multiplications", Proc. Int'l. Conf. on Acoustics,
    Speech, and Signal Processing 1989 (ICASSP '89), pp. 988-991.
  The primary algorithm described there uses 11 multiplies and 29 adds.
  We use their alternate method with 12 multiplies and 32 adds.
  The advantage of this method is that no data path contains more than one
  multiplication; this allows a very simple and accurate implementation in
  scaled fixed-point arithmetic, with a minimal number of shifts. }

{ Original : jidctint.c ;  Copyright (C) 1991-1996, Thomas G. Lane. }
{ ;-------------------------------------------------------------------------
  ; JIDCTINT.ASM
  ; 80386 protected mode assembly translation of JIDCTINT.C
  ; **** Optimized to all hell by Jason M. Felice (jasonf@apk.net) ****
  ; **** E-mail welcome											 ****
  ;
  ; ** This code does not make O/S calls -- use it for OS/2, Win95, WinNT,
  ; ** DOS prot. mode., Linux, whatever... have fun.
  ;
  ; ** Note, this code is dependant on the structure member order in the .h
  ; ** files for the following structures:
  ;	-- amazingly NOT j_decompress_struct... cool.
  ;	-- jpeg_component_info (dependant on position of dct_table element)
  ;
  ; Originally created with the /Fa option of MSVC 4.0 (why work when you
  ; don't have to?)
  ;
  ; (this code, when compiled is 1K bytes smaller than the optimized MSVC
  ; release build, not to mention 120-130 ms faster in my profile test with 1
  ; small color and and 1 medium black-and-white jpeg: stats using TASM 4.0
  ; and MSVC 4.0 to create a non-console app; jpeg_idct_islow accumulated
  ; 5,760 hits on all trials)
  ;
  ; TASM -t -ml -os jidctint.asm, jidctint.obj
  ;-------------------------------------------------------------------------
   Converted to Delphi 2.0 BASM for PasJPEG
   by Jacques NOMSSI NZALI  <nomssi@physik.tu-chemnitz.de>
   October 13th 1996
    * assumes Delphi "register" calling convention
        first 3 parameter are in EAX,EDX,ECX
    * register allocation revised
}


{ Perform dequantization and inverse DCT on one block of coefficients. }

{GLOBAL}
procedure jpeg_idct_islow (cinfo : j_decompress_ptr;
                          compptr : jpeg_component_info_ptr;
		          coef_block : JCOEFPTR;
		          output_buf : JSAMPARRAY;
                          output_col : JDIMENSION);

{$ENDIF JPEG_IDCT_ASM}

{== imjidctfst interface ==}

{ This file contains a fast, not so accurate integer implementation of the
  inverse DCT (Discrete Cosine Transform).  In the IJG code, this routine
  must also perform dequantization of the input coefficients.

  A 2-D IDCT can be done by 1-D IDCT on each column followed by 1-D IDCT
  on each row (or vice versa, but it's more convenient to emit a row at
  a time).  Direct algorithms are also available, but they are much more
  complex and seem not to be any faster when reduced to code.

  This implementation is based on Arai, Agui, and Nakajima's algorithm for
  scaled DCT.  Their original paper (Trans. IEICE E-71(11):1095) is in
  Japanese, but the algorithm is described in the Pennebaker & Mitchell
  JPEG textbook (see REFERENCES section in file README).  The following code
  is based directly on figure 4-8 in P&M.
  While an 8-point DCT cannot be done in less than 11 multiplies, it is
  possible to arrange the computation so that many of the multiplies are
  simple scalings of the final outputs.  These multiplies can then be
  folded into the multiplications or divisions by the JPEG quantization
  table entries.  The AA&N method leaves only 5 multiplies and 29 adds
  to be done in the DCT itself.
  The primary disadvantage of this method is that with fixed-point math,
  accuracy is lost due to imprecise representation of the scaled
  quantization values.  The smaller the quantization table entry, the less
  precise the scaled value, so this implementation does worse with high-
  quality-setting files than with low-quality ones. }

{ Original : jidctfst.c ; Copyright (C) 1994-1996, Thomas G. Lane. }


{ Perform dequantization and inverse DCT on one block of coefficients. }

{GLOBAL}
procedure jpeg_idct_ifast (cinfo : j_decompress_ptr;
                           compptr : jpeg_component_info_ptr;
		           coef_block : JCOEFPTR;
		           output_buf : JSAMPARRAY;
                           output_col : JDIMENSION);

{== imjidctflt interface ==}

{ This file contains a floating-point implementation of the
  inverse DCT (Discrete Cosine Transform).  In the IJG code, this routine
  must also perform dequantization of the input coefficients.

  This implementation should be more accurate than either of the integer
  IDCT implementations.  However, it may not give the same results on all
  machines because of differences in roundoff behavior.  Speed will depend
  on the hardware's floating point capacity.

  A 2-D IDCT can be done by 1-D IDCT on each column followed by 1-D IDCT
  on each row (or vice versa, but it's more convenient to emit a row at
  a time).  Direct algorithms are also available, but they are much more
  complex and seem not to be any faster when reduced to code.

  This implementation is based on Arai, Agui, and Nakajima's algorithm for
  scaled DCT.  Their original paper (Trans. IEICE E-71(11):1095) is in
  Japanese, but the algorithm is described in the Pennebaker & Mitchell
  JPEG textbook (see REFERENCES section in file README).  The following code
  is based directly on figure 4-8 in P&M.
  While an 8-point DCT cannot be done in less than 11 multiplies, it is
  possible to arrange the computation so that many of the multiplies are
  simple scalings of the final outputs.  These multiplies can then be
  folded into the multiplications or divisions by the JPEG quantization
  table entries.  The AA&N method leaves only 5 multiplies and 29 adds
  to be done in the DCT itself.
  The primary disadvantage of this method is that with a fixed-point
  implementation, accuracy is lost due to imprecise representation of the
  scaled quantization values.  However, that problem does not arise if
  we use floating point arithmetic. }

{ Original: jidctflt.c ; Copyright (C) 1994-1996, Thomas G. Lane. }


{ Perform dequantization and inverse DCT on one block of coefficients. }

{GLOBAL}
procedure jpeg_idct_float (cinfo : j_decompress_ptr;
                           compptr : jpeg_component_info_ptr;
		           coef_block : JCOEFPTR;
		           output_buf : JSAMPARRAY;
                           output_col : JDIMENSION);

{== imjidctred interface ==}

{ This file contains inverse-DCT routines that produce reduced-size output:
  either 4x4, 2x2, or 1x1 pixels from an 8x8 DCT block.

  The implementation is based on the Loeffler, Ligtenberg and Moschytz (LL&M)
  algorithm used in jidctint.c.  We simply replace each 8-to-8 1-D IDCT step
  with an 8-to-4 step that produces the four averages of two adjacent outputs
  (or an 8-to-2 step producing two averages of four outputs, for 2x2 output).
  These steps were derived by computing the corresponding values at the end
  of the normal LL&M code, then simplifying as much as possible.

  1x1 is trivial: just take the DC coefficient divided by 8.

  See jidctint.c for additional comments. }


{ Original : jidctred.c ; Copyright (C) 1994-1998, Thomas G. Lane. }


{ Perform dequantization and inverse DCT on one block of coefficients,
  producing a reduced-size 1x1 output block. }

{GLOBAL}
procedure jpeg_idct_1x1 (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
	                 coef_block : JCOEFPTR;
	                 output_buf : JSAMPARRAY;
                         output_col : JDIMENSION);

{ Perform dequantization and inverse DCT on one block of coefficients,
  producing a reduced-size 2x2 output block. }

{GLOBAL}
procedure jpeg_idct_2x2 (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
	                 coef_block : JCOEFPTR;
                         output_buf : JSAMPARRAY;
                         output_col : JDIMENSION);

{ Perform dequantization and inverse DCT on one block of coefficients,
  producing a reduced-size 4x4 output block. }

{GLOBAL}
procedure jpeg_idct_4x4 (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
	                 coef_block : JCOEFPTR;
	                 output_buf : JSAMPARRAY;
                         output_col : JDIMENSION);

{== imjccolor interface ==}

{  This file contains input colorspace conversion routines. }

{ Original : jccolor.c ;  Copyright (C) 1991-1996, Thomas G. Lane. }


{ Module initialization routine for input colorspace conversion. }

{GLOBAL}
procedure jinit_color_converter (cinfo : j_compress_ptr);

{== imjcsample interface ==}

{ This file contains downsampling routines.

  Downsampling input data is counted in "row groups".  A row group
  is defined to be max_v_samp_factor pixel rows of each component,
  from which the downsampler produces v_samp_factor sample rows.
  A single row group is processed in each call to the downsampler module.

  The downsampler is responsible for edge-expansion of its output data
  to fill an integral number of DCT blocks horizontally.  The source buffer
  may be modified if it is helpful for this purpose (the source buffer is
  allocated wide enough to correspond to the desired output width).
  The caller (the prep controller) is responsible for vertical padding.

  The downsampler may request "context rows" by setting need_context_rows
  during startup.  In this case, the input arrays will contain at least
  one row group's worth of pixels above and below the passed-in data;
  the caller will create dummy rows at image top and bottom by replicating
  the first or last real pixel row.

  An excellent reference for image resampling is
    Digital Image Warping, George Wolberg, 1990.
    Pub. by IEEE Computer Society Press, Los Alamitos, CA. ISBN 0-8186-8944-7.

  The downsampling algorithm used here is a simple average of the source
  pixels covered by the output pixel.  The hi-falutin sampling literature
  refers to this as a "box filter".  In general the characteristics of a box
  filter are not very good, but for the specific cases we normally use (1:1
  and 2:1 ratios) the box is equivalent to a "triangle filter" which is not
  nearly so bad.  If you intend to use other sampling ratios, you'd be well
  advised to improve this code.

  A simple input-smoothing capability is provided.  This is mainly intended
  for cleaning up color-dithered GIF input files (if you find it inadequate,
  we suggest using an external filtering program such as pnmconvol).  When
  enabled, each input pixel P is replaced by a weighted sum of itself and its
  eight neighbors.  P's weight is 1-8*SF and each neighbor's weight is SF,
  where SF := (smoothing_factor / 1024).
  Currently, smoothing is only supported for 2h2v sampling factors. }

{ Original: jcsample.c ; Copyright (C) 1991-1996, Thomas G. Lane. }


{ Module initialization routine for downsampling.
  Note that we must select a routine for each component. }

{GLOBAL}
procedure jinit_downsampler (cinfo : j_compress_ptr);

{== imjcprepct interface ==}

{ Original : jcprepct.c ;  Copyright (C) 1994-1996, Thomas G. Lane. }

{ This file contains the compression preprocessing controller.
  This controller manages the color conversion, downsampling,
  and edge expansion steps.

  Most of the complexity here is associated with buffering input rows
  as required by the downsampler.  See the comments at the head of
  jcsample.c for the downsampler's needs. }


{GLOBAL}
procedure jinit_c_prep_controller (cinfo : j_compress_ptr;
                                   need_full_buffer : boolean);

{== imjcdctmgr interface ==}

{ Original : jcdctmgr.c ;  Copyright (C) 1994-1996, Thomas G. Lane. }

{ This file is part of the Independent JPEG Group's software.
  For conditions of distribution and use, see the accompanying README file.

  This file contains the forward-DCT management logic.
  This code selects a particular DCT implementation to be used,
  and it performs related housekeeping chores including coefficient
  quantization. }


{ Initialize FDCT manager. }

{GLOBAL}
procedure jinit_forward_dct (cinfo : j_compress_ptr);

{== imjchuff interface ==}

{ This file contains Huffman entropy encoding routines.

  Much of the complexity here has to do with supporting output suspension.
  If the data destination module demands suspension, we want to be able to
  back up to the start of the current MCU.  To do this, we copy state
  variables into local working storage, and update them back to the
  permanent JPEG objects only upon successful completion of an MCU. }

{ Original: jchuff.c; Copyright (C) 1991-1997, Thomas G. Lane. }


{ The legal range of a DCT coefficient is
   -1024 .. +1023  for 8-bit data;
  -16384 .. +16383 for 12-bit data.
  Hence the magnitude should always fit in 10 or 14 bits respectively. }


{$ifdef BITS_IN_JSAMPLE_IS_8}
const
  MAX_COEF_BITS = 10;
{$else}
const
  MAX_COEF_BITS = 14;
{$endif}

{ Derived data constructed for each Huffman table }
{ Declarations shared with jcphuff.c }
type
  c_derived_tbl_ptr = ^c_derived_tbl;
  c_derived_tbl = record
    ehufco : array[0..256-1] of uInt;	{ code for each symbol }
    ehufsi : array[0..256-1] of byte;   { length of code for each symbol }
    { If no code has been allocated for a symbol S, ehufsi[S] contains 0 }
  end;
{ for JCHUFF und JCPHUFF }
type
  TLongTable = array[0..256] of long;
  TLongTablePtr = ^TLongTable;

{ Compute the derived values for a Huffman table.
  Note this is also used by jcphuff.c. }

{GLOBAL}
procedure jpeg_make_c_derived_tbl (cinfo : j_compress_ptr;
                                   isDC : boolean;
                                   tblno : int;
			           var pdtbl : c_derived_tbl_ptr);

{ Generate the optimal coding for the given counts, fill htbl.
  Note this is also used by jcphuff.c. }

{GLOBAL}
procedure jpeg_gen_optimal_table (cinfo : j_compress_ptr;
                                  htbl : JHUFF_TBL_PTR;
                                  var freq : TLongTable);  { Nomssi }

{ Module initialization routine for Huffman entropy encoding. }

{GLOBAL}
procedure jinit_huff_encoder (cinfo : j_compress_ptr);

{== imjcphuff interface ==}

{ This file contains Huffman entropy encoding routines for progressive JPEG.

  We do not support output suspension in this module, since the library
  currently does not allow multiple-scan files to be written with output
  suspension. }

{ Original: jcphuff.c;  Copyright (C) 1995-1997, Thomas G. Lane. }


{ Module initialization routine for progressive Huffman entropy encoding. }

{GLOBAL}
procedure jinit_phuff_encoder (cinfo : j_compress_ptr);

{== imjccoefct interface ==}

{ This file contains the coefficient buffer controller for compression.
  This controller is the top level of the JPEG compressor proper.
  The coefficient buffer lies between forward-DCT and entropy encoding steps.}

{ Original: jccoefct.c; Copyright (C) 1994-1997, Thomas G. Lane. }


{ We use a full-image coefficient buffer when doing Huffman optimization,
  and also for writing multiple-scan JPEG files.  In all cases, the DCT
  step is run during the first pass, and subsequent passes need only read
  the buffered coefficients. }
{$ifdef ENTROPY_OPT_SUPPORTED}
  {$define FULL_COEF_BUFFER_SUPPORTED}
{$else}
  {$ifdef C_MULTISCAN_FILES_SUPPORTED}
    {$define FULL_COEF_BUFFER_SUPPORTED}
  {$endif}
{$endif}

{ Initialize coefficient buffer controller. }

{GLOBAL}
procedure jinit_c_coef_controller (cinfo : j_compress_ptr;
                                   need_full_buffer : boolean);

{== imjcmainct interface ==}

{ This file contains the main buffer controller for compression.
  The main buffer lies between the pre-processor and the JPEG
  compressor proper; it holds downsampled data in the JPEG colorspace. }

{ Original : jcmainct.c ; Copyright (C) 1994-1996, Thomas G. Lane. }


{ Note: currently, there is no operating mode in which a full-image buffer
  is needed at this step.  If there were, that mode could not be used with
  "raw data" input, since this module is bypassed in that case.  However,
  we've left the code here for possible use in special applications. }

{$undef FULL_MAIN_BUFFER_SUPPORTED}


{ Initialize main buffer controller. }

{GLOBAL}
procedure jinit_c_main_controller (cinfo : j_compress_ptr;
                                   need_full_buffer : boolean);

{== imjcmarker interface ==}

{ This file contains routines to write JPEG datastream markers. }

{ Original: jcmarker.c; Copyright (C) 1991-1998, Thomas G. Lane. }


const
                { JPEG marker codes }
  M_SOF0  = $c0;
  M_SOF1  = $c1;
  M_SOF2  = $c2;
  M_SOF3  = $c3;

  M_SOF5  = $c5;
  M_SOF6  = $c6;
  M_SOF7  = $c7;

  M_JPG   = $c8;
  M_SOF9  = $c9;
  M_SOF10 = $ca;
  M_SOF11 = $cb;

  M_SOF13 = $cd;
  M_SOF14 = $ce;
  M_SOF15 = $cf;

  M_DHT   = $c4;

  M_DAC   = $cc;

  M_RST0  = $d0;
  M_RST1  = $d1;
  M_RST2  = $d2;
  M_RST3  = $d3;
  M_RST4  = $d4;
  M_RST5  = $d5;
  M_RST6  = $d6;
  M_RST7  = $d7;

  M_SOI   = $d8;
  M_EOI   = $d9;
  M_SOS   = $da;
  M_DQT   = $db;
  M_DNL   = $dc;
  M_DRI   = $dd;
  M_DHP   = $de;
  M_EXP   = $df;

  M_APP0  = $e0;
  M_APP1  = $e1;
  M_APP2  = $e2;
  M_APP3  = $e3;
  M_APP4  = $e4;
  M_APP5  = $e5;
  M_APP6  = $e6;
  M_APP7  = $e7;
  M_APP8  = $e8;
  M_APP9  = $e9;
  M_APP10 = $ea;
  M_APP11 = $eb;
  M_APP12 = $ec;
  M_APP13 = $ed;
  M_APP14 = $ee;
  M_APP15 = $ef;

  M_JPG0  = $f0;
  M_JPG13 = $fd;
  M_COM   = $fe;

  M_TEM   = $01;

  M_ERROR = $100;

type
  jcmarker_JPEG_MARKER = Word;

{ Private state }

type
  jcmarker_my_marker_ptr = ^my_marker_writer;
  my_marker_writer = record
    pub : jpeg_marker_writer; { public fields }

    last_restart_interval : uint; { last DRI value emitted; 0 after SOI }
  end;


{GLOBAL}
procedure jinit_marker_writer (cinfo : j_compress_ptr);

{== imjcmaster interface ==}

{ This file contains master control logic for the JPEG compressor.
  These routines are concerned with parameter validation, initial setup,
  and inter-pass control (determining the number of passes and the work
  to be done in each pass). }

{ Original: jcmaster.c ; Copyright (C) 1991-1997, Thomas G. Lane. }


{ Initialize master compression control. }

{GLOBAL}
procedure jinit_c_master_control (cinfo : j_compress_ptr;
                                  transcode_only : boolean);

{== imjcinit interface ==}

{ Original: jcinit.c ;  Copyright (C) 1991-1997, Thomas G. Lane. }

{ This file contains initialization logic for the JPEG compressor.
  This routine is in charge of selecting the modules to be executed and
  making an initialization call to each one.

  Logically, this code belongs in jcmaster.c.  It's split out because
  linking this routine implies linking the entire compression library.
  For a transcoding-only application, we want to be able to use jcmaster.c
  without linking in the whole library. }


{ Master selection of compression modules.
  This is done once at the start of processing an image.  We determine
  which modules will be used and give them appropriate initialization calls. }

{GLOBAL}
procedure jinit_compress_master (cinfo : j_compress_ptr);

{== imjcapimin interface ==}

{  This file contains application interface code for the compression half
  of the JPEG library.  These are the "minimum" API routines that may be
  needed in either the normal full-compression case or the transcoding-only
  case.

  Most of the routines intended to be called directly by an application
  are in this file or in jcapistd.c.  But also see jcparam.c for
  parameter-setup helper routines, jcomapi.c for routines shared by
  compression and decompression, and jctrans.c for the transcoding case. }

{ jcapimin.c ;  Copyright (C) 1994-1998, Thomas G. Lane. }


{ Initialization of JPEG compression objects.
  Nomssi: This is a macro in the original code.

  jpeg_create_compress() and jpeg_create_decompress() are the exported
  names that applications should call.  These expand to calls on
  jpeg_CreateCompress and jpeg_CreateDecompress with additional information
  passed for version mismatch checking.
  NB: you must set up the error-manager BEFORE calling jpeg_create_xxx. }

procedure jpeg_create_compress(cinfo : j_compress_ptr);


{ Initialization of a JPEG compression object.
  The error manager must already be set up (in case memory manager fails). }

{GLOBAL}
procedure jpeg_CreateCompress (cinfo : j_compress_ptr;
                               version : int;
                               structsize : size_t);

{ Destruction of a JPEG compression object }

{GLOBAL}
procedure jpeg_destroy_compress (cinfo : j_compress_ptr);


{ Abort processing of a JPEG compression operation,
  but don't destroy the object itself. }

{GLOBAL}
procedure jpeg_abort_compress (cinfo : j_compress_ptr);


{ Forcibly suppress or un-suppress all quantization and Huffman tables.
  Marks all currently defined tables as already written (if suppress)
  or not written (if !suppress).  This will control whether they get emitted
  by a subsequent jpeg_start_compress call.

  This routine is exported for use by applications that want to produce
  abbreviated JPEG datastreams.  It logically belongs in jcparam.c, but
  since it is called by jpeg_start_compress, we put it here --- otherwise
  jcparam.o would be linked whether the application used it or not. }

{GLOBAL}
procedure jpeg_suppress_tables (cinfo : j_compress_ptr;
                                suppress : boolean);


{ Finish JPEG compression.

  If a multipass operating mode was selected, this may do a great deal of
  work including most of the actual output. }

{GLOBAL}
procedure jpeg_finish_compress (cinfo : j_compress_ptr);

{ Write a special marker.
  This is only recommended for writing COM or APPn markers.
  Must be called after jpeg_start_compress() and before
  first call to jpeg_write_scanlines() or jpeg_write_raw_data(). }

{GLOBAL}
procedure jpeg_write_marker (cinfo : j_compress_ptr;
                             marker : int;
		             dataptr : JOCTETptr;
                             datalen : uInt);

{GLOBAL}
procedure jpeg_write_m_header (cinfo : j_compress_ptr;
                               marker : int;
                               datalen : uint);
{GLOBAL}
procedure jpeg_write_m_byte (cinfo : j_compress_ptr; val : int);

{ Alternate compression function: just write an abbreviated table file.
  Before calling this, all parameters and a data destination must be set up.

  To produce a pair of files containing abbreviated tables and abbreviated
  image data, one would proceed as follows:

 		initialize JPEG object
 		set JPEG parameters
 		set destination to table file
 		jpeg_write_tables(cinfo);
 		set destination to image file
 		jpeg_start_compress(cinfo, FALSE);
 		write data...
 		jpeg_finish_compress(cinfo);

  jpeg_write_tables has the side effect of marking all tables written
  (same as jpeg_suppress_tables(..., TRUE)).  Thus a subsequent start_compress
  will not re-emit the tables unless it is passed write_all_tables=TRUE. }


{GLOBAL}
procedure jpeg_write_tables (cinfo : j_compress_ptr);

{== imjcapistd interface ==}

{ Original : jcapistd.c ; Copyright (C) 1994-1996, Thomas G. Lane. }

{ This file is part of the Independent JPEG Group's software.
  For conditions of distribution and use, see the accompanying README file.

  This file contains application interface code for the compression half
  of the JPEG library.  These are the "standard" API routines that are
  used in the normal full-compression case.  They are not used by a
  transcoding-only application.  Note that if an application links in
  jpeg_start_compress, it will end up linking in the entire compressor.
  We thus must separate this file from jcapimin.c to avoid linking the
  whole compression library into a transcoder. }


{ Compression initialization.
  Before calling this, all parameters and a data destination must be set up.

  We require a write_all_tables parameter as a failsafe check when writing
  multiple datastreams from the same compression object.  Since prior runs
  will have left all the tables marked sent_table=TRUE, a subsequent run
  would emit an abbreviated stream (no tables) by default.  This may be what
  is wanted, but for safety's sake it should not be the default behavior:
  programmers should have to make a deliberate choice to emit abbreviated
  images.  Therefore the documentation and examples should encourage people
  to pass write_all_tables=TRUE; then it will take active thought to do the
  wrong thing. }

{GLOBAL}
procedure jpeg_start_compress (cinfo : j_compress_ptr;
                               write_all_tables : boolean);


{ Write some scanlines of data to the JPEG compressor.

  The return value will be the number of lines actually written.
  This should be less than the supplied num_lines only in case that
  the data destination module has requested suspension of the compressor,
  or if more than image_height scanlines are passed in.

  Note: we warn about excess calls to jpeg_write_scanlines() since
  this likely signals an application programmer error.  However,
  excess scanlines passed in the last valid call are *silently* ignored,
  so that the application need not adjust num_lines for end-of-image
  when using a multiple-scanline buffer. }

{GLOBAL}
function jpeg_write_scanlines (cinfo : j_compress_ptr;
                              scanlines : JSAMPARRAY;
		              num_lines : JDIMENSION) : JDIMENSION;

{ Alternate entry point to write raw data.
  Processes exactly one iMCU row per call, unless suspended. }

{GLOBAL}
function jpeg_write_raw_data (cinfo : j_compress_ptr;
                              data : JSAMPIMAGE;
		              num_lines : JDIMENSION) : JDIMENSION;

{== imjcparam interface ==}

{ This file contains optional default-setting code for the JPEG compressor.
  Applications do not have to use this file, but those that don't use it
  must know a lot more about the innards of the JPEG code. }

{ Original: jcparam.c ; Copyright (C) 1991-1998, Thomas G. Lane. }


{ Quantization table setup routines }

{GLOBAL}
procedure jpeg_add_quant_table (cinfo : j_compress_ptr;
                                which_tbl : int;
		                const basic_table : array of uInt;
		                scale_factor : int;
                                force_baseline : boolean);

{GLOBAL}
procedure jpeg_set_linear_quality (cinfo : j_compress_ptr;
                                   scale_factor : int;
			           force_baseline : boolean);
{ Set or change the 'quality' (quantization) setting, using default tables
  and a straight percentage-scaling quality scale.  In most cases it's better
  to use jpeg_set_quality (below); this entry point is provided for
  applications that insist on a linear percentage scaling. }

{GLOBAL}
function jpeg_quality_scaling (quality : int) : int;
{ Convert a user-specified quality rating to a percentage scaling factor
  for an underlying quantization table, using our recommended scaling curve.
  The input 'quality' factor should be 0 (terrible) to 100 (very good). }

{GLOBAL}
procedure jpeg_set_quality (cinfo : j_compress_ptr;
                            quality : int;
                            force_baseline : boolean);
{ Set or change the 'quality' (quantization) setting, using default tables.
  This is the standard quality-adjusting entry point for typical user
  interfaces; only those who want detailed control over quantization tables
  would use the preceding three routines directly. }

{GLOBAL}
procedure jpeg_set_defaults (cinfo : j_compress_ptr);

{ Create a recommended progressive-JPEG script.
  cinfo^.num_components and cinfo^.jpeg_color_space must be correct. }

{ Set the JPEG colorspace, and choose colorspace-dependent default values. }

{GLOBAL}
procedure jpeg_set_colorspace (cinfo : j_compress_ptr;
                               colorspace : J_COLOR_SPACE);

{ Select an appropriate JPEG colorspace for in_color_space. }

{GLOBAL}
procedure jpeg_default_colorspace (cinfo : j_compress_ptr);

{GLOBAL}
procedure jpeg_simple_progression (cinfo : j_compress_ptr);

{== imjdmarker interface ==}

{ This file contains routines to decode JPEG datastream markers.
  Most of the complexity arises from our desire to support input
  suspension: if not all of the data for a marker is available;
  we must exit back to the application. On resumption; we reprocess
  the marker. }

{ Original: jdmarker.c;  Copyright (C) 1991-1998; Thomas G. Lane. }
{ History
   9.7.96                   Conversion to pascal started      jnn
   22.3.98                  updated to 6b                     jnn }


type
  jdmarker_JPEG_MARKER = uint;        { JPEG marker codes }

{ Private state }

type
  jdmarker_my_marker_ptr = ^my_marker_reader;
  my_marker_reader = record
    pub : jpeg_marker_reader; { public fields }

    { Application-overridable marker processing methods }
    process_COM : jpeg_marker_parser_method;
    process_APPn : array[0..16-1] of jpeg_marker_parser_method;

    { Limit on marker data length to save for each marker type }
    length_limit_COM : uint;
    length_limit_APPn : array[0..16-1] of uint;

    { Status of COM/APPn marker saving }
    cur_marker : jpeg_saved_marker_ptr;	{ NIL if not processing a marker }
    bytes_read : uint;		{ data bytes read so far in marker }
    { Note: cur_marker is not linked into marker_list until it's all read. }
  end;

{GLOBAL}
function jpeg_resync_to_restart(cinfo : j_decompress_ptr;
                                desired : int) : boolean;
{GLOBAL}
procedure jinit_marker_reader (cinfo : j_decompress_ptr);

{$ifdef SAVE_MARKERS_SUPPORTED}

{GLOBAL}
procedure jpeg_save_markers (cinfo : j_decompress_ptr;
                             marker_code : int;
		             length_limit : uint);
{$ENDIF}

{GLOBAL}
procedure jpeg_set_marker_processor (cinfo : j_decompress_ptr;
                                     marker_code : int;
	                             routine : jpeg_marker_parser_method);

{== imjdinput interface ==}

{ Original: jdinput.c ; Copyright (C) 1991-1997, Thomas G. Lane. }

{ This file is part of the Independent JPEG Group's software.
  For conditions of distribution and use, see the accompanying README file.

  This file contains input control logic for the JPEG decompressor.
  These routines are concerned with controlling the decompressor's input
  processing (marker reading and coefficient decoding).  The actual input
  reading is done in jdmarker.c, jdhuff.c, and jdphuff.c. }


{ Initialize the input controller module.
  This is called only once, when the decompression object is created. }

{GLOBAL}
procedure jinit_input_controller (cinfo : j_decompress_ptr);

{== imjdhuff interface ==}

{ This file contains declarations for Huffman entropy decoding routines
  that are shared between the sequential decoder (jdhuff.c) and the
  progressive decoder (jdphuff.c).  No other modules need to see these. }

{ This file contains Huffman entropy decoding routines.

  Much of the complexity here has to do with supporting input suspension.
  If the data source module demands suspension, we want to be able to back
  up to the start of the current MCU.  To do this, we copy state variables
  into local working storage, and update them back to the permanent
  storage only upon successful completion of an MCU. }

{ Original: jdhuff.h+jdhuff.c;  Copyright (C) 1991-1997, Thomas G. Lane. }


{ Declarations shared with jdphuff.c }


{ Derived data constructed for each Huffman table }

const
  HUFF_LOOKAHEAD  = 8;          { # of bits of lookahead }

type
  d_derived_tbl_ptr = ^d_derived_tbl;
  d_derived_tbl = record
    { Basic tables: (element [0] of each array is unused) }
    maxcode : array[0..18-1] of INT32;       { largest code of length k (-1 if none) }
    { (maxcode[17] is a sentinel to ensure jpeg_huff_decode terminates) }
    valoffset : array[0..17-1] of INT32;     { huffval[] offset for codes of length k }
    { valoffset[k] = huffval[] index of 1st symbol of code length k, less
      the smallest code of length k; so given a code of length k, the
      corresponding symbol is huffval[code + valoffset[k]] }

    { Link to public Huffman table (needed only in jpeg_huff_decode) }
    pub : JHUFF_TBL_PTR;

    { Lookahead tables: indexed by the next HUFF_LOOKAHEAD bits of
      the input data stream.  If the next Huffman code is no more
      than HUFF_LOOKAHEAD bits long, we can obtain its length and
      the corresponding symbol directly from these tables. }

    look_nbits : array[0..(1 shl HUFF_LOOKAHEAD)-1] of int;
                                { # bits, or 0 if too long }
    look_sym : array[0..(1 shl HUFF_LOOKAHEAD)-1] of UINT8;
                                { symbol, or unused }
  end;

{ Fetching the next N bits from the input stream is a time-critical operation
  for the Huffman decoders.  We implement it with a combination of inline
  macros and out-of-line subroutines.  Note that N (the number of bits
  demanded at one time) never exceeds 15 for JPEG use.

  We read source bytes into get_buffer and dole out bits as needed.
  If get_buffer already contains enough bits, they are fetched in-line
  by the macros CHECK_BIT_BUFFER and GET_BITS.  When there aren't enough
  bits, jpeg_fill_bit_buffer is called; it will attempt to fill get_buffer
  as full as possible (not just to the number of bits needed; this
  prefetching reduces the overhead cost of calling jpeg_fill_bit_buffer).
  Note that jpeg_fill_bit_buffer may return FALSE to indicate suspension.
  On TRUE return, jpeg_fill_bit_buffer guarantees that get_buffer contains
  at least the requested number of bits --- dummy zeroes are inserted if
  necessary. }


type
  bit_buf_type = INT32 ;        { type of bit-extraction buffer }
const
  BIT_BUF_SIZE = 32;            { size of buffer in bits }

{ If long is > 32 bits on your machine, and shifting/masking longs is
  reasonably fast, making bit_buf_type be long and setting BIT_BUF_SIZE
  appropriately should be a win.  Unfortunately we can't define the size
  with something like  #define BIT_BUF_SIZE (sizeof(bit_buf_type)*8)
  because not all machines measure sizeof in 8-bit bytes. }

type
  bitread_perm_state = record   { Bitreading state saved across MCUs }
    get_buffer : bit_buf_type;  { current bit-extraction buffer }
    bits_left : int;            { # of unused bits in it }
  end;

type
  bitread_working_state = record
    { Bitreading working state within an MCU }
    { current data source location }
    { We need a copy, rather than munging the original, in case of suspension }
    next_input_byte : JOCTETptr;  { => next byte to read from source }
    bytes_in_buffer : size_t;     { # of bytes remaining in source buffer }
    { Bit input buffer --- note these values are kept in register variables,
      not in this struct, inside the inner loops. }

    get_buffer : bit_buf_type;  { current bit-extraction buffer }
    bits_left : int;            { # of unused bits in it }
    { Pointer needed by jpeg_fill_bit_buffer }
    cinfo : j_decompress_ptr;   { back link to decompress master record }
  end;

{ Module initialization routine for Huffman entropy decoding. }

{GLOBAL}
procedure jinit_huff_decoder (cinfo : j_decompress_ptr);

{GLOBAL}
function jpeg_huff_decode(var state : bitread_working_state;
                          get_buffer : bit_buf_type; {register}
                          bits_left : int; {register}
                          htbl : d_derived_tbl_ptr;
                          min_bits : int) : int;

{ Compute the derived values for a Huffman table.
  Note this is also used by jdphuff.c. }

{GLOBAL}
procedure jpeg_make_d_derived_tbl (cinfo : j_decompress_ptr;
                                   isDC : boolean;
                                   tblno : int;
			           var pdtbl : d_derived_tbl_ptr);

{ Load up the bit buffer to a depth of at least nbits }

function jpeg_fill_bit_buffer	(var state : bitread_working_state;
                                 get_buffer : bit_buf_type;  {register}
	                         bits_left : int; {register}
                                 nbits : int) : boolean;

{== imjdphuff interface ==}

{ This file contains Huffman entropy decoding routines for progressive JPEG.

  Much of the complexity here has to do with supporting input suspension.
  If the data source module demands suspension, we want to be able to back
  up to the start of the current MCU.  To do this, we copy state variables
  into local working storage, and update them back to the permanent
  storage only upon successful completion of an MCU. }

{ Original: jdphuff.c ; Copyright (C) 1995-1997, Thomas G. Lane. }


{GLOBAL}
procedure jinit_phuff_decoder (cinfo : j_decompress_ptr);

{== imjdcoefct interface ==}

{ This file contains the coefficient buffer controller for decompression.
  This controller is the top level of the JPEG decompressor proper.
  The coefficient buffer lies between entropy decoding and inverse-DCT steps.

  In buffered-image mode, this controller is the interface between
  input-oriented processing and output-oriented processing.
  Also, the input side (only) is used when reading a file for transcoding. }

{ Original: jdcoefct.c ; Copyright (C) 1994-1997, Thomas G. Lane. }


{GLOBAL}
procedure jinit_d_coef_controller (cinfo : j_decompress_ptr;
                                   need_full_buffer : boolean);

{== imjdcolor interface ==}

{ This file contains output colorspace conversion routines. }

{ Original: jdcolor.c ; Copyright (C) 1991-1997, Thomas G. Lane. }


{ Module initialization routine for output colorspace conversion. }

{GLOBAL}
procedure jinit_color_deconverter (cinfo : j_decompress_ptr);

{== imjdsample interface ==}

{ Original: jdsample.c; Copyright (C) 1991-1996, Thomas G. Lane. }

{ This file contains upsampling routines.

  Upsampling input data is counted in "row groups".  A row group
  is defined to be (v_samp_factor * DCT_scaled_size / min_DCT_scaled_size)
  sample rows of each component.  Upsampling will normally produce
  max_v_samp_factor pixel rows from each row group (but this could vary
  if the upsampler is applying a scale factor of its own).

  An excellent reference for image resampling is
    Digital Image Warping, George Wolberg, 1990.
    Pub. by IEEE Computer Society Press, Los Alamitos, CA. ISBN 0-8186-8944-7.}


{ Pointer to routine to upsample a single component }
type
  upsample1_ptr = procedure (cinfo : j_decompress_ptr;
                             compptr : jpeg_component_info_ptr;
                             input_data : JSAMPARRAY;
                             var output_data_ptr : JSAMPARRAY);

{ Module initialization routine for upsampling. }

{GLOBAL}
procedure jinit_upsampler (cinfo : j_decompress_ptr);

{== imjdmerge interface ==}

{  This file contains code for merged upsampling/color conversion.

  This file combines functions from jdsample.c and jdcolor.c;
  read those files first to understand what's going on.

  When the chroma components are to be upsampled by simple replication
  (ie, box filtering), we can save some work in color conversion by
  calculating all the output pixels corresponding to a pair of chroma
  samples at one time.  In the conversion equations
 	R := Y           + K1 * Cr
 	G := Y + K2 * Cb + K3 * Cr
 	B := Y + K4 * Cb
  only the Y term varies among the group of pixels corresponding to a pair
  of chroma samples, so the rest of the terms can be calculated just once.
  At typical sampling ratios, this eliminates half or three-quarters of the
  multiplications needed for color conversion.

  This file currently provides implementations for the following cases:
 	YCbCr => RGB color conversion only.
 	Sampling ratios of 2h1v or 2h2v.
 	No scaling needed at upsample time.
 	Corner-aligned (non-CCIR601) sampling alignment.
  Other special cases could be added, but in most applications these are
  the only common cases.  (For uncommon cases we fall back on the more
  general code in jdsample.c and jdcolor.c.) }

{ Original: jdmerge.c ;  Copyright (C) 1994-1996, Thomas G. Lane. }


{ Module initialization routine for merged upsampling/color conversion.

  NB: this is called under the conditions determined by use_merged_upsample()
  in jdmaster.c.  That routine MUST correspond to the actual capabilities
  of this module; no safety checks are made here. }

{GLOBAL}
procedure jinit_merged_upsampler (cinfo : j_decompress_ptr);

{== imjdpostct interface ==}

{ Original: jdpostct.c ; Copyright (C) 1994-1996, Thomas G. Lane. }

{ This file contains the decompression postprocessing controller.
  This controller manages the upsampling, color conversion, and color
  quantization/reduction steps; specifically, it controls the buffering
  between upsample/color conversion and color quantization/reduction.

  If no color quantization/reduction is required, then this module has no
  work to do, and it just hands off to the upsample/color conversion code.
  An integrated upsample/convert/quantize process would replace this module
  entirely. }


{ Initialize postprocessing controller. }

{GLOBAL}
procedure jinit_d_post_controller (cinfo : j_decompress_ptr;
                                   need_full_buffer : boolean);

{== imjddctmgr interface ==}

{ Original : jddctmgr.c ;  Copyright (C) 1994-1996, Thomas G. Lane. }

{ This file contains the inverse-DCT management logic.
  This code selects a particular IDCT implementation to be used,
  and it performs related housekeeping chores.  No code in this file
  is executed per IDCT step, only during output pass setup.

  Note that the IDCT routines are responsible for performing coefficient
  dequantization as well as the IDCT proper.  This module sets up the
  dequantization multiplier table needed by the IDCT routine. }


{ Initialize IDCT manager. }

{GLOBAL}
procedure jinit_inverse_dct (cinfo : j_decompress_ptr);

{== imjquant1 interface ==}

{ This file contains 1-pass color quantization (color mapping) routines.
  These routines provide mapping to a fixed color map using equally spaced
  color values.  Optional Floyd-Steinberg or ordered dithering is available. }

{ Original: jquant1.c; Copyright (C) 1991-1996, Thomas G. Lane. }


{GLOBAL}
procedure jinit_1pass_quantizer (cinfo : j_decompress_ptr);

{== imjquant2 interface ==}

{ This file contains 2-pass color quantization (color mapping) routines.
  These routines provide selection of a custom color map for an image,
  followed by mapping of the image to that color map, with optional
  Floyd-Steinberg dithering.
  It is also possible to use just the second pass to map to an arbitrary
  externally-given color map.

  Note: ordered dithering is not supported, since there isn't any fast
  way to compute intercolor distances; it's unclear that ordered dither's
  fundamental assumptions even hold with an irregularly spaced color map. }

{ Original: jquant2.c; Copyright (C) 1991-1996, Thomas G. Lane. }


{ Module initialization routine for 2-pass color quantization. }


{GLOBAL}
procedure jinit_2pass_quantizer (cinfo : j_decompress_ptr);

{== imjdmainct interface ==}

{ This file is part of the Independent JPEG Group's software.
  For conditions of distribution and use, see the accompanying README file.

  This file contains the main buffer controller for decompression.
  The main buffer lies between the JPEG decompressor proper and the
  post-processor; it holds downsampled data in the JPEG colorspace.

  Note that this code is bypassed in raw-data mode, since the application
  supplies the equivalent of the main buffer in that case. }

{ Original: jdmainct.c ; Copyright (C) 1994-1996, Thomas G. Lane.  }


{ In the current system design, the main buffer need never be a full-image
  buffer; any full-height buffers will be found inside the coefficient or
  postprocessing controllers.  Nonetheless, the main controller is not
  trivial.  Its responsibility is to provide context rows for upsampling/
  rescaling, and doing this in an efficient fashion is a bit tricky.

  Postprocessor input data is counted in "row groups".  A row group
  is defined to be (v_samp_factor * DCT_scaled_size / min_DCT_scaled_size)
  sample rows of each component.  (We require DCT_scaled_size values to be
  chosen such that these numbers are integers.  In practice DCT_scaled_size
  values will likely be powers of two, so we actually have the stronger
  condition that DCT_scaled_size / min_DCT_scaled_size is an integer.)
  Upsampling will typically produce max_v_samp_factor pixel rows from each
  row group (times any additional scale factor that the upsampler is
  applying).

  The coefficient controller will deliver data to us one iMCU row at a time;
  each iMCU row contains v_samp_factor * DCT_scaled_size sample rows, or
  exactly min_DCT_scaled_size row groups.  (This amount of data corresponds
  to one row of MCUs when the image is fully interleaved.)  Note that the
  number of sample rows varies across components, but the number of row
  groups does not.  Some garbage sample rows may be included in the last iMCU
  row at the bottom of the image.

  Depending on the vertical scaling algorithm used, the upsampler may need
  access to the sample row(s) above and below its current input row group.
  The upsampler is required to set need_context_rows TRUE at global
  selection
  time if so.  When need_context_rows is FALSE, this controller can simply
  obtain one iMCU row at a time from the coefficient controller and dole it
  out as row groups to the postprocessor.

  When need_context_rows is TRUE, this controller guarantees that the buffer
  passed to postprocessing contains at least one row group's worth of samples
  above and below the row group(s) being processed.  Note that the context
  rows "above" the first passed row group appear at negative row offsets in
  the passed buffer.  At the top and bottom of the image, the required
  context rows are manufactured by duplicating the first or last real sample
  row; this avoids having special cases in the upsampling inner loops.

  The amount of context is fixed at one row group just because that's a
  convenient number for this controller to work with.  The existing
  upsamplers really only need one sample row of context.  An upsampler
  supporting arbitrary output rescaling might wish for more than one row
  group of context when shrinking the image; tough, we don't handle that.
  (This is justified by the assumption that downsizing will be handled mostly
  by adjusting the DCT_scaled_size values, so that the actual scale factor at
  the upsample step needn't be much less than one.)

  To provide the desired context, we have to retain the last two row groups
  of one iMCU row while reading in the next iMCU row.  (The last row group
  can't be processed until we have another row group for its below-context,
  and so we have to save the next-to-last group too for its above-context.)
  We could do this most simply by copying data around in our buffer, but
  that'd be very slow.  We can avoid copying any data by creating a rather
  strange pointer structure.  Here's how it works.  We allocate a workspace
  consisting of M+2 row groups (where M = min_DCT_scaled_size is the number
  of row groups per iMCU row).  We create two sets of redundant pointers to
  the workspace.  Labeling the physical row groups 0 to M+1, the synthesized
  pointer lists look like this:
                    M+1                          M-1
  master pointer --> 0         master pointer --> 0
                     1                            1
                    ...                          ...
                    M-3                          M-3
                    M-2                           M
                    M-1                          M+1
                     M                           M-2
                    M+1                          M-1
                     0                            0
  We read alternate iMCU rows using each master pointer; thus the last two
  row groups of the previous iMCU row remain un-overwritten in the workspace.
  The pointer lists are set up so that the required context rows appear to
  be adjacent to the proper places when we pass the pointer lists to the
  upsampler.

  The above pictures describe the normal state of the pointer lists.
  At top and bottom of the image, we diddle the pointer lists to duplicate
  the first or last sample row as necessary (this is cheaper than copying
  sample rows around).

  This scheme breaks down if M < 2, ie, min_DCT_scaled_size is 1.  In that
  situation each iMCU row provides only one row group so the buffering logic
  must be different (eg, we must read two iMCU rows before we can emit the
  first row group).  For now, we simply do not support providing context
  rows when min_DCT_scaled_size is 1.  That combination seems unlikely to
  be worth providing --- if someone wants a 1/8th-size preview, they probably
  want it quick and dirty, so a context-free upsampler is sufficient. }


{GLOBAL}
procedure jinit_d_main_controller (cinfo : j_decompress_ptr;
                                   need_full_buffer : boolean);

{== imjdmaster interface ==}

{ This file contains master control logic for the JPEG decompressor.
  These routines are concerned with selecting the modules to be executed
  and with determining the number of passes and the work to be done in each
  pass. }

{ Original: jdmaster.c ; Copyright (C) 1991-1998, Thomas G. Lane.  }


{ Compute output image dimensions and related values.
  NOTE: this is exported for possible use by application.
  Hence it mustn't do anything that can't be done twice.
  Also note that it may be called before the master module is initialized! }

{GLOBAL}
procedure jpeg_calc_output_dimensions (cinfo : j_decompress_ptr);
{ Do computations that are needed before master selection phase }


{$ifdef D_MULTISCAN_FILES_SUPPORTED}

{GLOBAL}
procedure jpeg_new_colormap (cinfo : j_decompress_ptr);

{$endif}

{ Initialize master decompression control and select active modules.
  This is performed at the start of jpeg_start_decompress. }

{GLOBAL}
procedure jinit_master_decompress (cinfo : j_decompress_ptr);

{== imjdapimin interface ==}

{ This file contains application interface code for the decompression half
  of the JPEG library.  These are the "minimum" API routines that may be
  needed in either the normal full-decompression case or the
  transcoding-only case.

  Most of the routines intended to be called directly by an application
  are in this file or in jdapistd.c.  But also see jcomapi.c for routines
  shared by compression and decompression, and jdtrans.c for the transcoding
  case. }

{ Original : jdapimin.c ;  Copyright (C) 1994-1998, Thomas G. Lane. }


{ Nomssi }
procedure jpeg_create_decompress(cinfo : j_decompress_ptr);

{ Initialization of a JPEG decompression object.
  The error manager must already be set up (in case memory manager fails). }

{GLOBAL}
procedure jpeg_CreateDecompress (cinfo : j_decompress_ptr;
                                 version : int;
                                 structsize : size_t);

{ Destruction of a JPEG decompression object }

{GLOBAL}
procedure jpeg_destroy_decompress (cinfo : j_decompress_ptr);


{ Decompression startup: read start of JPEG datastream to see what's there.
  Need only initialize JPEG object and supply a data source before calling.

  This routine will read as far as the first SOS marker (ie, actual start of
  compressed data), and will save all tables and parameters in the JPEG
  object.  It will also initialize the decompression parameters to default
  values, and finally return JPEG_HEADER_OK.  On return, the application may
  adjust the decompression parameters and then call jpeg_start_decompress.
  (Or, if the application only wanted to determine the image parameters,
  the data need not be decompressed.  In that case, call jpeg_abort or
  jpeg_destroy to release any temporary space.)
  If an abbreviated (tables only) datastream is presented, the routine will
  return JPEG_HEADER_TABLES_ONLY upon reaching EOI.  The application may then
  re-use the JPEG object to read the abbreviated image datastream(s).
  It is unnecessary (but OK) to call jpeg_abort in this case.
  The JPEG_SUSPENDED return code only occurs if the data source module
  requests suspension of the decompressor.  In this case the application
  should load more source data and then re-call jpeg_read_header to resume
  processing.
  If a non-suspending data source is used and require_image is TRUE, then the
  return code need not be inspected since only JPEG_HEADER_OK is possible.

  This routine is now just a front end to jpeg_consume_input, with some
  extra error checking. }

{GLOBAL}
function jpeg_read_header (cinfo : j_decompress_ptr;
                           require_image : boolean) : int;

{ Consume data in advance of what the decompressor requires.
  This can be called at any time once the decompressor object has
  been created and a data source has been set up.

  This routine is essentially a state machine that handles a couple
  of critical state-transition actions, namely initial setup and
  transition from header scanning to ready-for-start_decompress.
  All the actual input is done via the input controller's consume_input
  method. }

{GLOBAL}
function jpeg_consume_input (cinfo : j_decompress_ptr) : int;

{ Have we finished reading the input file? }

{GLOBAL}
function jpeg_input_complete (cinfo : j_decompress_ptr) : boolean;

{ Is there more than one scan? }

{GLOBAL}
function jpeg_has_multiple_scans (cinfo : j_decompress_ptr) : boolean;


{ Finish JPEG decompression.

  This will normally just verify the file trailer and release temp storage.

  Returns FALSE if suspended.  The return value need be inspected only if
  a suspending data source is used. }

{GLOBAL}
function jpeg_finish_decompress (cinfo : j_decompress_ptr) : boolean;

{== imjdapistd interface ==}

{ Original : jdapistd.c ;  Copyright (C) 1994-1996, Thomas G. Lane. }

{  This file is part of the Independent JPEG Group's software.
  For conditions of distribution and use, see the accompanying README file.

  This file contains application interface code for the decompression half
  of the JPEG library.  These are the "standard" API routines that are
  used in the normal full-decompression case.  They are not used by a
  transcoding-only application.  Note that if an application links in
  jpeg_start_decompress, it will end up linking in the entire decompressor.
  We thus must separate this file from jdapimin.c to avoid linking the
  whole decompression library into a transcoder. }


{ Read some scanlines of data from the JPEG decompressor.

  The return value will be the number of lines actually read.
  This may be less than the number requested in several cases,
  including bottom of image, data source suspension, and operating
  modes that emit multiple scanlines at a time.

  Note: we warn about excess calls to jpeg_read_scanlines() since
  this likely signals an application programmer error.  However,
  an oversize buffer (max_lines > scanlines remaining) is not an error. }

{GLOBAL}
function jpeg_read_scanlines (cinfo : j_decompress_ptr;
                              scanlines : JSAMPARRAY;
		              max_lines : JDIMENSION) : JDIMENSION;


{ Alternate entry point to read raw data.
  Processes exactly one iMCU row per call, unless suspended. }

{GLOBAL}
function jpeg_read_raw_data (cinfo : j_decompress_ptr;
                             data : JSAMPIMAGE;
		             max_lines : JDIMENSION) : JDIMENSION;

{$ifdef D_MULTISCAN_FILES_SUPPORTED}

{ Initialize for an output pass in buffered-image mode. }

{GLOBAL}
function jpeg_start_output (cinfo : j_decompress_ptr;
                            scan_number : int) : boolean;

{ Finish up after an output pass in buffered-image mode.

  Returns FALSE if suspended.  The return value need be inspected only if
  a suspending data source is used. }

{GLOBAL}
function jpeg_finish_output (cinfo : j_decompress_ptr) : boolean;

{$endif} { D_MULTISCAN_FILES_SUPPORTED }

{ Decompression initialization.
  jpeg_read_header must be completed before calling this.

  If a multipass operating mode was selected, this will do all but the
  last pass, and thus may take a great deal of time.

  Returns FALSE if suspended.  The return value need be inspected only if
  a suspending data source is used. }

{GLOBAL}
function jpeg_start_decompress (cinfo : j_decompress_ptr) : boolean;

implementation

uses
  SysUtils;

{== imjmorecfg implementation ==}


{== imjdeferr implementation ==}


{== imjpeglib implementation ==}


{== imjinclude implementation ==}

procedure MEMZERO(target : pointer; size : size_t);
begin
  FillChar(target^, size, 0);
end;

procedure MEMCOPY(dest, src : pointer; size : size_t);
begin
  Move(src^, dest^, size);
end;

{ In ANSI C, and indeed any rational implementation, size_t is also the
  type returned by sizeof().  However, it seems there are some irrational
  implementations out there, in which sizeof() returns an int even though
  size_t is defined as long or unsigned long.  To ensure consistent results
  we always use this SIZEOF() macro in place of using sizeof() directly. }


{#define
  SIZEOF(object)  (size_t(sizeof(object))}

{== imjutils implementation ==}

{GLOBAL}
function jdiv_round_up (a : long; b : long) : long;
{ Compute a/b rounded up to next integer, ie, ceil(a/b) }
{ Assumes a >= 0, b > 0 }
begin
  jdiv_round_up := (a + b - long(1)) div b;
end;


{GLOBAL}
function jround_up (a : long; b : long) : long;
{ Compute a rounded up to next multiple of b, ie, ceil(a/b)*b }
{ Assumes a >= 0, b > 0 }
begin
  Inc(a, b - long(1));
  jround_up := a - (a mod b);
end;

{ MEMCOPY() and MEMZERO() for sample arrays and coefficient-block arrays
  (these were FAR on 80x86). }


procedure FMEMZERO(target : pointer; size : size_t);
begin
  FillChar(target^, size, 0);
end;

procedure FMEMCOPY(dest,src : pointer; size : size_t);
begin
  Move(src^, dest^, size);
end;


{GLOBAL}
procedure jcopy_sample_rows (input_array : JSAMPARRAY; source_row : int;
                             output_array : JSAMPARRAY; dest_row : int;
		             num_rows : int; num_cols : JDIMENSION);
{ Copy some rows of samples from one place to another.
  num_rows rows are copied from input_array[source_row++]
  to output_array[dest_row++]; these areas may overlap for duplication.
  The source and destination arrays must be at least as wide as num_cols. }
var
  inptr, outptr : JSAMPLE_PTR; {register}
  count : size_t; {register}
  row : int; {register}
begin
  count := size_t(num_cols * SIZEOF(JSAMPLE));
  Inc(JSAMPROW_PTR(input_array), source_row);
  Inc(JSAMPROW_PTR(output_array), dest_row);

  for row := pred(num_rows) downto 0 do
  begin
    inptr := JSAMPLE_PTR(input_array^[0]);
    Inc(JSAMPROW_PTR(input_array));
    outptr := JSAMPLE_PTR(output_array^[0]);
    Inc(JSAMPROW_PTR(output_array));
    FMEMCOPY(outptr, inptr, count);
  end;
end;


{GLOBAL}
procedure jcopy_block_row (input_row : JBLOCKROW;
                           output_row : JBLOCKROW;
                           num_blocks : JDIMENSION);
{ Copy a row of coefficient blocks from one place to another. }
begin
  FMEMCOPY(output_row, input_row, num_blocks * (DCTSIZE2 * SIZEOF(JCOEF)));
end;


{GLOBAL}
procedure jzero_far (target : pointer;{far} bytestozero : size_t);
{ Zero out a chunk of FAR memory. }
{ This might be sample-array data, block-array data, or alloc_large data. }
begin
  FMEMZERO(target, bytestozero);
end;

{== imjcomapi implementation ==}

{ Abort processing of a JPEG compression or decompression operation,
  but don't destroy the object itself.

  For this, we merely clean up all the nonpermanent memory pools.
  Note that temp files (virtual arrays) are not allowed to belong to
  the permanent pool, so we will be able to close all temp files here.
  Closing a data source or destination, if necessary, is the application's
  responsibility. }


{GLOBAL}
procedure jpeg_abort (cinfo : j_common_ptr);
var
  pool : int;
begin
  { Do nothing if called on a not-initialized or destroyed JPEG object. }
  if (cinfo^.mem = NIL) then
    exit;

  { Releasing pools in reverse order might help avoid fragmentation
    with some (brain-damaged) malloc libraries. }

  for pool := JPOOL_NUMPOOLS-1 downto JPOOL_PERMANENT+1 do
  begin
    cinfo^.mem^.free_pool (cinfo, pool);
  end;

  { Reset overall state for possible reuse of object }
  if (cinfo^.is_decompressor) then
  begin
    cinfo^.global_state := DSTATE_START;
    { Try to keep application from accessing now-deleted marker list.
      A bit kludgy to do it here, but this is the most central place. }
    j_decompress_ptr(cinfo)^.marker_list := NIL;
  end
  else
  begin
    cinfo^.global_state := CSTATE_START;
  end;
end;


{ Destruction of a JPEG object.

  Everything gets deallocated except the master jpeg_compress_struct itself
  and the error manager struct.  Both of these are supplied by the application
  and must be freed, if necessary, by the application.  (Often they are on
  the stack and so don't need to be freed anyway.)
  Closing a data source or destination, if necessary, is the application's
  responsibility. }


{GLOBAL}
procedure jpeg_destroy (cinfo : j_common_ptr);
begin
  { We need only tell the memory manager to release everything. }
  { NB: mem pointer is NIL if memory mgr failed to initialize. }
  if (cinfo^.mem <> NIL) then
    cinfo^.mem^.self_destruct (cinfo);
  cinfo^.mem := NIL;           { be safe if jpeg_destroy is called twice }
  cinfo^.global_state := 0;    { mark it destroyed }
end;


{ Convenience routines for allocating quantization and Huffman tables.
  (Would jutils.c be a more reasonable place to put these?) }


{GLOBAL}
function jpeg_alloc_quant_table (cinfo : j_common_ptr) : JQUANT_TBL_PTR;
var
  tbl : JQUANT_TBL_PTR;
begin
  tbl := JQUANT_TBL_PTR(
    cinfo^.mem^.alloc_small (cinfo, JPOOL_PERMANENT, SIZEOF(JQUANT_TBL))
                      );
  tbl^.sent_table := FALSE;   { make sure this is false in any new table }
  jpeg_alloc_quant_table := tbl;
end;


{GLOBAL}
function jpeg_alloc_huff_table (cinfo : j_common_ptr) : JHUFF_TBL_PTR;
var
  tbl : JHUFF_TBL_PTR;
begin
  tbl := JHUFF_TBL_PTR(
    cinfo^.mem^.alloc_small (cinfo, JPOOL_PERMANENT, SIZEOF(JHUFF_TBL))
                     );
  tbl^.sent_table := FALSE;   { make sure this is false in any new table }
  jpeg_alloc_huff_table := tbl;
end;

{== imjerror implementation ==}

{ How to format a message string, in format_message() ? }


{ Error exit handler: must not return to caller.

  Applications may override this if they want to get control back after
  an error.  Typically one would longjmp somewhere instead of exiting.
  The setjmp buffer can be made a private field within an expanded error
  handler object.  Note that the info needed to generate an error message
  is stored in the error object, so you can generate the message now or
  later, at your convenience.
  You should make sure that the JPEG object is cleaned up (with jpeg_abort
  or jpeg_destroy) at some point. }


{METHODDEF}
procedure error_exit (cinfo : j_common_ptr);
begin
  { Always display the message }
  cinfo^.err^.output_message(cinfo);

  { Let the memory manager delete any temp files before we die }
  jpeg_destroy(cinfo);

  halt(EXIT_FAILURE);
end;


{ Actual output of an error or trace message.
  Applications may override this method to send JPEG messages somewhere
  other than stderr. }

{ Macros to simplify using the error and trace message stuff }
{ The first parameter is either type of cinfo pointer }

{ Fatal errors (print message and exit) }
procedure ERREXIT(cinfo : j_common_ptr; code : J_MESSAGE_CODE);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.error_exit(cinfo);
end;

procedure ERREXIT1(cinfo : j_common_ptr; code : J_MESSAGE_CODE; p1 : uInt);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.error_exit (cinfo);
end;

procedure ERREXIT2(cinfo : j_common_ptr; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.msg_parm.i[1] := p2;
  cinfo^.err^.error_exit (cinfo);
end;

procedure ERREXIT3(cinfo : j_common_ptr; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.msg_parm.i[1] := p2;
  cinfo^.err^.msg_parm.i[2] := p3;
  cinfo^.err^.error_exit (cinfo);
end;

procedure ERREXIT4(cinfo : j_common_ptr; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int; p4 : int);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.msg_parm.i[1] := p2;
  cinfo^.err^.msg_parm.i[2] := p3;
  cinfo^.err^.msg_parm.i[3] := p4;
  cinfo^.err^.error_exit (cinfo);
end;

procedure ERREXITS(cinfo : j_common_ptr;code : J_MESSAGE_CODE;
                   str : AnsiString);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.s := str;  { string[JMSG_STR_PARM_MAX] }
  cinfo^.err^.error_exit (cinfo);
end;

{ Nonfatal errors (we can keep going, but the data is probably corrupt) }

procedure WARNMS(cinfo : j_common_ptr; code : J_MESSAGE_CODE);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.emit_message(cinfo, -1);
end;

procedure WARNMS1(cinfo : j_common_ptr;code : J_MESSAGE_CODE; p1 : int);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.emit_message (cinfo, -1);
end;

procedure WARNMS2(cinfo : j_common_ptr; code : J_MESSAGE_CODE;
                  p1 : int; p2 : int);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.msg_parm.i[1] := p2;
  cinfo^.err^.emit_message (cinfo, -1);
end;

{ Informational/debugging messages }
procedure TRACEMS(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.emit_message(cinfo, lvl);
end;

procedure TRACEMS1(cinfo : j_common_ptr; lvl : int;
                   code : J_MESSAGE_CODE; p1 : long);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.emit_message (cinfo, lvl);
end;

procedure TRACEMS2(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                   p1 : int;
                   p2 : int);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.msg_parm.i[1] := p2;
  cinfo^.err^.emit_message (cinfo, lvl);
end;

procedure TRACEMS3(cinfo : j_common_ptr;
                   lvl : int;
                   code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int);
var
  _mp : int8array;
begin
  _mp[0] := p1; _mp[1] := p2; _mp[2] := p3;
  cinfo^.err^.msg_parm.i := _mp;
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.emit_message (cinfo, lvl);
end;


procedure TRACEMS4(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int; p4 : int);
var
  _mp : int8array;
begin
  _mp[0] := p1; _mp[1] := p2; _mp[2] := p3; _mp[3] := p4;
  cinfo^.err^.msg_parm.i := _mp;
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.emit_message (cinfo, lvl);
end;

procedure TRACEMS5(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int; p4 : int; p5 : int);
var
  _mp : ^int8array;
begin
  _mp := @cinfo^.err^.msg_parm.i;
  _mp^[0] := p1; _mp^[1] := p2; _mp^[2] := p3;
  _mp^[3] := p4; _mp^[5] := p5;
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.emit_message (cinfo, lvl);
end;

procedure TRACEMS8(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                  p1 : int; p2 : int; p3 : int; p4 : int;
                  p5 : int; p6 : int; p7 : int; p8 : int);
var
  _mp : int8array;
begin
  _mp[0] := p1; _mp[1] := p2; _mp[2] := p3; _mp[3] := p4;
  _mp[4] := p5; _mp[5] := p6; _mp[6] := p7; _mp[7] := p8;
  cinfo^.err^.msg_parm.i := _mp;
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.emit_message (cinfo, lvl);
end;

procedure TRACEMSS(cinfo : j_common_ptr; lvl : int;
                   code : J_MESSAGE_CODE; str : AnsiString);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.s := str; { string JMSG_STR_PARM_MAX }
  cinfo^.err^.emit_message (cinfo, lvl);
end;

{METHODDEF}
procedure output_message (cinfo : j_common_ptr);
var
  buffer : AnsiString; {[JMSG_LENGTH_MAX];}
begin
  { Create the message }
  cinfo^.err^.format_message (cinfo, buffer);

  { Send it to stderr, adding a newline }
  WriteLn(output, buffer);
end;


{ Decide whether to emit a trace or warning message.
  msg_level is one of:
    -1: recoverable corrupt-data warning, may want to abort.
     0: important advisory messages (always display to user).
     1: first level of tracing detail.
     2,3,...: successively more detailed tracing messages.
  An application might override this method if it wanted to abort on warnings
  or change the policy about which messages to display. }


{METHODDEF}
procedure emit_message (cinfo : j_common_ptr; msg_level : int);
var
  err : jpeg_error_mgr_ptr;
begin
  err := cinfo^.err;
  if (msg_level < 0) then
  begin
    { It's a warning message.  Since corrupt files may generate many warnings,
      the policy implemented here is to show only the first warning,
      unless trace_level >= 3. }

    if (err^.num_warnings = 0) or (err^.trace_level >= 3) then
      err^.output_message(cinfo);
    { Always count warnings in num_warnings. }
    Inc( err^.num_warnings );
  end
  else
  begin
    { It's a trace message.  Show it if trace_level >= msg_level. }
    if (err^.trace_level >= msg_level) then
      err^.output_message (cinfo);
  end;
end;


{ Format a message string for the most recent JPEG error or message.
  The message is stored into buffer, which should be at least JMSG_LENGTH_MAX
  characters.  Note that no '\n' character is added to the string.
  Few applications should need to override this method. }


{METHODDEF}
procedure format_message (cinfo : j_common_ptr; var buffer : AnsiString);
var
  err : jpeg_error_mgr_ptr;
  msg_code : J_MESSAGE_CODE;
  msgtext : AnsiString;
  isstring : boolean;
begin
  err := cinfo^.err;
  msg_code := J_MESSAGE_CODE(err^.msg_code);
  msgtext := '';

  { Look up message string in proper table }
  if (msg_code > JMSG_NOMESSAGE)
    and (msg_code <= J_MESSAGE_CODE(err^.last_jpeg_message)) then
  begin
    msgtext := err^.jpeg_message_table^[msg_code];
  end
  else
  if (err^.addon_message_table <> NIL) and
     (msg_code >= err^.first_addon_message) and
     (msg_code <= err^.last_addon_message) then
  begin
    msgtext := err^.addon_message_table^[J_MESSAGE_CODE
           (ord(msg_code) - ord(err^.first_addon_message))];
  end;

  { Defend against bogus message number }
  if (msgtext = '') then
  begin
    err^.msg_parm.i[0] := int(msg_code);
    msgtext := err^.jpeg_message_table^[JMSG_NOMESSAGE];
  end;

  { Check for string parameter, as indicated by %s in the message text }
  isstring := Pos('%s', string(msgtext)) > 0;

  { Format the message into the passed buffer }
  if (isstring) then
    buffer := Concat(msgtext, err^.msg_parm.s)
  else
  begin
   buffer := AnsiString(Format(string(msgtext), [
        err^.msg_parm.i[0], err^.msg_parm.i[1],
        err^.msg_parm.i[2], err^.msg_parm.i[3],
        err^.msg_parm.i[4], err^.msg_parm.i[5],
        err^.msg_parm.i[6], err^.msg_parm.i[7] ]));
  end;
end;


{ Reset error state variables at start of a new image.
  This is called during compression startup to reset trace/error
  processing to default state, without losing any application-specific
  method pointers.  An application might possibly want to override
  this method if it has additional error processing state. }


{METHODDEF}
procedure reset_error_mgr (cinfo : j_common_ptr);
begin
  cinfo^.err^.num_warnings := 0;
  { trace_level is not reset since it is an application-supplied parameter }
  cinfo^.err^.msg_code := 0;      { may be useful as a flag for "no error" }
end;


{ Fill in the standard error-handling methods in a jpeg_error_mgr object.
  Typical call is:
        cinfo : jpeg_compress_struct;
        err : jpeg_error_mgr;

        cinfo.err := jpeg_std_error(@err);
  after which the application may override some of the methods. }


{GLOBAL}
function jpeg_std_error (var err : jpeg_error_mgr) : jpeg_error_mgr_ptr;
begin
  err.error_exit := error_exit;
  err.emit_message := emit_message;
  err.output_message := output_message;
  err.format_message := format_message;
  err.reset_error_mgr := reset_error_mgr;

  err.trace_level := 0;         { default := no tracing }
  err.num_warnings := 0;        { no warnings emitted yet }
  err.msg_code := 0;            { may be useful as a flag for "no error" }

  { Initialize message table pointers }
  err.jpeg_message_table := @jpeg_std_message_table;
  err.last_jpeg_message := pred(JMSG_LASTMSGCODE);

  err.addon_message_table := NIL;
  err.first_addon_message := JMSG_NOMESSAGE;  { for safety }
  err.last_addon_message := JMSG_NOMESSAGE;

  jpeg_std_error := @err;
end;

{== imjmem implementation ==}

{ This structure holds whatever state is needed to access a single
  backing-store object.  The read/write/close method pointers are called
  by jmemmgr.c to manipulate the backing-store object; all other fields
  are private to the system-dependent backing store routines. }


{ These two functions are used to allocate and release small chunks of
  memory.  (Typically the total amount requested through jpeg_get_small is
  no more than 20K or so; this will be requested in chunks of a few K each.)
  Behavior should be the same as for the standard library functions malloc
  and free; in particular, jpeg_get_small must return NIL on failure.
  On most systems, these ARE malloc and free.  jpeg_free_small is passed the
  size of the object being freed, just in case it's needed.
  On an 80x86 machine using small-data memory model, these manage near heap. }


{ Near-memory allocation and freeing are controlled by the regular library
  routines malloc() and free(). }

{GLOBAL}
function jpeg_get_small (cinfo : j_common_ptr;
                         sizeofobject : size_t) : pointer;
var
  p : pointer;
begin
  GetMem(p, sizeofobject);
  jpeg_get_small := p;
end;

{GLOBAL}
{object is a reserved word in Object Pascal }
procedure jpeg_free_small (cinfo : j_common_ptr;
                           an_object : pointer;
                           sizeofobject : size_t);
begin
  FreeMem(an_object, sizeofobject);
end;

{ These two functions are used to allocate and release large chunks of
  memory (up to the total free space designated by jpeg_mem_available).
  The interface is the same as above, except that on an 80x86 machine,
  far pointers are used.  On most other machines these are identical to
  the jpeg_get/free_small routines; but we keep them separate anyway,
  in case a different allocation strategy is desirable for large chunks. }


{GLOBAL}
function jpeg_get_large (cinfo : j_common_ptr;
                         sizeofobject : size_t) : voidp; {far}
var
  p : pointer;
begin
  GetMem(p, sizeofobject);
  jpeg_get_large := p;
end;

{GLOBAL}
procedure jpeg_free_large (cinfo : j_common_ptr;
                          {var?} an_object : voidp; {FAR}
                          sizeofobject : size_t);
begin
  Freemem(an_object, sizeofobject);
end;

{ This routine computes the total space still available for allocation by
  jpeg_get_large.  If more space than this is needed, backing store will be
  used.  NOTE: any memory already allocated must not be counted.

  There is a minimum space requirement, corresponding to the minimum
  feasible buffer sizes; jmemmgr.c will request that much space even if
  jpeg_mem_available returns zero.  The maximum space needed, enough to hold
  all working storage in memory, is also passed in case it is useful.
  Finally, the total space already allocated is passed.  If no better
  method is available, cinfo^.mem^.max_memory_to_use - already_allocated
  is often a suitable calculation.

  It is OK for jpeg_mem_available to underestimate the space available
  (that'll just lead to more backing-store access than is really necessary).
  However, an overestimate will lead to failure.  Hence it's wise to subtract
  a slop factor from the true available space.  5% should be enough.

  On machines with lots of virtual memory, any large constant may be returned.
  Conversely, zero may be returned to always use the minimum amount of memory.}


{ This routine computes the total memory space available for allocation.
  It's impossible to do this in a portable way; our current solution is
  to make the user tell us (with a default value set at compile time).
  If you can actually get the available space, it's a good idea to subtract
  a slop factor of 5% or so. }

const
  DEFAULT_MAX_MEM = long(300000);   { for total usage about 450K }

{GLOBAL}
function jpeg_mem_available (cinfo : j_common_ptr;
                             min_bytes_needed : long;
                             max_bytes_needed : long;
                             already_allocated : long) : long;
begin
  {jpeg_mem_available := cinfo^.mem^.max_memory_to_use - already_allocated;}
  jpeg_mem_available := max_bytes_needed;
end;


{ Initial opening of a backing-store object.  This must fill in the
  read/write/close pointers in the object.  The read/write routines
  may take an error exit if the specified maximum file size is exceeded.
  (If jpeg_mem_available always returns a large value, this routine can
  just take an error exit.) }


{ Initial opening of a backing-store object. }

{GLOBAL}
procedure jpeg_open_backing_store (cinfo : j_common_ptr;
                                   info : backing_store_ptr;
                                   total_bytes_needed : long);
begin
  ERREXIT(cinfo, JERR_NO_BACKING_STORE);
end;

{ These routines take care of any system-dependent initialization and
  cleanup required.  jpeg_mem_init will be called before anything is
  allocated (and, therefore, nothing in cinfo is of use except the error
  manager pointer).  It should return a suitable default value for
  max_memory_to_use; this may subsequently be overridden by the surrounding
  application.  (Note that max_memory_to_use is only important if
  jpeg_mem_available chooses to consult it ... no one else will.)
  jpeg_mem_term may assume that all requested memory has been freed and that
  all opened backing-store objects have been closed. }


{ These routines take care of any system-dependent initialization and
  cleanup required. }


{GLOBAL}
function jpeg_mem_init (cinfo : j_common_ptr) : long;
begin
  jpeg_mem_init := DEFAULT_MAX_MEM;   { default for max_memory_to_use }
end;

{GLOBAL}
procedure jpeg_mem_term (cinfo : j_common_ptr);
begin

end;

{== imjmemmgr implementation ==}

{ Some important notes:
    The allocation routines provided here must never return NIL.
    They should exit to error_exit if unsuccessful.

    It's not a good idea to try to merge the sarray and barray routines,
    even though they are textually almost the same, because samples are
    usually stored as bytes while coefficients are shorts or ints.  Thus,
    in machines where byte pointers have a different representation from
    word pointers, the resulting machine code could not be the same.  }


{ Many machines require storage alignment: longs must start on 4-byte
  boundaries, doubles on 8-byte boundaries, etc.  On such machines, malloc()
  always returns pointers that are multiples of the worst-case alignment
  requirement, and we had better do so too.
  There isn't any really portable way to determine the worst-case alignment
  requirement.  This module assumes that the alignment requirement is
  multiples of sizeof(ALIGN_TYPE).
  By default, we define ALIGN_TYPE as double.  This is necessary on some
  workstations (where doubles really do need 8-byte alignment) and will work
  fine on nearly everything.  If your machine has lesser alignment needs,
  you can save a few bytes by making ALIGN_TYPE smaller.
  The only place I know of where this will NOT work is certain Macintosh
  680x0 compilers that define double as a 10-byte IEEE extended float.
  Doing 10-byte alignment is counterproductive because longwords won't be
  aligned well.  Put "#define ALIGN_TYPE long" in jconfig.h if you have
  such a compiler. }

{$ifndef ALIGN_TYPE} { so can override from jconfig.h }
type
  ALIGN_TYPE = double;
{$endif}


{ We allocate objects from "pools", where each pool is gotten with a single
  request to jpeg_get_small() or jpeg_get_large().  There is no per-object
  overhead within a pool, except for alignment padding.  Each pool has a
  header with a link to the next pool of the same class.
  Small and large pool headers are identical except that the latter's
  link pointer must be FAR on 80x86 machines.
  Notice that the "real" header fields are union'ed with a dummy ALIGN_TYPE
  field.  This forces the compiler to make SIZEOF(small_pool_hdr) a multiple
  of the alignment requirement of ALIGN_TYPE. }

type
  small_pool_ptr = ^small_pool_hdr;
  small_pool_hdr = record
  case byte of
    0:(hdr : record
               next : small_pool_ptr;   { next in list of pools }
               bytes_used : size_t;     { how many bytes already used within pool }
               bytes_left : size_t;     { bytes still available in this pool }
             end);
    1:(dummy : ALIGN_TYPE);             { included in union to ensure alignment }
  end; {small_pool_hdr;}

type
  large_pool_ptr = ^large_pool_hdr; {FAR}
  large_pool_hdr = record
  case byte of
    0:(hdr : record
               next : large_pool_ptr;   { next in list of pools }
               bytes_used : size_t;     { how many bytes already used within pool }
               bytes_left : size_t;     { bytes still available in this pool }
             end);
    1:(dummy : ALIGN_TYPE);             { included in union to ensure alignment }
  end; {large_pool_hdr;}


{ Here is the full definition of a memory manager object. }

type
  my_mem_ptr = ^my_memory_mgr;
  my_memory_mgr = record
    pub : jpeg_memory_mgr;	{ public fields }

    { Each pool identifier (lifetime class) names a linked list of pools. }
    small_list : array[0..JPOOL_NUMPOOLS-1] of small_pool_ptr ;
    large_list : array[0..JPOOL_NUMPOOLS-1] of large_pool_ptr ;

    { Since we only have one lifetime class of virtual arrays, only one
      linked list is necessary (for each datatype).  Note that the virtual
      array control blocks being linked together are actually stored somewhere
      in the small-pool list. }

    virt_sarray_list : jvirt_sarray_ptr;
    virt_barray_list : jvirt_barray_ptr;

    { This counts total space obtained from jpeg_get_small/large }
    total_space_allocated : long;

    { alloc_sarray and alloc_barray set this value for use by virtual
      array routines. }

    last_rowsperchunk : JDIMENSION;	{ from most recent alloc_sarray/barray }
  end; {my_memory_mgr;}


{LOCAL}
procedure out_of_memory (cinfo : j_common_ptr; which : int);
{ Report an out-of-memory error and stop execution }
begin
  ERREXIT1(cinfo, JERR_OUT_OF_MEMORY, which);
end;


{ Allocation of "small" objects.

  For these, we use pooled storage.  When a new pool must be created,
  we try to get enough space for the current request plus a "slop" factor,
  where the slop will be the amount of leftover space in the new pool.
  The speed vs. space tradeoff is largely determined by the slop values.
  A different slop value is provided for each pool class (lifetime),
  and we also distinguish the first pool of a class from later ones.
  NOTE: the values given work fairly well on both 16- and 32-bit-int
  machines, but may be too small if longs are 64 bits or more. }

const
  first_pool_slop : array[0..JPOOL_NUMPOOLS-1] of size_t  =
	(1600,			{ first PERMANENT pool }
	16000);			{ first IMAGE pool }

const
  extra_pool_slop : array[0..JPOOL_NUMPOOLS-1] of size_t =
	(0,			{ additional PERMANENT pools }
	5000);			{ additional IMAGE pools }

const
  MIN_SLOP = 50;		{ greater than 0 to avoid futile looping }


{METHODDEF}
function alloc_small (cinfo : j_common_ptr;
                      pool_id : int;
                      sizeofobject : size_t) : pointer;
type
  byteptr = ^byte;
{ Allocate a "small" object }
var
  mem : my_mem_ptr;
  hdr_ptr, prev_hdr_ptr : small_pool_ptr;
  data_ptr : byteptr;
  odd_bytes, min_request, slop : size_t;
begin
  mem := my_mem_ptr (cinfo^.mem);

  { Check for unsatisfiable request (do now to ensure no overflow below) }
  if (sizeofobject > size_t(MAX_ALLOC_CHUNK-SIZEOF(small_pool_hdr))) then
    out_of_memory(cinfo, 1);	{ request exceeds malloc's ability }

  { Round up the requested size to a multiple of SIZEOF(ALIGN_TYPE) }
  odd_bytes := sizeofobject mod SIZEOF(ALIGN_TYPE);
  if (odd_bytes > 0) then
    Inc(sizeofobject, SIZEOF(ALIGN_TYPE) - odd_bytes);

  { See if space is available in any existing pool }
  if (pool_id < 0) or (pool_id >= JPOOL_NUMPOOLS) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_POOL_ID, pool_id);	{ safety check }
  prev_hdr_ptr := NIL;
  hdr_ptr := mem^.small_list[pool_id];
  while (hdr_ptr <> NIL) do
  begin
    if (hdr_ptr^.hdr.bytes_left >= sizeofobject) then
      break;			{ found pool with enough space }
    prev_hdr_ptr := hdr_ptr;
    hdr_ptr := hdr_ptr^.hdr.next;
  end;

  { Time to make a new pool? }
  if (hdr_ptr = NIL) then
  begin
    { min_request is what we need now, slop is what will be leftover }
    min_request := sizeofobject + SIZEOF(small_pool_hdr);
    if (prev_hdr_ptr = NIL) then	{ first pool in class? }
      slop := first_pool_slop[pool_id]
    else
      slop := extra_pool_slop[pool_id];
    { Don't ask for more than MAX_ALLOC_CHUNK }
    if (slop > size_t (MAX_ALLOC_CHUNK-min_request)) then
      slop := size_t (MAX_ALLOC_CHUNK-min_request);
    { Try to get space, if fail reduce slop and try again }
    while TRUE do
    begin
      hdr_ptr := small_pool_ptr(jpeg_get_small(cinfo, min_request + slop));
      if (hdr_ptr <> NIL) then
	break;
      slop := slop div 2;
      if (slop < MIN_SLOP) then   { give up when it gets real small }
	out_of_memory(cinfo, 2);  { jpeg_get_small failed }
    end;
    Inc(mem^.total_space_allocated, min_request + slop);
    { Success, initialize the new pool header and add to end of list }
    hdr_ptr^.hdr.next := NIL;
    hdr_ptr^.hdr.bytes_used := 0;
    hdr_ptr^.hdr.bytes_left := sizeofobject + slop;
    if (prev_hdr_ptr = NIL) then       { first pool in class? }
      mem^.small_list[pool_id] := hdr_ptr
    else
      prev_hdr_ptr^.hdr.next := hdr_ptr;
  end;

  { OK, allocate the object from the current pool }
  data_ptr := byteptr (hdr_ptr);
  Inc(small_pool_ptr(data_ptr));  { point to first data byte in pool }
  Inc(data_ptr, hdr_ptr^.hdr.bytes_used); { point to place for object }
  Inc(hdr_ptr^.hdr.bytes_used, sizeofobject);
  Dec(hdr_ptr^.hdr.bytes_left, sizeofobject);

  alloc_small := pointer(data_ptr);
end;


{ Allocation of "large" objects.

  The external semantics of these are the same as "small" objects,
  except that FAR pointers are used on 80x86.  However the pool
  management heuristics are quite different.  We assume that each
  request is large enough that it may as well be passed directly to
  jpeg_get_large; the pool management just links everything together
  so that we can free it all on demand.
  Note: the major use of "large" objects is in JSAMPARRAY and JBLOCKARRAY
  structures.  The routines that create these structures (see below)
  deliberately bunch rows together to ensure a large request size. }

{METHODDEF}
function alloc_large (cinfo : j_common_ptr;
                      pool_id : int;
                      sizeofobject : size_t) : pointer;
{ Allocate a "large" object }
var
  mem : my_mem_ptr;
  hdr_ptr : large_pool_ptr;
  odd_bytes : size_t;
var
  dest_ptr : large_pool_ptr;
begin
  mem := my_mem_ptr (cinfo^.mem);

  { Check for unsatisfiable request (do now to ensure no overflow below) }
  if (sizeofobject > size_t (MAX_ALLOC_CHUNK-SIZEOF(large_pool_hdr))) then
    out_of_memory(cinfo, 3);	{ request exceeds malloc's ability }

  { Round up the requested size to a multiple of SIZEOF(ALIGN_TYPE) }
  odd_bytes := sizeofobject mod SIZEOF(ALIGN_TYPE);
  if (odd_bytes > 0) then
    Inc(sizeofobject, SIZEOF(ALIGN_TYPE) - odd_bytes);

  { Always make a new pool }
  if (pool_id < 0) or (pool_id >= JPOOL_NUMPOOLS) then
    ERREXIT1(cinfo, JERR_BAD_POOL_ID, pool_id);	{ safety check }

  hdr_ptr := large_pool_ptr (jpeg_get_large(cinfo, sizeofobject +
					    SIZEOF(large_pool_hdr)));
  if (hdr_ptr = NIL) then
    out_of_memory(cinfo, 4);	{ jpeg_get_large failed }
  Inc(mem^.total_space_allocated, sizeofobject + SIZEOF(large_pool_hdr));

  { Success, initialize the new pool header and add to list }
  hdr_ptr^.hdr.next := mem^.large_list[pool_id];
  { We maintain space counts in each pool header for statistical purposes,
    even though they are not needed for allocation. }

  hdr_ptr^.hdr.bytes_used := sizeofobject;
  hdr_ptr^.hdr.bytes_left := 0;
  mem^.large_list[pool_id] := hdr_ptr;

  {alloc_large := pointerFAR (hdr_ptr + 1); - point to first data byte in pool }
  dest_ptr := hdr_ptr;
  Inc(large_pool_ptr(dest_ptr));
  alloc_large := dest_ptr;
end;


{ Creation of 2-D sample arrays.
  The pointers are in near heap, the samples themselves in FAR heap.

  To minimize allocation overhead and to allow I/O of large contiguous
  blocks, we allocate the sample rows in groups of as many rows as possible
  without exceeding MAX_ALLOC_CHUNK total bytes per allocation request.
  NB: the virtual array control routines, later in this file, know about
  this chunking of rows.  The rowsperchunk value is left in the mem manager
  object so that it can be saved away if this sarray is the workspace for
  a virtual array. }

{METHODDEF}
function alloc_sarray (cinfo : j_common_ptr;
                       pool_id : int;
	               samplesperrow : JDIMENSION;
                       numrows : JDIMENSION) : JSAMPARRAY;
{ Allocate a 2-D sample array }
var
  mem : my_mem_ptr;
  the_result : JSAMPARRAY;
  workspace : JSAMPROW;
  rowsperchunk, currow, i : JDIMENSION;
  ltemp : long;
begin
  mem := my_mem_ptr(cinfo^.mem);

  { Calculate max # of rows allowed in one allocation chunk }
  ltemp := (MAX_ALLOC_CHUNK-SIZEOF(large_pool_hdr)) div
	  (long(samplesperrow) * SIZEOF(JSAMPLE));
  if (ltemp <= 0) then
    ERREXIT(cinfo, JERR_WIDTH_OVERFLOW);
  if (ltemp < long(numrows)) then
    rowsperchunk := JDIMENSION (ltemp)
  else
    rowsperchunk := numrows;
  mem^.last_rowsperchunk := rowsperchunk;

  { Get space for row pointers (small object) }
  the_result := JSAMPARRAY (alloc_small(cinfo, pool_id,
				    size_t (numrows * SIZEOF(JSAMPROW))));

  { Get the rows themselves (large objects) }
  currow := 0;
  while (currow < numrows) do
  begin
    {rowsperchunk := MIN(rowsperchunk, numrows - currow);}
    if rowsperchunk > numrows - currow then
      rowsperchunk := numrows - currow;

    workspace := JSAMPROW (alloc_large(cinfo, pool_id,
	size_t (size_t(rowsperchunk) * size_t(samplesperrow)
		  * SIZEOF(JSAMPLE))) );
    for i := pred(rowsperchunk) downto 0 do
    begin
      the_result^[currow] := workspace;
      Inc(currow);
      Inc(JSAMPLE_PTR(workspace), samplesperrow);
    end;
  end;

  alloc_sarray := the_result;
end;


{ Creation of 2-D coefficient-block arrays.
  This is essentially the same as the code for sample arrays, above. }

{METHODDEF}
function alloc_barray (cinfo : j_common_ptr;
                       pool_id : int;
	               blocksperrow : JDIMENSION;
                       numrows : JDIMENSION) : JBLOCKARRAY;
{ Allocate a 2-D coefficient-block array }
var
  mem : my_mem_ptr;
  the_result : JBLOCKARRAY;
  workspace : JBLOCKROW;
  rowsperchunk, currow, i : JDIMENSION;
  ltemp : long;
begin
  mem := my_mem_ptr(cinfo^.mem);

  { Calculate max # of rows allowed in one allocation chunk }
  ltemp := (MAX_ALLOC_CHUNK-SIZEOF(large_pool_hdr)) div
	  (long(blocksperrow) * SIZEOF(JBLOCK));

  if (ltemp <= 0) then
    ERREXIT(cinfo, JERR_WIDTH_OVERFLOW);
  if (ltemp < long(numrows)) then
    rowsperchunk := JDIMENSION (ltemp)
  else
    rowsperchunk := numrows;
  mem^.last_rowsperchunk := rowsperchunk;

  { Get space for row pointers (small object) }
  the_result := JBLOCKARRAY (alloc_small(cinfo, pool_id,
				     size_t (numrows * SIZEOF(JBLOCKROW))) );

  { Get the rows themselves (large objects) }
  currow := 0;
  while (currow < numrows) do
  begin
    {rowsperchunk := MIN(rowsperchunk, numrows - currow);}
    if rowsperchunk > numrows - currow then
      rowsperchunk := numrows - currow;

    workspace := JBLOCKROW (alloc_large(cinfo, pool_id,
	size_t (size_t(rowsperchunk) * size_t(blocksperrow)
		  * SIZEOF(JBLOCK))) );
    for i := rowsperchunk downto 1 do
    begin
      the_result^[currow] := workspace;
      Inc(currow);
      Inc(JBLOCK_PTR(workspace), blocksperrow);
    end;
  end;

  alloc_barray := the_result;
end;


{ About virtual array management:

  The above "normal" array routines are only used to allocate strip buffers
  (as wide as the image, but just a few rows high).  Full-image-sized buffers
  are handled as "virtual" arrays.  The array is still accessed a strip at a
  time, but the memory manager must save the whole array for repeated
  accesses.  The intended implementation is that there is a strip buffer in
  memory (as high as is possible given the desired memory limit), plus a
  backing file that holds the rest of the array.

  The request_virt_array routines are told the total size of the image and
  the maximum number of rows that will be accessed at once.  The in-memory
  buffer must be at least as large as the maxaccess value.

  The request routines create control blocks but not the in-memory buffers.
  That is postponed until realize_virt_arrays is called.  At that time the
  total amount of space needed is known (approximately, anyway), so free
  memory can be divided up fairly.

  The access_virt_array routines are responsible for making a specific strip
  area accessible (after reading or writing the backing file, if necessary).
  Note that the access routines are told whether the caller intends to modify
  the accessed strip; during a read-only pass this saves having to rewrite
  data to disk.  The access routines are also responsible for pre-zeroing
  any newly accessed rows, if pre-zeroing was requested.

  In current usage, the access requests are usually for nonoverlapping
  strips; that is, successive access start_row numbers differ by exactly
  num_rows := maxaccess.  This means we can get good performance with simple
  buffer dump/reload logic, by making the in-memory buffer be a multiple
  of the access height; then there will never be accesses across bufferload
  boundaries.  The code will still work with overlapping access requests,
  but it doesn't handle bufferload overlaps very efficiently. }


{METHODDEF}
function request_virt_sarray (cinfo : j_common_ptr;
                              pool_id : int;
                              pre_zero : boolean;
		              samplesperrow : JDIMENSION;
                              numrows : JDIMENSION;
		              maxaccess : JDIMENSION) : jvirt_sarray_ptr;
{ Request a virtual 2-D sample array }
var
  mem : my_mem_ptr;
  the_result : jvirt_sarray_ptr;
begin
  mem := my_mem_ptr (cinfo^.mem);

  { Only IMAGE-lifetime virtual arrays are currently supported }
  if (pool_id <> JPOOL_IMAGE) then
    ERREXIT1(cinfo, JERR_BAD_POOL_ID, pool_id);	{ safety check }

  { get control block }
  the_result := jvirt_sarray_ptr (alloc_small(cinfo, pool_id,
				  SIZEOF(jvirt_sarray_control)) );

  the_result^.mem_buffer := NIL;	{ marks array not yet realized }
  the_result^.rows_in_array := numrows;
  the_result^.samplesperrow := samplesperrow;
  the_result^.maxaccess := maxaccess;
  the_result^.pre_zero := pre_zero;
  the_result^.b_s_open := FALSE;	{ no associated backing-store object }
  the_result^.next := mem^.virt_sarray_list; { add to list of virtual arrays }
  mem^.virt_sarray_list := the_result;

  request_virt_sarray := the_result;
end;


{METHODDEF}
function request_virt_barray (cinfo : j_common_ptr;
                              pool_id : int;
                              pre_zero : boolean;
		              blocksperrow : JDIMENSION;
                              numrows : JDIMENSION;
		              maxaccess : JDIMENSION) : jvirt_barray_ptr;
{ Request a virtual 2-D coefficient-block array }
var
  mem : my_mem_ptr;
  the_result : jvirt_barray_ptr;
begin
  mem := my_mem_ptr(cinfo^.mem);

  { Only IMAGE-lifetime virtual arrays are currently supported }
  if (pool_id <> JPOOL_IMAGE) then
    ERREXIT1(cinfo, JERR_BAD_POOL_ID, pool_id);	{ safety check }

  { get control block }
  the_result := jvirt_barray_ptr(alloc_small(cinfo, pool_id,
				  SIZEOF(jvirt_barray_control)) );

  the_result^.mem_buffer := NIL;	{ marks array not yet realized }
  the_result^.rows_in_array := numrows;
  the_result^.blocksperrow := blocksperrow;
  the_result^.maxaccess := maxaccess;
  the_result^.pre_zero := pre_zero;
  the_result^.b_s_open := FALSE;	{ no associated backing-store object }
  the_result^.next := mem^.virt_barray_list; { add to list of virtual arrays }
  mem^.virt_barray_list := the_result;

  request_virt_barray := the_result;
end;


{METHODDEF}
procedure realize_virt_arrays (cinfo : j_common_ptr);
{ Allocate the in-memory buffers for any unrealized virtual arrays }
var
  mem : my_mem_ptr;
  space_per_minheight, maximum_space, avail_mem : long;
  minheights, max_minheights : long;
  sptr : jvirt_sarray_ptr;
  bptr : jvirt_barray_ptr;
begin
  mem := my_mem_ptr (cinfo^.mem);
  { Compute the minimum space needed (maxaccess rows in each buffer)
    and the maximum space needed (full image height in each buffer).
    These may be of use to the system-dependent jpeg_mem_available routine. }

  space_per_minheight := 0;
  maximum_space := 0;
  sptr := mem^.virt_sarray_list;
  while (sptr <> NIL) do
  begin
    if (sptr^.mem_buffer = NIL) then
    begin { if not realized yet }
      Inc(space_per_minheight, long(sptr^.maxaccess) *
			     long(sptr^.samplesperrow) * SIZEOF(JSAMPLE));
      Inc(maximum_space, long(sptr^.rows_in_array) *
		       long(sptr^.samplesperrow) * SIZEOF(JSAMPLE));
    end;
    sptr := sptr^.next;
  end;
  bptr := mem^.virt_barray_list;
  while (bptr <> NIL) do
  begin
    if (bptr^.mem_buffer = NIL) then
    begin { if not realized yet }
      Inc(space_per_minheight, long(bptr^.maxaccess) *
			     long(bptr^.blocksperrow) * SIZEOF(JBLOCK));
      Inc(maximum_space, long(bptr^.rows_in_array) *
		       long(bptr^.blocksperrow) * SIZEOF(JBLOCK));
    end;
    bptr := bptr^.next;
  end;

  if (space_per_minheight <= 0) then
    exit;			{ no unrealized arrays, no work }

  { Determine amount of memory to actually use; this is system-dependent. }
  avail_mem := jpeg_mem_available(cinfo, space_per_minheight, maximum_space,
				 mem^.total_space_allocated);

  { If the maximum space needed is available, make all the buffers full
    height; otherwise parcel it out with the same number of minheights
    in each buffer. }

  if (avail_mem >= maximum_space) then
    max_minheights := long(1000000000)
  else
  begin
    max_minheights := avail_mem div space_per_minheight;
    { If there doesn't seem to be enough space, try to get the minimum
      anyway.  This allows a "stub" implementation of jpeg_mem_available(). }
    if (max_minheights <= 0) then
      max_minheights := 1;
  end;

  { Allocate the in-memory buffers and initialize backing store as needed. }

  sptr := mem^.virt_sarray_list;
  while (sptr <> NIL) do
  begin
    if (sptr^.mem_buffer = NIL) then
    begin { if not realized yet }
      minheights := (long(sptr^.rows_in_array) - long(1)) div LongInt(sptr^.maxaccess) + long(1);
      if (minheights <= max_minheights) then
      begin
	{ This buffer fits in memory }
	sptr^.rows_in_mem := sptr^.rows_in_array;
      end
      else
      begin
	{ It doesn't fit in memory, create backing store. }
	sptr^.rows_in_mem := JDIMENSION(max_minheights) * sptr^.maxaccess;
	jpeg_open_backing_store(cinfo,
                                @sptr^.b_s_info,
				long(sptr^.rows_in_array) *
				long(sptr^.samplesperrow) *
				long(SIZEOF(JSAMPLE)));
	sptr^.b_s_open := TRUE;
      end;
      sptr^.mem_buffer := alloc_sarray(cinfo, JPOOL_IMAGE,
				      sptr^.samplesperrow, sptr^.rows_in_mem);
      sptr^.rowsperchunk := mem^.last_rowsperchunk;
      sptr^.cur_start_row := 0;
      sptr^.first_undef_row := 0;
      sptr^.dirty := FALSE;
    end;
    sptr := sptr^.next;
  end;

  bptr := mem^.virt_barray_list;
  while (bptr <> NIL) do
  begin
    if (bptr^.mem_buffer = NIL) then
    begin { if not realized yet }
      minheights := (long(bptr^.rows_in_array) - long(1)) div LongInt(bptr^.maxaccess) + long(1);
      if (minheights <= max_minheights) then
      begin
	{ This buffer fits in memory }
	bptr^.rows_in_mem := bptr^.rows_in_array;
      end
      else
      begin
	{ It doesn't fit in memory, create backing store. }
	bptr^.rows_in_mem := JDIMENSION (max_minheights) * bptr^.maxaccess;
	jpeg_open_backing_store(cinfo,
                                @bptr^.b_s_info,
				long(bptr^.rows_in_array) *
				long(bptr^.blocksperrow) *
				long(SIZEOF(JBLOCK)));
	bptr^.b_s_open := TRUE;
      end;
      bptr^.mem_buffer := alloc_barray(cinfo, JPOOL_IMAGE,
				      bptr^.blocksperrow, bptr^.rows_in_mem);
      bptr^.rowsperchunk := mem^.last_rowsperchunk;
      bptr^.cur_start_row := 0;
      bptr^.first_undef_row := 0;
      bptr^.dirty := FALSE;
    end;
    bptr := bptr^.next;
  end;
end;


{LOCAL}
procedure do_sarray_io (cinfo : j_common_ptr;
                        ptr : jvirt_sarray_ptr;
                        writing : boolean);
{ Do backing store read or write of a virtual sample array }
var
  bytesperrow, file_offset, byte_count, rows, thisrow, i : long;
begin

  bytesperrow := long(ptr^.samplesperrow * SIZEOF(JSAMPLE));
  file_offset := LongInt(ptr^.cur_start_row) * bytesperrow;
  { Loop to read or write each allocation chunk in mem_buffer }
  i := 0;
  while i < long(ptr^.rows_in_mem) do
  begin

    { One chunk, but check for short chunk at end of buffer }
    {rows := MIN(long(ptr^.rowsperchunk), long(ptr^.rows_in_mem - i));}
    rows := long(ptr^.rowsperchunk);
    if rows > long(ptr^.rows_in_mem) - i then
      rows := long(ptr^.rows_in_mem) - i;
    { Transfer no more than is currently defined }
    thisrow := long (ptr^.cur_start_row) + i;
    {rows := MIN(rows, long(ptr^.first_undef_row) - thisrow);}
    if (rows > long(ptr^.first_undef_row) - thisrow) then
      rows := long(ptr^.first_undef_row) - thisrow;
    { Transfer no more than fits in file }
    {rows := MIN(rows, long(ptr^.rows_in_array) - thisrow);}
    if (rows > long(ptr^.rows_in_array) - thisrow) then
      rows := long(ptr^.rows_in_array) - thisrow;

    if (rows <= 0) then        { this chunk might be past end of file! }
      break;
    byte_count := rows * bytesperrow;
    if (writing) then
      ptr^.b_s_info.write_backing_store (cinfo,
                                        @ptr^.b_s_info,
					pointer {FAR} (ptr^.mem_buffer^[i]),
					file_offset, byte_count)
    else
      ptr^.b_s_info.read_backing_store (cinfo,
                                        @ptr^.b_s_info,
					pointer {FAR} (ptr^.mem_buffer^[i]),
					file_offset, byte_count);
    Inc(file_offset, byte_count);
    Inc(i, ptr^.rowsperchunk);
  end;
end;


{LOCAL}
procedure do_barray_io (cinfo : j_common_ptr;
                       ptr : jvirt_barray_ptr;
                       writing : boolean);
{ Do backing store read or write of a virtual coefficient-block array }
var
  bytesperrow, file_offset, byte_count, rows, thisrow, i : long;
begin
  bytesperrow := long (ptr^.blocksperrow) * SIZEOF(JBLOCK);
  file_offset := LongInt(ptr^.cur_start_row) * bytesperrow;
  { Loop to read or write each allocation chunk in mem_buffer }
  i := 0;
  while (i < long(ptr^.rows_in_mem)) do
  begin
    { One chunk, but check for short chunk at end of buffer }
    {rows := MIN(long(ptr^.rowsperchunk), long(ptr^.rows_in_mem - i));}
    rows := long(ptr^.rowsperchunk);
    if rows > long(ptr^.rows_in_mem) - i then
      rows := long(ptr^.rows_in_mem) - i;
    { Transfer no more than is currently defined }
    thisrow := long (ptr^.cur_start_row) + i;
    {rows := MIN(rows, long(ptr^.first_undef_row - thisrow));}
    if rows > long(ptr^.first_undef_row) - thisrow then
      rows := long(ptr^.first_undef_row) - thisrow;
    { Transfer no more than fits in file }
    {rows := MIN(rows, long (ptr^.rows_in_array - thisrow));}
    if (rows > long (ptr^.rows_in_array) - thisrow) then
      rows := long (ptr^.rows_in_array) - thisrow;

    if (rows <= 0) then		{ this chunk might be past end of file! }
      break;
    byte_count := rows * bytesperrow;
    if (writing) then
      ptr^.b_s_info.write_backing_store (cinfo,
                                         @ptr^.b_s_info,
	                                 {FAR} pointer(ptr^.mem_buffer^[i]),
					  file_offset, byte_count)
    else
      ptr^.b_s_info.read_backing_store (cinfo,
                                        @ptr^.b_s_info,
					{FAR} pointer(ptr^.mem_buffer^[i]),
					file_offset, byte_count);
    Inc(file_offset, byte_count);
    Inc(i, ptr^.rowsperchunk);
  end;
end;


{METHODDEF}
function access_virt_sarray (cinfo : j_common_ptr;
                             ptr : jvirt_sarray_ptr;
		             start_row : JDIMENSION;
                             num_rows : JDIMENSION;
		             writable : boolean ) : JSAMPARRAY;
{ Access the part of a virtual sample array starting at start_row }
{ and extending for num_rows rows.  writable is true if  }
{ caller intends to modify the accessed area. }
var
  end_row : JDIMENSION;
  undef_row : JDIMENSION;
var
  bytesperrow : size_t;
var
  ltemp : long;
begin
  end_row := start_row + num_rows;
  { debugging check }
  if (end_row > ptr^.rows_in_array) or (num_rows > ptr^.maxaccess) or
     (ptr^.mem_buffer = NIL) then
    ERREXIT(cinfo, JERR_BAD_VIRTUAL_ACCESS);

  { Make the desired part of the virtual array accessible }
  if (start_row < ptr^.cur_start_row) or
     (end_row > ptr^.cur_start_row+ptr^.rows_in_mem) then
  begin
    if (not ptr^.b_s_open) then
      ERREXIT(cinfo, JERR_VIRTUAL_BUG);
    { Flush old buffer contents if necessary }
    if (ptr^.dirty) then
    begin
      do_sarray_io(cinfo, ptr, TRUE);
      ptr^.dirty := FALSE;
    end;
    { Decide what part of virtual array to access.
      Algorithm: if target address > current window, assume forward scan,
      load starting at target address.  If target address < current window,
      assume backward scan, load so that target area is top of window.
      Note that when switching from forward write to forward read, will have
      start_row := 0, so the limiting case applies and we load from 0 anyway. }
    if (start_row > ptr^.cur_start_row) then
    begin
      ptr^.cur_start_row := start_row;
    end
    else
    begin
      { use long arithmetic here to avoid overflow & unsigned problems }


      ltemp := long(end_row) - long(ptr^.rows_in_mem);
      if (ltemp < 0) then
	ltemp := 0;		{ don't fall off front end of file }
      ptr^.cur_start_row := JDIMENSION(ltemp);
    end;
    { Read in the selected part of the array.
      During the initial write pass, we will do no actual read
      because the selected part is all undefined. }

    do_sarray_io(cinfo, ptr, FALSE);
  end;
  { Ensure the accessed part of the array is defined; prezero if needed.
    To improve locality of access, we only prezero the part of the array
    that the caller is about to access, not the entire in-memory array. }
  if (ptr^.first_undef_row < end_row) then
  begin
    if (ptr^.first_undef_row < start_row) then
    begin
      if (writable) then	{ writer skipped over a section of array }
	ERREXIT(cinfo, JERR_BAD_VIRTUAL_ACCESS);
      undef_row := start_row;	{ but reader is allowed to read ahead }
    end
    else
    begin
      undef_row := ptr^.first_undef_row;
    end;
    if (writable) then
      ptr^.first_undef_row := end_row;
    if (ptr^.pre_zero) then
    begin
      bytesperrow := size_t(ptr^.samplesperrow) * SIZEOF(JSAMPLE);
      Dec(undef_row, ptr^.cur_start_row); { make indexes relative to buffer }
      Dec(end_row, ptr^.cur_start_row);
      while (undef_row < end_row) do
      begin
	jzero_far({FAR} pointer(ptr^.mem_buffer^[undef_row]), bytesperrow);
	Inc(undef_row);
      end;
    end
    else
    begin
      if (not writable) then	{ reader looking at undefined data }
	ERREXIT(cinfo, JERR_BAD_VIRTUAL_ACCESS);
    end;
  end;
  { Flag the buffer dirty if caller will write in it }
  if (writable) then
    ptr^.dirty := TRUE;
  { Return address of proper part of the buffer }
  access_virt_sarray := JSAMPARRAY(@ ptr^.mem_buffer^[start_row - ptr^.cur_start_row]);
end;


{METHODDEF}
function access_virt_barray (cinfo : j_common_ptr;
                             ptr : jvirt_barray_ptr;
		             start_row : JDIMENSION;
                             num_rows : JDIMENSION;
		             writable : boolean) : JBLOCKARRAY;
{ Access the part of a virtual block array starting at start_row }
{ and extending for num_rows rows.  writable is true if  }
{ caller intends to modify the accessed area. }
var
  end_row : JDIMENSION;
  undef_row : JDIMENSION;
  ltemp : long;
var
  bytesperrow : size_t;
begin
  end_row := start_row + num_rows;

  { debugging check }
  if (end_row > ptr^.rows_in_array) or (num_rows > ptr^.maxaccess) or
     (ptr^.mem_buffer = NIL) then
    ERREXIT(cinfo, JERR_BAD_VIRTUAL_ACCESS);

  { Make the desired part of the virtual array accessible }
  if (start_row < ptr^.cur_start_row) or
     (end_row > ptr^.cur_start_row+ptr^.rows_in_mem) then
  begin
    if (not ptr^.b_s_open) then
      ERREXIT(cinfo, JERR_VIRTUAL_BUG);
    { Flush old buffer contents if necessary }
    if (ptr^.dirty) then
    begin
      do_barray_io(cinfo, ptr, TRUE);
      ptr^.dirty := FALSE;
    end;
    { Decide what part of virtual array to access.
      Algorithm: if target address > current window, assume forward scan,
      load starting at target address.  If target address < current window,
      assume backward scan, load so that target area is top of window.
      Note that when switching from forward write to forward read, will have
      start_row := 0, so the limiting case applies and we load from 0 anyway. }

    if (start_row > ptr^.cur_start_row) then
    begin
      ptr^.cur_start_row := start_row;
    end
    else
    begin
      { use long arithmetic here to avoid overflow & unsigned problems }

      ltemp := long(end_row) - long(ptr^.rows_in_mem);
      if (ltemp < 0) then
	ltemp := 0;		{ don't fall off front end of file }
      ptr^.cur_start_row := JDIMENSION (ltemp);
    end;
    { Read in the selected part of the array.
      During the initial write pass, we will do no actual read
      because the selected part is all undefined. }

    do_barray_io(cinfo, ptr, FALSE);
  end;
  { Ensure the accessed part of the array is defined; prezero if needed.
    To improve locality of access, we only prezero the part of the array
    that the caller is about to access, not the entire in-memory array. }

  if (ptr^.first_undef_row < end_row) then
  begin
    if (ptr^.first_undef_row < start_row) then
    begin
      if (writable) then	{ writer skipped over a section of array }
	ERREXIT(cinfo, JERR_BAD_VIRTUAL_ACCESS);
      undef_row := start_row;	{ but reader is allowed to read ahead }
    end
    else
    begin
      undef_row := ptr^.first_undef_row;
    end;
    if (writable) then
      ptr^.first_undef_row := end_row;
    if (ptr^.pre_zero) then
    begin
      bytesperrow := size_t (ptr^.blocksperrow) * SIZEOF(JBLOCK);
      Dec(undef_row, ptr^.cur_start_row); { make indexes relative to buffer }
      Dec(end_row, ptr^.cur_start_row);
      while (undef_row < end_row) do
      begin
	jzero_far({FAR}pointer(ptr^.mem_buffer^[undef_row]), bytesperrow);
	Inc(undef_row);
      end;
    end
    else
    begin
      if (not writable) then	{ reader looking at undefined data }
	ERREXIT(cinfo, JERR_BAD_VIRTUAL_ACCESS);
    end;
  end;
  { Flag the buffer dirty if caller will write in it }
  if (writable) then
    ptr^.dirty := TRUE;
  { Return address of proper part of the buffer }
  access_virt_barray := JBLOCKARRAY(@ ptr^.mem_buffer^[start_row - ptr^.cur_start_row]);
end;


{ Release all objects belonging to a specified pool. }

{METHODDEF}
procedure free_pool (cinfo : j_common_ptr; pool_id : int);
var
  mem : my_mem_ptr;
  shdr_ptr : small_pool_ptr;
  lhdr_ptr : large_pool_ptr;
  space_freed : size_t;
var
  sptr : jvirt_sarray_ptr;
  bptr : jvirt_barray_ptr;
var
  next_lhdr_ptr : large_pool_ptr;
  next_shdr_ptr : small_pool_ptr;
begin
  mem := my_mem_ptr(cinfo^.mem);

  if (pool_id < 0) or (pool_id >= JPOOL_NUMPOOLS) then
    ERREXIT1(cinfo, JERR_BAD_POOL_ID, pool_id);	{ safety check }


  { If freeing IMAGE pool, close any virtual arrays first }
  if (pool_id = JPOOL_IMAGE) then
  begin
    sptr := mem^.virt_sarray_list;
    while (sptr <> NIL) do
    begin
      if (sptr^.b_s_open) then
      begin	{ there may be no backing store }
	sptr^.b_s_open := FALSE;	{ prevent recursive close if error }
	sptr^.b_s_info.close_backing_store (cinfo, @sptr^.b_s_info);
      end;
      sptr := sptr^.next;
    end;
    mem^.virt_sarray_list := NIL;
    bptr := mem^.virt_barray_list;
    while (bptr <> NIL) do
    begin
      if (bptr^.b_s_open) then
      begin	{ there may be no backing store }
	bptr^.b_s_open := FALSE;	{ prevent recursive close if error }
	bptr^.b_s_info.close_backing_store (cinfo, @bptr^.b_s_info);
      end;
      bptr := bptr^.next;
    end;
    mem^.virt_barray_list := NIL;
  end;

  { Release large objects }
  lhdr_ptr := mem^.large_list[pool_id];
  mem^.large_list[pool_id] := NIL;

  while (lhdr_ptr <> NIL) do
  begin
    next_lhdr_ptr := lhdr_ptr^.hdr.next;
    space_freed := lhdr_ptr^.hdr.bytes_used +
		  lhdr_ptr^.hdr.bytes_left +
		  SIZEOF(large_pool_hdr);
    jpeg_free_large(cinfo, {FAR} pointer(lhdr_ptr), space_freed);
    Dec(mem^.total_space_allocated, space_freed);
    lhdr_ptr := next_lhdr_ptr;
  end;

  { Release small objects }
  shdr_ptr := mem^.small_list[pool_id];
  mem^.small_list[pool_id] := NIL;

  while (shdr_ptr <> NIL) do
  begin
    next_shdr_ptr := shdr_ptr^.hdr.next;
    space_freed := shdr_ptr^.hdr.bytes_used +
		  shdr_ptr^.hdr.bytes_left +
		  SIZEOF(small_pool_hdr);
    jpeg_free_small(cinfo, pointer(shdr_ptr), space_freed);
    Dec(mem^.total_space_allocated, space_freed);
    shdr_ptr := next_shdr_ptr;
  end;
end;


{ Close up shop entirely.
  Note that this cannot be called unless cinfo^.mem is non-NIL. }

{METHODDEF}
procedure self_destruct (cinfo : j_common_ptr);
var
  pool : int;
begin
  { Close all backing store, release all memory.
    Releasing pools in reverse order might help avoid fragmentation
    with some (brain-damaged) malloc libraries. }

  for pool := JPOOL_NUMPOOLS-1 downto JPOOL_PERMANENT do
  begin
    free_pool(cinfo, pool);
  end;

  { Release the memory manager control block too. }
  jpeg_free_small(cinfo, pointer(cinfo^.mem), SIZEOF(my_memory_mgr));
  cinfo^.mem := NIL;		{ ensures I will be called only once }

  jpeg_mem_term(cinfo);		{ system-dependent cleanup }
end;


{ Memory manager initialization.
  When this is called, only the error manager pointer is valid in cinfo! }

{GLOBAL}
procedure jinit_memory_mgr (cinfo : j_common_ptr);
var
  mem : my_mem_ptr;
  max_to_use : long;
  pool : int;
  test_mac : size_t;
begin
  cinfo^.mem := NIL;		{ for safety if init fails }

  { Check for configuration errors.
    SIZEOF(ALIGN_TYPE) should be a power of 2; otherwise, it probably
    doesn't reflect any real hardware alignment requirement.
    The test is a little tricky: for X>0, X and X-1 have no one-bits
    in common if and only if X is a power of 2, ie has only one one-bit.
    Some compilers may give an "unreachable code" warning here; ignore it. }
  if ((SIZEOF(ALIGN_TYPE) and (SIZEOF(ALIGN_TYPE)-1)) <> 0) then
    ERREXIT(cinfo, JERR_BAD_ALIGN_TYPE);
  { MAX_ALLOC_CHUNK must be representable as type size_t, and must be
    a multiple of SIZEOF(ALIGN_TYPE).
    Again, an "unreachable code" warning may be ignored here.
    But a "constant too large" warning means you need to fix MAX_ALLOC_CHUNK. }

  test_mac := size_t (MAX_ALLOC_CHUNK);
  if (long (test_mac) <> MAX_ALLOC_CHUNK) or
      ((MAX_ALLOC_CHUNK mod SIZEOF(ALIGN_TYPE)) <> 0) then
    ERREXIT(cinfo, JERR_BAD_ALLOC_CHUNK);

  max_to_use := jpeg_mem_init(cinfo); { system-dependent initialization }

  { Attempt to allocate memory manager's control block }
  mem := my_mem_ptr (jpeg_get_small(cinfo, SIZEOF(my_memory_mgr)));

  if (mem = NIL) then
  begin
    jpeg_mem_term(cinfo);	{ system-dependent cleanup }
    ERREXIT1(cinfo, JERR_OUT_OF_MEMORY, 0);
  end;

  { OK, fill in the method pointers }
  mem^.pub.alloc_small := alloc_small;
  mem^.pub.alloc_large := alloc_large;
  mem^.pub.alloc_sarray := alloc_sarray;
  mem^.pub.alloc_barray := alloc_barray;
  mem^.pub.request_virt_sarray := request_virt_sarray;
  mem^.pub.request_virt_barray := request_virt_barray;
  mem^.pub.realize_virt_arrays := realize_virt_arrays;
  mem^.pub.access_virt_sarray := access_virt_sarray;
  mem^.pub.access_virt_barray := access_virt_barray;
  mem^.pub.free_pool := free_pool;
  mem^.pub.self_destruct := self_destruct;

  { Make MAX_ALLOC_CHUNK accessible to other modules }
  mem^.pub.max_alloc_chunk := MAX_ALLOC_CHUNK;

  { Initialize working state }
  mem^.pub.max_memory_to_use := max_to_use;

  for pool := JPOOL_NUMPOOLS-1 downto JPOOL_PERMANENT do
  begin
    mem^.small_list[pool] := NIL;
    mem^.large_list[pool] := NIL;
  end;
  mem^.virt_sarray_list := NIL;
  mem^.virt_barray_list := NIL;

  mem^.total_space_allocated := SIZEOF(my_memory_mgr);

  { Declare ourselves open for business }
  cinfo^.mem := @mem^.pub;


end;

{== imjdct implementation ==}


{== imjfdctint implementation ==}

{ This module is specialized to the case DCTSIZE = 8. }

{$ifndef DCTSIZE_IS_8}
  Sorry, this code only copes with 8x8 DCTs. { deliberate syntax err }
{$endif}


{ The poop on this scaling stuff is as follows:

  Each 1-D DCT step produces outputs which are a factor of sqrt(N)
  larger than the true DCT outputs.  The final outputs are therefore
  a factor of N larger than desired; since N=8 this can be cured by
  a simple right shift at the end of the algorithm.  The advantage of
  this arrangement is that we save two multiplications per 1-D DCT,
  because the y0 and y4 outputs need not be divided by sqrt(N).
  In the IJG code, this factor of 8 is removed by the quantization step
  (in jcdctmgr.c), NOT in this module.

  We have to do addition and subtraction of the integer inputs, which
  is no problem, and multiplication by fractional constants, which is
  a problem to do in integer arithmetic.  We multiply all the constants
  by CONST_SCALE and convert them to integer constants (thus retaining
  CONST_BITS bits of precision in the constants).  After doing a
  multiplication we have to divide the product by CONST_SCALE, with proper
  rounding, to produce the correct output.  This division can be done
  cheaply as a right shift of CONST_BITS bits.  We postpone shifting
  as long as possible so that partial sums can be added together with
  full fractional precision.

  The outputs of the first pass are scaled up by PASS1_BITS bits so that
  they are represented to better-than-integral precision.  These outputs
  require BITS_IN_JSAMPLE + PASS1_BITS + 3 bits; this fits in a 16-bit word
  with the recommended scaling.  (For 12-bit sample data, the intermediate
  array is INT32 anyway.)

  To avoid overflow of the 32-bit intermediate results in pass 2, we must
  have BITS_IN_JSAMPLE + CONST_BITS + PASS1_BITS <= 26.  Error analysis
  shows that the values given below are the most effective. }

{$ifdef BITS_IN_JSAMPLE_IS_8}
const
  jfdctint_CONST_BITS = 13;
  jfdctint_PASS1_BITS = 2;
{$else}
const
  jfdctint_CONST_BITS = 13;
  jfdctint_PASS1_BITS = 1;	{ lose a little precision to avoid overflow }
{$endif}

const
  jfdctint_CONST_SCALE = (INT32(1) shl jfdctint_CONST_BITS);

const
  jfdctint_FIX_0_298631336 = INT32(Round(jfdctint_CONST_SCALE * 0.298631336));  {2446}
  jfdctint_FIX_0_390180644 = INT32(Round(jfdctint_CONST_SCALE * 0.390180644));  {3196}
  jfdctint_FIX_0_541196100 = INT32(Round(jfdctint_CONST_SCALE * 0.541196100));  {4433}
  jfdctint_FIX_0_765366865 = INT32(Round(jfdctint_CONST_SCALE * 0.765366865));  {6270}
  jfdctint_FIX_0_899976223 = INT32(Round(jfdctint_CONST_SCALE * 0.899976223));  {7373}
  jfdctint_FIX_1_175875602 = INT32(Round(jfdctint_CONST_SCALE * 1.175875602));  {9633}
  jfdctint_FIX_1_501321110 = INT32(Round(jfdctint_CONST_SCALE * 1.501321110));  {12299}
  jfdctint_FIX_1_847759065 = INT32(Round(jfdctint_CONST_SCALE * 1.847759065));  {15137}
  jfdctint_FIX_1_961570560 = INT32(Round(jfdctint_CONST_SCALE * 1.961570560));  {16069}
  jfdctint_FIX_2_053119869 = INT32(Round(jfdctint_CONST_SCALE * 2.053119869));  {16819}
  jfdctint_FIX_2_562915447 = INT32(Round(jfdctint_CONST_SCALE * 2.562915447));  {20995}
  jfdctint_FIX_3_072711026 = INT32(Round(jfdctint_CONST_SCALE * 3.072711026));  {25172}


{ Multiply an INT32 variable by an INT32 constant to yield an INT32 result.
  For 8-bit samples with the recommended scaling, all the variable
  and constant values involved are no more than 16 bits wide, so a
  16x16->32 bit multiply can be used instead of a full 32x32 multiply.
  For 12-bit samples, a full 32-bit multiplication will be needed. }

{$ifdef BITS_IN_JSAMPLE_IS_8}

   {MULTIPLY16C16(var,const)}
   function jfdctint_Multiply(X, Y: int): INT32;
   begin
     jfdctint_Multiply := int(X) * INT32(Y);
   end;

{$else}
   function jfdctint_Multiply(X, Y: INT32): INT32;
   begin
     jfdctint_Multiply := X * Y;
   end;
{$endif}

{ Descale and correctly round an INT32 value that's scaled by N bits.
  We assume RIGHT_SHIFT rounds towards minus infinity, so adding
  the fudge factor is correct for either sign of X. }

function jfdctint_DESCALE(x : INT32; n : int) : INT32;
var
  shift_temp : INT32;
begin
{$ifdef RIGHT_SHIFT_IS_UNSIGNED}
  shift_temp := x + (INT32(1) shl (n-1));
  if shift_temp < 0 then
    jfdctint_Descale :=  (shift_temp shr n) or ((not INT32(0)) shl (32-n))
  else
    jfdctint_Descale :=  (shift_temp shr n);
{$else}
  jfdctint_Descale := (x + (INT32(1) shl (n-1)) shr n;
{$endif}
end;


{ Perform the forward DCT on one block of samples. }

{GLOBAL}
procedure jpeg_fdct_islow (var data : array of DCTELEM);
type
  PWorkspace = ^TWorkspace;
  TWorkspace = array [0..DCTSIZE2-1] of DCTELEM;
var
  tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7 : INT32;
  tmp10, tmp11, tmp12, tmp13 : INT32;
  z1, z2, z3, z4, z5 : INT32;
  dataptr : PWorkspace;
  ctr : int;
  {SHIFT_TEMPS}
begin

  { Pass 1: process rows. }
  { Note results are scaled up by sqrt(8) compared to a true DCT; }
  { furthermore, we scale the results by 2**PASS1_BITS. }

  dataptr := PWorkspace(@data);
  for ctr := DCTSIZE-1 downto 0 do
  begin
    tmp0 := dataptr^[0] + dataptr^[7];
    tmp7 := dataptr^[0] - dataptr^[7];
    tmp1 := dataptr^[1] + dataptr^[6];
    tmp6 := dataptr^[1] - dataptr^[6];
    tmp2 := dataptr^[2] + dataptr^[5];
    tmp5 := dataptr^[2] - dataptr^[5];
    tmp3 := dataptr^[3] + dataptr^[4];
    tmp4 := dataptr^[3] - dataptr^[4];

    { Even part per LL&M figure 1 --- note that published figure is faulty;
      rotator "sqrt(2)*c1" should be "sqrt(2)*c6".  }

    tmp10 := tmp0 + tmp3;
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    dataptr^[0] := DCTELEM ((tmp10 + tmp11) shl jfdctint_PASS1_BITS);
    dataptr^[4] := DCTELEM ((tmp10 - tmp11) shl jfdctint_PASS1_BITS);

    z1 := jfdctint_MULTIPLY(tmp12 + tmp13, jfdctint_FIX_0_541196100);
    dataptr^[2] := DCTELEM (jfdctint_DESCALE(z1 + jfdctint_MULTIPLY(tmp13, jfdctint_FIX_0_765366865),
				   jfdctint_CONST_BITS-jfdctint_PASS1_BITS));
    dataptr^[6] := DCTELEM (jfdctint_DESCALE(z1 + jfdctint_MULTIPLY(tmp12, - jfdctint_FIX_1_847759065),
				   jfdctint_CONST_BITS-jfdctint_PASS1_BITS));

    { Odd part per figure 8 --- note paper omits factor of sqrt(2).
      cK represents cos(K*pi/16).
      i0..i3 in the paper are tmp4..tmp7 here. }

    z1 := tmp4 + tmp7;
    z2 := tmp5 + tmp6;
    z3 := tmp4 + tmp6;
    z4 := tmp5 + tmp7;
    z5 := jfdctint_MULTIPLY(z3 + z4, jfdctint_FIX_1_175875602); { sqrt(2) * c3 }

    tmp4 := jfdctint_MULTIPLY(tmp4, jfdctint_FIX_0_298631336); { sqrt(2) * (-c1+c3+c5-c7) }
    tmp5 := jfdctint_MULTIPLY(tmp5, jfdctint_FIX_2_053119869); { sqrt(2) * ( c1+c3-c5+c7) }
    tmp6 := jfdctint_MULTIPLY(tmp6, jfdctint_FIX_3_072711026); { sqrt(2) * ( c1+c3+c5-c7) }
    tmp7 := jfdctint_MULTIPLY(tmp7, jfdctint_FIX_1_501321110); { sqrt(2) * ( c1+c3-c5-c7) }
    z1 := jfdctint_MULTIPLY(z1, - jfdctint_FIX_0_899976223); { sqrt(2) * (c7-c3) }
    z2 := jfdctint_MULTIPLY(z2, - jfdctint_FIX_2_562915447); { sqrt(2) * (-c1-c3) }
    z3 := jfdctint_MULTIPLY(z3, - jfdctint_FIX_1_961570560); { sqrt(2) * (-c3-c5) }
    z4 := jfdctint_MULTIPLY(z4, - jfdctint_FIX_0_390180644); { sqrt(2) * (c5-c3) }

    Inc(z3, z5);
    Inc(z4, z5);

    dataptr^[7] := DCTELEM(jfdctint_DESCALE(tmp4 + z1 + z3, jfdctint_CONST_BITS-jfdctint_PASS1_BITS));
    dataptr^[5] := DCTELEM(jfdctint_DESCALE(tmp5 + z2 + z4, jfdctint_CONST_BITS-jfdctint_PASS1_BITS));
    dataptr^[3] := DCTELEM(jfdctint_DESCALE(tmp6 + z2 + z3, jfdctint_CONST_BITS-jfdctint_PASS1_BITS));
    dataptr^[1] := DCTELEM(jfdctint_DESCALE(tmp7 + z1 + z4, jfdctint_CONST_BITS-jfdctint_PASS1_BITS));

    Inc(DCTELEMPTR(dataptr), DCTSIZE);	{ advance pointer to next row }
  end;

  { Pass 2: process columns.
    We remove the PASS1_BITS scaling, but leave the results scaled up
    by an overall factor of 8. }

  dataptr := PWorkspace(@data);
  for ctr := DCTSIZE-1 downto 0 do
  begin
    tmp0 := dataptr^[DCTSIZE*0] + dataptr^[DCTSIZE*7];
    tmp7 := dataptr^[DCTSIZE*0] - dataptr^[DCTSIZE*7];
    tmp1 := dataptr^[DCTSIZE*1] + dataptr^[DCTSIZE*6];
    tmp6 := dataptr^[DCTSIZE*1] - dataptr^[DCTSIZE*6];
    tmp2 := dataptr^[DCTSIZE*2] + dataptr^[DCTSIZE*5];
    tmp5 := dataptr^[DCTSIZE*2] - dataptr^[DCTSIZE*5];
    tmp3 := dataptr^[DCTSIZE*3] + dataptr^[DCTSIZE*4];
    tmp4 := dataptr^[DCTSIZE*3] - dataptr^[DCTSIZE*4];

    { Even part per LL&M figure 1 --- note that published figure is faulty;
      rotator "sqrt(2)*c1" should be "sqrt(2)*c6". }

    tmp10 := tmp0 + tmp3;
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    dataptr^[DCTSIZE*0] := DCTELEM (jfdctint_DESCALE(tmp10 + tmp11, jfdctint_PASS1_BITS));
    dataptr^[DCTSIZE*4] := DCTELEM (jfdctint_DESCALE(tmp10 - tmp11, jfdctint_PASS1_BITS));

    z1 := jfdctint_MULTIPLY(tmp12 + tmp13, jfdctint_FIX_0_541196100);
    dataptr^[DCTSIZE*2] := DCTELEM (jfdctint_DESCALE(z1 + jfdctint_MULTIPLY(tmp13, jfdctint_FIX_0_765366865),
					   jfdctint_CONST_BITS+jfdctint_PASS1_BITS));
    dataptr^[DCTSIZE*6] := DCTELEM (jfdctint_DESCALE(z1 + jfdctint_MULTIPLY(tmp12, - jfdctint_FIX_1_847759065),
					   jfdctint_CONST_BITS+jfdctint_PASS1_BITS));

    { Odd part per figure 8 --- note paper omits factor of sqrt(2).
      cK represents cos(K*pi/16).
      i0..i3 in the paper are tmp4..tmp7 here. }

    z1 := tmp4 + tmp7;
    z2 := tmp5 + tmp6;
    z3 := tmp4 + tmp6;
    z4 := tmp5 + tmp7;
    z5 := jfdctint_MULTIPLY(z3 + z4, jfdctint_FIX_1_175875602); { sqrt(2) * c3 }

    tmp4 := jfdctint_MULTIPLY(tmp4, jfdctint_FIX_0_298631336); { sqrt(2) * (-c1+c3+c5-c7) }
    tmp5 := jfdctint_MULTIPLY(tmp5, jfdctint_FIX_2_053119869); { sqrt(2) * ( c1+c3-c5+c7) }
    tmp6 := jfdctint_MULTIPLY(tmp6, jfdctint_FIX_3_072711026); { sqrt(2) * ( c1+c3+c5-c7) }
    tmp7 := jfdctint_MULTIPLY(tmp7, jfdctint_FIX_1_501321110); { sqrt(2) * ( c1+c3-c5-c7) }
    z1 := jfdctint_MULTIPLY(z1, - jfdctint_FIX_0_899976223); { sqrt(2) * (c7-c3) }
    z2 := jfdctint_MULTIPLY(z2, - jfdctint_FIX_2_562915447); { sqrt(2) * (-c1-c3) }
    z3 := jfdctint_MULTIPLY(z3, - jfdctint_FIX_1_961570560); { sqrt(2) * (-c3-c5) }
    z4 := jfdctint_MULTIPLY(z4, - jfdctint_FIX_0_390180644); { sqrt(2) * (c5-c3) }

    Inc(z3, z5);
    Inc(z4, z5);

    dataptr^[DCTSIZE*7] := DCTELEM (jfdctint_DESCALE(tmp4 + z1 + z3,
					   jfdctint_CONST_BITS+jfdctint_PASS1_BITS));
    dataptr^[DCTSIZE*5] := DCTELEM (jfdctint_DESCALE(tmp5 + z2 + z4,
					   jfdctint_CONST_BITS+jfdctint_PASS1_BITS));
    dataptr^[DCTSIZE*3] := DCTELEM (jfdctint_DESCALE(tmp6 + z2 + z3,
					   jfdctint_CONST_BITS+jfdctint_PASS1_BITS));
    dataptr^[DCTSIZE*1] := DCTELEM (jfdctint_DESCALE(tmp7 + z1 + z4,
					   jfdctint_CONST_BITS+jfdctint_PASS1_BITS));

    Inc(DCTELEMPTR(dataptr));	{ advance pointer to next column }
  end;
end;

{== imjfdctfst implementation ==}

{ This module is specialized to the case DCTSIZE = 8. }

{$ifndef DCTSIZE_IS_8}
  Sorry, this code only copes with 8x8 DCTs. { deliberate syntax err }
{$endif}


{ Scaling decisions are generally the same as in the LL&M algorithm;
  see jfdctint.c for more details.  However, we choose to descale
  (right shift) multiplication products as soon as they are formed,
  rather than carrying additional fractional bits into subsequent additions.
  This compromises accuracy slightly, but it lets us save a few shifts.
  More importantly, 16-bit arithmetic is then adequate (for 8-bit samples)
  everywhere except in the multiplications proper; this saves a good deal
  of work on 16-bit-int machines.

  Again to save a few shifts, the intermediate results between pass 1 and
  pass 2 are not upscaled, but are represented only to integral precision.

  A final compromise is to represent the multiplicative constants to only
  8 fractional bits, rather than 13.  This saves some shifting work on some
  machines, and may also reduce the cost of multiplication (since there
  are fewer one-bits in the constants). }

const
  jfdctfst_CONST_BITS = 8;
const
  jfdctfst_CONST_SCALE = (INT32(1) shl jfdctfst_CONST_BITS);


const
  FIX_0_382683433 = INT32(Round(jfdctfst_CONST_SCALE * 0.382683433)); {98}
  jfdctfst_FIX_0_541196100 = INT32(Round(jfdctfst_CONST_SCALE * 0.541196100)); {139}
  FIX_0_707106781 = INT32(Round(jfdctfst_CONST_SCALE * 0.707106781)); {181}
  FIX_1_306562965 = INT32(Round(jfdctfst_CONST_SCALE * 1.306562965)); {334}

{ Descale and correctly round an INT32 value that's scaled by N bits.
  We assume RIGHT_SHIFT rounds towards minus infinity, so adding
  the fudge factor is correct for either sign of X. }

function jfdctfst_DESCALE(x : INT32; n : int) : INT32;
var
  shift_temp : INT32;
begin
{ We can gain a little more speed, with a further compromise in accuracy,
  by omitting the addition in a descaling shift.  This yields an incorrectly
  rounded result half the time... }
{$ifndef USE_ACCURATE_ROUNDING}
  shift_temp := x;
{$else}
  shift_temp := x + (INT32(1) shl (n-1));
{$endif}

{$ifdef RIGHT_SHIFT_IS_UNSIGNED}
  if shift_temp < 0 then
    jfdctfst_Descale :=  (shift_temp shr n) or ((not INT32(0)) shl (32-n))
  else
{$endif}
    jfdctfst_Descale :=  (shift_temp shr n);
end;

{ Multiply a DCTELEM variable by an INT32 constant, and immediately
  descale to yield a DCTELEM result. }


   function jfdctfst_MULTIPLY(X : DCTELEM; Y: INT32): DCTELEM;
   begin
     jfdctfst_Multiply := jfdctfst_DeScale((X) * (Y), jfdctfst_CONST_BITS);
   end;


{ Perform the forward DCT on one block of samples. }

{GLOBAL}
procedure jpeg_fdct_ifast (var data : array of DCTELEM);
type
  PWorkspace = ^TWorkspace;
  TWorkspace = array [0..DCTSIZE2-1] of DCTELEM;
var
  tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7 : DCTELEM;
  tmp10, tmp11, tmp12, tmp13 : DCTELEM;
  z1, z2, z3, z4, z5, z11, z13 : DCTELEM;
  dataptr :  PWorkspace;
  ctr : int;
  {SHIFT_TEMPS}
begin
  { Pass 1: process rows. }

  dataptr := PWorkspace(@data);
  for ctr := DCTSIZE-1 downto 0 do
  begin
    tmp0 := dataptr^[0] + dataptr^[7];
    tmp7 := dataptr^[0] - dataptr^[7];
    tmp1 := dataptr^[1] + dataptr^[6];
    tmp6 := dataptr^[1] - dataptr^[6];
    tmp2 := dataptr^[2] + dataptr^[5];
    tmp5 := dataptr^[2] - dataptr^[5];
    tmp3 := dataptr^[3] + dataptr^[4];
    tmp4 := dataptr^[3] - dataptr^[4];

    { Even part }

    tmp10 := tmp0 + tmp3;	{ phase 2 }
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    dataptr^[0] := tmp10 + tmp11; { phase 3 }
    dataptr^[4] := tmp10 - tmp11;

    z1 := jfdctfst_MULTIPLY(tmp12 + tmp13, FIX_0_707106781); { c4 }
    dataptr^[2] := tmp13 + z1;	{ phase 5 }
    dataptr^[6] := tmp13 - z1;

    { Odd part }

    tmp10 := tmp4 + tmp5;	{ phase 2 }
    tmp11 := tmp5 + tmp6;
    tmp12 := tmp6 + tmp7;

    { The rotator is modified from fig 4-8 to avoid extra negations. }
    z5 := jfdctfst_MULTIPLY(tmp10 - tmp12, FIX_0_382683433); { c6 }
    z2 := jfdctfst_MULTIPLY(tmp10, jfdctfst_FIX_0_541196100) + z5; { c2-c6 }
    z4 := jfdctfst_MULTIPLY(tmp12, FIX_1_306562965) + z5; { c2+c6 }
    z3 := jfdctfst_MULTIPLY(tmp11, FIX_0_707106781); { c4 }

    z11 := tmp7 + z3;		{ phase 5 }
    z13 := tmp7 - z3;

    dataptr^[5] := z13 + z2;	{ phase 6 }
    dataptr^[3] := z13 - z2;
    dataptr^[1] := z11 + z4;
    dataptr^[7] := z11 - z4;

    Inc(DCTELEMPTR(dataptr), DCTSIZE);	{ advance pointer to next row }
  end;

  { Pass 2: process columns. }

  dataptr := PWorkspace(@data);
  for ctr := DCTSIZE-1 downto 0 do
  begin
    tmp0 := dataptr^[DCTSIZE*0] + dataptr^[DCTSIZE*7];
    tmp7 := dataptr^[DCTSIZE*0] - dataptr^[DCTSIZE*7];
    tmp1 := dataptr^[DCTSIZE*1] + dataptr^[DCTSIZE*6];
    tmp6 := dataptr^[DCTSIZE*1] - dataptr^[DCTSIZE*6];
    tmp2 := dataptr^[DCTSIZE*2] + dataptr^[DCTSIZE*5];
    tmp5 := dataptr^[DCTSIZE*2] - dataptr^[DCTSIZE*5];
    tmp3 := dataptr^[DCTSIZE*3] + dataptr^[DCTSIZE*4];
    tmp4 := dataptr^[DCTSIZE*3] - dataptr^[DCTSIZE*4];

    { Even part }

    tmp10 := tmp0 + tmp3;	{ phase 2 }
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    dataptr^[DCTSIZE*0] := tmp10 + tmp11; { phase 3 }
    dataptr^[DCTSIZE*4] := tmp10 - tmp11;

    z1 := jfdctfst_MULTIPLY(tmp12 + tmp13, FIX_0_707106781); { c4 }
    dataptr^[DCTSIZE*2] := tmp13 + z1; { phase 5 }
    dataptr^[DCTSIZE*6] := tmp13 - z1;

    { Odd part }

    tmp10 := tmp4 + tmp5;	{ phase 2 }
    tmp11 := tmp5 + tmp6;
    tmp12 := tmp6 + tmp7;

    { The rotator is modified from fig 4-8 to avoid extra negations. }
    z5 := jfdctfst_MULTIPLY(tmp10 - tmp12, FIX_0_382683433); { c6 }
    z2 := jfdctfst_MULTIPLY(tmp10, jfdctfst_FIX_0_541196100) + z5; { c2-c6 }
    z4 := jfdctfst_MULTIPLY(tmp12, FIX_1_306562965) + z5; { c2+c6 }
    z3 := jfdctfst_MULTIPLY(tmp11, FIX_0_707106781); { c4 }

    z11 := tmp7 + z3;		{ phase 5 }
    z13 := tmp7 - z3;

    dataptr^[DCTSIZE*5] := z13 + z2; { phase 6 }
    dataptr^[DCTSIZE*3] := z13 - z2;
    dataptr^[DCTSIZE*1] := z11 + z4;
    dataptr^[DCTSIZE*7] := z11 - z4;

    Inc(DCTELEMPTR(dataptr));	{ advance pointer to next column }
  end;
end;

{== imjfdctflt implementation ==}

{ This module is specialized to the case DCTSIZE = 8. }

{$ifndef DCTSIZE_IS_8}
  Sorry, this code only copes with 8x8 DCTs. { deliberate syntax err }
{$endif}


{ Perform the forward DCT on one block of samples.}

{GLOBAL}
procedure jpeg_fdct_float (var data : array of FAST_FLOAT);
type
  PWorkspace = ^TWorkspace;
  TWorkspace = array [0..DCTSIZE2-1] of FAST_FLOAT;
var
  tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7 : FAST_FLOAT;
  tmp10, tmp11, tmp12, tmp13 : FAST_FLOAT;
  z1, z2, z3, z4, z5, z11, z13 : FAST_FLOAT;
  dataptr : PWorkspace;
  ctr : int;
begin
  { Pass 1: process rows. }

  dataptr := PWorkspace(@data);
  for ctr := DCTSIZE-1 downto 0 do
  begin
    tmp0 := dataptr^[0] + dataptr^[7];
    tmp7 := dataptr^[0] - dataptr^[7];
    tmp1 := dataptr^[1] + dataptr^[6];
    tmp6 := dataptr^[1] - dataptr^[6];
    tmp2 := dataptr^[2] + dataptr^[5];
    tmp5 := dataptr^[2] - dataptr^[5];
    tmp3 := dataptr^[3] + dataptr^[4];
    tmp4 := dataptr^[3] - dataptr^[4];

    { Even part }

    tmp10 := tmp0 + tmp3;	{ phase 2 }
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    dataptr^[0] := tmp10 + tmp11; { phase 3 }
    dataptr^[4] := tmp10 - tmp11;

    z1 := (tmp12 + tmp13) * ({FAST_FLOAT}(0.707106781)); { c4 }
    dataptr^[2] := tmp13 + z1;	{ phase 5 }
    dataptr^[6] := tmp13 - z1;

    { Odd part }

    tmp10 := tmp4 + tmp5;	{ phase 2 }
    tmp11 := tmp5 + tmp6;
    tmp12 := tmp6 + tmp7;

    { The rotator is modified from fig 4-8 to avoid extra negations. }
    z5 := (tmp10 - tmp12) * ( {FAST_FLOAT}(0.382683433)); { c6 }
    z2 := {FAST_FLOAT}(0.541196100) * tmp10 + z5; { c2-c6 }
    z4 := {FAST_FLOAT}(1.306562965) * tmp12 + z5; { c2+c6 }
    z3 := tmp11 * {FAST_FLOAT} (0.707106781); { c4 }

    z11 := tmp7 + z3;		{ phase 5 }
    z13 := tmp7 - z3;

    dataptr^[5] := z13 + z2;	{ phase 6 }
    dataptr^[3] := z13 - z2;
    dataptr^[1] := z11 + z4;
    dataptr^[7] := z11 - z4;

    Inc(FAST_FLOAT_PTR(dataptr), DCTSIZE);  { advance pointer to next row }
  end;

  { Pass 2: process columns. }

  dataptr := PWorkspace(@data);
  for ctr := DCTSIZE-1 downto 0 do
  begin
    tmp0 := dataptr^[DCTSIZE*0] + dataptr^[DCTSIZE*7];
    tmp7 := dataptr^[DCTSIZE*0] - dataptr^[DCTSIZE*7];
    tmp1 := dataptr^[DCTSIZE*1] + dataptr^[DCTSIZE*6];
    tmp6 := dataptr^[DCTSIZE*1] - dataptr^[DCTSIZE*6];
    tmp2 := dataptr^[DCTSIZE*2] + dataptr^[DCTSIZE*5];
    tmp5 := dataptr^[DCTSIZE*2] - dataptr^[DCTSIZE*5];
    tmp3 := dataptr^[DCTSIZE*3] + dataptr^[DCTSIZE*4];
    tmp4 := dataptr^[DCTSIZE*3] - dataptr^[DCTSIZE*4];

    { Even part }

    tmp10 := tmp0 + tmp3;	{ phase 2 }
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    dataptr^[DCTSIZE*0] := tmp10 + tmp11; { phase 3 }
    dataptr^[DCTSIZE*4] := tmp10 - tmp11;

    z1 := (tmp12 + tmp13) * {FAST_FLOAT} (0.707106781); { c4 }
    dataptr^[DCTSIZE*2] := tmp13 + z1; { phase 5 }
    dataptr^[DCTSIZE*6] := tmp13 - z1;

    { Odd part }

    tmp10 := tmp4 + tmp5;	{ phase 2 }
    tmp11 := tmp5 + tmp6;
    tmp12 := tmp6 + tmp7;

    { The rotator is modified from fig 4-8 to avoid extra negations. }
    z5 := (tmp10 - tmp12) * {FAST_FLOAT} (0.382683433); { c6 }
    z2 := {FAST_FLOAT} (0.541196100) * tmp10 + z5; { c2-c6 }
    z4 := {FAST_FLOAT} (1.306562965) * tmp12 + z5; { c2+c6 }
    z3 := tmp11 * {FAST_FLOAT} (0.707106781); { c4 }

    z11 := tmp7 + z3;		{ phase 5 }
    z13 := tmp7 - z3;

    dataptr^[DCTSIZE*5] := z13 + z2; { phase 6 }
    dataptr^[DCTSIZE*3] := z13 - z2;
    dataptr^[DCTSIZE*1] := z11 + z4;
    dataptr^[DCTSIZE*7] := z11 - z4;

    Inc(FAST_FLOAT_PTR(dataptr));   { advance pointer to next column }
  end;
end;

{== imjidctint implementation ==}

{$IFNDEF JPEG_IDCT_ASM}

{ This module is specialized to the case DCTSIZE = 8. }

{$ifndef DCTSIZE_IS_8}
  Sorry, this code only copes with 8x8 DCTs. { deliberate syntax err }
{$endif}

{ The poop on this scaling stuff is as follows:

  Each 1-D IDCT step produces outputs which are a factor of sqrt(N)
  larger than the true IDCT outputs.  The final outputs are therefore
  a factor of N larger than desired; since N=8 this can be cured by
  a simple right shift at the end of the algorithm.  The advantage of
  this arrangement is that we save two multiplications per 1-D IDCT,
  because the y0 and y4 inputs need not be divided by sqrt(N).

  We have to do addition and subtraction of the integer inputs, which
  is no problem, and multiplication by fractional constants, which is
  a problem to do in integer arithmetic.  We multiply all the constants
  by CONST_SCALE and convert them to integer constants (thus retaining
  CONST_BITS bits of precision in the constants).  After doing a
  multiplication we have to divide the product by CONST_SCALE, with proper
  rounding, to produce the correct output.  This division can be done
  cheaply as a right shift of CONST_BITS bits.  We postpone shifting
  as long as possible so that partial sums can be added together with
  full fractional precision.

  The outputs of the first pass are scaled up by PASS1_BITS bits so that
  they are represented to better-than-integral precision.  These outputs
  require BITS_IN_JSAMPLE + PASS1_BITS + 3 bits; this fits in a 16-bit word
  with the recommended scaling.  (To scale up 12-bit sample data further, an
  intermediate INT32 array would be needed.)

  To avoid overflow of the 32-bit intermediate results in pass 2, we must
  have BITS_IN_JSAMPLE + CONST_BITS + PASS1_BITS <= 26.  Error analysis
  shows that the values given below are the most effective. }

{$ifdef BITS_IN_JSAMPLE_IS_8}
const
  jidctint_CONST_BITS = 13;
  jidctint_PASS1_BITS = 2;
{$else}
const
  jidctint_CONST_BITS = 13;
  jidctint_PASS1_BITS = 1;	{ lose a little precision to avoid overflow }
{$endif}

const
  jidctint_CONST_SCALE = (INT32(1) shl jidctint_CONST_BITS);

const
  jidctint_FIX_0_298631336 = INT32(Round(jidctint_CONST_SCALE * 0.298631336));  {2446}
  jidctint_FIX_0_390180644 = INT32(Round(jidctint_CONST_SCALE * 0.390180644));  {3196}
  jidctint_FIX_0_541196100 = INT32(Round(jidctint_CONST_SCALE * 0.541196100));  {4433}
  jidctint_FIX_0_765366865 = INT32(Round(jidctint_CONST_SCALE * 0.765366865));  {6270}
  jidctint_FIX_0_899976223 = INT32(Round(jidctint_CONST_SCALE * 0.899976223));  {7373}
  jidctint_FIX_1_175875602 = INT32(Round(jidctint_CONST_SCALE * 1.175875602));  {9633}
  jidctint_FIX_1_501321110 = INT32(Round(jidctint_CONST_SCALE * 1.501321110));  {12299}
  jidctint_FIX_1_847759065 = INT32(Round(jidctint_CONST_SCALE * 1.847759065));  {15137}
  jidctint_FIX_1_961570560 = INT32(Round(jidctint_CONST_SCALE * 1.961570560));  {16069}
  jidctint_FIX_2_053119869 = INT32(Round(jidctint_CONST_SCALE * 2.053119869));  {16819}
  jidctint_FIX_2_562915447 = INT32(Round(jidctint_CONST_SCALE * 2.562915447));  {20995}
  jidctint_FIX_3_072711026 = INT32(Round(jidctint_CONST_SCALE * 3.072711026));  {25172}


{ Multiply an INT32 variable by an INT32 constant to yield an INT32 result.
  For 8-bit samples with the recommended scaling, all the variable
  and constant values involved are no more than 16 bits wide, so a
  16x16->32 bit multiply can be used instead of a full 32x32 multiply.
  For 12-bit samples, a full 32-bit multiplication will be needed. }

{$ifdef BITS_IN_JSAMPLE_IS_8}


   function jidctint_Multiply(X, Y: INT32): INT32;
   begin
     jidctint_Multiply := INT32(X) * INT32(Y);
   end;


{$else}
  {#define MULTIPLY(var,const)  ((var) * (const))}
   function jidctint_Multiply(X, Y: INT32): INT32;
   begin
     jidctint_Multiply := INT32(X) * INT32(Y);
   end;
{$endif}


{ Dequantize a coefficient by multiplying it by the multiplier-table
  entry; produce an int result.  In this module, both inputs and result
  are 16 bits or less, so either int or short multiply will work. }

function jidctint_DEQUANTIZE(coef,quantval : int) : int;
begin
  jidctint_Dequantize := ( ISLOW_MULT_TYPE(coef) * quantval);
end;

{ Descale and correctly round an INT32 value that's scaled by N bits.
  We assume RIGHT_SHIFT rounds towards minus infinity, so adding
  the fudge factor is correct for either sign of X. }

function jidctint_DESCALE(x : INT32; n : int) : INT32;
var
  shift_temp : INT32;
begin
{$ifdef RIGHT_SHIFT_IS_UNSIGNED}
  shift_temp := x + (INT32(1) shl (n-1));
  if shift_temp < 0 then
    jidctint_Descale :=  (shift_temp shr n) or ((not INT32(0)) shl (32-n))
  else
    jidctint_Descale :=  (shift_temp shr n);
{$else}
  jidctint_Descale := (x + (INT32(1) shl (n-1)) shr n;
{$endif}
end;

{ Perform dequantization and inverse DCT on one block of coefficients. }

{GLOBAL}
procedure jpeg_idct_islow (cinfo : j_decompress_ptr;
                           compptr : jpeg_component_info_ptr;
		           coef_block : JCOEFPTR;
		           output_buf : JSAMPARRAY;
                           output_col : JDIMENSION);
type
  PWorkspace = ^TWorkspace;
  TWorkspace = coef_bits_field; { buffers data between passes }
var
  tmp0, tmp1, tmp2, tmp3 : INT32;
  tmp10, tmp11, tmp12, tmp13 : INT32;
  z1, z2, z3, z4, z5 : INT32;
  inptr : JCOEFPTR;
  quantptr : ISLOW_MULT_TYPE_FIELD_PTR;
  wsptr : PWorkspace;
  outptr : JSAMPROW;
  range_limit : JSAMPROW;
  ctr : int;
  workspace : TWorkspace;
  {SHIFT_TEMPS}
var
  dcval : int;
var
  dcval_ : JSAMPLE;
begin
{ Each IDCT routine is responsible for range-limiting its results and
  converting them to unsigned form (0..MAXJSAMPLE).  The raw outputs could
  be quite far out of range if the input data is corrupt, so a bulletproof
  range-limiting step is required.  We use a mask-and-table-lookup method
  to do the combined operations quickly.  See the comments with
  prepare_range_limit_table (in jdmaster.c) for more info. }

  range_limit := JSAMPROW(@(cinfo^.sample_range_limit^[CENTERJSAMPLE]));


  { Pass 1: process columns from input, store into work array. }
  { Note results are scaled up by sqrt(8) compared to a true IDCT; }
  { furthermore, we scale the results by 2**PASS1_BITS. }

  inptr := coef_block;
  quantptr := ISLOW_MULT_TYPE_FIELD_PTR (compptr^.dct_table);
  wsptr := PWorkspace(@workspace);
  for ctr := pred(DCTSIZE) downto 0 do
  begin
    { Due to quantization, we will usually find that many of the input
      coefficients are zero, especially the AC terms.  We can exploit this
      by short-circuiting the IDCT calculation for any column in which all
      the AC terms are zero.  In that case each output is equal to the
      DC coefficient (with scale factor as needed).
      With typical images and quantization tables, half or more of the
      column DCT calculations can be simplified this way. }

    if ((inptr^[DCTSIZE*1]=0) and (inptr^[DCTSIZE*2]=0) and
        (inptr^[DCTSIZE*3]=0) and (inptr^[DCTSIZE*4]=0) and
        (inptr^[DCTSIZE*5]=0) and (inptr^[DCTSIZE*6]=0) and
	(inptr^[DCTSIZE*7]=0)) then
    begin
      { AC terms all zero }
      dcval := jidctint_DEQUANTIZE(inptr^[DCTSIZE*0], quantptr^[DCTSIZE*0]) shl jidctint_PASS1_BITS;

      wsptr^[DCTSIZE*0] := dcval;
      wsptr^[DCTSIZE*1] := dcval;
      wsptr^[DCTSIZE*2] := dcval;
      wsptr^[DCTSIZE*3] := dcval;
      wsptr^[DCTSIZE*4] := dcval;
      wsptr^[DCTSIZE*5] := dcval;
      wsptr^[DCTSIZE*6] := dcval;
      wsptr^[DCTSIZE*7] := dcval;

      Inc(JCOEF_PTR(inptr));		{ advance pointers to next column }
      Inc(ISLOW_MULT_TYPE_PTR(quantptr));
      Inc(int_ptr(wsptr));
      continue;
    end;

    { Even part: reverse the even part of the forward DCT. }
    { The rotator is sqrt(2)*c(-6). }

    z2 := jidctint_DEQUANTIZE(inptr^[DCTSIZE*2], quantptr^[DCTSIZE*2]);
    z3 := jidctint_DEQUANTIZE(inptr^[DCTSIZE*6], quantptr^[DCTSIZE*6]);

    z1 := jidctint_MULTIPLY(z2 + z3, jidctint_FIX_0_541196100);
    tmp2 := z1 + jidctint_MULTIPLY(z3, - jidctint_FIX_1_847759065);
    tmp3 := z1 + jidctint_MULTIPLY(z2, jidctint_FIX_0_765366865);

    z2 := jidctint_DEQUANTIZE(inptr^[DCTSIZE*0], quantptr^[DCTSIZE*0]);
    z3 := jidctint_DEQUANTIZE(inptr^[DCTSIZE*4], quantptr^[DCTSIZE*4]);

    tmp0 := (z2 + z3) shl jidctint_CONST_BITS;
    tmp1 := (z2 - z3) shl jidctint_CONST_BITS;

    tmp10 := tmp0 + tmp3;
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    { Odd part per figure 8; the matrix is unitary and hence its
      transpose is its inverse.  i0..i3 are y7,y5,y3,y1 respectively. }

    tmp0 := jidctint_DEQUANTIZE(inptr^[DCTSIZE*7], quantptr^[DCTSIZE*7]);
    tmp1 := jidctint_DEQUANTIZE(inptr^[DCTSIZE*5], quantptr^[DCTSIZE*5]);
    tmp2 := jidctint_DEQUANTIZE(inptr^[DCTSIZE*3], quantptr^[DCTSIZE*3]);
    tmp3 := jidctint_DEQUANTIZE(inptr^[DCTSIZE*1], quantptr^[DCTSIZE*1]);

    z1 := tmp0 + tmp3;
    z2 := tmp1 + tmp2;
    z3 := tmp0 + tmp2;
    z4 := tmp1 + tmp3;
    z5 := jidctint_MULTIPLY(z3 + z4, jidctint_FIX_1_175875602); { sqrt(2) * c3 }

    tmp0 := jidctint_MULTIPLY(tmp0, jidctint_FIX_0_298631336); { sqrt(2) * (-c1+c3+c5-c7) }
    tmp1 := jidctint_MULTIPLY(tmp1, jidctint_FIX_2_053119869); { sqrt(2) * ( c1+c3-c5+c7) }
    tmp2 := jidctint_MULTIPLY(tmp2, jidctint_FIX_3_072711026); { sqrt(2) * ( c1+c3+c5-c7) }
    tmp3 := jidctint_MULTIPLY(tmp3, jidctint_FIX_1_501321110); { sqrt(2) * ( c1+c3-c5-c7) }
    z1 := jidctint_MULTIPLY(z1, - jidctint_FIX_0_899976223); { sqrt(2) * (c7-c3) }
    z2 := jidctint_MULTIPLY(z2, - jidctint_FIX_2_562915447); { sqrt(2) * (-c1-c3) }
    z3 := jidctint_MULTIPLY(z3, - jidctint_FIX_1_961570560); { sqrt(2) * (-c3-c5) }
    z4 := jidctint_MULTIPLY(z4, - jidctint_FIX_0_390180644); { sqrt(2) * (c5-c3) }

    Inc(z3, z5);
    Inc(z4, z5);

    Inc(tmp0, z1 + z3);
    Inc(tmp1, z2 + z4);
    Inc(tmp2, z2 + z3);
    Inc(tmp3, z1 + z4);

    { Final output stage: inputs are tmp10..tmp13, tmp0..tmp3 }

    wsptr^[DCTSIZE*0] := int (jidctint_DESCALE(tmp10 + tmp3, jidctint_CONST_BITS-jidctint_PASS1_BITS));
    wsptr^[DCTSIZE*7] := int (jidctint_DESCALE(tmp10 - tmp3, jidctint_CONST_BITS-jidctint_PASS1_BITS));
    wsptr^[DCTSIZE*1] := int (jidctint_DESCALE(tmp11 + tmp2, jidctint_CONST_BITS-jidctint_PASS1_BITS));
    wsptr^[DCTSIZE*6] := int (jidctint_DESCALE(tmp11 - tmp2, jidctint_CONST_BITS-jidctint_PASS1_BITS));
    wsptr^[DCTSIZE*2] := int (jidctint_DESCALE(tmp12 + tmp1, jidctint_CONST_BITS-jidctint_PASS1_BITS));
    wsptr^[DCTSIZE*5] := int (jidctint_DESCALE(tmp12 - tmp1, jidctint_CONST_BITS-jidctint_PASS1_BITS));
    wsptr^[DCTSIZE*3] := int (jidctint_DESCALE(tmp13 + tmp0, jidctint_CONST_BITS-jidctint_PASS1_BITS));
    wsptr^[DCTSIZE*4] := int (jidctint_DESCALE(tmp13 - tmp0, jidctint_CONST_BITS-jidctint_PASS1_BITS));

    Inc(JCOEF_PTR(inptr));		{ advance pointers to next column }
    Inc(ISLOW_MULT_TYPE_PTR(quantptr));
    Inc(int_ptr(wsptr));
  end;

  { Pass 2: process rows from work array, store into output array. }
  { Note that we must descale the results by a factor of 8 == 2**3, }
  { and also undo the PASS1_BITS scaling. }

  wsptr := @workspace;
  for ctr := 0 to pred(DCTSIZE) do
  begin
    outptr := output_buf^[ctr];
    Inc(JSAMPLE_PTR(outptr), output_col);
    { Rows of zeroes can be exploited in the same way as we did with columns.
      However, the column calculation has created many nonzero AC terms, so
      the simplification applies less often (typically 5% to 10% of the time).
      On machines with very fast multiplication, it's possible that the
      test takes more time than it's worth.  In that case this section
      may be commented out. }

{$ifndef NO_ZERO_ROW_TEST}
    if ((wsptr^[1]=0) and (wsptr^[2]=0) and (wsptr^[3]=0) and (wsptr^[4]=0)
       and (wsptr^[5]=0) and (wsptr^[6]=0) and (wsptr^[7]=0)) then
    begin
      { AC terms all zero }
      JSAMPLE(dcval_) := range_limit^[int(jidctint_DESCALE(INT32(wsptr^[0]),
                          jidctint_PASS1_BITS+3)) and RANGE_MASK];

      outptr^[0] := dcval_;
      outptr^[1] := dcval_;
      outptr^[2] := dcval_;
      outptr^[3] := dcval_;
      outptr^[4] := dcval_;
      outptr^[5] := dcval_;
      outptr^[6] := dcval_;
      outptr^[7] := dcval_;

      Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
      continue;
    end;
{$endif}

    { Even part: reverse the even part of the forward DCT. }
    { The rotator is sqrt(2)*c(-6). }

    z2 := INT32 (wsptr^[2]);
    z3 := INT32 (wsptr^[6]);

    z1 := jidctint_MULTIPLY(z2 + z3, jidctint_FIX_0_541196100);
    tmp2 := z1 + jidctint_MULTIPLY(z3, - jidctint_FIX_1_847759065);
    tmp3 := z1 + jidctint_MULTIPLY(z2, jidctint_FIX_0_765366865);

    tmp0 := (INT32(wsptr^[0]) + INT32(wsptr^[4])) shl jidctint_CONST_BITS;
    tmp1 := (INT32(wsptr^[0]) - INT32(wsptr^[4])) shl jidctint_CONST_BITS;

    tmp10 := tmp0 + tmp3;
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    { Odd part per figure 8; the matrix is unitary and hence its
      transpose is its inverse.  i0..i3 are y7,y5,y3,y1 respectively. }

    tmp0 := INT32(wsptr^[7]);
    tmp1 := INT32(wsptr^[5]);
    tmp2 := INT32(wsptr^[3]);
    tmp3 := INT32(wsptr^[1]);

    z1 := tmp0 + tmp3;
    z2 := tmp1 + tmp2;
    z3 := tmp0 + tmp2;
    z4 := tmp1 + tmp3;
    z5 := jidctint_MULTIPLY(z3 + z4, jidctint_FIX_1_175875602); { sqrt(2) * c3 }

    tmp0 := jidctint_MULTIPLY(tmp0, jidctint_FIX_0_298631336); { sqrt(2) * (-c1+c3+c5-c7) }
    tmp1 := jidctint_MULTIPLY(tmp1, jidctint_FIX_2_053119869); { sqrt(2) * ( c1+c3-c5+c7) }
    tmp2 := jidctint_MULTIPLY(tmp2, jidctint_FIX_3_072711026); { sqrt(2) * ( c1+c3+c5-c7) }
    tmp3 := jidctint_MULTIPLY(tmp3, jidctint_FIX_1_501321110); { sqrt(2) * ( c1+c3-c5-c7) }
    z1 := jidctint_MULTIPLY(z1, - jidctint_FIX_0_899976223); { sqrt(2) * (c7-c3) }
    z2 := jidctint_MULTIPLY(z2, - jidctint_FIX_2_562915447); { sqrt(2) * (-c1-c3) }
    z3 := jidctint_MULTIPLY(z3, - jidctint_FIX_1_961570560); { sqrt(2) * (-c3-c5) }
    z4 := jidctint_MULTIPLY(z4, - jidctint_FIX_0_390180644); { sqrt(2) * (c5-c3) }

    Inc(z3, z5);
    Inc(z4, z5);

    Inc(tmp0, z1 + z3);
    Inc(tmp1, z2 + z4);
    Inc(tmp2, z2 + z3);
    Inc(tmp3, z1 + z4);

    { Final output stage: inputs are tmp10..tmp13, tmp0..tmp3 }

    outptr^[0] := range_limit^[ int(jidctint_DESCALE(tmp10 + tmp3,
					  jidctint_CONST_BITS+jidctint_PASS1_BITS+3))
			    and RANGE_MASK];
    outptr^[7] := range_limit^[ int(jidctint_DESCALE(tmp10 - tmp3,
					  jidctint_CONST_BITS+jidctint_PASS1_BITS+3))
			    and RANGE_MASK];
    outptr^[1] := range_limit^[ int(jidctint_DESCALE(tmp11 + tmp2,
					  jidctint_CONST_BITS+jidctint_PASS1_BITS+3))
			    and RANGE_MASK];
    outptr^[6] := range_limit^[ int(jidctint_DESCALE(tmp11 - tmp2,
                                          jidctint_CONST_BITS+jidctint_PASS1_BITS+3))
                            and RANGE_MASK];
    outptr^[2] := range_limit^[ int(jidctint_DESCALE(tmp12 + tmp1,
                                          jidctint_CONST_BITS+jidctint_PASS1_BITS+3))
                            and RANGE_MASK];
    outptr^[5] := range_limit^[ int(jidctint_DESCALE(tmp12 - tmp1,
                                          jidctint_CONST_BITS+jidctint_PASS1_BITS+3))
                            and RANGE_MASK];
    outptr^[3] := range_limit^[ int(jidctint_DESCALE(tmp13 + tmp0,
                                          jidctint_CONST_BITS+jidctint_PASS1_BITS+3))
                            and RANGE_MASK];
    outptr^[4] := range_limit^[ int(jidctint_DESCALE(tmp13 - tmp0,
                                          jidctint_CONST_BITS+jidctint_PASS1_BITS+3))
                            and RANGE_MASK];

    Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
  end;
end;

{$ENDIF JPEG_IDCT_ASM}

{== imjidctasm implementation ==}

{$IFDEF JPEG_IDCT_ASM}

{ This module is specialized to the case DCTSIZE = 8. }

{$ifndef DCTSIZE_IS_8}
  Sorry, this code only copes with 8x8 DCTs. { deliberate syntax err }
{$endif}

{ The poop on this scaling stuff is as follows:

  Each 1-D IDCT step produces outputs which are a factor of sqrt(N)
  larger than the true IDCT outputs.  The final outputs are therefore
  a factor of N larger than desired; since N=8 this can be cured by
  a simple right shift at the end of the algorithm.  The advantage of
  this arrangement is that we save two multiplications per 1-D IDCT,
  because the y0 and y4 inputs need not be divided by sqrt(N).

  We have to do addition and subtraction of the integer inputs, which
  is no problem, and multiplication by fractional constants, which is
  a problem to do in integer arithmetic.  We multiply all the constants
  by CONST_SCALE and convert them to integer constants (thus retaining
  CONST_BITS bits of precision in the constants).  After doing a
  multiplication we have to divide the product by CONST_SCALE, with proper
  rounding, to produce the correct output.  This division can be done
  cheaply as a right shift of CONST_BITS bits.  We postpone shifting
  as long as possible so that partial sums can be added together with
  full fractional precision.

  The outputs of the first pass are scaled up by PASS1_BITS bits so that
  they are represented to better-than-integral precision.  These outputs
  require BITS_IN_JSAMPLE + PASS1_BITS + 3 bits; this fits in a 16-bit word
  with the recommended scaling.  (To scale up 12-bit sample data further, an
  intermediate INT32 array would be needed.)

  To avoid overflow of the 32-bit intermediate results in pass 2, we must
  have BITS_IN_JSAMPLE + CONST_BITS + PASS1_BITS <= 26.  Error analysis
  shows that the values given below are the most effective. }

const
  jidctasm_CONST_BITS = 13;

{$ifdef BITS_IN_JSAMPLE_IS_8}
const
  jidctasm_PASS1_BITS = 2;
{$else}
const
  jidctasm_PASS1_BITS = 1;	{ lose a little precision to avoid overflow }
{$endif}

const
  jidctasm_CONST_SCALE = (INT32(1) shl jidctasm_CONST_BITS);

const
  jidctasm_FIX_0_298631336 = INT32(Round(jidctasm_CONST_SCALE * 0.298631336));  {2446}
  jidctasm_FIX_0_390180644 = INT32(Round(jidctasm_CONST_SCALE * 0.390180644));  {3196}
  jidctasm_FIX_0_541196100 = INT32(Round(jidctasm_CONST_SCALE * 0.541196100));  {4433}
  jidctasm_FIX_0_765366865 = INT32(Round(jidctasm_CONST_SCALE * 0.765366865));  {6270}
  jidctasm_FIX_0_899976223 = INT32(Round(jidctasm_CONST_SCALE * 0.899976223));  {7373}
  jidctasm_FIX_1_175875602 = INT32(Round(jidctasm_CONST_SCALE * 1.175875602));  {9633}
  jidctasm_FIX_1_501321110 = INT32(Round(jidctasm_CONST_SCALE * 1.501321110));  {12299}
  jidctasm_FIX_1_847759065 = INT32(Round(jidctasm_CONST_SCALE * 1.847759065));  {15137}
  jidctasm_FIX_1_961570560 = INT32(Round(jidctasm_CONST_SCALE * 1.961570560));  {16069}
  jidctasm_FIX_2_053119869 = INT32(Round(jidctasm_CONST_SCALE * 2.053119869));  {16819}
  jidctasm_FIX_2_562915447 = INT32(Round(jidctasm_CONST_SCALE * 2.562915447));  {20995}
  jidctasm_FIX_3_072711026 = INT32(Round(jidctasm_CONST_SCALE * 3.072711026));  {25172}


{ for DESCALE }
const
  ROUND_CONST = (INT32(1) shl (jidctasm_CONST_BITS-jidctasm_PASS1_BITS-1));
const
  ROUND_CONST_2 = (INT32(1) shl (jidctasm_CONST_BITS+jidctasm_PASS1_BITS+3-1));

{ Perform dequantization and inverse DCT on one block of coefficients. }

{GLOBAL}
procedure jpeg_idct_islow (cinfo : j_decompress_ptr;
                           compptr : jpeg_component_info_ptr;
		           coef_block : JCOEFPTR;
		           output_buf : JSAMPARRAY;
                           output_col : JDIMENSION);
type
  PWorkspace = ^TWorkspace;
  TWorkspace = coef_bits_field; { buffers data between passes }
const
  coefDCTSIZE = DCTSIZE*SizeOf(JCOEF);
  wrkDCTSIZE = DCTSIZE*SizeOf(int);
var
  tmp0, tmp1, tmp2, tmp3 : INT32;
  tmp10, tmp11, tmp12, tmp13 : INT32;
  z1, z2, z3, z4, z5 : INT32;
var
  inptr : JCOEFPTR;
  quantptr : ISLOW_MULT_TYPE_FIELD_PTR;
  wsptr : PWorkspace;
  outptr : JSAMPROW;
var
  range_limit : JSAMPROW;
  ctr : int;
  workspace : TWorkspace;
var
  dcval : int;
var
  dcval_ : JSAMPLE;
asm
  push  edi
  push  esi
  push  ebx

  cld	{ The only direction we use, might as well set it now, as opposed }
        { to inside 2 loops. }

{ Each IDCT routine is responsible for range-limiting its results and
  converting them to unsigned form (0..MAXJSAMPLE).  The raw outputs could
  be quite far out of range if the input data is corrupt, so a bulletproof
  range-limiting step is required.  We use a mask-and-table-lookup method
  to do the combined operations quickly.  See the comments with
  prepare_range_limit_table (in jdmaster.c) for more info. }

  {range_limit := JSAMPROW(@(cinfo^.sample_range_limit^[CENTERJSAMPLE]));}
  mov	eax, [eax].jpeg_decompress_struct.sample_range_limit {eax=cinfo}
  add	eax, (MAXJSAMPLE+1 + CENTERJSAMPLE)*(Type JSAMPLE)
  mov	range_limit, eax

  { Pass 1: process columns from input, store into work array. }
  { Note results are scaled up by sqrt(8) compared to a true IDCT; }
  { furthermore, we scale the results by 2**PASS1_BITS. }

  {inptr := coef_block;}
  mov	esi, ecx     { ecx=coef_block }
  {quantptr := ISLOW_MULT_TYPE_FIELD_PTR (compptr^.dct_table);}
  mov	edi, [edx].jpeg_component_info.dct_table  { edx=compptr }

  {wsptr := PWorkspace(@workspace);}
  lea	ecx, workspace

  {for ctr := pred(DCTSIZE) downto 0 do
  begin}
  mov	ctr, DCTSIZE
@loop518:
    { Due to quantization, we will usually find that many of the input
      coefficients are zero, especially the AC terms.  We can exploit this
      by short-circuiting the IDCT calculation for any column in which all
      the AC terms are zero.  In that case each output is equal to the
      DC coefficient (with scale factor as needed).
      With typical images and quantization tables, half or more of the
      column DCT calculations can be simplified this way. }

    {if ((inptr^[DCTSIZE*1]) or (inptr^[DCTSIZE*2]) or (inptr^[DCTSIZE*3]) or
	(inptr^[DCTSIZE*4]) or (inptr^[DCTSIZE*5]) or (inptr^[DCTSIZE*6]) or
	(inptr^[DCTSIZE*7]) = 0) then
    begin}
  mov	eax, DWORD PTR [esi+coefDCTSIZE*1]
  or	eax, DWORD PTR [esi+coefDCTSIZE*2]
  or	eax, DWORD PTR [esi+coefDCTSIZE*3]
  mov	edx, DWORD PTR [esi+coefDCTSIZE*4]
  or    eax, edx
  or	eax, DWORD PTR [esi+coefDCTSIZE*5]
  or	eax, DWORD PTR [esi+coefDCTSIZE*6]
  or	eax, DWORD PTR [esi+coefDCTSIZE*7]
  jne	@loop520

      { AC terms all zero }
      {dcval := ISLOW_MULT_TYPE(inptr^[DCTSIZE*0]) *
               (quantptr^[DCTSIZE*0]) shl PASS1_BITS;}
  mov	eax, DWORD PTR [esi+coefDCTSIZE*0]
  imul	eax, DWORD PTR [edi+wrkDCTSIZE*0]
  shl	eax, jidctasm_PASS1_BITS

  {wsptr^[DCTSIZE*0] := dcval;
  wsptr^[DCTSIZE*1] := dcval;
  wsptr^[DCTSIZE*2] := dcval;
  wsptr^[DCTSIZE*3] := dcval;
  wsptr^[DCTSIZE*4] := dcval;
  wsptr^[DCTSIZE*5] := dcval;
  wsptr^[DCTSIZE*6] := dcval;
  wsptr^[DCTSIZE*7] := dcval;}

  mov	DWORD PTR [ecx+ wrkDCTSIZE*0], eax
  mov	DWORD PTR [ecx+ wrkDCTSIZE*1], eax
  mov	DWORD PTR [ecx+ wrkDCTSIZE*2], eax
  mov	DWORD PTR [ecx+ wrkDCTSIZE*3], eax
  mov	DWORD PTR [ecx+ wrkDCTSIZE*4], eax
  mov	DWORD PTR [ecx+ wrkDCTSIZE*5], eax
  mov	DWORD PTR [ecx+ wrkDCTSIZE*6], eax
  mov	DWORD PTR [ecx+ wrkDCTSIZE*7], eax

      {Inc(JCOEF_PTR(inptr));		{ advance pointers to next column }
      {Inc(ISLOW_MULT_TYPE_PTR(quantptr));
      Inc(int_ptr(wsptr));
      continue;}
  dec	ctr
  je	@loop519

  add   esi, Type JCOEF
  add	edi, Type ISLOW_MULT_TYPE
  add	ecx, Type int  { int_ptr }
  jmp	@loop518

@loop520:

    {end;}

    { Even part: reverse the even part of the forward DCT. }
    { The rotator is sqrt(2)*c(-6). }

    {z2 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*2]) * quantptr^[DCTSIZE*2];
    z3 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*6]) * quantptr^[DCTSIZE*6];

    z1 := (z2 + z3) * INT32(FIX_0_541196100);
    tmp2 := z1 + INT32(z3) * INT32(- FIX_1_847759065);
    tmp3 := z1 + INT32(z2) * INT32(FIX_0_765366865);}

  mov	edx, DWORD PTR [esi+coefDCTSIZE*2]
  imul	edx, DWORD PTR [edi+wrkDCTSIZE*2]  {z2}

  mov	eax, DWORD PTR [esi+coefDCTSIZE*6]
  imul	eax, DWORD PTR [edi+wrkDCTSIZE*6]  {z3}

  lea   ebx, [eax+edx]
  imul  ebx, jidctasm_FIX_0_541196100               {z1}

  imul  eax, (-jidctasm_FIX_1_847759065)
  add   eax, ebx
  mov   tmp2, eax

  imul  edx, jidctasm_FIX_0_765366865
  add   edx, ebx
  mov   tmp3, edx

    {z2 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*0]) * quantptr^[DCTSIZE*0];
    z3 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*4]) * quantptr^[DCTSIZE*4];}

  mov	edx, DWORD PTR [esi+coefDCTSIZE*4]
  imul	edx, DWORD PTR [edi+wrkDCTSIZE*4]      { z3 = edx }

  mov	eax, DWORD PTR [esi+coefDCTSIZE*0]
  imul	eax, DWORD PTR [edi+wrkDCTSIZE*0]      { z2 = eax }

    {tmp0 := (z2 + z3) shl CONST_BITS;
    tmp1 := (z2 - z3) shl CONST_BITS;}
  lea ebx,[eax+edx]
  sub eax, edx
  shl ebx, jidctasm_CONST_BITS                          { tmp0 = ebx }
  shl eax, jidctasm_CONST_BITS                          { tmp1 = eax }

    {tmp10 := tmp0 + tmp3;
    tmp13 := tmp0 - tmp3;}
  mov edx, tmp3
  sub ebx, edx
  mov tmp13, ebx
  add edx, edx
  add ebx, edx
  mov tmp10, ebx

    {tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;}
  mov   ebx, tmp2
  sub   eax, ebx
  mov   tmp12, eax
  add   ebx, ebx
  add   eax, ebx
  mov	tmp11, eax

    { Odd part per figure 8; the matrix is unitary and hence its
      transpose is its inverse.  i0..i3 are y7,y5,y3,y1 respectively. }

    {tmp0 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*7]) * quantptr^[DCTSIZE*7];}
  mov	eax, DWORD PTR [esi+coefDCTSIZE*7]
  imul	eax, DWORD PTR [edi+wrkDCTSIZE*7]
  mov   edx, eax                            { edx = tmp0 }
    {tmp0 := (tmp0) * INT32(FIX_0_298631336); { sqrt(2) * (-c1+c3+c5-c7) }
  imul  eax, jidctasm_FIX_0_298631336
  mov	tmp0, eax

    {tmp3 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*1]) * quantptr^[DCTSIZE*1];}
  mov	eax, DWORD PTR [esi+coefDCTSIZE*1]
  imul	eax, DWORD PTR [edi+wrkDCTSIZE*1]
  mov	tmp3, eax

    {z1 := tmp0 + tmp3;}
    {z1 := (z1) * INT32(- FIX_0_899976223); { sqrt(2) * (c7-c3) }
  add	eax, edx
  imul eax, (-jidctasm_FIX_0_899976223)
  mov  z1, eax

    {tmp1 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*5]) * quantptr^[DCTSIZE*5];}
  mov	eax, DWORD PTR [esi+coefDCTSIZE*5]
  imul	eax, DWORD PTR [edi+wrkDCTSIZE*5]
  mov ebx, eax                            { ebx = tmp1 }
    {tmp1 := (tmp1) * INT32(FIX_2_053119869); { sqrt(2) * ( c1+c3-c5+c7) }
  imul  eax, jidctasm_FIX_2_053119869
  mov	tmp1, eax

    {tmp2 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*3]) * quantptr^[DCTSIZE*3];}
  mov	eax, DWORD PTR [esi+coefDCTSIZE*3]
  imul	eax, DWORD PTR [edi+wrkDCTSIZE*3]
  mov	tmp2, eax

    {z3 := tmp0 + tmp2;}
  add	edx, eax                              { edx = z3 }

    {z2 := tmp1 + tmp2;}
    {z2 := (z2) * INT32(- FIX_2_562915447); { sqrt(2) * (-c1-c3) }
  add	eax, ebx
  imul  eax, (-jidctasm_FIX_2_562915447)
  mov	z2, eax

    {z4 := tmp1 + tmp3;}
  add	ebx, tmp3                             { ebx = z4 }

    {z5 := INT32(z3 + z4) * INT32(FIX_1_175875602); { sqrt(2) * c3 }
  lea   eax, [edx+ebx]
  imul eax, jidctasm_FIX_1_175875602                   { eax = z5 }

    {z4 := (z4) * INT32(- FIX_0_390180644); { sqrt(2) * (c5-c3) }
    {Inc(z4, z5);}
  imul   ebx, (-jidctasm_FIX_0_390180644)
  add    ebx, eax
  mov    z4, ebx

    {z3 := (z3) * INT32(- FIX_1_961570560); { sqrt(2) * (-c3-c5) }
    {Inc(z3, z5);}
  imul edx, (-jidctasm_FIX_1_961570560)
  add  eax, edx                        { z3 = eax }

    {Inc(tmp0, z1 + z3);}
  mov   ebx, z1
  add	ebx, eax
  add	tmp0, ebx

    {tmp2 := (tmp2) * INT32(FIX_3_072711026); { sqrt(2) * ( c1+c3+c5-c7) }
    {Inc(tmp2, z2 + z3);}
  mov   ebx, tmp2
  imul  ebx, jidctasm_FIX_3_072711026
  mov	edx, z2                        { z2 = edx }
  add   ebx, edx
  add   eax, ebx
  mov	tmp2, eax

    {Inc(tmp1, z2 + z4);}
  mov   eax, z4                        { z4 = eax }
  add   edx, eax
  add   tmp1, edx

    {tmp3 := (tmp3) * INT32(FIX_1_501321110); { sqrt(2) * ( c1+c3-c5-c7) }
    {Inc(tmp3, z1 + z4);}
  mov	edx, tmp3
  imul  edx, jidctasm_FIX_1_501321110

  add	edx, eax
  add   edx, z1                        { tmp3 = edx }

    { Final output stage: inputs are tmp10..tmp13, tmp0..tmp3 }

    {wsptr^[DCTSIZE*0] := int (DESCALE(tmp10 + tmp3, CONST_BITS-PASS1_BITS));}
    {wsptr^[DCTSIZE*7] := int (DESCALE(tmp10 - tmp3, CONST_BITS-PASS1_BITS));}
  mov	eax, tmp10
  add   eax, ROUND_CONST
  lea   ebx, [eax+edx]
  sar	ebx, jidctasm_CONST_BITS-jidctasm_PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*0], ebx

  sub	eax, edx
  sar	eax, jidctasm_CONST_BITS-jidctasm_PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*7], eax

    {wsptr^[DCTSIZE*1] := int (DESCALE(tmp11 + tmp2, CONST_BITS-PASS1_BITS));}
    {wsptr^[DCTSIZE*6] := int (DESCALE(tmp11 - tmp2, CONST_BITS-PASS1_BITS));}
  mov	eax, tmp11
  add   eax, ROUND_CONST
  mov   edx, tmp2
  lea	ebx, [eax+edx]
  sar	ebx, jidctasm_CONST_BITS-jidctasm_PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*1], ebx

  sub	eax, edx
  sar	eax, jidctasm_CONST_BITS-jidctasm_PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*6], eax

    {wsptr^[DCTSIZE*2] := int (DESCALE(tmp12 + tmp1, CONST_BITS-PASS1_BITS));}
    {wsptr^[DCTSIZE*5] := int (DESCALE(tmp12 - tmp1, CONST_BITS-PASS1_BITS));}
  mov	eax, tmp12
  add   eax, ROUND_CONST
  mov   edx, tmp1
  lea	ebx, [eax+edx]
  sar	ebx, jidctasm_CONST_BITS-jidctasm_PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*2], ebx

  sub	eax, edx
  sar	eax, jidctasm_CONST_BITS-jidctasm_PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*5], eax

    {wsptr^[DCTSIZE*3] := int (DESCALE(tmp13 + tmp0, CONST_BITS-PASS1_BITS));}
    {wsptr^[DCTSIZE*4] := int (DESCALE(tmp13 - tmp0, CONST_BITS-PASS1_BITS));}
  mov	eax, tmp13
  add   eax, ROUND_CONST
  mov   edx, tmp0
  lea   ebx, [eax+edx]
  sar	ebx, jidctasm_CONST_BITS-jidctasm_PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*3], ebx

  sub	eax, edx
  sar	eax, jidctasm_CONST_BITS-jidctasm_PASS1_BITS
  mov	DWORD PTR [ecx+wrkDCTSIZE*4], eax

    {Inc(JCOEF_PTR(inptr));		{ advance pointers to next column }
    {Inc(ISLOW_MULT_TYPE_PTR(quantptr));
    Inc(int_ptr(wsptr));}
  dec	ctr
  je	@loop519

  add   esi, Type JCOEF
  add	edi, Type ISLOW_MULT_TYPE
  add	ecx, Type int  { int_ptr }
  {end;}
	jmp	@loop518
@loop519:
  { Save to memory what we've registerized for the preceding loop. }

  { Pass 2: process rows from work array, store into output array. }
  { Note that we must descale the results by a factor of 8 == 2**3, }
  { and also undo the PASS1_BITS scaling. }

  {wsptr := @workspace;}
  lea	esi, workspace

  {for ctr := 0 to pred(DCTSIZE) do
  begin}
  mov	ctr, 0
@loop523:

    {outptr := output_buf^[ctr];}
  mov	eax, ctr
  mov	ebx, output_buf
  mov	edi, DWORD PTR [ebx+eax*4]           { 4 = SizeOf(pointer) }

    {Inc(JSAMPLE_PTR(outptr), output_col);}
  add	edi, uInt(output_col)

    { Rows of zeroes can be exploited in the same way as we did with columns.
      However, the column calculation has created many nonzero AC terms, so
      the simplification applies less often (typically 5% to 10% of the time).
      On machines with very fast multiplication, it's possible that the
      test takes more time than it's worth.  In that case this section
      may be commented out. }

{$ifndef NO_ZERO_ROW_TEST}
    {if ((wsptr^[1]) or (wsptr^[2]) or (wsptr^[3]) or (wsptr^[4]) or
        (wsptr^[5]) or (wsptr^[6]) or (wsptr^[7]) = 0) then
    begin}
	mov	eax, DWORD PTR [esi+4*1]
	or	eax, DWORD PTR [esi+4*2]
	or	eax, DWORD PTR [esi+4*3]
        jne     @loop525            { Nomssi: early exit path may help }
	or	eax, DWORD PTR [esi+4*4]
	or	eax, DWORD PTR [esi+4*5]
	or	eax, DWORD PTR [esi+4*6]
	or	eax, DWORD PTR [esi+4*7]
	jne	@loop525

      { AC terms all zero }
      {JSAMPLE(dcval_) := range_limit^[int(DESCALE(INT32(wsptr^[0]),
                          PASS1_BITS+3)) and RANGE_MASK];}
	mov	eax, DWORD PTR [esi+4*0]
	add	eax, (INT32(1) shl (jidctasm_PASS1_BITS+3-1))
	sar	eax, jidctasm_PASS1_BITS+3
	and	eax, RANGE_MASK
        mov     ebx, range_limit
	mov	al, BYTE PTR [ebx+eax]
        mov     ah, al

      {outptr^[0] := dcval_;
      outptr^[1] := dcval_;
      outptr^[2] := dcval_;
      outptr^[3] := dcval_;
      outptr^[4] := dcval_;
      outptr^[5] := dcval_;
      outptr^[6] := dcval_;
      outptr^[7] := dcval_;}

	stosw
	stosw
	stosw
	stosw

      {Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
      {continue;}
	add esi, wrkDCTSIZE
	inc	ctr
	cmp	ctr, DCTSIZE
	jl	@loop523
	jmp @loop524
    {end;}
@loop525:
{$endif}


    { Even part: reverse the even part of the forward DCT. }
    { The rotator is sqrt(2)*c(-6). }

    {z2 := INT32 (wsptr^[2]);}
  mov	edx, DWORD PTR [esi+4*2]                   { z2 = edx }

    {z3 := INT32 (wsptr^[6]);}
  mov	ecx, DWORD PTR [esi+4*6]                   { z3 = ecx }

    {z1 := (z2 + z3) * INT32(FIX_0_541196100);}
  lea   eax, [edx+ecx]
  imul  eax, jidctasm_FIX_0_541196100
  mov	ebx, eax                                   { z1 = ebx }

    {tmp2 := z1 + (z3) * INT32(- FIX_1_847759065);}
  imul  ecx, (-jidctasm_FIX_1_847759065)
  add	ecx, ebx                                   { tmp2 = ecx }

    {tmp3 := z1 + (z2) * INT32(FIX_0_765366865);}
  imul  edx, jidctasm_FIX_0_765366865
  add	ebx, edx                                   { tmp3 = ebx }

    {tmp0 := (INT32(wsptr^[0]) + INT32(wsptr^[4])) shl CONST_BITS;}
    {tmp1 := (INT32(wsptr^[0]) - INT32(wsptr^[4])) shl CONST_BITS;}
  mov	edx, DWORD PTR [esi+4*4]
  mov   eax, DWORD PTR [esi+4*0]
  sub   eax, edx
  add   edx, edx
  add   edx, eax
  shl	edx, jidctasm_CONST_BITS              { tmp0 = edx }
  shl	eax, jidctasm_CONST_BITS              { tmp1 = eax }

    {tmp10 := tmp0 + tmp3;}
    {tmp13 := tmp0 - tmp3;}
  sub   edx, ebx
  mov	tmp13, edx
  add   ebx, ebx
  add   edx, ebx
  mov	tmp10, edx

    {tmp11 := tmp1 + tmp2;}
    {tmp12 := tmp1 - tmp2;}
  lea   ebx, [ecx+eax]
  mov	tmp11, ebx
  sub	eax, ecx
  mov	tmp12, eax

    { Odd part per figure 8; the matrix is unitary and hence its
      transpose is its inverse.  i0..i3 are y7,y5,y3,y1 respectively. }

{ The following lines no longer produce code, since wsptr has been
  optimized to esi, it is more efficient to access these values
  directly.
    tmp0 := INT32(wsptr^[7]);
    tmp1 := INT32(wsptr^[5]);
    tmp2 := INT32(wsptr^[3]);
    tmp3 := INT32(wsptr^[1]); }

    {z2 := tmp1 + tmp2;}
    {z2 := (z2) * INT32(- FIX_2_562915447); { sqrt(2) * (-c1-c3) }
  mov	ebx, DWORD PTR [esi+4*3]              { tmp2 }
  mov   ecx, DWORD PTR [esi+4*5]              { tmp1 }
  lea   eax, [ebx+ecx]
  imul  eax, (-jidctasm_FIX_2_562915447)
  mov	z2, eax

    {z3 := tmp0 + tmp2;}
  mov	edx, DWORD PTR [esi+4*7]              { tmp0 }
  add   ebx, edx                              { old z3 = ebx }
  mov	eax, ebx
    {z3 := (z3) * INT32(- FIX_1_961570560); { sqrt(2) * (-c3-c5) }
  imul eax, (-jidctasm_FIX_1_961570560)
  mov	z3, eax

    {z1 := tmp0 + tmp3;}
    {z1 := (z1) * INT32(- FIX_0_899976223); { sqrt(2) * (c7-c3) }
  mov	eax, DWORD PTR [esi+4*1]               { tmp3 }
  add	edx, eax
  imul  edx, (-jidctasm_FIX_0_899976223)                { z1 = edx }

    {z4 := tmp1 + tmp3;}
  add	eax, ecx                              { +tmp1 }
  add	ebx, eax                              { z3 + z4 = ebx }
    {z4 := (z4) * INT32(- FIX_0_390180644); { sqrt(2) * (c5-c3) }
  imul eax, (-jidctasm_FIX_0_390180644)                { z4 = eax }

    {z5 := (z3 + z4) * INT32(FIX_1_175875602); { sqrt(2) * c3 }
    {Inc(z3, z5);}
  imul ebx, jidctasm_FIX_1_175875602
  mov  ecx, z3
  add  ecx, ebx                                { ecx = z3 }

    {Inc(z4, z5);}
  add ebx, eax                                 { z4 = ebx }

    {tmp0 := (tmp0) * INT32(FIX_0_298631336); { sqrt(2) * (-c1+c3+c5-c7) }
    {Inc(tmp0, z1 + z3);}
  mov   eax, DWORD PTR [esi+4*7]
  imul  eax, jidctasm_FIX_0_298631336
  add   eax, edx
  add   eax, ecx
  mov	tmp0, eax

    {tmp1 := (tmp1) * INT32(FIX_2_053119869); { sqrt(2) * ( c1+c3-c5+c7) }
    {Inc(tmp1, z2 + z4);}
  mov  eax, DWORD PTR [esi+4*5]
  imul eax, jidctasm_FIX_2_053119869
  add  eax, z2
  add  eax, ebx
  mov  tmp1, eax

    {tmp2 := (tmp2) * INT32(FIX_3_072711026); { sqrt(2) * ( c1+c3+c5-c7) }
    {Inc(tmp2, z2 + z3);}
  mov	eax, DWORD PTR [esi+4*3]
  imul  eax, jidctasm_FIX_3_072711026
  add   eax, z2
  add   ecx, eax                      { ecx = tmp2 }

    {tmp3 := (tmp3) * INT32(FIX_1_501321110); { sqrt(2) * ( c1+c3-c5-c7) }
    {Inc(tmp3, z1 + z4);}
  mov	eax, DWORD PTR [esi+4*1]
  imul  eax, jidctasm_FIX_1_501321110
  add   eax, edx
  add   ebx, eax                   { ebx = tmp3 }

    { Final output stage: inputs are tmp10..tmp13, tmp0..tmp3 }

    {outptr^[0] := range_limit^[ int(DESCALE(tmp10 + tmp3,
                      CONST_BITS+PASS1_BITS+3)) and RANGE_MASK]; }
    {outptr^[7] := range_limit^[ int(DESCALE(tmp10 - tmp3,
                        CONST_BITS+PASS1_BITS+3)) and RANGE_MASK];}

  mov	edx, tmp10
  add   edx, ROUND_CONST_2
  lea	eax, [ebx+edx]
  sub   edx, ebx

  shr	eax, jidctasm_CONST_BITS+jidctasm_PASS1_BITS+3
  and	eax, RANGE_MASK
  mov   ebx, range_limit           { once for all }
  mov	al, BYTE PTR [ebx+eax]
  mov   [edi+0], al

  shr	edx, jidctasm_CONST_BITS+jidctasm_PASS1_BITS+3
  and	edx, RANGE_MASK
  mov	al, BYTE PTR [ebx+edx]
  mov   [edi+7], al

    {outptr^[1] := range_limit^[ int(DESCALE(tmp11 + tmp2,
                        CONST_BITS+PASS1_BITS+3)) and RANGE_MASK];}
  mov	eax, tmp11
  add   eax, ROUND_CONST_2
  lea	edx, [eax+ecx]
  shr	edx, jidctasm_CONST_BITS+jidctasm_PASS1_BITS+3
  and	edx, RANGE_MASK
  mov	dl, BYTE PTR [ebx+edx]
  mov   [edi+1], dl

    {outptr^[6] := range_limit^[ int(DESCALE(tmp11 - tmp2,
			CONST_BITS+PASS1_BITS+3)) and RANGE_MASK];}
  sub	eax, ecx
  shr	eax, jidctasm_CONST_BITS+jidctasm_PASS1_BITS+3
  and	eax, RANGE_MASK
  mov	al, BYTE PTR [ebx+eax]
  mov   [edi+6], al

    {outptr^[2] := range_limit^[ int(DESCALE(tmp12 + tmp1,
			CONST_BITS+PASS1_BITS+3)) and RANGE_MASK];}
  mov	eax, tmp12
  add   eax, ROUND_CONST_2
  mov   ecx, tmp1
  lea	edx, [eax+ecx]
  shr	edx, jidctasm_CONST_BITS+jidctasm_PASS1_BITS+3
  and	edx, RANGE_MASK
  mov	dl, BYTE PTR [ebx+edx]
  mov   [edi+2], dl

    {outptr^[5] := range_limit^[ int(DESCALE(tmp12 - tmp1,
			CONST_BITS+PASS1_BITS+3)) and RANGE_MASK];}
  sub	eax, ecx
  shr	eax, jidctasm_CONST_BITS+jidctasm_PASS1_BITS+3
  and	eax, RANGE_MASK
  mov	al, BYTE PTR [ebx+eax]
  mov   [edi+5], al

    {outptr^[3] := range_limit^[ int(DESCALE(tmp13 + tmp0,
			CONST_BITS+PASS1_BITS+3)) and RANGE_MASK];}
  mov	eax, tmp13
  add   eax, ROUND_CONST_2
  mov   ecx, tmp0
  lea   edx, [eax+ecx]
  shr	edx, jidctasm_CONST_BITS+jidctasm_PASS1_BITS+3
  and	edx, RANGE_MASK
  mov	dl, BYTE PTR [ebx+edx]
  mov   [edi+3], dl

    {outptr^[4] := range_limit^[ int(DESCALE(tmp13 - tmp0,
			CONST_BITS+PASS1_BITS+3)) and RANGE_MASK];}
  sub	eax, ecx
  shr	eax, jidctasm_CONST_BITS+jidctasm_PASS1_BITS+3
  and	eax, RANGE_MASK
  mov	al, BYTE PTR [ebx+eax]
  mov   [edi+4], al

    {Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
  add	esi, wrkDCTSIZE
  add	edi, DCTSIZE

  {end;}
  inc	ctr
  cmp	ctr, DCTSIZE
  jl	@loop523

@loop524:
@loop496:
  pop   ebx
  pop   esi
  pop   edi
end;

{$ENDIF JPEG_IDCT_ASM}

{== imjidctfst implementation ==}

{ This module is specialized to the case DCTSIZE = 8. }

{$ifndef DCTSIZE_IS_8}
  Sorry, this code only copes with 8x8 DCTs. { deliberate syntax err }
{$endif}

{ Scaling decisions are generally the same as in the LL&M algorithm;
  see jidctint.c for more details.  However, we choose to descale
  (right shift) multiplication products as soon as they are formed,
  rather than carrying additional fractional bits into subsequent additions.
  This compromises accuracy slightly, but it lets us save a few shifts.
  More importantly, 16-bit arithmetic is then adequate (for 8-bit samples)
  everywhere except in the multiplications proper; this saves a good deal
  of work on 16-bit-int machines.

  The dequantized coefficients are not integers because the AA&N scaling
  factors have been incorporated.  We represent them scaled up by PASS1_BITS,
  so that the first and second IDCT rounds have the same input scaling.
  For 8-bit JSAMPLEs, we choose IFAST_SCALE_BITS = PASS1_BITS so as to
  avoid a descaling shift; this compromises accuracy rather drastically
  for small quantization table entries, but it saves a lot of shifts.
  For 12-bit JSAMPLEs, there's no hope of using 16x16 multiplies anyway,
  so we use a much larger scaling factor to preserve accuracy.

  A final compromise is to represent the multiplicative constants to only
  8 fractional bits, rather than 13.  This saves some shifting work on some
  machines, and may also reduce the cost of multiplication (since there
  are fewer one-bits in the constants). }

{$ifdef BITS_IN_JSAMPLE_IS_8}
const
  jidctfst_CONST_BITS = 8;
  jidctfst_PASS1_BITS = 2;
{$else}
const
  jidctfst_CONST_BITS = 8;
  jidctfst_PASS1_BITS = 1;	{ lose a little precision to avoid overflow }
{$endif}


const
  FIX_1_082392200 = INT32(Round((INT32(1) shl jidctfst_CONST_BITS)*1.082392200));  {277}
  FIX_1_414213562 = INT32(Round((INT32(1) shl jidctfst_CONST_BITS)*1.414213562));  {362}
  jidctfst_FIX_1_847759065 = INT32(Round((INT32(1) shl jidctfst_CONST_BITS)*1.847759065));  {473}
  FIX_2_613125930 = INT32(Round((INT32(1) shl jidctfst_CONST_BITS)*2.613125930));  {669}


{ Descale and correctly round an INT32 value that's scaled by N bits.
  We assume RIGHT_SHIFT rounds towards minus infinity, so adding
  the fudge factor is correct for either sign of X. }

function jidctfst_DESCALE(x : INT32; n : int) : INT32;
var
  shift_temp : INT32;
begin
{$ifdef USE_ACCURATE_ROUNDING}
  shift_temp := x + (INT32(1) shl (n-1));
{$else}
{ We can gain a little more speed, with a further compromise in accuracy,
  by omitting the addition in a descaling shift.  This yields an incorrectly
  rounded result half the time... }
  shift_temp := x;
{$endif}

{$ifdef RIGHT_SHIFT_IS_UNSIGNED}
  if shift_temp < 0 then
    jidctfst_Descale :=  (shift_temp shr n) or ((not INT32(0)) shl (32-n))
  else
{$endif}
    jidctfst_Descale :=  (shift_temp shr n);
end;


{ Multiply a DCTELEM variable by an INT32 constant, and immediately
  descale to yield a DCTELEM result. }

  {(DCTELEM( DESCALE((var) * (const), CONST_BITS))}
  function jidctfst_Multiply(Avar, Aconst: Integer): DCTELEM;
  begin
    jidctfst_Multiply := DCTELEM( Avar*INT32(Aconst) div (INT32(1) shl jidctfst_CONST_BITS));
  end;


{ Dequantize a coefficient by multiplying it by the multiplier-table
  entry; produce a DCTELEM result.  For 8-bit data a 16x16->16
  multiplication will do.  For 12-bit data, the multiplier table is
  declared INT32, so a 32-bit multiply will be used. }

{$ifdef BITS_IN_JSAMPLE_IS_8}
  function jidctfst_DEQUANTIZE(coef,quantval : int) : int;
  begin
    jidctfst_Dequantize := ( IFAST_MULT_TYPE(coef) * quantval);
  end;
{$else}
  function jidctfst_DEQUANTIZE(coef,quantval : INT32) : int;
  begin
    jidctfst_Dequantize := jidctfst_DESCALE((coef)*(quantval), IFAST_SCALE_BITS-jidctfst_PASS1_BITS);
  end;
{$endif}


{ Like DESCALE, but applies to a DCTELEM and produces an int.
  We assume that int right shift is unsigned if INT32 right shift is. }

function IDESCALE(x : DCTELEM; n : int) : int;
{$ifdef BITS_IN_JSAMPLE_IS_8}
const
  DCTELEMBITS = 16;	{ DCTELEM may be 16 or 32 bits }
{$else}
const
  DCTELEMBITS = 32;	{ DCTELEM must be 32 bits }
{$endif}
var
  ishift_temp : DCTELEM;
begin
{$ifndef USE_ACCURATE_ROUNDING}
  ishift_temp := x + (INT32(1) shl (n-1));
{$else}
{ We can gain a little more speed, with a further compromise in accuracy,
  by omitting the addition in a descaling shift.  This yields an incorrectly
  rounded result half the time... }
  ishift_temp := x;
{$endif}

{$ifdef RIGHT_SHIFT_IS_UNSIGNED}
  if ishift_temp < 0 then
    IDescale :=  (ishift_temp shr n)
             or ((not DCTELEM(0)) shl (DCTELEMBITS-n))
  else
{$endif}
    IDescale :=  (ishift_temp shr n);
end;


{ Perform dequantization and inverse DCT on one block of coefficients. }

{GLOBAL}
procedure jpeg_idct_ifast (cinfo : j_decompress_ptr;
                           compptr : jpeg_component_info_ptr;
		           coef_block : JCOEFPTR;
		           output_buf : JSAMPARRAY;
                           output_col : JDIMENSION);
type
  PWorkspace = ^TWorkspace;
  TWorkspace = coef_bits_field; { buffers data between passes }
var
  tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7 : DCTELEM;
  tmp10, tmp11, tmp12, tmp13 : DCTELEM;
  z5, z10, z11, z12, z13 : DCTELEM;
  inptr : JCOEFPTR;
  quantptr : IFAST_MULT_TYPE_FIELD_PTR;
  wsptr : PWorkspace;
  outptr : JSAMPROW;
  range_limit : JSAMPROW;
  ctr : int;
  workspace : TWorkspace;        { buffers data between passes }
  {SHIFT_TEMPS}			{ for DESCALE }
  {ISHIFT_TEMPS}		{ for IDESCALE }
var
  dcval : int;
var
  dcval_ : JSAMPLE;
begin
{ Each IDCT routine is responsible for range-limiting its results and
  converting them to unsigned form (0..MAXJSAMPLE).  The raw outputs could
  be quite far out of range if the input data is corrupt, so a bulletproof
  range-limiting step is required.  We use a mask-and-table-lookup method
  to do the combined operations quickly.  See the comments with
  prepare_range_limit_table (in jdmaster.c) for more info. }

  range_limit := JSAMPROW(@(cinfo^.sample_range_limit^[CENTERJSAMPLE]));
  { Pass 1: process columns from input, store into work array. }

  inptr := coef_block;
  quantptr := IFAST_MULT_TYPE_FIELD_PTR(compptr^.dct_table);
  wsptr := @workspace;
  for ctr := pred(DCTSIZE) downto 0 do
  begin
    { Due to quantization, we will usually find that many of the input
      coefficients are zero, especially the AC terms.  We can exploit this
      by short-circuiting the IDCT calculation for any column in which all
      the AC terms are zero.  In that case each output is equal to the
      DC coefficient (with scale factor as needed).
      With typical images and quantization tables, half or more of the
      column DCT calculations can be simplified this way. }

    if (inptr^[DCTSIZE*1]=0) and (inptr^[DCTSIZE*2]=0) and (inptr^[DCTSIZE*3]=0) and
       (inptr^[DCTSIZE*4]=0) and (inptr^[DCTSIZE*5]=0) and (inptr^[DCTSIZE*6]=0) and
       (inptr^[DCTSIZE*7]=0) then
    begin
      { AC terms all zero }
      dcval := int(jidctfst_DEQUANTIZE(inptr^[DCTSIZE*0], quantptr^[DCTSIZE*0]));

      wsptr^[DCTSIZE*0] := dcval;
      wsptr^[DCTSIZE*1] := dcval;
      wsptr^[DCTSIZE*2] := dcval;
      wsptr^[DCTSIZE*3] := dcval;
      wsptr^[DCTSIZE*4] := dcval;
      wsptr^[DCTSIZE*5] := dcval;
      wsptr^[DCTSIZE*6] := dcval;
      wsptr^[DCTSIZE*7] := dcval;

      Inc(JCOEF_PTR(inptr));		{ advance pointers to next column }
      Inc(IFAST_MULT_TYPE_PTR(quantptr));
      Inc(int_ptr(wsptr));
      continue;
    end;

    { Even part }

    tmp0 := jidctfst_DEQUANTIZE(inptr^[DCTSIZE*0], quantptr^[DCTSIZE*0]);
    tmp1 := jidctfst_DEQUANTIZE(inptr^[DCTSIZE*2], quantptr^[DCTSIZE*2]);
    tmp2 := jidctfst_DEQUANTIZE(inptr^[DCTSIZE*4], quantptr^[DCTSIZE*4]);
    tmp3 := jidctfst_DEQUANTIZE(inptr^[DCTSIZE*6], quantptr^[DCTSIZE*6]);

    tmp10 := tmp0 + tmp2;	{ phase 3 }
    tmp11 := tmp0 - tmp2;

    tmp13 := tmp1 + tmp3;	{ phases 5-3 }
    tmp12 := jidctfst_MULTIPLY(tmp1 - tmp3, FIX_1_414213562) - tmp13; { 2*c4 }

    tmp0 := tmp10 + tmp13;	{ phase 2 }
    tmp3 := tmp10 - tmp13;
    tmp1 := tmp11 + tmp12;
    tmp2 := tmp11 - tmp12;

    { Odd part }

    tmp4 := jidctfst_DEQUANTIZE(inptr^[DCTSIZE*1], quantptr^[DCTSIZE*1]);
    tmp5 := jidctfst_DEQUANTIZE(inptr^[DCTSIZE*3], quantptr^[DCTSIZE*3]);
    tmp6 := jidctfst_DEQUANTIZE(inptr^[DCTSIZE*5], quantptr^[DCTSIZE*5]);
    tmp7 := jidctfst_DEQUANTIZE(inptr^[DCTSIZE*7], quantptr^[DCTSIZE*7]);

    z13 := tmp6 + tmp5;		{ phase 6 }
    z10 := tmp6 - tmp5;
    z11 := tmp4 + tmp7;
    z12 := tmp4 - tmp7;

    tmp7 := z11 + z13;		{ phase 5 }
    tmp11 := jidctfst_MULTIPLY(z11 - z13, FIX_1_414213562); { 2*c4 }

    z5 := jidctfst_MULTIPLY(z10 + z12, jidctfst_FIX_1_847759065); { 2*c2 }
    tmp10 := jidctfst_MULTIPLY(z12, FIX_1_082392200) - z5; { 2*(c2-c6) }
    tmp12 := jidctfst_MULTIPLY(z10, - FIX_2_613125930) + z5; { -2*(c2+c6) }

    tmp6 := tmp12 - tmp7;	{ phase 2 }
    tmp5 := tmp11 - tmp6;
    tmp4 := tmp10 + tmp5;

    wsptr^[DCTSIZE*0] := int (tmp0 + tmp7);
    wsptr^[DCTSIZE*7] := int (tmp0 - tmp7);
    wsptr^[DCTSIZE*1] := int (tmp1 + tmp6);
    wsptr^[DCTSIZE*6] := int (tmp1 - tmp6);
    wsptr^[DCTSIZE*2] := int (tmp2 + tmp5);
    wsptr^[DCTSIZE*5] := int (tmp2 - tmp5);
    wsptr^[DCTSIZE*4] := int (tmp3 + tmp4);
    wsptr^[DCTSIZE*3] := int (tmp3 - tmp4);

    Inc(JCOEF_PTR(inptr));		{ advance pointers to next column }
    Inc(IFAST_MULT_TYPE_PTR(quantptr));
    Inc(int_ptr(wsptr));
  end;

  { Pass 2: process rows from work array, store into output array. }
  { Note that we must descale the results by a factor of 8 == 2**3, }
  { and also undo the PASS1_BITS scaling. }

  wsptr := @workspace;
  for ctr := 0 to pred(DCTSIZE) do
  begin
    outptr := JSAMPROW(@output_buf^[ctr]^[output_col]);
    { Rows of zeroes can be exploited in the same way as we did with columns.
      However, the column calculation has created many nonzero AC terms, so
      the simplification applies less often (typically 5% to 10% of the time).
      On machines with very fast multiplication, it's possible that the
      test takes more time than it's worth.  In that case this section
      may be commented out. }

{$ifndef NO_ZERO_ROW_TEST}
    if (wsptr^[1]=0) and (wsptr^[2]=0) and (wsptr^[3]=0) and (wsptr^[4]=0) and
       (wsptr^[5]=0) and (wsptr^[6]=0) and (wsptr^[7]=0) then
    begin
      { AC terms all zero }
      dcval_ := range_limit^[IDESCALE(wsptr^[0], jidctfst_PASS1_BITS+3)
                          and RANGE_MASK];

      outptr^[0] := dcval_;
      outptr^[1] := dcval_;
      outptr^[2] := dcval_;
      outptr^[3] := dcval_;
      outptr^[4] := dcval_;
      outptr^[5] := dcval_;
      outptr^[6] := dcval_;
      outptr^[7] := dcval_;

      Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
      continue;
    end;
{$endif}

    { Even part }

    tmp10 := (DCTELEM(wsptr^[0]) + DCTELEM(wsptr^[4]));
    tmp11 := (DCTELEM(wsptr^[0]) - DCTELEM(wsptr^[4]));

    tmp13 := (DCTELEM(wsptr^[2]) + DCTELEM(wsptr^[6]));
    tmp12 := jidctfst_MULTIPLY(DCTELEM(wsptr^[2]) - DCTELEM(wsptr^[6]), FIX_1_414213562)
	    - tmp13;

    tmp0 := tmp10 + tmp13;
    tmp3 := tmp10 - tmp13;
    tmp1 := tmp11 + tmp12;
    tmp2 := tmp11 - tmp12;

    { Odd part }

    z13 := DCTELEM(wsptr^[5]) + DCTELEM(wsptr^[3]);
    z10 := DCTELEM(wsptr^[5]) - DCTELEM(wsptr^[3]);
    z11 := DCTELEM(wsptr^[1]) + DCTELEM(wsptr^[7]);
    z12 := DCTELEM(wsptr^[1]) - DCTELEM(wsptr^[7]);

    tmp7 := z11 + z13;		{ phase 5 }
    tmp11 := jidctfst_MULTIPLY(z11 - z13, FIX_1_414213562); { 2*c4 }

    z5 := jidctfst_MULTIPLY(z10 + z12, jidctfst_FIX_1_847759065); { 2*c2 }
    tmp10 := jidctfst_MULTIPLY(z12, FIX_1_082392200) - z5; { 2*(c2-c6) }
    tmp12 := jidctfst_MULTIPLY(z10, - FIX_2_613125930) + z5; { -2*(c2+c6) }

    tmp6 := tmp12 - tmp7;	{ phase 2 }
    tmp5 := tmp11 - tmp6;
    tmp4 := tmp10 + tmp5;

    { Final output stage: scale down by a factor of 8 and range-limit }

    outptr^[0] := range_limit^[IDESCALE(tmp0 + tmp7, jidctfst_PASS1_BITS+3)
			    and RANGE_MASK];
    outptr^[7] := range_limit^[IDESCALE(tmp0 - tmp7, jidctfst_PASS1_BITS+3)
			    and RANGE_MASK];
    outptr^[1] := range_limit^[IDESCALE(tmp1 + tmp6, jidctfst_PASS1_BITS+3)
			    and RANGE_MASK];
    outptr^[6] := range_limit^[IDESCALE(tmp1 - tmp6, jidctfst_PASS1_BITS+3)
			    and RANGE_MASK];
    outptr^[2] := range_limit^[IDESCALE(tmp2 + tmp5, jidctfst_PASS1_BITS+3)
			    and RANGE_MASK];
    outptr^[5] := range_limit^[IDESCALE(tmp2 - tmp5, jidctfst_PASS1_BITS+3)
			    and RANGE_MASK];
    outptr^[4] := range_limit^[IDESCALE(tmp3 + tmp4, jidctfst_PASS1_BITS+3)
			    and RANGE_MASK];
    outptr^[3] := range_limit^[IDESCALE(tmp3 - tmp4, jidctfst_PASS1_BITS+3)
			    and RANGE_MASK];

    Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
  end;
end;

{== imjidctflt implementation ==}

{ This module is specialized to the case DCTSIZE = 8. }

{$ifndef DCTSIZE_IS_8}
  Sorry, this code only copes with 8x8 DCTs. { deliberate syntax err }
{$endif}


{ Dequantize a coefficient by multiplying it by the multiplier-table
  entry; produce a float result. }

function jidctflt_DEQUANTIZE(coef : int; quantval : FAST_FLOAT) : FAST_FLOAT;
begin
  jidctflt_Dequantize := ( (coef) * quantval);
end;

{ Descale and correctly round an INT32 value that's scaled by N bits.
  We assume RIGHT_SHIFT rounds towards minus infinity, so adding
  the fudge factor is correct for either sign of X. }

function jidctflt_DESCALE(x : INT32; n : int) : INT32;
var
  shift_temp : INT32;
begin
{$ifdef RIGHT_SHIFT_IS_UNSIGNED}
  shift_temp := x + (INT32(1) shl (n-1));
  if shift_temp < 0 then
    jidctflt_Descale :=  (shift_temp shr n) or ((not INT32(0)) shl (32-n))
  else
    jidctflt_Descale :=  (shift_temp shr n);
{$else}
  jidctflt_Descale := (x + (INT32(1) shl (n-1)) shr n;
{$endif}
end;


{ Perform dequantization and inverse DCT on one block of coefficients. }

{GLOBAL}
procedure jpeg_idct_float (cinfo : j_decompress_ptr;
                           compptr : jpeg_component_info_ptr;
		           coef_block : JCOEFPTR;
		           output_buf : JSAMPARRAY;
                           output_col : JDIMENSION);
type
  PWorkspace = ^TWorkspace;
  TWorkspace = array[0..DCTSIZE2-1] of FAST_FLOAT;
var
  tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7 : FAST_FLOAT;
  tmp10, tmp11, tmp12, tmp13 : FAST_FLOAT;
  z5, z10, z11, z12, z13 : FAST_FLOAT;
  inptr : JCOEFPTR;
  quantptr : FLOAT_MULT_TYPE_FIELD_PTR;
  wsptr : PWorkSpace;
  outptr : JSAMPROW;
  range_limit : JSAMPROW;
  ctr : int;
  workspace : TWorkspace; { buffers data between passes }
  {SHIFT_TEMPS}
var
  dcval : FAST_FLOAT;
begin
{ Each IDCT routine is responsible for range-limiting its results and
  converting them to unsigned form (0..MAXJSAMPLE).  The raw outputs could
  be quite far out of range if the input data is corrupt, so a bulletproof
  range-limiting step is required.  We use a mask-and-table-lookup method
  to do the combined operations quickly.  See the comments with
  prepare_range_limit_table (in jdmaster.c) for more info. }

  range_limit := JSAMPROW(@(cinfo^.sample_range_limit^[CENTERJSAMPLE]));

  { Pass 1: process columns from input, store into work array. }

  inptr := coef_block;
  quantptr := FLOAT_MULT_TYPE_FIELD_PTR (compptr^.dct_table);
  wsptr := @workspace;
  for ctr := pred(DCTSIZE) downto 0 do
  begin
    { Due to quantization, we will usually find that many of the input
      coefficients are zero, especially the AC terms.  We can exploit this
      by short-circuiting the IDCT calculation for any column in which all
      the AC terms are zero.  In that case each output is equal to the
      DC coefficient (with scale factor as needed).
      With typical images and quantization tables, half or more of the
      column DCT calculations can be simplified this way. }

    if (inptr^[DCTSIZE*1]=0) and (inptr^[DCTSIZE*2]=0) and
       (inptr^[DCTSIZE*3]=0) and (inptr^[DCTSIZE*4]=0) and
       (inptr^[DCTSIZE*5]=0) and (inptr^[DCTSIZE*6]=0) and
       (inptr^[DCTSIZE*7]=0) then
    begin
      { AC terms all zero }
      FAST_FLOAT(dcval) := jidctflt_DEQUANTIZE(inptr^[DCTSIZE*0], quantptr^[DCTSIZE*0]);

      wsptr^[DCTSIZE*0] := dcval;
      wsptr^[DCTSIZE*1] := dcval;
      wsptr^[DCTSIZE*2] := dcval;
      wsptr^[DCTSIZE*3] := dcval;
      wsptr^[DCTSIZE*4] := dcval;
      wsptr^[DCTSIZE*5] := dcval;
      wsptr^[DCTSIZE*6] := dcval;
      wsptr^[DCTSIZE*7] := dcval;

      Inc(JCOEF_PTR(inptr));		{ advance pointers to next column }
      Inc(FLOAT_MULT_TYPE_PTR(quantptr));
      Inc(FAST_FLOAT_PTR(wsptr));
      continue;
    end;

    { Even part }

    tmp0 := jidctflt_DEQUANTIZE(inptr^[DCTSIZE*0], quantptr^[DCTSIZE*0]);
    tmp1 := jidctflt_DEQUANTIZE(inptr^[DCTSIZE*2], quantptr^[DCTSIZE*2]);
    tmp2 := jidctflt_DEQUANTIZE(inptr^[DCTSIZE*4], quantptr^[DCTSIZE*4]);
    tmp3 := jidctflt_DEQUANTIZE(inptr^[DCTSIZE*6], quantptr^[DCTSIZE*6]);

    tmp10 := tmp0 + tmp2;	{ phase 3 }
    tmp11 := tmp0 - tmp2;

    tmp13 := tmp1 + tmp3;	{ phases 5-3 }
    tmp12 := (tmp1 - tmp3) * ({FAST_FLOAT}(1.414213562)) - tmp13; { 2*c4 }

    tmp0 := tmp10 + tmp13;	{ phase 2 }
    tmp3 := tmp10 - tmp13;
    tmp1 := tmp11 + tmp12;
    tmp2 := tmp11 - tmp12;

    { Odd part }

    tmp4 := jidctflt_DEQUANTIZE(inptr^[DCTSIZE*1], quantptr^[DCTSIZE*1]);
    tmp5 := jidctflt_DEQUANTIZE(inptr^[DCTSIZE*3], quantptr^[DCTSIZE*3]);
    tmp6 := jidctflt_DEQUANTIZE(inptr^[DCTSIZE*5], quantptr^[DCTSIZE*5]);
    tmp7 := jidctflt_DEQUANTIZE(inptr^[DCTSIZE*7], quantptr^[DCTSIZE*7]);

    z13 := tmp6 + tmp5;		{ phase 6 }
    z10 := tmp6 - tmp5;
    z11 := tmp4 + tmp7;
    z12 := tmp4 - tmp7;

    tmp7 := z11 + z13;		{ phase 5 }
    tmp11 := (z11 - z13) * ({FAST_FLOAT}(1.414213562)); { 2*c4 }

    z5 := (z10 + z12) * ({FAST_FLOAT}(1.847759065)); { 2*c2 }
    tmp10 := ({FAST_FLOAT}(1.082392200)) * z12 - z5; { 2*(c2-c6) }
    tmp12 := ({FAST_FLOAT}(-2.613125930)) * z10 + z5; { -2*(c2+c6) }

    tmp6 := tmp12 - tmp7;	{ phase 2 }
    tmp5 := tmp11 - tmp6;
    tmp4 := tmp10 + tmp5;

    wsptr^[DCTSIZE*0] := tmp0 + tmp7;
    wsptr^[DCTSIZE*7] := tmp0 - tmp7;
    wsptr^[DCTSIZE*1] := tmp1 + tmp6;
    wsptr^[DCTSIZE*6] := tmp1 - tmp6;
    wsptr^[DCTSIZE*2] := tmp2 + tmp5;
    wsptr^[DCTSIZE*5] := tmp2 - tmp5;
    wsptr^[DCTSIZE*4] := tmp3 + tmp4;
    wsptr^[DCTSIZE*3] := tmp3 - tmp4;

    Inc(JCOEF_PTR(inptr));		{ advance pointers to next column }
    Inc(FLOAT_MULT_TYPE_PTR(quantptr));
    Inc(FAST_FLOAT_PTR(wsptr));
  end;

  { Pass 2: process rows from work array, store into output array. }
  { Note that we must descale the results by a factor of 8 = 2**3. }

  wsptr := @workspace;
  for ctr := 0 to pred(DCTSIZE) do
  begin
    outptr := JSAMPROW(@(output_buf^[ctr]^[output_col]));
    { Rows of zeroes can be exploited in the same way as we did with columns.
      However, the column calculation has created many nonzero AC terms, so
      the simplification applies less often (typically 5% to 10% of the time).
      And testing floats for zero is relatively expensive, so we don't bother. }

    { Even part }

    tmp10 := wsptr^[0] + wsptr^[4];
    tmp11 := wsptr^[0] - wsptr^[4];

    tmp13 := wsptr^[2] + wsptr^[6];
    tmp12 := (wsptr^[2] - wsptr^[6]) * ({FAST_FLOAT}(1.414213562)) - tmp13;

    tmp0 := tmp10 + tmp13;
    tmp3 := tmp10 - tmp13;
    tmp1 := tmp11 + tmp12;
    tmp2 := tmp11 - tmp12;

    { Odd part }

    z13 := wsptr^[5] + wsptr^[3];
    z10 := wsptr^[5] - wsptr^[3];
    z11 := wsptr^[1] + wsptr^[7];
    z12 := wsptr^[1] - wsptr^[7];

    tmp7 := z11 + z13;
    tmp11 := (z11 - z13) * ({FAST_FLOAT}(1.414213562));

    z5 := (z10 + z12) * ({FAST_FLOAT}(1.847759065)); { 2*c2 }
    tmp10 := ({FAST_FLOAT}(1.082392200)) * z12 - z5; { 2*(c2-c6) }
    tmp12 := ({FAST_FLOAT}(-2.613125930)) * z10 + z5; { -2*(c2+c6) }

    tmp6 := tmp12 - tmp7;
    tmp5 := tmp11 - tmp6;
    tmp4 := tmp10 + tmp5;

    { Final output stage: scale down by a factor of 8 and range-limit }

    outptr^[0] := range_limit^[ int(jidctflt_DESCALE( INT32(Round((tmp0 + tmp7))), 3))
			    and RANGE_MASK];
    outptr^[7] := range_limit^[ int(jidctflt_DESCALE( INT32(Round((tmp0 - tmp7))), 3))
			    and RANGE_MASK];
    outptr^[1] := range_limit^[ int(jidctflt_DESCALE( INT32(Round((tmp1 + tmp6))), 3))
			    and RANGE_MASK];
    outptr^[6] := range_limit^[ int(jidctflt_DESCALE( INT32(Round((tmp1 - tmp6))), 3))
			    and RANGE_MASK];
    outptr^[2] := range_limit^[ int(jidctflt_DESCALE( INT32(Round((tmp2 + tmp5))), 3))
			    and RANGE_MASK];
    outptr^[5] := range_limit^[ int(jidctflt_DESCALE( INT32(Round((tmp2 - tmp5))), 3))
			    and RANGE_MASK];
    outptr^[4] := range_limit^[ int(jidctflt_DESCALE( INT32(Round((tmp3 + tmp4))), 3))
			    and RANGE_MASK];
    outptr^[3] := range_limit^[ int(jidctflt_DESCALE( INT32(Round((tmp3 - tmp4))), 3))
			    and RANGE_MASK];

    Inc(FAST_FLOAT_PTR(wsptr), DCTSIZE);	{ advance pointer to next row }
  end;
end;

{== imjidctred implementation ==}

{ This module is specialized to the case DCTSIZE = 8. }

{$ifndef DCTSIZE_IS_8}
  Sorry, this code only copes with 8x8 DCTs. { deliberate syntax err }
{$endif}


{ Scaling is the same as in jidctint.c. }

{$ifdef BITS_IN_JSAMPLE_IS_8}
const
  jidctred_CONST_BITS = 13;
  jidctred_PASS1_BITS = 2;
{$else}
const
  jidctred_CONST_BITS = 13;
  jidctred_PASS1_BITS = 1;	{ lose a little precision to avoid overflow }
{$endif}

const
  FIX_0_211164243 = INT32(Round((INT32(1) shl jidctred_CONST_BITS) * 0.211164243)); {1730}
  FIX_0_509795579 = INT32(Round((INT32(1) shl jidctred_CONST_BITS) * 0.509795579)); {4176}
  FIX_0_601344887 = INT32(Round((INT32(1) shl jidctred_CONST_BITS) * 0.601344887)); {4926}
  FIX_0_720959822 = INT32(Round((INT32(1) shl jidctred_CONST_BITS) * 0.720959822)); {5906}
  jidctred_FIX_0_765366865 = INT32(Round((INT32(1) shl jidctred_CONST_BITS) * 0.765366865)); {6270}
  FIX_0_850430095 = INT32(Round((INT32(1) shl jidctred_CONST_BITS) * 0.850430095)); {6967}
  jidctred_FIX_0_899976223 = INT32(Round((INT32(1) shl jidctred_CONST_BITS) * 0.899976223)); {7373}
  FIX_1_061594337 = INT32(Round((INT32(1) shl jidctred_CONST_BITS) * 1.061594337)); {8697}
  FIX_1_272758580 = INT32(Round((INT32(1) shl jidctred_CONST_BITS) * 1.272758580)); {10426}
  FIX_1_451774981 = INT32(Round((INT32(1) shl jidctred_CONST_BITS) * 1.451774981)); {11893}
  jidctred_FIX_1_847759065 = INT32(Round((INT32(1) shl jidctred_CONST_BITS) * 1.847759065)); {15137}
  FIX_2_172734803 = INT32(Round((INT32(1) shl jidctred_CONST_BITS) * 2.172734803)); {17799}
  jidctred_FIX_2_562915447 = INT32(Round((INT32(1) shl jidctred_CONST_BITS) * 2.562915447)); {20995}
  FIX_3_624509785 = INT32(Round((INT32(1) shl jidctred_CONST_BITS) * 3.624509785)); {29692}


{ Multiply an INT32 variable by an INT32 constant to yield an INT32 result.
  For 8-bit samples with the recommended scaling, all the variable
  and constant values involved are no more than 16 bits wide, so a
  16x16->32 bit multiply can be used instead of a full 32x32 multiply.
  For 12-bit samples, a full 32-bit multiplication will be needed. }

{$ifdef BITS_IN_JSAMPLE_IS_8}

   {function Multiply(X, Y: Integer): integer; assembler;
   asm
     mov ax, X
     imul Y
     mov al, ah
     mov ah, dl
   end;}

   {MULTIPLY16C16(var,const)}
   function jidctred_Multiply(X, Y: Integer): INT32;
   begin
     jidctred_Multiply := X*INT32(Y);
   end;


{$else}
   function jidctred_Multiply(X, Y: INT32): INT32;
   begin
     jidctred_Multiply := X*Y;
   end;
{$endif}


{ Dequantize a coefficient by multiplying it by the multiplier-table
  entry; produce an int result.  In this module, both inputs and result
  are 16 bits or less, so either int or short multiply will work. }

function jidctred_DEQUANTIZE(coef,quantval : int) : int;
begin
  jidctred_Dequantize := ( ISLOW_MULT_TYPE(coef) * quantval);
end;


{ Descale and correctly round an INT32 value that's scaled by N bits.
  We assume RIGHT_SHIFT rounds towards minus infinity, so adding
  the fudge factor is correct for either sign of X. }

function jidctred_DESCALE(x : INT32; n : int) : INT32;
var
  shift_temp : INT32;
begin
{$ifdef RIGHT_SHIFT_IS_UNSIGNED}
  shift_temp := x + (INT32(1) shl (n-1));
  if shift_temp < 0 then
    jidctred_Descale :=  (shift_temp shr n) or ((not INT32(0)) shl (32-n))
  else
    jidctred_Descale :=  (shift_temp shr n);
{$else}
  jidctred_Descale := (x + (INT32(1) shl (n-1)) shr n;
{$endif}
end;

{ Perform dequantization and inverse DCT on one block of coefficients,
  producing a reduced-size 4x4 output block. }

{GLOBAL}
procedure jpeg_idct_4x4 (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
	                 coef_block : JCOEFPTR;
	                 output_buf : JSAMPARRAY;
                         output_col : JDIMENSION);
type
  PWorkspace = ^TWorkspace;
  TWorkspace = array[0..(DCTSIZE*4)-1] of int; { buffers data between passes }
var
  tmp0, tmp2, tmp10, tmp12 : INT32;
  z1, z2, z3, z4 : INT32;
  inptr : JCOEFPTR;
  quantptr : ISLOW_MULT_TYPE_FIELD_PTR;
  wsptr : PWorkspace;
  outptr : JSAMPROW;
  range_limit : JSAMPROW;
  ctr : int;
  workspace : TWorkspace;	{ buffers data between passes }
  {SHIFT_TEMPS}
var
  dcval : int;
var
  dcval_ : JSAMPLE;
begin
{ Each IDCT routine is responsible for range-limiting its results and
  converting them to unsigned form (0..MAXJSAMPLE).  The raw outputs could
  be quite far out of range if the input data is corrupt, so a bulletproof
  range-limiting step is required.  We use a mask-and-table-lookup method
  to do the combined operations quickly.  See the comments with
  prepare_range_limit_table (in jdmaster.c) for more info. }

  range_limit := JSAMPROW(@(cinfo^.sample_range_limit^[CENTERJSAMPLE]));

  { Pass 1: process columns from input, store into work array. }

  inptr := coef_block;
  quantptr := ISLOW_MULT_TYPE_FIELD_PTR (compptr^.dct_table);
  wsptr := @workspace;
  for ctr := DCTSIZE downto 1 do
  begin
    { Don't bother to process column 4, because second pass won't use it }
    if (ctr = DCTSIZE-4) then
    begin
      Inc(JCOEF_PTR(inptr));
      Inc(ISLOW_MULT_TYPE_PTR(quantptr));
      Inc(int_ptr(wsptr));

      continue;
    end;
    if (inptr^[DCTSIZE*1]=0) and (inptr^[DCTSIZE*2]=0) and (inptr^[DCTSIZE*3]=0) and
       (inptr^[DCTSIZE*5]=0) and (inptr^[DCTSIZE*6]=0) and (inptr^[DCTSIZE*7]=0) then
    begin
      { AC terms all zero; we need not examine term 4 for 4x4 output }
      dcval := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*0]) *
                      quantptr^[DCTSIZE*0]) shl jidctred_PASS1_BITS;

      wsptr^[DCTSIZE*0] := dcval;
      wsptr^[DCTSIZE*1] := dcval;
      wsptr^[DCTSIZE*2] := dcval;
      wsptr^[DCTSIZE*3] := dcval;

      Inc(JCOEF_PTR(inptr));
      Inc(ISLOW_MULT_TYPE_PTR(quantptr));
      Inc(int_ptr(wsptr));

      continue;
    end;

    { Even part }

    tmp0 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*0]) * quantptr^[DCTSIZE*0]);

    tmp0 := tmp0 shl (jidctred_CONST_BITS+1);

    z2 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*2]) * quantptr^[DCTSIZE*2]);
    z3 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*6]) * quantptr^[DCTSIZE*6]);

    tmp2 := jidctred_MULTIPLY(z2, jidctred_FIX_1_847759065) + jidctred_MULTIPLY(z3, - jidctred_FIX_0_765366865);

    tmp10 := tmp0 + tmp2;
    tmp12 := tmp0 - tmp2;

    { Odd part }

    z1 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*7]) * quantptr^[DCTSIZE*7];
    z2 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*5]) * quantptr^[DCTSIZE*5];
    z3 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*3]) * quantptr^[DCTSIZE*3];
    z4 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*1]) * quantptr^[DCTSIZE*1];

    tmp0 := jidctred_MULTIPLY(z1, - FIX_0_211164243) { sqrt(2) * (c3-c1) }
	  + jidctred_MULTIPLY(z2, FIX_1_451774981) { sqrt(2) * (c3+c7) }
	  + jidctred_MULTIPLY(z3, - FIX_2_172734803) { sqrt(2) * (-c1-c5) }
	  + jidctred_MULTIPLY(z4, FIX_1_061594337); { sqrt(2) * (c5+c7) }

    tmp2 := jidctred_MULTIPLY(z1, - FIX_0_509795579) { sqrt(2) * (c7-c5) }
	  + jidctred_MULTIPLY(z2, - FIX_0_601344887) { sqrt(2) * (c5-c1) }
	  + jidctred_MULTIPLY(z3, jidctred_FIX_0_899976223) { sqrt(2) * (c3-c7) }
	  + jidctred_MULTIPLY(z4, jidctred_FIX_2_562915447); { sqrt(2) * (c1+c3) }

    { Final output stage }

    wsptr^[DCTSIZE*0] := int(jidctred_DESCALE(tmp10 + tmp2, jidctred_CONST_BITS-jidctred_PASS1_BITS+1));
    wsptr^[DCTSIZE*3] := int(jidctred_DESCALE(tmp10 - tmp2, jidctred_CONST_BITS-jidctred_PASS1_BITS+1));
    wsptr^[DCTSIZE*1] := int(jidctred_DESCALE(tmp12 + tmp0, jidctred_CONST_BITS-jidctred_PASS1_BITS+1));
    wsptr^[DCTSIZE*2] := int(jidctred_DESCALE(tmp12 - tmp0, jidctred_CONST_BITS-jidctred_PASS1_BITS+1));

    Inc(JCOEF_PTR(inptr));
    Inc(ISLOW_MULT_TYPE_PTR(quantptr));
    Inc(int_ptr(wsptr));
  end;

  { Pass 2: process 4 rows from work array, store into output array. }

  wsptr := @workspace;
  for ctr := 0 to pred(4) do
  begin
    outptr := JSAMPROW(@ output_buf^[ctr]^[output_col]);
    { It's not clear whether a zero row test is worthwhile here ... }

{$ifndef NO_ZERO_ROW_TEST}
    if (wsptr^[1]=0) and (wsptr^[2]=0) and (wsptr^[3]=0) and
       (wsptr^[5]=0) and (wsptr^[6]=0) and (wsptr^[7]=0) then
    begin
      { AC terms all zero }
      dcval_ := range_limit^[int(jidctred_DESCALE(INT32(wsptr^[0]), jidctred_PASS1_BITS+3))
				  and RANGE_MASK];

      outptr^[0] := dcval_;
      outptr^[1] := dcval_;
      outptr^[2] := dcval_;
      outptr^[3] := dcval_;

      Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
      continue;
    end;
{$endif}

    { Even part }

    tmp0 := (INT32(wsptr^[0])) shl (jidctred_CONST_BITS+1);

    tmp2 := jidctred_MULTIPLY(INT32(wsptr^[2]), jidctred_FIX_1_847759065)
	  + jidctred_MULTIPLY(INT32(wsptr^[6]), - jidctred_FIX_0_765366865);

    tmp10 := tmp0 + tmp2;
    tmp12 := tmp0 - tmp2;

    { Odd part }

    z1 := INT32(wsptr^[7]);
    z2 := INT32(wsptr^[5]);
    z3 := INT32(wsptr^[3]);
    z4 := INT32(wsptr^[1]);

    tmp0 := jidctred_MULTIPLY(z1, - FIX_0_211164243) { sqrt(2) * (c3-c1) }
	  + jidctred_MULTIPLY(z2, FIX_1_451774981) { sqrt(2) * (c3+c7) }
	  + jidctred_MULTIPLY(z3, - FIX_2_172734803) { sqrt(2) * (-c1-c5) }
	  + jidctred_MULTIPLY(z4, FIX_1_061594337); { sqrt(2) * (c5+c7) }

    tmp2 := jidctred_MULTIPLY(z1, - FIX_0_509795579) { sqrt(2) * (c7-c5) }
	  + jidctred_MULTIPLY(z2, - FIX_0_601344887) { sqrt(2) * (c5-c1) }
	  + jidctred_MULTIPLY(z3, jidctred_FIX_0_899976223) { sqrt(2) * (c3-c7) }
	  + jidctred_MULTIPLY(z4, jidctred_FIX_2_562915447); { sqrt(2) * (c1+c3) }

    { Final output stage }

    outptr^[0] := range_limit^[ int(jidctred_DESCALE(tmp10 + tmp2,
					  jidctred_CONST_BITS+jidctred_PASS1_BITS+3+1))
			    and RANGE_MASK];
    outptr^[3] := range_limit^[ int(jidctred_DESCALE(tmp10 - tmp2,
					  jidctred_CONST_BITS+jidctred_PASS1_BITS+3+1))
			    and RANGE_MASK];
    outptr^[1] := range_limit^[ int(jidctred_DESCALE(tmp12 + tmp0,
					  jidctred_CONST_BITS+jidctred_PASS1_BITS+3+1))
			    and RANGE_MASK];
    outptr^[2] := range_limit^[ int(jidctred_DESCALE(tmp12 - tmp0,
				 	  jidctred_CONST_BITS+jidctred_PASS1_BITS+3+1))
			    and RANGE_MASK];

    Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
  end;
end;


{ Perform dequantization and inverse DCT on one block of coefficients,
  producing a reduced-size 2x2 output block. }

{GLOBAL}
procedure jpeg_idct_2x2 (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
	                 coef_block : JCOEFPTR;
                         output_buf : JSAMPARRAY;
                         output_col : JDIMENSION);
type
  PWorkspace = ^TWorkspace;
  TWorkspace = array[0..(DCTSIZE*2)-1] of int; { buffers data between passes }
var
  tmp0, tmp10, z1 : INT32;
  inptr : JCOEFPTR;
  quantptr : ISLOW_MULT_TYPE_FIELD_PTR;
  wsptr : PWorkspace;
  outptr : JSAMPROW;
  range_limit : JSAMPROW;
  ctr : int;
  workspace : TWorkspace;  { buffers data between passes }
  {SHIFT_TEMPS}
var
  dcval : int;
var
  dcval_ : JSAMPLE;
begin
{ Each IDCT routine is responsible for range-limiting its results and
  converting them to unsigned form (0..MAXJSAMPLE).  The raw outputs could
  be quite far out of range if the input data is corrupt, so a bulletproof
  range-limiting step is required.  We use a mask-and-table-lookup method
  to do the combined operations quickly.  See the comments with
  prepare_range_limit_table (in jdmaster.c) for more info. }

  range_limit := JSAMPROW(@(cinfo^.sample_range_limit^[CENTERJSAMPLE]));
  { Pass 1: process columns from input, store into work array. }

  inptr := coef_block;
  quantptr := ISLOW_MULT_TYPE_FIELD_PTR (compptr^.dct_table);
  wsptr := @workspace;
  for ctr := DCTSIZE downto 1 do
  begin
    { Don't bother to process columns 2,4,6 }
    if (ctr = DCTSIZE-2) or (ctr = DCTSIZE-4) or (ctr = DCTSIZE-6) then
    begin
      Inc(JCOEF_PTR(inptr));
      Inc(ISLOW_MULT_TYPE_PTR(quantptr));
      Inc(int_ptr(wsptr));

      continue;
    end;
    if (inptr^[DCTSIZE*1]=0) and (inptr^[DCTSIZE*3]=0) and
       (inptr^[DCTSIZE*5]=0) and (inptr^[DCTSIZE*7]=0) then
    begin
      { AC terms all zero; we need not examine terms 2,4,6 for 2x2 output }
      dcval := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*0]) *
                 quantptr^[DCTSIZE*0]) shl jidctred_PASS1_BITS;

      wsptr^[DCTSIZE*0] := dcval;
      wsptr^[DCTSIZE*1] := dcval;

      Inc(JCOEF_PTR(inptr));
      Inc(ISLOW_MULT_TYPE_PTR(quantptr));
      Inc(int_ptr(wsptr));

      continue;
    end;

    { Even part }

    z1 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*0]) * quantptr^[DCTSIZE*0]);

    tmp10 := z1 shl (jidctred_CONST_BITS+2);

    { Odd part }

    z1 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*7]) * quantptr^[DCTSIZE*7]);
    tmp0 := jidctred_MULTIPLY(z1, - FIX_0_720959822); { sqrt(2) * (c7-c5+c3-c1) }
    z1 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*5]) * quantptr^[DCTSIZE*5]);
    Inc(tmp0, jidctred_MULTIPLY(z1, FIX_0_850430095)); { sqrt(2) * (-c1+c3+c5+c7) }
    z1 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*3]) * quantptr^[DCTSIZE*3]);
    Inc(tmp0, jidctred_MULTIPLY(z1, - FIX_1_272758580)); { sqrt(2) * (-c1+c3-c5-c7) }
    z1 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*1]) * quantptr^[DCTSIZE*1]);
    Inc(tmp0, jidctred_MULTIPLY(z1, FIX_3_624509785)); { sqrt(2) * (c1+c3+c5+c7) }

    { Final output stage }

    wsptr^[DCTSIZE*0] := int (jidctred_DESCALE(tmp10 + tmp0, jidctred_CONST_BITS-jidctred_PASS1_BITS+2));
    wsptr^[DCTSIZE*1] := int (jidctred_DESCALE(tmp10 - tmp0, jidctred_CONST_BITS-jidctred_PASS1_BITS+2));

    Inc(JCOEF_PTR(inptr));
    Inc(ISLOW_MULT_TYPE_PTR(quantptr));
    Inc(int_ptr(wsptr));
  end;

  { Pass 2: process 2 rows from work array, store into output array. }

  wsptr := @workspace;
  for ctr := 0 to pred(2) do
  begin
    outptr := JSAMPROW(@ output_buf^[ctr]^[output_col]);
    { It's not clear whether a zero row test is worthwhile here ... }

{$ifndef NO_ZERO_ROW_TEST}
    if (wsptr^[1]=0) and (wsptr^[3]=0) and (wsptr^[5]=0) and (wsptr^[7]= 0) then
    begin
      { AC terms all zero }
      dcval_ := range_limit^[ int(jidctred_DESCALE(INT32(wsptr^[0]), jidctred_PASS1_BITS+3))
				  and RANGE_MASK];

      outptr^[0] := dcval_;
      outptr^[1] := dcval_;

      Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
      continue;
    end;
{$endif}

    { Even part }

    tmp10 := (INT32 (wsptr^[0])) shl (jidctred_CONST_BITS+2);

    { Odd part }

    tmp0 := jidctred_MULTIPLY( INT32(wsptr^[7]), - FIX_0_720959822) { sqrt(2) * (c7-c5+c3-c1) }
	  + jidctred_MULTIPLY( INT32(wsptr^[5]), FIX_0_850430095) { sqrt(2) * (-c1+c3+c5+c7) }
	  + jidctred_MULTIPLY( INT32(wsptr^[3]), - FIX_1_272758580) { sqrt(2) * (-c1+c3-c5-c7) }
	  + jidctred_MULTIPLY( INT32(wsptr^[1]), FIX_3_624509785); { sqrt(2) * (c1+c3+c5+c7) }

    { Final output stage }

    outptr^[0] := range_limit^[ int(jidctred_DESCALE(tmp10 + tmp0,
					  jidctred_CONST_BITS+jidctred_PASS1_BITS+3+2))
			    and RANGE_MASK];
    outptr^[1] := range_limit^[ int(jidctred_DESCALE(tmp10 - tmp0,
					  jidctred_CONST_BITS+jidctred_PASS1_BITS+3+2))
			    and RANGE_MASK];

    Inc(int_ptr(wsptr), DCTSIZE);		{ advance pointer to next row }
  end;
end;


{ Perform dequantization and inverse DCT on one block of coefficients,
  producing a reduced-size 1x1 output block. }

{GLOBAL}
procedure jpeg_idct_1x1 (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
	                 coef_block : JCOEFPTR;
	                 output_buf : JSAMPARRAY;
                         output_col : JDIMENSION);
var
  dcval : int;
  quantptr : ISLOW_MULT_TYPE_FIELD_PTR;
  range_limit : JSAMPROW;
  {SHIFT_TEMPS}
begin
{ Each IDCT routine is responsible for range-limiting its results and
  converting them to unsigned form (0..MAXJSAMPLE).  The raw outputs could
  be quite far out of range if the input data is corrupt, so a bulletproof
  range-limiting step is required.  We use a mask-and-table-lookup method
  to do the combined operations quickly.  See the comments with
  prepare_range_limit_table (in jdmaster.c) for more info. }

  range_limit := JSAMPROW(@(cinfo^.sample_range_limit^[CENTERJSAMPLE]));
  { Pass 1: process columns from input, store into work array. }

  { We hardly need an inverse DCT routine for this: just take the
    average pixel value, which is one-eighth of the DC coefficient. }

  quantptr := ISLOW_MULT_TYPE_FIELD_PTR (compptr^.dct_table);
  dcval := (ISLOW_MULT_TYPE(coef_block^[0]) * quantptr^[0]);
  dcval := int (jidctred_DESCALE( INT32(dcval), 3));

  output_buf^[0]^[output_col] := range_limit^[dcval and RANGE_MASK];
end;

{== imjccolor implementation ==}

{ Private subobject }
type
  INT32_FIELD = array[0..MaxInt div SizeOf(INT32) - 1] of INT32;
  INT32_FIELD_PTR = ^INT32_FIELD;

type
  jccolor_my_cconvert_ptr = ^my_color_converter;
  my_color_converter = record
    pub : jpeg_color_converter; { public fields }

    { Private state for RGB -> YCC conversion }
    rgb_ycc_tab : INT32_FIELD_PTR;	{ => table for RGB to YCbCr conversion }
  end; {my_color_converter;}


{*************** RGB -> YCbCr conversion: most common case *************}

{
  YCbCr is defined per CCIR 601-1, except that Cb and Cr are
  normalized to the range 0..MAXJSAMPLE rather than -0.5 .. 0.5.
  The conversion equations to be implemented are therefore
 	Y  =  0.29900 * R + 0.58700 * G + 0.11400 * B
 	Cb = -0.16874 * R - 0.33126 * G + 0.50000 * B  + CENTERJSAMPLE
 	Cr =  0.50000 * R - 0.41869 * G - 0.08131 * B  + CENTERJSAMPLE
  (These numbers are derived from TIFF 6.0 section 21, dated 3-June-92.)
  Note: older versions of the IJG code used a zero offset of MAXJSAMPLE/2,
  rather than CENTERJSAMPLE, for Cb and Cr.  This gave equal positive and
  negative swings for Cb/Cr, but meant that grayscale values (Cb=Cr=0)
  were not represented exactly.  Now we sacrifice exact representation of
  maximum red and maximum blue in order to get exact grayscales.

  To avoid floating-point arithmetic, we represent the fractional constants
  as integers scaled up by 2^16 (about 4 digits precision); we have to divide
  the products by 2^16, with appropriate rounding, to get the correct answer.

  For even more speed, we avoid doing any multiplications in the inner loop
  by precalculating the constants times R,G,B for all possible values.
  For 8-bit JSAMPLEs this is very reasonable (only 256 entries per table);
  for 12-bit samples it is still acceptable.  It's not very reasonable for
  16-bit samples, but if you want lossless storage you shouldn't be changing
  colorspace anyway.
  The CENTERJSAMPLE offsets and the rounding fudge-factor of 0.5 are included
  in the tables to save adding them separately in the inner loop. }
const
  jccolor_SCALEBITS   =  16;	{ speediest right-shift on some machines }
  CBCR_OFFSET = INT32(CENTERJSAMPLE shl jccolor_SCALEBITS);
  jccolor_ONE_HALF    = INT32(1) shl (jccolor_SCALEBITS-1);


{ We allocate one big table and divide it up into eight parts, instead of
  doing eight alloc_small requests.  This lets us use a single table base
  address, which can be held in a register in the inner loops on many
  machines (more than can hold all eight addresses, anyway). }

  R_Y_OFF     = 0;                              { offset to R => Y section }
  G_Y_OFF     = 1*(MAXJSAMPLE+1);               { offset to G => Y section }
  B_Y_OFF     = 2*(MAXJSAMPLE+1);               { etc. }
  R_CB_OFF    = 3*(MAXJSAMPLE+1);
  G_CB_OFF    = 4*(MAXJSAMPLE+1);
  B_CB_OFF    = 5*(MAXJSAMPLE+1);
  R_CR_OFF    = B_CB_OFF;                       { B=>Cb, R=>Cr are the same }
  G_CR_OFF    = 6*(MAXJSAMPLE+1);
  B_CR_OFF    = 7*(MAXJSAMPLE+1);
  TABLE_SIZE  = 8*(MAXJSAMPLE+1);


{ Initialize for RGB->YCC colorspace conversion. }

{METHODDEF}
procedure rgb_ycc_start (cinfo : j_compress_ptr);
const
  FIX_0_29900 = INT32(Round(0.29900 * (1 shl jccolor_SCALEBITS)));
  FIX_0_58700 = INT32(Round(0.58700 * (1 shl jccolor_SCALEBITS)));
  FIX_0_11400 = INT32(Round(0.11400 * (1 shl jccolor_SCALEBITS)));
  FIX_0_16874 = INT32(Round(0.16874 * (1 shl jccolor_SCALEBITS)));
  FIX_0_33126 = INT32(Round(0.33126 * (1 shl jccolor_SCALEBITS)));
  FIX_0_50000 = INT32(Round(0.50000 * (1 shl jccolor_SCALEBITS)));
  FIX_0_41869 = INT32(Round(0.41869 * (1 shl jccolor_SCALEBITS)));
  FIX_0_08131 = INT32(Round(0.08131 * (1 shl jccolor_SCALEBITS)));
var
  cconvert : jccolor_my_cconvert_ptr;
  rgb_ycc_tab : INT32_FIELD_PTR;
  i : INT32;
begin
  cconvert := jccolor_my_cconvert_ptr (cinfo^.cconvert);

  { Allocate and fill in the conversion tables. }
  rgb_ycc_tab := INT32_FIELD_PTR(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				(TABLE_SIZE * SIZEOF(INT32))) );
  cconvert^.rgb_ycc_tab := rgb_ycc_tab;

  for i := 0 to MAXJSAMPLE do
  begin
    rgb_ycc_tab^[i+R_Y_OFF] := FIX_0_29900 * i;
    rgb_ycc_tab^[i+G_Y_OFF] := FIX_0_58700 * i;
    rgb_ycc_tab^[i+B_Y_OFF] := FIX_0_11400 * i     + jccolor_ONE_HALF;
    rgb_ycc_tab^[i+R_CB_OFF] := (-FIX_0_16874) * i;
    rgb_ycc_tab^[i+G_CB_OFF] := (-FIX_0_33126) * i;
    { We use a rounding fudge-factor of 0.5-epsilon for Cb and Cr.
      This ensures that the maximum output will round to MAXJSAMPLE
      not MAXJSAMPLE+1, and thus that we don't have to range-limit. }

    rgb_ycc_tab^[i+B_CB_OFF] := FIX_0_50000 * i    + CBCR_OFFSET + jccolor_ONE_HALF-1;
{  B=>Cb and R=>Cr tables are the same
    rgb_ycc_tab^[i+R_CR_OFF] := FIX_0_50000 * i    + CBCR_OFFSET + ONE_HALF-1;
}
    rgb_ycc_tab^[i+G_CR_OFF] := (-FIX_0_41869) * i;
    rgb_ycc_tab^[i+B_CR_OFF] := (-FIX_0_08131) * i;
  end;
end;


{ Convert some rows of samples to the JPEG colorspace.

  Note that we change from the application's interleaved-pixel format
  to our internal noninterleaved, one-plane-per-component format.
  The input buffer is therefore three times as wide as the output buffer.

  A starting row offset is provided only for the output buffer.  The caller
  can easily adjust the passed input_buf value to accommodate any row
  offset required on that side. }

{METHODDEF}
procedure rgb_ycc_convert (cinfo : j_compress_ptr;
		           input_buf : JSAMPARRAY;
                           output_buf :  JSAMPIMAGE;
		           output_row : JDIMENSION;
                           num_rows : int);
var
  cconvert : jccolor_my_cconvert_ptr;
  {register} r, g, b : int;
  {register} ctab : INT32_FIELD_PTR;
  {register} inptr : JSAMPROW;
  {register} outptr0, outptr1, outptr2 : JSAMPROW;
  {register} col : JDIMENSION;
  num_cols : JDIMENSION;
begin
  cconvert := jccolor_my_cconvert_ptr (cinfo^.cconvert);
  ctab := cconvert^.rgb_ycc_tab;
  num_cols := cinfo^.image_width;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    inptr := input_buf^[0];
    Inc(JSAMPROW_PTR(input_buf));
    outptr0 := output_buf^[0]^[output_row];
    outptr1 := output_buf^[1]^[output_row];
    outptr2 := output_buf^[2]^[output_row];
    Inc(output_row);
    for col := 0 to pred(num_cols) do
    begin
      r := GETJSAMPLE(inptr^[RGB_RED]);
      g := GETJSAMPLE(inptr^[RGB_GREEN]);
      b := GETJSAMPLE(inptr^[RGB_BLUE]);
      Inc(JSAMPLE_PTR(inptr), RGB_PIXELSIZE);
      { If the inputs are 0..MAXJSAMPLE, the outputs of these equations
        must be too; we do not need an explicit range-limiting operation.
        Hence the value being shifted is never negative, and we don't
        need the general RIGHT_SHIFT macro. }

      { Y }
      outptr0^[col] := JSAMPLE(
		((ctab^[r+R_Y_OFF] + ctab^[g+G_Y_OFF] + ctab^[b+B_Y_OFF])
		 shr jccolor_SCALEBITS) );
      { Cb }
      outptr1^[col] := JSAMPLE(
		((ctab^[r+R_CB_OFF] + ctab^[g+G_CB_OFF] + ctab^[b+B_CB_OFF])
		 shr jccolor_SCALEBITS) );
      { Cr }
      outptr2^[col] := JSAMPLE(
		((ctab^[r+R_CR_OFF] + ctab^[g+G_CR_OFF] + ctab^[b+B_CR_OFF])
		 shr jccolor_SCALEBITS) );
    end;
  end;
end;


{*************** Cases other than RGB -> YCbCr *************}


{ Convert some rows of samples to the JPEG colorspace.
  This version handles RGB -> grayscale conversion, which is the same
  as the RGB -> Y portion of RGB -> YCbCr.
  We assume rgb_ycc_start has been called (we only use the Y tables). }

{METHODDEF}
procedure rgb_gray_convert (cinfo : j_compress_ptr;
		            input_buf : JSAMPARRAY;
                            output_buf : JSAMPIMAGE;
		            output_row : JDIMENSION;
                            num_rows : int);
var
  cconvert : jccolor_my_cconvert_ptr;
  {register} r, g, b : int;
  {register} ctab :INT32_FIELD_PTR;
  {register} inptr : JSAMPROW;
  {register} outptr : JSAMPROW;
  {register} col : JDIMENSION;
  num_cols : JDIMENSION;
begin
  cconvert := jccolor_my_cconvert_ptr (cinfo^.cconvert);
  ctab := cconvert^.rgb_ycc_tab;
  num_cols := cinfo^.image_width;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    inptr := input_buf[0];
    Inc(JSAMPROW_PTR(input_buf));
    outptr := output_buf[0][output_row];
    Inc(output_row);
    for col := 0 to num_cols - 1 do
    begin
      r := GETJSAMPLE(inptr[RGB_RED]);
      g := GETJSAMPLE(inptr[RGB_GREEN]);
      b := GETJSAMPLE(inptr[RGB_BLUE]);
      Inc(JSAMPLE_PTR(inptr), RGB_PIXELSIZE);
      (* Y *)
      // kylix 3 compiler crashes on this
      // it also crashes Delphi OSX compiler 9 years later :(
      {$IF not (Defined(DCC) and not Defined(MSWINDOWS))}
      outptr[col] := JSAMPLE(((ctab[r+R_Y_OFF] + ctab[g+G_Y_OFF] + ctab[b+B_Y_OFF]) shr jccolor_SCALEBITS));
      {$IFEND}
    end;
  end;
end;


{ Convert some rows of samples to the JPEG colorspace.
  This version handles Adobe-style CMYK -> YCCK conversion,
  where we convert R=1-C, G=1-M, and B=1-Y to YCbCr using the same
  conversion as above, while passing K (black) unchanged.
  We assume rgb_ycc_start has been called. }

{METHODDEF}
procedure cmyk_ycck_convert (cinfo : j_compress_ptr;
		             input_buf : JSAMPARRAY;
                             output_buf : JSAMPIMAGE;
		             output_row : JDIMENSION;
                             num_rows : int);
var
  cconvert : jccolor_my_cconvert_ptr;
  {register} r, g, b : int;
  {register} ctab : INT32_FIELD_PTR;
  {register} inptr : JSAMPROW;
  {register} outptr0, outptr1, outptr2, outptr3 : JSAMPROW;
  {register} col : JDIMENSION;
  num_cols : JDIMENSION;
begin
  cconvert := jccolor_my_cconvert_ptr (cinfo^.cconvert);
  ctab := cconvert^.rgb_ycc_tab;
  num_cols := cinfo^.image_width;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    inptr := input_buf^[0];
    Inc(JSAMPROW_PTR(input_buf));
    outptr0 := output_buf^[0]^[output_row];
    outptr1 := output_buf^[1]^[output_row];
    outptr2 := output_buf^[2]^[output_row];
    outptr3 := output_buf^[3]^[output_row];
    Inc(output_row);
    for col := 0 to pred(num_cols) do
    begin
      r := MAXJSAMPLE - GETJSAMPLE(inptr^[0]);
      g := MAXJSAMPLE - GETJSAMPLE(inptr^[1]);
      b := MAXJSAMPLE - GETJSAMPLE(inptr^[2]);
      { K passes through as-is }
      outptr3^[col] := inptr^[3];	{ don't need GETJSAMPLE here }
      Inc(JSAMPLE_PTR(inptr), 4);
      { If the inputs are 0..MAXJSAMPLE, the outputs of these equations
        must be too; we do not need an explicit range-limiting operation.
        Hence the value being shifted is never negative, and we don't
        need the general RIGHT_SHIFT macro. }

      { Y }
      outptr0^[col] := JSAMPLE (
		((ctab^[r+R_Y_OFF] + ctab^[g+G_Y_OFF] + ctab^[b+B_Y_OFF])
		 shr jccolor_SCALEBITS) );
      { Cb }
      outptr1^[col] := JSAMPLE(
		((ctab^[r+R_CB_OFF] + ctab^[g+G_CB_OFF] + ctab^[b+B_CB_OFF])
		 shr jccolor_SCALEBITS) );
      { Cr }
      outptr2^[col] := JSAMPLE (
		((ctab^[r+R_CR_OFF] + ctab^[g+G_CR_OFF] + ctab^[b+B_CR_OFF])
		 shr jccolor_SCALEBITS) );
    end;
  end;
end;


{ Convert some rows of samples to the JPEG colorspace.
  This version handles grayscale output with no conversion.
  The source can be either plain grayscale or YCbCr (since Y = gray). }

{METHODDEF}
procedure jccolor_grayscale_convert (cinfo : j_compress_ptr;
                            input_buf : JSAMPARRAY;
                            output_buf : JSAMPIMAGE;
                            output_row : JDIMENSION;
                            num_rows: int);
var
  {register} inptr : JSAMPROW;
  {register} outptr : JSAMPROW;
  {register} col : JDIMENSION;
  num_cols :JDIMENSION;
  instride : int;
begin
  num_cols := cinfo^.image_width;
  instride := cinfo^.input_components;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    inptr := input_buf^[0];
    Inc(JSAMPROW_PTR(input_buf));
    outptr := output_buf^[0]^[output_row];
    Inc(output_row);
    for col := 0 to pred(num_cols) do
    begin
      outptr^[col] := inptr^[0];	{ don't need GETJSAMPLE() here }
      Inc(JSAMPLE_PTR(inptr), instride);
    end;
  end;
end;


{ Convert some rows of samples to the JPEG colorspace.
  This version handles multi-component colorspaces without conversion.
  We assume input_components = num_components. }

{METHODDEF}
procedure jccolor_null_convert (cinfo : j_compress_ptr;
	                input_buf : JSAMPARRAY;
                        output_buf : JSAMPIMAGE;
                        output_row : JDIMENSION;
                        num_rows : int);
var
  {register} inptr : JSAMPROW;
  {register} outptr : JSAMPROW;
  {register} col : JDIMENSION;
  {register} ci : int;
  nc : int;
  num_cols : JDIMENSION;
begin
  nc := cinfo^.num_components;
  num_cols := cinfo^.image_width;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    { It seems fastest to make a separate pass for each component. }
    for ci := 0 to pred(nc) do
    begin
      inptr := input_buf^[0];
      outptr := output_buf^[ci]^[output_row];
      for col := 0 to pred(num_cols) do
      begin
	outptr^[col] := inptr^[ci]; { don't need GETJSAMPLE() here }
	Inc(JSAMPLE_PTR(inptr), nc);
      end;
    end;
    Inc(JSAMPROW_PTR(input_buf));
    Inc(output_row);
  end;
end;


{ Empty method for start_pass. }

{METHODDEF}
procedure null_method (cinfo : j_compress_ptr);
begin
  { no work needed }
end;


{ Module initialization routine for input colorspace conversion. }

{GLOBAL}
procedure jinit_color_converter (cinfo : j_compress_ptr);
var
  cconvert : jccolor_my_cconvert_ptr;
begin
  cconvert := jccolor_my_cconvert_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(my_color_converter)) );
  cinfo^.cconvert := jpeg_color_converter_ptr(cconvert);
  { set start_pass to null method until we find out differently }
  cconvert^.pub.start_pass := null_method;

  { Make sure input_components agrees with in_color_space }
  case (cinfo^.in_color_space) of
  JCS_GRAYSCALE:
    if (cinfo^.input_components <> 1) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_IN_COLORSPACE);

{$ifdef RGB_PIXELSIZE <> 3}
  JCS_RGB:
    if (cinfo^.input_components <> RGB_PIXELSIZE) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_IN_COLORSPACE);
{$else} { share code with YCbCr }
  JCS_RGB,
{$endif}
  JCS_YCbCr:
    if (cinfo^.input_components <> 3) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_IN_COLORSPACE);

  JCS_CMYK,
  JCS_YCCK:
    if (cinfo^.input_components <> 4) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_IN_COLORSPACE);

  else			{ JCS_UNKNOWN can be anything }
    if (cinfo^.input_components < 1) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_IN_COLORSPACE);
  end;

  { Check num_components, set conversion method based on requested space }
  case (cinfo^.jpeg_color_space) of
  JCS_GRAYSCALE:
    begin
      if (cinfo^.num_components <> 1) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);
      if (cinfo^.in_color_space = JCS_GRAYSCALE) then
        cconvert^.pub.color_convert := jccolor_grayscale_convert
      else
        if (cinfo^.in_color_space = JCS_RGB) then
        begin
          cconvert^.pub.start_pass := rgb_ycc_start;
          cconvert^.pub.color_convert := rgb_gray_convert;
        end
        else
          if (cinfo^.in_color_space = JCS_YCbCr) then
            cconvert^.pub.color_convert := jccolor_grayscale_convert
          else
            ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  JCS_RGB:
    begin
      if (cinfo^.num_components <> 3) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);
        if (cinfo^.in_color_space = JCS_RGB) and (RGB_PIXELSIZE = 3) then
          cconvert^.pub.color_convert := jccolor_null_convert
        else
          ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  JCS_YCbCr:
    begin
      if (cinfo^.num_components <> 3) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);
      if (cinfo^.in_color_space = JCS_RGB) then
      begin
        cconvert^.pub.start_pass := rgb_ycc_start;
        cconvert^.pub.color_convert := rgb_ycc_convert;
      end
      else
        if (cinfo^.in_color_space = JCS_YCbCr) then
          cconvert^.pub.color_convert := jccolor_null_convert
        else
          ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  JCS_CMYK:
    begin
      if (cinfo^.num_components <> 4) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);
      if (cinfo^.in_color_space = JCS_CMYK) then
        cconvert^.pub.color_convert := jccolor_null_convert
      else
        ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  JCS_YCCK:
    begin
      if (cinfo^.num_components <> 4) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);
      if (cinfo^.in_color_space = JCS_CMYK) then
      begin
        cconvert^.pub.start_pass := rgb_ycc_start;
        cconvert^.pub.color_convert := cmyk_ycck_convert;
      end
      else
        if (cinfo^.in_color_space = JCS_YCCK) then
          cconvert^.pub.color_convert := jccolor_null_convert
        else
          ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  else		{ allow null conversion of JCS_UNKNOWN }
    begin
      if (cinfo^.jpeg_color_space <> cinfo^.in_color_space) or
	 (cinfo^.num_components <> cinfo^.input_components) then
        ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
      cconvert^.pub.color_convert := jccolor_null_convert;
    end;
  end;
end;

{== imjcsample implementation ==}

{ Pointer to routine to downsample a single component }
type
  downsample1_ptr = procedure(cinfo : j_compress_ptr;
                              compptr : jpeg_component_info_ptr;
		              input_data : JSAMPARRAY;
                              output_data : JSAMPARRAY);

{ Private subobject }

type
  my_downsample_ptr = ^my_downsampler;
  my_downsampler = record
    pub : jpeg_downsampler;	{ public fields }

    { Downsampling method pointers, one per component }
    methods : array[0..MAX_COMPONENTS-1] of downsample1_ptr;
  end;

{ Initialize for a downsampling pass. }

{METHODDEF}
procedure start_pass_downsample (cinfo : j_compress_ptr);
begin
  { no work for now }
end;


{ Expand a component horizontally from width input_cols to width output_cols,
  by duplicating the rightmost samples. }

{LOCAL}
procedure expand_right_edge (image_data : JSAMPARRAY;
                             num_rows : int;
		             input_cols : JDIMENSION;
                             output_cols : JDIMENSION);
var
  {register} ptr : JSAMPLE_PTR;
  {register} pixval : JSAMPLE;
  {register} count : int;
  row : int;
  numcols : int;
begin
  numcols := int (output_cols - input_cols);

  if (numcols > 0) then
  begin
    for row := 0 to pred(num_rows) do
    begin
      ptr := JSAMPLE_PTR(@(image_data^[row]^[input_cols-1]));
      pixval := ptr^;		{ don't need GETJSAMPLE() here }
      for count := pred(numcols) downto 0 do
      begin
        Inc(ptr);
	ptr^ := pixval;
      end;
    end;
  end;
end;


{ Do downsampling for a whole row group (all components).

  In this version we simply downsample each component independently. }

{METHODDEF}
procedure sep_downsample (cinfo : j_compress_ptr;
		          input_buf : JSAMPIMAGE;
                          in_row_index : JDIMENSION;
		          output_buf : JSAMPIMAGE;
                          out_row_group_index : JDIMENSION);
var
  downsample : my_downsample_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
  in_ptr, out_ptr : JSAMPARRAY;
begin
  downsample := my_downsample_ptr (cinfo^.downsample);

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    in_ptr := JSAMPARRAY(@ input_buf^[ci]^[in_row_index]);
    out_ptr := JSAMPARRAY(@ output_buf^[ci]^
                     [out_row_group_index * JDIMENSION(compptr^.v_samp_factor)]);
    downsample^.methods[ci] (cinfo, compptr, in_ptr, out_ptr);
    Inc(compptr);
  end;
end;


{ Downsample pixel values of a single component.
  One row group is processed per call.
  This version handles arbitrary integral sampling ratios, without smoothing.
  Note that this version is not actually used for customary sampling ratios. }

{METHODDEF}
procedure int_downsample (cinfo : j_compress_ptr;
                          compptr : jpeg_component_info_ptr;
		          input_data : JSAMPARRAY;
                          output_data : JSAMPARRAY);
var
  inrow, outrow, h_expand, v_expand, numpix, numpix2, h, v : int;
  outcol, outcol_h :  JDIMENSION;	{ outcol_h = outcol*h_expand }
  output_cols : JDIMENSION;
  inptr,
  outptr : JSAMPLE_PTR;
  outvalue : INT32;
begin
  output_cols := compptr^.width_in_blocks * DCTSIZE;

  h_expand := cinfo^.max_h_samp_factor div compptr^.h_samp_factor;
  v_expand := cinfo^.max_v_samp_factor div compptr^.v_samp_factor;
  numpix := h_expand * v_expand;
  numpix2 := numpix div 2;

  { Expand input data enough to let all the output samples be generated
    by the standard loop.  Special-casing padded output would be more
    efficient. }

  expand_right_edge(input_data, cinfo^.max_v_samp_factor,
		    cinfo^.image_width, output_cols * JDIMENSION(h_expand));

  inrow := 0;
  for outrow := 0 to pred(compptr^.v_samp_factor) do
  begin
    outptr := JSAMPLE_PTR(output_data^[outrow]);
    outcol_h := 0;
    for outcol := 0 to pred(output_cols) do
    begin
      outvalue := 0;
      for v := 0 to pred(v_expand) do
      begin
	inptr := @(input_data^[inrow+v]^[outcol_h]);
	for h := 0 to pred(h_expand) do
        begin
	  Inc(outvalue, INT32 (GETJSAMPLE(inptr^)) );
          Inc(inptr);
	end;
      end;
      outptr^ := JSAMPLE ((outvalue + numpix2) div numpix);
      Inc(outptr);
      Inc(outcol_h, h_expand);
    end;
    Inc(inrow, v_expand);
  end;
end;


{ Downsample pixel values of a single component.
  This version handles the special case of a full-size component,
  without smoothing. }

{METHODDEF}
procedure fullsize_downsample (cinfo : j_compress_ptr;
                               compptr : jpeg_component_info_ptr;
		               input_data : JSAMPARRAY;
                               output_data : JSAMPARRAY);
begin
  { Copy the data }
  jcopy_sample_rows(input_data, 0, output_data, 0,
		    cinfo^.max_v_samp_factor, cinfo^.image_width);
  { Edge-expand }
  expand_right_edge(output_data, cinfo^.max_v_samp_factor,
		    cinfo^.image_width, compptr^.width_in_blocks * DCTSIZE);
end;


{ Downsample pixel values of a single component.
  This version handles the common case of 2:1 horizontal and 1:1 vertical,
  without smoothing.

  A note about the "bias" calculations: when rounding fractional values to
  integer, we do not want to always round 0.5 up to the next integer.
  If we did that, we'd introduce a noticeable bias towards larger values.
  Instead, this code is arranged so that 0.5 will be rounded up or down at
  alternate pixel locations (a simple ordered dither pattern). }

{METHODDEF}
procedure h2v1_downsample (cinfo : j_compress_ptr;
                           compptr : jpeg_component_info_ptr;
		           input_data : JSAMPARRAY;
                           output_data : JSAMPARRAY);
var
  outrow : int;
  outcol : JDIMENSION;
  output_cols : JDIMENSION;
  {register} inptr, outptr : JSAMPLE_PTR;
  {register} bias : int;
begin
  output_cols := compptr^.width_in_blocks * DCTSIZE;

  { Expand input data enough to let all the output samples be generated
    by the standard loop.  Special-casing padded output would be more
    efficient. }

  expand_right_edge(input_data, cinfo^.max_v_samp_factor,
		    cinfo^.image_width, output_cols * 2);

  for outrow := 0 to pred(compptr^.v_samp_factor) do
  begin
    outptr := JSAMPLE_PTR(output_data^[outrow]);
    inptr := JSAMPLE_PTR(input_data^[outrow]);
    bias := 0;		     { bias := 0,1,0,1,... for successive samples }
    for outcol := 0 to pred(output_cols) do
    begin
      outptr^ := JSAMPLE ((GETJSAMPLE(inptr^) +
                           GETJSAMPLE(JSAMPROW(inptr)^[1]) + bias) shr 1);
      Inc(outptr);
      bias := bias xor 1;    { 0=>1, 1=>0 }
      Inc(inptr, 2);
    end;
  end;
end;


{ Downsample pixel values of a single component.
  This version handles the standard case of 2:1 horizontal and 2:1 vertical,
  without smoothing. }

{METHODDEF}
procedure h2v2_downsample (cinfo : j_compress_ptr;
                           compptr : jpeg_component_info_ptr;
                           input_data : JSAMPARRAY;
                           output_data : JSAMPARRAY);
var
  inrow, outrow : int;
  outcol : JDIMENSION;
  output_cols : JDIMENSION;
  {register} inptr0, inptr1, outptr : JSAMPLE_PTR;
  {register} bias : int;
begin
  output_cols := compptr^.width_in_blocks * DCTSIZE;

  { Expand input data enough to let all the output samples be generated
    by the standard loop.  Special-casing padded output would be more
    efficient. }

  expand_right_edge(input_data, cinfo^.max_v_samp_factor,
		    cinfo^.image_width, output_cols * 2);

  inrow := 0;
  for outrow := 0 to pred(compptr^.v_samp_factor) do
  begin
    outptr := JSAMPLE_PTR(output_data^[outrow]);
    inptr0 := JSAMPLE_PTR(input_data^[inrow]);
    inptr1 := JSAMPLE_PTR(input_data^[inrow+1]);
    bias := 1;			{ bias := 1,2,1,2,... for successive samples }
    for outcol := 0 to pred(output_cols) do
    begin
      outptr^ := JSAMPLE ((GETJSAMPLE(inptr0^) +
                           GETJSAMPLE(JSAMPROW(inptr0)^[1]) +
		           GETJSAMPLE(inptr1^) +
                           GETJSAMPLE(JSAMPROW(inptr1)^[1]) + bias) shr 2);
      Inc(outptr);
      bias := bias xor 3;       { 1=>2, 2=>1 }
      Inc(inptr0, 2);
      Inc(inptr1, 2);
    end;
    Inc(inrow, 2);
  end;
end;


{$ifdef INPUT_SMOOTHING_SUPPORTED}

{ Downsample pixel values of a single component.
  This version handles the standard case of 2:1 horizontal and 2:1 vertical,
  with smoothing.  One row of context is required. }

{METHODDEF}
procedure h2v2_smooth_downsample (cinfo : j_compress_ptr;
                                  compptr : jpeg_component_info_ptr;
                                  input_data : JSAMPARRAY;
                                  output_data : JSAMPARRAY);
var
  inrow, outrow : int;
  colctr : JDIMENSION;
  output_cols : JDIMENSION;
  {register} inptr0, inptr1, above_ptr, below_ptr, outptr : JSAMPLE_PTR;
  membersum, neighsum, memberscale, neighscale : INT32;
var
  prev_input_data : JSAMPARRAY;
  prev_inptr0, prev_inptr1, prev_above_ptr, prev_below_ptr : JSAMPLE_PTR;
begin
  output_cols := compptr^.width_in_blocks * DCTSIZE;

  { Expand input data enough to let all the output samples be generated
    by the standard loop.  Special-casing padded output would be more
    efficient. }

  prev_input_data := input_data;
  Dec(JSAMPROW_PTR(prev_input_data));
  expand_right_edge(prev_input_data, cinfo^.max_v_samp_factor + 2,
		    cinfo^.image_width, output_cols * 2);

  { We don't bother to form the individual "smoothed" input pixel values;
    we can directly compute the output which is the average of the four
    smoothed values.  Each of the four member pixels contributes a fraction
    (1-8*SF) to its own smoothed image and a fraction SF to each of the three
    other smoothed pixels, therefore a total fraction (1-5*SF)/4 to the final
    output.  The four corner-adjacent neighbor pixels contribute a fraction
    SF to just one smoothed pixel, or SF/4 to the final output; while the
    eight edge-adjacent neighbors contribute SF to each of two smoothed
    pixels, or SF/2 overall.  In order to use integer arithmetic, these
    factors are scaled by 2^16 := 65536.
    Also recall that SF := smoothing_factor / 1024. }

  memberscale := 16384 - cinfo^.smoothing_factor * 80; { scaled (1-5*SF)/4 }
  neighscale := cinfo^.smoothing_factor * 16; { scaled SF/4 }

  inrow := 0;
  for outrow := 0 to pred(compptr^.v_samp_factor) do
  begin
    outptr := JSAMPLE_PTR(output_data^[outrow]);
    inptr0 := JSAMPLE_PTR(input_data^[inrow]);
    inptr1 := JSAMPLE_PTR(input_data^[inrow+1]);
    above_ptr := JSAMPLE_PTR(input_data^[inrow-1]);
    below_ptr := JSAMPLE_PTR(input_data^[inrow+2]);

    { Special case for first column: pretend column -1 is same as column 0 }
    membersum := GETJSAMPLE(inptr0^) + GETJSAMPLE(JSAMPROW(inptr0)^[1]) +
		GETJSAMPLE(inptr1^) + GETJSAMPLE(JSAMPROW(inptr1)^[1]);
    neighsum := GETJSAMPLE(above_ptr^) + GETJSAMPLE(JSAMPROW(above_ptr)^[1]) +
	       GETJSAMPLE(below_ptr^) + GETJSAMPLE(JSAMPROW(below_ptr)^[1]) +
	       GETJSAMPLE(inptr0^) + GETJSAMPLE(JSAMPROW(inptr0)^[2]) +
	       GETJSAMPLE(inptr1^) + GETJSAMPLE(JSAMPROW(inptr1)^[2]);
    Inc(neighsum, neighsum);
    Inc(neighsum, GETJSAMPLE(above_ptr^) +
                  GETJSAMPLE(JSAMPROW(above_ptr)^[2]) +
		  GETJSAMPLE(below_ptr^) +
                  GETJSAMPLE(JSAMPROW(below_ptr)^[2]) );
    membersum := membersum * memberscale + neighsum * neighscale;
    outptr^ := JSAMPLE ((membersum + 32768) shr 16);
    Inc(outptr);
    prev_inptr0 := inptr0;
    prev_inptr1 := inptr1;
    Inc(prev_inptr0);
    Inc(prev_inptr1);
    Inc(inptr0, 2);
    Inc(inptr1, 2);
    prev_above_ptr := above_ptr;
    prev_below_ptr := below_ptr;
    Inc(above_ptr, 2);
    Inc(below_ptr, 2);
    Inc(prev_above_ptr, 1);
    Inc(prev_below_ptr, 1);

    for colctr := pred(output_cols - 2) downto 0 do
    begin
      { sum of pixels directly mapped to this output element }
      membersum := GETJSAMPLE(inptr0^) + GETJSAMPLE(JSAMPROW(inptr0)^[1]) +
                   GETJSAMPLE(inptr1^) + GETJSAMPLE(JSAMPROW(inptr1)^[1]);
      { sum of edge-neighbor pixels }
      neighsum := GETJSAMPLE(above_ptr^) + GETJSAMPLE(JSAMPROW(above_ptr)^[1]) +
                  GETJSAMPLE(below_ptr^) + GETJSAMPLE(JSAMPROW(below_ptr)^[1]) +
                  GETJSAMPLE(prev_inptr0^) + GETJSAMPLE(JSAMPROW(inptr0)^[2]) +
                  GETJSAMPLE(prev_inptr1^) + GETJSAMPLE(JSAMPROW(inptr1)^[2]);
      { The edge-neighbors count twice as much as corner-neighbors }
      Inc(neighsum, neighsum);
      { Add in the corner-neighbors }
      Inc(neighsum, GETJSAMPLE(prev_above_ptr^) +
                    GETJSAMPLE(JSAMPROW(above_ptr)^[2]) +
		    GETJSAMPLE(prev_below_ptr^) +
                    GETJSAMPLE(JSAMPROW(below_ptr)^[2]) );
      { form final output scaled up by 2^16 }
      membersum := membersum * memberscale + neighsum * neighscale;
      { round, descale and output it }
      outptr^ := JSAMPLE ((membersum + 32768) shr 16);
      Inc(outptr);
      Inc(inptr0, 2);
      Inc(inptr1, 2);
      Inc(prev_inptr0, 2);
      Inc(prev_inptr1, 2);
      Inc(above_ptr, 2);
      Inc(below_ptr, 2);
      Inc(prev_above_ptr, 2);
      Inc(prev_below_ptr, 2);
    end;

    { Special case for last column }
    membersum := GETJSAMPLE(inptr0^) + GETJSAMPLE(JSAMPROW(inptr0)^[1]) +
		 GETJSAMPLE(inptr1^) + GETJSAMPLE(JSAMPROW(inptr1)^[1]);
    neighsum := GETJSAMPLE(above_ptr^) + GETJSAMPLE(JSAMPROW(above_ptr)^[1]) +
	        GETJSAMPLE(below_ptr^) + GETJSAMPLE(JSAMPROW(below_ptr)^[1]) +
	        GETJSAMPLE(prev_inptr0^) + GETJSAMPLE(JSAMPROW(inptr0)^[1]) +
	        GETJSAMPLE(prev_inptr1^) + GETJSAMPLE(JSAMPROW(inptr1)^[1]);
    Inc(neighsum, neighsum);
    Inc(neighsum, GETJSAMPLE(prev_above_ptr^) +
                  GETJSAMPLE(JSAMPROW(above_ptr)^[1]) +
		  GETJSAMPLE(prev_below_ptr^) +
                  GETJSAMPLE(JSAMPROW(below_ptr)^[1]) );
    membersum := membersum * memberscale + neighsum * neighscale;
    outptr^ := JSAMPLE ((membersum + 32768) shr 16);

    Inc(inrow, 2);
  end;
end;


{ Downsample pixel values of a single component.
  This version handles the special case of a full-size component,
  with smoothing.  One row of context is required. }

{METHODDEF}
procedure fullsize_smooth_downsample (cinfo : j_compress_ptr;
                                      compptr : jpeg_component_info_ptr;
			              input_data : JSAMPARRAY;
                                      output_data : JSAMPARRAY);
var
  outrow : int;
  colctr : JDIMENSION;
  output_cols : JDIMENSION;
  {register} inptr, above_ptr, below_ptr, outptr : JSAMPLE_PTR;
  membersum, neighsum, memberscale, neighscale : INT32;
  colsum, lastcolsum, nextcolsum : int;
var
  prev_input_data : JSAMPARRAY;
begin
  output_cols := compptr^.width_in_blocks * DCTSIZE;

  { Expand input data enough to let all the output samples be generated
    by the standard loop.  Special-casing padded output would be more
    efficient. }

  prev_input_data := input_data;
  Dec(JSAMPROW_PTR(prev_input_data));
  expand_right_edge(prev_input_data, cinfo^.max_v_samp_factor + 2,
		    cinfo^.image_width, output_cols);

  { Each of the eight neighbor pixels contributes a fraction SF to the
    smoothed pixel, while the main pixel contributes (1-8*SF).  In order
    to use integer arithmetic, these factors are multiplied by 2^16 := 65536.
    Also recall that SF := smoothing_factor / 1024. }

  memberscale := long(65536) - cinfo^.smoothing_factor * long(512); { scaled 1-8*SF }
  neighscale := cinfo^.smoothing_factor * 64; { scaled SF }

  for outrow := 0 to pred(compptr^.v_samp_factor) do
  begin
    outptr := JSAMPLE_PTR(output_data^[outrow]);
    inptr := JSAMPLE_PTR(input_data^[outrow]);
    above_ptr := JSAMPLE_PTR(input_data^[outrow-1]);
    below_ptr := JSAMPLE_PTR(input_data^[outrow+1]);

    { Special case for first column }
    colsum := GETJSAMPLE(above_ptr^) + GETJSAMPLE(below_ptr^) +
	     GETJSAMPLE(inptr^);
    Inc(above_ptr);
    Inc(below_ptr);
    membersum := GETJSAMPLE(inptr^);
    Inc(inptr);
    nextcolsum := GETJSAMPLE(above_ptr^) + GETJSAMPLE(below_ptr^) +
		  GETJSAMPLE(inptr^);
    neighsum := colsum + (colsum - membersum) + nextcolsum;
    membersum := membersum * memberscale + neighsum * neighscale;
    outptr^ := JSAMPLE ((membersum + 32768) shr 16);
    Inc(outptr);
    lastcolsum := colsum; colsum := nextcolsum;

    for colctr := pred(output_cols - 2) downto 0 do
    begin
      membersum := GETJSAMPLE(inptr^);
      Inc(inptr);
      Inc(above_ptr);
      Inc(below_ptr);
      nextcolsum := GETJSAMPLE(above_ptr^) + GETJSAMPLE(below_ptr^) +
		    GETJSAMPLE(inptr^);
      neighsum := lastcolsum + (colsum - membersum) + nextcolsum;
      membersum := membersum * memberscale + neighsum * neighscale;
      outptr^ := JSAMPLE ((membersum + 32768) shr 16);
      Inc(outptr);
      lastcolsum := colsum; colsum := nextcolsum;
    end;

    { Special case for last column }
    membersum := GETJSAMPLE(inptr^);
    neighsum := lastcolsum + (colsum - membersum) + colsum;
    membersum := membersum * memberscale + neighsum * neighscale;
    outptr^ := JSAMPLE ((membersum + 32768) shr 16);
  end;
end;

{$endif} { INPUT_SMOOTHING_SUPPORTED }


{ Module initialization routine for downsampling.
  Note that we must select a routine for each component. }

{GLOBAL}
procedure jinit_downsampler (cinfo : j_compress_ptr);
var
  downsample : my_downsample_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
  smoothok : boolean;
begin
  smoothok := TRUE;

  downsample := my_downsample_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(my_downsampler)) );
  cinfo^.downsample := jpeg_downsampler_ptr (downsample);
  downsample^.pub.start_pass := start_pass_downsample;
  downsample^.pub.downsample := sep_downsample;
  downsample^.pub.need_context_rows := FALSE;

  if (cinfo^.CCIR601_sampling) then
    ERREXIT(j_common_ptr(cinfo), JERR_CCIR601_NOTIMPL);

  { Verify we can handle the sampling factors, and set up method pointers }
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    if (compptr^.h_samp_factor = cinfo^.max_h_samp_factor) and
       (compptr^.v_samp_factor = cinfo^.max_v_samp_factor) then
    begin
{$ifdef INPUT_SMOOTHING_SUPPORTED}
      if (cinfo^.smoothing_factor <> 0) then
      begin
	downsample^.methods[ci] := fullsize_smooth_downsample;
	downsample^.pub.need_context_rows := TRUE;
      end
      else
{$endif}
	downsample^.methods[ci] := fullsize_downsample;
    end
    else
      if (compptr^.h_samp_factor * 2 = cinfo^.max_h_samp_factor) and
         (compptr^.v_samp_factor = cinfo^.max_v_samp_factor) then
      begin
        smoothok := FALSE;
        downsample^.methods[ci] := h2v1_downsample;
      end
      else
        if (compptr^.h_samp_factor * 2 = cinfo^.max_h_samp_factor) and
	   (compptr^.v_samp_factor * 2 = cinfo^.max_v_samp_factor) then
        begin
  {$ifdef INPUT_SMOOTHING_SUPPORTED}
        if (cinfo^.smoothing_factor <> 0) then
        begin
	  downsample^.methods[ci] := h2v2_smooth_downsample;
	  downsample^.pub.need_context_rows := TRUE;
        end
        else
  {$endif}
          downsample^.methods[ci] := h2v2_downsample;
        end
        else
          if ((cinfo^.max_h_samp_factor mod compptr^.h_samp_factor) = 0) and
	     ((cinfo^.max_v_samp_factor mod compptr^.v_samp_factor) = 0) then
          begin
            smoothok := FALSE;
            downsample^.methods[ci] := int_downsample;
          end
          else
            ERREXIT(j_common_ptr(cinfo), JERR_FRACT_SAMPLE_NOTIMPL);
    Inc(compptr);
  end;

{$ifdef INPUT_SMOOTHING_SUPPORTED}
  if (cinfo^.smoothing_factor <> 0) and (not smoothok) then
    TRACEMS(j_common_ptr(cinfo), 0, JTRC_SMOOTH_NOTIMPL);
{$endif}
end;

{== imjcprepct implementation ==}

{ At present, jcsample.c can request context rows only for smoothing.
  In the future, we might also need context rows for CCIR601 sampling
  or other more-complex downsampling procedures.  The code to support
  context rows should be compiled only if needed. }

{$ifdef INPUT_SMOOTHING_SUPPORTED}
  {$define CONTEXT_ROWS_SUPPORTED}
{$endif}


{ For the simple (no-context-row) case, we just need to buffer one
  row group's worth of pixels for the downsampling step.  At the bottom of
  the image, we pad to a full row group by replicating the last pixel row.
  The downsampler's last output row is then replicated if needed to pad
  out to a full iMCU row.

  When providing context rows, we must buffer three row groups' worth of
  pixels.  Three row groups are physically allocated, but the row pointer
  arrays are made five row groups high, with the extra pointers above and
  below "wrapping around" to point to the last and first real row groups.
  This allows the downsampler to access the proper context rows.
  At the top and bottom of the image, we create dummy context rows by
  copying the first or last real pixel row.  This copying could be avoided
  by pointer hacking as is done in jdmainct.c, but it doesn't seem worth the
  trouble on the compression side. }


{ Private buffer controller object }

type
  my_prep_ptr = ^my_prep_controller;
  my_prep_controller = record
    pub : jpeg_c_prep_controller; { public fields }

    { Downsampling input buffer.  This buffer holds color-converted data
      until we have enough to do a downsample step.  }

    color_buf : array[0..MAX_COMPONENTS-1] of JSAMPARRAY;

    rows_to_go : JDIMENSION;	{ counts rows remaining in source image }
    next_buf_row : int;		{ index of next row to store in color_buf }

  {$ifdef CONTEXT_ROWS_SUPPORTED}	{ only needed for context case }
    this_row_group : int;         { starting row index of group to process }
    next_buf_stop : int;	        { downsample when we reach this index }
  {$endif}
  end; {my_prep_controller;}


{ Initialize for a processing pass. }

{METHODDEF}
procedure start_pass_prep (cinfo : j_compress_ptr;
                           pass_mode : J_BUF_MODE );
var
  prep : my_prep_ptr;
begin
  prep := my_prep_ptr (cinfo^.prep);

  if (pass_mode <> JBUF_PASS_THRU) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);

  { Initialize total-height counter for detecting bottom of image }
  prep^.rows_to_go := cinfo^.image_height;
  { Mark the conversion buffer empty }
  prep^.next_buf_row := 0;
{$ifdef CONTEXT_ROWS_SUPPORTED}
  { Preset additional state variables for context mode.
    These aren't used in non-context mode, so we needn't test which mode. }
  prep^.this_row_group := 0;
  { Set next_buf_stop to stop after two row groups have been read in. }
  prep^.next_buf_stop := 2 * cinfo^.max_v_samp_factor;
{$endif}
end;


{ Expand an image vertically from height input_rows to height output_rows,
  by duplicating the bottom row. }

{LOCAL}
procedure expand_bottom_edge (image_data : JSAMPARRAY;
                              num_cols : JDIMENSION;
		              input_rows : int;
                              output_rows : int);
var
  {register} row : int;
begin
  for row := input_rows to pred(output_rows) do
  begin
    jcopy_sample_rows(image_data, input_rows-1, image_data, row,
		      1, num_cols);
  end;
end;


{ Process some data in the simple no-context case.

  Preprocessor output data is counted in "row groups".  A row group
  is defined to be v_samp_factor sample rows of each component.
  Downsampling will produce this much data from each max_v_samp_factor
  input rows. }

{METHODDEF}
procedure pre_process_data (cinfo : j_compress_ptr;
		           input_buf : JSAMPARRAY;
                           var in_row_ctr : JDIMENSION;
		           in_rows_avail : JDIMENSION;
		           output_buf : JSAMPIMAGE;
                           var out_row_group_ctr : JDIMENSION;
                           out_row_groups_avail : JDIMENSION);
var
  prep : my_prep_ptr;
  numrows, ci : int;
  inrows : JDIMENSION;
  compptr : jpeg_component_info_ptr;
var
  local_input_buf : JSAMPARRAY;
begin
  prep := my_prep_ptr (cinfo^.prep);

  while (in_row_ctr < in_rows_avail) and
	(out_row_group_ctr < out_row_groups_avail) do
  begin
    { Do color conversion to fill the conversion buffer. }
    inrows := in_rows_avail - in_row_ctr;
    numrows := cinfo^.max_v_samp_factor - prep^.next_buf_row;
    {numrows := int( MIN(JDIMENSION(numrows), inrows) );}
    if inrows < JDIMENSION(numrows) then
      numrows := int(inrows);
    local_input_buf := JSAMPARRAY(@(input_buf^[in_row_ctr]));
    cinfo^.cconvert^.color_convert (cinfo, local_input_buf,
                                    JSAMPIMAGE(@prep^.color_buf),
				    JDIMENSION(prep^.next_buf_row),
				    numrows);
    Inc(in_row_ctr, numrows);
    Inc(prep^.next_buf_row, numrows);
    Dec(prep^.rows_to_go, numrows);
    { If at bottom of image, pad to fill the conversion buffer. }
    if (prep^.rows_to_go = 0) and
       (prep^.next_buf_row < cinfo^.max_v_samp_factor) then
    begin
      for ci := 0 to pred(cinfo^.num_components) do
      begin
	expand_bottom_edge(prep^.color_buf[ci], cinfo^.image_width,
			   prep^.next_buf_row, cinfo^.max_v_samp_factor);
      end;
      prep^.next_buf_row := cinfo^.max_v_samp_factor;
    end;
    { If we've filled the conversion buffer, empty it. }
    if (prep^.next_buf_row = cinfo^.max_v_samp_factor) then
    begin
      cinfo^.downsample^.downsample (cinfo,
                                     JSAMPIMAGE(@prep^.color_buf),
                                     JDIMENSION (0),
				     output_buf,
                                     out_row_group_ctr);
      prep^.next_buf_row := 0;
      Inc(out_row_group_ctr);;
    end;
    { If at bottom of image, pad the output to a full iMCU height.
      Note we assume the caller is providing a one-iMCU-height output buffer! }
    if (prep^.rows_to_go = 0) and
       (out_row_group_ctr < out_row_groups_avail) then
    begin
      compptr := jpeg_component_info_ptr(cinfo^.comp_info);
      for ci := 0 to pred(cinfo^.num_components) do
      begin
	expand_bottom_edge(output_buf^[ci],
		   compptr^.width_in_blocks * DCTSIZE,
                   int (out_row_group_ctr) * compptr^.v_samp_factor,
		   int (out_row_groups_avail) * compptr^.v_samp_factor);
        Inc(compptr);
      end;
      out_row_group_ctr := out_row_groups_avail;
      break;			{ can exit outer loop without test }
    end;
  end;
end;


{$ifdef CONTEXT_ROWS_SUPPORTED}

{ Process some data in the context case. }

{METHODDEF}
procedure pre_process_context (cinfo : j_compress_ptr;
		              input_buf : JSAMPARRAY;
                              var in_row_ctr : JDIMENSION;
		              in_rows_avail : JDIMENSION;
		              output_buf : JSAMPIMAGE;
                              var out_row_group_ctr : JDIMENSION;
		              out_row_groups_avail : JDIMENSION);
var
  prep : my_prep_ptr;
  numrows, ci : int;
  buf_height : int;
  inrows : JDIMENSION;
var
  row : int;

begin
  prep := my_prep_ptr (cinfo^.prep);
  buf_height := cinfo^.max_v_samp_factor * 3;

  while (out_row_group_ctr < out_row_groups_avail) do
  begin
    if (in_row_ctr < in_rows_avail) then
    begin
      { Do color conversion to fill the conversion buffer. }
      inrows := in_rows_avail - in_row_ctr;
      numrows := prep^.next_buf_stop - prep^.next_buf_row;
      {numrows := int ( MIN( JDIMENSION(numrows), inrows) );}
      if inrows < JDIMENSION(numrows) then
        numrows := int(inrows);
      cinfo^.cconvert^.color_convert (cinfo,
                                      JSAMPARRAY(@input_buf^[in_row_ctr]),
                                      JSAMPIMAGE(@prep^.color_buf),
				      JDIMENSION (prep^.next_buf_row),
				      numrows);
      { Pad at top of image, if first time through }
      if (prep^.rows_to_go = cinfo^.image_height) then
      begin
	for ci := 0 to pred(cinfo^.num_components) do
        begin
	  for row := 1 to cinfo^.max_v_samp_factor do
          begin
	    jcopy_sample_rows(prep^.color_buf[ci], 0,
			      prep^.color_buf[ci], -row,
			      1, cinfo^.image_width);
	  end;
	end;
      end;
      Inc(in_row_ctr, numrows);
      Inc(prep^.next_buf_row, numrows);
      Dec(prep^.rows_to_go, numrows);
    end
    else
    begin
      { Return for more data, unless we are at the bottom of the image. }
      if (prep^.rows_to_go <> 0) then
	break;
      { When at bottom of image, pad to fill the conversion buffer. }
      if (prep^.next_buf_row < prep^.next_buf_stop) then
      begin
	for ci := 0 to pred(cinfo^.num_components) do
        begin
	  expand_bottom_edge(prep^.color_buf[ci], cinfo^.image_width,
			     prep^.next_buf_row, prep^.next_buf_stop);
	end;
	prep^.next_buf_row := prep^.next_buf_stop;
      end;
    end;
    { If we've gotten enough data, downsample a row group. }
    if (prep^.next_buf_row = prep^.next_buf_stop) then
    begin
      cinfo^.downsample^.downsample (cinfo,
                                     JSAMPIMAGE(@prep^.color_buf),
				     JDIMENSION(prep^.this_row_group),
				     output_buf,
                                     out_row_group_ctr);
      Inc(out_row_group_ctr);
      { Advance pointers with wraparound as necessary. }
      Inc(prep^.this_row_group, cinfo^.max_v_samp_factor);
      if (prep^.this_row_group >= buf_height) then
	prep^.this_row_group := 0;
      if (prep^.next_buf_row >= buf_height) then
	prep^.next_buf_row := 0;
      prep^.next_buf_stop := prep^.next_buf_row + cinfo^.max_v_samp_factor;
    end;
  end;
end;


{ Create the wrapped-around downsampling input buffer needed for context mode. }

{LOCAL}
procedure create_context_buffer (cinfo : j_compress_ptr);
var
  prep : my_prep_ptr;
  rgroup_height : int;
  ci, i : int;
  compptr : jpeg_component_info_ptr;
  true_buffer, fake_buffer : JSAMPARRAY;
begin
  prep := my_prep_ptr (cinfo^.prep);
  rgroup_height := cinfo^.max_v_samp_factor;
  { Grab enough space for fake row pointers for all the components;
    we need five row groups' worth of pointers for each component. }

  fake_buffer := JSAMPARRAY(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				(cinfo^.num_components * 5 * rgroup_height) *
				SIZEOF(JSAMPROW)) );

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Allocate the actual buffer space (3 row groups) for this component.
      We make the buffer wide enough to allow the downsampler to edge-expand
      horizontally within the buffer, if it so chooses. }
    true_buffer := cinfo^.mem^.alloc_sarray
      (j_common_ptr(cinfo), JPOOL_IMAGE,
       JDIMENSION (( long(compptr^.width_in_blocks) * DCTSIZE *
		      cinfo^.max_h_samp_factor) div compptr^.h_samp_factor),
       JDIMENSION (3 * rgroup_height));
    { Copy true buffer row pointers into the middle of the fake row array }
    MEMCOPY(JSAMPARRAY(@ fake_buffer^[rgroup_height]), true_buffer,
	    3 * rgroup_height * SIZEOF(JSAMPROW));
    { Fill in the above and below wraparound pointers }
    for i := 0 to pred(rgroup_height) do
    begin
      fake_buffer^[i] := true_buffer^[2 * rgroup_height + i];
      fake_buffer^[4 * rgroup_height + i] := true_buffer^[i];
    end;
    prep^.color_buf[ci] := JSAMPARRAY(@ fake_buffer^[rgroup_height]);
    Inc(JSAMPROW_PTR(fake_buffer), 5 * rgroup_height); { point to space for next component }
    Inc(compptr);
  end;
end;

{$endif} { CONTEXT_ROWS_SUPPORTED }


{ Initialize preprocessing controller. }

{GLOBAL}
procedure jinit_c_prep_controller (cinfo : j_compress_ptr;
                                   need_full_buffer : boolean);
var
  prep : my_prep_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
begin

  if (need_full_buffer)	then    { safety check }
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);

  prep := my_prep_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(my_prep_controller)) );
  cinfo^.prep := jpeg_c_prep_controller_ptr(prep);
  prep^.pub.start_pass := start_pass_prep;

  { Allocate the color conversion buffer.
    We make the buffer wide enough to allow the downsampler to edge-expand
    horizontally within the buffer, if it so chooses. }

  if (cinfo^.downsample^.need_context_rows) then
  begin
    { Set up to provide context rows }
{$ifdef CONTEXT_ROWS_SUPPORTED}
    prep^.pub.pre_process_data := pre_process_context;
    create_context_buffer(cinfo);
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
  end
  else
  begin
    { No context, just make it tall enough for one row group }
    prep^.pub.pre_process_data := pre_process_data;
    compptr := jpeg_component_info_ptr(cinfo^.comp_info);
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      prep^.color_buf[ci] := cinfo^.mem^.alloc_sarray
	(j_common_ptr(cinfo), JPOOL_IMAGE,
	 JDIMENSION (( long(compptr^.width_in_blocks) * DCTSIZE *
			cinfo^.max_h_samp_factor) div compptr^.h_samp_factor),
	 JDIMENSION(cinfo^.max_v_samp_factor) );
      Inc(compptr);
    end;
  end;
end;

{== imjcdctmgr implementation ==}

{ Private subobject for this module }

type
  my_fdct_ptr = ^my_fdct_controller;
  my_fdct_controller = record
    pub : jpeg_forward_dct;	{ public fields }

    { Pointer to the DCT routine actually in use }
    do_dct : forward_DCT_method_ptr;

    { The actual post-DCT divisors --- not identical to the quant table
      entries, because of scaling (especially for an unnormalized DCT).
      Each table is given in normal array order. }

    divisors : array[0..NUM_QUANT_TBLS-1] of DCTELEM_FIELD_PTR;

  {$ifdef DCT_FLOAT_SUPPORTED}
    { Same as above for the floating-point case. }
    do_float_dct : float_DCT_method_ptr;
    float_divisors : array[0..NUM_QUANT_TBLS-1] of FAST_FLOAT_FIELD_PTR;
  {$endif}
  end;


{ Initialize for a processing pass.
  Verify that all referenced Q-tables are present, and set up
  the divisor table for each one.
  In the current implementation, DCT of all components is done during
  the first pass, even if only some components will be output in the
  first scan.  Hence all components should be examined here. }

{METHODDEF}
procedure start_pass_fdctmgr (cinfo : j_compress_ptr);
var
  fdct : my_fdct_ptr;
  ci, qtblno, i : int;
  compptr : jpeg_component_info_ptr;
  qtbl : JQUANT_TBL_PTR;
  dtbl : DCTELEM_FIELD_PTR;
{$ifdef DCT_IFAST_SUPPORTED}
const
  jcdctmgr_CONST_BITS = 14;
  aanscales : array[0..DCTSIZE2-1] of INT16 =
         ({ precomputed values scaled up by 14 bits }
	  16384, 22725, 21407, 19266, 16384, 12873,  8867,  4520,
	  22725, 31521, 29692, 26722, 22725, 17855, 12299,  6270,
	  21407, 29692, 27969, 25172, 21407, 16819, 11585,  5906,
	  19266, 26722, 25172, 22654, 19266, 15137, 10426,  5315,
	  16384, 22725, 21407, 19266, 16384, 12873,  8867,  4520,
	  12873, 17855, 16819, 15137, 12873, 10114,  6967,  3552,
	   8867, 12299, 11585, 10426,  8867,  6967,  4799,  2446,
	   4520,  6270,  5906,  5315,  4520,  3552,  2446,  1247);
  {SHIFT_TEMPS}

  { Descale and correctly round an INT32 value that's scaled by N bits.
    We assume RIGHT_SHIFT rounds towards minus infinity, so adding
    the fudge factor is correct for either sign of X. }

  function jcdctmgr_DESCALE(x : INT32; n : int) : INT32;
  var
    shift_temp : INT32;
  begin
    shift_temp := x + (INT32(1) shl (n-1));
  {$ifdef RIGHT_SHIFT_IS_UNSIGNED}
    if shift_temp < 0 then
      jcdctmgr_Descale :=  (shift_temp shr n) or ((not INT32(0)) shl (32-n))
    else
  {$endif}
      jcdctmgr_Descale :=  (shift_temp shr n);
  end;

{$endif}
{$ifdef DCT_FLOAT_SUPPORTED}
var
  fdtbl : FAST_FLOAT_FIELD_PTR;
  row, col : int;
const
  aanscalefactor : array[0..DCTSIZE-1] of double =
    (1.0, 1.387039845, 1.306562965, 1.175875602,
     1.0, 0.785694958, 0.541196100, 0.275899379);
{$endif}
begin
  fdct := my_fdct_ptr (cinfo^.fdct);
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    qtblno := compptr^.quant_tbl_no;
    { Make sure specified quantization table is present }
    if (qtblno < 0) or (qtblno >= NUM_QUANT_TBLS) or
       (cinfo^.quant_tbl_ptrs[qtblno] = NIL) then
      ERREXIT1(j_common_ptr(cinfo), JERR_NO_QUANT_TABLE, qtblno);
    qtbl := cinfo^.quant_tbl_ptrs[qtblno];
    { Compute divisors for this quant table }
    { We may do this more than once for same table, but it's not a big deal }
    case (cinfo^.dct_method) of
{$ifdef DCT_ISLOW_SUPPORTED}
    JDCT_ISLOW:
    begin
      { For LL&M IDCT method, divisors are equal to raw quantization
        coefficients multiplied by 8 (to counteract scaling). }

      if (fdct^.divisors[qtblno] = NIL) then
      begin
	fdct^.divisors[qtblno] := DCTELEM_FIELD_PTR(
	  cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				      DCTSIZE2 * SIZEOF(DCTELEM)) );
      end;
      dtbl := fdct^.divisors[qtblno];
      for i := 0 to pred(DCTSIZE2) do
      begin
	dtbl^[i] := (DCTELEM(qtbl^.quantval[i])) shl 3;
      end;
    end;
{$endif}
{$ifdef DCT_IFAST_SUPPORTED}
    JDCT_IFAST:
      begin
        { For AA&N IDCT method, divisors are equal to quantization
          coefficients scaled by scalefactor[row]*scalefactor[col], where
            scalefactor[0] := 1
            scalefactor[k] := cos(k*PI/16) * sqrt(2)    for k=1..7
          We apply a further scale factor of 8. }


	if (fdct^.divisors[qtblno] = NIL) then
        begin
	  fdct^.divisors[qtblno] := DCTELEM_FIELD_PTR(
	    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
					DCTSIZE2 * SIZEOF(DCTELEM)) );
	end;
	dtbl := fdct^.divisors[qtblno];
	for i := 0 to pred(DCTSIZE2) do
        begin
	  dtbl^[i] := DCTELEM(
                     {MULTIPLY16V16}
	    jcdctmgr_DESCALE( INT32(qtbl^.quantval[i]) * INT32 (aanscales[i]),
		     jcdctmgr_CONST_BITS-3) );
	end;
      end;
{$endif}
{$ifdef DCT_FLOAT_SUPPORTED}

    JDCT_FLOAT:
      begin
	{ For float AA&N IDCT method, divisors are equal to quantization
	  coefficients scaled by scalefactor[row]*scalefactor[col], where
	    scalefactor[0] := 1
	    scalefactor[k] := cos(k*PI/16) * sqrt(2)    for k=1..7
	  We apply a further scale factor of 8.
	  What's actually stored is 1/divisor so that the inner loop can
	  use a multiplication rather than a division. }

	if (fdct^.float_divisors[qtblno] = NIL) then
        begin
	  fdct^.float_divisors[qtblno] := FAST_FLOAT_FIELD_PTR(
	    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
					DCTSIZE2 * SIZEOF(FAST_FLOAT)) );
	end;
	fdtbl := fdct^.float_divisors[qtblno];
	i := 0;
	for row := 0 to pred(DCTSIZE) do
        begin
	  for col := 0 to pred(DCTSIZE) do
          begin
	    fdtbl^[i] := {FAST_FLOAT}
	      (1.0 / (( {double}(qtbl^.quantval[i]) *
		       aanscalefactor[row] * aanscalefactor[col] * 8.0)));
	    Inc(i);
	  end;
	end;
      end;
{$endif}
    else
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
    end;
    Inc(compptr);
  end;
end;


{ Perform forward DCT on one or more blocks of a component.

  The input samples are taken from the sample_data[] array starting at
  position start_row/start_col, and moving to the right for any additional
  blocks. The quantized coefficients are returned in coef_blocks[]. }

{METHODDEF}
procedure forward_DCT (cinfo : j_compress_ptr;
                       compptr : jpeg_component_info_ptr;
	               sample_data : JSAMPARRAY;
                       coef_blocks : JBLOCKROW;
	               start_row : JDIMENSION;
                       start_col : JDIMENSION;
	               num_blocks : JDIMENSION);
{ This version is used for integer DCT implementations. }
var
  { This routine is heavily used, so it's worth coding it tightly. }
  fdct : my_fdct_ptr;
  do_dct : forward_DCT_method_ptr;
  divisors : DCTELEM_FIELD_PTR;
  workspace : array[0..DCTSIZE2-1] of DCTELEM;	{ work area for FDCT subroutine }
  bi : JDIMENSION;
var
  {register} workspaceptr : DCTELEMPTR;
  {register} elemptr : JSAMPLE_PTR;
  {register} elemr : int;
{$ifndef DCTSIZE_IS_8}
var
  {register} elemc : int;
{$endif}
var
  {register} temp, qval : DCTELEM;
  {register} i : int;
  {register} output_ptr : JCOEFPTR;
begin
  fdct := my_fdct_ptr (cinfo^.fdct);
  do_dct := fdct^.do_dct;
  divisors := fdct^.divisors[compptr^.quant_tbl_no];

  Inc(JSAMPROW_PTR(sample_data), start_row);	{ fold in the vertical offset once }

  for bi := 0 to pred(num_blocks) do
  begin

    { Load data into workspace, applying unsigned->signed conversion }

    workspaceptr := @workspace[0];
    for elemr := 0 to pred(DCTSIZE) do
    begin
      elemptr := @sample_data^[elemr]^[start_col];
{$ifdef DCTSIZE_IS_8}		{ unroll the inner loop }
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      {Inc(elemptr);       - Value never used }
{$else}
      for elemc := pred(DCTSIZE) downto 0 do
      begin
        workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
        Inc(workspaceptr);
        Inc(elemptr);
      end;
{$endif}
    end;

    { Perform the DCT }
    do_dct (workspace);

    { Quantize/descale the coefficients, and store into coef_blocks[] }

    output_ptr := JCOEFPTR(@coef_blocks^[bi]);
    for i := 0 to pred(DCTSIZE2) do
    begin
      qval := divisors^[i];
      temp := workspace[i];
      { Divide the coefficient value by qval, ensuring proper rounding.
	Since C does not specify the direction of rounding for negative
	quotients, we have to force the dividend positive for portability.

	In most files, at least half of the output values will be zero
	(at default quantization settings, more like three-quarters...)
	so we should ensure that this case is fast.  On many machines,
	a comparison is enough cheaper than a divide to make a special test
	a win.  Since both inputs will be nonnegative, we need only test
	for a < b to discover whether a/b is 0.
	If your machine's division is fast enough, define FAST_DIVIDE. }

      if (temp < 0) then
      begin
	temp := -temp;
	Inc(temp, qval shr 1);	{ for rounding }
        {DIVIDE_BY(temp, qval);}
        {$ifdef FAST_DIVIDE}
          temp := temp div qval;
        {$else}
          if (temp >= qval) then
            temp := temp div qval
          else
            temp := 0;
        {$endif}
	temp := -temp;
      end
      else
      begin
	Inc(temp, qval shr 1);	{ for rounding }
        {DIVIDE_BY(temp, qval);}
        {$ifdef FAST_DIVIDE}
          temp := temp div qval;
        {$else}
          if (temp >= qval) then
            temp := temp div qval
          else
            temp := 0;
        {$endif}
      end;
      output_ptr^[i] := JCOEF (temp);
    end;
    Inc(start_col, DCTSIZE);
  end;
end;


{$ifdef DCT_FLOAT_SUPPORTED}

{METHODDEF}
procedure forward_DCT_float (cinfo : j_compress_ptr;
                             compptr : jpeg_component_info_ptr;
		             sample_data : JSAMPARRAY;
                             coef_blocks : JBLOCKROW;
		             start_row : JDIMENSION;
                             start_col : JDIMENSION;
		             num_blocks : JDIMENSION);
{ This version is used for floating-point DCT implementations. }
var
  { This routine is heavily used, so it's worth coding it tightly. }
  fdct : my_fdct_ptr;
  do_dct : float_DCT_method_ptr;
  divisors : FAST_FLOAT_FIELD_PTR;
  workspace : array[0..DCTSIZE2-1] of FAST_FLOAT; { work area for FDCT subroutine }
  bi : JDIMENSION;
var
  {register} workspaceptr : FAST_FLOAT_PTR;
  {register} elemptr : JSAMPLE_PTR;
  {register} elemr : int;
{$ifndef DCTSIZE_IS_8}
var
  {register} elemc : int;
{$endif}
var
  {register} temp : FAST_FLOAT;
  {register} i : int;
  {register} output_ptr : JCOEFPTR;
begin
  fdct := my_fdct_ptr (cinfo^.fdct);
  do_dct := fdct^.do_float_dct;
  divisors := fdct^.float_divisors[compptr^.quant_tbl_no];

  Inc(JSAMPROW_PTR(sample_data), start_row);	{ fold in the vertical offset once }

  for bi := 0 to pred(num_blocks) do
  begin
    { Load data into workspace, applying unsigned->signed conversion }

    workspaceptr := @workspace[0];
    for elemr := 0 to pred(DCTSIZE) do
    begin
      elemptr := @(sample_data^[elemr]^[start_col]);
{$ifdef DCTSIZE_IS_8}		{ unroll the inner loop }
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      {Inc(elemptr);         - value never used }
{$else}
      for elemc := pred(DCTSIZE) downto 0 do
      begin
	workspaceptr^ := {FAST_FLOAT}(
	  (GETJSAMPLE(elemptr^) - CENTERJSAMPLE) );
        Inc(workspaceptr);
        Inc(elemptr);
      end;
{$endif}
    end;


    { Perform the DCT }
    do_dct (workspace);

    { Quantize/descale the coefficients, and store into coef_blocks[] }

    output_ptr := JCOEFPTR(@(coef_blocks^[bi]));

    for i := 0 to pred(DCTSIZE2) do
    begin
      { Apply the quantization and scaling factor }
      temp := workspace[i] * divisors^[i];
      { Round to nearest integer.
	Since C does not specify the direction of rounding for negative
	quotients, we have to force the dividend positive for portability.
	The maximum coefficient size is +-16K (for 12-bit data), so this
	code should work for either 16-bit or 32-bit ints. }
      output_ptr^[i] := JCOEF ( int(Trunc (temp + {FAST_FLOAT}(16384.5))) - 16384);
    end;
    Inc(start_col, DCTSIZE);
  end;
end;

{$endif} { DCT_FLOAT_SUPPORTED }


{ Initialize FDCT manager. }

{GLOBAL}
procedure jinit_forward_dct (cinfo : j_compress_ptr);
var
  fdct : my_fdct_ptr;
  i : int;
begin
  fdct := my_fdct_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(my_fdct_controller)) );
  cinfo^.fdct := jpeg_forward_dct_ptr (fdct);
  fdct^.pub.start_pass := start_pass_fdctmgr;

  case (cinfo^.dct_method) of
{$ifdef DCT_ISLOW_SUPPORTED}
  JDCT_ISLOW:
    begin
      fdct^.pub.forward_DCT := forward_DCT;
      fdct^.do_dct := jpeg_fdct_islow;
    end;
{$endif}
{$ifdef DCT_IFAST_SUPPORTED}
  JDCT_IFAST:
    begin
      fdct^.pub.forward_DCT := forward_DCT;
      fdct^.do_dct := jpeg_fdct_ifast;
    end;
{$endif}
{$ifdef DCT_FLOAT_SUPPORTED}
  JDCT_FLOAT:
    begin
      fdct^.pub.forward_DCT := forward_DCT_float;
      fdct^.do_float_dct := jpeg_fdct_float;
    end;
{$endif}
  else
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
  end;

  { Mark divisor tables unallocated }
  for i := 0 to pred(NUM_QUANT_TBLS) do
  begin
    fdct^.divisors[i] := NIL;
{$ifdef DCT_FLOAT_SUPPORTED}
    fdct^.float_divisors[i] := NIL;
{$endif}
  end;
end;

{== imjchuff implementation ==}

{ Expanded entropy encoder object for Huffman encoding.

  The savable_state subrecord contains fields that change within an MCU,
  but must not be updated permanently until we complete the MCU. }

type
  jchuff_savable_state = record
    put_buffer : INT32;		{ current bit-accumulation buffer }
    put_bits : int;		{ # of bits now in it }
    last_dc_val : array[0..MAX_COMPS_IN_SCAN-1] of int;
                                { last DC coef for each component }
  end;


type
  jchuff_huff_entropy_ptr = ^huff_entropy_encoder;
  huff_entropy_encoder = record
    pub : jpeg_entropy_encoder; { public fields }

    saved : jchuff_savable_state;	{ Bit buffer & DC state at start of MCU }

    { These fields are NOT loaded into local working state. }
    restarts_to_go : uInt;	{ MCUs left in this restart interval }
    next_restart_num : int;	{ next restart number to write (0-7) }

    { Pointers to derived tables (these workspaces have image lifespan) }
    dc_derived_tbls : array[0..NUM_HUFF_TBLS-1] of c_derived_tbl_ptr;
    ac_derived_tbls : array[0..NUM_HUFF_TBLS-1] of c_derived_tbl_ptr;

  {$ifdef ENTROPY_OPT_SUPPORTED} { Statistics tables for optimization }
    dc_count_ptrs : array[0..NUM_HUFF_TBLS-1] of TLongTablePtr;
    ac_count_ptrs : array[0..NUM_HUFF_TBLS-1] of TLongTablePtr;
  {$endif}
  end;


{ Working state while writing an MCU.
  This struct contains all the fields that are needed by subroutines. }

type
  working_state = record
    next_output_byte : JOCTETptr; { => next byte to write in buffer }
    free_in_buffer : size_t;	  { # of byte spaces remaining in buffer }
    cur : jchuff_savable_state;	  { Current bit buffer & DC state }
    cinfo : j_compress_ptr;	  { dump_buffer needs access to this }
  end;


{ Forward declarations }
{METHODDEF}
function encode_mcu_huff (cinfo : j_compress_ptr;
                          const MCU_data : array of JBLOCKROW) : boolean;
                          forward;
{METHODDEF}
procedure finish_pass_huff (cinfo : j_compress_ptr); forward;
{$ifdef ENTROPY_OPT_SUPPORTED}
{METHODDEF}
function encode_mcu_gather (cinfo : j_compress_ptr;
                            const MCU_data: array of JBLOCKROW) : boolean;
                            forward;

{METHODDEF}
procedure finish_pass_gather (cinfo : j_compress_ptr); forward;
{$endif}


{ Initialize for a Huffman-compressed scan.
  If gather_statistics is TRUE, we do not output anything during the scan,
  just count the Huffman symbols used and generate Huffman code tables. }

{METHODDEF}
procedure start_pass_huff (cinfo : j_compress_ptr;
                           gather_statistics : boolean);
var
  entropy : jchuff_huff_entropy_ptr;
  ci, dctbl, actbl : int;
  compptr : jpeg_component_info_ptr;
begin
  entropy := jchuff_huff_entropy_ptr (cinfo^.entropy);

  if (gather_statistics) then
  begin
{$ifdef ENTROPY_OPT_SUPPORTED}
    entropy^.pub.encode_mcu := encode_mcu_gather;
    entropy^.pub.finish_pass := finish_pass_gather;
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
  end
  else
  begin
    entropy^.pub.encode_mcu := encode_mcu_huff;
    entropy^.pub.finish_pass := finish_pass_huff;
  end;

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    dctbl := compptr^.dc_tbl_no;
    actbl := compptr^.ac_tbl_no;
    if (gather_statistics) then
    begin
{$ifdef ENTROPY_OPT_SUPPORTED}
      { Check for invalid table indexes }
      { (make_c_derived_tbl does this in the other path) }
      if (dctbl < 0) or (dctbl >= NUM_HUFF_TBLS) then
	ERREXIT1(j_common_ptr(cinfo), JERR_NO_HUFF_TABLE, dctbl);
      if (actbl < 0) or (actbl >= NUM_HUFF_TBLS) then
	ERREXIT1(j_common_ptr(cinfo), JERR_NO_HUFF_TABLE, actbl);
      { Allocate and zero the statistics tables }
      { Note that jpeg_gen_optimal_table expects 257 entries in each table! }
      if (entropy^.dc_count_ptrs[dctbl] = NIL) then
	entropy^.dc_count_ptrs[dctbl] := TLongTablePtr(
	  cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				      257 * SIZEOF(long)) );
      MEMZERO(entropy^.dc_count_ptrs[dctbl], 257 * SIZEOF(long));
      if (entropy^.ac_count_ptrs[actbl] = NIL) then
	entropy^.ac_count_ptrs[actbl] := TLongTablePtr(
	  cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				      257 * SIZEOF(long)) );
      MEMZERO(entropy^.ac_count_ptrs[actbl], 257 * SIZEOF(long));
{$endif}
    end
    else
    begin
      { Compute derived values for Huffman tables }
      { We may do this more than once for a table, but it's not expensive }
      jpeg_make_c_derived_tbl(cinfo, TRUE, dctbl,
			      entropy^.dc_derived_tbls[dctbl]);
      jpeg_make_c_derived_tbl(cinfo, FALSE, actbl,
			      entropy^.ac_derived_tbls[actbl]);
    end;
    { Initialize DC predictions to 0 }
    entropy^.saved.last_dc_val[ci] := 0;
  end;

  { Initialize bit buffer to empty }
  entropy^.saved.put_buffer := 0;
  entropy^.saved.put_bits := 0;

  { Initialize restart stuff }
  entropy^.restarts_to_go := cinfo^.restart_interval;
  entropy^.next_restart_num := 0;
end;


{ Compute the derived values for a Huffman table.
  This routine also performs some validation checks on the table.

  Note this is also used by jcphuff.c. }

{GLOBAL}
procedure jpeg_make_c_derived_tbl (cinfo : j_compress_ptr;
                                   isDC : boolean;
                                   tblno : int;
			           var pdtbl : c_derived_tbl_ptr);
var
  htbl : JHUFF_TBL_PTR;
  dtbl : c_derived_tbl_ptr;
  p, i, l, lastp, si, maxsymbol : int;
  huffsize : array[0..257-1] of byte;
  huffcode : array[0..257-1] of uInt;
  code : uInt;
begin
  { Note that huffsize[] and huffcode[] are filled in code-length order,
    paralleling the order of the symbols themselves in htbl->huffval[]. }

  { Find the input Huffman table }
  if (tblno < 0) or (tblno >= NUM_HUFF_TBLS) then
    ERREXIT1(j_common_ptr(cinfo), JERR_NO_HUFF_TABLE, tblno);
  if isDC then
    htbl := cinfo^.dc_huff_tbl_ptrs[tblno]
  else
    htbl := cinfo^.ac_huff_tbl_ptrs[tblno];
  if (htbl = NIL) then
    ERREXIT1(j_common_ptr(cinfo), JERR_NO_HUFF_TABLE, tblno);

  { Allocate a workspace if we haven't already done so. }
  if (pdtbl = NIL) then
    pdtbl := c_derived_tbl_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				  SIZEOF(c_derived_tbl)) );
  dtbl := pdtbl;

  { Figure C.1: make table of Huffman code length for each symbol }

  p := 0;
  for l := 1 to 16 do
  begin
    i := int(htbl^.bits[l]);
    if (i < 0) and (p + i > 256) then	{ protect against table overrun }
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_HUFF_TABLE);
    while (i > 0) do
    begin
      huffsize[p] := byte(l);
      Inc(p);
      Dec(i);
    end;
  end;
  huffsize[p] := 0;
  lastp := p;

  { Figure C.2: generate the codes themselves }
  { We also validate that the counts represent a legal Huffman code tree. }

  code := 0;
  si := huffsize[0];
  p := 0;
  while (huffsize[p] <> 0) do
  begin
    while (( int(huffsize[p]) ) = si) do
    begin
      huffcode[p] := code;
      Inc(p);
      Inc(code);
    end;
    { code is now 1 more than the last code used for codelength si; but
      it must still fit in si bits, since no code is allowed to be all ones. }

    if (INT32(code) >= (INT32(1) shl si)) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_HUFF_TABLE);
    code := code shl 1;
    Inc(si);
  end;

  { Figure C.3: generate encoding tables }
  { These are code and size indexed by symbol value }

  { Set all codeless symbols to have code length 0;
    this lets us detect duplicate VAL entries here, and later
    allows emit_bits to detect any attempt to emit such symbols. }

  MEMZERO(@dtbl^.ehufsi, SIZEOF(dtbl^.ehufsi));

  { This is also a convenient place to check for out-of-range
    and duplicated VAL entries.  We allow 0..255 for AC symbols
    but only 0..15 for DC.  (We could constrain them further
    based on data depth and mode, but this seems enough.) }

  if isDC then
    maxsymbol := 15
  else
    maxsymbol := 255;

  for p := 0 to pred(lastp) do
  begin
    i := htbl^.huffval[p];
    if (i < 0) or (i > maxsymbol) or (dtbl^.ehufsi[i] <> 0) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_HUFF_TABLE);
    dtbl^.ehufco[i] := huffcode[p];
    dtbl^.ehufsi[i] := huffsize[p];
  end;
end;


{ Outputting bytes to the file }


{LOCAL}
function jchuff_dump_buffer (var state : working_state) : boolean;
{ Empty the output buffer; return TRUE if successful, FALSE if must suspend }
var
  dest : jpeg_destination_mgr_ptr;
begin
  dest := state.cinfo^.dest;

  if (not dest^.empty_output_buffer (state.cinfo)) then
  begin
    jchuff_dump_buffer := FALSE;
    exit;
  end;
  { After a successful buffer dump, must reset buffer pointers }
  state.next_output_byte := dest^.next_output_byte;
  state.free_in_buffer := dest^.free_in_buffer;
  jchuff_dump_buffer := TRUE;
end;


{ Outputting bits to the file }

{ Only the right 24 bits of put_buffer are used; the valid bits are
  left-justified in this part.  At most 16 bits can be passed to emit_bits
  in one call, and we never retain more than 7 bits in put_buffer
  between calls, so 24 bits are sufficient. }


{LOCAL}
function jchuff_emit_bits (var state : working_state;
                    code : uInt;
                    size : int) : boolean;  {INLINE}
{ Emit some bits; return TRUE if successful, FALSE if must suspend }
var
  { This routine is heavily used, so it's worth coding tightly. }
  {register} put_buffer : INT32;
  {register} put_bits : int;
var
  c : int;
begin
  put_buffer := INT32 (code);
  put_bits := state.cur.put_bits;

  { if size is 0, caller used an invalid Huffman table entry }
  if (size = 0) then
    ERREXIT(j_common_ptr(state.cinfo), JERR_HUFF_MISSING_CODE);

  put_buffer := put_buffer and pred(INT32(1) shl size);
                { mask off any extra bits in code }

  Inc(put_bits, size);          { new number of bits in buffer }

  put_buffer := put_buffer shl (24 - put_bits);
                                { align incoming bits }
  put_buffer := put_buffer or state.cur.put_buffer;
                                { and merge with old buffer contents }
  while (put_bits >= 8) do
  begin
    c := int ((put_buffer shr 16) and $FF);

    {emit_byte(state, c, return FALSE);}
    { Emit a byte, return FALSE if must suspend. }
    state.next_output_byte^ := JOCTET (c);
    Inc(state.next_output_byte);
    Dec(state.free_in_buffer);
    if (state.free_in_buffer = 0) then
      if not jchuff_dump_buffer(state) then
      begin
        jchuff_emit_bits := FALSE;
        exit;
      end;

    if (c = $FF) then          { need to stuff a zero byte? }
    begin
      {emit_byte(state, 0, return FALSE);}
      state.next_output_byte^ := JOCTET (0);
      Inc(state.next_output_byte);
      Dec(state.free_in_buffer);
      if (state.free_in_buffer = 0) then
	if not jchuff_dump_buffer(state) then
	begin
          jchuff_emit_bits := FALSE;
          exit;
        end;

    end;
    put_buffer := put_buffer shl 8;
    Dec(put_bits, 8);
  end;

  state.cur.put_buffer := put_buffer; { update state variables }
  state.cur.put_bits := put_bits;

  jchuff_emit_bits := TRUE;
end;


{LOCAL}
function jchuff_flush_bits (var state : working_state) : boolean;
begin
  if (not jchuff_emit_bits(state, $7F, 7)) then { fill any partial byte with ones }
  begin
    jchuff_flush_bits := FALSE;
    exit;
  end;
  state.cur.put_buffer := 0;	{ and reset bit-buffer to empty }
  state.cur.put_bits := 0;
  jchuff_flush_bits := TRUE;
end;


{ Encode a single block's worth of coefficients }

{LOCAL}
function encode_one_block (var state : working_state;
                           const block : JBLOCK;
                           last_dc_val : int;
                           dctbl : c_derived_tbl_ptr;
                           actbl : c_derived_tbl_ptr) : boolean;
var
  {register} temp, temp2 : int;
  {register} nbits : int;
  {register} k, r, i : int;
begin
  { Encode the DC coefficient difference per section F.1.2.1 }

  temp2 := block[0] - last_dc_val;
  temp := temp2;

  if (temp < 0) then
  begin
    temp := -temp;		{ temp is abs value of input }
    { For a negative input, want temp2 := bitwise complement of abs(input) }
    { This code assumes we are on a two's complement machine }
    Dec(temp2);
  end;

  { Find the number of bits needed for the magnitude of the coefficient }
  nbits := 0;
  while (temp <> 0) do
  begin
    Inc(nbits);
    temp := temp shr 1;
  end;

  { Check for out-of-range coefficient values.
    Since we're encoding a difference, the range limit is twice as much. }

  if (nbits > MAX_COEF_BITS+1) then
    ERREXIT(j_common_ptr(state.cinfo), JERR_BAD_DCT_COEF);

  { Emit the Huffman-coded symbol for the number of bits }
  if not jchuff_emit_bits(state, dctbl^.ehufco[nbits], dctbl^.ehufsi[nbits]) then
  begin
    encode_one_block := FALSE;
    exit;
  end;

  { Emit that number of bits of the value, if positive, }
  { or the complement of its magnitude, if negative. }
  if (nbits <> 0) then              { emit_bits rejects calls with size 0 }
    if not jchuff_emit_bits(state, uInt(temp2), nbits) then
    begin
      encode_one_block := FALSE;
      exit;
    end;

  { Encode the AC coefficients per section F.1.2.2 }

  r := 0;			{ r := run length of zeros }

  for k := 1 to pred(DCTSIZE2) do
  begin
    temp := block[jpeg_natural_order[k]];
    if (temp = 0) then
    begin
      Inc(r);
    end
    else
    begin
      { if run length > 15, must emit special run-length-16 codes ($F0) }
      while (r > 15) do
      begin
	if not jchuff_emit_bits(state, actbl^.ehufco[$F0], actbl^.ehufsi[$F0]) then
        begin
          encode_one_block := FALSE;
          exit;
        end;
	Dec(r, 16);
      end;

      temp2 := temp;
      if (temp < 0) then
      begin
	temp := -temp;		{ temp is abs value of input }
	{ This code assumes we are on a two's complement machine }
	Dec(temp2);
      end;

      { Find the number of bits needed for the magnitude of the coefficient }
      nbits := 0;		{ there must be at least one 1 bit }
      repeat
	Inc(nbits);
        temp := temp shr 1;
      until (temp = 0);

      { Check for out-of-range coefficient values }
      if (nbits > MAX_COEF_BITS) then
	ERREXIT(j_common_ptr(state.cinfo), JERR_BAD_DCT_COEF);

      { Emit Huffman symbol for run length / number of bits }
      i := (r shl 4) + nbits;
      if not jchuff_emit_bits(state, actbl^.ehufco[i], actbl^.ehufsi[i]) then
      begin
        encode_one_block := FALSE;
        exit;
      end;

      { Emit that number of bits of the value, if positive, }
      { or the complement of its magnitude, if negative. }
      if not jchuff_emit_bits(state, uInt(temp2), nbits) then
      begin
        encode_one_block := FALSE;
        exit;
      end;

      r := 0;
    end;
  end;

  { If the last coef(s) were zero, emit an end-of-block code }
  if (r > 0) then
    if not jchuff_emit_bits(state, actbl^.ehufco[0], actbl^.ehufsi[0]) then
    begin
      encode_one_block := FALSE;
      exit;
    end;

  encode_one_block := TRUE;
end;


{ Emit a restart marker & resynchronize predictions. }

{LOCAL}
function jchuff_emit_restart (var state : working_state;
                       restart_num : int) : boolean;
var
  ci : int;
begin
  if (not jchuff_flush_bits(state)) then
  begin
    jchuff_emit_restart  := FALSE;
    exit;
  end;

  {emit_byte(state, $FF, return FALSE);}
  { Emit a byte, return FALSE if must suspend. }
  state.next_output_byte^ := JOCTET ($FF);
  Inc(state.next_output_byte);
  Dec(state.free_in_buffer);
  if (state.free_in_buffer = 0) then
    if not jchuff_dump_buffer(state) then
    begin
      jchuff_emit_restart := FALSE;
      exit;
    end;

  {emit_byte(state, JPEG_RST0 + restart_num, return FALSE);}
  { Emit a byte, return FALSE if must suspend. }
  state.next_output_byte^ := JOCTET (JPEG_RST0 + restart_num);
  Inc(state.next_output_byte);
  Dec(state.free_in_buffer);
  if (state.free_in_buffer = 0) then
    if not jchuff_dump_buffer(state) then
    begin
      jchuff_emit_restart := FALSE;
      exit;
    end;

  { Re-initialize DC predictions to 0 }
  for ci := 0 to pred(state.cinfo^.comps_in_scan) do
    state.cur.last_dc_val[ci] := 0;

  { The restart counter is not updated until we successfully write the MCU. }

  jchuff_emit_restart := TRUE;
end;


{ Encode and output one MCU's worth of Huffman-compressed coefficients. }

{METHODDEF}
function encode_mcu_huff (cinfo : j_compress_ptr;
                          const MCU_data: array of JBLOCKROW) : boolean;
var
  entropy : jchuff_huff_entropy_ptr;
  state : working_state;
  blkn, ci : int;
  compptr : jpeg_component_info_ptr;
begin
  entropy := jchuff_huff_entropy_ptr (cinfo^.entropy);
  { Load up working state }
  state.next_output_byte := cinfo^.dest^.next_output_byte;
  state.free_in_buffer := cinfo^.dest^.free_in_buffer;
  {ASSIGN_STATE(state.cur, entropy^.saved);}
  state.cur := entropy^.saved;
  state.cinfo := cinfo;

  { Emit restart marker if needed }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
      if not jchuff_emit_restart(state, entropy^.next_restart_num) then
      begin
	encode_mcu_huff := FALSE;
        exit;
      end;
  end;

  { Encode the MCU data blocks }
  for blkn := 0  to pred(cinfo^.blocks_in_MCU) do
  begin
    ci := cinfo^.MCU_membership[blkn];
    compptr := cinfo^.cur_comp_info[ci];
    if not encode_one_block(state,
                            MCU_data[blkn]^[0],
                            state.cur.last_dc_val[ci],
                            entropy^.dc_derived_tbls[compptr^.dc_tbl_no],
                            entropy^.ac_derived_tbls[compptr^.ac_tbl_no]) then
    begin
      encode_mcu_huff := FALSE;
      exit;
    end;
    { Update last_dc_val }
    state.cur.last_dc_val[ci] := MCU_data[blkn]^[0][0];
  end;

  { Completed MCU, so update state }
  cinfo^.dest^.next_output_byte := state.next_output_byte;
  cinfo^.dest^.free_in_buffer := state.free_in_buffer;
  {ASSIGN_STATE(entropy^.saved, state.cur);}
  entropy^.saved := state.cur;

  { Update restart-interval state too }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
    begin
      entropy^.restarts_to_go := cinfo^.restart_interval;
      Inc(entropy^.next_restart_num);
      with entropy^ do
        next_restart_num := next_restart_num and 7;
    end;
    Dec(entropy^.restarts_to_go);
  end;

  encode_mcu_huff := TRUE;
end;


{ Finish up at the end of a Huffman-compressed scan. }

{METHODDEF}
procedure finish_pass_huff (cinfo : j_compress_ptr);
var
  entropy : jchuff_huff_entropy_ptr;
  state : working_state;
begin
  entropy := jchuff_huff_entropy_ptr (cinfo^.entropy);

  { Load up working state ... flush_bits needs it }
  state.next_output_byte := cinfo^.dest^.next_output_byte;
  state.free_in_buffer := cinfo^.dest^.free_in_buffer;
  {ASSIGN_STATE(state.cur, entropy^.saved);}
  state.cur := entropy^.saved;
  state.cinfo := cinfo;

  { Flush out the last data }
  if not jchuff_flush_bits(state) then
    ERREXIT(j_common_ptr(cinfo), JERR_CANT_SUSPEND);

  { Update state }
  cinfo^.dest^.next_output_byte := state.next_output_byte;
  cinfo^.dest^.free_in_buffer := state.free_in_buffer;
  {ASSIGN_STATE(entropy^.saved, state.cur);}
  entropy^.saved := state.cur;
end;


{ Huffman coding optimization.

  We first scan the supplied data and count the number of uses of each symbol
  that is to be Huffman-coded. (This process MUST agree with the code above.)
  Then we build a Huffman coding tree for the observed counts.
  Symbols which are not needed at all for the particular image are not
  assigned any code, which saves space in the DHT marker as well as in
  the compressed data. }

{$ifdef ENTROPY_OPT_SUPPORTED}


{ Process a single block's worth of coefficients }

{LOCAL}
procedure htest_one_block (cinfo : j_compress_ptr;
                           const block : JBLOCK;
                           last_dc_val : int;
		           dc_counts : TLongTablePtr;
                           ac_counts : TLongTablePtr);

var
  {register} temp : int;
  {register} nbits : int;
  {register} k, r : int;
begin
  { Encode the DC coefficient difference per section F.1.2.1 }
  temp := block[0] - last_dc_val;
  if (temp < 0) then
    temp := -temp;

  { Find the number of bits needed for the magnitude of the coefficient }
  nbits := 0;
  while (temp <> 0) do
  begin
    Inc(nbits);
    temp := temp shr 1;
  end;

  { Check for out-of-range coefficient values.
    Since we're encoding a difference, the range limit is twice as much. }

  if (nbits > MAX_COEF_BITS+1) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_DCT_COEF);

  { Count the Huffman symbol for the number of bits }
  Inc(dc_counts^[nbits]);

  { Encode the AC coefficients per section F.1.2.2 }

  r := 0;			{ r := run length of zeros }

  for k := 1 to pred(DCTSIZE2) do
  begin
    temp := block[jpeg_natural_order[k]];
    if (temp = 0) then
    begin
      Inc(r);
    end
    else
    begin
      { if run length > 15, must emit special run-length-16 codes ($F0) }
      while (r > 15) do
      begin
	Inc(ac_counts^[$F0]);
	Dec(r, 16);
      end;

      { Find the number of bits needed for the magnitude of the coefficient }
      if (temp < 0) then
	temp := -temp;

      { Find the number of bits needed for the magnitude of the coefficient }
      nbits := 0;		{ there must be at least one 1 bit }
      repeat
        Inc(nbits);
        temp := temp shr 1;
      until (temp = 0);


      { Count Huffman symbol for run length / number of bits }
      Inc(ac_counts^[(r shl 4) + nbits]);

      r := 0;
    end;
  end;

  { If the last coef(s) were zero, emit an end-of-block code }
  if (r > 0) then
    Inc(ac_counts^[0]);
end;


{ Trial-encode one MCU's worth of Huffman-compressed coefficients.
  No data is actually output, so no suspension return is possible. }

{METHODDEF}
function encode_mcu_gather (cinfo : j_compress_ptr;
                           const MCU_data: array of JBLOCKROW) : boolean;
var
  entropy : jchuff_huff_entropy_ptr;
  blkn, ci : int;
  compptr : jpeg_component_info_ptr;
begin
  entropy := jchuff_huff_entropy_ptr (cinfo^.entropy);
  { Take care of restart intervals if needed }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
    begin
      { Re-initialize DC predictions to 0 }
      for ci := 0 to pred(cinfo^.comps_in_scan) do
	entropy^.saved.last_dc_val[ci] := 0;
      { Update restart state }
      entropy^.restarts_to_go := cinfo^.restart_interval;
    end;
    Dec(entropy^.restarts_to_go);
  end;

  for blkn := 0 to pred(cinfo^.blocks_in_MCU) do
  begin
    ci := cinfo^.MCU_membership[blkn];
    compptr := cinfo^.cur_comp_info[ci];
    htest_one_block(cinfo, MCU_data[blkn]^[0],
                    entropy^.saved.last_dc_val[ci],
                    entropy^.dc_count_ptrs[compptr^.dc_tbl_no],
		    entropy^.ac_count_ptrs[compptr^.ac_tbl_no]);
    entropy^.saved.last_dc_val[ci] := MCU_data[blkn]^[0][0];
  end;

  encode_mcu_gather := TRUE;
end;


{ Generate the best Huffman code table for the given counts, fill htbl.
  Note this is also used by jcphuff.c.

  The JPEG standard requires that no symbol be assigned a codeword of all
  one bits (so that padding bits added at the end of a compressed segment
  can't look like a valid code).  Because of the canonical ordering of
  codewords, this just means that there must be an unused slot in the
  longest codeword length category.  Section K.2 of the JPEG spec suggests
  reserving such a slot by pretending that symbol 256 is a valid symbol
  with count 1.  In theory that's not optimal; giving it count zero but
  including it in the symbol set anyway should give a better Huffman code.
  But the theoretically better code actually seems to come out worse in
  practice, because it produces more all-ones bytes (which incur stuffed
  zero bytes in the final file).  In any case the difference is tiny.

  The JPEG standard requires Huffman codes to be no more than 16 bits long.
  If some symbols have a very small but nonzero probability, the Huffman tree
  must be adjusted to meet the code length restriction.  We currently use
  the adjustment method suggested in JPEG section K.2.  This method is *not*
  optimal; it may not choose the best possible limited-length code.  But
  typically only very-low-frequency symbols will be given less-than-optimal
  lengths, so the code is almost optimal.  Experimental comparisons against
  an optimal limited-length-code algorithm indicate that the difference is
  microscopic --- usually less than a hundredth of a percent of total size.
  So the extra complexity of an optimal algorithm doesn't seem worthwhile. }


{GLOBAL}
procedure jpeg_gen_optimal_table (cinfo : j_compress_ptr;
                                  htbl : JHUFF_TBL_PTR;
                                  var freq : TLongTable);
const
  MAX_CLEN = 32;		{ assumed maximum initial code length }
var
  bits : array[0..MAX_CLEN+1-1] of UINT8;  { bits[k] := # of symbols with code length k }
  codesize : array[0..257-1] of int;       { codesize[k] := code length of symbol k }
  others : array[0..257-1] of int;         { next symbol in current branch of tree }
  c1, c2 : int;
  p, i, j : int;
  v : long;
begin
  { This algorithm is explained in section K.2 of the JPEG standard }

  MEMZERO(@bits, SIZEOF(bits));
  MEMZERO(@codesize, SIZEOF(codesize));
  for i := 0 to 256 do
    others[i] := -1;		{ init links to empty }

  freq[256] := 1;		{ make sure 256 has a nonzero count }
  { Including the pseudo-symbol 256 in the Huffman procedure guarantees
    that no real symbol is given code-value of all ones, because 256
    will be placed last in the largest codeword category. }

  { Huffman's basic algorithm to assign optimal code lengths to symbols }

  while TRUE do
  begin
    { Find the smallest nonzero frequency, set c1 := its symbol }
    { In case of ties, take the larger symbol number }
    c1 := -1;
    v := long(1000000000);
    for i := 0 to 256 do
    begin
      if (freq[i] <> 0) and (freq[i] <= v) then
      begin
        v := freq[i];
        c1 := i;
      end;
    end;

    { Find the next smallest nonzero frequency, set c2 := its symbol }
    { In case of ties, take the larger symbol number }
    c2 := -1;
    v := long(1000000000);
    for i := 0 to 256 do
    begin
      if (freq[i] <> 0) and (freq[i] <= v) and (i <> c1) then
      begin
        v := freq[i];
        c2 := i;
      end;
    end;

    { Done if we've merged everything into one frequency }
    if (c2 < 0) then
      break;

    { Else merge the two counts/trees }
    Inc(freq[c1], freq[c2]);
    freq[c2] := 0;

    { Increment the codesize of everything in c1's tree branch }
    Inc(codesize[c1]);
    while (others[c1] >= 0) do
    begin
      c1 := others[c1];
      Inc(codesize[c1]);
    end;

    others[c1] := c2;		{ chain c2 onto c1's tree branch }

    { Increment the codesize of everything in c2's tree branch }
    Inc(codesize[c2]);
    while (others[c2] >= 0) do
    begin
      c2 := others[c2];
      Inc(codesize[c2]);
    end;
  end;

  { Now count the number of symbols of each code length }
  for i := 0 to 256 do
  begin
    if (codesize[i]<>0) then
    begin
      { The JPEG standard seems to think that this can't happen, }
      { but I'm paranoid... }
      if (codesize[i] > MAX_CLEN) then
	ERREXIT(j_common_ptr(cinfo), JERR_HUFF_CLEN_OVERFLOW);

      Inc(bits[codesize[i]]);
    end;
  end;

  { JPEG doesn't allow symbols with code lengths over 16 bits, so if the pure
    Huffman procedure assigned any such lengths, we must adjust the coding.
    Here is what the JPEG spec says about how this next bit works:
    Since symbols are paired for the longest Huffman code, the symbols are
    removed from this length category two at a time.  The prefix for the pair
    (which is one bit shorter) is allocated to one of the pair; then,
    skipping the BITS entry for that prefix length, a code word from the next
    shortest nonzero BITS entry is converted into a prefix for two code words
    one bit longer.  }

  for i := MAX_CLEN downto 17 do
  begin
    while (bits[i] > 0) do
    begin
      j := i - 2;               { find length of new prefix to be used }
      while (bits[j] = 0) do
      	Dec(j);

      Dec(bits[i], 2);          { remove two symbols }
      Inc(bits[i-1]);           { one goes in this length }
      Inc(bits[j+1], 2);        { two new symbols in this length }
      Dec(bits[j]);		{ symbol of this length is now a prefix }
    end;
  end;

  { Delphi 2: FOR-loop variable 'i' may be undefined after loop }
  i := 16;                      { Nomssi: work around }

  { Remove the count for the pseudo-symbol 256 from the largest codelength }
  while (bits[i] = 0) do        { find largest codelength still in use }
    Dec(i);
  Dec(bits[i]);

  { Return final symbol counts (only for lengths 0..16) }
  MEMCOPY(@htbl^.bits, @bits, SIZEOF(htbl^.bits));

  { Return a list of the symbols sorted by code length }
  { It's not real clear to me why we don't need to consider the codelength
    changes made above, but the JPEG spec seems to think this works. }

  p := 0;
  for i := 1 to MAX_CLEN do
  begin
    for j := 0 to 255 do
    begin
      if (codesize[j] = i) then
      begin
	htbl^.huffval[p] := UINT8 (j);
	Inc(p);
      end;
    end;
  end;

  { Set sent_table FALSE so updated table will be written to JPEG file. }
  htbl^.sent_table := FALSE;
end;


{ Finish up a statistics-gathering pass and create the new Huffman tables. }

{METHODDEF}
procedure finish_pass_gather (cinfo : j_compress_ptr);
var
  entropy : jchuff_huff_entropy_ptr;
  ci, dctbl, actbl : int;
  compptr : jpeg_component_info_ptr;
  htblptr : ^JHUFF_TBL_PTR;
  did_dc : array[0..NUM_HUFF_TBLS-1] of boolean;
  did_ac : array[0..NUM_HUFF_TBLS-1] of boolean;
begin
  entropy := jchuff_huff_entropy_ptr (cinfo^.entropy);

  { It's important not to apply jpeg_gen_optimal_table more than once
    per table, because it clobbers the input frequency counts! }

  MEMZERO(@did_dc, SIZEOF(did_dc));
  MEMZERO(@did_ac, SIZEOF(did_ac));

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    dctbl := compptr^.dc_tbl_no;
    actbl := compptr^.ac_tbl_no;
    if (not did_dc[dctbl]) then
    begin
      htblptr := @(cinfo^.dc_huff_tbl_ptrs[dctbl]);
      if ( htblptr^ = NIL) then
	htblptr^ := jpeg_alloc_huff_table(j_common_ptr(cinfo));
      jpeg_gen_optimal_table(cinfo, htblptr^, entropy^.dc_count_ptrs[dctbl]^);
      did_dc[dctbl] := TRUE;
    end;
    if (not did_ac[actbl]) then
    begin
      htblptr := @(cinfo^.ac_huff_tbl_ptrs[actbl]);
      if ( htblptr^ = NIL) then
	htblptr^ := jpeg_alloc_huff_table(j_common_ptr(cinfo));
      jpeg_gen_optimal_table(cinfo, htblptr^, entropy^.ac_count_ptrs[actbl]^);
      did_ac[actbl] := TRUE;
    end;
  end;
end;

{$endif} { ENTROPY_OPT_SUPPORTED }


{ Module initialization routine for Huffman entropy encoding. }

{GLOBAL}
procedure jinit_huff_encoder (cinfo : j_compress_ptr);
var
  entropy : jchuff_huff_entropy_ptr;
  i : int;
begin
  entropy := jchuff_huff_entropy_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(huff_entropy_encoder)) );
  cinfo^.entropy := jpeg_entropy_encoder_ptr (entropy);
  entropy^.pub.start_pass := start_pass_huff;

  { Mark tables unallocated }
  for i := 0 to pred(NUM_HUFF_TBLS) do
  begin
    entropy^.ac_derived_tbls[i] := NIL;
    entropy^.dc_derived_tbls[i] := NIL;
{$ifdef ENTROPY_OPT_SUPPORTED}
    entropy^.ac_count_ptrs[i] := NIL;
    entropy^.dc_count_ptrs[i] := NIL;
{$endif}
  end;
end;

{== imjcphuff implementation ==}

{ Expanded entropy encoder object for progressive Huffman encoding. }
type
  jcphuff_phuff_entropy_ptr = ^phuff_entropy_encoder;
  phuff_entropy_encoder = record
    pub : jpeg_entropy_encoder; { public fields }

    { Mode flag: TRUE for optimization, FALSE for actual data output }
    gather_statistics : boolean;

    { Bit-level coding status.
      next_output_byte/free_in_buffer are local copies of cinfo^.dest fields.}

    next_output_byte : JOCTETptr; { => next byte to write in buffer }
    free_in_buffer : size_t;    { # of byte spaces remaining in buffer }
    put_buffer : INT32;		{ current bit-accumulation buffer }
    put_bits : int;             { # of bits now in it }
    cinfo : j_compress_ptr;     { link to cinfo (needed for dump_buffer) }

    { Coding status for DC components }
    last_dc_val : array[0..MAX_COMPS_IN_SCAN-1] of int;
                                { last DC coef for each component }

    { Coding status for AC components }
    ac_tbl_no : int;            { the table number of the single component }
    EOBRUN : uInt;              { run length of EOBs }
    BE : uInt;                  { # of buffered correction bits before MCU }
    bit_buffer : JBytePtr;      { buffer for correction bits (1 per char) }
    { packing correction bits tightly would save some space but cost time... }

    restarts_to_go : uInt;	{ MCUs left in this restart interval }
    next_restart_num : int;     { next restart number to write (0-7) }

    { Pointers to derived tables (these workspaces have image lifespan).
      Since any one scan codes only DC or only AC, we only need one set
      of tables, not one for DC and one for AC. }

    derived_tbls : array[0..NUM_HUFF_TBLS-1] of c_derived_tbl_ptr;

    { Statistics tables for optimization; again, one set is enough }
    count_ptrs : array[0..NUM_HUFF_TBLS-1] of TLongTablePtr;
  end;


{ MAX_CORR_BITS is the number of bits the AC refinement correction-bit
  buffer can hold.  Larger sizes may slightly improve compression, but
  1000 is already well into the realm of overkill.
  The minimum safe size is 64 bits. }

const
  MAX_CORR_BITS = 1000;         { Max # of correction bits I can buffer }


{ Forward declarations }
{METHODDEF}
function encode_mcu_DC_first (cinfo : j_compress_ptr;
                              const MCU_data: array of JBLOCKROW) : boolean;
                              forward;
{METHODDEF}
function encode_mcu_AC_first (cinfo : j_compress_ptr;
                              const MCU_data: array of JBLOCKROW) : boolean;
                              forward;
{METHODDEF}
function encode_mcu_DC_refine (cinfo : j_compress_ptr;
                              const MCU_data: array of JBLOCKROW) : boolean;
                              forward;
{METHODDEF}
function encode_mcu_AC_refine (cinfo : j_compress_ptr;
                              const MCU_data: array of JBLOCKROW) : boolean;
                              forward;

{METHODDEF}
procedure finish_pass_phuff (cinfo : j_compress_ptr); forward;

{METHODDEF}
procedure finish_pass_gather_phuff (cinfo : j_compress_ptr); forward;


{ Initialize for a Huffman-compressed scan using progressive JPEG. }

{METHODDEF}
procedure start_pass_phuff (cinfo : j_compress_ptr;
                            gather_statistics : boolean);
var
  entropy : jcphuff_phuff_entropy_ptr;
  is_DC_band : boolean;
  ci, tbl : int;
  compptr : jpeg_component_info_ptr;
begin
  tbl := 0;
  entropy := jcphuff_phuff_entropy_ptr (cinfo^.entropy);

  entropy^.cinfo := cinfo;
  entropy^.gather_statistics := gather_statistics;

  is_DC_band := (cinfo^.Ss = 0);

  { We assume jcmaster.c already validated the scan parameters. }

  { Select execution routines }
  if (cinfo^.Ah = 0) then
  begin
    if (is_DC_band) then
      entropy^.pub.encode_mcu := encode_mcu_DC_first
    else
      entropy^.pub.encode_mcu := encode_mcu_AC_first;
  end
  else
  begin
    if (is_DC_band) then
      entropy^.pub.encode_mcu := encode_mcu_DC_refine
    else
    begin
      entropy^.pub.encode_mcu := encode_mcu_AC_refine;
      { AC refinement needs a correction bit buffer }
      if (entropy^.bit_buffer = NIL) then
	entropy^.bit_buffer := JBytePtr(
	  cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				      MAX_CORR_BITS * SIZEOF(byte)) );
    end;
  end;
  if (gather_statistics) then
    entropy^.pub.finish_pass := finish_pass_gather_phuff
  else
    entropy^.pub.finish_pass := finish_pass_phuff;

  { Only DC coefficients may be interleaved, so cinfo^.comps_in_scan = 1
    for AC coefficients. }

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    { Initialize DC predictions to 0 }
    entropy^.last_dc_val[ci] := 0;
    { Get table index }
    if (is_DC_band) then
    begin
      if (cinfo^.Ah <> 0) then  { DC refinement needs no table }
	continue;
      tbl := compptr^.dc_tbl_no;
    end
    else
    begin
      tbl := compptr^.ac_tbl_no;
      entropy^.ac_tbl_no := tbl;
    end;
    if (gather_statistics) then
    begin
      { Check for invalid table index }
      { (make_c_derived_tbl does this in the other path) }
      if (tbl < 0) or (tbl >= NUM_HUFF_TBLS) then
        ERREXIT1(j_common_ptr(cinfo), JERR_NO_HUFF_TABLE, tbl);
      { Allocate and zero the statistics tables }
      { Note that jpeg_gen_optimal_table expects 257 entries in each table! }
      if (entropy^.count_ptrs[tbl] = NIL) then
	entropy^.count_ptrs[tbl] := TLongTablePtr(
	  cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				      257 * SIZEOF(long)) );
      MEMZERO(entropy^.count_ptrs[tbl], 257 * SIZEOF(long));
    end else
    begin
      { Compute derived values for Huffman table }
      { We may do this more than once for a table, but it's not expensive }
      jpeg_make_c_derived_tbl(cinfo, is_DC_band, tbl,
			      entropy^.derived_tbls[tbl]);
    end;
  end;

  { Initialize AC stuff }
  entropy^.EOBRUN := 0;
  entropy^.BE := 0;

  { Initialize bit buffer to empty }
  entropy^.put_buffer := 0;
  entropy^.put_bits := 0;

  { Initialize restart stuff }
  entropy^.restarts_to_go := cinfo^.restart_interval;
  entropy^.next_restart_num := 0;
end;


{LOCAL}
procedure jcphuff_dump_buffer (entropy : jcphuff_phuff_entropy_ptr);
{ Empty the output buffer; we do not support suspension in this module. }
var
  dest : jpeg_destination_mgr_ptr;
begin
  dest := entropy^.cinfo^.dest;

  if (not dest^.empty_output_buffer (entropy^.cinfo)) then
    ERREXIT(j_common_ptr(entropy^.cinfo), JERR_CANT_SUSPEND);
  { After a successful buffer dump, must reset buffer pointers }
  entropy^.next_output_byte := dest^.next_output_byte;
  entropy^.free_in_buffer := dest^.free_in_buffer;
end;


{ Outputting bits to the file }

{ Only the right 24 bits of put_buffer are used; the valid bits are
  left-justified in this part.  At most 16 bits can be passed to emit_bits
  in one call, and we never retain more than 7 bits in put_buffer
  between calls, so 24 bits are sufficient. }


{LOCAL}
procedure jcphuff_emit_bits (entropy : jcphuff_phuff_entropy_ptr;
                     code : uInt;
                     size : int); {INLINE}
{ Emit some bits, unless we are in gather mode }
var
  {register} put_buffer : INT32;
  {register} put_bits : int;
var
  c : int;
begin
  { This routine is heavily used, so it's worth coding tightly. }
  put_buffer := INT32 (code);
  put_bits := entropy^.put_bits;

  { if size is 0, caller used an invalid Huffman table entry }
  if (size = 0) then
    ERREXIT(j_common_ptr(entropy^.cinfo), JERR_HUFF_MISSING_CODE);

  if (entropy^.gather_statistics) then
    exit;			{ do nothing if we're only getting stats }

  put_buffer := put_buffer and ((INT32(1) shl size) - 1);
                                { mask off any extra bits in code }

  Inc(put_bits, size);          { new number of bits in buffer }

  put_buffer := put_buffer shl (24 - put_bits); { align incoming bits }

  put_buffer := put_buffer or entropy^.put_buffer;
                                { and merge with old buffer contents }

  while (put_bits >= 8) do
  begin
    c := int ((put_buffer shr 16) and $FF);

    {emit_byte(entropy, c);}
    { Outputting bytes to the file.
      NB: these must be called only when actually outputting,
      that is, entropy^.gather_statistics = FALSE. }
    { Emit a byte }
    entropy^.next_output_byte^ := JOCTET(c);
    Inc(entropy^.next_output_byte);
    Dec(entropy^.free_in_buffer);
    if (entropy^.free_in_buffer = 0) then
      jcphuff_dump_buffer(entropy);

    if (c = $FF) then
    begin		{ need to stuff a zero byte? }
      {emit_byte(entropy, 0);}
      entropy^.next_output_byte^ := JOCTET(0);
      Inc(entropy^.next_output_byte);
      Dec(entropy^.free_in_buffer);
      if (entropy^.free_in_buffer = 0) then
        jcphuff_dump_buffer(entropy);
    end;
    put_buffer := put_buffer shl 8;
    Dec(put_bits, 8);
  end;

  entropy^.put_buffer := put_buffer; { update variables }
  entropy^.put_bits := put_bits;
end;


{LOCAL}
procedure jcphuff_flush_bits (entropy : jcphuff_phuff_entropy_ptr);
begin
  jcphuff_emit_bits(entropy, $7F, 7); { fill any partial byte with ones }
  entropy^.put_buffer := 0;     { and reset bit-buffer to empty }
  entropy^.put_bits := 0;
end;

{ Emit (or just count) a Huffman symbol. }


{LOCAL}
procedure emit_symbol (entropy : jcphuff_phuff_entropy_ptr;
                       tbl_no : int;
                       symbol : int); {INLINE}
var
  tbl : c_derived_tbl_ptr;
begin
  if (entropy^.gather_statistics) then
    Inc(entropy^.count_ptrs[tbl_no]^[symbol])
  else
  begin
    tbl := entropy^.derived_tbls[tbl_no];
    jcphuff_emit_bits(entropy, tbl^.ehufco[symbol], tbl^.ehufsi[symbol]);
  end;
end;


{ Emit bits from a correction bit buffer. }

{LOCAL}
procedure emit_buffered_bits (entropy : jcphuff_phuff_entropy_ptr;
                              bufstart : JBytePtr;
		              nbits : uInt);
var
  bufptr : byteptr;
begin
  if (entropy^.gather_statistics) then
    exit;			{ no real work }

  bufptr := byteptr(bufstart);
  while (nbits > 0) do
  begin
    jcphuff_emit_bits(entropy, uInt(bufptr^), 1);
    Inc(bufptr);
    Dec(nbits);
  end;
end;


{ Emit any pending EOBRUN symbol. }

{LOCAL}
procedure emit_eobrun (entropy : jcphuff_phuff_entropy_ptr);
var
  {register} temp, nbits : int;
begin
  if (entropy^.EOBRUN > 0) then
  begin	                       { if there is any pending EOBRUN }
    temp := entropy^.EOBRUN;
    nbits := 0;
    temp := temp shr 1;
    while (temp <> 0) do
    begin
      Inc(nbits);
      temp := temp shr 1;
    end;

    { safety check: shouldn't happen given limited correction-bit buffer }
    if (nbits > 14) then
      ERREXIT(j_common_ptr(entropy^.cinfo), JERR_HUFF_MISSING_CODE);

    emit_symbol(entropy, entropy^.ac_tbl_no, nbits shl 4);
    if (nbits <> 0) then
      jcphuff_emit_bits(entropy, entropy^.EOBRUN, nbits);

    entropy^.EOBRUN := 0;

    { Emit any buffered correction bits }
    emit_buffered_bits(entropy, entropy^.bit_buffer, entropy^.BE);
    entropy^.BE := 0;
  end;
end;


{ Emit a restart marker & resynchronize predictions. }

{LOCAL}
procedure jcphuff_emit_restart (entropy : jcphuff_phuff_entropy_ptr;
                        restart_num : int);
var
  ci : int;
begin
  emit_eobrun(entropy);

  if (not entropy^.gather_statistics) then
  begin
    jcphuff_flush_bits(entropy);
    {emit_byte(entropy, $FF);}
    { Outputting bytes to the file.
      NB: these must be called only when actually outputting,
      that is, entropy^.gather_statistics = FALSE. }

    entropy^.next_output_byte^ := JOCTET($FF);
    Inc(entropy^.next_output_byte);
    Dec(entropy^.free_in_buffer);
    if (entropy^.free_in_buffer = 0) then
      jcphuff_dump_buffer(entropy);

    {emit_byte(entropy, JPEG_RST0 + restart_num);}
    entropy^.next_output_byte^ := JOCTET(JPEG_RST0 + restart_num);
    Inc(entropy^.next_output_byte);
    Dec(entropy^.free_in_buffer);
    if (entropy^.free_in_buffer = 0) then
      jcphuff_dump_buffer(entropy);
  end;

  if (entropy^.cinfo^.Ss = 0) then
  begin
    { Re-initialize DC predictions to 0 }
    for ci := 0 to pred(entropy^.cinfo^.comps_in_scan) do
      entropy^.last_dc_val[ci] := 0;
  end
  else
  begin
    { Re-initialize all AC-related fields to 0 }
    entropy^.EOBRUN := 0;
    entropy^.BE := 0;
  end;
end;


{ MCU encoding for DC initial scan (either spectral selection,
  or first pass of successive approximation). }

{METHODDEF}
function encode_mcu_DC_first (cinfo : j_compress_ptr;
                              const MCU_data: array of JBLOCKROW) : boolean;
var
  entropy : jcphuff_phuff_entropy_ptr;
  {register} temp, temp2 : int;
  {register} nbits : int;
  blkn, ci : int;
  Al : int;
  block : JBLOCK_PTR;
  compptr : jpeg_component_info_ptr;
  ishift_temp : int;
begin
  entropy := jcphuff_phuff_entropy_ptr (cinfo^.entropy);
  Al := cinfo^.Al;

  entropy^.next_output_byte := cinfo^.dest^.next_output_byte;
  entropy^.free_in_buffer := cinfo^.dest^.free_in_buffer;

  { Emit restart marker if needed }
  if (cinfo^.restart_interval <> 0) then
    if (entropy^.restarts_to_go = 0) then
      jcphuff_emit_restart(entropy, entropy^.next_restart_num);

  { Encode the MCU data blocks }
  for blkn := 0 to pred(cinfo^.blocks_in_MCU) do
  begin
    block := JBLOCK_PTR(MCU_data[blkn]);
    ci := cinfo^.MCU_membership[blkn];
    compptr := cinfo^.cur_comp_info[ci];

    { Compute the DC value after the required point transform by Al.
      This is simply an arithmetic right shift. }

    {temp2 := IRIGHT_SHIFT( int(block^[0]), Al);}
    {IRIGHT_SHIFT_IS_UNSIGNED}
    ishift_temp := int(block^[0]);
    if ishift_temp < 0 then
      temp2 := (ishift_temp shr Al) or ((not 0) shl (16-Al))
    else
      temp2 := ishift_temp shr Al;


    { DC differences are figured on the point-transformed values. }
    temp := temp2 - entropy^.last_dc_val[ci];
    entropy^.last_dc_val[ci] := temp2;

    { Encode the DC coefficient difference per section G.1.2.1 }
    temp2 := temp;
    if (temp < 0) then
    begin
      temp := -temp;		{ temp is abs value of input }
      { For a negative input, want temp2 := bitwise complement of abs(input) }
      { This code assumes we are on a two's complement machine }
      Dec(temp2);
    end;

    { Find the number of bits needed for the magnitude of the coefficient }
    nbits := 0;
    while (temp <> 0) do
    begin
      Inc(nbits);
      temp := temp shr 1;
    end;

    { Check for out-of-range coefficient values.
      Since we're encoding a difference, the range limit is twice as much. }

    if (nbits > MAX_COEF_BITS+1) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_DCT_COEF);

    { Count/emit the Huffman-coded symbol for the number of bits }
    emit_symbol(entropy, compptr^.dc_tbl_no, nbits);

    { Emit that number of bits of the value, if positive, }
    { or the complement of its magnitude, if negative. }
    if (nbits <> 0) then       { emit_bits rejects calls with size 0 }
      jcphuff_emit_bits(entropy, uInt(temp2), nbits);
  end;

  cinfo^.dest^.next_output_byte := entropy^.next_output_byte;
  cinfo^.dest^.free_in_buffer := entropy^.free_in_buffer;

  { Update restart-interval state too }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
    begin
      entropy^.restarts_to_go := cinfo^.restart_interval;
      Inc(entropy^.next_restart_num);
      with entropy^ do
        next_restart_num := next_restart_num and 7;
    end;
    Dec(entropy^.restarts_to_go);
  end;

  encode_mcu_DC_first := TRUE;
end;


{ MCU encoding for AC initial scan (either spectral selection,
  or first pass of successive approximation). }

{METHODDEF}
function encode_mcu_AC_first (cinfo : j_compress_ptr;
                              const MCU_data: array of JBLOCKROW) : boolean;
var
  entropy : jcphuff_phuff_entropy_ptr;
  {register} temp, temp2 : int;
  {register} nbits : int;
  {register} r, k : int;
  Se : int;
  Al : int;
  block : JBLOCK_PTR;
begin
  entropy := jcphuff_phuff_entropy_ptr (cinfo^.entropy);
  Se := cinfo^.Se;
  Al := cinfo^.Al;

  entropy^.next_output_byte := cinfo^.dest^.next_output_byte;
  entropy^.free_in_buffer := cinfo^.dest^.free_in_buffer;

  { Emit restart marker if needed }
  if (cinfo^.restart_interval <> 0) then
    if (entropy^.restarts_to_go = 0) then
      jcphuff_emit_restart(entropy, entropy^.next_restart_num);

  { Encode the MCU data block }
  block := JBLOCK_PTR(MCU_data[0]);

  { Encode the AC coefficients per section G.1.2.2, fig. G.3 }

  r := 0;			{ r := run length of zeros }

  for k := cinfo^.Ss to Se do
  begin
    temp := (block^[jpeg_natural_order[k]]);
    if (temp = 0) then
    begin
      Inc(r);
      continue;
    end;
    { We must apply the point transform by Al.  For AC coefficients this
      is an integer division with rounding towards 0.  To do this portably
      in C, we shift after obtaining the absolute value; so the code is
      interwoven with finding the abs value (temp) and output bits (temp2). }

    if (temp < 0) then
    begin
      temp := -temp;		{ temp is abs value of input }
      temp := temp shr Al;	{ apply the point transform }
      { For a negative coef, want temp2 := bitwise complement of abs(coef) }
      temp2 := not temp;
    end
    else
    begin
      temp := temp shr Al;	{ apply the point transform }
      temp2 := temp;
    end;
    { Watch out for case that nonzero coef is zero after point transform }
    if (temp = 0) then
    begin
      Inc(r);
      continue;
    end;

    { Emit any pending EOBRUN }
    if (entropy^.EOBRUN > 0) then
      emit_eobrun(entropy);
    { if run length > 15, must emit special run-length-16 codes ($F0) }
    while (r > 15) do
    begin
      emit_symbol(entropy, entropy^.ac_tbl_no, $F0);
      Dec(r, 16);
    end;

    { Find the number of bits needed for the magnitude of the coefficient }
    nbits := 0;			{ there must be at least one 1 bit }
    repeat
      Inc(nbits);
      temp := temp shr 1;
    until (temp = 0);

    { Check for out-of-range coefficient values }
    if (nbits > MAX_COEF_BITS) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_DCT_COEF);

    { Count/emit Huffman symbol for run length / number of bits }
    emit_symbol(entropy, entropy^.ac_tbl_no, (r shl 4) + nbits);

    { Emit that number of bits of the value, if positive, }
    { or the complement of its magnitude, if negative. }
    jcphuff_emit_bits(entropy, uInt(temp2), nbits);

    r := 0;			{ reset zero run length }
  end;

  if (r > 0) then
  begin			        { If there are trailing zeroes, }
    Inc(entropy^.EOBRUN);	{ count an EOB }
    if (entropy^.EOBRUN = $7FFF) then
      emit_eobrun(entropy);	{ force it out to avoid overflow }
  end;

  cinfo^.dest^.next_output_byte := entropy^.next_output_byte;
  cinfo^.dest^.free_in_buffer := entropy^.free_in_buffer;

  { Update restart-interval state too }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
    begin
      entropy^.restarts_to_go := cinfo^.restart_interval;
      Inc(entropy^.next_restart_num);
      with entropy^ do
        next_restart_num := next_restart_num and 7;
    end;
    Dec(entropy^.restarts_to_go);
  end;

  encode_mcu_AC_first := TRUE;
end;


{ MCU encoding for DC successive approximation refinement scan.
  Note: we assume such scans can be multi-component, although the spec
  is not very clear on the point. }

{METHODDEF}
function encode_mcu_DC_refine (cinfo : j_compress_ptr;
                              const MCU_data: array of JBLOCKROW) : boolean;
var
  entropy : jcphuff_phuff_entropy_ptr;
  {register} temp : int;
  blkn : int;
  Al : int;
  block : JBLOCK_PTR;
begin
  entropy := jcphuff_phuff_entropy_ptr (cinfo^.entropy);
  Al := cinfo^.Al;

  entropy^.next_output_byte := cinfo^.dest^.next_output_byte;
  entropy^.free_in_buffer := cinfo^.dest^.free_in_buffer;

  { Emit restart marker if needed }
  if (cinfo^.restart_interval <> 0) then
    if (entropy^.restarts_to_go = 0) then
      jcphuff_emit_restart(entropy, entropy^.next_restart_num);

  { Encode the MCU data blocks }
  for blkn := 0 to pred(cinfo^.blocks_in_MCU) do
  begin
    block := JBLOCK_PTR(MCU_data[blkn]);

    { We simply emit the Al'th bit of the DC coefficient value. }
    temp := block^[0];
    jcphuff_emit_bits(entropy, uInt(temp shr Al), 1);
  end;

  cinfo^.dest^.next_output_byte := entropy^.next_output_byte;
  cinfo^.dest^.free_in_buffer := entropy^.free_in_buffer;

  { Update restart-interval state too }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
    begin
      entropy^.restarts_to_go := cinfo^.restart_interval;
      Inc(entropy^.next_restart_num);
      with entropy^ do
        next_restart_num := next_restart_num and 7;
    end;
    Dec(entropy^.restarts_to_go);
  end;

  encode_mcu_DC_refine := TRUE;
end;


{ MCU encoding for AC successive approximation refinement scan. }

{METHODDEF}
function encode_mcu_AC_refine (cinfo : j_compress_ptr;
                               const MCU_data: array of JBLOCKROW) : boolean;

var
  entropy : jcphuff_phuff_entropy_ptr;
  {register} temp : int;
  {register} r, k : int;
  EOB : int;
  BR_buffer : JBytePtr;
  BR : uInt;
  Se : int;
  Al : int;
  block : JBLOCK_PTR;
  absvalues : array[0..DCTSIZE2-1] of int;
begin
  entropy := jcphuff_phuff_entropy_ptr(cinfo^.entropy);
  Se := cinfo^.Se;
  Al := cinfo^.Al;

  entropy^.next_output_byte := cinfo^.dest^.next_output_byte;
  entropy^.free_in_buffer := cinfo^.dest^.free_in_buffer;

  { Emit restart marker if needed }
  if (cinfo^.restart_interval <> 0) then
    if (entropy^.restarts_to_go = 0) then
      jcphuff_emit_restart(entropy, entropy^.next_restart_num);

  { Encode the MCU data block }
  block := JBLOCK_PTR(MCU_data[0]);

  { It is convenient to make a pre-pass to determine the transformed
    coefficients' absolute values and the EOB position. }

  EOB := 0;
  for k := cinfo^.Ss to Se do
  begin
    temp := block^[jpeg_natural_order[k]];
    { We must apply the point transform by Al.  For AC coefficients this
      is an integer division with rounding towards 0.  To do this portably
      in C, we shift after obtaining the absolute value. }

    if (temp < 0) then
      temp := -temp;		{ temp is abs value of input }
    temp := temp shr Al;		{ apply the point transform }
    absvalues[k] := temp;	{ save abs value for main pass }
    if (temp = 1) then
      EOB := k;			{ EOB := index of last newly-nonzero coef }
  end;

  { Encode the AC coefficients per section G.1.2.3, fig. G.7 }

  r := 0;			{ r := run length of zeros }
  BR := 0;			{ BR := count of buffered bits added now }
  BR_buffer := JBytePtr(@(entropy^.bit_buffer^[entropy^.BE]));
                                { Append bits to buffer }

  for k := cinfo^.Ss to Se do
  begin
    temp := absvalues[k];
    if (temp = 0) then
    begin
      Inc(r);
      continue;
    end;

    { Emit any required ZRLs, but not if they can be folded into EOB }
    while (r > 15) and (k <= EOB) do
    begin
      { emit any pending EOBRUN and the BE correction bits }
      emit_eobrun(entropy);
      { Emit ZRL }
      emit_symbol(entropy, entropy^.ac_tbl_no, $F0);
      Dec(r, 16);
      { Emit buffered correction bits that must be associated with ZRL }
      emit_buffered_bits(entropy, BR_buffer, BR);
      BR_buffer := entropy^.bit_buffer; { BE bits are gone now }
      BR := 0;
    end;

    { If the coef was previously nonzero, it only needs a correction bit.
      NOTE: a straight translation of the spec's figure G.7 would suggest
      that we also need to test r > 15.  But if r > 15, we can only get here
      if k > EOB, which implies that this coefficient is not 1. }
    if (temp > 1) then
    begin
      { The correction bit is the next bit of the absolute value. }
      BR_buffer^[BR] := byte (temp and 1);
      Inc(BR);
      continue;
    end;

    { Emit any pending EOBRUN and the BE correction bits }
    emit_eobrun(entropy);

    { Count/emit Huffman symbol for run length / number of bits }
    emit_symbol(entropy, entropy^.ac_tbl_no, (r shl 4) + 1);

    { Emit output bit for newly-nonzero coef }
    if (block^[jpeg_natural_order[k]] < 0) then
      temp := 0
    else
      temp := 1;
    jcphuff_emit_bits(entropy, uInt(temp), 1);

    { Emit buffered correction bits that must be associated with this code }
    emit_buffered_bits(entropy, BR_buffer, BR);
    BR_buffer := entropy^.bit_buffer; { BE bits are gone now }
    BR := 0;
    r := 0;			{ reset zero run length }
  end;

  if (r > 0) or (BR > 0) then
  begin	                        { If there are trailing zeroes, }
    Inc(entropy^.EOBRUN);       { count an EOB }
    Inc(entropy^.BE, BR);          { concat my correction bits to older ones }
    { We force out the EOB if we risk either:
      1. overflow of the EOB counter;
      2. overflow of the correction bit buffer during the next MCU. }

    if (entropy^.EOBRUN = $7FFF) or
       (entropy^.BE > (MAX_CORR_BITS-DCTSIZE2+1)) then
      emit_eobrun(entropy);
  end;

  cinfo^.dest^.next_output_byte := entropy^.next_output_byte;
  cinfo^.dest^.free_in_buffer := entropy^.free_in_buffer;

  { Update restart-interval state too }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
    begin
      entropy^.restarts_to_go := cinfo^.restart_interval;
      Inc(entropy^.next_restart_num);
      with entropy^ do
        next_restart_num := next_restart_num and 7;
    end;
    Dec(entropy^.restarts_to_go);
  end;

  encode_mcu_AC_refine := TRUE;
end;


{ Finish up at the end of a Huffman-compressed progressive scan. }

{METHODDEF}
procedure finish_pass_phuff (cinfo : j_compress_ptr);
var
  entropy : jcphuff_phuff_entropy_ptr;
begin
  entropy := jcphuff_phuff_entropy_ptr (cinfo^.entropy);

  entropy^.next_output_byte := cinfo^.dest^.next_output_byte;
  entropy^.free_in_buffer := cinfo^.dest^.free_in_buffer;

  { Flush out any buffered data }
  emit_eobrun(entropy);
  jcphuff_flush_bits(entropy);

  cinfo^.dest^.next_output_byte := entropy^.next_output_byte;
  cinfo^.dest^.free_in_buffer := entropy^.free_in_buffer;
end;


{ Finish up a statistics-gathering pass and create the new Huffman tables. }

{METHODDEF}
procedure finish_pass_gather_phuff (cinfo : j_compress_ptr);
var
  entropy : jcphuff_phuff_entropy_ptr;
  is_DC_band : boolean;
  ci, tbl : int;
  compptr : jpeg_component_info_ptr;
  htblptr : ^JHUFF_TBL_PTR;
  did : array[0..NUM_HUFF_TBLS-1] of boolean;
begin
  tbl := 0;
  entropy := jcphuff_phuff_entropy_ptr (cinfo^.entropy);

  { Flush out buffered data (all we care about is counting the EOB symbol) }
  emit_eobrun(entropy);

  is_DC_band := (cinfo^.Ss = 0);

  { It's important not to apply jpeg_gen_optimal_table more than once
    per table, because it clobbers the input frequency counts! }

  MEMZERO(@did, SIZEOF(did));

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    if (is_DC_band) then
    begin
      if (cinfo^.Ah <> 0) then     { DC refinement needs no table }
	continue;
      tbl := compptr^.dc_tbl_no;
    end
    else
    begin
      tbl := compptr^.ac_tbl_no;
    end;
    if (not did[tbl]) then
    begin
      if (is_DC_band) then
        htblptr := @(cinfo^.dc_huff_tbl_ptrs[tbl])
      else
        htblptr := @(cinfo^.ac_huff_tbl_ptrs[tbl]);
      if (htblptr^ = NIL) then
        htblptr^ := jpeg_alloc_huff_table(j_common_ptr(cinfo));
      jpeg_gen_optimal_table(cinfo, htblptr^, entropy^.count_ptrs[tbl]^);
      did[tbl] := TRUE;
    end;
  end;
end;


{ Module initialization routine for progressive Huffman entropy encoding. }

{GLOBAL}
procedure jinit_phuff_encoder (cinfo : j_compress_ptr);
var
  entropy : jcphuff_phuff_entropy_ptr;
  i : int;
begin
  entropy := jcphuff_phuff_entropy_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(phuff_entropy_encoder)) );
  cinfo^.entropy := jpeg_entropy_encoder_ptr(entropy);
  entropy^.pub.start_pass := start_pass_phuff;

  { Mark tables unallocated }
  for i := 0 to pred(NUM_HUFF_TBLS) do
  begin
    entropy^.derived_tbls[i] := NIL;
    entropy^.count_ptrs[i] := NIL;
  end;
  entropy^.bit_buffer := NIL;	{ needed only in AC refinement scan }
end;

{== imjccoefct implementation ==}

{ Private buffer controller object }

type
  jccoefct_my_coef_ptr = ^jccoefct_my_coef_controller;
  jccoefct_my_coef_controller = record
    pub : jpeg_c_coef_controller; { public fields }

    iMCU_row_num : JDIMENSION;	{ iMCU row # within image }
    mcu_ctr : JDIMENSION;	{ counts MCUs processed in current row }
    MCU_vert_offset : int;	{ counts MCU rows within iMCU row }
    MCU_rows_per_iMCU_row : int;  { number of such rows needed }

    { For single-pass compression, it's sufficient to buffer just one MCU
      (although this may prove a bit slow in practice).  We allocate a
      workspace of C_MAX_BLOCKS_IN_MCU coefficient blocks, and reuse it for each
      MCU constructed and sent.  (On 80x86, the workspace is FAR even though
      it's not really very big; this is to keep the module interfaces unchanged
      when a large coefficient buffer is necessary.)
      In multi-pass modes, this array points to the current MCU's blocks
      within the virtual arrays. }

    MCU_buffer : array[0..C_MAX_BLOCKS_IN_MCU-1] of JBLOCKROW;

    { In multi-pass modes, we need a virtual block array for each component. }
    whole_image : array[0..MAX_COMPONENTS-1] of jvirt_barray_ptr;
  end;


{ Forward declarations }
{METHODDEF}
function compress_data(cinfo : j_compress_ptr;
                       input_buf : JSAMPIMAGE) : boolean; forward;
{$ifdef FULL_COEF_BUFFER_SUPPORTED}
{METHODDEF}
function compress_first_pass(cinfo : j_compress_ptr;
                             input_buf : JSAMPIMAGE) : boolean;  forward;
{METHODDEF}
function compress_output(cinfo : j_compress_ptr;
                         input_buf : JSAMPIMAGE) : boolean;  forward;
{$endif}


{LOCAL}
procedure jccoefct_start_iMCU_row (cinfo : j_compress_ptr);
{ Reset within-iMCU-row counters for a new row }
var
  coef : jccoefct_my_coef_ptr;
begin
  coef := jccoefct_my_coef_ptr (cinfo^.coef);

  { In an interleaved scan, an MCU row is the same as an iMCU row.
    In a noninterleaved scan, an iMCU row has v_samp_factor MCU rows.
    But at the bottom of the image, process only what's left. }
  if (cinfo^.comps_in_scan > 1) then
  begin
    coef^.MCU_rows_per_iMCU_row := 1;
  end
  else
  begin
    if (coef^.iMCU_row_num < (cinfo^.total_iMCU_rows-1)) then
      coef^.MCU_rows_per_iMCU_row := cinfo^.cur_comp_info[0]^.v_samp_factor
    else
      coef^.MCU_rows_per_iMCU_row := cinfo^.cur_comp_info[0]^.last_row_height;
  end;

  coef^.mcu_ctr := 0;
  coef^.MCU_vert_offset := 0;
end;


{ Initialize for a processing pass. }

{METHODDEF}
procedure start_pass_coef (cinfo : j_compress_ptr;
                           pass_mode : J_BUF_MODE);
var
  coef : jccoefct_my_coef_ptr;
begin
  coef := jccoefct_my_coef_ptr (cinfo^.coef);

  coef^.iMCU_row_num := 0;
  jccoefct_start_iMCU_row(cinfo);

  case (pass_mode) of
  JBUF_PASS_THRU:
    begin
      if (coef^.whole_image[0] <> NIL) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
      coef^.pub.compress_data := compress_data;
    end;
{$ifdef FULL_COEF_BUFFER_SUPPORTED}
  JBUF_SAVE_AND_PASS:
    begin
      if (coef^.whole_image[0] = NIL) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
      coef^.pub.compress_data := compress_first_pass;
    end;
  JBUF_CRANK_DEST:
    begin
      if (coef^.whole_image[0] = NIL) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
      coef^.pub.compress_data := compress_output;
    end;
{$endif}
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
  end;
end;


{ Process some data in the single-pass case.
  We process the equivalent of one fully interleaved MCU row ("iMCU" row)
  per call, ie, v_samp_factor block rows for each component in the image.
  Returns TRUE if the iMCU row is completed, FALSE if suspended.

  NB: input_buf contains a plane for each component in image,
  which we index according to the component's SOF position. }


{METHODDEF}
function compress_data (cinfo : j_compress_ptr;
                        input_buf : JSAMPIMAGE) : boolean;
var
  coef : jccoefct_my_coef_ptr;
  MCU_col_num : JDIMENSION;	{ index of current MCU within row }
  last_MCU_col : JDIMENSION;
  last_iMCU_row : JDIMENSION;
  blkn, bi, ci, yindex, yoffset, blockcnt : int;
  ypos, xpos : JDIMENSION;
  compptr : jpeg_component_info_ptr;
begin
  coef := jccoefct_my_coef_ptr (cinfo^.coef);
  last_MCU_col := cinfo^.MCUs_per_row - 1;
  last_iMCU_row := cinfo^.total_iMCU_rows - 1;

  { Loop to write as much as one whole iMCU row }
  for yoffset := coef^.MCU_vert_offset to pred(coef^.MCU_rows_per_iMCU_row) do
  begin
    for MCU_col_num := coef^.mcu_ctr to last_MCU_col do
    begin
      { Determine where data comes from in input_buf and do the DCT thing.
        Each call on forward_DCT processes a horizontal row of DCT blocks
        as wide as an MCU; we rely on having allocated the MCU_buffer[] blocks
        sequentially.  Dummy blocks at the right or bottom edge are filled in
        specially.  The data in them does not matter for image reconstruction,
        so we fill them with values that will encode to the smallest amount of
        data, viz: all zeroes in the AC entries, DC entries equal to previous
        block's DC value.  (Thanks to Thomas Kinsman for this idea.) }

      blkn := 0;
      for ci := 0 to pred(cinfo^.comps_in_scan) do
      begin
	compptr := cinfo^.cur_comp_info[ci];
        if (MCU_col_num < last_MCU_col)  then
          blockcnt := compptr^.MCU_width
        else
          blockcnt := compptr^.last_col_width;
	xpos := MCU_col_num * JDIMENSION(compptr^.MCU_sample_width);
	ypos := yoffset * DCTSIZE;      { ypos = (yoffset+yindex) * DCTSIZE }
	for yindex := 0 to pred(compptr^.MCU_height) do
        begin
	  if (coef^.iMCU_row_num < last_iMCU_row) or
	     (yoffset+yindex < compptr^.last_row_height) then
          begin
	    cinfo^.fdct^.forward_DCT (cinfo, compptr,
				      input_buf^[compptr^.component_index],
				      coef^.MCU_buffer[blkn],
				      ypos, xpos, JDIMENSION (blockcnt));

	    if (blockcnt < compptr^.MCU_width) then
            begin
	      { Create some dummy blocks at the right edge of the image. }
	      jzero_far({FAR}pointer(coef^.MCU_buffer[blkn + blockcnt]),
			(compptr^.MCU_width - blockcnt) * SIZEOF(JBLOCK));
	      for bi := blockcnt to pred(compptr^.MCU_width) do
              begin
		coef^.MCU_buffer[blkn+bi]^[0][0] := coef^.MCU_buffer[blkn+bi-1]^[0][0];
	      end;
	    end;
	  end
          else
          begin
	    { Create a row of dummy blocks at the bottom of the image. }
	    jzero_far({FAR}pointer(coef^.MCU_buffer[blkn]),
		      compptr^.MCU_width * SIZEOF(JBLOCK));
	    for bi := 0 to pred(compptr^.MCU_width) do
            begin
	      coef^.MCU_buffer[blkn+bi]^[0][0] := coef^.MCU_buffer[blkn-1]^[0][0];
	    end;
	  end;
	  Inc(blkn, compptr^.MCU_width);
	  Inc(ypos, DCTSIZE);
	end;
      end;
      { Try to write the MCU.  In event of a suspension failure, we will
        re-DCT the MCU on restart (a bit inefficient, could be fixed...) }

      if (not cinfo^.entropy^.encode_mcu (cinfo, JBLOCKARRAY(@coef^.MCU_buffer)^)) then
      begin
	{ Suspension forced; update state counters and exit }
	coef^.MCU_vert_offset := yoffset;
	coef^.mcu_ctr := MCU_col_num;
	compress_data := FALSE;
        exit;
      end;
    end;
    { Completed an MCU row, but perhaps not an iMCU row }
    coef^.mcu_ctr := 0;
  end;
  { Completed the iMCU row, advance counters for next one }
  Inc(coef^.iMCU_row_num);
  jccoefct_start_iMCU_row(cinfo);
  compress_data := TRUE;
end;


{$ifdef FULL_COEF_BUFFER_SUPPORTED}

{ Process some data in the first pass of a multi-pass case.
  We process the equivalent of one fully interleaved MCU row ("iMCU" row)
  per call, ie, v_samp_factor block rows for each component in the image.
  This amount of data is read from the source buffer, DCT'd and quantized,
  and saved into the virtual arrays.  We also generate suitable dummy blocks
  as needed at the right and lower edges.  (The dummy blocks are constructed
  in the virtual arrays, which have been padded appropriately.)  This makes
  it possible for subsequent passes not to worry about real vs. dummy blocks.

  We must also emit the data to the entropy encoder.  This is conveniently
  done by calling compress_output() after we've loaded the current strip
  of the virtual arrays.

  NB: input_buf contains a plane for each component in image.  All
  components are DCT'd and loaded into the virtual arrays in this pass.
  However, it may be that only a subset of the components are emitted to
  the entropy encoder during this first pass; be careful about looking
  at the scan-dependent variables (MCU dimensions, etc). }

{METHODDEF}
function compress_first_pass (cinfo : j_compress_ptr;
                              input_buf : JSAMPIMAGE) : boolean;
var
  coef : jccoefct_my_coef_ptr;
  last_iMCU_row : JDIMENSION;
  blocks_across, MCUs_across, MCUindex : JDIMENSION;
  bi, ci, h_samp_factor, block_row, block_rows, ndummy : int;
  lastDC : JCOEF;
  compptr : jpeg_component_info_ptr;
  buffer : JBLOCKARRAY;
  thisblockrow, lastblockrow : JBLOCKROW;
begin
  coef := jccoefct_my_coef_ptr (cinfo^.coef);
  last_iMCU_row := cinfo^.total_iMCU_rows - 1;

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Align the virtual buffer for this component. }
    buffer := cinfo^.mem^.access_virt_barray
      (j_common_ptr(cinfo), coef^.whole_image[ci],
       coef^.iMCU_row_num * JDIMENSION(compptr^.v_samp_factor),
       JDIMENSION (compptr^.v_samp_factor), TRUE);
    { Count non-dummy DCT block rows in this iMCU row. }
    if (coef^.iMCU_row_num < last_iMCU_row) then
      block_rows := compptr^.v_samp_factor
    else
    begin
      { NB: can't use last_row_height here, since may not be set! }
      block_rows := int (compptr^.height_in_blocks) mod compptr^.v_samp_factor;
      if (block_rows = 0) then
        block_rows := compptr^.v_samp_factor;
    end;
    blocks_across := compptr^.width_in_blocks;
    h_samp_factor := compptr^.h_samp_factor;
    { Count number of dummy blocks to be added at the right margin. }
    ndummy := int (blocks_across) mod h_samp_factor;
    if (ndummy > 0) then
      ndummy := h_samp_factor - ndummy;
    { Perform DCT for all non-dummy blocks in this iMCU row.  Each call
      on forward_DCT processes a complete horizontal row of DCT blocks. }

    for block_row := 0 to pred(block_rows) do
    begin
      thisblockrow := buffer^[block_row];
      cinfo^.fdct^.forward_DCT (cinfo, compptr,
	                        input_buf^[ci],
                                thisblockrow,
				JDIMENSION (block_row * DCTSIZE),
				JDIMENSION (0),
                                blocks_across);
      if (ndummy > 0) then
      begin
	{ Create dummy blocks at the right edge of the image. }
	Inc(JBLOCK_PTR(thisblockrow), blocks_across); { => first dummy block }
	jzero_far({FAR}pointer(thisblockrow), ndummy * SIZEOF(JBLOCK));
	{lastDC := thisblockrow^[-1][0];}
        { work around Range Checking }
        Dec(JBLOCK_PTR(thisblockrow));
        lastDC := thisblockrow^[0][0];
        Inc(JBLOCK_PTR(thisblockrow));

	for bi := 0 to pred(ndummy) do
        begin
	  thisblockrow^[bi][0] := lastDC;
	end;
      end;
    end;
    { If at end of image, create dummy block rows as needed.
      The tricky part here is that within each MCU, we want the DC values
      of the dummy blocks to match the last real block's DC value.
      This squeezes a few more bytes out of the resulting file... }

    if (coef^.iMCU_row_num = last_iMCU_row) then
    begin
      Inc(blocks_across, ndummy);       { include lower right corner }
      MCUs_across := blocks_across div JDIMENSION(h_samp_factor);
      for block_row := block_rows to pred(compptr^.v_samp_factor) do
      begin
	thisblockrow := buffer^[block_row];
	lastblockrow := buffer^[block_row-1];
	jzero_far({FAR} pointer(thisblockrow),
		  size_t(blocks_across * SIZEOF(JBLOCK)));
	for MCUindex := 0 to pred(MCUs_across) do
        begin
	  lastDC := lastblockrow^[h_samp_factor-1][0];
	  for bi := 0 to pred(h_samp_factor) do
          begin
	    thisblockrow^[bi][0] := lastDC;
	  end;
	  Inc(JBLOCK_PTR(thisblockrow), h_samp_factor); { advance to next MCU in row }
	  Inc(JBLOCK_PTR(lastblockrow), h_samp_factor);
	end;
      end;
    end;
    Inc(compptr);
  end;
  { NB: compress_output will increment iMCU_row_num if successful.
    A suspension return will result in redoing all the work above next time.}


  { Emit data to the entropy encoder, sharing code with subsequent passes }
  compress_first_pass := compress_output(cinfo, input_buf);
end;


{ Process some data in subsequent passes of a multi-pass case.
  We process the equivalent of one fully interleaved MCU row ("iMCU" row)
  per call, ie, v_samp_factor block rows for each component in the scan.
  The data is obtained from the virtual arrays and fed to the entropy coder.
  Returns TRUE if the iMCU row is completed, FALSE if suspended.

  NB: input_buf is ignored; it is likely to be a NIL pointer. }

{METHODDEF}
function compress_output (cinfo : j_compress_ptr;
                          input_buf : JSAMPIMAGE) : boolean;
var
  coef : jccoefct_my_coef_ptr;
  MCU_col_num : JDIMENSION;	{ index of current MCU within row }
  blkn, ci, xindex, yindex, yoffset : int;
  start_col : JDIMENSION;
  buffer : array[0..MAX_COMPS_IN_SCAN-1] of JBLOCKARRAY;
  buffer_ptr : JBLOCKROW;
  compptr : jpeg_component_info_ptr;
begin
  coef := jccoefct_my_coef_ptr (cinfo^.coef);

  { Align the virtual buffers for the components used in this scan.
    NB: during first pass, this is safe only because the buffers will
    already be aligned properly, so jmemmgr.c won't need to do any I/O. }

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    buffer[ci] := cinfo^.mem^.access_virt_barray (
       j_common_ptr(cinfo), coef^.whole_image[compptr^.component_index],
       coef^.iMCU_row_num * JDIMENSION(compptr^.v_samp_factor),
       JDIMENSION (compptr^.v_samp_factor), FALSE);
  end;

  { Loop to process one whole iMCU row }
  for yoffset := coef^.MCU_vert_offset to pred(coef^.MCU_rows_per_iMCU_row) do
  begin
    for MCU_col_num := coef^.mcu_ctr to pred(cinfo^.MCUs_per_row) do
    begin
      { Construct list of pointers to DCT blocks belonging to this MCU }
      blkn := 0;			{ index of current DCT block within MCU }
      for ci := 0 to pred(cinfo^.comps_in_scan) do
      begin
	compptr := cinfo^.cur_comp_info[ci];
	start_col := MCU_col_num * JDIMENSION(compptr^.MCU_width);
	for yindex := 0 to pred(compptr^.MCU_height) do
        begin
	  buffer_ptr := JBLOCKROW(@ buffer[ci]^[yindex+yoffset]^[start_col]);
	  for xindex := 0 to pred(compptr^.MCU_width) do
          begin
	    coef^.MCU_buffer[blkn] := buffer_ptr;
	    Inc(blkn);
            Inc(JBLOCK_PTR(buffer_ptr));
	  end;
	end;
      end;
      { Try to write the MCU. }
      if (not cinfo^.entropy^.encode_mcu (cinfo, coef^.MCU_buffer)) then
      begin
	{ Suspension forced; update state counters and exit }
	coef^.MCU_vert_offset := yoffset;
	coef^.mcu_ctr := MCU_col_num;
	compress_output := FALSE;
        exit;
      end;
    end;
    { Completed an MCU row, but perhaps not an iMCU row }
    coef^.mcu_ctr := 0;
  end;
  { Completed the iMCU row, advance counters for next one }
  Inc(coef^.iMCU_row_num);
  jccoefct_start_iMCU_row(cinfo);
  compress_output := TRUE;
end;

{$endif} { FULL_COEF_BUFFER_SUPPORTED }


{ Initialize coefficient buffer controller. }

{GLOBAL}
procedure jinit_c_coef_controller (cinfo : j_compress_ptr;
                                   need_full_buffer : boolean);
var
  coef : jccoefct_my_coef_ptr;
var
  buffer : JBLOCKROW;
  i : int;
var
  ci : int;
  compptr : jpeg_component_info_ptr;
begin
  coef := jccoefct_my_coef_ptr (
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(jccoefct_my_coef_controller)) );
  cinfo^.coef := jpeg_c_coef_controller_ptr(coef);
  coef^.pub.start_pass := start_pass_coef;

  { Create the coefficient buffer. }
  if (need_full_buffer) then
  begin
{$ifdef FULL_COEF_BUFFER_SUPPORTED}
    { Allocate a full-image virtual array for each component, }
    { padded to a multiple of samp_factor DCT blocks in each direction. }

    compptr := jpeg_component_info_ptr(cinfo^.comp_info);
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      coef^.whole_image[ci] := cinfo^.mem^.request_virt_barray
	(j_common_ptr(cinfo), JPOOL_IMAGE, FALSE,
	 JDIMENSION (jround_up( long (compptr^.width_in_blocks),
				long (compptr^.h_samp_factor) )),
	 JDIMENSION (jround_up(long (compptr^.height_in_blocks),
				long (compptr^.v_samp_factor))),
	 JDIMENSION (compptr^.v_samp_factor));
      Inc(compptr);
    end;
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
{$endif}
  end
  else
  begin
    { We only need a single-MCU buffer. }
    buffer := JBLOCKROW (
      cinfo^.mem^.alloc_large (j_common_ptr(cinfo), JPOOL_IMAGE,
				  C_MAX_BLOCKS_IN_MCU * SIZEOF(JBLOCK)) );
    for i := 0 to pred(C_MAX_BLOCKS_IN_MCU) do
    begin
      coef^.MCU_buffer[i] := JBLOCKROW(@ buffer^[i]);
    end;
    coef^.whole_image[0] := NIL; { flag for no virtual arrays }
  end;
end;

{== imjcmainct implementation ==}

{ Private buffer controller object }

type
  jcmainct_my_main_ptr = ^jcmainct_my_main_controller;
  jcmainct_my_main_controller = record
    pub : jpeg_c_main_controller; { public fields }

    cur_iMCU_row : JDIMENSION;	{ number of current iMCU row }
    rowgroup_ctr : JDIMENSION;	{ counts row groups received in iMCU row }
    suspended : boolean;		{ remember if we suspended output }
    pass_mode : J_BUF_MODE;		{ current operating mode }

    { If using just a strip buffer, this points to the entire set of buffers
      (we allocate one for each component).  In the full-image case, this
      points to the currently accessible strips of the virtual arrays. }

    buffer : array[0..MAX_COMPONENTS-1] of JSAMPARRAY;

  {$ifdef FULL_MAIN_BUFFER_SUPPORTED}
    { If using full-image storage, this array holds pointers to virtual-array
      control blocks for each component.  Unused if not full-image storage. }

    whole_image : array[0..MAX_COMPONENTS-1] of jvirt_sarray_ptr;
  {$endif}
  end; {my_main_controller}


{ Forward declarations }
{METHODDEF}
procedure jcmainct_process_data_simple_main(cinfo : j_compress_ptr;
                                   input_buf : JSAMPARRAY;
                                   var in_row_ctr: JDIMENSION;
                                   in_rows_avail : JDIMENSION); forward;

{$ifdef FULL_MAIN_BUFFER_SUPPORTED}
{METHODDEF}
procedure process_data_buffer_main(cinfo : j_compress_ptr;
                                   input_buf : JSAMPARRAY;
                                   var in_row_ctr : JDIMENSION;
                                   in_rows_avail : JDIMENSION); forward;
{$endif}


{ Initialize for a processing pass. }

{METHODDEF}
procedure jcmainct_start_pass_main (cinfo : j_compress_ptr;
                           pass_mode : J_BUF_MODE);
var
  main : jcmainct_my_main_ptr;
begin
  main := jcmainct_my_main_ptr (cinfo^.main);

  { Do nothing in raw-data mode. }
  if (cinfo^.raw_data_in) then
    exit;

  main^.cur_iMCU_row := 0;	{ initialize counters }
  main^.rowgroup_ctr := 0;
  main^.suspended := FALSE;
  main^.pass_mode := pass_mode;	{ save mode for use by process_data }

  case (pass_mode) of
  JBUF_PASS_THRU:
    begin
{$ifdef FULL_MAIN_BUFFER_SUPPORTED}
      if (main^.whole_image[0] <> NIL) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
{$endif}
      main^.pub.process_data := jcmainct_process_data_simple_main;
    end;
{$ifdef FULL_MAIN_BUFFER_SUPPORTED}
  JBUF_SAVE_SOURCE,
  JBUF_CRANK_DEST,
  JBUF_SAVE_AND_PASS:
    begin
      if (main^.whole_image[0] = NIL) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
      main^.pub.process_data := process_data_buffer_main;
    end;
{$endif}
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
  end;
end;


{ Process some data.
  This routine handles the simple pass-through mode,
  where we have only a strip buffer. }

{METHODDEF}
procedure jcmainct_process_data_simple_main (cinfo : j_compress_ptr;
			            input_buf : JSAMPARRAY;
                                    var in_row_ctr : JDIMENSION;
                                    in_rows_avail : JDIMENSION);
var
  main : jcmainct_my_main_ptr;
begin
  main := jcmainct_my_main_ptr (cinfo^.main);

  while (main^.cur_iMCU_row < cinfo^.total_iMCU_rows) do
  begin
    { Read input data if we haven't filled the main buffer yet }
    if (main^.rowgroup_ctr < DCTSIZE) then
      cinfo^.prep^.pre_process_data (cinfo,
                                     input_buf,
                                     in_row_ctr,
                                     in_rows_avail,
				     JSAMPIMAGE(@main^.buffer),
                                     main^.rowgroup_ctr,
				     JDIMENSION(DCTSIZE));

    { If we don't have a full iMCU row buffered, return to application for
      more data.  Note that preprocessor will always pad to fill the iMCU row
      at the bottom of the image. }
    if (main^.rowgroup_ctr <> DCTSIZE) then
      exit;

    { Send the completed row to the compressor }
    if (not cinfo^.coef^.compress_data (cinfo, JSAMPIMAGE(@main^.buffer))) then
    begin
      { If compressor did not consume the whole row, then we must need to
        suspend processing and return to the application.  In this situation
        we pretend we didn't yet consume the last input row; otherwise, if
        it happened to be the last row of the image, the application would
        think we were done. }

      if (not main^.suspended) then
      begin
	Dec(in_row_ctr);
	main^.suspended := TRUE;
      end;
      exit;
    end;
    { We did finish the row.  Undo our little suspension hack if a previous
      call suspended; then mark the main buffer empty. }

    if (main^.suspended) then
    begin
      Inc(in_row_ctr);
      main^.suspended := FALSE;
    end;
    main^.rowgroup_ctr := 0;
    Inc(main^.cur_iMCU_row);
  end;
end;


{$ifdef FULL_MAIN_BUFFER_SUPPORTED}

{ Process some data.
  This routine handles all of the modes that use a full-size buffer. }

{METHODDEF}
procedure process_data_buffer_main (cinfo : j_compress_ptr;
			            input_buf : JSAMPARRAY;
                                    var in_row_ctr : JDIMENSION;
			            in_rows_avail : JDIMENSION);
var
  main : jcmainct_my_main_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
  writing : boolean;
begin
  main := jcmainct_my_main_ptr (cinfo^.main);
  writing := (main^.pass_mode <> JBUF_CRANK_DEST);

  while (main^.cur_iMCU_row < cinfo^.total_iMCU_rows) do
  begin
    { Realign the virtual buffers if at the start of an iMCU row. }
    if (main^.rowgroup_ctr = 0) then
    begin
      compptr := cinfo^.comp_info;
      for ci := 0 to pred(cinfo^.num_components) do
      begin
	main^.buffer[ci] := cinfo^.mem^.access_virt_sarray
	  (j_common_ptr (cinfo), main^.whole_image[ci],
	   main^.cur_iMCU_row * (compptr^.v_samp_factor * DCTSIZE),
	   JDIMENSION (compptr^.v_samp_factor * DCTSIZE), writing);
        Inc(compptr);
      end;
      { In a read pass, pretend we just read some source data. }
      if (not writing) then
      begin
	Inc(in_row_ctr, cinfo^.max_v_samp_factor * DCTSIZE);
	main^.rowgroup_ctr := DCTSIZE;
      end;
    end;

    { If a write pass, read input data until the current iMCU row is full. }
    { Note: preprocessor will pad if necessary to fill the last iMCU row. }
    if (writing) then
    begin
      cinfo^.prep^.pre_process_data (cinfo,
                                     input_buf, in_row_ctr, in_rows_avail,
				     JSAMPIMAGE(@main^.buffer),
                                     main^.rowgroup_ctr,
				     JDIMENSION (DCTSIZE));

      { Return to application if we need more data to fill the iMCU row. }
      if (main^.rowgroup_ctr < DCTSIZE) then
	exit;
    end;

    { Emit data, unless this is a sink-only pass. }
    if (main^.pass_mode <> JBUF_SAVE_SOURCE) then
    begin
      if (not cinfo^.coef^.compress_data (cinfo,
                                          JSAMPIMAGE(@main^.buffer))) then
      begin
	{ If compressor did not consume the whole row, then we must need to
	  suspend processing and return to the application.  In this situation
	  we pretend we didn't yet consume the last input row; otherwise, if
	  it happened to be the last row of the image, the application would
	  think we were done. }

	if (not main^.suspended) then
        begin
	  Dec(in_row_ctr);
	  main^.suspended := TRUE;
	end;
	exit;
      end;
      { We did finish the row.  Undo our little suspension hack if a previous
        call suspended; then mark the main buffer empty. }

      if (main^.suspended) then
      begin
	Inc(in_row_ctr);
	main^.suspended := FALSE;
      end;
    end;

    { If get here, we are done with this iMCU row.  Mark buffer empty. }
    main^.rowgroup_ctr := 0;
    Inc(main^.cur_iMCU_row);
  end;
end;

{$endif} { FULL_MAIN_BUFFER_SUPPORTED }


{ Initialize main buffer controller. }

{GLOBAL}
procedure jinit_c_main_controller (cinfo : j_compress_ptr;
                                   need_full_buffer : boolean);
var
  main : jcmainct_my_main_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
begin
  main := jcmainct_my_main_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(jcmainct_my_main_controller)) );
  cinfo^.main := jpeg_c_main_controller_ptr(main);
  main^.pub.start_pass := jcmainct_start_pass_main;

  { We don't need to create a buffer in raw-data mode. }
  if (cinfo^.raw_data_in) then
    exit;

  { Create the buffer.  It holds downsampled data, so each component
    may be of a different size. }

  if (need_full_buffer) then
  begin
{$ifdef FULL_MAIN_BUFFER_SUPPORTED}
    { Allocate a full-image virtual array for each component }
    { Note we pad the bottom to a multiple of the iMCU height }
    compptr := cinfo^.comp_info;
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      main^.whole_image[ci] := cinfo^.mem^.request_virt_sarray
	(j_common_ptr(cinfo), JPOOL_IMAGE, FALSE,
	 compptr^.width_in_blocks * DCTSIZE,
	 JDIMENSION (jround_up( long (compptr^.height_in_blocks),
				long (compptr^.v_samp_factor)) * DCTSIZE),
	 JDIMENSION (compptr^.v_samp_factor * DCTSIZE));
      Inc(compptr);
    end;
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
{$endif}
  end
  else
  begin
{$ifdef FULL_MAIN_BUFFER_SUPPORTED}
    main^.whole_image[0] := NIL; { flag for no virtual arrays }
{$endif}
    { Allocate a strip buffer for each component }
    compptr := jpeg_component_info_ptr(cinfo^.comp_info);
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      main^.buffer[ci] := cinfo^.mem^.alloc_sarray
	(j_common_ptr(cinfo), JPOOL_IMAGE,
	 compptr^.width_in_blocks * DCTSIZE,
	 JDIMENSION (compptr^.v_samp_factor * DCTSIZE));
      Inc(compptr);
    end;
  end;
end;

{== imjcmarker implementation ==}

{ Basic output routines.

  Note that we do not support suspension while writing a marker.
  Therefore, an application using suspension must ensure that there is
  enough buffer space for the initial markers (typ. 600-700 bytes) before
  calling jpeg_start_compress, and enough space to write the trailing EOI
  (a few bytes) before calling jpeg_finish_compress.  Multipass compression
  modes are not supported at all with suspension, so those two are the only
  points where markers will be written. }


{LOCAL}
procedure emit_byte (cinfo : j_compress_ptr; val : int);
{ Emit a byte }
var
  dest : jpeg_destination_mgr_ptr;
begin
  dest := cinfo^.dest;

  dest^.next_output_byte^ := JOCTET(val);
  Inc(dest^.next_output_byte);

  Dec(dest^.free_in_buffer);
  if (dest^.free_in_buffer = 0) then
  begin
    if not dest^.empty_output_buffer(cinfo) then
      ERREXIT(j_common_ptr(cinfo), JERR_CANT_SUSPEND);
  end;
end;


{LOCAL}
procedure emit_marker(cinfo : j_compress_ptr; mark : jcmarker_JPEG_MARKER);
{ Emit a marker code }
begin
  emit_byte(cinfo, $FF);
  emit_byte(cinfo, int(mark));
end;


{LOCAL}
procedure emit_2bytes (cinfo : j_compress_ptr; value : int);
{ Emit a 2-byte integer; these are always MSB first in JPEG files }
begin
  emit_byte(cinfo, (value shr 8) and $FF);
  emit_byte(cinfo, value and $FF);
end;


{ Routines to write specific marker types. }

{LOCAL}
function emit_dqt (cinfo : j_compress_ptr; index : int) : int;
{ Emit a DQT marker }
{ Returns the precision used (0 = 8bits, 1 = 16bits) for baseline checking }
var
  qtbl : JQUANT_TBL_PTR;
  prec : int;
  i : int;
var
  qval : uint;
begin
  qtbl := cinfo^.quant_tbl_ptrs[index];
  if (qtbl = NIL) then
    ERREXIT1(j_common_ptr(cinfo), JERR_NO_QUANT_TABLE, index);

  prec := 0;
  for i := 0 to Pred(DCTSIZE2) do
  begin
    if (qtbl^.quantval[i] > 255) then
      prec := 1;
  end;

  if not qtbl^.sent_table then
  begin
    emit_marker(cinfo, M_DQT);

    if (prec <> 0) then
      emit_2bytes(cinfo, DCTSIZE2*2 + 1 + 2)
    else
      emit_2bytes(cinfo, DCTSIZE2 + 1 + 2);

    emit_byte(cinfo, index + (prec shl 4));

    for i := 0 to Pred(DCTSIZE2) do
    begin
      { The table entries must be emitted in zigzag order. }
      qval := qtbl^.quantval[jpeg_natural_order[i]];
      if (prec <> 0) then
	emit_byte(cinfo, int(qval shr 8));
      emit_byte(cinfo, int(qval and $FF));
    end;

    qtbl^.sent_table := TRUE;
  end;

  emit_dqt := prec;
end;


{LOCAL}
procedure emit_dht (cinfo : j_compress_ptr; index : int; is_ac : boolean);
{ Emit a DHT marker }
var
  htbl : JHUFF_TBL_PTR;
  length, i : int;
begin
  if (is_ac) then
  begin
    htbl := cinfo^.ac_huff_tbl_ptrs[index];
    index := index + $10;                { output index has AC bit set }
  end
  else
  begin
    htbl := cinfo^.dc_huff_tbl_ptrs[index];
  end;

  if (htbl = NIL) then
    ERREXIT1(j_common_ptr(cinfo), JERR_NO_HUFF_TABLE, index);

  if not htbl^.sent_table then
  begin
    emit_marker(cinfo, M_DHT);

    length := 0;
    for i := 1 to 16 do
      length := length + htbl^.bits[i];

    emit_2bytes(cinfo, length + 2 + 1 + 16);
    emit_byte(cinfo, index);

    for i := 1 to 16 do
      emit_byte(cinfo, htbl^.bits[i]);

    for i := 0 to Pred(length) do
      emit_byte(cinfo, htbl^.huffval[i]);

    htbl^.sent_table := TRUE;
  end;
end;


{LOCAL}
procedure emit_dac (cinfo : j_compress_ptr);
{ Emit a DAC marker }
{ Since the useful info is so small, we want to emit all the tables in }
{ one DAC marker.  Therefore this routine does its own scan of the table. }
{$ifdef C_ARITH_CODING_SUPPORTED}
var
  dc_in_use : array[0..NUM_ARITH_TBLS] of byte;
  ac_in_use : array[0..NUM_ARITH_TBLS] of byte;
  length, i : int;
  compptr : jpeg_component_info_ptr;
begin
  for i := 0 to pred(NUM_ARITH_TBLS) do
  begin
    dc_in_use[i] := 0;
    ac_in_use[i] := 0;
  end;

  for i := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[i];
    dc_in_use[compptr^.dc_tbl_no] := 1;
    ac_in_use[compptr^.ac_tbl_no] := 1;
  end;

  length := 0;
  for i := 0 to pred(NUM_ARITH_TBLS) do
    Inc(length, dc_in_use[i] + ac_in_use[i]);

  emit_marker(cinfo, M_DAC);

  emit_2bytes(cinfo, length*2 + 2);

  for i := 0 to pred(NUM_ARITH_TBLS) do
  begin
    if (dc_in_use[i] <> 0) then
    begin
      emit_byte(cinfo, i);
      emit_byte(cinfo, cinfo^.arith_dc_L[i] + (cinfo^.arith_dc_U[i] shl 4));
    end;
    if (ac_in_use[i] <> 0) then
    begin
      emit_byte(cinfo, i + $10);
      emit_byte(cinfo, cinfo^.arith_ac_K[i]);
    end;
  end;
end;
{$else}
begin
end;
{$endif}  {C_ARITH_CODING_SUPPORTED}


{LOCAL}
procedure emit_dri (cinfo : j_compress_ptr);
{ Emit a DRI marker }
begin
  emit_marker(cinfo, M_DRI);

  emit_2bytes(cinfo, 4);	{ fixed length }

  emit_2bytes(cinfo, int(cinfo^.restart_interval));
end;


{LOCAL}
procedure emit_sof (cinfo : j_compress_ptr; code : jcmarker_JPEG_MARKER);
{ Emit a SOF marker }
var
  ci : int;
  compptr : jpeg_component_info_ptr;
begin
  emit_marker(cinfo, code);

  emit_2bytes(cinfo, 3 * cinfo^.num_components + 2 + 5 + 1); { length }

  { Make sure image isn't bigger than SOF field can handle }
  if (long(cinfo^.image_height) > long(65535)) or
     (long(cinfo^.image_width) > long(65535)) then
    ERREXIT1(j_common_ptr(cinfo), JERR_IMAGE_TOO_BIG, uInt(65535));

  emit_byte(cinfo, cinfo^.data_precision);
  emit_2bytes(cinfo, int(cinfo^.image_height));
  emit_2bytes(cinfo, int(cinfo^.image_width));

  emit_byte(cinfo, cinfo^.num_components);

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0  to Pred(cinfo^.num_components) do
  begin
    emit_byte(cinfo, compptr^.component_id);
    emit_byte(cinfo, (compptr^.h_samp_factor shl 4) + compptr^.v_samp_factor);
    emit_byte(cinfo, compptr^.quant_tbl_no);
    Inc(compptr);
  end;
end;


{LOCAL}
procedure emit_sos (cinfo : j_compress_ptr);
{ Emit a SOS marker }
var
  i, td, ta : int;
  compptr : jpeg_component_info_ptr;
begin
  emit_marker(cinfo, M_SOS);

  emit_2bytes(cinfo, 2 * cinfo^.comps_in_scan + 2 + 1 + 3); { length }

  emit_byte(cinfo, cinfo^.comps_in_scan);

  for i := 0 to Pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[i];
    emit_byte(cinfo, compptr^.component_id);
    td := compptr^.dc_tbl_no;
    ta := compptr^.ac_tbl_no;
    if (cinfo^.progressive_mode) then
    begin
      { Progressive mode: only DC or only AC tables are used in one scan;
        furthermore, Huffman coding of DC refinement uses no table at all.
        We emit 0 for unused field(s); this is recommended by the P&M text
        but does not seem to be specified in the standard. }

      if (cinfo^.Ss = 0) then
      begin
	ta := 0;                { DC scan }
	if (cinfo^.Ah <> 0) and not cinfo^.arith_code then
	  td := 0;              { no DC table either }
      end
      else
      begin
	td := 0;			{ AC scan }
      end;
    end;
    emit_byte(cinfo, (td shl 4) + ta);
  end;

  emit_byte(cinfo, cinfo^.Ss);
  emit_byte(cinfo, cinfo^.Se);
  emit_byte(cinfo, (cinfo^.Ah shl 4) + cinfo^.Al);
end;


{LOCAL}
procedure emit_jfif_app0 (cinfo : j_compress_ptr);
{ Emit a JFIF-compliant APP0 marker }
{
 Length of APP0 block	(2 bytes)
 Block ID			(4 bytes - ASCII "JFIF")
 Zero byte			(1 byte to terminate the ID string)
 Version Major, Minor   (2 bytes - major first)
 Units			(1 byte - $00 = none, $01 = inch, $02 = cm)
 Xdpu			(2 bytes - dots per unit horizontal)
 Ydpu			(2 bytes - dots per unit vertical)
 Thumbnail X size		(1 byte)
 Thumbnail Y size		(1 byte)
}
begin
  emit_marker(cinfo, M_APP0);

  emit_2bytes(cinfo, 2 + 4 + 1 + 2 + 1 + 2 + 2 + 1 + 1); { length }

  emit_byte(cinfo, $4A);	{ Identifier: ASCII "JFIF" }
  emit_byte(cinfo, $46);
  emit_byte(cinfo, $49);
  emit_byte(cinfo, $46);
  emit_byte(cinfo, 0);
  emit_byte(cinfo, cinfo^.JFIF_major_version); { Version fields }
  emit_byte(cinfo, cinfo^.JFIF_minor_version);
  emit_byte(cinfo, cinfo^.density_unit); { Pixel size information }
  emit_2bytes(cinfo, int(cinfo^.X_density));
  emit_2bytes(cinfo, int(cinfo^.Y_density));
  emit_byte(cinfo, 0);		{ No thumbnail image }
  emit_byte(cinfo, 0);
end;


{LOCAL}
procedure emit_adobe_app14 (cinfo : j_compress_ptr);
{ Emit an Adobe APP14 marker }
{
  Length of APP14 block	(2 bytes)
  Block ID			(5 bytes - ASCII "Adobe")
  Version Number		(2 bytes - currently 100)
  Flags0			(2 bytes - currently 0)
  Flags1			(2 bytes - currently 0)
  Color transform		(1 byte)

  Although Adobe TN 5116 mentions Version = 101, all the Adobe files
  now in circulation seem to use Version = 100, so that's what we write.

  We write the color transform byte as 1 if the JPEG color space is
  YCbCr, 2 if it's YCCK, 0 otherwise.  Adobe's definition has to do with
  whether the encoder performed a transformation, which is pretty useless.
}
begin
  emit_marker(cinfo, M_APP14);

  emit_2bytes(cinfo, 2 + 5 + 2 + 2 + 2 + 1); { length }

  emit_byte(cinfo, $41);	{ Identifier: ASCII "Adobe" }
  emit_byte(cinfo, $64);
  emit_byte(cinfo, $6F);
  emit_byte(cinfo, $62);
  emit_byte(cinfo, $65);
  emit_2bytes(cinfo, 100);	{ Version }
  emit_2bytes(cinfo, 0);	{ Flags0 }
  emit_2bytes(cinfo, 0);	{ Flags1 }
  case (cinfo^.jpeg_color_space) of
  JCS_YCbCr:
    emit_byte(cinfo, 1);	{ Color transform = 1 }
  JCS_YCCK:
    emit_byte(cinfo, 2);	{ Color transform = 2 }
  else
    emit_byte(cinfo, 0);	{ Color transform = 0 }
  end;
end;


{ These routines allow writing an arbitrary marker with parameters.
  The only intended use is to emit COM or APPn markers after calling
  write_file_header and before calling write_frame_header.
  Other uses are not guaranteed to produce desirable results.
  Counting the parameter bytes properly is the caller's responsibility. }

{METHODDEF}
procedure write_marker_header (cinfo : j_compress_ptr;
                               marker : int;
                               datalen : uint);
{ Emit an arbitrary marker header }
begin
  if (datalen > uint(65533)) then  { safety check }
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

  emit_marker(cinfo, jcmarker_JPEG_MARKER(marker));

  emit_2bytes(cinfo, int(datalen + 2));	{ total length }
end;

{METHODDEF}
procedure write_marker_byte (cinfo : j_compress_ptr; val : int);
{ Emit one byte of marker parameters following write_marker_header }
begin
  emit_byte(cinfo, val);
end;

{ Write datastream header.
  This consists of an SOI and optional APPn markers.
  We recommend use of the JFIF marker, but not the Adobe marker,
  when using YCbCr or grayscale data.  The JFIF marker should NOT
  be used for any other JPEG colorspace.  The Adobe marker is helpful
  to distinguish RGB, CMYK, and YCCK colorspaces.
  Note that an application can write additional header markers after
  jpeg_start_compress returns. }


{METHODDEF}
procedure write_file_header (cinfo : j_compress_ptr);
var
  marker : jcmarker_my_marker_ptr;
begin
  marker := jcmarker_my_marker_ptr(cinfo^.marker);

  emit_marker(cinfo, M_SOI);	 { first the SOI }

  { SOI is defined to reset restart interval to 0 }
  marker^.last_restart_interval := 0;

  if (cinfo^.write_JFIF_header)	then { next an optional JFIF APP0 }
    emit_jfif_app0(cinfo);
  if (cinfo^.write_Adobe_marker) then { next an optional Adobe APP14 }
    emit_adobe_app14(cinfo);
end;


{ Write frame header.
  This consists of DQT and SOFn markers.
  Note that we do not emit the SOF until we have emitted the DQT(s).
  This avoids compatibility problems with incorrect implementations that
  try to error-check the quant table numbers as soon as they see the SOF. }


{METHODDEF}
procedure write_frame_header (cinfo : j_compress_ptr);
var
  ci, prec : int;
  is_baseline : boolean;
  compptr : jpeg_component_info_ptr;
begin
  { Emit DQT for each quantization table.
    Note that emit_dqt() suppresses any duplicate tables. }

  prec := 0;
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to Pred(cinfo^.num_components) do
  begin
    prec := prec + emit_dqt(cinfo, compptr^.quant_tbl_no);
    Inc(compptr);
  end;
  { now prec is nonzero iff there are any 16-bit quant tables. }

  { Check for a non-baseline specification.
    Note we assume that Huffman table numbers won't be changed later. }

  if (cinfo^.arith_code) or (cinfo^.progressive_mode)
   or (cinfo^.data_precision <> 8) then
  begin
    is_baseline := FALSE;
  end
  else
  begin
    is_baseline := TRUE;
    compptr := jpeg_component_info_ptr(cinfo^.comp_info);
    for ci := 0 to Pred(cinfo^.num_components) do
    begin
      if (compptr^.dc_tbl_no > 1) or (compptr^.ac_tbl_no > 1) then
	is_baseline := FALSE;
      Inc(compptr);
    end;
    if (prec <> 0) and (is_baseline) then
    begin
      is_baseline := FALSE;
      { If it's baseline except for quantizer size, warn the user }
      {$IFDEF DEBUG}
      TRACEMS(j_common_ptr(cinfo), 0, JTRC_16BIT_TABLES);
      {$ENDIF}
    end;
  end;

  { Emit the proper SOF marker }
  if (cinfo^.arith_code) then
  begin
    emit_sof(cinfo, M_SOF9);	{ SOF code for arithmetic coding }
  end
  else
  begin
    if (cinfo^.progressive_mode) then
      emit_sof(cinfo, M_SOF2)	{ SOF code for progressive Huffman }
    else if (is_baseline) then
      emit_sof(cinfo, M_SOF0)	{ SOF code for baseline implementation }
    else
      emit_sof(cinfo, M_SOF1);	{ SOF code for non-baseline Huffman file }
  end;
end;


{ Write scan header.
  This consists of DHT or DAC markers, optional DRI, and SOS.
  Compressed data will be written following the SOS. }

{METHODDEF}
procedure write_scan_header (cinfo : j_compress_ptr);
var
  marker : jcmarker_my_marker_ptr;
  i : int;
  compptr : jpeg_component_info_ptr;
begin
  marker := jcmarker_my_marker_ptr(cinfo^.marker);
  if (cinfo^.arith_code) then
  begin
    { Emit arith conditioning info.  We may have some duplication
      if the file has multiple scans, but it's so small it's hardly
      worth worrying about. }
    emit_dac(cinfo);
  end
  else
  begin
    { Emit Huffman tables.
      Note that emit_dht() suppresses any duplicate tables. }
    for i := 0 to Pred(cinfo^.comps_in_scan) do
    begin
      compptr := cinfo^.cur_comp_info[i];
      if (cinfo^.progressive_mode) then
      begin
	{ Progressive mode: only DC or only AC tables are used in one scan }
	if (cinfo^.Ss = 0) then
        begin
	  if (cinfo^.Ah = 0) then  { DC needs no table for refinement scan }
	    emit_dht(cinfo, compptr^.dc_tbl_no, FALSE);
	end
        else
        begin
	  emit_dht(cinfo, compptr^.ac_tbl_no, TRUE);
	end;
      end
      else
      begin
	{ Sequential mode: need both DC and AC tables }
	emit_dht(cinfo, compptr^.dc_tbl_no, FALSE);
	emit_dht(cinfo, compptr^.ac_tbl_no, TRUE);
      end;
    end;
  end;

  { Emit DRI if required --- note that DRI value could change for each scan.
    We avoid wasting space with unnecessary DRIs, however. }

  if (cinfo^.restart_interval <> marker^.last_restart_interval) then
  begin
    emit_dri(cinfo);
    marker^.last_restart_interval := cinfo^.restart_interval;
  end;

  emit_sos(cinfo);
end;


{ Write datastream trailer. }


{METHODDEF}
procedure write_file_trailer (cinfo : j_compress_ptr);
begin
  emit_marker(cinfo, M_EOI);
end;


{ Write an abbreviated table-specification datastream.
  This consists of SOI, DQT and DHT tables, and EOI.
  Any table that is defined and not marked sent_table = TRUE will be
  emitted.  Note that all tables will be marked sent_table = TRUE at exit. }


{METHODDEF}
procedure write_tables_only (cinfo : j_compress_ptr);
var
  i : int;
begin
  emit_marker(cinfo, M_SOI);

  for i := 0 to Pred(NUM_QUANT_TBLS) do
  begin
    if (cinfo^.quant_tbl_ptrs[i] <> NIL) then
      emit_dqt(cinfo, i);  { dummy := ... }
  end;

  if (not cinfo^.arith_code) then
  begin
    for i := 0 to Pred(NUM_HUFF_TBLS) do
    begin
      if (cinfo^.dc_huff_tbl_ptrs[i] <> NIL) then
	emit_dht(cinfo, i, FALSE);
      if (cinfo^.ac_huff_tbl_ptrs[i] <> NIL) then
	emit_dht(cinfo, i, TRUE);
    end;
  end;

  emit_marker(cinfo, M_EOI);
end;


{ Initialize the marker writer module. }

{GLOBAL}
procedure jinit_marker_writer (cinfo : j_compress_ptr);
var
  marker : jcmarker_my_marker_ptr;
begin
  { Create the subobject }
  marker := jcmarker_my_marker_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(my_marker_writer)) );
  cinfo^.marker := jpeg_marker_writer_ptr(marker);
  { Initialize method pointers }
  marker^.pub.write_file_header := write_file_header;
  marker^.pub.write_frame_header := write_frame_header;
  marker^.pub.write_scan_header := write_scan_header;
  marker^.pub.write_file_trailer := write_file_trailer;
  marker^.pub.write_tables_only := write_tables_only;
  marker^.pub.write_marker_header := write_marker_header;
  marker^.pub.write_marker_byte := write_marker_byte;
  { Initialize private state }
  marker^.last_restart_interval := 0;
end;

{== imjcmaster implementation ==}

{ Private state }

type
  c_pass_type = (
	main_pass,		{ input data, also do first output step }
	huff_opt_pass,		{ Huffman code optimization pass }
	output_pass		{ data output pass }
                );

type
  jcmaster_my_master_ptr = ^my_comp_master;
  my_comp_master = record
    pub : jpeg_comp_master;	{ public fields }

    pass_type : c_pass_type; 	{ the type of the current pass }

    pass_number : int;		{ # of passes completed }
    total_passes : int;		{ total # of passes needed }

    scan_number : int;		{ current index in scan_info[] }
  end;


{ Support routines that do various essential calculations. }

{LOCAL}
procedure jcmaster_initial_setup (cinfo : j_compress_ptr);
{ Do computations that are needed before master selection phase }
var
  ci : int;
  compptr : jpeg_component_info_ptr;
  samplesperrow : long;
  jd_samplesperrow : JDIMENSION;
begin

  { Sanity check on image dimensions }
  if (cinfo^.image_height <= 0) or (cinfo^.image_width <= 0) or
     (cinfo^.num_components <= 0) or (cinfo^.input_components <= 0) then
    ERREXIT(j_common_ptr(cinfo), JERR_EMPTY_IMAGE);

  { Make sure image isn't bigger than I can handle }
  if ( long(cinfo^.image_height) > long(JPEG_MAX_DIMENSION)) or
      ( long(cinfo^.image_width) > long(JPEG_MAX_DIMENSION)) then
    ERREXIT1(j_common_ptr(cinfo), JERR_IMAGE_TOO_BIG,
                                  uInt(JPEG_MAX_DIMENSION));

  { Width of an input scanline must be representable as JDIMENSION. }
  samplesperrow := long (cinfo^.image_width) * long (cinfo^.input_components);
  jd_samplesperrow := JDIMENSION (samplesperrow);
  if ( long(jd_samplesperrow) <> samplesperrow) then
    ERREXIT(j_common_ptr(cinfo), JERR_WIDTH_OVERFLOW);

  { For now, precision must match compiled-in value... }
  if (cinfo^.data_precision <> BITS_IN_JSAMPLE) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PRECISION, cinfo^.data_precision);

  { Check that number of components won't exceed internal array sizes }
  if (cinfo^.num_components > MAX_COMPONENTS) then
    ERREXIT2(j_common_ptr(cinfo), JERR_COMPONENT_COUNT, cinfo^.num_components,
	     MAX_COMPONENTS);

  { Compute maximum sampling factors; check factor validity }
  cinfo^.max_h_samp_factor := 1;
  cinfo^.max_v_samp_factor := 1;
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    if (compptr^.h_samp_factor<=0) or (compptr^.h_samp_factor>MAX_SAMP_FACTOR)
    or (compptr^.v_samp_factor<=0) or (compptr^.v_samp_factor>MAX_SAMP_FACTOR) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_SAMPLING);
    { MAX }
    if cinfo^.max_h_samp_factor > compptr^.h_samp_factor then
      cinfo^.max_h_samp_factor := cinfo^.max_h_samp_factor
    else
      cinfo^.max_h_samp_factor := compptr^.h_samp_factor;
    { MAX }
    if cinfo^.max_v_samp_factor > compptr^.v_samp_factor then
      cinfo^.max_v_samp_factor := cinfo^.max_v_samp_factor
    else
      cinfo^.max_v_samp_factor := compptr^.v_samp_factor;
    Inc(compptr);
  end;

  { Compute dimensions of components }
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Fill in the correct component_index value; don't rely on application }
    compptr^.component_index := ci;
    { For compression, we never do DCT scaling. }
    compptr^.DCT_scaled_size := DCTSIZE;
    { Size in DCT blocks }
    compptr^.width_in_blocks := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_width) * long (compptr^.h_samp_factor),
		    long (cinfo^.max_h_samp_factor * DCTSIZE)) );
    compptr^.height_in_blocks := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_height) * long (compptr^.v_samp_factor),
		    long (cinfo^.max_v_samp_factor * DCTSIZE)) );
    { Size in samples }
    compptr^.downsampled_width := JDIMENSION (
      jdiv_round_up(long(cinfo^.image_width) * long(compptr^.h_samp_factor),
		    long(cinfo^.max_h_samp_factor)) );
    compptr^.downsampled_height := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_height) * long(compptr^.v_samp_factor),
		    long (cinfo^.max_v_samp_factor)) );
    { Mark component needed (this flag isn't actually used for compression) }
    compptr^.component_needed := TRUE;
    Inc(compptr);
  end;

  { Compute number of fully interleaved MCU rows (number of times that
    main controller will call coefficient controller). }

  cinfo^.total_iMCU_rows := JDIMENSION (
    jdiv_round_up(long (cinfo^.image_height),
		  long (cinfo^.max_v_samp_factor*DCTSIZE)) );
end;


{$ifdef C_MULTISCAN_FILES_SUPPORTED}

{LOCAL}
procedure validate_script (cinfo : j_compress_ptr);
{ Verify that the scan script in cinfo^.scan_info[] is valid; also
  determine whether it uses progressive JPEG, and set cinfo^.progressive_mode. }
type
  IntRow = array[0..DCTSIZE2-1] of int;
  introw_ptr = ^IntRow;
var
  {const}scanptr : jpeg_scan_info_ptr;
  scanno, ncomps, ci, coefi, thisi : int;
  Ss, Se, Ah, Al : int;
  component_sent : array[0..MAX_COMPONENTS-1] of boolean;
{$ifdef C_PROGRESSIVE_SUPPORTED}
  last_bitpos_int_ptr : int_ptr;
  last_bitpos_ptr : introw_ptr;
  last_bitpos : array[0..MAX_COMPONENTS-1] of IntRow;
  { -1 until that coefficient has been seen; then last Al for it }
  { The JPEG spec simply gives the ranges 0..13 for Ah and Al, but that
    seems wrong: the upper bound ought to depend on data precision.
    Perhaps they really meant 0..N+1 for N-bit precision.
    Here we allow 0..10 for 8-bit data; Al larger than 10 results in
    out-of-range reconstructed DC values during the first DC scan,
    which might cause problems for some decoders. }
{$ifdef BITS_IN_JSAMPLE_IS_8}
const
  MAX_AH_AL = 10;
{$else}
const
  MAX_AH_AL = 13;
{$endif}
{$endif}
begin

  if (cinfo^.num_scans <= 0) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_SCAN_SCRIPT, 0);

  { For sequential JPEG, all scans must have Ss=0, Se=DCTSIZE2-1;
    for progressive JPEG, no scan can have this. }

  scanptr := cinfo^.scan_info;
  if (scanptr^.Ss <> 0) or (scanptr^.Se <> DCTSIZE2-1) then
  begin
{$ifdef C_PROGRESSIVE_SUPPORTED}
    cinfo^.progressive_mode := TRUE;
    last_bitpos_int_ptr := @(last_bitpos[0][0]);
    for ci := 0 to pred(cinfo^.num_components) do
      for coefi := 0 to pred(DCTSIZE2) do
      begin
	last_bitpos_int_ptr^ := -1;
        Inc(last_bitpos_int_ptr);
      end;
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
  end
  else
  begin
    cinfo^.progressive_mode := FALSE;
    for ci := 0 to pred(cinfo^.num_components) do
      component_sent[ci] := FALSE;
  end;

  for scanno := 1 to cinfo^.num_scans do
  begin
    { Validate component indexes }
    ncomps := scanptr^.comps_in_scan;
    if (ncomps <= 0) or (ncomps > MAX_COMPS_IN_SCAN) then
      ERREXIT2(j_common_ptr(cinfo), JERR_COMPONENT_COUNT, ncomps, MAX_COMPS_IN_SCAN);
    for ci := 0 to pred(ncomps) do
    begin
      thisi := scanptr^.component_index[ci];
      if (thisi < 0) or (thisi >= cinfo^.num_components) then
	ERREXIT1(j_common_ptr(cinfo), JERR_BAD_SCAN_SCRIPT, scanno);
      { Components must appear in SOF order within each scan }
      if (ci > 0) and (thisi <= scanptr^.component_index[ci-1]) then
	ERREXIT1(j_common_ptr(cinfo), JERR_BAD_SCAN_SCRIPT, scanno);
    end;
    { Validate progression parameters }
    Ss := scanptr^.Ss;
    Se := scanptr^.Se;
    Ah := scanptr^.Ah;
    Al := scanptr^.Al;
    if (cinfo^.progressive_mode) then
    begin
{$ifdef C_PROGRESSIVE_SUPPORTED}
      if (Ss < 0) or (Ss >= DCTSIZE2) or (Se < Ss) or (Se >= DCTSIZE2) or
	 (Ah < 0) or (Ah > MAX_AH_AL) or (Al < 0) or (Al > MAX_AH_AL) then
	ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);

      if (Ss < 0) or (Ss >= DCTSIZE2) or (Se < Ss) or (Se >= DCTSIZE2)
       or (Ah < 0) or (Ah > MAX_AH_AL) or (Al < 0) or (Al > MAX_AH_AL) then
	ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);
      if (Ss = 0) then
      begin
	if (Se <> 0) then	{ DC and AC together not OK }
	  ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);
      end
      else
      begin
	if (ncomps <> 1) then  { AC scans must be for only one component }
	  ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);
      end;
      for ci := 0 to pred(ncomps) do
      begin
	last_bitpos_ptr := @( last_bitpos[scanptr^.component_index[ci]]);
	if (Ss <> 0) and (last_bitpos_ptr^[0] < 0) then { AC without prior DC scan }
	  ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);
	for coefi := Ss to Se do
        begin
	  if (last_bitpos_ptr^[coefi] < 0) then
          begin
	    { first scan of this coefficient }
	    if (Ah <> 0) then
	      ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);
	  end
          else
          begin
	    { not first scan }
	    if (Ah <> last_bitpos_ptr^[coefi]) or (Al <> Ah-1) then
	      ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);
	  end;
	  last_bitpos_ptr^[coefi] := Al;
	end;
      end;
{$endif}
    end
    else
    begin
      { For sequential JPEG, all progression parameters must be these: }
      if (Ss <> 0) or (Se <> DCTSIZE2-1) or (Ah <> 0) or (Al <> 0) then
	ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);
      { Make sure components are not sent twice }
      for ci := 0 to pred(ncomps) do
      begin
	thisi := scanptr^.component_index[ci];
	if (component_sent[thisi]) then
	  ERREXIT1(j_common_ptr(cinfo), JERR_BAD_SCAN_SCRIPT, scanno);
	component_sent[thisi] := TRUE;
      end;
    end;
    Inc(scanptr);
  end;

  { Now verify that everything got sent. }
  if (cinfo^.progressive_mode) then
  begin
{$ifdef C_PROGRESSIVE_SUPPORTED}
    { For progressive mode, we only check that at least some DC data
      got sent for each component; the spec does not require that all bits
      of all coefficients be transmitted.  Would it be wiser to enforce
      transmission of all coefficient bits?? }

    for ci := 0 to pred(cinfo^.num_components) do
    begin
      if (last_bitpos[ci][0] < 0) then
	ERREXIT(j_common_ptr(cinfo), JERR_MISSING_DATA);
    end;
{$endif}
  end
  else
  begin
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      if (not component_sent[ci]) then
	ERREXIT(j_common_ptr(cinfo), JERR_MISSING_DATA);
    end;
  end;
end;

{$endif} { C_MULTISCAN_FILES_SUPPORTED }


{LOCAL}
procedure select_scan_parameters (cinfo : j_compress_ptr);
{ Set up the scan parameters for the current scan }
var
  master : jcmaster_my_master_ptr;
  {const} scanptr : jpeg_scan_info_ptr;
  ci : int;
var
  comp_infos : jpeg_component_info_list_ptr;
begin
{$ifdef C_MULTISCAN_FILES_SUPPORTED}
  if (cinfo^.scan_info <> NIL) then
  begin
    { Prepare for current scan --- the script is already validated }
    master := jcmaster_my_master_ptr (cinfo^.master);
    scanptr := cinfo^.scan_info;
    Inc(scanptr, master^.scan_number);

    cinfo^.comps_in_scan := scanptr^.comps_in_scan;
    comp_infos := cinfo^.comp_info;
    for ci := 0 to pred(scanptr^.comps_in_scan) do
    begin
      cinfo^.cur_comp_info[ci] :=
        @(comp_infos^[scanptr^.component_index[ci]]);
    end;
    cinfo^.Ss := scanptr^.Ss;
    cinfo^.Se := scanptr^.Se;
    cinfo^.Ah := scanptr^.Ah;
    cinfo^.Al := scanptr^.Al;
  end
  else
{$endif}
  begin
    { Prepare for single sequential-JPEG scan containing all components }
    if (cinfo^.num_components > MAX_COMPS_IN_SCAN) then
      ERREXIT2(j_common_ptr(cinfo), JERR_COMPONENT_COUNT, cinfo^.num_components,
	       MAX_COMPS_IN_SCAN);
    cinfo^.comps_in_scan := cinfo^.num_components;
    comp_infos := cinfo^.comp_info;
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      cinfo^.cur_comp_info[ci] := @(comp_infos^[ci]);
    end;
    cinfo^.Ss := 0;
    cinfo^.Se := DCTSIZE2-1;
    cinfo^.Ah := 0;
    cinfo^.Al := 0;
  end;
end;


{LOCAL}
procedure jcmaster_per_scan_setup (cinfo : j_compress_ptr);
{ Do computations that are needed before processing a JPEG scan }
{ cinfo^.comps_in_scan and cinfo^.cur_comp_info[] are already set }
var
  ci, mcublks, tmp : int;
  compptr : jpeg_component_info_ptr;
  nominal : long;
begin
  if (cinfo^.comps_in_scan = 1) then
  begin

    { Noninterleaved (single-component) scan }
    compptr := cinfo^.cur_comp_info[0];

    { Overall image size in MCUs }
    cinfo^.MCUs_per_row := compptr^.width_in_blocks;
    cinfo^.MCU_rows_in_scan := compptr^.height_in_blocks;

    { For noninterleaved scan, always one block per MCU }
    compptr^.MCU_width := 1;
    compptr^.MCU_height := 1;
    compptr^.MCU_blocks := 1;
    compptr^.MCU_sample_width := DCTSIZE;
    compptr^.last_col_width := 1;
    { For noninterleaved scans, it is convenient to define last_row_height
      as the number of block rows present in the last iMCU row. }

    tmp := int (compptr^.height_in_blocks) mod compptr^.v_samp_factor;
    if (tmp = 0) then
      tmp := compptr^.v_samp_factor;
    compptr^.last_row_height := tmp;

    { Prepare array describing MCU composition }
    cinfo^.blocks_in_MCU := 1;
    cinfo^.MCU_membership[0] := 0;

  end
  else
  begin

    { Interleaved (multi-component) scan }
    if (cinfo^.comps_in_scan <= 0) or
       (cinfo^.comps_in_scan > MAX_COMPS_IN_SCAN) then
      ERREXIT2(j_common_ptr(cinfo), JERR_COMPONENT_COUNT,
        cinfo^.comps_in_scan,  MAX_COMPS_IN_SCAN);

    { Overall image size in MCUs }
    cinfo^.MCUs_per_row := JDIMENSION (
      jdiv_round_up( long (cinfo^.image_width),
		     long (cinfo^.max_h_samp_factor*DCTSIZE)) );
    cinfo^.MCU_rows_in_scan := JDIMENSION (
      jdiv_round_up( long (cinfo^.image_height),
		     long (cinfo^.max_v_samp_factor*DCTSIZE)) );

    cinfo^.blocks_in_MCU := 0;

    for ci := 0 to pred(cinfo^.comps_in_scan) do
    begin
      compptr := cinfo^.cur_comp_info[ci];
      { Sampling factors give # of blocks of component in each MCU }
      compptr^.MCU_width := compptr^.h_samp_factor;
      compptr^.MCU_height := compptr^.v_samp_factor;
      compptr^.MCU_blocks := compptr^.MCU_width * compptr^.MCU_height;
      compptr^.MCU_sample_width := compptr^.MCU_width * DCTSIZE;
      { Figure number of non-dummy blocks in last MCU column & row }
      tmp := int (compptr^.width_in_blocks) mod compptr^.MCU_width;
      if (tmp = 0) then
        tmp := compptr^.MCU_width;
      compptr^.last_col_width := tmp;
      tmp := int (compptr^.height_in_blocks) mod compptr^.MCU_height;
      if (tmp = 0) then
        tmp := compptr^.MCU_height;
      compptr^.last_row_height := tmp;
      { Prepare array describing MCU composition }
      mcublks := compptr^.MCU_blocks;
      if (cinfo^.blocks_in_MCU + mcublks > C_MAX_BLOCKS_IN_MCU) then
	ERREXIT(j_common_ptr(cinfo), JERR_BAD_MCU_SIZE);
      while (mcublks > 0) do
      begin
        Dec(mcublks);
	cinfo^.MCU_membership[cinfo^.blocks_in_MCU] := ci;
        Inc(cinfo^.blocks_in_MCU);
      end;
    end;

  end;

  { Convert restart specified in rows to actual MCU count. }
  { Note that count must fit in 16 bits, so we provide limiting. }
  if (cinfo^.restart_in_rows > 0) then
  begin
    nominal := long(cinfo^.restart_in_rows) * long(cinfo^.MCUs_per_row);
    if nominal < long(65535) then
      cinfo^.restart_interval := uInt (nominal)
    else
      cinfo^.restart_interval := long(65535);
  end;
end;


{ Per-pass setup.
  This is called at the beginning of each pass.  We determine which modules
  will be active during this pass and give them appropriate start_pass calls.
  We also set is_last_pass to indicate whether any more passes will be
  required. }

{METHODDEF}
procedure prepare_for_pass (cinfo : j_compress_ptr);
var
  master : jcmaster_my_master_ptr;
var
  fallthrough : boolean;
begin
  master := jcmaster_my_master_ptr (cinfo^.master);
  fallthrough := true;

  case (master^.pass_type) of
  main_pass:
    begin
      { Initial pass: will collect input data, and do either Huffman
        optimization or data output for the first scan. }
      select_scan_parameters(cinfo);
      jcmaster_per_scan_setup(cinfo);
      if (not cinfo^.raw_data_in) then
      begin
        cinfo^.cconvert^.start_pass (cinfo);
        cinfo^.downsample^.start_pass (cinfo);
        cinfo^.prep^.start_pass (cinfo, JBUF_PASS_THRU);
      end;
      cinfo^.fdct^.start_pass (cinfo);
      cinfo^.entropy^.start_pass (cinfo, cinfo^.optimize_coding);
      if master^.total_passes > 1 then
        cinfo^.coef^.start_pass (cinfo, JBUF_SAVE_AND_PASS)
      else
        cinfo^.coef^.start_pass (cinfo, JBUF_PASS_THRU);
      cinfo^.main^.start_pass (cinfo, JBUF_PASS_THRU);
      if (cinfo^.optimize_coding) then
      begin
        { No immediate data output; postpone writing frame/scan headers }
        master^.pub.call_pass_startup := FALSE;
      end
      else
      begin
        { Will write frame/scan headers at first jpeg_write_scanlines call }
        master^.pub.call_pass_startup := TRUE;
      end;
    end;
{$ifdef ENTROPY_OPT_SUPPORTED}
  huff_opt_pass,
  output_pass:
    begin
      if (master^.pass_type = huff_opt_pass) then
      begin
        { Do Huffman optimization for a scan after the first one. }
        select_scan_parameters(cinfo);
        jcmaster_per_scan_setup(cinfo);
        if (cinfo^.Ss <> 0) or (cinfo^.Ah = 0) or (cinfo^.arith_code) then
        begin
          cinfo^.entropy^.start_pass (cinfo, TRUE);
          cinfo^.coef^.start_pass (cinfo, JBUF_CRANK_DEST);
          master^.pub.call_pass_startup := FALSE;
          fallthrough := false;
        end;
        { Special case: Huffman DC refinement scans need no Huffman table
          and therefore we can skip the optimization pass for them. }
        if fallthrough then
        begin
          master^.pass_type := output_pass;
          Inc(master^.pass_number);
          {FALLTHROUGH}
        end;
      end;
{$else}
  output_pass:
    begin
{$endif}
      if fallthrough then
      begin
        { Do a data-output pass. }
        { We need not repeat per-scan setup if prior optimization pass did it. }
        if (not cinfo^.optimize_coding) then
        begin
          select_scan_parameters(cinfo);
          jcmaster_per_scan_setup(cinfo);
        end;
        cinfo^.entropy^.start_pass (cinfo, FALSE);
        cinfo^.coef^.start_pass (cinfo, JBUF_CRANK_DEST);
        { We emit frame/scan headers now }
        if (master^.scan_number = 0) then
          cinfo^.marker^.write_frame_header (cinfo);
        cinfo^.marker^.write_scan_header (cinfo);
        master^.pub.call_pass_startup := FALSE;
      end;
    end;
  else
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
  end;

  master^.pub.is_last_pass := (master^.pass_number = master^.total_passes-1);

  { Set up progress monitor's pass info if present }
  if (cinfo^.progress <> NIL) then
  begin
    cinfo^.progress^.completed_passes := master^.pass_number;
    cinfo^.progress^.total_passes := master^.total_passes;
  end;
end;


{ Special start-of-pass hook.
  This is called by jpeg_write_scanlines if call_pass_startup is TRUE.
  In single-pass processing, we need this hook because we don't want to
  write frame/scan headers during jpeg_start_compress; we want to let the
  application write COM markers etc. between jpeg_start_compress and the
  jpeg_write_scanlines loop.
  In multi-pass processing, this routine is not used. }

{METHODDEF}
procedure pass_startup (cinfo : j_compress_ptr);
begin
  cinfo^.master^.call_pass_startup := FALSE; { reset flag so call only once }

  cinfo^.marker^.write_frame_header (cinfo);
  cinfo^.marker^.write_scan_header (cinfo);
end;


{ Finish up at end of pass. }

{METHODDEF}
procedure finish_pass_master (cinfo : j_compress_ptr);
var
  master : jcmaster_my_master_ptr;
begin
  master := jcmaster_my_master_ptr (cinfo^.master);

  { The entropy coder always needs an end-of-pass call,
    either to analyze statistics or to flush its output buffer. }
  cinfo^.entropy^.finish_pass (cinfo);

  { Update state for next pass }
  case (master^.pass_type) of
  main_pass:
    begin
      { next pass is either output of scan 0 (after optimization)
        or output of scan 1 (if no optimization). }

      master^.pass_type := output_pass;
      if (not cinfo^.optimize_coding) then
        Inc(master^.scan_number);
    end;
  huff_opt_pass:
    { next pass is always output of current scan }
    master^.pass_type := output_pass;
  output_pass:
    begin
      { next pass is either optimization or output of next scan }
      if (cinfo^.optimize_coding) then
        master^.pass_type := huff_opt_pass;
      Inc(master^.scan_number);
    end;
  end;

  Inc(master^.pass_number);
end;


{ Initialize master compression control. }

{GLOBAL}
procedure jinit_c_master_control (cinfo : j_compress_ptr;
                                  transcode_only : boolean);
var
  master : jcmaster_my_master_ptr;
begin
  master := jcmaster_my_master_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				  SIZEOF(my_comp_master)) );
  cinfo^.master := jpeg_comp_master_ptr(master);
  master^.pub.prepare_for_pass := prepare_for_pass;
  master^.pub.pass_startup := pass_startup;
  master^.pub.finish_pass := finish_pass_master;
  master^.pub.is_last_pass := FALSE;

  { Validate parameters, determine derived values }
  jcmaster_initial_setup(cinfo);

  if (cinfo^.scan_info <> NIL) then
  begin
{$ifdef C_MULTISCAN_FILES_SUPPORTED}
    validate_script(cinfo);
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
  end
  else
  begin
    cinfo^.progressive_mode := FALSE;
    cinfo^.num_scans := 1;
  end;

  if (cinfo^.progressive_mode) then  {  TEMPORARY HACK ??? }
    cinfo^.optimize_coding := TRUE;  { assume default tables no good for progressive mode }

  { Initialize my private state }
  if (transcode_only) then
  begin
    { no main pass in transcoding }
    if (cinfo^.optimize_coding) then
      master^.pass_type := huff_opt_pass
    else
      master^.pass_type := output_pass;
  end
  else
  begin
    { for normal compression, first pass is always this type: }
    master^.pass_type := main_pass;
  end;
  master^.scan_number := 0;
  master^.pass_number := 0;
  if (cinfo^.optimize_coding) then
    master^.total_passes := cinfo^.num_scans * 2
  else
    master^.total_passes := cinfo^.num_scans;
end;

{== imjcinit implementation ==}

{ Master selection of compression modules.
  This is done once at the start of processing an image.  We determine
  which modules will be used and give them appropriate initialization calls. }

{GLOBAL}
procedure jinit_compress_master (cinfo : j_compress_ptr);
begin
  { Initialize master control (includes parameter checking/processing) }
  jinit_c_master_control(cinfo, FALSE { full compression });

  { Preprocessing }
  if (not cinfo^.raw_data_in) then
  begin
    jinit_color_converter(cinfo);
    jinit_downsampler(cinfo);
    jinit_c_prep_controller(cinfo, FALSE { never need full buffer here });
  end;
  { Forward DCT }
  jinit_forward_dct(cinfo);
  { Entropy encoding: either Huffman or arithmetic coding. }
  if (cinfo^.arith_code) then
  begin
    ERREXIT(j_common_ptr(cinfo), JERR_ARITH_NOTIMPL);
  end
  else
  begin
    if (cinfo^.progressive_mode) then
    begin
{$ifdef C_PROGRESSIVE_SUPPORTED}
      jinit_phuff_encoder(cinfo);
{$else}
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
    end
    else
      jinit_huff_encoder(cinfo);
  end;

  { Need a full-image coefficient buffer in any multi-pass mode. }
  jinit_c_coef_controller(cinfo,
			  (cinfo^.num_scans > 1) or (cinfo^.optimize_coding));
  jinit_c_main_controller(cinfo, FALSE { never need full buffer here });

  jinit_marker_writer(cinfo);

  { We can now tell the memory manager to allocate virtual arrays. }
  cinfo^.mem^.realize_virt_arrays (j_common_ptr(cinfo));

  { Write the datastream header (SOI) immediately.
    Frame and scan headers are postponed till later.
    This lets application insert special markers after the SOI. }

  cinfo^.marker^.write_file_header (cinfo);
end;

{== imjcapimin implementation ==}

procedure jpeg_create_compress(cinfo : j_compress_ptr);
begin
  jpeg_CreateCompress(cinfo, JPEG_LIB_VERSION,
                      size_t(sizeof(jpeg_compress_struct)));
end;

{ Initialization of a JPEG compression object.
  The error manager must already be set up (in case memory manager fails). }

{GLOBAL}
procedure jpeg_CreateCompress (cinfo : j_compress_ptr;
                               version : int;
                               structsize : size_t);
var
  i : int;
var
  err : jpeg_error_mgr_ptr;
  client_data : voidp;
begin

  { Guard against version mismatches between library and caller. }
  cinfo^.mem := NIL;		{ so jpeg_destroy knows mem mgr not called }
  if (version <> JPEG_LIB_VERSION) then
    ERREXIT2(j_common_ptr(cinfo), JERR_BAD_LIB_VERSION, JPEG_LIB_VERSION, version);
  if (structsize <> SIZEOF(jpeg_compress_struct)) then
    ERREXIT2(j_common_ptr(cinfo), JERR_BAD_STRUCT_SIZE,
	     int(SIZEOF(jpeg_compress_struct)), int(structsize));

  { For debugging purposes, we zero the whole master structure.
    But the application has already set the err pointer, and may have set
    client_data, so we have to save and restore those fields. }

  err := cinfo^.err;
  client_data := cinfo^.client_data;
  MEMZERO(cinfo, SIZEOF(jpeg_compress_struct));
  cinfo^.err := err;
  cinfo^.is_decompressor := FALSE;
  cinfo^.client_data := client_data;

  { Initialize a memory manager instance for this object }
  jinit_memory_mgr(j_common_ptr(cinfo));

  { Zero out pointers to permanent structures. }
  cinfo^.progress := NIL;
  cinfo^.dest := NIL;

  cinfo^.comp_info := NIL;

  for i := 0 to pred(NUM_QUANT_TBLS) do
    cinfo^.quant_tbl_ptrs[i] := NIL;

  for i := 0 to pred(NUM_HUFF_TBLS) do
  begin
    cinfo^.dc_huff_tbl_ptrs[i] := NIL;
    cinfo^.ac_huff_tbl_ptrs[i] := NIL;
  end;

  cinfo^.script_space := NIL;

  cinfo^.input_gamma := 1.0;	{ in case application forgets }

  { OK, I'm ready }
  cinfo^.global_state := CSTATE_START;
end;


{ Destruction of a JPEG compression object }

{GLOBAL}
procedure jpeg_destroy_compress (cinfo : j_compress_ptr);
begin
  jpeg_destroy(j_common_ptr(cinfo)); { use common routine }
end;


{ Abort processing of a JPEG compression operation,
  but don't destroy the object itself. }

{GLOBAL}
procedure jpeg_abort_compress (cinfo : j_compress_ptr);
begin
  jpeg_abort(j_common_ptr(cinfo)); { use common routine }
end;


{ Forcibly suppress or un-suppress all quantization and Huffman tables.
  Marks all currently defined tables as already written (if suppress)
  or not written (if !suppress).  This will control whether they get emitted
  by a subsequent jpeg_start_compress call.

  This routine is exported for use by applications that want to produce
  abbreviated JPEG datastreams.  It logically belongs in jcparam.c, but
  since it is called by jpeg_start_compress, we put it here --- otherwise
  jcparam.o would be linked whether the application used it or not. }

{GLOBAL}
procedure jpeg_suppress_tables (cinfo : j_compress_ptr;
                                suppress : boolean);
var
  i : int;
  qtbl : JQUANT_TBL_PTR;
  htbl : JHUFF_TBL_PTR;
begin
  for i := 0 to pred(NUM_QUANT_TBLS) do
  begin
    qtbl := cinfo^.quant_tbl_ptrs[i];
    if (qtbl <> NIL) then
      qtbl^.sent_table := suppress;
  end;

  for i := 0 to pred(NUM_HUFF_TBLS) do
  begin
    htbl := cinfo^.dc_huff_tbl_ptrs[i];
    if (htbl <> NIL) then
      htbl^.sent_table := suppress;
    htbl := cinfo^.ac_huff_tbl_ptrs[i];
    if (htbl <> NIL) then
      htbl^.sent_table := suppress;
  end;
end;


{ Finish JPEG compression.

  If a multipass operating mode was selected, this may do a great deal of
  work including most of the actual output. }

{GLOBAL}
procedure jpeg_finish_compress (cinfo : j_compress_ptr);
var
  iMCU_row : JDIMENSION;
begin
  if (cinfo^.global_state = CSTATE_SCANNING) or
     (cinfo^.global_state = CSTATE_RAW_OK) then
  begin
    { Terminate first pass }
    if (cinfo^.next_scanline < cinfo^.image_height) then
      ERREXIT(j_common_ptr(cinfo), JERR_TOO_LITTLE_DATA);
    cinfo^.master^.finish_pass (cinfo);
  end
  else
    if (cinfo^.global_state <> CSTATE_WRCOEFS) then
      ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  { Perform any remaining passes }
  while (not cinfo^.master^.is_last_pass) do
  begin
    cinfo^.master^.prepare_for_pass (cinfo);
    for iMCU_row := 0 to pred(cinfo^.total_iMCU_rows) do
    begin
      if (cinfo^.progress <> NIL) then
      begin
      	cinfo^.progress^.pass_counter := long (iMCU_row);
      	cinfo^.progress^.pass_limit := long (cinfo^.total_iMCU_rows);
      	cinfo^.progress^.progress_monitor (j_common_ptr(cinfo));
      end;
      { We bypass the main controller and invoke coef controller directly;
        all work is being done from the coefficient buffer. }

      if (not cinfo^.coef^.compress_data (cinfo, JSAMPIMAGE(NIL))) then
      	ERREXIT(j_common_ptr(cinfo), JERR_CANT_SUSPEND);
    end;
    cinfo^.master^.finish_pass (cinfo);
  end;
  { Write EOI, do final cleanup }
  cinfo^.marker^.write_file_trailer (cinfo);
  cinfo^.dest^.term_destination (cinfo);
  { We can use jpeg_abort to release memory and reset global_state }
  jpeg_abort(j_common_ptr(cinfo));
end;


{ Write a special marker.
  This is only recommended for writing COM or APPn markers.
  Must be called after jpeg_start_compress() and before
  first call to jpeg_write_scanlines() or jpeg_write_raw_data(). }

{GLOBAL}
procedure jpeg_write_marker (cinfo : j_compress_ptr;
                             marker : int;
		             dataptr : JOCTETptr;
                             datalen : uInt);
var
  write_marker_byte : procedure(info : j_compress_ptr; val : int);
begin
  if (cinfo^.next_scanline <> 0) or
     ((cinfo^.global_state <> CSTATE_SCANNING) and
      (cinfo^.global_state <> CSTATE_RAW_OK) and
      (cinfo^.global_state <> CSTATE_WRCOEFS)) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  cinfo^.marker^.write_marker_header (cinfo, marker, datalen);
  write_marker_byte := cinfo^.marker^.write_marker_byte; { copy for speed }
  while (datalen <> 0) do
  begin
    Dec(datalen);
    write_marker_byte (cinfo, dataptr^);
    Inc(dataptr);
  end;
end;

{ Same, but piecemeal. }

{GLOBAL}
procedure jpeg_write_m_header (cinfo : j_compress_ptr;
                               marker : int;
                               datalen : uint);
begin
  if (cinfo^.next_scanline <> 0) or
     ((cinfo^.global_state <> CSTATE_SCANNING) and
      (cinfo^.global_state <> CSTATE_RAW_OK) and
      (cinfo^.global_state <> CSTATE_WRCOEFS)) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  cinfo^.marker^.write_marker_header (cinfo, marker, datalen);
end;

{GLOBAL}
procedure jpeg_write_m_byte (cinfo : j_compress_ptr; val : int);
begin
  cinfo^.marker^.write_marker_byte (cinfo, val);
end;


{ Alternate compression function: just write an abbreviated table file.
  Before calling this, all parameters and a data destination must be set up.

  To produce a pair of files containing abbreviated tables and abbreviated
  image data, one would proceed as follows:

 		initialize JPEG object
 		set JPEG parameters
 		set destination to table file
 		jpeg_write_tables(cinfo);
 		set destination to image file
 		jpeg_start_compress(cinfo, FALSE);
 		write data...
 		jpeg_finish_compress(cinfo);

  jpeg_write_tables has the side effect of marking all tables written
  (same as jpeg_suppress_tables(..., TRUE)).  Thus a subsequent start_compress
  will not re-emit the tables unless it is passed write_all_tables=TRUE. }

{GLOBAL}
procedure jpeg_write_tables (cinfo : j_compress_ptr);
begin
  if (cinfo^.global_state <> CSTATE_START) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  { (Re)initialize error mgr and destination modules }
  cinfo^.err^.reset_error_mgr (j_common_ptr(cinfo));
  cinfo^.dest^.init_destination (cinfo);
  { Initialize the marker writer ... bit of a crock to do it here. }
  jinit_marker_writer(cinfo);
  { Write them tables! }
  cinfo^.marker^.write_tables_only (cinfo);
  { And clean up. }
  cinfo^.dest^.term_destination (cinfo);

  { In library releases up through v6a, we called jpeg_abort() here to free
    any working memory allocated by the destination manager and marker
    writer.  Some applications had a problem with that: they allocated space
    of their own from the library memory manager, and didn't want it to go
    away during write_tables.  So now we do nothing.  This will cause a
    memory leak if an app calls write_tables repeatedly without doing a full
    compression cycle or otherwise resetting the JPEG object.  However, that
    seems less bad than unexpectedly freeing memory in the normal case.
    An app that prefers the old behavior can call jpeg_abort for itself after
    each call to jpeg_write_tables(). }
end;

{== imjcapistd implementation ==}

{ Compression initialization.
  Before calling this, all parameters and a data destination must be set up.

  We require a write_all_tables parameter as a failsafe check when writing
  multiple datastreams from the same compression object.  Since prior runs
  will have left all the tables marked sent_table=TRUE, a subsequent run
  would emit an abbreviated stream (no tables) by default.  This may be what
  is wanted, but for safety's sake it should not be the default behavior:
  programmers should have to make a deliberate choice to emit abbreviated
  images.  Therefore the documentation and examples should encourage people
  to pass write_all_tables=TRUE; then it will take active thought to do the
  wrong thing. }

{GLOBAL}
procedure jpeg_start_compress (cinfo : j_compress_ptr;
                               write_all_tables : boolean);
begin
  if (cinfo^.global_state <> CSTATE_START) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  if (write_all_tables) then
    jpeg_suppress_tables(cinfo, FALSE);	{ mark all tables to be written }

  { (Re)initialize error mgr and destination modules }
  cinfo^.err^.reset_error_mgr (j_common_ptr(cinfo));
  cinfo^.dest^.init_destination (cinfo);
  { Perform master selection of active modules }
  jinit_compress_master(cinfo);
  { Set up for the first pass }
  cinfo^.master^.prepare_for_pass (cinfo);
  { Ready for application to drive first pass through jpeg_write_scanlines
    or jpeg_write_raw_data. }

  cinfo^.next_scanline := 0;
  if cinfo^.raw_data_in then
    cinfo^.global_state := CSTATE_RAW_OK
  else
    cinfo^.global_state := CSTATE_SCANNING;
end;


{ Write some scanlines of data to the JPEG compressor.

  The return value will be the number of lines actually written.
  This should be less than the supplied num_lines only in case that
  the data destination module has requested suspension of the compressor,
  or if more than image_height scanlines are passed in.

  Note: we warn about excess calls to jpeg_write_scanlines() since
  this likely signals an application programmer error.  However,
  excess scanlines passed in the last valid call are *silently* ignored,
  so that the application need not adjust num_lines for end-of-image
  when using a multiple-scanline buffer. }

{GLOBAL}
function jpeg_write_scanlines (cinfo : j_compress_ptr;
                              scanlines : JSAMPARRAY;
		              num_lines : JDIMENSION) : JDIMENSION;
var
  row_ctr, rows_left : JDIMENSION;
begin
  if (cinfo^.global_state <> CSTATE_SCANNING) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  if (cinfo^.next_scanline >= cinfo^.image_height) then
    WARNMS(j_common_ptr(cinfo), JWRN_TOO_MUCH_DATA);

  { Call progress monitor hook if present }
  if (cinfo^.progress <> NIL) then
  begin
    cinfo^.progress^.pass_counter := long (cinfo^.next_scanline);
    cinfo^.progress^.pass_limit := long (cinfo^.image_height);
    cinfo^.progress^.progress_monitor (j_common_ptr(cinfo));
  end;

  { Give master control module another chance if this is first call to
    jpeg_write_scanlines.  This lets output of the frame/scan headers be
    delayed so that application can write COM, etc, markers between
    jpeg_start_compress and jpeg_write_scanlines. }
  if (cinfo^.master^.call_pass_startup) then
    cinfo^.master^.pass_startup (cinfo);

  { Ignore any extra scanlines at bottom of image. }
  rows_left := cinfo^.image_height - cinfo^.next_scanline;
  if (num_lines > rows_left) then
    num_lines := rows_left;

  row_ctr := 0;
  cinfo^.main^.process_data (cinfo, scanlines, {var}row_ctr, num_lines);
  Inc(cinfo^.next_scanline, row_ctr);
  jpeg_write_scanlines := row_ctr;
end;


{ Alternate entry point to write raw data.
  Processes exactly one iMCU row per call, unless suspended. }

{GLOBAL}
function jpeg_write_raw_data (cinfo : j_compress_ptr;
                              data : JSAMPIMAGE;
		              num_lines : JDIMENSION) : JDIMENSION;
var
  lines_per_iMCU_row : JDIMENSION;
begin
  if (cinfo^.global_state <> CSTATE_RAW_OK) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  if (cinfo^.next_scanline >= cinfo^.image_height) then
  begin
    WARNMS(j_common_ptr(cinfo), JWRN_TOO_MUCH_DATA);
    jpeg_write_raw_data := 0;
    exit;
  end;

  { Call progress monitor hook if present }
  if (cinfo^.progress <> NIL) then
  begin
    cinfo^.progress^.pass_counter := long(cinfo^.next_scanline);
    cinfo^.progress^.pass_limit := long(cinfo^.image_height);
    cinfo^.progress^.progress_monitor (j_common_ptr(cinfo));
  end;

  { Give master control module another chance if this is first call to
    jpeg_write_raw_data.  This lets output of the frame/scan headers be
    delayed so that application can write COM, etc, markers between
    jpeg_start_compress and jpeg_write_raw_data. }

  if (cinfo^.master^.call_pass_startup) then
    cinfo^.master^.pass_startup (cinfo);

  { Verify that at least one iMCU row has been passed. }
  lines_per_iMCU_row := cinfo^.max_v_samp_factor * DCTSIZE;
  if (num_lines < lines_per_iMCU_row) then
    ERREXIT(j_common_ptr(cinfo), JERR_BUFFER_SIZE);

  { Directly compress the row. }
  if (not cinfo^.coef^.compress_data (cinfo, data)) then
  begin
    { If compressor did not consume the whole row, suspend processing. }
    jpeg_write_raw_data := 0;
    exit;
  end;

  { OK, we processed one iMCU row. }
  Inc(cinfo^.next_scanline, lines_per_iMCU_row);
  jpeg_write_raw_data := lines_per_iMCU_row;
end;

{== imjcparam implementation ==}

{ Quantization table setup routines }

{GLOBAL}
procedure jpeg_add_quant_table (cinfo : j_compress_ptr;
                      which_tbl : int;
		      const basic_table : array of uInt;
		      scale_factor : int;
                      force_baseline : boolean);
{ Define a quantization table equal to the basic_table times
  a scale factor (given as a percentage).
  If force_baseline is TRUE, the computed quantization table entries
  are limited to 1..255 for JPEG baseline compatibility. }
var
  qtblptr :^JQUANT_TBL_PTR;
  i : int;
  temp : long;
begin
  { Safety check to ensure start_compress not called yet. }
  if (cinfo^.global_state <> CSTATE_START) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  if (which_tbl < 0) or (which_tbl >= NUM_QUANT_TBLS) then
    ERREXIT1(j_common_ptr(cinfo), JERR_DQT_INDEX, which_tbl);

  qtblptr := @(cinfo^.quant_tbl_ptrs[which_tbl]);

  if (qtblptr^ = NIL) then
    qtblptr^ := jpeg_alloc_quant_table(j_common_ptr(cinfo));

  for i := 0 to pred(DCTSIZE2) do
  begin
    temp := (long(basic_table[i]) * scale_factor + long(50)) div long(100);
    { limit the values to the valid range }
    if (temp <= long(0)) then
      temp := long(1);
    if (temp > long(32767)) then
      temp := long(32767); { max quantizer needed for 12 bits }
    if (force_baseline) and (temp > long(255)) then
      temp := long(255);		{ limit to baseline range if requested }
    (qtblptr^)^.quantval[i] := UINT16 (temp);
  end;

  { Initialize sent_table FALSE so table will be written to JPEG file. }
  (qtblptr^)^.sent_table := FALSE;
end;


{GLOBAL}
procedure jpeg_set_linear_quality (cinfo : j_compress_ptr;
                                   scale_factor : int;
			           force_baseline : boolean);
{ Set or change the 'quality' (quantization) setting, using default tables
  and a straight percentage-scaling quality scale.  In most cases it's better
  to use jpeg_set_quality (below); this entry point is provided for
  applications that insist on a linear percentage scaling. }

{ These are the sample quantization tables given in JPEG spec section K.1.
  The spec says that the values given produce "good" quality, and
  when divided by 2, "very good" quality. }

const
  std_luminance_quant_tbl : array[0..DCTSIZE2-1] of uInt =
   (16,  11,  10,  16,  24,  40,  51,  61,
    12,  12,  14,  19,  26,  58,  60,  55,
    14,  13,  16,  24,  40,  57,  69,  56,
    14,  17,  22,  29,  51,  87,  80,  62,
    18,  22,  37,  56,  68, 109, 103,  77,
    24,  35,  55,  64,  81, 104, 113,  92,
    49,  64,  78,  87, 103, 121, 120, 101,
    72,  92,  95,  98, 112, 100, 103,  99);

const
  std_chrominance_quant_tbl : array[0..DCTSIZE2-1] of uInt =
   (17,  18,  24,  47,  99,  99,  99,  99,
    18,  21,  26,  66,  99,  99,  99,  99,
    24,  26,  56,  99,  99,  99,  99,  99,
    47,  66,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99);
begin
  { Set up two quantization tables using the specified scaling }
  jpeg_add_quant_table(cinfo, 0, std_luminance_quant_tbl,
		       scale_factor, force_baseline);
  jpeg_add_quant_table(cinfo, 1, std_chrominance_quant_tbl,
		       scale_factor, force_baseline);
end;


{GLOBAL}
function jpeg_quality_scaling (quality : int) : int;
{ Convert a user-specified quality rating to a percentage scaling factor
  for an underlying quantization table, using our recommended scaling curve.
  The input 'quality' factor should be 0 (terrible) to 100 (very good). }
begin
  { Safety limit on quality factor.  Convert 0 to 1 to avoid zero divide. }
  if (quality <= 0) then
    quality := 1;
  if (quality > 100) then
    quality := 100;

  { The basic table is used as-is (scaling 100) for a quality of 50.
    Qualities 50..100 are converted to scaling percentage 200 - 2*Q;
    note that at Q=100 the scaling is 0, which will cause jpeg_add_quant_table
    to make all the table entries 1 (hence, minimum quantization loss).
    Qualities 1..50 are converted to scaling percentage 5000/Q. }
  if (quality < 50) then
    quality := 5000 div quality
  else
    quality := 200 - quality*2;

  jpeg_quality_scaling := quality;
end;


{GLOBAL}
procedure jpeg_set_quality (cinfo : j_compress_ptr;
                            quality : int;
                            force_baseline : boolean);
{ Set or change the 'quality' (quantization) setting, using default tables.
  This is the standard quality-adjusting entry point for typical user
  interfaces; only those who want detailed control over quantization tables
  would use the preceding three routines directly. }
begin
  { Convert user 0-100 rating to percentage scaling }
  quality := jpeg_quality_scaling(quality);

  { Set up standard quality tables }
  jpeg_set_linear_quality(cinfo, quality, force_baseline);
end;


{ Huffman table setup routines }

{LOCAL}
procedure add_huff_table (cinfo : j_compress_ptr;
                          var htblptr : JHUFF_TBL_PTR;
                          var bits : array of UINT8;
                          var val : array of UINT8);
{ Define a Huffman table }
var
  nsymbols, len : int;
begin
  if (htblptr = NIL) then
    htblptr := jpeg_alloc_huff_table(j_common_ptr(cinfo));

  { Copy the number-of-symbols-of-each-code-length counts }
  MEMCOPY(@htblptr^.bits, @bits, SIZEOF(htblptr^.bits));


  { Validate the counts.  We do this here mainly so we can copy the right
    number of symbols from the val[] array, without risking marching off
    the end of memory.  jchuff.c will do a more thorough test later. }

  nsymbols := 0;
  for len := 1 to 16 do
    Inc(nsymbols, bits[len]);
  if (nsymbols < 1) or (nsymbols > 256) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_HUFF_TABLE);

  MEMCOPY(@htblptr^.huffval, @val, nsymbols * SIZEOF(UINT8));

  { Initialize sent_table FALSE so table will be written to JPEG file. }
  (htblptr)^.sent_table := FALSE;
end;


{$J+}
{LOCAL}
procedure std_huff_tables (cinfo : j_compress_ptr);
{ Set up the standard Huffman tables (cf. JPEG standard section K.3) }
{ IMPORTANT: these are only valid for 8-bit data precision! }
  const bits_dc_luminance : array[0..17-1] of UINT8 =
    ({ 0-base } 0, 0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0);
  const val_dc_luminance : array[0..11] of UINT8 =
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);

  const bits_dc_chrominance : array[0..17-1] of UINT8 =
    ( { 0-base } 0, 0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0 );
  const val_dc_chrominance : array[0..11] of UINT8 =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );

  const bits_ac_luminance : array[0..17-1] of UINT8 =
    ( { 0-base } 0, 0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, $7d );
  const val_ac_luminance : array[0..161] of UINT8 =
    ( $01, $02, $03, $00, $04, $11, $05, $12,
      $21, $31, $41, $06, $13, $51, $61, $07,
      $22, $71, $14, $32, $81, $91, $a1, $08,
      $23, $42, $b1, $c1, $15, $52, $d1, $f0,
      $24, $33, $62, $72, $82, $09, $0a, $16,
      $17, $18, $19, $1a, $25, $26, $27, $28,
      $29, $2a, $34, $35, $36, $37, $38, $39,
      $3a, $43, $44, $45, $46, $47, $48, $49,
      $4a, $53, $54, $55, $56, $57, $58, $59,
      $5a, $63, $64, $65, $66, $67, $68, $69,
      $6a, $73, $74, $75, $76, $77, $78, $79,
      $7a, $83, $84, $85, $86, $87, $88, $89,
      $8a, $92, $93, $94, $95, $96, $97, $98,
      $99, $9a, $a2, $a3, $a4, $a5, $a6, $a7,
      $a8, $a9, $aa, $b2, $b3, $b4, $b5, $b6,
      $b7, $b8, $b9, $ba, $c2, $c3, $c4, $c5,
      $c6, $c7, $c8, $c9, $ca, $d2, $d3, $d4,
      $d5, $d6, $d7, $d8, $d9, $da, $e1, $e2,
      $e3, $e4, $e5, $e6, $e7, $e8, $e9, $ea,
      $f1, $f2, $f3, $f4, $f5, $f6, $f7, $f8,
      $f9, $fa );

  const bits_ac_chrominance : array[0..17-1] of UINT8 =
    ( { 0-base } 0, 0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, $77 );
  const val_ac_chrominance : array[0..161] of UINT8 =
    ( $00, $01, $02, $03, $11, $04, $05, $21,
      $31, $06, $12, $41, $51, $07, $61, $71,
      $13, $22, $32, $81, $08, $14, $42, $91,
      $a1, $b1, $c1, $09, $23, $33, $52, $f0,
      $15, $62, $72, $d1, $0a, $16, $24, $34,
      $e1, $25, $f1, $17, $18, $19, $1a, $26,
      $27, $28, $29, $2a, $35, $36, $37, $38,
      $39, $3a, $43, $44, $45, $46, $47, $48,
      $49, $4a, $53, $54, $55, $56, $57, $58,
      $59, $5a, $63, $64, $65, $66, $67, $68,
      $69, $6a, $73, $74, $75, $76, $77, $78,
      $79, $7a, $82, $83, $84, $85, $86, $87,
      $88, $89, $8a, $92, $93, $94, $95, $96,
      $97, $98, $99, $9a, $a2, $a3, $a4, $a5,
      $a6, $a7, $a8, $a9, $aa, $b2, $b3, $b4,
      $b5, $b6, $b7, $b8, $b9, $ba, $c2, $c3,
      $c4, $c5, $c6, $c7, $c8, $c9, $ca, $d2,
      $d3, $d4, $d5, $d6, $d7, $d8, $d9, $da,
      $e2, $e3, $e4, $e5, $e6, $e7, $e8, $e9,
      $ea, $f2, $f3, $f4, $f5, $f6, $f7, $f8,
      $f9, $fa );
begin
  add_huff_table(cinfo, cinfo^.dc_huff_tbl_ptrs[0],
  bits_dc_luminance, val_dc_luminance);
  add_huff_table(cinfo, cinfo^.ac_huff_tbl_ptrs[0],
  bits_ac_luminance, val_ac_luminance);
  add_huff_table(cinfo, cinfo^.dc_huff_tbl_ptrs[1],
  bits_dc_chrominance, val_dc_chrominance);
  add_huff_table(cinfo, cinfo^.ac_huff_tbl_ptrs[1],
  bits_ac_chrominance, val_ac_chrominance);
end;


{ Default parameter setup for compression.

  Applications that don't choose to use this routine must do their
  own setup of all these parameters.  Alternately, you can call this
  to establish defaults and then alter parameters selectively.  This
  is the recommended approach since, if we add any new parameters,
  your code will still work (they'll be set to reasonable defaults). }

{GLOBAL}
procedure jpeg_set_defaults (cinfo : j_compress_ptr);
var
  i : int;
begin
  { Safety check to ensure start_compress not called yet. }
  if (cinfo^.global_state <> CSTATE_START) then
    ERREXIT1(J_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  { Allocate comp_info array large enough for maximum component count.
    Array is made permanent in case application wants to compress
    multiple images at same param settings. }

  if (cinfo^.comp_info = NIL) then
    cinfo^.comp_info := jpeg_component_info_list_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
             MAX_COMPONENTS * SIZEOF(jpeg_component_info)) );

  { Initialize everything not dependent on the color space }

  cinfo^.data_precision := BITS_IN_JSAMPLE;
  { Set up two quantization tables using default quality of 75 }
  jpeg_set_quality(cinfo, 75, TRUE);
  { Set up two Huffman tables }
  std_huff_tables(cinfo);

  { Initialize default arithmetic coding conditioning }
  for i := 0 to pred(NUM_ARITH_TBLS) do
  begin
    cinfo^.arith_dc_L[i] := 0;
    cinfo^.arith_dc_U[i] := 1;
    cinfo^.arith_ac_K[i] := 5;
  end;

  { Default is no multiple-scan output }
  cinfo^.scan_info := NIL;
  cinfo^.num_scans := 0;

  { Expect normal source image, not raw downsampled data }
  cinfo^.raw_data_in := FALSE;

  { Use Huffman coding, not arithmetic coding, by default }
  cinfo^.arith_code := FALSE;

  { By default, don't do extra passes to optimize entropy coding }
  cinfo^.optimize_coding := FALSE;
  { The standard Huffman tables are only valid for 8-bit data precision.
    If the precision is higher, force optimization on so that usable
    tables will be computed.  This test can be removed if default tables
    are supplied that are valid for the desired precision. }

  if (cinfo^.data_precision > 8) then
    cinfo^.optimize_coding := TRUE;

  { By default, use the simpler non-cosited sampling alignment }
  cinfo^.CCIR601_sampling := FALSE;

  { No input smoothing }
  cinfo^.smoothing_factor := 0;

  { DCT algorithm preference }
  cinfo^.dct_method := JDCT_DEFAULT;

  { No restart markers }
  cinfo^.restart_interval := 0;
  cinfo^.restart_in_rows := 0;

  { Fill in default JFIF marker parameters.  Note that whether the marker
    will actually be written is determined by jpeg_set_colorspace.

    By default, the library emits JFIF version code 1.01.
    An application that wants to emit JFIF 1.02 extension markers should set
    JFIF_minor_version to 2.  We could probably get away with just defaulting
    to 1.02, but there may still be some decoders in use that will complain
    about that; saying 1.01 should minimize compatibility problems. }

  cinfo^.JFIF_major_version := 1; { Default JFIF version = 1.01 }
  cinfo^.JFIF_minor_version := 1;
  cinfo^.density_unit := 0;	{ Pixel size is unknown by default }
  cinfo^.X_density := 1;		{ Pixel aspect ratio is square by default }
  cinfo^.Y_density := 1;

  { Choose JPEG colorspace based on input space, set defaults accordingly }

  jpeg_default_colorspace(cinfo);
end;


{ Select an appropriate JPEG colorspace for in_color_space. }

{GLOBAL}
procedure jpeg_default_colorspace (cinfo : j_compress_ptr);
begin
  case (cinfo^.in_color_space) of
  JCS_GRAYSCALE:
    jpeg_set_colorspace(cinfo, JCS_GRAYSCALE);
  JCS_RGB:
    jpeg_set_colorspace(cinfo, JCS_YCbCr);
  JCS_YCbCr:
    jpeg_set_colorspace(cinfo, JCS_YCbCr);
  JCS_CMYK:
    jpeg_set_colorspace(cinfo, JCS_CMYK); { By default, no translation }
  JCS_YCCK:
    jpeg_set_colorspace(cinfo, JCS_YCCK);
  JCS_UNKNOWN:
    jpeg_set_colorspace(cinfo, JCS_UNKNOWN);
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_IN_COLORSPACE);
  end;
end;


{ Set the JPEG colorspace, and choose colorspace-dependent default values. }

{GLOBAL}
procedure jpeg_set_colorspace (cinfo : j_compress_ptr;
                               colorspace : J_COLOR_SPACE);
  { macro }
  procedure SET_COMP(index,id,hsamp,vsamp,quant,dctbl,actbl : int);
  begin
    with cinfo^.comp_info^[index] do
    begin
      component_id := (id);
      h_samp_factor := (hsamp);
      v_samp_factor := (vsamp);
      quant_tbl_no := (quant);
      dc_tbl_no := (dctbl);
      ac_tbl_no := (actbl);
    end;
  end;

var
  ci : int;
begin
  { Safety check to ensure start_compress not called yet. }
  if (cinfo^.global_state <> CSTATE_START) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  { For all colorspaces, we use Q and Huff tables 0 for luminance components,
    tables 1 for chrominance components. }

  cinfo^.jpeg_color_space := colorspace;

  cinfo^.write_JFIF_header := FALSE; { No marker for non-JFIF colorspaces }
  cinfo^.write_Adobe_marker := FALSE; { write no Adobe marker by default }

  case (colorspace) of
  JCS_GRAYSCALE:
    begin
      cinfo^.write_JFIF_header := TRUE; { Write a JFIF marker }
      cinfo^.num_components := 1;
      { JFIF specifies component ID 1 }
      SET_COMP(0, 1, 1,1, 0, 0,0);
    end;
  JCS_RGB:
    begin
      cinfo^.write_Adobe_marker := TRUE; { write Adobe marker to flag RGB }
      cinfo^.num_components := 3;
      SET_COMP(0, $52 { 'R' }, 1,1, 0, 0,0);
      SET_COMP(1, $47 { 'G' }, 1,1, 0, 0,0);
      SET_COMP(2, $42 { 'B' }, 1,1, 0, 0,0);
    end;
  JCS_YCbCr:
    begin
      cinfo^.write_JFIF_header := TRUE; { Write a JFIF marker }
      cinfo^.num_components := 3;
      { JFIF specifies component IDs 1,2,3 }
      { We default to 2x2 subsamples of chrominance }
      SET_COMP(0, 1, 2,2, 0, 0,0);
      SET_COMP(1, 2, 1,1, 1, 1,1);
      SET_COMP(2, 3, 1,1, 1, 1,1);
    end;
  JCS_CMYK:
    begin
      cinfo^.write_Adobe_marker := TRUE; { write Adobe marker to flag CMYK }
      cinfo^.num_components := 4;
      SET_COMP(0, $43 { 'C' }, 1,1, 0, 0,0);
      SET_COMP(1, $4D { 'M' }, 1,1, 0, 0,0);
      SET_COMP(2, $59 { 'Y' }, 1,1, 0, 0,0);
      SET_COMP(3, $4B { 'K' }, 1,1, 0, 0,0);
    end;
  JCS_YCCK:
    begin
      cinfo^.write_Adobe_marker := TRUE; { write Adobe marker to flag YCCK }
      cinfo^.num_components := 4;
      SET_COMP(0, 1, 2,2, 0, 0,0);
      SET_COMP(1, 2, 1,1, 1, 1,1);
      SET_COMP(2, 3, 1,1, 1, 1,1);
      SET_COMP(3, 4, 2,2, 0, 0,0);
    end;
  JCS_UNKNOWN:
    begin
      cinfo^.num_components := cinfo^.input_components;
      if (cinfo^.num_components < 1)
      or (cinfo^.num_components > MAX_COMPONENTS) then
        ERREXIT2(j_common_ptr(cinfo), JERR_COMPONENT_COUNT,
                 cinfo^.num_components, MAX_COMPONENTS);
      for ci := 0 to pred(cinfo^.num_components) do
      begin
        SET_COMP(ci, ci, 1,1, 0, 0,0);
      end;
    end;
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);
  end;
end;


{$ifdef C_PROGRESSIVE_SUPPORTED}

{LOCAL}
function fill_a_scan (scanptr : jpeg_scan_info_ptr;
                      ci : int; Ss : int;
                      Se : int; Ah : int;
                      Al : int) : jpeg_scan_info_ptr;
{ Support routine: generate one scan for specified component }
begin
  scanptr^.comps_in_scan := 1;
  scanptr^.component_index[0] := ci;
  scanptr^.Ss := Ss;
  scanptr^.Se := Se;
  scanptr^.Ah := Ah;
  scanptr^.Al := Al;
  Inc(scanptr);
  fill_a_scan := scanptr;
end;

{LOCAL}
function fill_scans (scanptr : jpeg_scan_info_ptr;
                     ncomps : int;
	             Ss : int; Se : int;
                     Ah : int; Al : int) : jpeg_scan_info_ptr;
{ Support routine: generate one scan for each component }
var
  ci : int;
begin

  for ci := 0 to pred(ncomps) do
  begin
    scanptr^.comps_in_scan := 1;
    scanptr^.component_index[0] := ci;
    scanptr^.Ss := Ss;
    scanptr^.Se := Se;
    scanptr^.Ah := Ah;
    scanptr^.Al := Al;
    Inc(scanptr);
  end;
  fill_scans := scanptr;
end;

{LOCAL}
function fill_dc_scans (scanptr : jpeg_scan_info_ptr;
                        ncomps : int;
                        Ah : int; Al : int) : jpeg_scan_info_ptr;
{ Support routine: generate interleaved DC scan if possible, else N scans }
var
  ci : int;
begin

  if (ncomps <= MAX_COMPS_IN_SCAN) then
  begin
    { Single interleaved DC scan }
    scanptr^.comps_in_scan := ncomps;
    for ci := 0 to pred(ncomps) do
      scanptr^.component_index[ci] := ci;
    scanptr^.Ss := 0;
    scanptr^.Se := 0;
    scanptr^.Ah := Ah;
    scanptr^.Al := Al;
    Inc(scanptr);
  end
  else
  begin
    { Noninterleaved DC scan for each component }
    scanptr := fill_scans(scanptr, ncomps, 0, 0, Ah, Al);
  end;
  fill_dc_scans := scanptr;
end;


{ Create a recommended progressive-JPEG script.
  cinfo^.num_components and cinfo^.jpeg_color_space must be correct. }

{GLOBAL}
procedure jpeg_simple_progression (cinfo : j_compress_ptr);
var
  ncomps : int;
  nscans : int;
  scanptr : jpeg_scan_info_ptr;
begin
  ncomps := cinfo^.num_components;

  { Safety check to ensure start_compress not called yet. }
  if (cinfo^.global_state <> CSTATE_START) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  { Figure space needed for script.  Calculation must match code below! }
  if (ncomps = 3) and (cinfo^.jpeg_color_space = JCS_YCbCr) then
  begin
    { Custom script for YCbCr color images. }
    nscans := 10;
  end
  else
  begin
    { All-purpose script for other color spaces. }
    if (ncomps > MAX_COMPS_IN_SCAN) then
      nscans := 6 * ncomps	{ 2 DC + 4 AC scans per component }
    else
      nscans := 2 + 4 * ncomps;	{ 2 DC scans; 4 AC scans per component }
  end;

  { Allocate space for script.
    We need to put it in the permanent pool in case the application performs
    multiple compressions without changing the settings.  To avoid a memory
    leak if jpeg_simple_progression is called repeatedly for the same JPEG
    object, we try to re-use previously allocated space, and we allocate
    enough space to handle YCbCr even if initially asked for grayscale. }

  if (cinfo^.script_space = NIL) or (cinfo^.script_space_size < nscans) then
  begin
    if nscans > 10 then
      cinfo^.script_space_size := nscans
    else
      cinfo^.script_space_size := 10;

    cinfo^.script_space := jpeg_scan_info_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
		cinfo^.script_space_size * SIZEOF(jpeg_scan_info)) );
  end;
  scanptr := cinfo^.script_space;

  cinfo^.scan_info := scanptr;
  cinfo^.num_scans := nscans;

  if (ncomps = 3) and (cinfo^.jpeg_color_space = JCS_YCbCr) then
  begin
    { Custom script for YCbCr color images. }
    { Initial DC scan }
    scanptr := fill_dc_scans(scanptr, ncomps, 0, 1);
    { Initial AC scan: get some luma data out in a hurry }
    scanptr := fill_a_scan(scanptr, 0, 1, 5, 0, 2);
    { Chroma data is too small to be worth expending many scans on }
    scanptr := fill_a_scan(scanptr, 2, 1, 63, 0, 1);
    scanptr := fill_a_scan(scanptr, 1, 1, 63, 0, 1);
    { Complete spectral selection for luma AC }
    scanptr := fill_a_scan(scanptr, 0, 6, 63, 0, 2);
    { Refine next bit of luma AC }
    scanptr := fill_a_scan(scanptr, 0, 1, 63, 2, 1);
    { Finish DC successive approximation }
    scanptr := fill_dc_scans(scanptr, ncomps, 1, 0);
    { Finish AC successive approximation }
    scanptr := fill_a_scan(scanptr, 2, 1, 63, 1, 0);
    scanptr := fill_a_scan(scanptr, 1, 1, 63, 1, 0);
    { Luma bottom bit comes last since it's usually largest scan }
    scanptr := fill_a_scan(scanptr, 0, 1, 63, 1, 0);
  end
  else
  begin
    { All-purpose script for other color spaces. }
    { Successive approximation first pass }
    scanptr := fill_dc_scans(scanptr, ncomps, 0, 1);
    scanptr := fill_scans(scanptr, ncomps, 1, 5, 0, 2);
    scanptr := fill_scans(scanptr, ncomps, 6, 63, 0, 2);
    { Successive approximation second pass }
    scanptr := fill_scans(scanptr, ncomps, 1, 63, 2, 1);
    { Successive approximation final pass }
    scanptr := fill_dc_scans(scanptr, ncomps, 1, 0);
    scanptr := fill_scans(scanptr, ncomps, 1, 63, 1, 0);
  end;
end;

{$endif}

{$J-}

{== imjdmarker implementation ==}

{ At all times, cinfo1.src.next_input_byte and .bytes_in_buffer reflect
  the current restart point; we update them only when we have reached a
  suitable place to restart if a suspension occurs. }


{ Routines to process JPEG markers.

  Entry condition: JPEG marker itself has been read and its code saved
    in cinfo^.unread_marker; input restart point is just after the marker.

  Exit: if return TRUE, have read and processed any parameters, and have
    updated the restart point to point after the parameters.
    If return FALSE, was forced to suspend before reaching end of
    marker parameters; restart point has not been moved.  Same routine
    will be called again after application supplies more input data.

  This approach to suspension assumes that all of a marker's parameters
  can fit into a single input bufferload.  This should hold for "normal"
  markers.  Some COM/APPn markers might have large parameter segments
  that might not fit.  If we are simply dropping such a marker, we use
  skip_input_data to get past it, and thereby put the problem on the
  source manager's shoulders.  If we are saving the marker's contents
  into memory, we use a slightly different convention: when forced to
  suspend, the marker processor updates the restart point to the end of
  what it's consumed (ie, the end of the buffer) before returning FALSE.
  On resumption, cinfo->unread_marker still contains the marker code,
  but the data source will point to the next chunk of marker data.
  The marker processor must retain internal state to deal with this.

  Note that we don't bother to avoid duplicate trace messages if a
  suspension occurs within marker parameters.  Other side effects
  require more care. }

{LOCAL}
function get_soi (cinfo : j_decompress_ptr) : boolean;
{ Process an SOI marker }
var
  i : int;
begin
  {$IFDEF DEBUG}
  TRACEMS(j_common_ptr(cinfo), 1, JTRC_SOI);
  {$ENDIF}

  if (cinfo^.marker^.saw_SOI) then
    ERREXIT(j_common_ptr(cinfo), JERR_SOI_DUPLICATE);

  { Reset all parameters that are defined to be reset by SOI }

  for i := 0 to Pred(NUM_ARITH_TBLS) do
  with cinfo^ do
  begin
    arith_dc_L[i] := 0;
    arith_dc_U[i] := 1;
    arith_ac_K[i] := 5;
  end;
  cinfo^.restart_interval := 0;

  { Set initial assumptions for colorspace etc }

  with cinfo^ do
  begin
    jpeg_color_space := JCS_UNKNOWN;
    CCIR601_sampling := FALSE; { Assume non-CCIR sampling??? }

    saw_JFIF_marker := FALSE;
    JFIF_major_version := 1; { set default JFIF APP0 values }
    JFIF_minor_version := 1;
    density_unit := 0;
    X_density := 1;
    Y_density := 1;
    saw_Adobe_marker := FALSE;
    Adobe_transform := 0;

    marker^.saw_SOI := TRUE;
  end;
  get_soi := TRUE;
end; { get_soi }


{LOCAL}
function get_sof(cinfo : j_decompress_ptr;
                 is_prog : boolean;
                 is_arith : boolean) : boolean;
{ Process a SOFn marker }
var
  length : INT32;
  c, ci : int;
  compptr : jpeg_component_info_ptr;
{ Declare and initialize local copies of input pointer/count }
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;
{}
  cinfo^.progressive_mode := is_prog;
  cinfo^.arith_code := is_arith;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sof := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );


  { Read a byte into variable cinfo^.data_precision.
    If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sof := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  cinfo^.data_precision := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

{ Read two bytes interpreted as an unsigned 16-bit integer.
  cinfo^.image_height should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sof := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  cinfo^.image_height := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( cinfo^.image_height, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

{ Read two bytes interpreted as an unsigned 16-bit integer.
  cinfo^.image_width should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sof := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  cinfo^.image_width := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( cinfo^.image_width, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  { Read a byte into variable cinfo^.num_components.
    If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sof := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  cinfo^.num_components := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  Dec(length, 8);

  {$IFDEF DEBUG}
  TRACEMS4(j_common_ptr(cinfo), 1, JTRC_SOF, cinfo^.unread_marker,
	   int(cinfo^.image_width), int(cinfo^.image_height),
	   cinfo^.num_components);
  {$ENDIF}

  if (cinfo^.marker^.saw_SOF) then
    ERREXIT(j_common_ptr(cinfo), JERR_SOF_DUPLICATE);

  { We don't support files in which the image height is initially specified }
  { as 0 and is later redefined by DNL.  As long as we have to check that,  }
  { might as well have a general sanity check. }
  if (cinfo^.image_height <= 0) or (cinfo^.image_width <= 0)
      or (cinfo^.num_components <= 0) then
    ERREXIT(j_common_ptr(cinfo), JERR_EMPTY_IMAGE);

  if (length <> (cinfo^.num_components * 3)) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

  if (cinfo^.comp_info = NIL) then { do only once, even if suspend }
    cinfo^.comp_info := jpeg_component_info_list_ptr(
     cinfo^.mem^.alloc_small(j_common_ptr(cinfo), JPOOL_IMAGE,
                  cinfo^.num_components * SIZEOF(jpeg_component_info)));

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    compptr^.component_index := ci;

    { Read a byte into variable compptr^.component_id.
      If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    compptr^.component_id := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    { Read a byte into variable c. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    c := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    compptr^.h_samp_factor := (c shr 4) and 15;
    compptr^.v_samp_factor := (c      ) and 15;

    { Read a byte into variable compptr^.quant_tbl_no.
      If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    compptr^.quant_tbl_no := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    {$IFDEF DEBUG}
    TRACEMS4(j_common_ptr(cinfo), 1, JTRC_SOF_COMPONENT,
	     compptr^.component_id, compptr^.h_samp_factor,
	     compptr^.v_samp_factor, compptr^.quant_tbl_no);
    {$ENDIF}

    Inc(compptr);
  end;

  cinfo^.marker^.saw_SOF := TRUE;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_sof := TRUE;
end;  { get_sof }


{LOCAL}
function get_sos (cinfo : j_decompress_ptr) : boolean;
{ Process a SOS marker }
label
  id_found;
var
  length : INT32;
  i, ci, n, c, cc : int;
  compptr : jpeg_component_info_ptr;
{ Declare and initialize local copies of input pointer/count }
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;    { Array[] of JOCTET; }
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{}

  if not cinfo^.marker^.saw_SOF then
    ERREXIT(j_common_ptr(cinfo), JERR_SOS_NO_SOF);

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sos := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sos := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );


  { Read a byte into variable n (Number of components).
    If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sos := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  n := GETJOCTET(next_input_byte^);  { Number of components }
  Inc(next_input_byte);

  {$IFDEF DEBUG}
  TRACEMS1(j_common_ptr(cinfo), 1, JTRC_SOS, n);
  {$ENDIF}

  if ((length <> (n * 2 + 6)) or (n < 1) or (n > MAX_COMPS_IN_SCAN)) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

  cinfo^.comps_in_scan := n;

  { Collect the component-spec parameters }

  for i := 0 to Pred(n) do
  begin
    { Read a byte into variable cc. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sos := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    cc := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    { Read a byte into variable c. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sos := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    c := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    compptr := jpeg_component_info_ptr(cinfo^.comp_info);
    for ci := 0 to Pred(cinfo^.num_components) do
    begin
      if (cc = compptr^.component_id) then
	goto id_found;
      Inc(compptr);
    end;

    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_COMPONENT_ID, cc);

  id_found:

    cinfo^.cur_comp_info[i] := compptr;
    compptr^.dc_tbl_no := (c shr 4) and 15;
    compptr^.ac_tbl_no := (c      ) and 15;

    {$IFDEF DEBUG}
    TRACEMS3(j_common_ptr(cinfo), 1, JTRC_SOS_COMPONENT, cc,
	     compptr^.dc_tbl_no, compptr^.ac_tbl_no);
    {$ENDIF}
  end;

  { Collect the additional scan parameters Ss, Se, Ah/Al. }
  { Read a byte into variable c. If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sos := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  c := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  cinfo^.Ss := c;

  { Read a byte into variable c. If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sos := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  c := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  cinfo^.Se := c;

  { Read a byte into variable c. If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sos := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  c := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  cinfo^.Ah := (c shr 4) and 15;
  cinfo^.Al := (c     ) and 15;

  {$IFDEF DEBUG}
  TRACEMS4(j_common_ptr(cinfo), 1, JTRC_SOS_PARAMS, cinfo^.Ss, cinfo^.Se,
	   cinfo^.Ah, cinfo^.Al);
  {$ENDIF}

  { Prepare to scan data & restart markers }
  cinfo^.marker^.next_restart_num := 0;

  { Count another SOS marker }
  Inc( cinfo^.input_scan_number );

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_sos := TRUE;
end;  { get_sos }


{METHODDEF}
function skip_variable (cinfo : j_decompress_ptr) : boolean;
{ Skip over an unknown or uninteresting variable-length marker }
var
  length : INT32;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;    { Array[] of JOCTET; }
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      skip_variable := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := uint(GETJOCTET(next_input_byte^)) shl 8;
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      skip_variable := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  Inc( length, GETJOCTET(next_input_byte^));
  Inc( next_input_byte );

  Dec(length, 2);

  {$IFDEF DEBUG}
  TRACEMS2(j_common_ptr(cinfo), 1, JTRC_MISC_MARKER,
    cinfo^.unread_marker, int(length));
  {$ENDIF}

  { Unload the local copies --- do this only at a restart boundary }
  { do before skip_input_data }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  if (length > 0) then
    cinfo^.src^.skip_input_data(cinfo, long(length));

  skip_variable := TRUE;
end;  { skip_variable }


{$IFDEF D_ARITH_CODING_SUPPORTED}

{LOCAL}
function get_dac (cinfo : j_decompress_ptr) : boolean;
{ Process a DAC marker }
var
  length : INT32;
  index, val : int;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_dac := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dac := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  Dec(length,  2);

  while (length > 0) do
  begin
    { Read a byte into variable index. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dac := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    index := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    { Read a byte into variable val. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dac := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    val := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    Dec( length, 2);

    {$IFDEF DEBUG}
    TRACEMS2(j_common_ptr(cinfo), 1, JTRC_DAC, index, val);
    {$ENDIF}

    if (index < 0) or (index >= (2*NUM_ARITH_TBLS)) then
      ERREXIT1(j_common_ptr(cinfo) , JERR_DAC_INDEX, index);

    if (index >= NUM_ARITH_TBLS) then
    begin { define AC table }
      cinfo^.arith_ac_K[index-NUM_ARITH_TBLS] := UINT8(val);
    end
    else
    begin { define DC table }
      cinfo^.arith_dc_L[index] := UINT8(val and $0F);
      cinfo^.arith_dc_U[index] := UINT8(val shr 4);
      if (cinfo^.arith_dc_L[index] > cinfo^.arith_dc_U[index]) then
	ERREXIT1(j_common_ptr(cinfo) , JERR_DAC_VALUE, val);
    end;
  end;

  if (length <> 0) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_dac := TRUE;
end;  { get_dac }

{$ELSE}

{LOCAL}
function get_dac (cinfo : j_decompress_ptr) : boolean;
begin
  get_dac := skip_variable(cinfo);
end;

{$ENDIF}

{LOCAL}
function get_dht (cinfo : j_decompress_ptr) : boolean;
{ Process a DHT marker }
var
  length : INT32;
  bits : Array[0..17-1] of UINT8;
  huffval : Array[0..256-1] of UINT8;
  i, index, count : int;
  htblptr : ^JHUFF_TBL_PTR;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_dht := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dht := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  Dec(length,  2);

  while (length > 16) do
  begin
    { Read a byte into variable index. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dht := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    index := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    {$IFDEF DEBUG}
    TRACEMS1(j_common_ptr(cinfo), 1, JTRC_DHT, index);
    {$ENDIF}

    bits[0] := 0;
    count := 0;
    for i := 1 to 16 do
    begin
      { Read a byte into variable bits[i]. If must suspend, return FALSE. }
      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          get_dht := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      bits[i] := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);

      Inc( count, bits[i] );
    end;

    Dec( length, (1 + 16) );

    {$IFDEF DEBUG}
    TRACEMS8(j_common_ptr(cinfo), 2, JTRC_HUFFBITS,
	     bits[1], bits[2], bits[3], bits[4],
	     bits[5], bits[6], bits[7], bits[8]);
    TRACEMS8(j_common_ptr(cinfo), 2, JTRC_HUFFBITS,
	     bits[9], bits[10], bits[11], bits[12],
	     bits[13], bits[14], bits[15], bits[16]);
    {$ENDIF}

    { Here we just do minimal validation of the counts to avoid walking
      off the end of our table space.  jdhuff.c will check more carefully. }

    if (count > 256) or (INT32(count) > length) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_HUFF_TABLE);

    for i := 0 to Pred(count) do
    begin
    { Read a byte into variable huffval[i]. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          get_dht := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      huffval[i] := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);
    end;

    Dec( length, count );

    if (index and $10)<>0 then
    begin  { AC table definition }
      Dec( index, $10 );
      htblptr := @cinfo^.ac_huff_tbl_ptrs[index];
    end
    else
    begin { DC table definition }
      htblptr := @cinfo^.dc_huff_tbl_ptrs[index];
    end;

    if (index < 0) or (index >= NUM_HUFF_TBLS) then
      ERREXIT1(j_common_ptr(cinfo), JERR_DHT_INDEX, index);

    if (htblptr^ = NIL) then
      htblptr^ := jpeg_alloc_huff_table(j_common_ptr(cinfo));

    MEMCOPY(@(htblptr^)^.bits, @bits, SIZEOF((htblptr^)^.bits));
    MEMCOPY(@(htblptr^)^.huffval, @huffval, SIZEOF((htblptr^)^.huffval));
  end;

  if (length <> 0) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_dht := TRUE;
end;  { get_dht }


{LOCAL}
function get_dqt (cinfo : j_decompress_ptr) : boolean;
{ Process a DQT marker }
var
  length : INT32;
  n, i, prec : int;
  tmp : uint;
  quant_ptr : JQUANT_TBL_PTR;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_dqt := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dqt := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  Dec( length, 2 );

  while (length > 0) do
  begin
    { Read a byte into variable n. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dqt := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    n := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    prec := n shr 4;
    n := n and $0F;

    {$IFDEF DEBUG}
    TRACEMS2(j_common_ptr(cinfo), 1, JTRC_DQT, n, prec);
    {$ENDIF}

    if (n >= NUM_QUANT_TBLS) then
      ERREXIT1(j_common_ptr(cinfo) , JERR_DQT_INDEX, n);

    if (cinfo^.quant_tbl_ptrs[n] = NIL) then
      cinfo^.quant_tbl_ptrs[n] := jpeg_alloc_quant_table(j_common_ptr(cinfo));
    quant_ptr := cinfo^.quant_tbl_ptrs[n];

    for i := 0 to Pred(DCTSIZE2) do
    begin
      if (prec <> 0) then
      begin
      { Read two bytes interpreted as an unsigned 16-bit integer.
        tmp should be declared unsigned int or perhaps INT32. }

      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
        if (bytes_in_buffer = 0) then
        begin
          if (not datasrc^.fill_input_buffer(cinfo)) then
          begin
            get_dqt := FALSE;
            exit;
          end;
          { Reload the local copies }
          next_input_byte := datasrc^.next_input_byte;
          bytes_in_buffer := datasrc^.bytes_in_buffer;
        end;
        Dec( bytes_in_buffer );

        tmp := (uint( GETJOCTET(next_input_byte^)) shl 8);
        Inc( next_input_byte );
        { make a byte available.
          Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
          but we must reload the local copies after a successful fill. }
          if (bytes_in_buffer = 0) then
          begin
            if (not datasrc^.fill_input_buffer(cinfo)) then
            begin
              get_dqt := FALSE;
              exit;
            end;
            { Reload the local copies }
            next_input_byte := datasrc^.next_input_byte;
            bytes_in_buffer := datasrc^.bytes_in_buffer;
          end;
          Dec( bytes_in_buffer );

        Inc( tmp, GETJOCTET( next_input_byte^));
        Inc( next_input_byte );

      end
      else
      begin
      { Read a byte into variable tmp. If must suspend, return FALSE. }
      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
        if (bytes_in_buffer = 0) then
        begin
          if (not datasrc^.fill_input_buffer(cinfo)) then
          begin
            get_dqt := FALSE;
            exit;
          end;
          { Reload the local copies }
          next_input_byte := datasrc^.next_input_byte;
          bytes_in_buffer := datasrc^.bytes_in_buffer;
        end;
        Dec( bytes_in_buffer );

        tmp := GETJOCTET(next_input_byte^);
        Inc(next_input_byte);
      end;

      { We convert the zigzag-order table to natural array order. }
      quant_ptr^.quantval[jpeg_natural_order[i]] := UINT16(tmp);
    end;

    if (cinfo^.err^.trace_level >= 2) then
    begin
      i := 0;
      while i < Pred(DCTSIZE2) do
      begin
        {$IFDEF DEBUG}
	TRACEMS8(j_common_ptr(cinfo), 2, JTRC_QUANTVALS,
		 quant_ptr^.quantval[i],   quant_ptr^.quantval[i+1],
		 quant_ptr^.quantval[i+2], quant_ptr^.quantval[i+3],
		 quant_ptr^.quantval[i+4], quant_ptr^.quantval[i+5],
		 quant_ptr^.quantval[i+6], quant_ptr^.quantval[i+7]);
        {$ENDIF}
        Inc(i, 8);
      end;
    end;

    Dec( length, DCTSIZE2+1 );
    if (prec <> 0) then
      Dec( length, DCTSIZE2 );
  end;

  if (length <> 0) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_dqt := TRUE;
end;  { get_dqt }


{LOCAL}
function get_dri (cinfo : j_decompress_ptr) : boolean;
{ Process a DRI marker }
var
  length : INT32;
  tmp : uint;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_dri := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dri := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  if (length <> 4) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

{ Read two bytes interpreted as an unsigned 16-bit integer.
  tmp should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_dri := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  tmp := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dri := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( tmp, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  {$IFDEF DEBUG}
  TRACEMS1(j_common_ptr(cinfo), 1, JTRC_DRI, tmp);
  {$ENDIF}

  cinfo^.restart_interval := tmp;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_dri := TRUE;
end;  { get_dri }


{ Routines for processing APPn and COM markers.
  These are either saved in memory or discarded, per application request.
  APP0 and APP14 are specially checked to see if they are
  JFIF and Adobe markers, respectively. }

const
  APP0_DATA_LEN	= 14;   { Length of interesting data in APP0 }
  APP14_DATA_LEN = 12;  { Length of interesting data in APP14 }
  APPN_DATA_LEN = 14;   { Must be the largest of the above!! }


{LOCAL}
procedure examine_app0 (cinfo : j_decompress_ptr;
                        var data : array of JOCTET;
                        datalen : uint;
                        remaining : INT32);

{ Examine first few bytes from an APP0.
  Take appropriate action if it is a JFIF marker.
  datalen is # of bytes at data[], remaining is length of rest of marker data.
}
{$IFDEF DEBUG}
var
  totallen : INT32;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  totallen := INT32(datalen) + remaining;
  {$ENDIF}
  if (datalen >= APP0_DATA_LEN) and
     (GETJOCTET(data[0]) = $4A) and
     (GETJOCTET(data[1]) = $46) and
     (GETJOCTET(data[2]) = $49) and
     (GETJOCTET(data[3]) = $46) and
     (GETJOCTET(data[4]) = 0) then
  begin
    { Found JFIF APP0 marker: save info }
    cinfo^.saw_JFIF_marker := TRUE;
    cinfo^.JFIF_major_version := GETJOCTET(data[5]);
    cinfo^.JFIF_minor_version := GETJOCTET(data[6]);
    cinfo^.density_unit := GETJOCTET(data[7]);
    cinfo^.X_density := (GETJOCTET(data[8]) shl 8) + GETJOCTET(data[9]);
    cinfo^.Y_density := (GETJOCTET(data[10]) shl 8) + GETJOCTET(data[11]);
    { Check version.
      Major version must be 1, anything else signals an incompatible change.
      (We used to treat this as an error, but now it's a nonfatal warning,
      because some bozo at Hijaak couldn't read the spec.)
      Minor version should be 0..2, but process anyway if newer. }

    if (cinfo^.JFIF_major_version <> 1) then
      WARNMS2(j_common_ptr(cinfo), JWRN_JFIF_MAJOR,
	      cinfo^.JFIF_major_version, cinfo^.JFIF_minor_version);
    { Generate trace messages }
    {$IFDEF DEBUG}
    TRACEMS5(j_common_ptr(cinfo), 1, JTRC_JFIF,
	     cinfo^.JFIF_major_version, cinfo^.JFIF_minor_version,
	     cinfo^.X_density, cinfo^.Y_density, cinfo^.density_unit);
    { Validate thumbnail dimensions and issue appropriate messages }
    if (GETJOCTET(data[12]) or GETJOCTET(data[13])) <> 0 then
      TRACEMS2(j_common_ptr(cinfo), 1, JTRC_JFIF_THUMBNAIL,
	       GETJOCTET(data[12]), GETJOCTET(data[13]));
    Dec(totallen, APP0_DATA_LEN);
    if (totallen <>
	( INT32(GETJOCTET(data[12])) * INT32(GETJOCTET(data[13])) * INT32(3) )) then
      TRACEMS1(j_common_ptr(cinfo), 1, JTRC_JFIF_BADTHUMBNAILSIZE, int(totallen));
    {$ENDIF}
  end
  else
    if (datalen >= 6) and
      (GETJOCTET(data[0]) = $4A) and
      (GETJOCTET(data[1]) = $46) and
      (GETJOCTET(data[2]) = $58) and
      (GETJOCTET(data[3]) = $58) and
      (GETJOCTET(data[4]) = 0) then
    begin
    { Found JFIF "JFXX" extension APP0 marker }
    { The library doesn't actually do anything with these,
      but we try to produce a helpful trace message. }
      {$IFDEF DEBUG}
      case (GETJOCTET(data[5])) of
        $10:
          TRACEMS1(j_common_ptr(cinfo), 1, JTRC_THUMB_JPEG, int(totallen));
        $11:
          TRACEMS1(j_common_ptr(cinfo), 1, JTRC_THUMB_PALETTE, int(totallen));
        $13:
          TRACEMS1(j_common_ptr(cinfo), 1, JTRC_THUMB_RGB, int(totallen));
        else
          TRACEMS2(j_common_ptr(cinfo), 1, JTRC_JFIF_EXTENSION,
	           GETJOCTET(data[5]), int(totallen));
      end;
      {$ENDIF}
    end
    else
    begin
      { Start of APP0 does not match "JFIF" or "JFXX", or too short }
      {$IFDEF DEBUG}
      TRACEMS1(j_common_ptr(cinfo), 1, JTRC_APP0, int(totallen));
      {$ENDIF}
    end;
end;


{LOCAL}
procedure examine_app14 (cinfo : j_decompress_ptr;
                         var data : array of JOCTET;
	                 datalen : uint;
                         remaining : INT32);
{ Examine first few bytes from an APP14.
  Take appropriate action if it is an Adobe marker.
  datalen is # of bytes at data[], remaining is length of rest of marker data.
 }
var
  {$IFDEF DEBUG}
  version, flags0, flags1,
  {$ENDIF}
  transform : uint;
begin
  if (datalen >= APP14_DATA_LEN) and
     (GETJOCTET(data[0]) = $41) and
     (GETJOCTET(data[1]) = $64) and
     (GETJOCTET(data[2]) = $6F) and
     (GETJOCTET(data[3]) = $62) and
     (GETJOCTET(data[4]) = $65) then
  begin
    { Found Adobe APP14 marker }
    {$IFDEF DEBUG}
    version := (GETJOCTET(data[5]) shl 8) + GETJOCTET(data[6]);
    flags0 := (GETJOCTET(data[7]) shl 8) + GETJOCTET(data[8]);
    flags1 := (GETJOCTET(data[9]) shl 8) + GETJOCTET(data[10]);
    {$ENDIF}
    transform := GETJOCTET(data[11]);
    {$IFDEF DEBUG}
    TRACEMS4(j_common_ptr(cinfo), 1, JTRC_ADOBE, version, flags0, flags1, transform);
    {$ENDIF}
    cinfo^.saw_Adobe_marker := TRUE;
    cinfo^.Adobe_transform := UINT8 (transform);
  end
  else
  begin
    { Start of APP14 does not match "Adobe", or too short }
    {$IFDEF DEBUG}
    TRACEMS1(j_common_ptr(cinfo), 1, JTRC_APP14, int (datalen + remaining));
    {$ENDIF}
  end;
end;


{METHODDEF}
function get_interesting_appn (cinfo : j_decompress_ptr) : boolean;
{ Process an APP0 or APP14 marker without saving it }
var
  length : INT32;
  b : array[0..APPN_DATA_LEN-1] of JOCTET;
  i, numtoread: uint;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_interesting_appn := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );

  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_interesting_appn := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  Inc( length, GETJOCTET(next_input_byte^));
  Inc( next_input_byte );

  Dec(length, 2);

  { get the interesting part of the marker data }
  if (length >= APPN_DATA_LEN) then
    numtoread := APPN_DATA_LEN
  else
    if (length > 0) then
      numtoread := uint(length)
    else
      numtoread := 0;

  if numtoread > 0 then
  begin
    for i := 0 to numtoread-1 do
    begin
    { Read a byte into b[i]. If must suspend, return FALSE. }
      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          get_interesting_appn := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      b[i] := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);
    end;
  end;

  Dec(length, numtoread);

  { process it }
  case (cinfo^.unread_marker) of
  M_APP0:
    examine_app0(cinfo, b, numtoread, length);
  M_APP14:
    examine_app14(cinfo, b, numtoread, length);
  else
    { can't get here unless jpeg_save_markers chooses wrong processor }
    ERREXIT1(j_common_ptr(cinfo), JERR_UNKNOWN_MARKER, cinfo^.unread_marker);
  end;

  { skip any remaining data -- could be lots }

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  if (length > 0) then
    cinfo^.src^.skip_input_data(cinfo, long(length));

  get_interesting_appn := TRUE;
end;

{$ifdef SAVE_MARKERS_SUPPORTED}

{METHODDEF}
function save_marker (cinfo : j_decompress_ptr) : boolean;
{ Save an APPn or COM marker into the marker list }
var
  marker : jdmarker_my_marker_ptr;
  cur_marker : jpeg_saved_marker_ptr;
  bytes_read, data_length : uint;
  data : JOCTET_FIELD_PTR;
  length : INT32;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
var
  limit : uint;
var
  prev : jpeg_saved_marker_ptr;
begin
  { local copies of input pointer/count }
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

  marker := jdmarker_my_marker_ptr(cinfo^.marker);
  cur_marker := marker^.cur_marker;
  length := 0;

  if (cur_marker = NIL) then
  begin
    { begin reading a marker }
    { Read two bytes interpreted as an unsigned 16-bit integer. }

    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        save_marker := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    length := (uint( GETJOCTET(next_input_byte^)) shl 8);
    Inc( next_input_byte );

    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        save_marker := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    Inc( length, GETJOCTET(next_input_byte^));
    Inc( next_input_byte );

    Dec(length, 2);
    if (length >= 0) then
    begin		{ watch out for bogus length word }
      { figure out how much we want to save }

      if (cinfo^.unread_marker = int(M_COM)) then
	limit := marker^.length_limit_COM
      else
	limit := marker^.length_limit_APPn[cinfo^.unread_marker - int(M_APP0)];
      if (uint(length) < limit) then
	limit := uint(length);
      { allocate and initialize the marker item }
      cur_marker := jpeg_saved_marker_ptr(
	cinfo^.mem^.alloc_large (j_common_ptr(cinfo), JPOOL_IMAGE,
                               SIZEOF(jpeg_marker_struct) + limit) );
      cur_marker^.next := NIL;
      cur_marker^.marker := UINT8 (cinfo^.unread_marker);
      cur_marker^.original_length := uint(length);
      cur_marker^.data_length := limit;
      { data area is just beyond the jpeg_marker_struct }
      cur_marker^.data := JOCTET_FIELD_PTR(cur_marker);
      Inc(jpeg_saved_marker_ptr(cur_marker^.data));
      data := cur_marker^.data;

      marker^.cur_marker := cur_marker;
      marker^.bytes_read := 0;
      bytes_read := 0;
      data_length := limit;
    end
    else
    begin
      { deal with bogus length word }
      data_length := 0;
      bytes_read := 0;
      data := NIL;
    end
  end
  else
  begin
    { resume reading a marker }
    bytes_read := marker^.bytes_read;
    data_length := cur_marker^.data_length;
    data := cur_marker^.data;
    Inc(data, bytes_read);
  end;

  while (bytes_read < data_length) do
  begin
    { move the restart point to here }
    datasrc^.next_input_byte := next_input_byte;
    datasrc^.bytes_in_buffer := bytes_in_buffer;

    marker^.bytes_read := bytes_read;
    { If there's not at least one byte in buffer, suspend }
    if (bytes_in_buffer = 0) then
    begin
      if not datasrc^.fill_input_buffer (cinfo) then
      begin
        save_marker := FALSE;
        exit;
      end;
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;

    { Copy bytes with reasonable rapidity }
    while (bytes_read < data_length) and (bytes_in_buffer > 0) do
    begin
      JOCTETPTR(data)^ := next_input_byte^;
      Inc(JOCTETPTR(data));
      Inc(next_input_byte);
      Dec(bytes_in_buffer);
      Inc(bytes_read);
    end;
  end;

  { Done reading what we want to read }
  if (cur_marker <> NIL) then
  begin	{ will be NIL if bogus length word }
    { Add new marker to end of list }
    if (cinfo^.marker_list = NIL) then
    begin
      cinfo^.marker_list := cur_marker
    end
    else
    begin
      prev := cinfo^.marker_list;
      while (prev^.next <> NIL) do
	prev := prev^.next;
      prev^.next := cur_marker;
    end;
    { Reset pointer & calc remaining data length }
    data := cur_marker^.data;
    length := cur_marker^.original_length - data_length;
  end;
  { Reset to initial state for next marker }
  marker^.cur_marker := NIL;

  { Process the marker if interesting; else just make a generic trace msg }
  case (cinfo^.unread_marker) of
  M_APP0:
    examine_app0(cinfo, data^, data_length, length);
  M_APP14:
    examine_app14(cinfo, data^, data_length, length);
  else
    {$IFDEF DEBUG}
    TRACEMS2(j_common_ptr(cinfo), 1, JTRC_MISC_MARKER, cinfo^.unread_marker,
	     int(data_length + length));
    {$ENDIF}
  end;

  { skip any remaining data -- could be lots }
  { do before skip_input_data }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  if (length > 0) then
    cinfo^.src^.skip_input_data (cinfo, long(length) );

  save_marker := TRUE;
end;

{$endif} { SAVE_MARKERS_SUPPORTED }


{ Find the next JPEG marker, save it in cinfo^.unread_marker.
  Returns FALSE if had to suspend before reaching a marker;
  in that case cinfo^.unread_marker is unchanged.

  Note that the result might not be a valid marker code,
  but it will never be 0 or FF. }

{LOCAL}
function next_marker (cinfo : j_decompress_ptr) : boolean;
var
  c : int;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

  {while TRUE do}
  repeat
    { Read a byte into variable c. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        next_marker := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    c := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

   { Skip any non-FF bytes.
     This may look a bit inefficient, but it will not occur in a valid file.
     We sync after each discarded byte so that a suspending data source
     can discard the byte from its buffer. }

    while (c <> $FF) do
    begin
      Inc(cinfo^.marker^.discarded_bytes);
      { Unload the local copies --- do this only at a restart boundary }
      datasrc^.next_input_byte := next_input_byte;
      datasrc^.bytes_in_buffer := bytes_in_buffer;

      { Read a byte into variable c. If must suspend, return FALSE. }
      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          next_marker := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      c := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);

    end;
    { This loop swallows any duplicate FF bytes.  Extra FFs are legal as
      pad bytes, so don't count them in discarded_bytes.  We assume there
      will not be so many consecutive FF bytes as to overflow a suspending
      data source's input buffer. }

    repeat
      { Read a byte into variable c. If must suspend, return FALSE. }
      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          next_marker := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      c := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);
    Until (c <> $FF);
    if (c <> 0) then
      break;			{ found a valid marker, exit loop }
    { Reach here if we found a stuffed-zero data sequence (FF/00).
      Discard it and loop back to try again. }

    Inc(cinfo^.marker^.discarded_bytes, 2);
    { Unload the local copies --- do this only at a restart boundary }
    datasrc^.next_input_byte := next_input_byte;
    datasrc^.bytes_in_buffer := bytes_in_buffer;
  Until False;

  if (cinfo^.marker^.discarded_bytes <> 0) then
  begin
    WARNMS2(j_common_ptr(cinfo), JWRN_EXTRANEOUS_DATA,
            cinfo^.marker^.discarded_bytes, c);
    cinfo^.marker^.discarded_bytes := 0;
  end;

  cinfo^.unread_marker := c;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  next_marker := TRUE;
end;  { next_marker }


{LOCAL}
function first_marker (cinfo : j_decompress_ptr) : boolean;
{ Like next_marker, but used to obtain the initial SOI marker. }
{ For this marker, we do not allow preceding garbage or fill; otherwise,
  we might well scan an entire input file before realizing it ain't JPEG.
  If an application wants to process non-JFIF files, it must seek to the
  SOI before calling the JPEG library. }
var
  c, c2 : int;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

  { Read a byte into variable c. If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      first_marker := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  c := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  { Read a byte into variable c2. If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      first_marker := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  c2 := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  if (c <> $FF) or (c2 <> int(M_SOI)) then
    ERREXIT2(j_common_ptr(cinfo), JERR_NO_SOI, c, c2);

  cinfo^.unread_marker := c2;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  first_marker := TRUE;
end;  { first_marker }


{ Read markers until SOS or EOI.

  Returns same codes as are defined for jpeg_consume_input:
  JPEG_SUSPENDED, JPEG_REACHED_SOS, or JPEG_REACHED_EOI.   }

{METHODDEF}
function read_markers (cinfo : j_decompress_ptr) : int;
begin
  { Outer loop repeats once for each marker. }
  repeat
    { Collect the marker proper, unless we already did. }
    { NB: first_marker() enforces the requirement that SOI appear first. }
    if (cinfo^.unread_marker = 0) then
    begin
      if not cinfo^.marker^.saw_SOI then
      begin
        if not first_marker(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;
      end
      else
      begin
        if not next_marker(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;
      end;
    end;
    { At this point cinfo^.unread_marker contains the marker code and the
      input point is just past the marker proper, but before any parameters.
      A suspension will cause us to return with this state still true. }

    case (cinfo^.unread_marker) of
      M_SOI:
        if not get_soi(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_SOF0,             { Baseline }
      M_SOF1:             { Extended sequential, Huffman }
        if not get_sof(cinfo, FALSE, FALSE) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;
      M_SOF2:                     { Progressive, Huffman }
        if not get_sof(cinfo, TRUE, FALSE) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_SOF9:                     { Extended sequential, arithmetic }
        if not get_sof(cinfo, FALSE, TRUE) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_SOF10:                    { Progressive, arithmetic }
        if not get_sof(cinfo, TRUE, TRUE) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      { Currently unsupported SOFn types }
      M_SOF3,                     { Lossless, Huffman }
      M_SOF5,                     { Differential sequential, Huffman }
      M_SOF6,                     { Differential progressive, Huffman }
      M_SOF7,                     { Differential lossless, Huffman }
      M_JPG,                      { Reserved for JPEG extensions }
      M_SOF11,                    { Lossless, arithmetic }
      M_SOF13,                    { Differential sequential, arithmetic }
      M_SOF14,                    { Differential progressive, arithmetic }
      M_SOF15:                    { Differential lossless, arithmetic }
        ERREXIT1(j_common_ptr(cinfo), JERR_SOF_UNSUPPORTED, cinfo^.unread_marker);

      M_SOS:
        begin
          if not get_sos(cinfo) then
          begin
            read_markers := JPEG_SUSPENDED;
            exit;
          end;
          cinfo^.unread_marker := 0;       { processed the marker }
          read_markers := JPEG_REACHED_SOS;
          exit;
        end;

      M_EOI:
        begin
          {$IFDEF DEBUG}
          TRACEMS(j_common_ptr(cinfo), 1, JTRC_EOI);
          {$ENDIF}
          cinfo^.unread_marker := 0;       { processed the marker }
          read_markers := JPEG_REACHED_EOI;
          exit;
        end;

      M_DAC:
        if not get_dac(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_DHT:
        if not get_dht(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_DQT:
        if not get_dqt(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_DRI:
        if not get_dri(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_APP0,
      M_APP1,
      M_APP2,
      M_APP3,
      M_APP4,
      M_APP5,
      M_APP6,
      M_APP7,
      M_APP8,
      M_APP9,
      M_APP10,
      M_APP11,
      M_APP12,
      M_APP13,
      M_APP14,
      M_APP15:
        if not jdmarker_my_marker_ptr(cinfo^.marker)^.
                process_APPn[cinfo^.unread_marker - int(M_APP0)](cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_COM:
        if not jdmarker_my_marker_ptr(cinfo^.marker)^.process_COM (cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_RST0,		{ these are all parameterless }
      M_RST1,
      M_RST2,
      M_RST3,
      M_RST4,
      M_RST5,
      M_RST6,
      M_RST7,
      M_TEM:
        {$IFDEF DEBUG}
        TRACEMS1(j_common_ptr(cinfo), 1, JTRC_PARMLESS_MARKER,
          cinfo^.unread_marker)
        {$ENDIF}
        ;

      M_DNL:		{ Ignore DNL ... perhaps the wrong thing }
        if not skip_variable(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      else			{ must be DHP, EXP, JPGn, or RESn }
        { For now, we treat the reserved markers as fatal errors since they are
          likely to be used to signal incompatible JPEG Part 3 extensions.
          Once the JPEG 3 version-number marker is well defined, this code
          ought to change! }
        ERREXIT1(j_common_ptr(cinfo) , JERR_UNKNOWN_MARKER,
          cinfo^.unread_marker);
    end; { end of case }
    { Successfully processed marker, so reset state variable }
    cinfo^.unread_marker := 0;
  Until false;
end;  { read_markers }


{ Read a restart marker, which is expected to appear next in the datastream;
  if the marker is not there, take appropriate recovery action.
  Returns FALSE if suspension is required.

  This is called by the entropy decoder after it has read an appropriate
  number of MCUs.  cinfo^.unread_marker may be nonzero if the entropy decoder
  has already read a marker from the data source.  Under normal conditions
  cinfo^.unread_marker will be reset to 0 before returning; if not reset,
  it holds a marker which the decoder will be unable to read past. }

{METHODDEF}
function read_restart_marker (cinfo : j_decompress_ptr) :boolean;
begin
  { Obtain a marker unless we already did. }
  { Note that next_marker will complain if it skips any data. }
  if (cinfo^.unread_marker = 0) then
  begin
    if not next_marker(cinfo) then
    begin
      read_restart_marker := FALSE;
      exit;
    end;
  end;

  if (cinfo^.unread_marker = (int(M_RST0) + cinfo^.marker^.next_restart_num)) then
  begin
    { Normal case --- swallow the marker and let entropy decoder continue }
    {$IFDEF DEBUG}
    TRACEMS1(j_common_ptr(cinfo), 3, JTRC_RST,
      cinfo^.marker^.next_restart_num);
    {$ENDIF}
    cinfo^.unread_marker := 0;
  end
  else
  begin
    { Uh-oh, the restart markers have been messed up. }
    { Let the data source manager determine how to resync. }
    if not cinfo^.src^.resync_to_restart(cinfo,
              cinfo^.marker^.next_restart_num) then
    begin
      read_restart_marker := FALSE;
      exit;
    end;
  end;

  { Update next-restart state }
  with cinfo^.marker^ do
    next_restart_num := (next_restart_num + 1) and 7;

  read_restart_marker := TRUE;
end; { read_restart_marker }


{ This is the default resync_to_restart method for data source managers
  to use if they don't have any better approach.  Some data source managers
  may be able to back up, or may have additional knowledge about the data
  which permits a more intelligent recovery strategy; such managers would
  presumably supply their own resync method.

  read_restart_marker calls resync_to_restart if it finds a marker other than
  the restart marker it was expecting.  (This code is *not* used unless
  a nonzero restart interval has been declared.)  cinfo^.unread_marker is
  the marker code actually found (might be anything, except 0 or FF).
  The desired restart marker number (0..7) is passed as a parameter.
  This routine is supposed to apply whatever error recovery strategy seems
  appropriate in order to position the input stream to the next data segment.
  Note that cinfo^.unread_marker is treated as a marker appearing before
  the current data-source input point; usually it should be reset to zero
  before returning.
  Returns FALSE if suspension is required.

  This implementation is substantially constrained by wanting to treat the
  input as a data stream; this means we can't back up.  Therefore, we have
  only the following actions to work with:
    1. Simply discard the marker and let the entropy decoder resume at next
       byte of file.
    2. Read forward until we find another marker, discarding intervening
       data.  (In theory we could look ahead within the current bufferload,
       without having to discard data if we don't find the desired marker.
       This idea is not implemented here, in part because it makes behavior
       dependent on buffer size and chance buffer-boundary positions.)
    3. Leave the marker unread (by failing to zero cinfo^.unread_marker).
       This will cause the entropy decoder to process an empty data segment,
       inserting dummy zeroes, and then we will reprocess the marker.

  #2 is appropriate if we think the desired marker lies ahead, while #3 is
  appropriate if the found marker is a future restart marker (indicating
  that we have missed the desired restart marker, probably because it got
  corrupted).
  We apply #2 or #3 if the found marker is a restart marker no more than
  two counts behind or ahead of the expected one.  We also apply #2 if the
  found marker is not a legal JPEG marker code (it's certainly bogus data).
  If the found marker is a restart marker more than 2 counts away, we do #1
  (too much risk that the marker is erroneous; with luck we will be able to
  resync at some future point).
  For any valid non-restart JPEG marker, we apply #3.  This keeps us from
  overrunning the end of a scan.  An implementation limited to single-scan
  files might find it better to apply #2 for markers other than EOI, since
  any other marker would have to be bogus data in that case. }


{GLOBAL}
function jpeg_resync_to_restart(cinfo : j_decompress_ptr;
                                desired : int) : boolean;
var
  marker : int;
  action : int;
begin
  marker := cinfo^.unread_marker;
  //action := 1;     { never used }
  { Always put up a warning. }
  WARNMS2(j_common_ptr(cinfo), JWRN_MUST_RESYNC, marker, desired);

  { Outer loop handles repeated decision after scanning forward. }
  repeat
    if (marker < int(M_SOF0)) then
      action := 2                { invalid marker }
    else
      if (marker < int(M_RST0)) or (marker > int(M_RST7)) then
        action := 3                { valid non-restart marker }
      else
      begin
        if (marker = (int(M_RST0) + ((desired+1) and 7))) or
           (marker = (int(M_RST0) + ((desired+2) and 7))) then
          action := 3              { one of the next two expected restarts }
        else
          if (marker = (int(M_RST0) + ((desired-1) and 7))) or
             (marker = (int(M_RST0) + ((desired-2) and 7))) then
            action := 2            { a prior restart, so advance }
          else
            action := 1;           { desired restart or too far away }
      end;

    {$IFDEF DEBUG}
    TRACEMS2(j_common_ptr(cinfo), 4, JTRC_RECOVERY_ACTION, marker, action);
    {$ENDIF}
    case action of
    1:
      { Discard marker and let entropy decoder resume processing. }
      begin
        cinfo^.unread_marker := 0;
        jpeg_resync_to_restart := TRUE;
        exit;
      end;
    2:
      { Scan to the next marker, and repeat the decision loop. }
      begin
        if not next_marker(cinfo) then
        begin
          jpeg_resync_to_restart := FALSE;
          exit;
        end;
        marker := cinfo^.unread_marker;
      end;
    3:
      { Return without advancing past this marker. }
      { Entropy decoder will be forced to process an empty segment. }
      begin
        jpeg_resync_to_restart := TRUE;
        exit;
      end;
    end; { case }
  Until false; { end loop }
end;  { jpeg_resync_to_restart }


{ Reset marker processing state to begin a fresh datastream. }

{METHODDEF}
procedure reset_marker_reader (cinfo : j_decompress_ptr);
var
  marker : jdmarker_my_marker_ptr;
begin
  marker := jdmarker_my_marker_ptr (cinfo^.marker);
  with cinfo^ do
  begin
    comp_info := NIL;            { until allocated by get_sof }
    input_scan_number := 0;      { no SOS seen yet }
    unread_marker := 0;          { no pending marker }
  end;
  marker^.pub.saw_SOI := FALSE;    { set internal state too }
  marker^.pub.saw_SOF := FALSE;
  marker^.pub.discarded_bytes := 0;
  marker^.cur_marker := NIL;
end; { reset_marker_reader }


{ Initialize the marker reader module.
  This is called only once, when the decompression object is created. }

{GLOBAL}
procedure jinit_marker_reader (cinfo : j_decompress_ptr);
var
  marker : jdmarker_my_marker_ptr;
  i : int;
begin
  { Create subobject in permanent pool }
  marker := jdmarker_my_marker_ptr(
     cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
                             SIZEOF(my_marker_reader))
                                    );
  cinfo^.marker := jpeg_marker_reader_ptr(marker);
  { Initialize method pointers }
  marker^.pub.reset_marker_reader := reset_marker_reader;
  marker^.pub.read_markers := read_markers;
  marker^.pub.read_restart_marker := read_restart_marker;
  { Initialize COM/APPn processing.
    By default, we examine and then discard APP0 and APP14,
    but simply discard COM and all other APPn. }

  marker^.process_COM := skip_variable;
  marker^.length_limit_COM := 0;
  for i := 0 to 16-1 do
  begin
    marker^.process_APPn[i] := skip_variable;
    marker^.length_limit_APPn[i] := 0;
  end;
  marker^.process_APPn[0] := get_interesting_appn;
  marker^.process_APPn[14] := get_interesting_appn;
  { Reset marker processing state }
  reset_marker_reader(cinfo);
end; { jinit_marker_reader }


{ Control saving of COM and APPn markers into marker_list. }


{$ifdef SAVE_MARKERS_SUPPORTED}

{GLOBAL}
procedure jpeg_save_markers (cinfo : j_decompress_ptr;
                             marker_code : int;
		             length_limit : uint);
var
  marker : jdmarker_my_marker_ptr;
  maxlength : long;
  processor : jpeg_marker_parser_method;
begin
  marker := jdmarker_my_marker_ptr (cinfo^.marker);

  { Length limit mustn't be larger than what we can allocate
    (should only be a concern in a 16-bit environment). }

  maxlength := cinfo^.mem^.max_alloc_chunk - SIZEOF(jpeg_marker_struct);
  if (long(length_limit) > maxlength) then
    length_limit := uint(maxlength);

  { Choose processor routine to use.
    APP0/APP14 have special requirements. }

  if (length_limit <> 0) then
  begin
    processor := save_marker;
    { If saving APP0/APP14, save at least enough for our internal use. }
    if (marker_code = int(M_APP0)) and (length_limit < APP0_DATA_LEN) then
      length_limit := APP0_DATA_LEN
    else
      if (marker_code = int(M_APP14)) and (length_limit < APP14_DATA_LEN) then
        length_limit := APP14_DATA_LEN;
  end
  else
  begin
    processor := skip_variable;
    { If discarding APP0/APP14, use our regular on-the-fly processor. }
    if (marker_code = int(M_APP0)) or (marker_code = int(M_APP14)) then
      processor := get_interesting_appn;
  end;

  if (marker_code = int(M_COM)) then
  begin
    marker^.process_COM := processor;
    marker^.length_limit_COM := length_limit;
  end
  else
    if (marker_code >= int(M_APP0)) and (marker_code <= int(M_APP15)) then
    begin
      marker^.process_APPn[marker_code - int(M_APP0)] := processor;
      marker^.length_limit_APPn[marker_code - int(M_APP0)] := length_limit;
    end
    else
      ERREXIT1(j_common_ptr(cinfo), JERR_UNKNOWN_MARKER, marker_code);
end;

{$endif} { SAVE_MARKERS_SUPPORTED }

{ Install a special processing method for COM or APPn markers. }

{GLOBAL}

procedure jpeg_set_marker_processor (cinfo : j_decompress_ptr;
                                     marker_code : int;
			             routine : jpeg_marker_parser_method);
var
  marker : jdmarker_my_marker_ptr;
begin
  marker := jdmarker_my_marker_ptr (cinfo^.marker);
  if (marker_code = int(M_COM)) then
    marker^.process_COM := routine
  else
    if (marker_code >= int(M_APP0)) and (marker_code <= int(M_APP15)) then
      marker^.process_APPn[marker_code - int(M_APP0)] := routine
    else
      ERREXIT1(j_common_ptr(cinfo), JERR_UNKNOWN_MARKER, marker_code);
end;

{== imjdinput implementation ==}

{ Private state }

type
  my_inputctl_ptr = ^my_input_controller;
  my_input_controller = record
    pub : jpeg_input_controller; { public fields }

    inheaders : boolean;		{ TRUE until first SOS is reached }
  end; {my_input_controller;}


{ Forward declarations }
{METHODDEF}
function consume_markers (cinfo : j_decompress_ptr) : int; forward;


{ Routines to calculate various quantities related to the size of the image. }

{LOCAL}
procedure jdinput_initial_setup (cinfo : j_decompress_ptr);
{ Called once, when first SOS marker is reached }
var
  ci : int;
  compptr : jpeg_component_info_ptr;
begin
  { Make sure image isn't bigger than I can handle }
  if (long(cinfo^.image_height) > long (JPEG_MAX_DIMENSION)) or
     (long(cinfo^.image_width) > long(JPEG_MAX_DIMENSION)) then
    ERREXIT1(j_common_ptr(cinfo), JERR_IMAGE_TOO_BIG, uInt(JPEG_MAX_DIMENSION));

  { For now, precision must match compiled-in value... }
  if (cinfo^.data_precision <> BITS_IN_JSAMPLE) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PRECISION, cinfo^.data_precision);

  { Check that number of components won't exceed internal array sizes }
  if (cinfo^.num_components > MAX_COMPONENTS) then
    ERREXIT2(j_common_ptr(cinfo), JERR_COMPONENT_COUNT, cinfo^.num_components,
	     MAX_COMPONENTS);

  { Compute maximum sampling factors; check factor validity }
  cinfo^.max_h_samp_factor := 1;
  cinfo^.max_v_samp_factor := 1;
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    if (compptr^.h_samp_factor<=0) or (compptr^.h_samp_factor>MAX_SAMP_FACTOR) or
       (compptr^.v_samp_factor<=0) or (compptr^.v_samp_factor>MAX_SAMP_FACTOR) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_SAMPLING);
    {cinfo^.max_h_samp_factor := MAX(cinfo^.max_h_samp_factor,
				   compptr^.h_samp_factor);
    cinfo^.max_v_samp_factor := MAX(cinfo^.max_v_samp_factor,
				   compptr^.v_samp_factor);}
    if cinfo^.max_h_samp_factor < compptr^.h_samp_factor then
      cinfo^.max_h_samp_factor := compptr^.h_samp_factor;
    if cinfo^.max_v_samp_factor < compptr^.v_samp_factor then
      cinfo^.max_v_samp_factor := compptr^.v_samp_factor;
    Inc(compptr);
  end;

  { We initialize DCT_scaled_size and min_DCT_scaled_size to DCTSIZE.
    In the full decompressor, this will be overridden by jdmaster.c;
    but in the transcoder, jdmaster.c is not used, so we must do it here. }

  cinfo^.min_DCT_scaled_size := DCTSIZE;

  { Compute dimensions of components }
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    compptr^.DCT_scaled_size := DCTSIZE;
    { Size in DCT blocks }
    compptr^.width_in_blocks := JDIMENSION(
      jdiv_round_up( long(cinfo^.image_width) * long(compptr^.h_samp_factor),
		     long(cinfo^.max_h_samp_factor * DCTSIZE)) );
    compptr^.height_in_blocks := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_height) * long(compptr^.v_samp_factor),
		    long (cinfo^.max_v_samp_factor * DCTSIZE)) );
    { downsampled_width and downsampled_height will also be overridden by
      jdmaster.c if we are doing full decompression.  The transcoder library
      doesn't use these values, but the calling application might. }

    { Size in samples }
    compptr^.downsampled_width := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_width) * long(compptr^.h_samp_factor),
		    long (cinfo^.max_h_samp_factor)) );
    compptr^.downsampled_height := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_height) * long(compptr^.v_samp_factor),
		    long (cinfo^.max_v_samp_factor)) );
    { Mark component needed, until color conversion says otherwise }
    compptr^.component_needed := TRUE;
    { Mark no quantization table yet saved for component }
    compptr^.quant_table := NIL;
    Inc(compptr);
  end;

  { Compute number of fully interleaved MCU rows. }
  cinfo^.total_iMCU_rows := JDIMENSION(
    jdiv_round_up(long(cinfo^.image_height),
		  long(cinfo^.max_v_samp_factor*DCTSIZE)) );

  { Decide whether file contains multiple scans }
  if (cinfo^.comps_in_scan < cinfo^.num_components) or
     (cinfo^.progressive_mode) then
    cinfo^.inputctl^.has_multiple_scans := TRUE
  else
    cinfo^.inputctl^.has_multiple_scans := FALSE;
end;


{LOCAL}
procedure jdinput_per_scan_setup (cinfo : j_decompress_ptr);
{ Do computations that are needed before processing a JPEG scan }
{ cinfo^.comps_in_scan and cinfo^.cur_comp_info[] were set from SOS marker }
var
  ci, mcublks, tmp : int;
  compptr : jpeg_component_info_ptr;
begin
  if (cinfo^.comps_in_scan = 1) then
  begin
    { Noninterleaved (single-component) scan }
    compptr := cinfo^.cur_comp_info[0];

    { Overall image size in MCUs }
    cinfo^.MCUs_per_row := compptr^.width_in_blocks;
    cinfo^.MCU_rows_in_scan := compptr^.height_in_blocks;

    { For noninterleaved scan, always one block per MCU }
    compptr^.MCU_width := 1;
    compptr^.MCU_height := 1;
    compptr^.MCU_blocks := 1;
    compptr^.MCU_sample_width := compptr^.DCT_scaled_size;
    compptr^.last_col_width := 1;
    { For noninterleaved scans, it is convenient to define last_row_height
      as the number of block rows present in the last iMCU row. }

    tmp := int (LongInt(compptr^.height_in_blocks) mod compptr^.v_samp_factor);
    if (tmp = 0) then
      tmp := compptr^.v_samp_factor;
    compptr^.last_row_height := tmp;

    { Prepare array describing MCU composition }
    cinfo^.blocks_in_MCU := 1;
    cinfo^.MCU_membership[0] := 0;

  end
  else
  begin

    { Interleaved (multi-component) scan }
    if (cinfo^.comps_in_scan <= 0) or (cinfo^.comps_in_scan > MAX_COMPS_IN_SCAN) then
      ERREXIT2(j_common_ptr(cinfo), JERR_COMPONENT_COUNT, cinfo^.comps_in_scan,
	       MAX_COMPS_IN_SCAN);

    { Overall image size in MCUs }
    cinfo^.MCUs_per_row := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_width),
		    long (cinfo^.max_h_samp_factor*DCTSIZE)) );
    cinfo^.MCU_rows_in_scan := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_height),
		    long (cinfo^.max_v_samp_factor*DCTSIZE)) );

    cinfo^.blocks_in_MCU := 0;

    for ci := 0 to pred(cinfo^.comps_in_scan) do
    begin
      compptr := cinfo^.cur_comp_info[ci];
      { Sampling factors give # of blocks of component in each MCU }
      compptr^.MCU_width := compptr^.h_samp_factor;
      compptr^.MCU_height := compptr^.v_samp_factor;
      compptr^.MCU_blocks := compptr^.MCU_width * compptr^.MCU_height;
      compptr^.MCU_sample_width := compptr^.MCU_width * compptr^.DCT_scaled_size;
      { Figure number of non-dummy blocks in last MCU column & row }
      tmp := int (LongInt(compptr^.width_in_blocks) mod compptr^.MCU_width);
      if (tmp = 0) then
        tmp := compptr^.MCU_width;
      compptr^.last_col_width := tmp;
      tmp := int (LongInt(compptr^.height_in_blocks) mod compptr^.MCU_height);
      if (tmp = 0) then
        tmp := compptr^.MCU_height;
      compptr^.last_row_height := tmp;
      { Prepare array describing MCU composition }
      mcublks := compptr^.MCU_blocks;
      if (LongInt(cinfo^.blocks_in_MCU) + mcublks > D_MAX_BLOCKS_IN_MCU) then
	ERREXIT(j_common_ptr(cinfo), JERR_BAD_MCU_SIZE);
      while (mcublks > 0) do
      begin
        Dec(mcublks);
	cinfo^.MCU_membership[cinfo^.blocks_in_MCU] := ci;
        Inc(cinfo^.blocks_in_MCU);
      end;
    end;

  end;
end;


{ Save away a copy of the Q-table referenced by each component present
  in the current scan, unless already saved during a prior scan.

  In a multiple-scan JPEG file, the encoder could assign different components
  the same Q-table slot number, but change table definitions between scans
  so that each component uses a different Q-table.  (The IJG encoder is not
  currently capable of doing this, but other encoders might.)  Since we want
  to be able to dequantize all the components at the end of the file, this
  means that we have to save away the table actually used for each component.
  We do this by copying the table at the start of the first scan containing
  the component.
  The JPEG spec prohibits the encoder from changing the contents of a Q-table
  slot between scans of a component using that slot.  If the encoder does so
  anyway, this decoder will simply use the Q-table values that were current
  at the start of the first scan for the component.

  The decompressor output side looks only at the saved quant tables,
  not at the current Q-table slots. }

{LOCAL}
procedure latch_quant_tables (cinfo : j_decompress_ptr);
var
  ci, qtblno : int;
  compptr : jpeg_component_info_ptr;
  qtbl : JQUANT_TBL_PTR;
begin
  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    { No work if we already saved Q-table for this component }
    if (compptr^.quant_table <> NIL) then
      continue;
    { Make sure specified quantization table is present }
    qtblno := compptr^.quant_tbl_no;
    if (qtblno < 0) or (qtblno >= NUM_QUANT_TBLS) or
       (cinfo^.quant_tbl_ptrs[qtblno] = NIL) then
      ERREXIT1(j_common_ptr(cinfo), JERR_NO_QUANT_TABLE, qtblno);
    { OK, save away the quantization table }
    qtbl := JQUANT_TBL_PTR(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				  SIZEOF(JQUANT_TBL)) );
    MEMCOPY(qtbl, cinfo^.quant_tbl_ptrs[qtblno], SIZEOF(JQUANT_TBL));
    compptr^.quant_table := qtbl;
  end;
end;


{ Initialize the input modules to read a scan of compressed data.
  The first call to this is done by jdmaster.c after initializing
  the entire decompressor (during jpeg_start_decompress).
  Subsequent calls come from consume_markers, below. }

{METHODDEF}
procedure jdinput_start_input_pass (cinfo : j_decompress_ptr);
begin
  jdinput_per_scan_setup(cinfo);
  latch_quant_tables(cinfo);
  cinfo^.entropy^.start_pass (cinfo);
  cinfo^.coef^.start_input_pass (cinfo);
  cinfo^.inputctl^.consume_input := cinfo^.coef^.consume_data;
end;


{ Finish up after inputting a compressed-data scan.
  This is called by the coefficient controller after it's read all
  the expected data of the scan. }

{METHODDEF}
procedure finish_input_pass (cinfo : j_decompress_ptr);
begin
  cinfo^.inputctl^.consume_input := consume_markers;
end;


{ Read JPEG markers before, between, or after compressed-data scans.
  Change state as necessary when a new scan is reached.
  Return value is JPEG_SUSPENDED, JPEG_REACHED_SOS, or JPEG_REACHED_EOI.

  The consume_input method pointer points either here or to the
  coefficient controller's consume_data routine, depending on whether
  we are reading a compressed data segment or inter-segment markers. }

{METHODDEF}
function consume_markers (cinfo : j_decompress_ptr) : int;
var
  val : int;
  inputctl : my_inputctl_ptr;
begin
  inputctl := my_inputctl_ptr (cinfo^.inputctl);

  if (inputctl^.pub.eoi_reached) then { After hitting EOI, read no further }
  begin
    consume_markers := JPEG_REACHED_EOI;
    exit;
  end;

  val := cinfo^.marker^.read_markers (cinfo);

  case (val) of
  JPEG_REACHED_SOS:	{ Found SOS }
    begin
      if (inputctl^.inheaders) then
      begin	{ 1st SOS }
        jdinput_initial_setup(cinfo);
        inputctl^.inheaders := FALSE;
        { Note: start_input_pass must be called by jdmaster.c
          before any more input can be consumed.  jdapimin.c is
          responsible for enforcing this sequencing. }
      end
      else
      begin			{ 2nd or later SOS marker }
        if (not inputctl^.pub.has_multiple_scans) then
	  ERREXIT(j_common_ptr(cinfo), JERR_EOI_EXPECTED); { Oops, I wasn't expecting this! }
        jdinput_start_input_pass(cinfo);
      end;
    end;
  JPEG_REACHED_EOI:	{ Found EOI }
    begin
      inputctl^.pub.eoi_reached := TRUE;
      if (inputctl^.inheaders) then
      begin	{ Tables-only datastream, apparently }
        if (cinfo^.marker^.saw_SOF) then
	  ERREXIT(j_common_ptr(cinfo), JERR_SOF_NO_SOS);
      end
      else
      begin
        { Prevent infinite loop in coef ctlr's decompress_data routine
          if user set output_scan_number larger than number of scans. }

        if (cinfo^.output_scan_number > cinfo^.input_scan_number) then
	  cinfo^.output_scan_number := cinfo^.input_scan_number;
      end;
    end;
  JPEG_SUSPENDED:;
  end;

  consume_markers := val;
end;


{ Reset state to begin a fresh datastream. }

{METHODDEF}
procedure reset_input_controller (cinfo : j_decompress_ptr);
var
  inputctl : my_inputctl_ptr;
begin
  inputctl := my_inputctl_ptr (cinfo^.inputctl);

  inputctl^.pub.consume_input := consume_markers;
  inputctl^.pub.has_multiple_scans := FALSE; { "unknown" would be better }
  inputctl^.pub.eoi_reached := FALSE;
  inputctl^.inheaders := TRUE;
  { Reset other modules }
  cinfo^.err^.reset_error_mgr (j_common_ptr(cinfo));
  cinfo^.marker^.reset_marker_reader (cinfo);
  { Reset progression state -- would be cleaner if entropy decoder did this }
  cinfo^.coef_bits := NIL;
end;


{ Initialize the input controller module.
  This is called only once, when the decompression object is created. }

{GLOBAL}
procedure jinit_input_controller (cinfo : j_decompress_ptr);
var
  inputctl : my_inputctl_ptr;
begin
  { Create subobject in permanent pool }
  inputctl := my_inputctl_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
				SIZEOF(my_input_controller)) );
  cinfo^.inputctl := jpeg_input_controller_ptr(inputctl);
  { Initialize method pointers }
  inputctl^.pub.consume_input := consume_markers;
  inputctl^.pub.reset_input_controller := reset_input_controller;
  inputctl^.pub.start_input_pass := jdinput_start_input_pass;
  inputctl^.pub.finish_input_pass := finish_input_pass;
  { Initialize state: can't use reset_input_controller since we don't
    want to try to reset other modules yet. }

  inputctl^.pub.has_multiple_scans := FALSE; { "unknown" would be better }
  inputctl^.pub.eoi_reached := FALSE;
  inputctl^.inheaders := TRUE;
end;

{== imjdhuff implementation ==}


{ Expanded entropy decoder object for Huffman decoding.

  The savable_state subrecord contains fields that change within an MCU,
  but must not be updated permanently until we complete the MCU. }

type
  jdhuff_savable_state = record
    last_dc_val : array[0..MAX_COMPS_IN_SCAN-1] of int; { last DC coef for each component }
  end;


type
  jdhuff_huff_entropy_ptr = ^huff_entropy_decoder;
  huff_entropy_decoder = record
    pub : jpeg_entropy_decoder; { public fields }

    { These fields are loaded into local variables at start of each MCU.
      In case of suspension, we exit WITHOUT updating them. }

    bitstate : bitread_perm_state;	{ Bit buffer at start of MCU }
    saved : jdhuff_savable_state;		{ Other state at start of MCU }

    { These fields are NOT loaded into local working state. }
    restarts_to_go : uInt;              { MCUs left in this restart interval }

    { Pointers to derived tables (these workspaces have image lifespan) }
    dc_derived_tbls : array[0..NUM_HUFF_TBLS] of d_derived_tbl_ptr;
    ac_derived_tbls : array[0..NUM_HUFF_TBLS] of d_derived_tbl_ptr;

    { Precalculated info set up by start_pass for use in decode_mcu: }

    { Pointers to derived tables to be used for each block within an MCU }
    dc_cur_tbls : array[0..D_MAX_BLOCKS_IN_MCU-1] of d_derived_tbl_ptr;
    ac_cur_tbls : array[0..D_MAX_BLOCKS_IN_MCU-1] of d_derived_tbl_ptr;
    { Whether we care about the DC and AC coefficient values for each block }
    dc_needed : array[0..D_MAX_BLOCKS_IN_MCU-1] of boolean;
    ac_needed : array[0..D_MAX_BLOCKS_IN_MCU-1] of boolean;
  end;


{ Initialize for a Huffman-compressed scan. }

{METHODDEF}
procedure start_pass_huff_decoder (cinfo : j_decompress_ptr);
var
  entropy : jdhuff_huff_entropy_ptr;
  ci, blkn, dctbl, actbl : int;
  compptr : jpeg_component_info_ptr;
begin
  entropy := jdhuff_huff_entropy_ptr (cinfo^.entropy);

  { Check that the scan parameters Ss, Se, Ah/Al are OK for sequential JPEG.
    This ought to be an error condition, but we make it a warning because
    there are some baseline files out there with all zeroes in these bytes. }

  if (cinfo^.Ss <> 0) or (cinfo^.Se <> DCTSIZE2-1) or
     (cinfo^.Ah <> 0) or (cinfo^.Al <> 0) then
    WARNMS(j_common_ptr(cinfo), JWRN_NOT_SEQUENTIAL);

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    dctbl := compptr^.dc_tbl_no;
    actbl := compptr^.ac_tbl_no;
    { Compute derived values for Huffman tables }
    { We may do this more than once for a table, but it's not expensive }
    jpeg_make_d_derived_tbl(cinfo, TRUE, dctbl,
			    entropy^.dc_derived_tbls[dctbl]);
    jpeg_make_d_derived_tbl(cinfo, FALSE, actbl,
			    entropy^.ac_derived_tbls[actbl]);
    { Initialize DC predictions to 0 }
    entropy^.saved.last_dc_val[ci] := 0;
  end;

  { Precalculate decoding info for each block in an MCU of this scan }
  for blkn := 0 to pred(cinfo^.blocks_in_MCU) do
  begin
    ci := cinfo^.MCU_membership[blkn];
    compptr := cinfo^.cur_comp_info[ci];
    { Precalculate which table to use for each block }
    entropy^.dc_cur_tbls[blkn] := entropy^.dc_derived_tbls[compptr^.dc_tbl_no];
    entropy^.ac_cur_tbls[blkn] := entropy^.ac_derived_tbls[compptr^.ac_tbl_no];
    { Decide whether we really care about the coefficient values }
    if (compptr^.component_needed) then
    begin
      entropy^.dc_needed[blkn] := TRUE;
      { we don't need the ACs if producing a 1/8th-size image }
      entropy^.ac_needed[blkn] := (compptr^.DCT_scaled_size > 1);
    end
    else
    begin
      entropy^.ac_needed[blkn] := FALSE;
      entropy^.dc_needed[blkn] := FALSE;
    end;
  end;

  { Initialize bitread state variables }
  entropy^.bitstate.bits_left := 0;
  entropy^.bitstate.get_buffer := 0; { unnecessary, but keeps Purify quiet }
  entropy^.pub.insufficient_data := FALSE;

  { Initialize restart counter }
  entropy^.restarts_to_go := cinfo^.restart_interval;
end;


{ Compute the derived values for a Huffman table.
  This routine also performs some validation checks on the table.

  Note this is also used by jdphuff.c. }

{GLOBAL}
procedure jpeg_make_d_derived_tbl (cinfo : j_decompress_ptr;
                                   isDC : boolean;
                                   tblno : int;
	                           var pdtbl : d_derived_tbl_ptr);
var
  htbl : JHUFF_TBL_PTR;
  dtbl : d_derived_tbl_ptr;
  p, i, l, si, numsymbols : int;
  lookbits, ctr : int;
  huffsize : array[0..257-1] of byte;
  huffcode : array[0..257-1] of uInt;
  code : uInt;
var
  sym : int;
begin
  { Note that huffsize[] and huffcode[] are filled in code-length order,
    paralleling the order of the symbols themselves in htbl^.huffval[]. }

  { Find the input Huffman table }
  if (tblno < 0) or (tblno >= NUM_HUFF_TBLS) then
    ERREXIT1(j_common_ptr(cinfo), JERR_NO_HUFF_TABLE, tblno);
  if isDC then
    htbl := cinfo^.dc_huff_tbl_ptrs[tblno]
  else
    htbl := cinfo^.ac_huff_tbl_ptrs[tblno];
  if (htbl = NIL) then
    ERREXIT1(j_common_ptr(cinfo), JERR_NO_HUFF_TABLE, tblno);

  { Allocate a workspace if we haven't already done so. }
  if (pdtbl = NIL) then
    pdtbl := d_derived_tbl_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				  SIZEOF(d_derived_tbl)) );
  dtbl := pdtbl;
  dtbl^.pub := htbl;		{ fill in back link }

  { Figure C.1: make table of Huffman code length for each symbol }

  p := 0;
  for l := 1 to 16 do
  begin
    i := int(htbl^.bits[l]);
    if (i < 0) or (p + i > 256) then  { protect against table overrun }
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_HUFF_TABLE);
    while (i > 0) do
    begin
      huffsize[p] := byte(l);
      Inc(p);
      Dec(i);
    end;
  end;
  huffsize[p] := 0;
  numsymbols := p;

  { Figure C.2: generate the codes themselves }
  { We also validate that the counts represent a legal Huffman code tree. }

  code := 0;
  si := huffsize[0];
  p := 0;
  while (huffsize[p] <> 0) do
  begin
    while (( int (huffsize[p]) ) = si) do
    begin
      huffcode[p] := code;
      Inc(p);
      Inc(code);
    end;
    { code is now 1 more than the last code used for codelength si; but
      it must still fit in si bits, since no code is allowed to be all ones. }

    if (INT32(code) >= (INT32(1) shl si)) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_HUFF_TABLE);

    code := code shl 1;
    Inc(si);
  end;

  { Figure F.15: generate decoding tables for bit-sequential decoding }

  p := 0;
  for l := 1 to 16 do
  begin
    if (htbl^.bits[l] <> 0) then
    begin
      { valoffset[l] = huffval[] index of 1st symbol of code length l,
        minus the minimum code of length l }

      dtbl^.valoffset[l] := INT32(p) - INT32(huffcode[p]);
      Inc(p, htbl^.bits[l]);
      dtbl^.maxcode[l] := huffcode[p-1]; { maximum code of length l }
    end
    else
    begin
      dtbl^.maxcode[l] := -1;	{ -1 if no codes of this length }
    end;
  end;
  dtbl^.maxcode[17] := long($FFFFF); { ensures jpeg_huff_decode terminates }

  { Compute lookahead tables to speed up decoding.
    First we set all the table entries to 0, indicating "too long";
    then we iterate through the Huffman codes that are short enough and
    fill in all the entries that correspond to bit sequences starting
    with that code. }

  MEMZERO(@dtbl^.look_nbits, SIZEOF(dtbl^.look_nbits));

  p := 0;
  for l := 1 to HUFF_LOOKAHEAD do
  begin
    for i := 1 to int (htbl^.bits[l]) do
    begin
      { l := current code's length, p := its index in huffcode[] & huffval[]. }
      { Generate left-justified code followed by all possible bit sequences }
      lookbits := huffcode[p] shl (HUFF_LOOKAHEAD-l);
      for ctr := pred(1 shl (HUFF_LOOKAHEAD-l)) downto 0 do
      begin
	dtbl^.look_nbits[lookbits] := l;
	dtbl^.look_sym[lookbits] := htbl^.huffval[p];
	Inc(lookbits);
      end;
      Inc(p);
    end;
  end;

  { Validate symbols as being reasonable.
    For AC tables, we make no check, but accept all byte values 0..255.
    For DC tables, we require the symbols to be in range 0..15.
    (Tighter bounds could be applied depending on the data depth and mode,
    but this is sufficient to ensure safe decoding.) }

  if (isDC) then
  begin
    for i := 0 to pred(numsymbols) do
    begin
      sym := htbl^.huffval[i];
      if (sym < 0) or (sym > 15) then
	ERREXIT(j_common_ptr(cinfo), JERR_BAD_HUFF_TABLE);
    end;
  end;
end;


{ Out-of-line code for bit fetching (shared with jdphuff.c).
  See jdhuff.h for info about usage.
  Note: current values of get_buffer and bits_left are passed as parameters,
  but are returned in the corresponding fields of the state struct.

  On most machines MIN_GET_BITS should be 25 to allow the full 32-bit width
  of get_buffer to be used.  (On machines with wider words, an even larger
  buffer could be used.)  However, on some machines 32-bit shifts are
  quite slow and take time proportional to the number of places shifted.
  (This is true with most PC compilers, for instance.)  In this case it may
  be a win to set MIN_GET_BITS to the minimum value of 15.  This reduces the
  average shift distance at the cost of more calls to jpeg_fill_bit_buffer. }

{$ifdef SLOW_SHIFT_32}
const
  MIN_GET_BITS = 15;	{ minimum allowable value }
{$else}
const
  MIN_GET_BITS = (BIT_BUF_SIZE-7);
{$endif}


{GLOBAL}
function jpeg_fill_bit_buffer (var state : bitread_working_state;
		              {register} get_buffer : bit_buf_type;
                              {register} bits_left : int;
		              nbits  : int) : boolean;
label
  no_more_bytes;
{ Load up the bit buffer to a depth of at least nbits }
var
  { Copy heavily used state fields into locals (hopefully registers) }
  {register} next_input_byte : {const} JOCTETptr;
  {register} bytes_in_buffer : size_t;
var
  {register} c : int;
var
  cinfo : j_decompress_ptr;
begin
  next_input_byte := state.next_input_byte;
  bytes_in_buffer := state.bytes_in_buffer;
  cinfo := state.cinfo;

  { Attempt to load at least MIN_GET_BITS bits into get_buffer. }
  { (It is assumed that no request will be for more than that many bits.) }
  { We fail to do so only if we hit a marker or are forced to suspend. }

  if (cinfo^.unread_marker = 0) then	{ cannot advance past a marker }
  begin
    while (bits_left < MIN_GET_BITS) do
    begin
      { Attempt to read a byte }
      if (bytes_in_buffer = 0) then
      begin
	if not cinfo^.src^.fill_input_buffer(cinfo) then
        begin
	  jpeg_fill_bit_buffer := FALSE;
          exit;
        end;
	next_input_byte := cinfo^.src^.next_input_byte;
	bytes_in_buffer := cinfo^.src^.bytes_in_buffer;
      end;
      Dec(bytes_in_buffer);
      c := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);


      { If it's $FF, check and discard stuffed zero byte }
      if (c = $FF) then
      begin
        { Loop here to discard any padding FF's on terminating marker,
	  so that we can save a valid unread_marker value.  NOTE: we will
	  accept multiple FF's followed by a 0 as meaning a single FF data
	  byte.  This data pattern is not valid according to the standard. }

        repeat
	  if (bytes_in_buffer = 0) then
          begin
	    if (not state.cinfo^.src^.fill_input_buffer (state.cinfo)) then
            begin
	      jpeg_fill_bit_buffer := FALSE;
              exit;
            end;
	    next_input_byte := state.cinfo^.src^.next_input_byte;
	    bytes_in_buffer := state.cinfo^.src^.bytes_in_buffer;
	  end;
	  Dec(bytes_in_buffer);
	  c := GETJOCTET(next_input_byte^);
          Inc(next_input_byte);
        Until (c <> $FF);

        if (c = 0) then
        begin
	  { Found FF/00, which represents an FF data byte }
	  c := $FF;
        end
        else
        begin
	  { Oops, it's actually a marker indicating end of compressed data.
            Save the marker code for later use.
	    Fine point: it might appear that we should save the marker into
	    bitread working state, not straight into permanent state.  But
	    once we have hit a marker, we cannot need to suspend within the
	    current MCU, because we will read no more bytes from the data
	    source.  So it is OK to update permanent state right away. }

	  cinfo^.unread_marker := c;
          { See if we need to insert some fake zero bits. }
	  goto no_more_bytes;
	end;
      end;

      { OK, load c into get_buffer }
      get_buffer := (get_buffer shl 8) or c;
      Inc(bits_left, 8);
    end { end while }
  end
  else
  begin
  no_more_bytes:
    { We get here if we've read the marker that terminates the compressed
      data segment.  There should be enough bits in the buffer register
      to satisfy the request; if so, no problem. }

    if (nbits > bits_left) then
    begin
      { Uh-oh.  Report corrupted data to user and stuff zeroes into
        the data stream, so that we can produce some kind of image.
        We use a nonvolatile flag to ensure that only one warning message
        appears per data segment. }

      if not cinfo^.entropy^.insufficient_data then
      begin
	WARNMS(j_common_ptr(cinfo), JWRN_HIT_MARKER);
	cinfo^.entropy^.insufficient_data := TRUE;
      end;
      { Fill the buffer with zero bits }
      get_buffer := get_buffer shl (MIN_GET_BITS - bits_left);
      bits_left := MIN_GET_BITS;
    end;
  end;

  { Unload the local registers }
  state.next_input_byte := next_input_byte;
  state.bytes_in_buffer := bytes_in_buffer;
  state.get_buffer := get_buffer;
  state.bits_left := bits_left;

  jpeg_fill_bit_buffer := TRUE;
end;


{ Out-of-line code for Huffman code decoding.
  See jdhuff.h for info about usage. }

{GLOBAL}
function jpeg_huff_decode (var state : bitread_working_state;
		          {register} get_buffer : bit_buf_type;
                          {register} bits_left : int;
		          htbl : d_derived_tbl_ptr;
                          min_bits : int) : int;
var
  {register} l : int;
  {register} code : INT32;
begin
  l := min_bits;

  { HUFF_DECODE has determined that the code is at least min_bits }
  { bits long, so fetch that many bits in one swoop. }

  {CHECK_BIT_BUFFER(state, l, return -1);}
  if (bits_left < l) then
  begin
    if (not jpeg_fill_bit_buffer(state, get_buffer, bits_left, l)) then
    begin
      jpeg_huff_decode := -1;
      exit;
    end;
    get_buffer := state.get_buffer;
    bits_left := state.bits_left;
  end;

  {code := GET_BITS(l);}
  Dec(bits_left, l);
  code := (int(get_buffer shr bits_left)) and ( pred(1 shl l) );

  { Collect the rest of the Huffman code one bit at a time. }
  { This is per Figure F.16 in the JPEG spec. }

  while (code > htbl^.maxcode[l]) do
  begin
    code := code shl 1;
    {CHECK_BIT_BUFFER(state, 1, return -1);}
    if (bits_left < 1) then
    begin
      if (not jpeg_fill_bit_buffer(state, get_buffer, bits_left, 1)) then
      begin
        jpeg_huff_decode := -1;
        exit;
      end;
      get_buffer := state.get_buffer;
      bits_left := state.bits_left;
    end;

    {code := code or GET_BITS(1);}
    Dec(bits_left);
    code := code or ( (int(get_buffer shr bits_left)) and pred(1 shl 1) );

    Inc(l);
  end;

  { Unload the local registers }
  state.get_buffer := get_buffer;
  state.bits_left := bits_left;

  { With garbage input we may reach the sentinel value l := 17. }

  if (l > 16) then
  begin
    WARNMS(j_common_ptr(state.cinfo), JWRN_HUFF_BAD_CODE);
    jpeg_huff_decode := 0;	{ fake a zero as the safest result }
    exit;
  end;

  jpeg_huff_decode := htbl^.pub^.huffval[ int (code + htbl^.valoffset[l]) ];
end;


{ Figure F.12: extend sign bit.
  On some machines, a shift and add will be faster than a table lookup. }

{$ifdef AVOID_TABLES}

#define HUFF_EXTEND(x,s)  ((x) < (1<<((s)-1)) ? (x) + (((-1)<<(s)) + 1) : (x))

{$else}

{$define HUFF_EXTEND(x,s)
  if (x < extend_test[s]) then
    := x + extend_offset[s]
  else
   x;}

const
  jdhuff_extend_test : array[0..16-1] of int =   { entry n is 2**(n-1) }
  ($0000, $0001, $0002, $0004, $0008, $0010, $0020, $0040,
   $0080, $0100, $0200, $0400, $0800, $1000, $2000, $4000);

const
  jdhuff_extend_offset : array[0..16-1] of int = { entry n is (-1 << n) + 1 }
(0, ((-1) shl 1) + 1, ((-1) shl 2) + 1, ((-1) shl 3) + 1, ((-1) shl 4) + 1,
    ((-1) shl 5) + 1, ((-1) shl 6) + 1, ((-1) shl 7) + 1, ((-1) shl 8) + 1,
    ((-1) shl 9) + 1, ((-1) shl 10) + 1, ((-1) shl 11) + 1,((-1) shl 12) + 1,
   ((-1) shl 13) + 1, ((-1) shl 14) + 1, ((-1) shl 15) + 1);

{$endif} { AVOID_TABLES }


{ Check for a restart marker & resynchronize decoder.
  Returns FALSE if must suspend. }

{LOCAL}
function jdhuff_process_restart (cinfo : j_decompress_ptr) : boolean;
var
  entropy : jdhuff_huff_entropy_ptr;
  ci : int;
begin
  entropy := jdhuff_huff_entropy_ptr (cinfo^.entropy);

  { Throw away any unused bits remaining in bit buffer; }
  { include any full bytes in next_marker's count of discarded bytes }
  Inc(cinfo^.marker^.discarded_bytes, entropy^.bitstate.bits_left div 8);
  entropy^.bitstate.bits_left := 0;

  { Advance past the RSTn marker }
  if (not cinfo^.marker^.read_restart_marker (cinfo)) then
  begin
    jdhuff_process_restart := FALSE;
    exit;
  end;

  { Re-initialize DC predictions to 0 }
  for ci := 0 to pred(cinfo^.comps_in_scan) do
    entropy^.saved.last_dc_val[ci] := 0;

  { Reset restart counter }
  entropy^.restarts_to_go := cinfo^.restart_interval;

  { Reset out-of-data flag, unless read_restart_marker left us smack up
    against a marker.  In that case we will end up treating the next data
    segment as empty, and we can avoid producing bogus output pixels by
    leaving the flag set. }

  if (cinfo^.unread_marker = 0) then
    entropy^.pub.insufficient_data := FALSE;

  jdhuff_process_restart := TRUE;
end;


{ Decode and return one MCU's worth of Huffman-compressed coefficients.
  The coefficients are reordered from zigzag order into natural array order,
  but are not dequantized.

  The i'th block of the MCU is stored into the block pointed to by
  MCU_data[i].  WE ASSUME THIS AREA HAS BEEN ZEROED BY THE CALLER.
  (Wholesale zeroing is usually a little faster than retail...)

  Returns FALSE if data source requested suspension.  In that case no
  changes have been made to permanent state.  (Exception: some output
  coefficients may already have been assigned.  This is harmless for
  this module, since we'll just re-assign them on the next call.) }

{METHODDEF}
function decode_mcu (cinfo : j_decompress_ptr;
                     var MCU_data : array of JBLOCKROW) : boolean;
label
  label1, label2, label3;
var
  entropy : jdhuff_huff_entropy_ptr;
  {register} s, k, r : int;
  blkn, ci : int;
  block : JBLOCK_PTR;
  {BITREAD_STATE_VARS}
  get_buffer : bit_buf_type ; {register}
  bits_left : int; {register}
  br_state : bitread_working_state;

  state : jdhuff_savable_state;
  dctbl : d_derived_tbl_ptr;
  actbl : d_derived_tbl_ptr;
var
  nb, look : int; {register}
begin
  entropy := jdhuff_huff_entropy_ptr (cinfo^.entropy);

  { Process restart marker if needed; may have to suspend }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
      if (not jdhuff_process_restart(cinfo)) then
      begin
        decode_mcu := FALSE;
        exit;
      end;
  end;

  { If we've run out of data, just leave the MCU set to zeroes.
    This way, we return uniform gray for the remainder of the segment. }

  if not entropy^.pub.insufficient_data then
  begin

    { Load up working state }
    {BITREAD_LOAD_STATE(cinfo,entropy^.bitstate);}
    br_state.cinfo := cinfo;
    br_state.next_input_byte := cinfo^.src^.next_input_byte;
    br_state.bytes_in_buffer := cinfo^.src^.bytes_in_buffer;
    get_buffer := entropy^.bitstate.get_buffer;
    bits_left := entropy^.bitstate.bits_left;

    {ASSIGN_STATE(state, entropy^.saved);}
    state := entropy^.saved;

    { Outer loop handles each block in the MCU }

    for blkn := 0 to pred(cinfo^.blocks_in_MCU) do
    begin
      block := JBLOCK_PTR(MCU_data[blkn]);
      dctbl := entropy^.dc_cur_tbls[blkn];
      actbl := entropy^.ac_cur_tbls[blkn];

      { Decode a single block's worth of coefficients }

      { Section F.2.2.1: decode the DC coefficient difference }
      {HUFF_DECODE(s, br_state, dctbl, return FALSE, label1);}
      if (bits_left < HUFF_LOOKAHEAD) then
      begin
        if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left, 0)) then
        begin
          decode_mcu := False;
          exit;
        end;
        get_buffer := br_state.get_buffer;
        bits_left := br_state.bits_left;
        if (bits_left < HUFF_LOOKAHEAD) then
        begin
          nb := 1;
          goto label1;
        end;
      end;
      {look := PEEK_BITS(HUFF_LOOKAHEAD);}
      look := int(get_buffer shr (bits_left -  HUFF_LOOKAHEAD)) and
                   pred(1 shl HUFF_LOOKAHEAD);

      nb := dctbl^.look_nbits[look];
      if (nb <> 0) then
      begin
        {DROP_BITS(nb);}
        Dec(bits_left, nb);

        s := dctbl^.look_sym[look];
      end
      else
      begin
        nb := HUFF_LOOKAHEAD+1;
    label1:
        s := jpeg_huff_decode(br_state,get_buffer,bits_left,dctbl,nb);
        if (s < 0) then
        begin
          decode_mcu := FALSE;
          exit;
        end;
        get_buffer := br_state.get_buffer;
        bits_left := br_state.bits_left;
      end;

      if (s <> 0) then
      begin
        {CHECK_BIT_BUFFER(br_state, s, return FALSE);}
        if (bits_left < s) then
        begin
          if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,s)) then
          begin
            decode_mcu := FALSE;
            exit;
          end;
          get_buffer := br_state.get_buffer;
          bits_left := br_state.bits_left;
        end;

        {r := GET_BITS(s);}
        Dec(bits_left, s);
        r := ( int(get_buffer shr bits_left)) and ( pred(1 shl s) );

        {s := HUFF_EXTEND(r, s);}
        if (r < jdhuff_extend_test[s]) then
          s := r + jdhuff_extend_offset[s]
        else
          s := r;
      end;

      if (entropy^.dc_needed[blkn]) then
      begin
	{ Convert DC difference to actual value, update last_dc_val }
        ci := cinfo^.MCU_membership[blkn];
	Inc(s, state.last_dc_val[ci]);
        state.last_dc_val[ci] := s;
        { Output the DC coefficient (assumes jpeg_natural_order[0] := 0) }
        block^[0] := JCOEF (s);
      end;

      if (entropy^.ac_needed[blkn]) then
      begin

	{ Section F.2.2.2: decode the AC coefficients }
	{ Since zeroes are skipped, output area must be cleared beforehand }
	k := 1;
        while (k < DCTSIZE2) do         { Nomssi: k is incr. in the loop }
        begin
          {HUFF_DECODE(s, br_state, actbl, return FALSE, label2);}
          if (bits_left < HUFF_LOOKAHEAD) then
          begin
            if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left, 0)) then
            begin
              decode_mcu := False;
              exit;
            end;
            get_buffer := br_state.get_buffer;
            bits_left := br_state.bits_left;
            if (bits_left < HUFF_LOOKAHEAD) then
            begin
              nb := 1;
              goto label2;
            end;
          end;
          {look := PEEK_BITS(HUFF_LOOKAHEAD);}
          look := int(get_buffer shr (bits_left -  HUFF_LOOKAHEAD)) and
                       pred(1 shl HUFF_LOOKAHEAD);

          nb := actbl^.look_nbits[look];
          if (nb <> 0) then
          begin
            {DROP_BITS(nb);}
            Dec(bits_left, nb);

            s := actbl^.look_sym[look];
          end
          else
          begin
            nb := HUFF_LOOKAHEAD+1;
        label2:
            s := jpeg_huff_decode(br_state,get_buffer,bits_left,actbl,nb);
            if (s < 0) then
            begin
              decode_mcu := FALSE;
              exit;
            end;
            get_buffer := br_state.get_buffer;
            bits_left := br_state.bits_left;
          end;

	  r := s shr 4;
	  s := s and 15;

          if (s <> 0) then
          begin
            Inc(k, r);
            {CHECK_BIT_BUFFER(br_state, s, return FALSE);}
            if (bits_left < s) then
            begin
              if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,s)) then
              begin
                decode_mcu := FALSE;
                exit;
              end;
              get_buffer := br_state.get_buffer;
              bits_left := br_state.bits_left;
            end;

            {r := GET_BITS(s);}
            Dec(bits_left, s);
            r := (int(get_buffer shr bits_left)) and ( pred(1 shl s) );

            {s := HUFF_EXTEND(r, s);}
            if (r < jdhuff_extend_test[s]) then
              s := r + jdhuff_extend_offset[s]
            else
              s := r;
            { Output coefficient in natural (dezigzagged) order.
              Note: the extra entries in jpeg_natural_order[] will save us
              if k >= DCTSIZE2, which could happen if the data is corrupted. }

            block^[jpeg_natural_order[k]] := JCOEF (s);
          end
          else
          begin
            if (r <> 15) then
              break;
            Inc(k, 15);
          end;
          Inc(k);
        end;
      end
      else
      begin

        { Section F.2.2.2: decode the AC coefficients }
        { In this path we just discard the values }
        k := 1;
        while (k < DCTSIZE2) do
        begin
	  {HUFF_DECODE(s, br_state, actbl, return FALSE, label3);}
          if (bits_left < HUFF_LOOKAHEAD) then
          begin
            if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left, 0)) then
            begin
              decode_mcu := False;
              exit;
            end;
            get_buffer := br_state.get_buffer;
            bits_left := br_state.bits_left;
            if (bits_left < HUFF_LOOKAHEAD) then
            begin
              nb := 1;
              goto label3;
            end;
          end;
          {look := PEEK_BITS(HUFF_LOOKAHEAD);}
          look := int(get_buffer shr (bits_left -  HUFF_LOOKAHEAD)) and
                       pred(1 shl HUFF_LOOKAHEAD);

          nb := actbl^.look_nbits[look];
          if (nb <> 0) then
          begin
            {DROP_BITS(nb);}
	    Dec(bits_left, nb);

            s := actbl^.look_sym[look];
          end
          else
          begin
            nb := HUFF_LOOKAHEAD+1;
        label3:
            s := jpeg_huff_decode(br_state,get_buffer,bits_left,actbl,nb);
            if (s < 0) then
            begin
              decode_mcu := FALSE;
              exit;
            end;
            get_buffer := br_state.get_buffer;
            bits_left := br_state.bits_left;
          end;

	  r := s shr 4;
	  s := s and 15;

	  if (s <> 0) then
          begin
	    Inc(k, r);
	    {CHECK_BIT_BUFFER(br_state, s, return FALSE);}
            if (bits_left < s) then
            begin
              if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,s)) then
              begin
                decode_mcu := FALSE;
                exit;
              end;
              get_buffer := br_state.get_buffer;
              bits_left := br_state.bits_left;
            end;

	    {DROP_BITS(s);}
            Dec(bits_left, s);
	  end
          else
          begin
	    if (r <> 15) then
	      break;
	    Inc(k, 15);
	  end;
          Inc(k);
        end;

      end;
    end;

    { Completed MCU, so update state }
    {BITREAD_SAVE_STATE(cinfo,entropy^.bitstate);}
    cinfo^.src^.next_input_byte := br_state.next_input_byte;
    cinfo^.src^.bytes_in_buffer := br_state.bytes_in_buffer;
    entropy^.bitstate.get_buffer := get_buffer;
    entropy^.bitstate.bits_left := bits_left;

    {ASSIGN_STATE(entropy^.saved, state);}
    entropy^.saved := state;

  end;

  { Account for restart interval (no-op if not using restarts) }
  if entropy^.restarts_to_go > 0 then
    Dec(entropy^.restarts_to_go);

  decode_mcu := TRUE;
end;


{ Module initialization routine for Huffman entropy decoding. }

{GLOBAL}
procedure jinit_huff_decoder (cinfo : j_decompress_ptr);
var
  entropy : jdhuff_huff_entropy_ptr;
  i : int;
begin
  entropy := jdhuff_huff_entropy_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(huff_entropy_decoder)) );
  cinfo^.entropy := jpeg_entropy_decoder_ptr (entropy);
  entropy^.pub.start_pass := start_pass_huff_decoder;
  entropy^.pub.decode_mcu := decode_mcu;

  { Mark tables unallocated }
  for i := 0 to pred(NUM_HUFF_TBLS) do
  begin
    entropy^.dc_derived_tbls[i] := NIL;
    entropy^.ac_derived_tbls[i] := NIL;
  end;
end;

{== imjdphuff implementation ==}

{ Expanded entropy decoder object for progressive Huffman decoding.

  The savable_state subrecord contains fields that change within an MCU,
  but must not be updated permanently until we complete the MCU. }

type
  jdphuff_savable_state = record
    EOBRUN : uInt;               { remaining EOBs in EOBRUN }
    last_dc_val : array[00..MAX_COMPS_IN_SCAN-1] of int;
                                 { last DC coef for each component }
  end;


type
  jdphuff_phuff_entropy_ptr  = ^phuff_entropy_decoder;
  phuff_entropy_decoder = record
    pub : jpeg_entropy_decoder; { public fields }

    { These fields are loaded into local variables at start of each MCU.
      In case of suspension, we exit WITHOUT updating them. }

    bitstate : bitread_perm_state;	{ Bit buffer at start of MCU }
    saved : jdphuff_savable_state;		{ Other state at start of MCU }

    { These fields are NOT loaded into local working state. }
    restarts_to_go : uInt;              { MCUs left in this restart interval }

    { Pointers to derived tables (these workspaces have image lifespan) }
    derived_tbls : array[0..NUM_HUFF_TBLS-1] of d_derived_tbl_ptr;

    ac_derived_tbl : d_derived_tbl_ptr; { active table during an AC scan }
  end;


{ Forward declarations }
{METHODDEF}
function decode_mcu_DC_first (cinfo : j_decompress_ptr;
                              var MCU_data : array of JBLOCKROW) : boolean;
                              forward;
{METHODDEF}
function decode_mcu_AC_first (cinfo : j_decompress_ptr;
                              var MCU_data : array of JBLOCKROW) : boolean;
                              forward;
{METHODDEF}
function decode_mcu_DC_refine (cinfo : j_decompress_ptr;
                               var MCU_data : array of JBLOCKROW) : boolean;
                               forward;
{METHODDEF}
function decode_mcu_AC_refine (cinfo : j_decompress_ptr;
                               var MCU_data : array of JBLOCKROW) : boolean;
                               forward;

{ Initialize for a Huffman-compressed scan. }

{METHODDEF}
procedure start_pass_phuff_decoder (cinfo : j_decompress_ptr);
var
  entropy : jdphuff_phuff_entropy_ptr;
  is_DC_band, bad : boolean;
  ci, coefi, tbl : int;
  coef_bit_ptr : coef_bits_ptr;
  compptr : jpeg_component_info_ptr;
var
  cindex : int;
  expected : int;
begin
  entropy := jdphuff_phuff_entropy_ptr (cinfo^.entropy);

  is_DC_band := (cinfo^.Ss = 0);

  { Validate scan parameters }
  bad := FALSE;
  if (is_DC_band) then
  begin
    if (cinfo^.Se <> 0) then
      bad := TRUE;
  end
  else
  begin
    { need not check Ss/Se < 0 since they came from unsigned bytes }
    if (cinfo^.Ss > cinfo^.Se) or (cinfo^.Se >= DCTSIZE2) then
      bad := TRUE;
    { AC scans may have only one component }
    if (cinfo^.comps_in_scan <> 1) then
      bad := TRUE;
  end;
  if (cinfo^.Ah <> 0) then
  begin
    { Successive approximation refinement scan: must have Al = Ah-1. }
    if (cinfo^.Al <> cinfo^.Ah-1) then
      bad := TRUE;
  end;
  if (cinfo^.Al > 13) then      { need not check for < 0 }
    bad := TRUE;
  { Arguably the maximum Al value should be less than 13 for 8-bit precision,
    but the spec doesn't say so, and we try to be liberal about what we
    accept.  Note: large Al values could result in out-of-range DC
    coefficients during early scans, leading to bizarre displays due to
    overflows in the IDCT math.  But we won't crash. }

  if (bad) then
    ERREXIT4(j_common_ptr(cinfo), JERR_BAD_PROGRESSION,
	     cinfo^.Ss, cinfo^.Se, cinfo^.Ah, cinfo^.Al);
  { Update progression status, and verify that scan order is legal.
    Note that inter-scan inconsistencies are treated as warnings
    not fatal errors ... not clear if this is right way to behave. }

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    cindex := cinfo^.cur_comp_info[ci]^.component_index;
    coef_bit_ptr := coef_bits_ptr(@(cinfo^.coef_bits^[cindex])); {^[0] ???
                                                                   Nomssi    }
    if (not is_DC_band) and (coef_bit_ptr^[0] < 0) then
      { AC without prior DC scan }
      WARNMS2(j_common_ptr(cinfo), JWRN_BOGUS_PROGRESSION, cindex, 0);
    for coefi := cinfo^.Ss to cinfo^.Se do
    begin
      if (coef_bit_ptr^[coefi] < 0) then
        expected :=  0
      else
        expected := coef_bit_ptr^[coefi];
      if (cinfo^.Ah <> expected) then
	WARNMS2(j_common_ptr(cinfo), JWRN_BOGUS_PROGRESSION, cindex, coefi);
      coef_bit_ptr^[coefi] := cinfo^.Al;
    end;
  end;

  { Select MCU decoding routine }
  if (cinfo^.Ah = 0) then
  begin
    if (is_DC_band) then
      entropy^.pub.decode_mcu := decode_mcu_DC_first
    else
      entropy^.pub.decode_mcu := decode_mcu_AC_first;
  end
  else
  begin
    if (is_DC_band) then
      entropy^.pub.decode_mcu := decode_mcu_DC_refine
    else
      entropy^.pub.decode_mcu := decode_mcu_AC_refine;
  end;

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    { Make sure requested tables are present, and compute derived tables.
      We may build same derived table more than once, but it's not expensive. }

    if (is_DC_band) then
    begin
      if (cinfo^.Ah = 0) then
      begin	{ DC refinement needs no table }
	tbl := compptr^.dc_tbl_no;
	jpeg_make_d_derived_tbl(cinfo, TRUE, tbl,
				 entropy^.derived_tbls[tbl]);
      end;
    end
    else
    begin
      tbl := compptr^.ac_tbl_no;
      jpeg_make_d_derived_tbl(cinfo, FALSE, tbl,
			       entropy^.derived_tbls[tbl]);
      { remember the single active table }
      entropy^.ac_derived_tbl := entropy^.derived_tbls[tbl];
    end;
    { Initialize DC predictions to 0 }
    entropy^.saved.last_dc_val[ci] := 0;
  end;

  { Initialize bitread state variables }
  entropy^.bitstate.bits_left := 0;
  entropy^.bitstate.get_buffer := 0; { unnecessary, but keeps Purify quiet }
  entropy^.pub.insufficient_data := FALSE;

  { Initialize private state variables }
  entropy^.saved.EOBRUN := 0;

  { Initialize restart counter }
  entropy^.restarts_to_go := cinfo^.restart_interval;
end;


{ Figure F.12: extend sign bit.
  On some machines, a shift and add will be faster than a table lookup. }

{$ifdef AVOID_TABLES}

#define HUFF_EXTEND(x,s)
  ((x) < (1shl((s)-1)) ? (x) + (((-1)shl(s)) + 1) : (x))

{$else}

{ #define HUFF_EXTEND(x,s)
  if (x) < extend_test[s] then
    (x) + extend_offset[s]
  else
    (x)}

const
 jdphuff_extend_test : Array[0..16-1] of int =   { entry n is 2**(n-1) }
   ($0000, $0001, $0002, $0004, $0008, $0010, $0020, $0040,
    $0080, $0100, $0200, $0400, $0800, $1000, $2000, $4000);

const
  jdphuff_extend_offset : array[0..16-1] of int = { entry n is (-1 shl n) + 1 }
  ( 0, ((-1) shl 1) + 1, ((-1) shl 2) + 1, ((-1) shl 3) + 1, ((-1) shl 4) + 1,
    ((-1) shl 5) + 1, ((-1) shl 6) + 1, ((-1) shl 7) + 1, ((-1) shl 8) + 1,
    ((-1) shl 9) + 1, ((-1) shl 10) + 1, ((-1) shl 11) + 1, ((-1) shl 12) + 1,
    ((-1) shl 13) + 1, ((-1) shl 14) + 1, ((-1) shl 15) + 1 );

{$endif} { AVOID_TABLES }


{ Check for a restart marker & resynchronize decoder.
  return:=s FALSE if must suspend. }

{LOCAL}
function jdphuff_process_restart (cinfo : j_decompress_ptr) : boolean;
var
  entropy : jdphuff_phuff_entropy_ptr;
  ci : int;
begin
  entropy := jdphuff_phuff_entropy_ptr (cinfo^.entropy);

  { Throw away any unused bits remaining in bit buffer; }
  { include any full bytes in next_marker's count of discarded bytes }
  Inc(cinfo^.marker^.discarded_bytes, entropy^.bitstate.bits_left div 8);
  entropy^.bitstate.bits_left := 0;

  { Advance past the RSTn marker }
  if (not cinfo^.marker^.read_restart_marker (cinfo)) then
  begin
    jdphuff_process_restart := FALSE;
    exit;
  end;

  { Re-initialize DC predictions to 0 }
  for ci := 0 to pred(cinfo^.comps_in_scan) do
    entropy^.saved.last_dc_val[ci] := 0;
  { Re-init EOB run count, too }
  entropy^.saved.EOBRUN := 0;

  { Reset restart counter }
  entropy^.restarts_to_go := cinfo^.restart_interval;

  { Reset out-of-data flag, unless read_restart_marker left us smack up
    against a marker.  In that case we will end up treating the next data
    segment as empty, and we can avoid producing bogus output pixels by
    leaving the flag set. }
  if (cinfo^.unread_marker = 0) then
    entropy^.pub.insufficient_data := FALSE;

  jdphuff_process_restart := TRUE;
end;


{ Huffman MCU decoding.
  Each of these routines decodes and returns one MCU's worth of
  Huffman-compressed coefficients.
  The coefficients are reordered from zigzag order into natural array order,
  but are not dequantized.

  The i'th block of the MCU is stored into the block pointed to by
  MCU_data[i].  WE ASSUME THIS AREA IS INITIALLY ZEROED BY THE CALLER.

  We return FALSE if data source requested suspension.  In that case no
  changes have been made to permanent state.  (Exception: some output
  coefficients may already have been assigned.  This is harmless for
  spectral selection, since we'll just re-assign them on the next call.
  Successive approximation AC refinement has to be more careful, however.) }


{ MCU decoding for DC initial scan (either spectral selection,
  or first pass of successive approximation). }

{METHODDEF}
function decode_mcu_DC_first (cinfo : j_decompress_ptr;
                              var MCU_data : array of JBLOCKROW) : boolean;
label
  label1;
var
  entropy : jdphuff_phuff_entropy_ptr;
  Al : int;
  {register} s, r : int;
  blkn, ci : int;
  block : JBLOCK_PTR;
  {BITREAD_STATE_VARS;}
  get_buffer : bit_buf_type ; {register}
  bits_left : int; {register}
  br_state : bitread_working_state;

  state : jdphuff_savable_state;
  tbl : d_derived_tbl_ptr;
  compptr : jpeg_component_info_ptr;
var
  nb, look : int; {register}
begin
  entropy := jdphuff_phuff_entropy_ptr (cinfo^.entropy);
  Al := cinfo^.Al;

  { Process restart marker if needed; may have to suspend }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
      if (not jdphuff_process_restart(cinfo)) then
      begin
	decode_mcu_DC_first := FALSE;
        exit;
      end;
  end;

  { If we've run out of data, just leave the MCU set to zeroes.
    This way, we return uniform gray for the remainder of the segment. }

  if not entropy^.pub.insufficient_data then
  begin

    { Load up working state }
    {BITREAD_LOAD_STATE(cinfo,entropy^.bitstate);}
    br_state.cinfo := cinfo;
    br_state.next_input_byte := cinfo^.src^.next_input_byte;
    br_state.bytes_in_buffer := cinfo^.src^.bytes_in_buffer;
    get_buffer := entropy^.bitstate.get_buffer;
    bits_left := entropy^.bitstate.bits_left;

    {ASSIGN_STATE(state, entropy^.saved);}
    state := entropy^.saved;

    { Outer loop handles each block in the MCU }

    for blkn := 0 to pred(cinfo^.blocks_in_MCU) do
    begin
      block := JBLOCK_PTR(MCU_data[blkn]);
      ci := cinfo^.MCU_membership[blkn];
      compptr := cinfo^.cur_comp_info[ci];
      tbl := entropy^.derived_tbls[compptr^.dc_tbl_no];

      { Decode a single block's worth of coefficients }

      { Section F.2.2.1: decode the DC coefficient difference }
      {HUFF_DECODE(s, br_state, tbl, return FALSE, label1);}
      if (bits_left < HUFF_LOOKAHEAD) then
      begin
        if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left, 0)) then
        begin
          decode_mcu_DC_first := FALSE;
          exit;
        end;
        get_buffer := br_state.get_buffer;
        bits_left := br_state.bits_left;
        if (bits_left < HUFF_LOOKAHEAD) then
        begin
          nb := 1;
          goto label1;
        end;
      end;
      {look := PEEK_BITS(HUFF_LOOKAHEAD);}
      look := int(get_buffer shr (bits_left -  HUFF_LOOKAHEAD)) and
                     pred(1 shl HUFF_LOOKAHEAD);

      nb := tbl^.look_nbits[look];
      if (nb <> 0) then
      begin
        {DROP_BITS(nb);}
        Dec(bits_left, nb);

        s := tbl^.look_sym[look];
      end
      else
      begin
        nb := HUFF_LOOKAHEAD+1;
    label1:
        s := jpeg_huff_decode(br_state,get_buffer,bits_left,tbl,nb);
        if (s < 0) then
        begin
          decode_mcu_DC_first := FALSE;
          exit;
        end;
        get_buffer := br_state.get_buffer;
        bits_left := br_state.bits_left;
      end;

      if (s <> 0) then
      begin
        {CHECK_BIT_BUFFER(br_state, s, return FALSE);}
        if (bits_left < s) then
        begin
          if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,s)) then
          begin
            decode_mcu_DC_first := FALSE;
            exit;
          end;
          get_buffer := br_state.get_buffer;
          bits_left := br_state.bits_left;
        end;

        {r := GET_BITS(s);}
        Dec(bits_left, s);
        r := (int(get_buffer shr bits_left)) and ( pred(1 shl s) );

        {s := HUFF_EXTEND(r, s);}
        if (r < jdphuff_extend_test[s]) then
          s := r + jdphuff_extend_offset[s]
        else
          s := r;
      end;

      { Convert DC difference to actual value, update last_dc_val }
      Inc(s, state.last_dc_val[ci]);
      state.last_dc_val[ci] := s;
      { Scale and output the DC coefficient (assumes jpeg_natural_order[0]=0) }
      block^[0] := JCOEF (s shl Al);
    end;

    { Completed MCU, so update state }
    {BITREAD_SAVE_STATE(cinfo,entropy^.bitstate);}
    cinfo^.src^.next_input_byte := br_state.next_input_byte;
    cinfo^.src^.bytes_in_buffer := br_state.bytes_in_buffer;
    entropy^.bitstate.get_buffer := get_buffer;
    entropy^.bitstate.bits_left := bits_left;

    {ASSIGN_STATE(entropy^.saved, state);}
    entropy^.saved := state;
  end;

  { Account for restart interval (no-op if not using restarts) }
  Dec(entropy^.restarts_to_go);

  decode_mcu_DC_first := TRUE;
end;


{ MCU decoding for AC initial scan (either spectral selection,
  or first pass of successive approximation). }

{METHODDEF}
function decode_mcu_AC_first (cinfo : j_decompress_ptr;
                              var MCU_data : array of JBLOCKROW) : boolean;
label
  label2;
var
  entropy : jdphuff_phuff_entropy_ptr;
  Se : int;
  Al : int;
  {register} s, k, r : int;
  EOBRUN : uInt;
  block : JBLOCK_PTR;
  {BITREAD_STATE_VARS;}
  get_buffer : bit_buf_type ; {register}
  bits_left : int; {register}
  br_state : bitread_working_state;

  tbl : d_derived_tbl_ptr;
var
  nb, look : int; {register}
begin
  entropy := jdphuff_phuff_entropy_ptr (cinfo^.entropy);
  Se := cinfo^.Se;
  Al := cinfo^.Al;

  { Process restart marker if needed; may have to suspend }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
      if (not jdphuff_process_restart(cinfo)) then
      begin
	decode_mcu_AC_first := FALSE;
        exit;
      end;
  end;

  { If we've run out of data, just leave the MCU set to zeroes.
    This way, we return uniform gray for the remainder of the segment. }
  if not entropy^.pub.insufficient_data then
  begin

    { Load up working state.
      We can avoid loading/saving bitread state if in an EOB run. }

    EOBRUN := entropy^.saved.EOBRUN; { only part of saved state we care about }

    { There is always only one block per MCU }

    if (EOBRUN > 0) then       { if it's a band of zeroes... }
      Dec(EOBRUN)              { ...process it now (we do nothing) }
    else
    begin
      {BITREAD_LOAD_STATE(cinfo,entropy^.bitstate);}
      br_state.cinfo := cinfo;
      br_state.next_input_byte := cinfo^.src^.next_input_byte;
      br_state.bytes_in_buffer := cinfo^.src^.bytes_in_buffer;
      get_buffer := entropy^.bitstate.get_buffer;
      bits_left := entropy^.bitstate.bits_left;

      block := JBLOCK_PTR(MCU_data[0]);
      tbl := entropy^.ac_derived_tbl;

      k := cinfo^.Ss;
      while (k <= Se) do
      begin
        {HUFF_DECODE(s, br_state, tbl, return FALSE, label2);}
        if (bits_left < HUFF_LOOKAHEAD) then
        begin
          if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left, 0)) then
          begin
            decode_mcu_AC_first := FALSE;
            exit;
          end;
          get_buffer := br_state.get_buffer;
          bits_left := br_state.bits_left;
          if (bits_left < HUFF_LOOKAHEAD) then
          begin
            nb := 1;
            goto label2;
          end;
        end;
        {look := PEEK_BITS(HUFF_LOOKAHEAD);}
        look := int(get_buffer shr (bits_left -  HUFF_LOOKAHEAD)) and
                       pred(1 shl HUFF_LOOKAHEAD);

        nb := tbl^.look_nbits[look];
        if (nb <> 0) then
        begin
          {DROP_BITS(nb);}
          Dec(bits_left, nb);

          s := tbl^.look_sym[look];
        end
        else
        begin
          nb := HUFF_LOOKAHEAD+1;
      label2:
          s := jpeg_huff_decode(br_state,get_buffer,bits_left,tbl,nb);
          if (s < 0) then
          begin
            decode_mcu_AC_first := FALSE;
            exit;
          end;
          get_buffer := br_state.get_buffer;
          bits_left := br_state.bits_left;
        end;

        r := s shr 4;
        s := s and 15;
        if (s <> 0) then
        begin
          Inc(k, r);
          {CHECK_BIT_BUFFER(br_state, s, return FALSE);}
          if (bits_left < s) then
          begin
            if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,s)) then
            begin
              decode_mcu_AC_first := FALSE;
              exit;
            end;
            get_buffer := br_state.get_buffer;
            bits_left := br_state.bits_left;
          end;

          {r := GET_BITS(s);}
          Dec(bits_left, s);
          r := (int(get_buffer shr bits_left)) and ( pred(1 shl s) );

          {s := HUFF_EXTEND(r, s);}
          if (r < jdphuff_extend_test[s]) then
            s := r + jdphuff_extend_offset[s]
          else
            s := r;

	  { Scale and output coefficient in natural (dezigzagged) order }
          block^[jpeg_natural_order[k]] := JCOEF (s shl Al);
        end
        else
        begin
          if (r = 15) then
          begin		{ ZRL }
            Inc(k, 15);	{ skip 15 zeroes in band }
          end
          else
          begin		{ EOBr, run length is 2^r + appended bits }
            EOBRUN := 1 shl r;
            if (r <> 0) then
            begin		{ EOBr, r > 0 }
	      {CHECK_BIT_BUFFER(br_state, r, return FALSE);}
              if (bits_left < r) then
              begin
                if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,r)) then
                begin
                  decode_mcu_AC_first := FALSE;
                  exit;
                end;
                get_buffer := br_state.get_buffer;
                bits_left := br_state.bits_left;
              end;

              {r := GET_BITS(r);}
              Dec(bits_left, r);
              r := (int(get_buffer shr bits_left)) and ( pred(1 shl r) );

              Inc(EOBRUN, r);
            end;
	    Dec(EOBRUN);          { this band is processed at this moment }
	    break;                { force end-of-band }
	  end;
        end;
        Inc(k);
      end;

      {BITREAD_SAVE_STATE(cinfo,entropy^.bitstate);}
      cinfo^.src^.next_input_byte := br_state.next_input_byte;
      cinfo^.src^.bytes_in_buffer := br_state.bytes_in_buffer;
      entropy^.bitstate.get_buffer := get_buffer;
      entropy^.bitstate.bits_left := bits_left;
    end;

    { Completed MCU, so update state }
    entropy^.saved.EOBRUN := EOBRUN; { only part of saved state we care about }
  end;

  { Account for restart interval (no-op if not using restarts) }
  Dec(entropy^.restarts_to_go);

  decode_mcu_AC_first := TRUE;
end;


{ MCU decoding for DC successive approximation refinement scan.
  Note: we assume such scans can be multi-component, although the spec
  is not very clear on the point. }

{METHODDEF}
function decode_mcu_DC_refine (cinfo : j_decompress_ptr;
                               var MCU_data : array of JBLOCKROW) : boolean;

var
  entropy : jdphuff_phuff_entropy_ptr;
  p1 : int;          { 1 in the bit position being coded }
  blkn : int;
  block : JBLOCK_PTR;
  {BITREAD_STATE_VARS;}
  get_buffer : bit_buf_type ; {register}
  bits_left : int; {register}
  br_state : bitread_working_state;
begin
  entropy := jdphuff_phuff_entropy_ptr (cinfo^.entropy);
  p1 := 1 shl cinfo^.Al;

  { Process restart marker if needed; may have to suspend }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
      if (not jdphuff_process_restart(cinfo)) then
      begin
	decode_mcu_DC_refine := FALSE;
        exit;
      end;
  end;

  { Not worth the cycles to check insufficient_data here,
    since we will not change the data anyway if we read zeroes. }

  { Load up working state }
  {BITREAD_LOAD_STATE(cinfo,entropy^.bitstate);}
  br_state.cinfo := cinfo;
  br_state.next_input_byte := cinfo^.src^.next_input_byte;
  br_state.bytes_in_buffer := cinfo^.src^.bytes_in_buffer;
  get_buffer := entropy^.bitstate.get_buffer;
  bits_left := entropy^.bitstate.bits_left;

  { Outer loop handles each block in the MCU }

  for blkn := 0 to pred(cinfo^.blocks_in_MCU) do
  begin
    block := JBLOCK_PTR(MCU_data[blkn]);

    { Encoded data is simply the next bit of the two's-complement DC value }
    {CHECK_BIT_BUFFER(br_state, 1, return FALSE);}
    if (bits_left < 1) then
    begin
      if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,1)) then
      begin
        decode_mcu_DC_refine := FALSE;
        exit;
      end;
      get_buffer := br_state.get_buffer;
      bits_left := br_state.bits_left;
    end;

    {if (GET_BITS(1)) then}
    Dec(bits_left);
    if (int(get_buffer shr bits_left)) and ( pred(1 shl 1) ) <> 0 then
      block^[0] := block^[0] or p1;
    { Note: since we use OR, repeating the assignment later is safe }
  end;

  { Completed MCU, so update state }
  {BITREAD_SAVE_STATE(cinfo,entropy^.bitstate);}
  cinfo^.src^.next_input_byte := br_state.next_input_byte;
  cinfo^.src^.bytes_in_buffer := br_state.bytes_in_buffer;
  entropy^.bitstate.get_buffer := get_buffer;
  entropy^.bitstate.bits_left := bits_left;

  { Account for restart interval (no-op if not using restarts) }
  Dec(entropy^.restarts_to_go);

  decode_mcu_DC_refine := TRUE;
end;


{ MCU decoding for AC successive approximation refinement scan. }

{METHODDEF}
function decode_mcu_AC_refine (cinfo : j_decompress_ptr;
                               var MCU_data : array of JBLOCKROW) : boolean;
label
  undoit, label3;
var
  entropy : jdphuff_phuff_entropy_ptr;
  Se : int;
  p1 : int;     { 1 in the bit position being coded }
  m1 : int;     { -1 in the bit position being coded }
  {register} s, k, r : int;
  EOBRUN : uInt;
  block : JBLOCK_PTR;
  thiscoef : JCOEF_PTR;
  {BITREAD_STATE_VARS;}
  get_buffer : bit_buf_type ; {register}
  bits_left : int; {register}
  br_state : bitread_working_state;

  tbl : d_derived_tbl_ptr;
  num_newnz : int;
  newnz_pos : array[0..DCTSIZE2-1] of int;
var
  pos : int;
var
  nb, look : int; {register}
begin
  num_newnz := 0;
  block := nil;

  entropy := jdphuff_phuff_entropy_ptr (cinfo^.entropy);
  Se := cinfo^.Se;
  p1 := 1 shl cinfo^.Al;	{ 1 in the bit position being coded }
  m1 := (-1) shl cinfo^.Al;	{ -1 in the bit position being coded }

  { Process restart marker if needed; may have to suspend }
  if (cinfo^.restart_interval <> 0) then
  begin
    if (entropy^.restarts_to_go = 0) then
      if (not jdphuff_process_restart(cinfo)) then
      begin
	decode_mcu_AC_refine := FALSE;
        exit;
      end;
  end;

  { If we've run out of data, don't modify the MCU. }
  if not entropy^.pub.insufficient_data then
  begin

    { Load up working state }
    {BITREAD_LOAD_STATE(cinfo,entropy^.bitstate);}
    br_state.cinfo := cinfo;
    br_state.next_input_byte := cinfo^.src^.next_input_byte;
    br_state.bytes_in_buffer := cinfo^.src^.bytes_in_buffer;
    get_buffer := entropy^.bitstate.get_buffer;
    bits_left := entropy^.bitstate.bits_left;

    EOBRUN := entropy^.saved.EOBRUN; { only part of saved state we care about }

    { There is always only one block per MCU }
    block := JBLOCK_PTR(MCU_data[0]);
    tbl := entropy^.ac_derived_tbl;

    { If we are forced to suspend, we must undo the assignments to any newly
      nonzero coefficients in the block, because otherwise we'd get confused
      next time about which coefficients were already nonzero.
      But we need not undo addition of bits to already-nonzero coefficients;
      instead, we can test the current bit position to see if we already did it.}

    num_newnz := 0;

    { initialize coefficient loop counter to start of band }
    k := cinfo^.Ss;

    if (EOBRUN = 0) then
    begin
      while (k <= Se) do
      begin
        {HUFF_DECODE(s, br_state, tbl, goto undoit, label3);}
        if (bits_left < HUFF_LOOKAHEAD) then
        begin
          if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left, 0)) then
            goto undoit;
          get_buffer := br_state.get_buffer;
          bits_left := br_state.bits_left;
          if (bits_left < HUFF_LOOKAHEAD) then
          begin
            nb := 1;
            goto label3;
          end;
        end;
        {look := PEEK_BITS(HUFF_LOOKAHEAD);}
        look := int(get_buffer shr (bits_left -  HUFF_LOOKAHEAD)) and
                       pred(1 shl HUFF_LOOKAHEAD);

        nb := tbl^.look_nbits[look];
        if (nb <> 0) then
        begin
          {DROP_BITS(nb);}
          Dec(bits_left, nb);

          s := tbl^.look_sym[look];
        end
        else
        begin
          nb := HUFF_LOOKAHEAD+1;
      label3:
          s := jpeg_huff_decode(br_state,get_buffer,bits_left,tbl,nb);
          if (s < 0) then
            goto undoit;
          get_buffer := br_state.get_buffer;
          bits_left := br_state.bits_left;
        end;

        r := s shr 4;
        s := s and 15;
        if (s <> 0) then
        begin
	  if (s <> 1) then	{ size of new coef should always be 1 }
	    WARNMS(j_common_ptr(cinfo), JWRN_HUFF_BAD_CODE);
          {CHECK_BIT_BUFFER(br_state, 1, goto undoit);}
          if (bits_left < 1) then
          begin
            if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,1)) then
              goto undoit;
            get_buffer := br_state.get_buffer;
            bits_left := br_state.bits_left;
          end;

          {if (GET_BITS(1)) then}
          Dec(bits_left);
          if (int(get_buffer shr bits_left)) and ( pred(1 shl 1) )<>0 then
	    s := p1		{ newly nonzero coef is positive }
	  else
	    s := m1;		{ newly nonzero coef is negative }
        end
        else
        begin
	  if (r <> 15) then
          begin
	    EOBRUN := 1 shl r;	{ EOBr, run length is 2^r + appended bits }
	    if (r <> 0) then
            begin
	      {CHECK_BIT_BUFFER(br_state, r, goto undoit);}
              if (bits_left < r) then
              begin
                if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,r)) then
                  goto undoit;
                get_buffer := br_state.get_buffer;
                bits_left := br_state.bits_left;
              end;

	      {r := GET_BITS(r);}
              Dec(bits_left, r);
              r := (int(get_buffer shr bits_left)) and ( pred(1 shl r) );

	      Inc(EOBRUN, r);
	    end;
	    break;		{ rest of block is handled by EOB logic }
	  end;
	  { note s := 0 for processing ZRL }
        end;
        { Advance over already-nonzero coefs and r still-zero coefs,
          appending correction bits to the nonzeroes.  A correction bit is 1
          if the absolute value of the coefficient must be increased. }

        repeat
	  thiscoef :=@(block^[jpeg_natural_order[k]]);
	  if (thiscoef^ <> 0) then
          begin
	    {CHECK_BIT_BUFFER(br_state, 1, goto undoit);}
            if (bits_left < 1) then
            begin
              if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,1)) then
                goto undoit;
              get_buffer := br_state.get_buffer;
              bits_left := br_state.bits_left;
            end;

	    {if (GET_BITS(1)) then}
            Dec(bits_left);
            if (int(get_buffer shr bits_left)) and ( pred(1 shl 1) )<>0 then
            begin
	      if ((thiscoef^ and p1) = 0) then
              begin { do nothing if already set it }
	        if (thiscoef^ >= 0) then
		  Inc(thiscoef^, p1)
	        else
		  Inc(thiscoef^, m1);
	      end;
	    end;
	  end
          else
          begin
            Dec(r);
	    if (r < 0) then
	      break;		{ reached target zero coefficient }
	  end;
	  Inc(k);
        until (k > Se);
        if (s <> 0) then
        begin
	  pos := jpeg_natural_order[k];
	  { Output newly nonzero coefficient }
	  block^[pos] := JCOEF (s);
	  { Remember its position in case we have to suspend }
	  newnz_pos[num_newnz] := pos;
          Inc(num_newnz);
        end;
        Inc(k);
      end;
    end;

    if (EOBRUN > 0) then
    begin
      { Scan any remaining coefficient positions after the end-of-band
        (the last newly nonzero coefficient, if any).  Append a correction
        bit to each already-nonzero coefficient.  A correction bit is 1
        if the absolute value of the coefficient must be increased. }

      while (k <= Se) do
      begin
        thiscoef := @(block^[jpeg_natural_order[k]]);
        if (thiscoef^ <> 0) then
        begin
	  {CHECK_BIT_BUFFER(br_state, 1, goto undoit);}
          if (bits_left < 1) then
          begin
            if (not jpeg_fill_bit_buffer(br_state,get_buffer,bits_left,1)) then
              goto undoit;
            get_buffer := br_state.get_buffer;
            bits_left := br_state.bits_left;
          end;

	  {if (GET_BITS(1)) then}
          Dec(bits_left);
          if (int(get_buffer shr bits_left)) and ( pred(1 shl 1) )<>0 then
          begin
	    if ((thiscoef^ and p1) = 0) then
            begin { do nothing if already changed it }
	      if (thiscoef^ >= 0) then
	        Inc(thiscoef^, p1)
	      else
	        Inc(thiscoef^, m1);
	    end;
	  end;
        end;
        Inc(k);
      end;
      { Count one block completed in EOB run }
      Dec(EOBRUN);
    end;

    { Completed MCU, so update state }
    {BITREAD_SAVE_STATE(cinfo,entropy^.bitstate);}
    cinfo^.src^.next_input_byte := br_state.next_input_byte;
    cinfo^.src^.bytes_in_buffer := br_state.bytes_in_buffer;
    entropy^.bitstate.get_buffer := get_buffer;
    entropy^.bitstate.bits_left := bits_left;

    entropy^.saved.EOBRUN := EOBRUN; { only part of saved state we care about }
  end;

  { Account for restart interval (no-op if not using restarts) }
  Dec(entropy^.restarts_to_go);

  decode_mcu_AC_refine := TRUE;
  exit;

undoit:
  { Re-zero any output coefficients that we made newly nonzero }
  while (num_newnz > 0) do
  begin
    Dec(num_newnz);
    block^[newnz_pos[num_newnz]] := 0;
  end;

  decode_mcu_AC_refine := FALSE;
end;


{ Module initialization routine for progressive Huffman entropy decoding. }

{GLOBAL}
procedure jinit_phuff_decoder (cinfo : j_decompress_ptr);
var
  entropy : jdphuff_phuff_entropy_ptr;
  coef_bit_ptr : int_ptr;
  ci, i : int;
begin
  entropy := jdphuff_phuff_entropy_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
				SIZEOF(phuff_entropy_decoder)) );
  cinfo^.entropy := jpeg_entropy_decoder_ptr (entropy);
  entropy^.pub.start_pass := start_pass_phuff_decoder;

  { Mark derived tables unallocated }
  for i := 0 to pred(NUM_HUFF_TBLS) do
  begin
    entropy^.derived_tbls[i] := NIL;
  end;

  { Create progression status table }
  cinfo^.coef_bits := coef_bits_ptrrow (
     cinfo^.mem^.alloc_small ( j_common_ptr (cinfo), JPOOL_IMAGE,
				cinfo^.num_components*DCTSIZE2*SIZEOF(int)) );
  coef_bit_ptr := @cinfo^.coef_bits^[0][0];
  for ci := 0 to pred(cinfo^.num_components) do
    for i := 0 to pred(DCTSIZE2) do
    begin
      coef_bit_ptr^ := -1;
      Inc(coef_bit_ptr);
    end;
end;

{== imjdcoefct implementation ==}

{ Block smoothing is only applicable for progressive JPEG, so: }
{$ifndef D_PROGRESSIVE_SUPPORTED}
{$undef BLOCK_SMOOTHING_SUPPORTED}
{$endif}

{ Private buffer controller object }

{$ifdef BLOCK_SMOOTHING_SUPPORTED}
const
  SAVED_COEFS = 6;              { we save coef_bits[0..5] }
type
  Latch = array[0..SAVED_COEFS-1] of int;
  Latch_ptr = ^Latch;
{$endif}

type
  jdcoefct_my_coef_ptr = ^jdcoefct_my_coef_controller;
  jdcoefct_my_coef_controller = record
    pub : jpeg_d_coef_controller; { public fields }

    { These variables keep track of the current location of the input side. }
    { cinfo^.input_iMCU_row is also used for this. }
    MCU_ctr : JDIMENSION;               { counts MCUs processed in current row }
    MCU_vert_offset : int;              { counts MCU rows within iMCU row }
    MCU_rows_per_iMCU_row : int;        { number of such rows needed }

    { The output side's location is represented by cinfo^.output_iMCU_row. }

    { In single-pass modes, it's sufficient to buffer just one MCU.
      We allocate a workspace of D_MAX_BLOCKS_IN_MCU coefficient blocks,
      and let the entropy decoder write into that workspace each time.
      (On 80x86, the workspace is FAR even though it's not really very big;
      this is to keep the module interfaces unchanged when a large coefficient
      buffer is necessary.)
      In multi-pass modes, this array points to the current MCU's blocks
      within the virtual arrays; it is used only by the input side. }

    MCU_buffer : array[0..D_MAX_BLOCKS_IN_MCU-1] of JBLOCKROW;

  {$ifdef D_MULTISCAN_FILES_SUPPORTED}
    { In multi-pass modes, we need a virtual block array for each component. }
    whole_image : jvirt_barray_tbl;
  {$endif}

  {$ifdef BLOCK_SMOOTHING_SUPPORTED}
    { When doing block smoothing, we latch coefficient Al values here }
    coef_bits_latch : Latch_Ptr;
  {$endif}
  end;

{ Forward declarations }
{METHODDEF}
function decompress_onepass (cinfo : j_decompress_ptr;
                             output_buf : JSAMPIMAGE) : int; forward;
{$ifdef D_MULTISCAN_FILES_SUPPORTED}
{METHODDEF}
function decompress_data (cinfo : j_decompress_ptr;
                          output_buf : JSAMPIMAGE) : int; forward;
{$endif}
{$ifdef BLOCK_SMOOTHING_SUPPORTED}
{LOCAL}
function smoothing_ok (cinfo : j_decompress_ptr) : boolean; forward;

{METHODDEF}
function decompress_smooth_data	(cinfo : j_decompress_ptr;
                                 output_buf : JSAMPIMAGE) : int; forward;
{$endif}


{LOCAL}
procedure jdcoefct_start_iMCU_row (cinfo : j_decompress_ptr);
{ Reset within-iMCU-row counters for a new row (input side) }
var
  coef : jdcoefct_my_coef_ptr;
begin
  coef := jdcoefct_my_coef_ptr (cinfo^.coef);

  { In an interleaved scan, an MCU row is the same as an iMCU row.
    In a noninterleaved scan, an iMCU row has v_samp_factor MCU rows.
    But at the bottom of the image, process only what's left. }

  if (cinfo^.comps_in_scan > 1) then
  begin
    coef^.MCU_rows_per_iMCU_row := 1;
  end
  else
  begin
    if (cinfo^.input_iMCU_row < (cinfo^.total_iMCU_rows-1)) then
      coef^.MCU_rows_per_iMCU_row := cinfo^.cur_comp_info[0]^.v_samp_factor
    else
      coef^.MCU_rows_per_iMCU_row := cinfo^.cur_comp_info[0]^.last_row_height;
  end;

  coef^.MCU_ctr := 0;
  coef^.MCU_vert_offset := 0;
end;


{ Initialize for an input processing pass. }

{METHODDEF}
procedure jdcoefct_start_input_pass (cinfo : j_decompress_ptr);
begin
  cinfo^.input_iMCU_row := 0;
  jdcoefct_start_iMCU_row(cinfo);
end;


{ Initialize for an output processing pass. }

{METHODDEF}
procedure start_output_pass (cinfo : j_decompress_ptr);
var
  coef : jdcoefct_my_coef_ptr;
begin
{$ifdef BLOCK_SMOOTHING_SUPPORTED}
  coef := jdcoefct_my_coef_ptr (cinfo^.coef);

  { If multipass, check to see whether to use block smoothing on this pass }
  if (coef^.pub.coef_arrays <> NIL) then
  begin
    if (cinfo^.do_block_smoothing) and smoothing_ok(cinfo) then
      coef^.pub.decompress_data := decompress_smooth_data
    else
      coef^.pub.decompress_data := decompress_data;
  end;
{$endif}
  cinfo^.output_iMCU_row := 0;
end;


{ Decompress and return some data in the single-pass case.
  Always attempts to emit one fully interleaved MCU row ("iMCU" row).
  Input and output must run in lockstep since we have only a one-MCU buffer.
  Return value is JPEG_ROW_COMPLETED, JPEG_SCAN_COMPLETED, or JPEG_SUSPENDED.

  NB: output_buf contains a plane for each component in image,
  which we index according to the component's SOF position.}

{METHODDEF}
function decompress_onepass (cinfo : j_decompress_ptr;
                             output_buf : JSAMPIMAGE) : int;
var
  coef : jdcoefct_my_coef_ptr;
  MCU_col_num : JDIMENSION;     { index of current MCU within row }
  last_MCU_col : JDIMENSION;
  last_iMCU_row : JDIMENSION;
  blkn, ci, xindex, yindex, yoffset, useful_width : int;
  output_ptr : JSAMPARRAY;
  start_col, output_col : JDIMENSION;
  compptr : jpeg_component_info_ptr;
  inverse_DCT : inverse_DCT_method_ptr;
begin
  coef := jdcoefct_my_coef_ptr (cinfo^.coef);
  last_MCU_col := cinfo^.MCUs_per_row - 1;
  last_iMCU_row := cinfo^.total_iMCU_rows - 1;

  { Loop to process as much as one whole iMCU row }
  for yoffset := coef^.MCU_vert_offset to pred(coef^.MCU_rows_per_iMCU_row) do
  begin
    for MCU_col_num := coef^.MCU_ctr to last_MCU_col do
    begin
      { Try to fetch an MCU.  Entropy decoder expects buffer to be zeroed. }
      jzero_far( coef^.MCU_buffer[0],
		size_t (cinfo^.blocks_in_MCU * SIZEOF(JBLOCK)));
      if (not cinfo^.entropy^.decode_mcu (cinfo, coef^.MCU_buffer)) then
      begin
	{ Suspension forced; update state counters and exit }
	coef^.MCU_vert_offset := yoffset;
	coef^.MCU_ctr := MCU_col_num;
	decompress_onepass := JPEG_SUSPENDED;
        exit;
      end;
      { Determine where data should go in output_buf and do the IDCT thing.
        We skip dummy blocks at the right and bottom edges (but blkn gets
        incremented past them!).  Note the inner loop relies on having
        allocated the MCU_buffer[] blocks sequentially. }

      blkn := 0;			{ index of current DCT block within MCU }
      for ci := 0 to pred(cinfo^.comps_in_scan) do
      begin
	compptr := cinfo^.cur_comp_info[ci];
	{ Don't bother to IDCT an uninteresting component. }
	if (not compptr^.component_needed) then
        begin
	  Inc(blkn, compptr^.MCU_blocks);
	  continue;
	end;
	inverse_DCT := cinfo^.idct^.inverse_DCT[compptr^.component_index];
        if (MCU_col_num < last_MCU_col) then
          useful_width := compptr^.MCU_width
        else
          useful_width := compptr^.last_col_width;

	output_ptr := JSAMPARRAY(@ output_buf^[compptr^.component_index]^
                                   [yoffset * compptr^.DCT_scaled_size]);
	start_col := LongInt(MCU_col_num) * compptr^.MCU_sample_width;
	for yindex := 0 to pred(compptr^.MCU_height) do
        begin
	  if (cinfo^.input_iMCU_row < last_iMCU_row) or
             (yoffset+yindex < compptr^.last_row_height) then
          begin
	    output_col := start_col;
	    for xindex := 0 to pred(useful_width) do
            begin
	      inverse_DCT (cinfo, compptr,
			   JCOEFPTR(coef^.MCU_buffer[blkn+xindex]),
			   output_ptr, output_col);
	      Inc(output_col, compptr^.DCT_scaled_size);
	    end;
	  end;
	  Inc(blkn, compptr^.MCU_width);
	  Inc(JSAMPROW_PTR(output_ptr), compptr^.DCT_scaled_size);
	end;
      end;
    end;
    { Completed an MCU row, but perhaps not an iMCU row }
    coef^.MCU_ctr := 0;
  end;
  { Completed the iMCU row, advance counters for next one }
  Inc(cinfo^.output_iMCU_row);

  Inc(cinfo^.input_iMCU_row);
  if (cinfo^.input_iMCU_row < cinfo^.total_iMCU_rows) then
  begin
    jdcoefct_start_iMCU_row(cinfo);
    decompress_onepass := JPEG_ROW_COMPLETED;
    exit;
  end;
  { Completed the scan }
  cinfo^.inputctl^.finish_input_pass (cinfo);
  decompress_onepass := JPEG_SCAN_COMPLETED;
end;

{ Dummy consume-input routine for single-pass operation. }

{METHODDEF}
function dummy_consume_data (cinfo : j_decompress_ptr) : int;
begin
  dummy_consume_data := JPEG_SUSPENDED;	{ Always indicate nothing was done }
end;


{$ifdef D_MULTISCAN_FILES_SUPPORTED}

{ Consume input data and store it in the full-image coefficient buffer.
  We read as much as one fully interleaved MCU row ("iMCU" row) per call,
  ie, v_samp_factor block rows for each component in the scan.
  Return value is JPEG_ROW_COMPLETED, JPEG_SCAN_COMPLETED, or JPEG_SUSPENDED.}

{METHODDEF}
function consume_data (cinfo : j_decompress_ptr) : int;
var
  coef : jdcoefct_my_coef_ptr;
  MCU_col_num : JDIMENSION;     { index of current MCU within row }
  blkn, ci, xindex, yindex, yoffset : int;
  start_col : JDIMENSION;
  buffer : array[0..MAX_COMPS_IN_SCAN-1] of JBLOCKARRAY;
  buffer_ptr : JBLOCKROW;
  compptr : jpeg_component_info_ptr;
begin
  coef := jdcoefct_my_coef_ptr (cinfo^.coef);

  { Align the virtual buffers for the components used in this scan. }
  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    buffer[ci] := cinfo^.mem^.access_virt_barray
      (j_common_ptr (cinfo), coef^.whole_image[compptr^.component_index],
       LongInt(cinfo^.input_iMCU_row) * compptr^.v_samp_factor,
       JDIMENSION (compptr^.v_samp_factor), TRUE);
    { Note: entropy decoder expects buffer to be zeroed,
      but this is handled automatically by the memory manager
      because we requested a pre-zeroed array. }

  end;

  { Loop to process one whole iMCU row }
  for yoffset := coef^.MCU_vert_offset to pred(coef^.MCU_rows_per_iMCU_row) do
  begin
    for MCU_col_num := coef^.MCU_ctr to pred(cinfo^.MCUs_per_row) do
    begin
      { Construct list of pointers to DCT blocks belonging to this MCU }
      blkn := 0;		{ index of current DCT block within MCU }
      for ci := 0 to pred(cinfo^.comps_in_scan) do
      begin
	compptr := cinfo^.cur_comp_info[ci];
	start_col := LongInt(MCU_col_num) * compptr^.MCU_width;
	for yindex := 0 to pred(compptr^.MCU_height) do
        begin
	  buffer_ptr := JBLOCKROW(@ buffer[ci]^[yindex+yoffset]^[start_col]);
	  for xindex := 0 to pred(compptr^.MCU_width) do
          begin
	    coef^.MCU_buffer[blkn] := buffer_ptr;
            Inc(blkn);
            Inc(JBLOCK_PTR(buffer_ptr));
	  end;
	end;
      end;
      { Try to fetch the MCU. }
      if (not cinfo^.entropy^.decode_mcu (cinfo, coef^.MCU_buffer)) then
      begin
	{ Suspension forced; update state counters and exit }
	coef^.MCU_vert_offset := yoffset;
	coef^.MCU_ctr := MCU_col_num;
	consume_data := JPEG_SUSPENDED;
        exit;
      end;
    end;
    { Completed an MCU row, but perhaps not an iMCU row }
    coef^.MCU_ctr := 0;
  end;
  { Completed the iMCU row, advance counters for next one }
  Inc(cinfo^.input_iMCU_row);
  if (cinfo^.input_iMCU_row < cinfo^.total_iMCU_rows) then
  begin
    jdcoefct_start_iMCU_row(cinfo);
    consume_data := JPEG_ROW_COMPLETED;
    exit;
  end;
  { Completed the scan }
  cinfo^.inputctl^.finish_input_pass (cinfo);
  consume_data := JPEG_SCAN_COMPLETED;
end;


{ Decompress and return some data in the multi-pass case.
  Always attempts to emit one fully interleaved MCU row ("iMCU" row).
  Return value is JPEG_ROW_COMPLETED, JPEG_SCAN_COMPLETED, or JPEG_SUSPENDED.

  NB: output_buf contains a plane for each component in image. }

{METHODDEF}
function decompress_data (cinfo : j_decompress_ptr;
                          output_buf : JSAMPIMAGE) : int;
var
  coef : jdcoefct_my_coef_ptr;
  last_iMCU_row : JDIMENSION;
  block_num : JDIMENSION;
  ci, block_row, block_rows : int;
  buffer : JBLOCKARRAY;
  buffer_ptr : JBLOCKROW;
  output_ptr : JSAMPARRAY;
  output_col : JDIMENSION;
  compptr : jpeg_component_info_ptr;
  inverse_DCT : inverse_DCT_method_ptr;
begin
  coef := jdcoefct_my_coef_ptr (cinfo^.coef);
  last_iMCU_row := cinfo^.total_iMCU_rows - 1;

  { Force some input to be done if we are getting ahead of the input. }
  while (cinfo^.input_scan_number < cinfo^.output_scan_number) or
	 ((cinfo^.input_scan_number = cinfo^.output_scan_number) and
	  (LongInt(cinfo^.input_iMCU_row) <= cinfo^.output_iMCU_row)) do
  begin
    if (cinfo^.inputctl^.consume_input(cinfo) = JPEG_SUSPENDED) then
    begin
      decompress_data := JPEG_SUSPENDED;
      exit;
    end;
  end;

  { OK, output from the virtual arrays. }
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Don't bother to IDCT an uninteresting component. }
    if (not compptr^.component_needed) then
      continue;
    { Align the virtual buffer for this component. }
    buffer := cinfo^.mem^.access_virt_barray
      (j_common_ptr (cinfo), coef^.whole_image[ci],
       cinfo^.output_iMCU_row * compptr^.v_samp_factor,
       JDIMENSION (compptr^.v_samp_factor), FALSE);
    { Count non-dummy DCT block rows in this iMCU row. }
    if (cinfo^.output_iMCU_row < LongInt(last_iMCU_row)) then
      block_rows := compptr^.v_samp_factor
    else
    begin
      { NB: can't use last_row_height here; it is input-side-dependent! }
      block_rows := int(LongInt(compptr^.height_in_blocks) mod compptr^.v_samp_factor);
      if (block_rows = 0) then
        block_rows := compptr^.v_samp_factor;
    end;
    inverse_DCT := cinfo^.idct^.inverse_DCT[ci];
    output_ptr := output_buf^[ci];
    { Loop over all DCT blocks to be processed. }
    for block_row := 0 to pred(block_rows) do
    begin
      buffer_ptr := buffer^[block_row];
      output_col := 0;
      for block_num := 0 to pred(compptr^.width_in_blocks) do
      begin
	inverse_DCT (cinfo, compptr, JCOEFPTR (buffer_ptr),
			output_ptr, output_col);
	Inc(JBLOCK_PTR(buffer_ptr));
	Inc(output_col, compptr^.DCT_scaled_size);
      end;
      Inc(JSAMPROW_PTR(output_ptr), compptr^.DCT_scaled_size);
    end;
    Inc(compptr);
  end;

  Inc(cinfo^.output_iMCU_row);
  if (cinfo^.output_iMCU_row < LongInt(cinfo^.total_iMCU_rows)) then
  begin
    decompress_data := JPEG_ROW_COMPLETED;
    exit;
  end;
  decompress_data := JPEG_SCAN_COMPLETED;
end;

{$endif} { D_MULTISCAN_FILES_SUPPORTED }


{$ifdef BLOCK_SMOOTHING_SUPPORTED}

{ This code applies interblock smoothing as described by section K.8
  of the JPEG standard: the first 5 AC coefficients are estimated from
  the DC values of a DCT block and its 8 neighboring blocks.
  We apply smoothing only for progressive JPEG decoding, and only if
  the coefficients it can estimate are not yet known to full precision. }

{ Natural-order array positions of the first 5 zigzag-order coefficients }
const
  Q01_POS = 1;
  Q10_POS = 8;
  Q20_POS = 16;
  Q11_POS = 9;
  Q02_POS = 2;

{ Determine whether block smoothing is applicable and safe.
  We also latch the current states of the coef_bits[] entries for the
  AC coefficients; otherwise, if the input side of the decompressor
  advances into a new scan, we might think the coefficients are known
  more accurately than they really are. }

{LOCAL}
function smoothing_ok (cinfo : j_decompress_ptr) : boolean;
var
  coef : jdcoefct_my_coef_ptr;
  smoothing_useful : boolean;
  ci, coefi : int;
  compptr : jpeg_component_info_ptr;
  qtable : JQUANT_TBL_PTR;
  coef_bits : coef_bits_ptr;
  coef_bits_latch : Latch_Ptr;
begin
  coef := jdcoefct_my_coef_ptr (cinfo^.coef);
  smoothing_useful := FALSE;

  if (not cinfo^.progressive_mode) or (cinfo^.coef_bits = NIL) then
  begin
    smoothing_ok := FALSE;
    exit;
  end;

  { Allocate latch area if not already done }
  if (coef^.coef_bits_latch = NIL) then
    coef^.coef_bits_latch := Latch_Ptr(
      cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
                               cinfo^.num_components *
                               (SAVED_COEFS * SIZEOF(int))) );
  coef_bits_latch := (coef^.coef_bits_latch);

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { All components' quantization values must already be latched. }
    qtable := compptr^.quant_table;
    if (qtable = NIL) then
    begin
      smoothing_ok := FALSE;
      exit;
    end;
    { Verify DC & first 5 AC quantizers are nonzero to avoid zero-divide. }
    if (qtable^.quantval[0] = 0) or
       (qtable^.quantval[Q01_POS] = 0) or
       (qtable^.quantval[Q10_POS] = 0) or
       (qtable^.quantval[Q20_POS] = 0) or
       (qtable^.quantval[Q11_POS] = 0) or
       (qtable^.quantval[Q02_POS] = 0) then
    begin
      smoothing_ok := FALSE;
      exit;
    end;
    { DC values must be at least partly known for all components. }
    coef_bits := @cinfo^.coef_bits^[ci];  { Nomssi }
    if (coef_bits^[0] < 0) then
    begin
      smoothing_ok := FALSE;
      exit;
    end;
    { Block smoothing is helpful if some AC coefficients remain inaccurate. }
    for coefi := 1 to 5 do
    begin
      coef_bits_latch^[coefi] := coef_bits^[coefi];
      if (coef_bits^[coefi] <> 0) then
	smoothing_useful := TRUE;
    end;
    Inc(coef_bits_latch {SAVED_COEFS});
    Inc(compptr);
  end;

  smoothing_ok := smoothing_useful;
end;


{ Variant of decompress_data for use when doing block smoothing. }

{METHODDEF}
function decompress_smooth_data (cinfo : j_decompress_ptr;
                        output_buf : JSAMPIMAGE) : int;
var
  coef : jdcoefct_my_coef_ptr;
  last_iMCU_row : JDIMENSION;
  block_num, last_block_column : JDIMENSION;
  ci, block_row, block_rows, access_rows : int;
  buffer : JBLOCKARRAY;
  buffer_ptr, prev_block_row, next_block_row : JBLOCKROW;
  output_ptr : JSAMPARRAY;
  output_col : JDIMENSION;
  compptr : jpeg_component_info_ptr;
  inverse_DCT : inverse_DCT_method_ptr;
  first_row, last_row : boolean;
  workspace : JBLOCK;
  coef_bits : Latch_Ptr; { coef_bits_ptr;  }
  quanttbl : JQUANT_TBL_PTR;
  Q00,Q01,Q02,Q10,Q11,Q20, num : INT32;
  DC1,DC2,DC3,DC4,DC5,DC6,DC7,DC8,DC9 : int;
  Al, pred : int;
var
  delta : JDIMENSION;
begin
  coef := jdcoefct_my_coef_ptr (cinfo^.coef);
  last_iMCU_row := cinfo^.total_iMCU_rows - 1;

  { Force some input to be done if we are getting ahead of the input. }
  while (cinfo^.input_scan_number <= cinfo^.output_scan_number) and
        (not cinfo^.inputctl^.eoi_reached) do
  begin
    if (cinfo^.input_scan_number = cinfo^.output_scan_number) then
    begin
      { If input is working on current scan, we ordinarily want it to
        have completed the current row.  But if input scan is DC,
        we want it to keep one row ahead so that next block row's DC
        values are up to date. }

      if (cinfo^.Ss = 0) then
        delta := 1
      else
        delta := 0;
      if (LongInt(cinfo^.input_iMCU_row) > cinfo^.output_iMCU_row+LongInt(delta)) then
	break;
    end;
    if (cinfo^.inputctl^.consume_input(cinfo) = JPEG_SUSPENDED) then
    begin
      decompress_smooth_data := JPEG_SUSPENDED;
      exit;
    end;
  end;

  { OK, output from the virtual arrays. }
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to (cinfo^.num_components-1) do
  begin
    { Don't bother to IDCT an uninteresting component. }
    if (not compptr^.component_needed) then
      continue;
    { Count non-dummy DCT block rows in this iMCU row. }
    if (cinfo^.output_iMCU_row < LongInt(last_iMCU_row)) then
    begin
      block_rows := compptr^.v_samp_factor;
      access_rows := block_rows * 2; { this and next iMCU row }
      last_row := FALSE;
    end
    else
    begin
      { NB: can't use last_row_height here; it is input-side-dependent! }
      block_rows := int (compptr^.height_in_blocks) mod compptr^.v_samp_factor;
      if (block_rows = 0) then
        block_rows := compptr^.v_samp_factor;
      access_rows := block_rows; { this iMCU row only }
      last_row := TRUE;
    end;
    { Align the virtual buffer for this component. }
    if (cinfo^.output_iMCU_row > 0) then
    begin
      Inc(access_rows, compptr^.v_samp_factor); { prior iMCU row too }
      buffer := cinfo^.mem^.access_virt_barray
	(j_common_ptr (cinfo), coef^.whole_image[ci],
	 (cinfo^.output_iMCU_row - 1) * compptr^.v_samp_factor,
	 JDIMENSION (access_rows), FALSE);
      Inc(JBLOCKROW_PTR(buffer), compptr^.v_samp_factor); { point to current iMCU row }
      first_row := FALSE;
    end
    else
    begin
      buffer := cinfo^.mem^.access_virt_barray
	(j_common_ptr (cinfo), coef^.whole_image[ci],
	 JDIMENSION (0), JDIMENSION (access_rows), FALSE);
      first_row := TRUE;
    end;
    { Fetch component-dependent info }
    coef_bits := coef^.coef_bits_latch;
    Inc(coef_bits,  ci);                        { ci * SAVED_COEFS}
    quanttbl := compptr^.quant_table;
    Q00 := quanttbl^.quantval[0];
    Q01 := quanttbl^.quantval[Q01_POS];
    Q10 := quanttbl^.quantval[Q10_POS];
    Q20 := quanttbl^.quantval[Q20_POS];
    Q11 := quanttbl^.quantval[Q11_POS];
    Q02 := quanttbl^.quantval[Q02_POS];
    inverse_DCT := cinfo^.idct^.inverse_DCT[ci];
    output_ptr := output_buf^[ci];
    { Loop over all DCT blocks to be processed. }
    for block_row := 0 to (block_rows-1) do
    begin
      buffer_ptr := buffer^[block_row];
      if (first_row) and (block_row = 0) then
	prev_block_row := buffer_ptr
      else
	prev_block_row := buffer^[block_row-1];
      if (last_row) and (block_row = block_rows-1) then
	next_block_row := buffer_ptr
      else
	next_block_row := buffer^[block_row+1];
      { We fetch the surrounding DC values using a sliding-register approach.
        Initialize all nine here so as to do the right thing on narrow pics.}

      DC3 := int(prev_block_row^[0][0]);
      DC2 := DC3;
      DC1 := DC2;
      DC6 := int(buffer_ptr^[0][0]);
      DC5 := DC6;
      DC4 := DC5;
      DC9 := int(next_block_row^[0][0]);
      DC8 := DC9;
      DC7 := DC8 ;
      output_col := 0;
      last_block_column := compptr^.width_in_blocks - 1;
      for block_num := 0 to last_block_column do
      begin
	{ Fetch current DCT block into workspace so we can modify it. }
	jcopy_block_row(buffer_ptr, JBLOCKROW (@workspace), JDIMENSION(1));
	{ Update DC values }
	if (block_num < last_block_column) then
        begin
	  DC3 := int (prev_block_row^[1][0]);
	  DC6 := int (buffer_ptr^[1][0]);
	  DC9 := int (next_block_row^[1][0]);
	end;
	{ Compute coefficient estimates per K.8.
	  An estimate is applied only if coefficient is still zero,
	  and is not known to be fully accurate. }

	{ AC01 }
	Al := coef_bits^[1];
	if (Al <> 0) and (workspace[1] = 0) then
        begin
	  num := 36 * Q00 * (DC4 - DC6);
	  if (num >= 0) then
          begin
	    pred := int (((Q01 shl 7) + num) div (Q01 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	  end
          else
          begin
	    pred := int (((Q01 shl 7) - num) div (Q01 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	    pred := -pred;
	  end;
	  workspace[1] := JCOEF (pred);
	end;
	{ AC10 }
	Al := coef_bits^[2];
	if (Al <> 0) and (workspace[8] = 0) then
        begin
	  num := 36 * Q00 * (DC2 - DC8);
	  if (num >= 0) then
          begin
	    pred := int (((Q10 shl 7) + num) div (Q10 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	  end
          else
          begin
	    pred := int (((Q10 shl 7) - num) div (Q10 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	    pred := -pred;
	  end;
	  workspace[8] := JCOEF (pred);
	end;
	{ AC20 }
	Al := coef_bits^[3];
	if (Al <> 0) and (workspace[16] = 0) then
        begin
	  num := 9 * Q00 * (DC2 + DC8 - 2*DC5);
	  if (num >= 0) then
          begin
	    pred := int (((Q20 shl 7) + num) div (Q20 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	  end
          else
          begin
	    pred := int (((Q20 shl 7) - num) div (Q20 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	    pred := -pred;
	  end;
	  workspace[16] := JCOEF (pred);
	end;
	{ AC11 }
	Al := coef_bits^[4];
	if (Al <> 0) and (workspace[9] = 0) then
        begin
	  num := 5 * Q00 * (DC1 - DC3 - DC7 + DC9);
	  if (num >= 0) then
          begin
	    pred := int (((Q11 shl 7) + num) div (Q11 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	  end
          else
          begin
	    pred := int (((Q11 shl 7) - num) div (Q11 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	    pred := -pred;
	  end;
	  workspace[9] := JCOEF (pred);
	end;
	{ AC02 }
	Al := coef_bits^[5];
	if (Al <> 0) and (workspace[2] = 0) then
        begin
	  num := 9 * Q00 * (DC4 + DC6 - 2*DC5);
	  if (num >= 0) then
          begin
	    pred := int (((Q02 shl 7) + num) div (Q02 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	  end
          else
          begin
	    pred := int (((Q02 shl 7) - num) div (Q02 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	    pred := -pred;
	  end;
	  workspace[2] := JCOEF (pred);
	end;
	{ OK, do the IDCT }
	inverse_DCT (cinfo, compptr, JCOEFPTR (@workspace),
			output_ptr, output_col);
	{ Advance for next column }
	DC1 := DC2; DC2 := DC3;
	DC4 := DC5; DC5 := DC6;
	DC7 := DC8; DC8 := DC9;
	Inc(JBLOCK_PTR(buffer_ptr));
        Inc(JBLOCK_PTR(prev_block_row));
        Inc(JBLOCK_PTR(next_block_row));
	Inc(output_col, compptr^.DCT_scaled_size);
      end;
      Inc(JSAMPROW_PTR(output_ptr), compptr^.DCT_scaled_size);
    end;
    Inc(compptr);
  end;

  Inc(cinfo^.output_iMCU_row);
  if (cinfo^.output_iMCU_row < LongInt(cinfo^.total_iMCU_rows)) then
  begin
    decompress_smooth_data := JPEG_ROW_COMPLETED;
    exit;
  end;
  decompress_smooth_data := JPEG_SCAN_COMPLETED;
end;

{$endif} { BLOCK_SMOOTHING_SUPPORTED }


{ Initialize coefficient buffer controller. }

{GLOBAL}
procedure jinit_d_coef_controller (cinfo : j_decompress_ptr;
                                   need_full_buffer : boolean);
var
  coef : jdcoefct_my_coef_ptr;
{$ifdef D_MULTISCAN_FILES_SUPPORTED}
var
  ci, access_rows : int;
  compptr : jpeg_component_info_ptr;
{$endif}
var
  buffer : JBLOCK_PTR;
  i : int;
begin
  coef := jdcoefct_my_coef_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
				SIZEOF(jdcoefct_my_coef_controller)) );
  cinfo^.coef := jpeg_d_coef_controller_ptr(coef);
  coef^.pub.start_input_pass := jdcoefct_start_input_pass;
  coef^.pub.start_output_pass := start_output_pass;
{$ifdef BLOCK_SMOOTHING_SUPPORTED}
  coef^.coef_bits_latch := NIL;
{$endif}

  { Create the coefficient buffer. }
  if (need_full_buffer) then
  begin
{$ifdef D_MULTISCAN_FILES_SUPPORTED}
    { Allocate a full-image virtual array for each component, }
    { padded to a multiple of samp_factor DCT blocks in each direction. }
    { Note we ask for a pre-zeroed array. }

    compptr := jpeg_component_info_ptr(cinfo^.comp_info);
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      access_rows := compptr^.v_samp_factor;
{$ifdef BLOCK_SMOOTHING_SUPPORTED}
      { If block smoothing could be used, need a bigger window }
      if (cinfo^.progressive_mode) then
	access_rows := access_rows * 3;
{$endif}
      coef^.whole_image[ci] := cinfo^.mem^.request_virt_barray
	(j_common_ptr (cinfo), JPOOL_IMAGE, TRUE,
	 JDIMENSION (jround_up( long(compptr^.width_in_blocks),
                                long(compptr^.h_samp_factor) )),
	 JDIMENSION (jround_up( long(compptr^.height_in_blocks),
				long(compptr^.v_samp_factor) )),
	 JDIMENSION (access_rows));
      Inc(compptr);
    end;
    coef^.pub.consume_data := consume_data;
    coef^.pub.decompress_data := decompress_data;
    coef^.pub.coef_arrays := @(coef^.whole_image);
                          { link to virtual arrays }
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
  end
  else
  begin
    { We only need a single-MCU buffer. }
    buffer := JBLOCK_PTR (
      cinfo^.mem^.alloc_large (j_common_ptr (cinfo), JPOOL_IMAGE,
				  D_MAX_BLOCKS_IN_MCU * SIZEOF(JBLOCK)) );
    for i := 0 to pred(D_MAX_BLOCKS_IN_MCU) do
    begin
      coef^.MCU_buffer[i] := JBLOCKROW(buffer);
      Inc(buffer);
    end;
    coef^.pub.consume_data := dummy_consume_data;
    coef^.pub.decompress_data := decompress_onepass;
    coef^.pub.coef_arrays := NIL; { flag for no virtual arrays }
  end;
end;

{== imjdcolor implementation ==}

{ Private subobject }
type
  jdcolor_int_Color_Table = array[0..MAXJSAMPLE+1-1] of int;
  int_table_ptr = ^jdcolor_int_Color_Table;
  jdcolor_INT32_Color_Table = array[0..MAXJSAMPLE+1-1] of INT32;
  INT32_table_ptr = ^jdcolor_INT32_Color_Table;
type
  jdcolor_my_cconvert_ptr = ^my_color_deconverter;
  my_color_deconverter = record
    pub : jpeg_color_deconverter; { public fields }

    { Private state for YCC^.RGB conversion }
    Cr_r_tab : int_table_ptr;	{ => table for Cr to R conversion }
    Cb_b_tab : int_table_ptr;	{ => table for Cb to B conversion }
    Cr_g_tab : INT32_table_ptr;	{ => table for Cr to G conversion }
    Cb_g_tab : INT32_table_ptr;	{ => table for Cb to G conversion }
  end;


{*************** YCbCr ^. RGB conversion: most common case *************}

{ YCbCr is defined per CCIR 601-1, except that Cb and Cr are
  normalized to the range 0..MAXJSAMPLE rather than -0.5 .. 0.5.
  The conversion equations to be implemented are therefore
 	R = Y                + 1.40200 * Cr
 	G = Y - 0.34414 * Cb - 0.71414 * Cr
 	B = Y + 1.77200 * Cb
  where Cb and Cr represent the incoming values less CENTERJSAMPLE.
  (These numbers are derived from TIFF 6.0 section 21, dated 3-June-92.)

  To avoid floating-point arithmetic, we represent the fractional constants
  as integers scaled up by 2^16 (about 4 digits precision); we have to divide
  the products by 2^16, with appropriate rounding, to get the correct answer.
  Notice that Y, being an integral input, does not contribute any fraction
  so it need not participate in the rounding.

  For even more speed, we avoid doing any multiplications in the inner loop
  by precalculating the constants times Cb and Cr for all possible values.
  For 8-bit JSAMPLEs this is very reasonable (only 256 entries per table);
  for 12-bit samples it is still acceptable.  It's not very reasonable for
  16-bit samples, but if you want lossless storage you shouldn't be changing
  colorspace anyway.
  The Cr=>R and Cb=>B values can be rounded to integers in advance; the
  values for the G calculation are left scaled up, since we must add them
  together before rounding. }

const
  jdcolor_SCALEBITS = 16;      { speediest right-shift on some machines }
  jdcolor_ONE_HALF  = (INT32(1) shl (jdcolor_SCALEBITS-1));


{ Initialize tables for YCC->RGB colorspace conversion. }

{LOCAL}
procedure jdcolor_build_ycc_rgb_table (cinfo : j_decompress_ptr);
const
  FIX_1_40200 = INT32(Round( 1.40200  * (1 shl jdcolor_SCALEBITS)));
  FIX_1_77200 = INT32(Round( 1.77200  * (1 shl jdcolor_SCALEBITS)));
  FIX_0_71414 = INT32(Round( 0.71414  * (1 shl jdcolor_SCALEBITS)));
  FIX_0_34414 = INT32(Round( 0.34414  * (1 shl jdcolor_SCALEBITS)));

var
  cconvert : jdcolor_my_cconvert_ptr;
  i : int;
  x : INT32;
var
  shift_temp : INT32;
begin
  cconvert := jdcolor_my_cconvert_ptr (cinfo^.cconvert);


  cconvert^.Cr_r_tab := int_table_ptr(
    cinfo^.mem^.alloc_small ( j_common_ptr(cinfo), JPOOL_IMAGE,
				(MAXJSAMPLE+1) * SIZEOF(int)) );
  cconvert^.Cb_b_tab := int_table_ptr (
    cinfo^.mem^.alloc_small ( j_common_ptr(cinfo), JPOOL_IMAGE,
				(MAXJSAMPLE+1) * SIZEOF(int)) );
  cconvert^.Cr_g_tab := INT32_table_ptr (
    cinfo^.mem^.alloc_small ( j_common_ptr(cinfo), JPOOL_IMAGE,
				(MAXJSAMPLE+1) * SIZEOF(INT32)) );
  cconvert^.Cb_g_tab := INT32_table_ptr (
    cinfo^.mem^.alloc_small ( j_common_ptr(cinfo), JPOOL_IMAGE,
				(MAXJSAMPLE+1) * SIZEOF(INT32)) );


  x := -CENTERJSAMPLE;
  for i := 0 to MAXJSAMPLE do
  begin
    { i is the actual input pixel value, in the range 0..MAXJSAMPLE }
    { The Cb or Cr value we are thinking of is x := i - CENTERJSAMPLE }
    { Cr=>R value is nearest int to 1.40200 * x }

    shift_temp := FIX_1_40200  * x + jdcolor_ONE_HALF;
    if shift_temp < 0 then  { SHIFT arithmetic RIGHT }
      cconvert^.Cr_r_tab^[i] := int((shift_temp shr jdcolor_SCALEBITS)
                             or ( (not INT32(0)) shl (32-jdcolor_SCALEBITS)))
    else
      cconvert^.Cr_r_tab^[i] := int(shift_temp shr jdcolor_SCALEBITS);

    { Cb=>B value is nearest int to 1.77200 * x }
    shift_temp := FIX_1_77200  * x + jdcolor_ONE_HALF;
    if shift_temp < 0 then   { SHIFT arithmetic RIGHT }
      cconvert^.Cb_b_tab^[i] := int((shift_temp shr jdcolor_SCALEBITS)
                                or ( (not INT32(0)) shl (32-jdcolor_SCALEBITS)))
    else
      cconvert^.Cb_b_tab^[i] := int(shift_temp shr jdcolor_SCALEBITS);

    { Cr=>G value is scaled-up -0.71414 * x }
    cconvert^.Cr_g_tab^[i] := (- FIX_0_71414 ) * x;
    { Cb=>G value is scaled-up -0.34414 * x }
    { We also add in ONE_HALF so that need not do it in inner loop }
    cconvert^.Cb_g_tab^[i] := (- FIX_0_34414 ) * x + jdcolor_ONE_HALF;
    Inc(x);
  end;
end;


{ Convert some rows of samples to the output colorspace.

  Note that we change from noninterleaved, one-plane-per-component format
  to interleaved-pixel format.  The output buffer is therefore three times
  as wide as the input buffer.
  A starting row offset is provided only for the input buffer.  The caller
  can easily adjust the passed output_buf value to accommodate any row
  offset required on that side. }

{METHODDEF}
procedure ycc_rgb_convert (cinfo : j_decompress_ptr;
		           input_buf : JSAMPIMAGE;
                           input_row : JDIMENSION;
                           output_buf : JSAMPARRAY;
                           num_rows : int);
var
  cconvert : jdcolor_my_cconvert_ptr;
  {register} y, cb, cr : int;
  {register} outptr : JSAMPROW;
  {register} inptr0, inptr1, inptr2 : JSAMPROW;
  {register} col : JDIMENSION;
  num_cols : JDIMENSION;
  { copy these pointers into registers if possible }
  {register} range_limit : range_limit_table_ptr;
  {register} Crrtab : int_table_ptr;
  {register} Cbbtab : int_table_ptr;
  {register} Crgtab : INT32_table_ptr;
  {register} Cbgtab : INT32_table_ptr;
var
  shift_temp : INT32;
begin
  cconvert := jdcolor_my_cconvert_ptr (cinfo^.cconvert);
  num_cols := cinfo^.output_width;
  range_limit := cinfo^.sample_range_limit;
  Crrtab := cconvert^.Cr_r_tab;
  Cbbtab := cconvert^.Cb_b_tab;
  Crgtab := cconvert^.Cr_g_tab;
  Cbgtab := cconvert^.Cb_g_tab;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    inptr0 := input_buf^[0]^[input_row];
    inptr1 := input_buf^[1]^[input_row];
    inptr2 := input_buf^[2]^[input_row];
    Inc(input_row);
    outptr := output_buf^[0];
    Inc(JSAMPROW_PTR(output_buf));
    for col := 0 to pred(num_cols) do
    begin
      y  := GETJSAMPLE(inptr0^[col]);
      cb := GETJSAMPLE(inptr1^[col]);
      cr := GETJSAMPLE(inptr2^[col]);
      { Range-limiting is essential due to noise introduced by DCT losses. }
      outptr^[RGB_RED] :=   range_limit^[y + Crrtab^[cr]];
      shift_temp := Cbgtab^[cb] + Crgtab^[cr];
      if shift_temp < 0 then   { SHIFT arithmetic RIGHT }
        outptr^[RGB_GREEN] := range_limit^[y + int((shift_temp shr jdcolor_SCALEBITS)
                              or ( (not INT32(0)) shl (32-jdcolor_SCALEBITS)))]
      else
        outptr^[RGB_GREEN] := range_limit^[y + int(shift_temp shr jdcolor_SCALEBITS)];

      outptr^[RGB_BLUE] :=  range_limit^[y + Cbbtab^[cb]];
      Inc(JSAMPLE_PTR(outptr), RGB_PIXELSIZE);
    end;
  end;
end;


{*************** Cases other than YCbCr -> RGB *************}


{ Color conversion for no colorspace change: just copy the data,
  converting from separate-planes to interleaved representation. }

{METHODDEF}
procedure jdcolor_null_convert (cinfo : j_decompress_ptr;
	                input_buf : JSAMPIMAGE;
                        input_row : JDIMENSION;
	                output_buf : JSAMPARRAY;
                        num_rows : int);
var
  {register} inptr,
             outptr : JSAMPLE_PTR;
  {register} count : JDIMENSION;
  {register} num_components : int;
  num_cols : JDIMENSION;
  ci : int;
begin
  num_components := cinfo^.num_components;
  num_cols := cinfo^.output_width;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    for ci := 0 to pred(num_components) do
    begin
      inptr := JSAMPLE_PTR(input_buf^[ci]^[input_row]);
      outptr := JSAMPLE_PTR(@(output_buf^[0]^[ci]));

      for count := pred(num_cols) downto 0 do
      begin
	outptr^ := inptr^;	{ needn't bother with GETJSAMPLE() here }
        Inc(inptr);
	Inc(outptr, num_components);
      end;
    end;
    Inc(input_row);
    Inc(JSAMPROW_PTR(output_buf));
  end;
end;


{ Color conversion for grayscale: just copy the data.
  This also works for YCbCr -> grayscale conversion, in which
  we just copy the Y (luminance) component and ignore chrominance. }

{METHODDEF}
procedure jdcolor_grayscale_convert (cinfo : j_decompress_ptr;
	                     input_buf : JSAMPIMAGE;
                             input_row : JDIMENSION;
		             output_buf : JSAMPARRAY;
                             num_rows : int);
begin
  jcopy_sample_rows(input_buf^[0], int(input_row), output_buf, 0,
		    num_rows, cinfo^.output_width);
end;

{ Convert grayscale to RGB: just duplicate the graylevel three times.
  This is provided to support applications that don't want to cope
  with grayscale as a separate case. }

{METHODDEF}
procedure gray_rgb_convert (cinfo : j_decompress_ptr;
	                    input_buf : JSAMPIMAGE;
                            input_row : JDIMENSION;
		            output_buf : JSAMPARRAY;
                            num_rows : int);
var
  {register} inptr, outptr : JSAMPLE_PTR;
  {register} col : JDIMENSION;
  num_cols : JDIMENSION;
begin
  num_cols := cinfo^.output_width;
  while (num_rows > 0) do
  begin
    inptr := JSAMPLE_PTR(input_buf^[0]^[input_row]);
    Inc(input_row);
    outptr := JSAMPLE_PTR(@output_buf^[0]);
    Inc(JSAMPROW_PTR(output_buf));
    for col := 0 to pred(num_cols) do
    begin
      { We can dispense with GETJSAMPLE() here }
      JSAMPROW(outptr)^[RGB_RED] := inptr^;
      JSAMPROW(outptr)^[RGB_GREEN] := inptr^;
      JSAMPROW(outptr)^[RGB_BLUE] := inptr^;
      Inc(inptr);
      Inc(outptr, RGB_PIXELSIZE);
    end;
    Dec(num_rows);
  end;
end;


{ Adobe-style YCCK -> CMYK conversion.
  We convert YCbCr to R=1-C, G=1-M, and B=1-Y using the same
  conversion as above, while passing K (black) unchanged.
  We assume build_ycc_rgb_table has been called. }

{METHODDEF}
procedure ycck_cmyk_convert (cinfo : j_decompress_ptr;
		             input_buf : JSAMPIMAGE;
                             input_row : JDIMENSION;
                             output_buf : JSAMPARRAY;
                             num_rows : int);
var
  cconvert : jdcolor_my_cconvert_ptr;
  {register} y, cb, cr : int;
  {register} outptr : JSAMPROW;
  {register} inptr0, inptr1, inptr2, inptr3 : JSAMPROW;
  {register} col : JDIMENSION;
  num_cols : JDIMENSION;
  { copy these pointers into registers if possible }
  {register} range_limit : range_limit_table_ptr;
  {register} Crrtab : int_table_ptr;
  {register} Cbbtab : int_table_ptr;
  {register} Crgtab : INT32_table_ptr;
  {register} Cbgtab : INT32_table_ptr;
var
  shift_temp : INT32;
begin
  cconvert := jdcolor_my_cconvert_ptr (cinfo^.cconvert);
  num_cols := cinfo^.output_width;
  { copy these pointers into registers if possible }
  range_limit := cinfo^.sample_range_limit;
  Crrtab := cconvert^.Cr_r_tab;
  Cbbtab := cconvert^.Cb_b_tab;
  Crgtab := cconvert^.Cr_g_tab;
  Cbgtab := cconvert^.Cb_g_tab;

  while (num_rows > 0) do
  begin
    Dec(num_rows);
    inptr0 := input_buf^[0]^[input_row];
    inptr1 := input_buf^[1]^[input_row];
    inptr2 := input_buf^[2]^[input_row];
    inptr3 := input_buf^[3]^[input_row];
    Inc(input_row);
    outptr := output_buf^[0];
    Inc(JSAMPROW_PTR(output_buf));
    for col := 0 to pred(num_cols) do
    begin
      y  := GETJSAMPLE(inptr0^[col]);
      cb := GETJSAMPLE(inptr1^[col]);
      cr := GETJSAMPLE(inptr2^[col]);
      { Range-limiting is essential due to noise introduced by DCT losses. }
      outptr^[0] := range_limit^[MAXJSAMPLE - (y + Crrtab^[cr])];	{ red }
      shift_temp := Cbgtab^[cb] + Crgtab^[cr];
      if shift_temp < 0 then
        outptr^[1] := range_limit^[MAXJSAMPLE - (y + int(
          (shift_temp shr jdcolor_SCALEBITS) or ((not INT32(0)) shl (32-jdcolor_SCALEBITS))
                                                        ) )]
      else
        outptr^[1] := range_limit^[MAXJSAMPLE -             { green }
                    (y + int(shift_temp shr jdcolor_SCALEBITS) )];
      outptr^[2] := range_limit^[MAXJSAMPLE - (y + Cbbtab^[cb])];	{ blue }
      { K passes through unchanged }
      outptr^[3] := inptr3^[col];	{ don't need GETJSAMPLE here }
      Inc(JSAMPLE_PTR(outptr), 4);
    end;
  end;
end;


{ Empty method for start_pass. }

{METHODDEF}
procedure start_pass_dcolor (cinfo : j_decompress_ptr);
begin
  { no work needed }
end;


{ Module initialization routine for output colorspace conversion. }

{GLOBAL}
procedure jinit_color_deconverter (cinfo : j_decompress_ptr);
var
  cconvert : jdcolor_my_cconvert_ptr;
  ci : int;
begin
  cconvert := jdcolor_my_cconvert_ptr (
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(my_color_deconverter)) );
  cinfo^.cconvert := jpeg_color_deconverter_ptr (cconvert);
  cconvert^.pub.start_pass := start_pass_dcolor;

  { Make sure num_components agrees with jpeg_color_space }
  case (cinfo^.jpeg_color_space) of
  JCS_GRAYSCALE:
    if (cinfo^.num_components <> 1) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);

  JCS_RGB,
  JCS_YCbCr:
    if (cinfo^.num_components <> 3) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);

  JCS_CMYK,
  JCS_YCCK:
    if (cinfo^.num_components <> 4) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);

  else                     { JCS_UNKNOWN can be anything }
    if (cinfo^.num_components < 1) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_J_COLORSPACE);
  end;

  { Set out_color_components and conversion method based on requested space.
    Also clear the component_needed flags for any unused components,
    so that earlier pipeline stages can avoid useless computation. }

  case (cinfo^.out_color_space) of
  JCS_GRAYSCALE:
    begin
      cinfo^.out_color_components := 1;
      if (cinfo^.jpeg_color_space = JCS_GRAYSCALE)
        or (cinfo^.jpeg_color_space = JCS_YCbCr) then
      begin
        cconvert^.pub.color_convert := jdcolor_grayscale_convert;
        { For color -> grayscale conversion, only the
          Y (0) component is needed }
        for ci := 1 to pred(cinfo^.num_components) do
	  cinfo^.comp_info^[ci].component_needed := FALSE;
      end
      else
        ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  JCS_RGB:
    begin
      cinfo^.out_color_components := RGB_PIXELSIZE;
      if (cinfo^.jpeg_color_space = JCS_YCbCr) then
      begin
        cconvert^.pub.color_convert := ycc_rgb_convert;
        jdcolor_build_ycc_rgb_table(cinfo);
      end
      else
        if (cinfo^.jpeg_color_space = JCS_GRAYSCALE) then
        begin
          cconvert^.pub.color_convert := gray_rgb_convert;
        end
        else
          if (cinfo^.jpeg_color_space = JCS_RGB) and (RGB_PIXELSIZE = 3) then
          begin
            cconvert^.pub.color_convert := jdcolor_null_convert;
          end
          else
            ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  JCS_CMYK:
    begin
      cinfo^.out_color_components := 4;
      if (cinfo^.jpeg_color_space = JCS_YCCK) then
      begin
        cconvert^.pub.color_convert := ycck_cmyk_convert;
        jdcolor_build_ycc_rgb_table(cinfo);
      end
      else
        if (cinfo^.jpeg_color_space = JCS_CMYK) then
        begin
          cconvert^.pub.color_convert := jdcolor_null_convert;
        end
        else
          ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;

  else
    begin { Permit null conversion to same output space }
      if (cinfo^.out_color_space = cinfo^.jpeg_color_space) then
      begin
        cinfo^.out_color_components := cinfo^.num_components;
        cconvert^.pub.color_convert := jdcolor_null_convert;
      end
      else			{ unsupported non-null conversion }
        ERREXIT(j_common_ptr(cinfo), JERR_CONVERSION_NOTIMPL);
    end;
  end;

  if (cinfo^.quantize_colors) then
    cinfo^.output_components := 1 { single colormapped output component }
  else
    cinfo^.output_components := cinfo^.out_color_components;
end;

{== imjdsample implementation ==}

{ Private subobject }

type
  jdsample_my_upsample_ptr = ^jdsample_my_upsampler;
  jdsample_my_upsampler = record
    pub : jpeg_upsampler;	{ public fields }

    { Color conversion buffer.  When using separate upsampling and color
      conversion steps, this buffer holds one upsampled row group until it
      has been color converted and output.
      Note: we do not allocate any storage for component(s) which are full-size,
      ie do not need rescaling.  The corresponding entry of color_buf[] is
      simply set to point to the input data array, thereby avoiding copying.}

    color_buf : array[0..MAX_COMPONENTS-1] of JSAMPARRAY;

    { Per-component upsampling method pointers }
    methods : array[0..MAX_COMPONENTS-1] of upsample1_ptr;

    next_row_out : int;         { counts rows emitted from color_buf }
    rows_to_go : JDIMENSION;    { counts rows remaining in image }

    { Height of an input row group for each component. }
    rowgroup_height : array[0..MAX_COMPONENTS-1] of int;

    { These arrays save pixel expansion factors so that int_expand need not
      recompute them each time.  They are unused for other upsampling methods.}
    h_expand : array[0..MAX_COMPONENTS-1] of UINT8 ;
    v_expand : array[0..MAX_COMPONENTS-1] of UINT8 ;
  end;


{ Initialize for an upsampling pass. }

{METHODDEF}
procedure start_pass_upsample (cinfo : j_decompress_ptr);
var
  upsample : jdsample_my_upsample_ptr;
begin
  upsample := jdsample_my_upsample_ptr (cinfo^.upsample);

  { Mark the conversion buffer empty }
  upsample^.next_row_out := cinfo^.max_v_samp_factor;
  { Initialize total-height counter for detecting bottom of image }
  upsample^.rows_to_go := cinfo^.output_height;
end;


{ Control routine to do upsampling (and color conversion).

  In this version we upsample each component independently.
  We upsample one row group into the conversion buffer, then apply
  color conversion a row at a time. }

{METHODDEF}
procedure sep_upsample (cinfo : j_decompress_ptr;
	                input_buf : JSAMPIMAGE;
                        var in_row_group_ctr : JDIMENSION;
	                in_row_groups_avail : JDIMENSION;
	                output_buf : JSAMPARRAY;
                        var out_row_ctr : JDIMENSION;
	                out_rows_avail : JDIMENSION);
var
  upsample : jdsample_my_upsample_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
  num_rows : JDIMENSION;
begin
  upsample := jdsample_my_upsample_ptr (cinfo^.upsample);

  { Fill the conversion buffer, if it's empty }
  if (upsample^.next_row_out >= cinfo^.max_v_samp_factor) then
  begin
    compptr := jpeg_component_info_ptr(cinfo^.comp_info);
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      { Invoke per-component upsample method.  Notice we pass a POINTER
        to color_buf[ci], so that fullsize_upsample can change it. }

      upsample^.methods[ci] (cinfo, compptr,
	JSAMPARRAY(@ input_buf^[ci]^
           [LongInt(in_row_group_ctr) * upsample^.rowgroup_height[ci]]),
	upsample^.color_buf[ci]);

      Inc(compptr);
    end;
    upsample^.next_row_out := 0;
  end;

  { Color-convert and emit rows }

  { How many we have in the buffer: }
  num_rows := JDIMENSION (cinfo^.max_v_samp_factor - upsample^.next_row_out);
  { Not more than the distance to the end of the image.  Need this test
    in case the image height is not a multiple of max_v_samp_factor: }

  if (num_rows > upsample^.rows_to_go) then
    num_rows := upsample^.rows_to_go;
  { And not more than what the client can accept: }
  Dec(out_rows_avail, out_row_ctr);
  if (num_rows > out_rows_avail) then
    num_rows := out_rows_avail;

  cinfo^.cconvert^.color_convert (cinfo,
                                 JSAMPIMAGE(@(upsample^.color_buf)),
	                         JDIMENSION (upsample^.next_row_out),
				 JSAMPARRAY(@(output_buf^[out_row_ctr])),
				 int (num_rows));

  { Adjust counts }
  Inc(out_row_ctr, num_rows);
  Dec(upsample^.rows_to_go, num_rows);
  Inc(upsample^.next_row_out, num_rows);
  { When the buffer is emptied, declare this input row group consumed }
  if (upsample^.next_row_out >= cinfo^.max_v_samp_factor) then
    Inc(in_row_group_ctr);
end;


{ These are the routines invoked by sep_upsample to upsample pixel values
  of a single component.  One row group is processed per call. }


{ For full-size components, we just make color_buf[ci] point at the
  input buffer, and thus avoid copying any data.  Note that this is
  safe only because sep_upsample doesn't declare the input row group
  "consumed" until we are done color converting and emitting it. }

{METHODDEF}
procedure fullsize_upsample (cinfo : j_decompress_ptr;
                             compptr : jpeg_component_info_ptr;
		             input_data : JSAMPARRAY;
                             var output_data_ptr : JSAMPARRAY);
begin
  output_data_ptr := input_data;
end;


{ This is a no-op version used for "uninteresting" components.
  These components will not be referenced by color conversion. }

{METHODDEF}
procedure noop_upsample (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
	                 input_data : JSAMPARRAY;
                         var output_data_ptr : JSAMPARRAY);
begin
  output_data_ptr := NIL;	{ safety check }
end;


{ This version handles any integral sampling ratios.
  This is not used for typical JPEG files, so it need not be fast.
  Nor, for that matter, is it particularly accurate: the algorithm is
  simple replication of the input pixel onto the corresponding output
  pixels.  The hi-falutin sampling literature refers to this as a
  "box filter".  A box filter tends to introduce visible artifacts,
  so if you are actually going to use 3:1 or 4:1 sampling ratios
  you would be well advised to improve this code. }

{METHODDEF}
procedure int_upsample (cinfo : j_decompress_ptr;
                        compptr : jpeg_component_info_ptr;
	                input_data : JSAMPARRAY;
                        var output_data_ptr : JSAMPARRAY);
var
  upsample : jdsample_my_upsample_ptr;
  output_data : JSAMPARRAY;
  {register} inptr, outptr : JSAMPLE_PTR;
  {register} invalue : JSAMPLE;
  {register} h : int;
  {outend}
  h_expand, v_expand : int;
  inrow, outrow : int;
var
  outcount : int;  { Nomssi: avoid pointer arithmetic }
begin
  upsample := jdsample_my_upsample_ptr (cinfo^.upsample);
  output_data := output_data_ptr;

  h_expand := upsample^.h_expand[compptr^.component_index];
  v_expand := upsample^.v_expand[compptr^.component_index];

  inrow := 0;
  outrow := 0;
  while (outrow < cinfo^.max_v_samp_factor) do
  begin
    { Generate one output row with proper horizontal expansion }
    inptr := JSAMPLE_PTR(input_data^[inrow]);
    outptr := JSAMPLE_PTR(output_data^[outrow]);
    outcount := cinfo^.output_width;
    while (outcount > 0) do     { Nomssi }
    begin
      invalue := inptr^;	{ don't need GETJSAMPLE() here }
      Inc(inptr);
      for h := pred(h_expand) downto 0 do
      begin
	outptr^ := invalue;
        inc(outptr);       { <-- fix: this was left out in PasJpeg 1.0 }
        Dec(outcount);        { thanks to Jannie Gerber for the report }
      end;
    end;

    { Generate any additional output rows by duplicating the first one }
    if (v_expand > 1) then
    begin
      jcopy_sample_rows(output_data, outrow, output_data, outrow+1,
			v_expand-1, cinfo^.output_width);
    end;
    Inc(inrow);
    Inc(outrow, v_expand);
  end;
end;


{ Fast processing for the common case of 2:1 horizontal and 1:1 vertical.
  It's still a box filter. }

{METHODDEF}
procedure h2v1_upsample (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
	                 input_data : JSAMPARRAY;
                         var output_data_ptr : JSAMPARRAY);
var
  output_data : JSAMPARRAY;
  {register} inptr, outptr : JSAMPLE_PTR;
  {register} invalue : JSAMPLE;
  {outend : JSAMPROW;}
  outcount : int;
  inrow : int;
begin
  output_data := output_data_ptr;

  for inrow := 0 to pred(cinfo^.max_v_samp_factor) do
  begin
    inptr := JSAMPLE_PTR(input_data^[inrow]);
    outptr := JSAMPLE_PTR(output_data^[inrow]);
    {outend := outptr + cinfo^.output_width;}
    outcount := cinfo^.output_width;
    while (outcount > 0) do
    begin
      invalue := inptr^;	{ don't need GETJSAMPLE() here }
      Inc(inptr);
      outptr^ := invalue;
      Inc(outptr);
      outptr^ := invalue;
      Inc(outptr);
      Dec(outcount, 2);         { Nomssi: to avoid pointer arithmetic }
    end;
  end;
end;


{ Fast processing for the common case of 2:1 horizontal and 2:1 vertical.
  It's still a box filter. }

{METHODDEF}
procedure h2v2_upsample (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
	                 input_data : JSAMPARRAY;
                         var output_data_ptr : JSAMPARRAY);
var
  output_data : JSAMPARRAY;
  {register} inptr, outptr : JSAMPLE_PTR;
  {register} invalue : JSAMPLE;
  {outend : JSAMPROW;}
  outcount : int;
  inrow, outrow : int;
begin
  output_data := output_data_ptr;

  inrow := 0;
  outrow := 0;
  while (outrow < cinfo^.max_v_samp_factor) do
  begin
    inptr := JSAMPLE_PTR(input_data^[inrow]);
    outptr := JSAMPLE_PTR(output_data^[outrow]);
    {outend := outptr + cinfo^.output_width;}
    outcount := cinfo^.output_width;
    while (outcount > 0) do
    begin
      invalue := inptr^;	{ don't need GETJSAMPLE() here }
      Inc(inptr);
      outptr^ := invalue;
      Inc(outptr);
      outptr^ := invalue;
      Inc(outptr);
      Dec(outcount, 2);
    end;
    jcopy_sample_rows(output_data, outrow, output_data, outrow+1,
		      1, cinfo^.output_width);
    Inc(inrow);
    Inc(outrow, 2);
  end;
end;


{ Fancy processing for the common case of 2:1 horizontal and 1:1 vertical.

  The upsampling algorithm is linear interpolation between pixel centers,
  also known as a "triangle filter".  This is a good compromise between
  speed and visual quality.  The centers of the output pixels are 1/4 and 3/4
  of the way between input pixel centers.

  A note about the "bias" calculations: when rounding fractional values to
  integer, we do not want to always round 0.5 up to the next integer.
  If we did that, we'd introduce a noticeable bias towards larger values.
  Instead, this code is arranged so that 0.5 will be rounded up or down at
  alternate pixel locations (a simple ordered dither pattern). }

{METHODDEF}
procedure h2v1_fancy_upsample (cinfo : j_decompress_ptr;
                               compptr : jpeg_component_info_ptr;
                               input_data : JSAMPARRAY;
                               var output_data_ptr : JSAMPARRAY);
var
  output_data : JSAMPARRAY;
  {register} pre_inptr, inptr, outptr : JSAMPLE_PTR;
  {register} invalue : int;
  {register} colctr : JDIMENSION;
  inrow : int;
begin
  output_data := output_data_ptr;

  for inrow := 0 to pred(cinfo^.max_v_samp_factor) do
  begin
    inptr := JSAMPLE_PTR(input_data^[inrow]);
    outptr := JSAMPLE_PTR(output_data^[inrow]);
    { Special case for first column }
    pre_inptr := inptr;
    invalue := GETJSAMPLE(inptr^);
    Inc(inptr);
    outptr^ := JSAMPLE (invalue);
    Inc(outptr);
    outptr^ := JSAMPLE ((invalue * 3 + GETJSAMPLE(inptr^) + 2) shr 2);
    Inc(outptr);

    for colctr := pred(compptr^.downsampled_width - 2) downto 0 do
    begin
      { General case: 3/4 * nearer pixel + 1/4 * further pixel }
      invalue := GETJSAMPLE(inptr^) * 3;
      Inc(inptr);
      outptr^ := JSAMPLE ((invalue + GETJSAMPLE(pre_inptr^) + 1) shr 2);
      Inc(pre_inptr);
      Inc(outptr);
      outptr^ := JSAMPLE ((invalue + GETJSAMPLE(inptr^) + 2) shr 2);
      Inc(outptr);
    end;

    { Special case for last column }
    invalue := GETJSAMPLE(inptr^);
    outptr^ := JSAMPLE ((invalue * 3 + GETJSAMPLE(pre_inptr^) + 1) shr 2);
    Inc(outptr);
    outptr^ := JSAMPLE (invalue);
    {Inc(outptr);                        - value never used }
  end;
end;


{ Fancy processing for the common case of 2:1 horizontal and 2:1 vertical.
  Again a triangle filter; see comments for h2v1 case, above.

  It is OK for us to reference the adjacent input rows because we demanded
  context from the main buffer controller (see initialization code). }

{METHODDEF}
procedure h2v2_fancy_upsample (cinfo : j_decompress_ptr;
                               compptr : jpeg_component_info_ptr;
		               input_data : JSAMPARRAY;
                               var output_data_ptr : JSAMPARRAY);
var
  output_data : JSAMPARRAY;
  {register} inptr0, inptr1, outptr : JSAMPLE_PTR;
{$ifdef BITS_IN_JSAMPLE_IS_8}
  {register} thiscolsum, lastcolsum, nextcolsum : int;
{$else}
  {register} thiscolsum, lastcolsum, nextcolsum : INT32;
{$endif}
  {register} colctr : JDIMENSION;
  inrow, outrow, v : int;
var
  prev_input_data : JSAMPARRAY;  { Nomssi work around }
begin
  output_data := output_data_ptr;

  outrow := 0;
  inrow := 0;
  while (outrow < cinfo^.max_v_samp_factor) do
  begin
    for v := 0 to pred(2) do
    begin
      { inptr0 points to nearest input row, inptr1 points to next nearest }
      inptr0 := JSAMPLE_PTR(input_data^[inrow]);
      if (v = 0) then         { next nearest is row above }
      begin
	{inptr1 := JSAMPLE_PTR(input_data^[inrow-1]);}
        prev_input_data := input_data;       { work around }
        Dec(JSAMPROW_PTR(prev_input_data));  { negative offsets }
	inptr1 := JSAMPLE_PTR(prev_input_data^[inrow]);
      end
      else                    { next nearest is row below }
	inptr1 := JSAMPLE_PTR(input_data^[inrow+1]);
      outptr := JSAMPLE_PTR(output_data^[outrow]);
      Inc(outrow);

      { Special case for first column }
      thiscolsum := GETJSAMPLE(inptr0^) * 3 + GETJSAMPLE(inptr1^);
      Inc(inptr0);
      Inc(inptr1);
      nextcolsum := GETJSAMPLE(inptr0^) * 3 + GETJSAMPLE(inptr1^);
      Inc(inptr0);
      Inc(inptr1);

      outptr^ := JSAMPLE ((thiscolsum * 4 + 8) shr 4);
      Inc(outptr);
      outptr^ := JSAMPLE ((thiscolsum * 3 + nextcolsum + 7) shr 4);
      Inc(outptr);
      lastcolsum := thiscolsum; thiscolsum := nextcolsum;

      for colctr := pred(compptr^.downsampled_width - 2) downto 0 do
      begin
	{ General case: 3/4 * nearer pixel + 1/4 * further pixel in each }
	{ dimension, thus 9/16, 3/16, 3/16, 1/16 overall }
	nextcolsum := GETJSAMPLE(inptr0^) * 3 + GETJSAMPLE(inptr1^);
        Inc(inptr0);
        Inc(inptr1);
	outptr^ := JSAMPLE ((thiscolsum * 3 + lastcolsum + 8) shr 4);
        Inc(outptr);
	outptr^ := JSAMPLE ((thiscolsum * 3 + nextcolsum + 7) shr 4);
        Inc(outptr);
	lastcolsum := thiscolsum;
        thiscolsum := nextcolsum;
      end;

      { Special case for last column }
      outptr^ := JSAMPLE ((thiscolsum * 3 + lastcolsum + 8) shr 4);
      Inc(outptr);
      outptr^ := JSAMPLE ((thiscolsum * 4 + 7) shr 4);
      {Inc(outptr);                     - value never used }
    end;
    Inc(inrow);
  end;
end;


{ Module initialization routine for upsampling. }

{GLOBAL}
procedure jinit_upsampler (cinfo : j_decompress_ptr);
var
  upsample : jdsample_my_upsample_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
  need_buffer, do_fancy : boolean;
  h_in_group, v_in_group, h_out_group, v_out_group : int;
begin
  upsample := jdsample_my_upsample_ptr (
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(jdsample_my_upsampler)) );
  cinfo^.upsample := jpeg_upsampler_ptr (upsample);
  upsample^.pub.start_pass := start_pass_upsample;
  upsample^.pub.upsample := sep_upsample;
  upsample^.pub.need_context_rows := FALSE; { until we find out differently }

  if (cinfo^.CCIR601_sampling)	then        { this isn't supported }
    ERREXIT(j_common_ptr(cinfo), JERR_CCIR601_NOTIMPL);

  { jdmainct.c doesn't support context rows when min_DCT_scaled_size := 1,
    so don't ask for it. }

  do_fancy := cinfo^.do_fancy_upsampling and (cinfo^.min_DCT_scaled_size > 1);

  { Verify we can handle the sampling factors, select per-component methods,
    and create storage as needed. }

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Compute size of an "input group" after IDCT scaling.  This many samples
      are to be converted to max_h_samp_factor * max_v_samp_factor pixels. }

    h_in_group := (compptr^.h_samp_factor * compptr^.DCT_scaled_size) div
		 cinfo^.min_DCT_scaled_size;
    v_in_group := (compptr^.v_samp_factor * compptr^.DCT_scaled_size) div
		 cinfo^.min_DCT_scaled_size;
    h_out_group := cinfo^.max_h_samp_factor;
    v_out_group := cinfo^.max_v_samp_factor;
    upsample^.rowgroup_height[ci] := v_in_group; { save for use later }
    need_buffer := TRUE;
    if (not compptr^.component_needed) then
    begin
      { Don't bother to upsample an uninteresting component. }
      upsample^.methods[ci] := noop_upsample;
      need_buffer := FALSE;
    end
    else
      if (h_in_group = h_out_group) and (v_in_group = v_out_group) then
      begin
        { Fullsize components can be processed without any work. }
        upsample^.methods[ci] := fullsize_upsample;
        need_buffer := FALSE;
      end
      else
        if (h_in_group * 2 = h_out_group) and
	         (v_in_group = v_out_group) then
        begin
        { Special cases for 2h1v upsampling }
          if (do_fancy) and (compptr^.downsampled_width > 2) then
	    upsample^.methods[ci] := h2v1_fancy_upsample
          else
	    upsample^.methods[ci] := h2v1_upsample;
        end
        else
          if (h_in_group * 2 = h_out_group) and
	           (v_in_group * 2 = v_out_group) then
          begin
            { Special cases for 2h2v upsampling }
            if (do_fancy) and (compptr^.downsampled_width > 2) then
            begin
	      upsample^.methods[ci] := h2v2_fancy_upsample;
	      upsample^.pub.need_context_rows := TRUE;
            end
            else
	      upsample^.methods[ci] := h2v2_upsample;
          end
          else
            if ((h_out_group mod h_in_group) = 0) and
	             ((v_out_group mod v_in_group) = 0) then
            begin
              { Generic integral-factors upsampling method }
              upsample^.methods[ci] := int_upsample;
              upsample^.h_expand[ci] := UINT8 (h_out_group div h_in_group);
              upsample^.v_expand[ci] := UINT8 (v_out_group div v_in_group);
            end
            else
              ERREXIT(j_common_ptr(cinfo), JERR_FRACT_SAMPLE_NOTIMPL);
    if (need_buffer) then
    begin
      upsample^.color_buf[ci] := cinfo^.mem^.alloc_sarray
	(j_common_ptr(cinfo), JPOOL_IMAGE,
	 JDIMENSION (jround_up( long (cinfo^.output_width),
				long (cinfo^.max_h_samp_factor))),
	 JDIMENSION (cinfo^.max_v_samp_factor));
    end;
    Inc(compptr);
  end;
end;

{== imjdmerge implementation ==}

{ Private subobject }

type  { the same definition as in JdColor }
  jdmerge_int_Color_Table = array[0..MAXJSAMPLE+1-1] of int;
  int_CConvertPtr = ^jdmerge_int_Color_Table;
  jdmerge_INT32_Color_Table = array[0..MAXJSAMPLE+1-1] of INT32;
  INT32_CConvertPtr = ^jdmerge_INT32_Color_Table;

type
  jdmerge_my_upsample_ptr = ^jdmerge_my_upsampler;
    jdmerge_my_upsampler = record
    pub : jpeg_upsampler;	{ public fields }

    { Pointer to routine to do actual upsampling/conversion of one row group }
    upmethod : procedure (cinfo : j_decompress_ptr;
			  input_buf : JSAMPIMAGE;
                          in_row_group_ctr : JDIMENSION;
			  output_buf : JSAMPARRAY);

    { Private state for YCC->RGB conversion }
    Cr_r_tab : int_CConvertPtr;		{ => table for Cr to R conversion }
    Cb_b_tab : int_CConvertPtr;		{ => table for Cb to B conversion }
    Cr_g_tab : INT32_CConvertPtr;	{ => table for Cr to G conversion }
    Cb_g_tab : INT32_CConvertPtr;	{ => table for Cb to G conversion }

    { For 2:1 vertical sampling, we produce two output rows at a time.
      We need a "spare" row buffer to hold the second output row if the
      application provides just a one-row buffer; we also use the spare
      to discard the dummy last row if the image height is odd. }

    spare_row : JSAMPROW;
    spare_full : boolean;		{ TRUE if spare buffer is occupied }

    out_row_width : JDIMENSION;	{ samples per output row }
    rows_to_go : JDIMENSION;	{ counts rows remaining in image }
  end; {my_upsampler;}


const
  jdmerge_SCALEBITS = 16;	{ speediest right-shift on some machines }
  jdmerge_ONE_HALF  = (INT32(1) shl (jdmerge_SCALEBITS-1));


{ Initialize tables for YCC->RGB colorspace conversion.
  This is taken directly from jdcolor.c; see that file for more info. }

{LOCAL}
procedure jdmerge_build_ycc_rgb_table (cinfo : j_decompress_ptr);
const
  FIX_1_40200 = INT32( Round(1.40200 * (INT32(1) shl jdmerge_SCALEBITS)) );
  FIX_1_77200 = INT32( Round(1.77200 * (INT32(1) shl jdmerge_SCALEBITS)) );
  FIX_0_71414 = INT32( Round(0.71414 * (INT32(1) shl jdmerge_SCALEBITS)) );
  FIX_0_34414 = INT32( Round(0.34414 * (INT32(1) shl jdmerge_SCALEBITS)) );
var
  upsample : jdmerge_my_upsample_ptr;
  i : int;
  x : INT32;
var
  shift_temp : INT32;
begin
  upsample := jdmerge_my_upsample_ptr (cinfo^.upsample);

  upsample^.Cr_r_tab := int_CConvertPtr (
    cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
				(MAXJSAMPLE+1) * SIZEOF(int)) );
  upsample^.Cb_b_tab := int_CConvertPtr (
    cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
				(MAXJSAMPLE+1) * SIZEOF(int)) );
  upsample^.Cr_g_tab := INT32_CConvertPtr (
    cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
				(MAXJSAMPLE+1) * SIZEOF(INT32)) );
  upsample^.Cb_g_tab := INT32_CConvertPtr (
    cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
				(MAXJSAMPLE+1) * SIZEOF(INT32)) );

  x := -CENTERJSAMPLE;
  for i := 0 to pred(MAXJSAMPLE) do
  begin
    { i is the actual input pixel value, in the range 0..MAXJSAMPLE }
    { The Cb or Cr value we are thinking of is x := i - CENTERJSAMPLE }
    { Cr=>R value is nearest int to 1.40200 * x }
    {upsample^.Cr_r_tab^[i] := int(
		    RIGHT_SHIFT(FIX_1_40200 * x + ONE_HALF, SCALEBITS) );}
    shift_temp := FIX_1_40200  * x + jdmerge_ONE_HALF;
    if shift_temp < 0 then  { SHIFT arithmetic RIGHT }
      upsample^.Cr_r_tab^[i] := int((shift_temp shr jdmerge_SCALEBITS)
                             or ( (not INT32(0)) shl (32-jdmerge_SCALEBITS)))
    else
      upsample^.Cr_r_tab^[i] := int(shift_temp shr jdmerge_SCALEBITS);


    { Cb=>B value is nearest int to 1.77200 * x }
    {upsample^.Cb_b_tab^[i] := int(
		    RIGHT_SHIFT(FIX_1_77200 * x + ONE_HALF, SCALEBITS) );}
    shift_temp := FIX_1_77200 * x + jdmerge_ONE_HALF;
    if shift_temp < 0 then  { SHIFT arithmetic RIGHT }
      upsample^.Cb_b_tab^[i] := int((shift_temp shr jdmerge_SCALEBITS)
                             or ( (not INT32(0)) shl (32-jdmerge_SCALEBITS)))
    else
      upsample^.Cb_b_tab^[i] := int(shift_temp shr jdmerge_SCALEBITS);

    { Cr=>G value is scaled-up -0.71414 * x }
    upsample^.Cr_g_tab^[i] := (- FIX_0_71414) * x;
    { Cb=>G value is scaled-up -0.34414 * x }
    { We also add in ONE_HALF so that need not do it in inner loop }
    upsample^.Cb_g_tab^[i] := (- FIX_0_34414) * x + jdmerge_ONE_HALF;
    Inc(x);
  end;
end;


{ Initialize for an upsampling pass. }

{METHODDEF}
procedure start_pass_merged_upsample (cinfo : j_decompress_ptr);
var
  upsample : jdmerge_my_upsample_ptr;
begin
  upsample := jdmerge_my_upsample_ptr (cinfo^.upsample);

  { Mark the spare buffer empty }
  upsample^.spare_full := FALSE;
  { Initialize total-height counter for detecting bottom of image }
  upsample^.rows_to_go := cinfo^.output_height;
end;


{ Control routine to do upsampling (and color conversion).

  The control routine just handles the row buffering considerations. }

{METHODDEF}
procedure merged_2v_upsample (cinfo : j_decompress_ptr;
		              input_buf : JSAMPIMAGE;
                              var in_row_group_ctr : JDIMENSION;
                              in_row_groups_avail : JDIMENSION;
                              output_buf : JSAMPARRAY;
                              var out_row_ctr : JDIMENSION;
                              out_rows_avail : JDIMENSION);
{ 2:1 vertical sampling case: may need a spare row. }
var
  upsample : jdmerge_my_upsample_ptr;
  work_ptrs : array[0..2-1] of JSAMPROW;
  num_rows : JDIMENSION;		{ number of rows returned to caller }
begin
  upsample := jdmerge_my_upsample_ptr (cinfo^.upsample);

  if (upsample^.spare_full) then
  begin
    { If we have a spare row saved from a previous cycle, just return it. }
    jcopy_sample_rows(JSAMPARRAY(@upsample^.spare_row),
                      0,
                      JSAMPARRAY(@ output_buf^[out_row_ctr]),
                      0, 1, upsample^.out_row_width);
    num_rows := 1;
    upsample^.spare_full := FALSE;
  end
  else
  begin
    { Figure number of rows to return to caller. }
    num_rows := 2;
    { Not more than the distance to the end of the image. }
    if (num_rows > upsample^.rows_to_go) then
      num_rows := upsample^.rows_to_go;
    { And not more than what the client can accept: }
    Dec(out_rows_avail, {var} out_row_ctr);
    if (num_rows > out_rows_avail) then
      num_rows := out_rows_avail;
    { Create output pointer array for upsampler. }
    work_ptrs[0] := output_buf^[out_row_ctr];
    if (num_rows > 1) then
    begin
      work_ptrs[1] := output_buf^[out_row_ctr + 1];
    end
    else
    begin
      work_ptrs[1] := upsample^.spare_row;
      upsample^.spare_full := TRUE;
    end;
    { Now do the upsampling. }
    upsample^.upmethod (cinfo, input_buf, {var}in_row_group_ctr,
                        JSAMPARRAY(@work_ptrs));
  end;

  { Adjust counts }
  Inc(out_row_ctr, num_rows);
  Dec(upsample^.rows_to_go, num_rows);
  { When the buffer is emptied, declare this input row group consumed }
  if (not upsample^.spare_full) then
    Inc(in_row_group_ctr);
end;


{METHODDEF}
procedure merged_1v_upsample (cinfo : j_decompress_ptr;
		             input_buf : JSAMPIMAGE;
                             var in_row_group_ctr : JDIMENSION;
		             in_row_groups_avail : JDIMENSION;
		             output_buf : JSAMPARRAY;
                             var out_row_ctr : JDIMENSION;
		             out_rows_avail : JDIMENSION);
{ 1:1 vertical sampling case: much easier, never need a spare row. }
var
  upsample : jdmerge_my_upsample_ptr;
begin
  upsample := jdmerge_my_upsample_ptr (cinfo^.upsample);

  { Just do the upsampling. }
  upsample^.upmethod (cinfo, input_buf, in_row_group_ctr,
			 JSAMPARRAY(@ output_buf^[out_row_ctr]));
  { Adjust counts }
  Inc(out_row_ctr);
  Inc(in_row_group_ctr);
end;


{ These are the routines invoked by the control routines to do
  the actual upsampling/conversion.  One row group is processed per call.

  Note: since we may be writing directly into application-supplied buffers,
  we have to be honest about the output width; we can't assume the buffer
  has been rounded up to an even width. }


{ Upsample and color convert for the case of 2:1 horizontal and 1:1 vertical. }

{METHODDEF}
procedure h2v1_merged_upsample (cinfo : j_decompress_ptr;
		                input_buf : JSAMPIMAGE;
                                in_row_group_ctr : JDIMENSION;
		                output_buf : JSAMPARRAY);
var
  upsample : jdmerge_my_upsample_ptr;
  {register} y, cred, cgreen, cblue : int;
  cb, cr : int;
  {register}  outptr : JSAMPROW;
  inptr0, inptr1, inptr2 : JSAMPLE_PTR;
  col : JDIMENSION;
  { copy these pointers into registers if possible }
  {register} range_limit : range_limit_table_ptr;
  Crrtab : int_CConvertPtr;
  Cbbtab : int_CConvertPtr;
  Crgtab : INT32_CConvertPtr;
  Cbgtab : INT32_CConvertPtr;
var
  shift_temp : INT32;
begin
  upsample := jdmerge_my_upsample_ptr (cinfo^.upsample);
  range_limit := cinfo^.sample_range_limit;
  Crrtab := upsample^.Cr_r_tab;
  Cbbtab := upsample^.Cb_b_tab;
  Crgtab := upsample^.Cr_g_tab;
  Cbgtab := upsample^.Cb_g_tab;

  inptr0 := JSAMPLE_PTR(input_buf^[0]^[in_row_group_ctr]);
  inptr1 := JSAMPLE_PTR(input_buf^[1]^[in_row_group_ctr]);
  inptr2 := JSAMPLE_PTR(input_buf^[2]^[in_row_group_ctr]);
  outptr := output_buf^[0];
  { Loop for each pair of output pixels }
  for col := pred(cinfo^.output_width shr 1) downto 0 do
  begin
    { Do the chroma part of the calculation }
    cb := GETJSAMPLE(inptr1^);
    Inc(inptr1);
    cr := GETJSAMPLE(inptr2^);
    Inc(inptr2);
    cred := Crrtab^[cr];
    {cgreen := int( RIGHT_SHIFT(Cbgtab[cb] + Crgtab[cr], SCALEBITS) );}
    shift_temp := Cbgtab^[cb] + Crgtab^[cr];
    if shift_temp < 0 then   { SHIFT arithmetic RIGHT }
      cgreen := int((shift_temp shr jdmerge_SCALEBITS)
                            or ( (not INT32(0)) shl (32-jdmerge_SCALEBITS)))
    else
      cgreen := int(shift_temp shr jdmerge_SCALEBITS);

    cblue := Cbbtab^[cb];
    { Fetch 2 Y values and emit 2 pixels }
    y  := GETJSAMPLE(inptr0^);
    Inc(inptr0);
    outptr^[RGB_RED] :=   range_limit^[y + cred];
    outptr^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr^[RGB_BLUE] :=  range_limit^[y + cblue];
    Inc(JSAMPLE_PTR(outptr), RGB_PIXELSIZE);
    y  := GETJSAMPLE(inptr0^);
    Inc(inptr0);
    outptr^[RGB_RED] :=   range_limit^[y + cred];
    outptr^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr^[RGB_BLUE] :=  range_limit^[y + cblue];
    Inc(JSAMPLE_PTR(outptr), RGB_PIXELSIZE);
  end;
  { If image width is odd, do the last output column separately }
  if Odd(cinfo^.output_width) then
  begin
    cb := GETJSAMPLE(inptr1^);
    cr := GETJSAMPLE(inptr2^);
    cred := Crrtab^[cr];
    {cgreen := int ( RIGHT_SHIFT(Cbgtab[cb] + Crgtab[cr], SCALEBITS) );}
    shift_temp := Cbgtab^[cb] + Crgtab^[cr];
    if shift_temp < 0 then   { SHIFT arithmetic RIGHT }
      cgreen := int((shift_temp shr jdmerge_SCALEBITS)
                            or ( (not INT32(0)) shl (32-jdmerge_SCALEBITS)))
    else
      cgreen := int(shift_temp shr jdmerge_SCALEBITS);

    cblue := Cbbtab^[cb];
    y  := GETJSAMPLE(inptr0^);
    outptr^[RGB_RED] :=   range_limit^[y + cred];
    outptr^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr^[RGB_BLUE] :=  range_limit^[y + cblue];
  end;
end;


{ Upsample and color convert for the case of 2:1 horizontal and 2:1 vertical. }

{METHODDEF}
procedure h2v2_merged_upsample (cinfo : j_decompress_ptr;
		                input_buf : JSAMPIMAGE;
                                in_row_group_ctr : JDIMENSION;
		                output_buf : JSAMPARRAY);
var
  upsample : jdmerge_my_upsample_ptr;
  {register} y, cred, cgreen, cblue : int;
  cb, cr : int;
  {register} outptr0, outptr1 : JSAMPROW;
  inptr00, inptr01, inptr1, inptr2 : JSAMPLE_PTR;
  col : JDIMENSION;
  { copy these pointers into registers if possible }
  {register} range_limit : range_limit_table_ptr;
  Crrtab : int_CConvertPtr;
  Cbbtab : int_CConvertPtr;
  Crgtab : INT32_CConvertPtr;
  Cbgtab : INT32_CConvertPtr;
var
  shift_temp : INT32;
begin
  upsample := jdmerge_my_upsample_ptr (cinfo^.upsample);
  range_limit := cinfo^.sample_range_limit;
  Crrtab := upsample^.Cr_r_tab;
  Cbbtab := upsample^.Cb_b_tab;
  Crgtab := upsample^.Cr_g_tab;
  Cbgtab := upsample^.Cb_g_tab;

  inptr00 := JSAMPLE_PTR(input_buf^[0]^[in_row_group_ctr*2]);
  inptr01 := JSAMPLE_PTR(input_buf^[0]^[in_row_group_ctr*2 + 1]);
  inptr1 := JSAMPLE_PTR(input_buf^[1]^[in_row_group_ctr]);
  inptr2 := JSAMPLE_PTR(input_buf^[2]^[in_row_group_ctr]);
  outptr0 := output_buf^[0];
  outptr1 := output_buf^[1];
  { Loop for each group of output pixels }
  for col := pred(cinfo^.output_width shr 1) downto 0 do
  begin
    { Do the chroma part of the calculation }
    cb := GETJSAMPLE(inptr1^);
    Inc(inptr1);
    cr := GETJSAMPLE(inptr2^);
    Inc(inptr2);
    cred := Crrtab^[cr];
    {cgreen := int( RIGHT_SHIFT(Cbgtab[cb] + Crgtab[cr], SCALEBITS) );}
    shift_temp := Cbgtab^[cb] + Crgtab^[cr];
    if shift_temp < 0 then   { SHIFT arithmetic RIGHT }
      cgreen := int((shift_temp shr jdmerge_SCALEBITS)
                            or ( (not INT32(0)) shl (32-jdmerge_SCALEBITS)))
    else
      cgreen := int(shift_temp shr jdmerge_SCALEBITS);

    cblue := Cbbtab^[cb];
    { Fetch 4 Y values and emit 4 pixels }
    y  := GETJSAMPLE(inptr00^);
    Inc(inptr00);
    outptr0^[RGB_RED] :=   range_limit^[y + cred];
    outptr0^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr0^[RGB_BLUE] :=  range_limit^[y + cblue];
    Inc(JSAMPLE_PTR(outptr0), RGB_PIXELSIZE);
    y  := GETJSAMPLE(inptr00^);
    Inc(inptr00);
    outptr0^[RGB_RED] :=   range_limit^[y + cred];
    outptr0^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr0^[RGB_BLUE] :=  range_limit^[y + cblue];
    Inc(JSAMPLE_PTR(outptr0), RGB_PIXELSIZE);
    y  := GETJSAMPLE(inptr01^);
    Inc(inptr01);
    outptr1^[RGB_RED] :=   range_limit^[y + cred];
    outptr1^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr1^[RGB_BLUE] :=  range_limit^[y + cblue];
    Inc(JSAMPLE_PTR(outptr1), RGB_PIXELSIZE);
    y  := GETJSAMPLE(inptr01^);
    Inc(inptr01);
    outptr1^[RGB_RED] :=   range_limit^[y + cred];
    outptr1^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr1^[RGB_BLUE] :=  range_limit^[y + cblue];
    Inc(JSAMPLE_PTR(outptr1), RGB_PIXELSIZE);
  end;
  { If image width is odd, do the last output column separately }
  if Odd(cinfo^.output_width) then
  begin
    cb := GETJSAMPLE(inptr1^);
    cr := GETJSAMPLE(inptr2^);
    cred := Crrtab^[cr];
    {cgreen := int (RIGHT_SHIFT(Cbgtab[cb] + Crgtab[cr], SCALEBITS));}
    shift_temp := Cbgtab^[cb] + Crgtab^[cr];
    if shift_temp < 0 then   { SHIFT arithmetic RIGHT }
      cgreen := int((shift_temp shr jdmerge_SCALEBITS)
                            or ( (not INT32(0)) shl (32-jdmerge_SCALEBITS)))
    else
      cgreen := int(shift_temp shr jdmerge_SCALEBITS);

    cblue := Cbbtab^[cb];
    y  := GETJSAMPLE(inptr00^);
    outptr0^[RGB_RED] :=   range_limit^[y + cred];
    outptr0^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr0^[RGB_BLUE] :=  range_limit^[y + cblue];
    y  := GETJSAMPLE(inptr01^);
    outptr1^[RGB_RED] :=   range_limit^[y + cred];
    outptr1^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr1^[RGB_BLUE] :=  range_limit^[y + cblue];
  end;
end;


{ Module initialization routine for merged upsampling/color conversion.

  NB: this is called under the conditions determined by use_merged_upsample()
  in jdmaster.c.  That routine MUST correspond to the actual capabilities
  of this module; no safety checks are made here. }


{GLOBAL}
procedure jinit_merged_upsampler (cinfo : j_decompress_ptr);
var
  upsample : jdmerge_my_upsample_ptr;
begin
  upsample := jdmerge_my_upsample_ptr (
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(jdmerge_my_upsampler)) );
  cinfo^.upsample := jpeg_upsampler_ptr (upsample);
  upsample^.pub.start_pass := start_pass_merged_upsample;
  upsample^.pub.need_context_rows := FALSE;

  upsample^.out_row_width := cinfo^.output_width * JDIMENSION(cinfo^.out_color_components);

  if (cinfo^.max_v_samp_factor = 2) then
  begin
    upsample^.pub.upsample := merged_2v_upsample;
    upsample^.upmethod := h2v2_merged_upsample;
    { Allocate a spare row buffer }
    upsample^.spare_row := JSAMPROW(
      cinfo^.mem^.alloc_large ( j_common_ptr(cinfo), JPOOL_IMAGE,
		size_t (upsample^.out_row_width * SIZEOF(JSAMPLE))) );
  end
  else
  begin
    upsample^.pub.upsample := merged_1v_upsample;
    upsample^.upmethod := h2v1_merged_upsample;
    { No spare row needed }
    upsample^.spare_row := NIL;
  end;

  jdmerge_build_ycc_rgb_table(cinfo);
end;

{== imjdpostct implementation ==}

{ Private buffer controller object }

type
  my_post_ptr = ^my_post_controller;
  my_post_controller = record
    pub : jpeg_d_post_controller; { public fields }

    { Color quantization source buffer: this holds output data from
      the upsample/color conversion step to be passed to the quantizer.
      For two-pass color quantization, we need a full-image buffer;
      for one-pass operation, a strip buffer is sufficient. }

    whole_image : jvirt_sarray_ptr;   { virtual array, or NIL if one-pass }
    buffer : JSAMPARRAY;		{ strip buffer, or current strip of virtual }
    strip_height : JDIMENSION;	{ buffer size in rows }
    { for two-pass mode only: }
    starting_row : JDIMENSION;	{ row # of first row in current strip }
    next_row : JDIMENSION;		{ index of next row to fill/empty in strip }
  end;

{ Forward declarations }
{METHODDEF}
procedure post_process_1pass(cinfo : j_decompress_ptr;
		             input_buf : JSAMPIMAGE;
                             var in_row_group_ctr : JDIMENSION;
		             in_row_groups_avail : JDIMENSION;
		             output_buf : JSAMPARRAY;
                             var out_row_ctr : JDIMENSION;
		             out_rows_avail : JDIMENSION); forward;
{$ifdef QUANT_2PASS_SUPPORTED}
{METHODDEF}
procedure post_process_prepass(cinfo : j_decompress_ptr;
		               input_buf : JSAMPIMAGE;
                               var in_row_group_ctr : JDIMENSION;
		               in_row_groups_avail : JDIMENSION;
		               output_buf : JSAMPARRAY;
                               var out_row_ctr : JDIMENSION;
		               out_rows_avail : JDIMENSION);  forward;
{METHODDEF}
procedure post_process_2pass(cinfo : j_decompress_ptr;
 		             input_buf : JSAMPIMAGE;
                             var in_row_group_ctr : JDIMENSION;
		             in_row_groups_avail : JDIMENSION;
		             output_buf : JSAMPARRAY;
                             var out_row_ctr : JDIMENSION;
		             out_rows_avail : JDIMENSION);  forward;
{$endif}


{ Initialize for a processing pass. }

{METHODDEF}
procedure start_pass_dpost (cinfo : j_decompress_ptr;
                            pass_mode : J_BUF_MODE);
var
  post : my_post_ptr;
begin
  post := my_post_ptr(cinfo^.post);

  case (pass_mode) of
  JBUF_PASS_THRU:
    if (cinfo^.quantize_colors) then
    begin
      { Single-pass processing with color quantization. }
      post^.pub.post_process_data := post_process_1pass;
      { We could be doing buffered-image output before starting a 2-pass
        color quantization; in that case, jinit_d_post_controller did not
        allocate a strip buffer.  Use the virtual-array buffer as workspace. }
      if (post^.buffer = NIL) then
      begin
	post^.buffer := cinfo^.mem^.access_virt_sarray
	  (j_common_ptr(cinfo), post^.whole_image,
	   JDIMENSION(0), post^.strip_height, TRUE);
      end;
    end
    else
    begin
      { For single-pass processing without color quantization,
        I have no work to do; just call the upsampler directly. }

      post^.pub.post_process_data := cinfo^.upsample^.upsample;
    end;

{$ifdef QUANT_2PASS_SUPPORTED}
  JBUF_SAVE_AND_PASS:
    begin
      { First pass of 2-pass quantization }
      if (post^.whole_image = NIL) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
      post^.pub.post_process_data := post_process_prepass;
    end;
  JBUF_CRANK_DEST:
    begin
      { Second pass of 2-pass quantization }
      if (post^.whole_image = NIL) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
      post^.pub.post_process_data := post_process_2pass;
    end;
{$endif} { QUANT_2PASS_SUPPORTED }
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
  end;
  post^.next_row := 0;
  post^.starting_row := 0;
end;


{ Process some data in the one-pass (strip buffer) case.
  This is used for color precision reduction as well as one-pass quantization. }

{METHODDEF}
procedure post_process_1pass (cinfo : j_decompress_ptr;
		              input_buf : JSAMPIMAGE;
                              var in_row_group_ctr : JDIMENSION;
		              in_row_groups_avail : JDIMENSION;
		              output_buf : JSAMPARRAY;
                              var out_row_ctr : JDIMENSION;
		              out_rows_avail : JDIMENSION);
var
  post : my_post_ptr;
  num_rows, max_rows : JDIMENSION;
begin
  post := my_post_ptr (cinfo^.post);

  { Fill the buffer, but not more than what we can dump out in one go. }
  { Note we rely on the upsampler to detect bottom of image. }
  max_rows := out_rows_avail - out_row_ctr;
  if (max_rows > post^.strip_height) then
    max_rows := post^.strip_height;
  num_rows := 0;
  cinfo^.upsample^.upsample (cinfo,
		             input_buf,
                             in_row_group_ctr,
                             in_row_groups_avail,
		             post^.buffer,
                             num_rows,  { var }
                             max_rows);
  { Quantize and emit data. }

  cinfo^.cquantize^.color_quantize (cinfo,
		post^.buffer,
                JSAMPARRAY(@ output_buf^[out_row_ctr]),
                int(num_rows));

  Inc(out_row_ctr, num_rows);
end;


{$ifdef QUANT_2PASS_SUPPORTED}

{ Process some data in the first pass of 2-pass quantization. }

{METHODDEF}
procedure post_process_prepass (cinfo : j_decompress_ptr;
                               	input_buf : JSAMPIMAGE;
                                var in_row_group_ctr : JDIMENSION;
		                in_row_groups_avail : JDIMENSION;
		                output_buf : JSAMPARRAY;
                                var out_row_ctr : JDIMENSION;
		                out_rows_avail:JDIMENSION);
var
  post : my_post_ptr;
  old_next_row, num_rows : JDIMENSION;
begin
  post := my_post_ptr(cinfo^.post);

  { Reposition virtual buffer if at start of strip. }
  if (post^.next_row = 0) then
  begin
    post^.buffer := cinfo^.mem^.access_virt_sarray
	(j_common_ptr(cinfo), post^.whole_image,
	 post^.starting_row, post^.strip_height, TRUE);
  end;

  { Upsample some data (up to a strip height's worth). }
  old_next_row := post^.next_row;
  cinfo^.upsample^.upsample (cinfo,
		input_buf, in_row_group_ctr, in_row_groups_avail,
		post^.buffer, post^.next_row, post^.strip_height);

  { Allow quantizer to scan new data.  No data is emitted, }
  { but we advance out_row_ctr so outer loop can tell when we're done. }
  if (post^.next_row > old_next_row) then
  begin
    num_rows := post^.next_row - old_next_row;


    cinfo^.cquantize^.color_quantize (cinfo,
                      JSAMPARRAY(@ post^.buffer^[old_next_row]),
			JSAMPARRAY(NIL),
                        int(num_rows));
    Inc(out_row_ctr, num_rows);
  end;

  { Advance if we filled the strip. }
  if (post^.next_row >= post^.strip_height) then
  begin
    Inc(post^.starting_row, post^.strip_height);
    post^.next_row := 0;
  end;
end;


{ Process some data in the second pass of 2-pass quantization. }

{METHODDEF}
procedure post_process_2pass (cinfo : j_decompress_ptr;
		              input_buf : JSAMPIMAGE;
                              var in_row_group_ctr : JDIMENSION;
		              in_row_groups_avail : JDIMENSION;
		              output_buf : JSAMPARRAY;
                              var out_row_ctr : JDIMENSION;
		              out_rows_avail : JDIMENSION);
var
  post : my_post_ptr;
  num_rows, max_rows : JDIMENSION;
begin
  post := my_post_ptr(cinfo^.post);

  { Reposition virtual buffer if at start of strip. }
  if (post^.next_row = 0) then
  begin
    post^.buffer := cinfo^.mem^.access_virt_sarray
	(j_common_ptr(cinfo), post^.whole_image,
	 post^.starting_row, post^.strip_height, FALSE);
  end;

  { Determine number of rows to emit. }
  num_rows := post^.strip_height - post^.next_row; { available in strip }
  max_rows := out_rows_avail - out_row_ctr; { available in output area }
  if (num_rows > max_rows) then
    num_rows := max_rows;
  { We have to check bottom of image here, can't depend on upsampler. }
  max_rows := cinfo^.output_height - post^.starting_row;
  if (num_rows > max_rows) then
    num_rows := max_rows;

  { Quantize and emit data. }
  cinfo^.cquantize^.color_quantize (cinfo,
                JSAMPARRAY(@ post^.buffer^[post^.next_row]),
                JSAMPARRAY(@ output_buf^[out_row_ctr]),
		int(num_rows));
  Inc(out_row_ctr, num_rows);

  { Advance if we filled the strip. }
  Inc(post^.next_row, num_rows);
  if (post^.next_row >= post^.strip_height) then
  begin
    Inc(post^.starting_row, post^.strip_height);
    post^.next_row := 0;
  end;
end;

{$endif} { QUANT_2PASS_SUPPORTED }


{ Initialize postprocessing controller. }

{GLOBAL}
procedure jinit_d_post_controller (cinfo : j_decompress_ptr;
                                   need_full_buffer : boolean);
var
  post : my_post_ptr;
begin
  post := my_post_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(my_post_controller)) );
  cinfo^.post := jpeg_d_post_controller_ptr (post);
  post^.pub.start_pass := start_pass_dpost;
  post^.whole_image := NIL;	{ flag for no virtual arrays }
  post^.buffer := NIL;		{ flag for no strip buffer }

  { Create the quantization buffer, if needed }
  if (cinfo^.quantize_colors) then
  begin
    { The buffer strip height is max_v_samp_factor, which is typically
      an efficient number of rows for upsampling to return.
      (In the presence of output rescaling, we might want to be smarter?) }

    post^.strip_height := JDIMENSION (cinfo^.max_v_samp_factor);
    if (need_full_buffer) then
    begin
      { Two-pass color quantization: need full-image storage. }
      { We round up the number of rows to a multiple of the strip height. }
{$ifdef QUANT_2PASS_SUPPORTED}
      post^.whole_image := cinfo^.mem^.request_virt_sarray
	(j_common_ptr(cinfo), JPOOL_IMAGE, FALSE,
	 LongInt(cinfo^.output_width) * cinfo^.out_color_components,
	 JDIMENSION (jround_up( long(cinfo^.output_height),
				long(post^.strip_height)) ),
	 post^.strip_height);
{$else}
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
{$endif} { QUANT_2PASS_SUPPORTED }
    end
    else
    begin
      { One-pass color quantization: just make a strip buffer. }
      post^.buffer := cinfo^.mem^.alloc_sarray
	(j_common_ptr (cinfo), JPOOL_IMAGE,
	 LongInt(cinfo^.output_width) * cinfo^.out_color_components,
	 post^.strip_height);
    end;
  end;
end;

{== imjddctmgr implementation ==}

{ The decompressor input side (jdinput.c) saves away the appropriate
  quantization table for each component at the start of the first scan
  involving that component.  (This is necessary in order to correctly
  decode files that reuse Q-table slots.)
  When we are ready to make an output pass, the saved Q-table is converted
  to a multiplier table that will actually be used by the IDCT routine.
  The multiplier table contents are IDCT-method-dependent.  To support
  application changes in IDCT method between scans, we can remake the
  multiplier tables if necessary.
  In buffered-image mode, the first output pass may occur before any data
  has been seen for some components, and thus before their Q-tables have
  been saved away.  To handle this case, multiplier tables are preset
  to zeroes; the result of the IDCT will be a neutral gray level. }


{ Private subobject for this module }

type
  my_idct_ptr = ^my_idct_controller;
  my_idct_controller = record
    pub : jpeg_inverse_dct;	{ public fields }

    { This array contains the IDCT method code that each multiplier table
      is currently set up for, or -1 if it's not yet set up.
      The actual multiplier tables are pointed to by dct_table in the
      per-component comp_info structures. }

    cur_method : array[0..MAX_COMPONENTS-1] of int;
  end; {my_idct_controller;}


{ Allocated multiplier tables: big enough for any supported variant }

type
  multiplier_table = record
  case byte of
    0:(islow_array : array[0..DCTSIZE2-1] of ISLOW_MULT_TYPE);
  {$ifdef DCT_IFAST_SUPPORTED}
    1:(ifast_array : array[0..DCTSIZE2-1] of IFAST_MULT_TYPE);
  {$endif}
  {$ifdef DCT_FLOAT_SUPPORTED}
    2:(float_array : array[0..DCTSIZE2-1] of FLOAT_MULT_TYPE);
  {$endif}
  end;


{ The current scaled-IDCT routines require ISLOW-style multiplier tables,
  so be sure to compile that code if either ISLOW or SCALING is requested. }

{$ifdef DCT_ISLOW_SUPPORTED}
  {$define PROVIDE_ISLOW_TABLES}
{$else}
  {$ifdef IDCT_SCALING_SUPPORTED}
    {$define PROVIDE_ISLOW_TABLES}
  {$endif}
{$endif}


{ Prepare for an output pass.
  Here we select the proper IDCT routine for each component and build
  a matching multiplier table. }

{METHODDEF}
procedure start_pass (cinfo : j_decompress_ptr);
var
  idct : my_idct_ptr;
  ci, i : int;
  compptr : jpeg_component_info_ptr;
  method : J_DCT_METHOD;
  method_ptr : inverse_DCT_method_ptr;
  qtbl : JQUANT_TBL_PTR;
{$ifdef PROVIDE_ISLOW_TABLES}
var
  ismtbl : ISLOW_MULT_TYPE_FIELD_PTR;
{$endif}
{$ifdef DCT_IFAST_SUPPORTED}
const
  jddctmgr_CONST_BITS = 14;
const
  aanscales : array[0..DCTSIZE2-1] of INT16 =
    ({ precomputed values scaled up by 14 bits }
     16384, 22725, 21407, 19266, 16384, 12873,  8867,  4520,
     22725, 31521, 29692, 26722, 22725, 17855, 12299,  6270,
     21407, 29692, 27969, 25172, 21407, 16819, 11585,  5906,
     19266, 26722, 25172, 22654, 19266, 15137, 10426,  5315,
     16384, 22725, 21407, 19266, 16384, 12873,  8867,  4520,
     12873, 17855, 16819, 15137, 12873, 10114,  6967,  3552,
     8867, 12299, 11585, 10426,  8867,  6967,  4799,  2446,
     4520,  6270,  5906,  5315,  4520,  3552,  2446,  1247);
var
  ifmtbl : IFAST_MULT_TYPE_FIELD_PTR;
  {SHIFT_TEMPS}

  { Descale and correctly round an INT32 value that's scaled by N bits.
    We assume RIGHT_SHIFT rounds towards minus infinity, so adding
    the fudge factor is correct for either sign of X. }

  function jddctmgr_DESCALE(x : INT32; n : int) : INT32;
  var
    shift_temp : INT32;
  begin
  {$ifdef RIGHT_SHIFT_IS_UNSIGNED}
    shift_temp := x + (INT32(1) shl (n-1));
    if shift_temp < 0 then
      jddctmgr_Descale :=  (shift_temp shr n) or ((not INT32(0)) shl (32-n))
    else
      jddctmgr_Descale :=  (shift_temp shr n);
  {$else}
    jddctmgr_Descale := (x + (INT32(1) shl (n-1)) shr n;
  {$endif}
  end;

{$endif}
{$ifdef DCT_FLOAT_SUPPORTED}
const
  aanscalefactor : array[0..DCTSIZE-1] of double =
  (1.0, 1.387039845, 1.306562965, 1.175875602,
    1.0, 0.785694958, 0.541196100, 0.275899379);
var
  fmtbl : FLOAT_MULT_TYPE_FIELD_PTR;
  row, col : int;
{$endif}
begin
  idct := my_idct_ptr (cinfo^.idct);
  method := J_DCT_METHOD(0);
  method_ptr := NIL;
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);

  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Select the proper IDCT routine for this component's scaling }
    case (compptr^.DCT_scaled_size) of
{$ifdef IDCT_SCALING_SUPPORTED}
    1:begin
        method_ptr := jpeg_idct_1x1;
        method := JDCT_ISLOW;	{ jidctred uses islow-style table }
      end;
    2:begin
        method_ptr := jpeg_idct_2x2;
        method := JDCT_ISLOW;	{ jidctred uses islow-style table }
      end;
    4:begin
        method_ptr := jpeg_idct_4x4;
        method := JDCT_ISLOW;	{ jidctred uses islow-style table }
      end;
{$endif}
    DCTSIZE:
      case (cinfo^.dct_method) of
{$ifdef DCT_ISLOW_SUPPORTED}
      JDCT_ISLOW:
        begin
          method_ptr := @jpeg_idct_islow;
          method := JDCT_ISLOW;
	end;
{$endif}
{$ifdef DCT_IFAST_SUPPORTED}
      JDCT_IFAST:
        begin
          method_ptr := @jpeg_idct_ifast;
          method := JDCT_IFAST;
        end;
{$endif}
{$ifdef DCT_FLOAT_SUPPORTED}
      JDCT_FLOAT:
        begin
          method_ptr := @jpeg_idct_float;
          method := JDCT_FLOAT;
        end;
{$endif}
      else
        ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
      end;
    else
      ERREXIT1(j_common_ptr(cinfo), JERR_BAD_DCTSIZE, compptr^.DCT_scaled_size);
    end;
    idct^.pub.inverse_DCT[ci] := method_ptr;
    { Create multiplier table from quant table.
      However, we can skip this if the component is uninteresting
      or if we already built the table.  Also, if no quant table
      has yet been saved for the component, we leave the
      multiplier table all-zero; we'll be reading zeroes from the
      coefficient controller's buffer anyway. }

    if (not compptr^.component_needed) or (idct^.cur_method[ci] = int(method)) then
      continue;
    qtbl := compptr^.quant_table;
    if (qtbl = NIL) then	{ happens if no data yet for component }
      continue;
    idct^.cur_method[ci] := int(method);
    case (method) of
{$ifdef PROVIDE_ISLOW_TABLES}
    JDCT_ISLOW:
      begin
	{ For LL&M IDCT method, multipliers are equal to raw quantization
	  coefficients, but are stored as ints to ensure access efficiency. }

	ismtbl := ISLOW_MULT_TYPE_FIELD_PTR (compptr^.dct_table);
	for i := 0 to pred(DCTSIZE2) do
        begin
	  ismtbl^[i] := ISLOW_MULT_TYPE (qtbl^.quantval[i]);
	end;
      end;
{$endif}
{$ifdef DCT_IFAST_SUPPORTED}
    JDCT_IFAST:
      begin
	{ For AA&N IDCT method, multipliers are equal to quantization
	  coefficients scaled by scalefactor[row]*scalefactor[col], where
	    scalefactor[0] := 1
	    scalefactor[k] := cos(k*PI/16) * sqrt(2)    for k=1..7
	  For integer operation, the multiplier table is to be scaled by
	  IFAST_SCALE_BITS. }

	ifmtbl := IFAST_MULT_TYPE_FIELD_PTR (compptr^.dct_table);

	for i := 0 to pred(DCTSIZE2) do
        begin
	  ifmtbl^[i] := IFAST_MULT_TYPE(
	    jddctmgr_DESCALE(  INT32 (qtbl^.quantval[i]) * INT32 (aanscales[i]),
		    jddctmgr_CONST_BITS-IFAST_SCALE_BITS) );
	end;
      end;
{$endif}
{$ifdef DCT_FLOAT_SUPPORTED}
    JDCT_FLOAT:
      begin
	{ For float AA&N IDCT method, multipliers are equal to quantization
	  coefficients scaled by scalefactor[row]*scalefactor[col], where
	    scalefactor[0] := 1
	    scalefactor[k] := cos(k*PI/16) * sqrt(2)    for k=1..7 }

	fmtbl := FLOAT_MULT_TYPE_FIELD_PTR(compptr^.dct_table);

	i := 0;
	for row := 0 to pred(DCTSIZE) do
        begin
	  for col := 0 to pred(DCTSIZE) do
          begin
	    fmtbl^[i] := {FLOAT_MULT_TYPE} (
	       {double} qtbl^.quantval[i] *
	       aanscalefactor[row] * aanscalefactor[col] );
	    Inc(i);
	  end;
	end;
      end;
{$endif}
    else
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
      break;
    end;
    Inc(compptr);
  end;
end;


{ Initialize IDCT manager. }

{GLOBAL}
procedure jinit_inverse_dct (cinfo : j_decompress_ptr);
var
  idct : my_idct_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
begin
  idct := my_idct_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(my_idct_controller)) );
  cinfo^.idct := jpeg_inverse_dct_ptr (idct);
  idct^.pub.start_pass := start_pass;

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Allocate and pre-zero a multiplier table for each component }
    compptr^.dct_table :=
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				  SIZEOF(multiplier_table));
    MEMZERO(compptr^.dct_table, SIZEOF(multiplier_table));
    { Mark multiplier table not yet set up for any method }
    idct^.cur_method[ci] := -1;
    Inc(compptr);
  end;
end;

{== imjquant1 implementation ==}

{ The main purpose of 1-pass quantization is to provide a fast, if not very
  high quality, colormapped output capability.  A 2-pass quantizer usually
  gives better visual quality; however, for quantized grayscale output this
  quantizer is perfectly adequate.  Dithering is highly recommended with this
  quantizer, though you can turn it off if you really want to.

  In 1-pass quantization the colormap must be chosen in advance of seeing the
  image.  We use a map consisting of all combinations of Ncolors[i] color
  values for the i'th component.  The Ncolors[] values are chosen so that
  their product, the total number of colors, is no more than that requested.
  (In most cases, the product will be somewhat less.)

  Since the colormap is orthogonal, the representative value for each color
  component can be determined without considering the other components;
  then these indexes can be combined into a colormap index by a standard
  N-dimensional-array-subscript calculation.  Most of the arithmetic involved
  can be precalculated and stored in the lookup table colorindex[].
  colorindex[i][j] maps pixel value j in component i to the nearest
  representative value (grid plane) for that component; this index is
  multiplied by the array stride for component i, so that the
  index of the colormap entry closest to a given pixel value is just
     sum( colorindex[component-number][pixel-component-value] )
  Aside from being fast, this scheme allows for variable spacing between
  representative values with no additional lookup cost.

  If gamma correction has been applied in color conversion, it might be wise
  to adjust the color grid spacing so that the representative colors are
  equidistant in linear space.  At this writing, gamma correction is not
  implemented by jdcolor, so nothing is done here. }


{ Declarations for ordered dithering.

  We use a standard 16x16 ordered dither array.  The basic concept of ordered
  dithering is described in many references, for instance Dale Schumacher's
  chapter II.2 of Graphics Gems II (James Arvo, ed. Academic Press, 1991).
  In place of Schumacher's comparisons against a "threshold" value, we add a
  "dither" value to the input pixel and then round the result to the nearest
  output value.  The dither value is equivalent to (0.5 - threshold) times
  the distance between output values.  For ordered dithering, we assume that
  the output colors are equally spaced; if not, results will probably be
  worse, since the dither may be too much or too little at a given point.

  The normal calculation would be to form pixel value + dither, range-limit
  this to 0..MAXJSAMPLE, and then index into the colorindex table as usual.
  We can skip the separate range-limiting step by extending the colorindex
  table in both directions. }


const
  ODITHER_SIZE  = 16;   { dimension of dither matrix }
{ NB: if ODITHER_SIZE is not a power of 2, ODITHER_MASK uses will break }
  ODITHER_CELLS = (ODITHER_SIZE*ODITHER_SIZE);  { # cells in matrix }
  ODITHER_MASK = (ODITHER_SIZE-1); { mask for wrapping around counters }

type
  ODITHER_vector = Array[0..ODITHER_SIZE-1] of int;
  ODITHER_MATRIX = Array[0..ODITHER_SIZE-1] of ODITHER_vector;
  {ODITHER_MATRIX_PTR = ^array[0..ODITHER_SIZE-1] of int;}
  ODITHER_MATRIX_PTR = ^ODITHER_MATRIX;

const
  base_dither_matrix : Array[0..ODITHER_SIZE-1,0..ODITHER_SIZE-1] of UINT8
  = (
  { Bayer's order-4 dither array.  Generated by the code given in
    Stephen Hawley's article "Ordered Dithering" in Graphics Gems I.
    The values in this array must range from 0 to ODITHER_CELLS-1. }

  (   0,192, 48,240, 12,204, 60,252,  3,195, 51,243, 15,207, 63,255 ),
  ( 128, 64,176,112,140, 76,188,124,131, 67,179,115,143, 79,191,127 ),
  (  32,224, 16,208, 44,236, 28,220, 35,227, 19,211, 47,239, 31,223 ),
  ( 160, 96,144, 80,172,108,156, 92,163, 99,147, 83,175,111,159, 95 ),
  (   8,200, 56,248,  4,196, 52,244, 11,203, 59,251,  7,199, 55,247 ),
  ( 136, 72,184,120,132, 68,180,116,139, 75,187,123,135, 71,183,119 ),
  (  40,232, 24,216, 36,228, 20,212, 43,235, 27,219, 39,231, 23,215 ),
  ( 168,104,152, 88,164,100,148, 84,171,107,155, 91,167,103,151, 87 ),
  (   2,194, 50,242, 14,206, 62,254,  1,193, 49,241, 13,205, 61,253 ),
  ( 130, 66,178,114,142, 78,190,126,129, 65,177,113,141, 77,189,125 ),
  (  34,226, 18,210, 46,238, 30,222, 33,225, 17,209, 45,237, 29,221 ),
  ( 162, 98,146, 82,174,110,158, 94,161, 97,145, 81,173,109,157, 93 ),
  (  10,202, 58,250,  6,198, 54,246,  9,201, 57,249,  5,197, 53,245 ),
  ( 138, 74,186,122,134, 70,182,118,137, 73,185,121,133, 69,181,117 ),
  (  42,234, 26,218, 38,230, 22,214, 41,233, 25,217, 37,229, 21,213 ),
  ( 170,106,154, 90,166,102,150, 86,169,105,153, 89,165,101,149, 85 )
  );


{ Declarations for Floyd-Steinberg dithering.

  Errors are accumulated into the array fserrors[], at a resolution of
  1/16th of a pixel count.  The error at a given pixel is propagated
  to its not-yet-processed neighbors using the standard F-S fractions,
 		...	(here)	7/16
 		3/16	5/16	1/16
  We work left-to-right on even rows, right-to-left on odd rows.

  We can get away with a single array (holding one row's worth of errors)
  by using it to store the current row's errors at pixel columns not yet
  processed, but the next row's errors at columns already processed.  We
  need only a few extra variables to hold the errors immediately around the
  current column.  (If we are lucky, those variables are in registers, but
  even if not, they're probably cheaper to access than array elements are.)

  The fserrors[] array is indexed [component#][position].
  We provide (#columns + 2) entries per component; the extra entry at each
  end saves us from special-casing the first and last pixels.

  Note: on a wide image, we might not have enough room in a PC's near data
  segment to hold the error array; so it is allocated with alloc_large. }

{$ifdef BITS_IN_JSAMPLE_IS_8}
type
  jquant1_FSERROR = INT16;              { 16 bits should be enough }
  jquant1_LOCFSERROR = int;             { use 'int' for calculation temps }
{$else}
type
  jquant1_FSERROR = INT32;              { may need more than 16 bits }
  jquant1_LOCFSERROR = INT32;           { be sure calculation temps are big enough }
{$endif}

type
  jquant1_jFSError = 0..(MaxInt div SIZEOF(jquant1_FSERROR))-1;
  jquant1_FS_ERROR_FIELD = array[jquant1_jFSError] of jquant1_FSERROR;
  jquant1_FS_ERROR_FIELD_PTR = ^jquant1_FS_ERROR_FIELD;{far}
                                { pointer to error array (in FAR storage!) }
  FSERRORPTR = ^jquant1_FSERROR;


{ Private subobject }

const
  MAX_Q_COMPS = 4;              { max components I can handle }

type
  jquant1_my_cquantize_ptr = ^jquant1_my_cquantizer;
  jquant1_my_cquantizer = record
    pub : jpeg_color_quantizer; { public fields }

    { Initially allocated colormap is saved here }
    sv_colormap : JSAMPARRAY;   { The color map as a 2-D pixel array }
    sv_actual : int;            { number of entries in use }

    colorindex : JSAMPARRAY;    { Precomputed mapping for speed }
    { colorindex[i][j] = index of color closest to pixel value j in component i,
      premultiplied as described above.  Since colormap indexes must fit into
      JSAMPLEs, the entries of this array will too. }

    is_padded : boolean;        { is the colorindex padded for odither? }

    Ncolors : array[0..MAX_Q_COMPS-1] of int;
                                { # of values alloced to each component }

    { Variables for ordered dithering }
    row_index : int;            { cur row's vertical index in dither matrix }
    odither : array[0..MAX_Q_COMPS-1] of ODITHER_MATRIX_PTR;
                                { one dither array per component }
    { Variables for Floyd-Steinberg dithering }
    fserrors : array[0..MAX_Q_COMPS-1] of jquant1_FS_ERROR_FIELD_PTR;
                                { accumulated errors }
    on_odd_row : boolean;       { flag to remember which row we are on }
  end;


{ Policy-making subroutines for create_colormap and create_colorindex.
  These routines determine the colormap to be used.  The rest of the module
  only assumes that the colormap is orthogonal.

   * select_ncolors decides how to divvy up the available colors
     among the components.
   * output_value defines the set of representative values for a component.
   * largest_input_value defines the mapping from input values to
     representative values for a component.
  Note that the latter two routines may impose different policies for
  different components, though this is not currently done. }


{LOCAL}
function select_ncolors (cinfo : j_decompress_ptr;
                         var Ncolors : array of int) : int;
{ Determine allocation of desired colors to components, }
{ and fill in Ncolors[] array to indicate choice. }
{ Return value is total number of colors (product of Ncolors[] values). }
var
  nc : int;
  max_colors : int;
  total_colors, iroot, i, j : int;
  changed : boolean;
  temp : long;
const
  RGB_order:array[0..2] of int = (RGB_GREEN, RGB_RED, RGB_BLUE);
begin
  nc := cinfo^.out_color_components; { number of color components }
  max_colors := cinfo^.desired_number_of_colors;

  { We can allocate at least the nc'th root of max_colors per component. }
  { Compute floor(nc'th root of max_colors). }
  iroot := 1;
  repeat
    Inc(iroot);
    temp := iroot;		{ set temp = iroot ** nc }
    for i := 1 to pred(nc) do
      temp := temp * iroot;
  until (temp > long(max_colors)); { repeat till iroot exceeds root }
  Dec(iroot);                   { now iroot = floor(root) }

  { Must have at least 2 color values per component }
  if (iroot < 2) then
    ERREXIT1(j_common_ptr(cinfo), JERR_QUANT_FEW_COLORS, int(temp));

  { Initialize to iroot color values for each component }
  total_colors := 1;
  for i := 0 to pred(nc) do
  begin
    Ncolors[i] := iroot;
    total_colors := total_colors * iroot;
  end;

  { We may be able to increment the count for one or more components without
    exceeding max_colors, though we know not all can be incremented.
    Sometimes, the first component can be incremented more than once!
    (Example: for 16 colors, we start at 2*2*2, go to 3*2*2, then 4*2*2.)
    In RGB colorspace, try to increment G first, then R, then B. }

  repeat
    changed := FALSE;
    for i := 0 to pred(nc) do
    begin
      if cinfo^.out_color_space = JCS_RGB then
        j := RGB_order[i]
      else
        j := i;
      { calculate new total_colors if Ncolors[j] is incremented }
      temp := total_colors div Ncolors[j];
      temp := temp * (Ncolors[j]+1);   { done in long arith to avoid oflo }
      if (temp > long(max_colors)) then
	break;                  { won't fit, done with this pass }
      Inc(Ncolors[j]);		{ OK, apply the increment }
      total_colors := int(temp);
      changed := TRUE;
    end;
  until not changed;

  select_ncolors := total_colors;
end;


{LOCAL}
function output_value (cinfo : j_decompress_ptr;
                       ci : int; j : int; maxj : int) : int;
{ Return j'th output value, where j will range from 0 to maxj }
{ The output values must fall in 0..MAXJSAMPLE in increasing order }
begin
  { We always provide values 0 and MAXJSAMPLE for each component;
    any additional values are equally spaced between these limits.
    (Forcing the upper and lower values to the limits ensures that
    dithering can't produce a color outside the selected gamut.) }

  output_value := int (( INT32(j) * MAXJSAMPLE + maxj div 2) div maxj);
end;


{LOCAL}
function largest_input_value (cinfo : j_decompress_ptr;
                              ci : int; j : int; maxj : int) : int;
{ Return largest input value that should map to j'th output value }
{ Must have largest(j=0) >= 0, and largest(j=maxj) >= MAXJSAMPLE }
begin
  { Breakpoints are halfway between values returned by output_value }
  largest_input_value := int (( INT32(2*j + 1) * MAXJSAMPLE +
                                 maxj) div (2*maxj));
end;


{ Create the colormap. }

{LOCAL}
procedure create_colormap (cinfo : j_decompress_ptr);
var
  cquantize : jquant1_my_cquantize_ptr;
  colormap : JSAMPARRAY;        { Created colormap }

  total_colors : int;           { Number of distinct output colors }
  i,j,k, nci, blksize, blkdist, ptr, val : int;
begin
  cquantize := jquant1_my_cquantize_ptr (cinfo^.cquantize);

  { Select number of colors for each component }
  total_colors := select_ncolors(cinfo, cquantize^.Ncolors);

  { Report selected color counts }
  {$IFDEF DEBUG}
  if (cinfo^.out_color_components = 3) then
    TRACEMS4(j_common_ptr(cinfo), 1, JTRC_QUANT_3_NCOLORS,
	     total_colors, cquantize^.Ncolors[0],
	     cquantize^.Ncolors[1], cquantize^.Ncolors[2])
  else
    TRACEMS1(j_common_ptr(cinfo), 1, JTRC_QUANT_NCOLORS, total_colors);
  {$ENDIF}

  { Allocate and fill in the colormap. }
  { The colors are ordered in the map in standard row-major order, }
  { i.e. rightmost (highest-indexed) color changes most rapidly. }

  colormap := cinfo^.mem^.alloc_sarray(
     j_common_ptr(cinfo), JPOOL_IMAGE,
     JDIMENSION(total_colors), JDIMENSION(cinfo^.out_color_components));

  { blksize is number of adjacent repeated entries for a component }
  { blkdist is distance between groups of identical entries for a component }
  blkdist := total_colors;

  for i := 0 to pred(cinfo^.out_color_components) do
  begin
    { fill in colormap entries for i'th color component }
    nci := cquantize^.Ncolors[i]; { # of distinct values for this color }
    blksize := blkdist div nci;
    for j := 0 to pred(nci) do
    begin
      { Compute j'th output value (out of nci) for component }
      val := output_value(cinfo, i, j, nci-1);
      { Fill in all colormap entries that have this value of this component }
      ptr := j * blksize;
      while (ptr < total_colors) do
      begin
	{ fill in blksize entries beginning at ptr }
	for k := 0 to pred(blksize) do
          colormap^[i]^[ptr+k] := JSAMPLE(val);

        Inc(ptr, blkdist);
      end;
    end;
    blkdist := blksize;		{ blksize of this color is blkdist of next }
  end;

  { Save the colormap in private storage,
    where it will survive color quantization mode changes. }

  cquantize^.sv_colormap := colormap;
  cquantize^.sv_actual := total_colors;
end;

{ Create the color index table. }

{LOCAL}
procedure create_colorindex (cinfo : j_decompress_ptr);
var
  cquantize : jquant1_my_cquantize_ptr;
  indexptr,
  help_indexptr : JSAMPROW;  { for negative offsets }
  i,j,k, nci, blksize, val, pad : int;
begin
  cquantize := jquant1_my_cquantize_ptr (cinfo^.cquantize);
  { For ordered dither, we pad the color index tables by MAXJSAMPLE in
    each direction (input index values can be -MAXJSAMPLE .. 2*MAXJSAMPLE).
    This is not necessary in the other dithering modes.  However, we
    flag whether it was done in case user changes dithering mode. }

  if (cinfo^.dither_mode = JDITHER_ORDERED) then
  begin
    pad := MAXJSAMPLE*2;
    cquantize^.is_padded := TRUE;
  end
  else
  begin
    pad := 0;
    cquantize^.is_padded := FALSE;
  end;

  cquantize^.colorindex := cinfo^.mem^.alloc_sarray
    (j_common_ptr(cinfo), JPOOL_IMAGE,
     JDIMENSION(MAXJSAMPLE+1 + pad),
     JDIMENSION(cinfo^.out_color_components));

  { blksize is number of adjacent repeated entries for a component }
  blksize := cquantize^.sv_actual;

  for i := 0 to pred(cinfo^.out_color_components) do
  begin
    { fill in colorindex entries for i'th color component }
    nci := cquantize^.Ncolors[i]; { # of distinct values for this color }
    blksize := blksize div nci;

    { adjust colorindex pointers to provide padding at negative indexes. }
    if (pad <> 0) then
      Inc(JSAMPLE_PTR(cquantize^.colorindex^[i]), MAXJSAMPLE);

    { in loop, val = index of current output value, }
    { and k = largest j that maps to current val }
    indexptr := cquantize^.colorindex^[i];
    val := 0;
    k := largest_input_value(cinfo, i, 0, nci-1);
    for j := 0 to MAXJSAMPLE do
    begin
      while (j > k) do          { advance val if past boundary }
      begin
        Inc(val);
	k := largest_input_value(cinfo, i, val, nci-1);
      end;
      { premultiply so that no multiplication needed in main processing }
      indexptr^[j] := JSAMPLE (val * blksize);
    end;
    { Pad at both ends if necessary }
    if (pad <> 0) then
    begin
      help_indexptr := indexptr;
      { adjust the help pointer to avoid negative offsets }
      Dec(JSAMPLE_PTR(help_indexptr), MAXJSAMPLE);

      for j := 1 to MAXJSAMPLE do
      begin
        {indexptr^[-j] := indexptr^[0];}
        help_indexptr^[MAXJSAMPLE-j] := indexptr^[0];
        indexptr^[MAXJSAMPLE+j] := indexptr^[MAXJSAMPLE];
      end;
    end;
  end;
end;


{ Create an ordered-dither array for a component having ncolors
  distinct output values. }

{LOCAL}
function make_odither_array (cinfo : j_decompress_ptr;
                             ncolors : int) : ODITHER_MATRIX_PTR;
var
  odither : ODITHER_MATRIX_PTR;
  j, k : int;
  num, den : INT32;
begin
  odither := ODITHER_MATRIX_PTR (
        cinfo^.mem^.alloc_small(j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(ODITHER_MATRIX)));
  { The inter-value distance for this color is MAXJSAMPLE/(ncolors-1).
    Hence the dither value for the matrix cell with fill order f
    (f=0..N-1) should be (N-1-2*f)/(2*N) * MAXJSAMPLE/(ncolors-1).
    On 16-bit-int machine, be careful to avoid overflow. }

  den := 2 * ODITHER_CELLS * ( INT32(ncolors - 1));
  for j := 0 to pred(ODITHER_SIZE) do
  begin
    for k := 0 to pred(ODITHER_SIZE) do
    begin
      num := ( INT32(ODITHER_CELLS-1 - 2*( int(base_dither_matrix[j][k]))))
	    * MAXJSAMPLE;
      { Ensure round towards zero despite C's lack of consistency
        about rounding negative values in integer division... }

      if num<0 then
        odither^[j][k] := int (-((-num) div den))
      else
        odither^[j][k] := int (num div den);
    end;
  end;
  make_odither_array := odither;
end;


{ Create the ordered-dither tables.
  Components having the same number of representative colors may
  share a dither table. }

{LOCAL}
procedure create_odither_tables (cinfo : j_decompress_ptr);
var
  cquantize : jquant1_my_cquantize_ptr;
  odither : ODITHER_MATRIX_PTR;
  i, j, nci : int;
begin
  cquantize := jquant1_my_cquantize_ptr (cinfo^.cquantize);

  for i := 0 to pred(cinfo^.out_color_components) do
  begin
    nci := cquantize^.Ncolors[i]; { # of distinct values for this color }
    odither := NIL;		  { search for matching prior component }
    for j := 0 to pred(i) do
    begin
      if (nci = cquantize^.Ncolors[j]) then
      begin
	odither := cquantize^.odither[j];
	break;
      end;
    end;
    if (odither = NIL)	then { need a new table? }
      odither := make_odither_array(cinfo, nci);
    cquantize^.odither[i] := odither;
  end;
end;


{ Map some rows of pixels to the output colormapped representation. }

{METHODDEF}
procedure color_quantize (cinfo : j_decompress_ptr;
                          input_buf : JSAMPARRAY;
		          output_buf : JSAMPARRAY;
                          num_rows : int);
{ General case, no dithering }
var
  cquantize : jquant1_my_cquantize_ptr;
  colorindex : JSAMPARRAY;
  pixcode, ci : int; {register}
  ptrin, ptrout : JSAMPLE_PTR; {register}
  row : int;
  col : JDIMENSION;
  width : JDIMENSION;
  nc : int; {register}
begin
  cquantize := jquant1_my_cquantize_ptr (cinfo^.cquantize);
  colorindex := cquantize^.colorindex;
  width := cinfo^.output_width;
  nc := cinfo^.out_color_components;

  for row := 0 to pred(num_rows) do
  begin
    ptrin := JSAMPLE_PTR(input_buf^[row]);
    ptrout := JSAMPLE_PTR(output_buf^[row]);
    for col := pred(width) downto 0 do
    begin
      pixcode := 0;
      for ci := 0 to pred(nc) do
      begin
	Inc(pixcode, GETJSAMPLE(colorindex^[ci]^[GETJSAMPLE(ptrin^)]) );
        Inc(ptrin);
      end;
      ptrout^ := JSAMPLE (pixcode);
      Inc(ptrout);
    end;
  end;
end;


{METHODDEF}
procedure color_quantize3 (cinfo : j_decompress_ptr;
                           input_buf : JSAMPARRAY;
		           output_buf : JSAMPARRAY;
                           num_rows : int);
{ Fast path for out_color_components=3, no dithering }
var
  cquantize : jquant1_my_cquantize_ptr;
  pixcode : int; {register}
  ptrin, ptrout : JSAMPLE_PTR; {register}
  colorindex0 : JSAMPROW;
  colorindex1 : JSAMPROW;
  colorindex2 : JSAMPROW;
  row : int;
  col : JDIMENSION;
  width : JDIMENSION;
begin
  cquantize := jquant1_my_cquantize_ptr (cinfo^.cquantize);
  colorindex0 := (cquantize^.colorindex)^[0];
  colorindex1 := (cquantize^.colorindex)^[1];
  colorindex2 := (cquantize^.colorindex)^[2];
  width := cinfo^.output_width;

  for row := 0 to pred(num_rows) do
  begin
    ptrin := JSAMPLE_PTR(input_buf^[row]);
    ptrout := JSAMPLE_PTR(output_buf^[row]);
    for col := pred(width) downto 0 do
    begin
      pixcode  := GETJSAMPLE((colorindex0)^[GETJSAMPLE(ptrin^)]);
      Inc(ptrin);
      Inc( pixcode, GETJSAMPLE((colorindex1)^[GETJSAMPLE(ptrin^)]) );
      Inc(ptrin);
      Inc( pixcode, GETJSAMPLE((colorindex2)^[GETJSAMPLE(ptrin^)]) );
      Inc(ptrin);
      ptrout^ := JSAMPLE (pixcode);
      Inc(ptrout);
    end;
  end;
end;


{METHODDEF}
procedure quantize_ord_dither (cinfo : j_decompress_ptr;
                               input_buf :  JSAMPARRAY;
		               output_buf : JSAMPARRAY;
                               num_rows : int);
{ General case, with ordered dithering }
var
  cquantize : jquant1_my_cquantize_ptr;
  input_ptr,                {register}
  output_ptr : JSAMPLE_PTR; {register}
  colorindex_ci : JSAMPROW;
  dither : ^ODITHER_vector;     { points to active row of dither matrix }
  row_index, col_index : int;   { current indexes into dither matrix }
  nc : int;
  ci : int;
  row : int;
  col : JDIMENSION;
  width : JDIMENSION;
var
  pad_offset : int;
begin
  cquantize := jquant1_my_cquantize_ptr (cinfo^.cquantize);
  nc := cinfo^.out_color_components;
  width := cinfo^.output_width;

  { Nomssi: work around negative offset }
  if jquant1_my_cquantize_ptr (cinfo^.cquantize)^.is_padded then
    pad_offset := MAXJSAMPLE
  else
    pad_offset := 0;

  for row := 0 to pred(num_rows) do
  begin
    { Initialize output values to 0 so can process components separately }
    jzero_far( {far} pointer(output_buf^[row]),
	      size_t(width * SIZEOF(JSAMPLE)));
    row_index := cquantize^.row_index;
    for ci := 0 to pred(nc) do
    begin
      input_ptr := JSAMPLE_PTR(@ input_buf^[row]^[ci]);
      output_ptr := JSAMPLE_PTR(output_buf^[row]);
      colorindex_ci := cquantize^.colorindex^[ci];
      { Nomssi }
      Dec(JSAMPLE_PTR(colorindex_ci), pad_offset);

      dither := @(cquantize^.odither[ci]^[row_index]);
      col_index := 0;

      for col := pred(width) downto 0 do
      begin
	{ Form pixel value + dither, range-limit to 0..MAXJSAMPLE,
	  select output value, accumulate into output code for this pixel.
	  Range-limiting need not be done explicitly, as we have extended
	  the colorindex table to produce the right answers for out-of-range
	  inputs.  The maximum dither is +- MAXJSAMPLE; this sets the
	  required amount of padding. }

	Inc(output_ptr^,
            colorindex_ci^[GETJSAMPLE(input_ptr^)+ pad_offset +
                                         dither^[col_index]]);
        Inc(output_ptr);
	Inc(input_ptr, nc);
	col_index := (col_index + 1) and ODITHER_MASK;
      end;
    end;
    { Advance row index for next row }
    row_index := (row_index + 1) and ODITHER_MASK;
    cquantize^.row_index := row_index;
  end;
end;

{METHODDEF}
procedure quantize3_ord_dither (cinfo : j_decompress_ptr;
                                input_buf : JSAMPARRAY;
		                output_buf : JSAMPARRAY;
                                num_rows : int);
{ Fast path for out_color_components=3, with ordered dithering }
var
  cquantize : jquant1_my_cquantize_ptr;
  pixcode : int; {register}
  input_ptr : JSAMPLE_PTR; {register}
  output_ptr : JSAMPLE_PTR; {register}
  colorindex0 : JSAMPROW;
  colorindex1 : JSAMPROW;
  colorindex2 : JSAMPROW;
  dither0 : ^ODITHER_vector;    { points to active row of dither matrix }
  dither1 : ^ODITHER_vector;
  dither2 : ^ODITHER_vector;
  row_index, col_index : int;   { current indexes into dither matrix }
  row : int;
  col : JDIMENSION;
  width : JDIMENSION;
var
  pad_offset : int;
begin
  cquantize := jquant1_my_cquantize_ptr (cinfo^.cquantize);
  colorindex0 := (cquantize^.colorindex)^[0];
  colorindex1 := (cquantize^.colorindex)^[1];
  colorindex2 := (cquantize^.colorindex)^[2];
  width := cinfo^.output_width;

  { Nomssi: work around negative offset }
  if jquant1_my_cquantize_ptr (cinfo^.cquantize)^.is_padded then
    pad_offset := MAXJSAMPLE
  else
    pad_offset := 0;

  Dec(JSAMPLE_PTR(colorindex0), pad_offset);
  Dec(JSAMPLE_PTR(colorindex1), pad_offset);
  Dec(JSAMPLE_PTR(colorindex2), pad_offset);

  for row := 0 to pred(num_rows) do
  begin
    row_index := cquantize^.row_index;
    input_ptr := JSAMPLE_PTR(input_buf^[row]);
    output_ptr := JSAMPLE_PTR(output_buf^[row]);
    dither0 := @(cquantize^.odither[0]^[row_index]);
    dither1 := @(cquantize^.odither[1]^[row_index]);
    dither2 := @(cquantize^.odither[2]^[row_index]);
    col_index := 0;


    for col := pred(width) downto 0 do
    begin
      pixcode := GETJSAMPLE(colorindex0^[GETJSAMPLE(input_ptr^) + pad_offset
                                         + dither0^[col_index]]);
      Inc(input_ptr);
      Inc(pixcode, GETJSAMPLE(colorindex1^[GETJSAMPLE(input_ptr^) + pad_offset
                                           + dither1^[col_index]]));
      Inc(input_ptr);
      Inc(pixcode, GETJSAMPLE(colorindex2^[GETJSAMPLE(input_ptr^) + pad_offset
                                           + dither2^[col_index]]));
      Inc(input_ptr);
      output_ptr^ := JSAMPLE (pixcode);
      Inc(output_ptr);
      col_index := (col_index + 1) and ODITHER_MASK;
    end;
    row_index := (row_index + 1) and ODITHER_MASK;
    cquantize^.row_index := row_index;
  end;
end;


{METHODDEF}
procedure quantize_fs_dither (cinfo : j_decompress_ptr;
                              input_buf : JSAMPARRAY;
		              output_buf : JSAMPARRAY;
                              num_rows : int);
{ General case, with Floyd-Steinberg dithering }
var
  cquantize : jquant1_my_cquantize_ptr;
  cur : jquant1_LOCFSERROR; {register}  { current error or pixel value }
  belowerr : jquant1_LOCFSERROR;        { error for pixel below cur }
  bpreverr : jquant1_LOCFSERROR;        { error for below/prev col }
  bnexterr : jquant1_LOCFSERROR;        { error for below/next col }
  delta : jquant1_LOCFSERROR;
  prev_errorptr,
  errorptr : FSERRORPTR; {register} { => fserrors[] at column before current }
  input_ptr,                {register}
  output_ptr : JSAMPLE_PTR; {register}
  colorindex_ci : JSAMPROW;
  colormap_ci : JSAMPROW;
  pixcode : int;
  nc : int;
  dir : int;			{ 1 for left-to-right, -1 for right-to-left }
  dirnc : int;			{ dir * nc }
  ci : int;
  row : int;
  col : JDIMENSION;
  width : JDIMENSION;
  range_limit : range_limit_table_ptr;
begin
  cquantize := jquant1_my_cquantize_ptr (cinfo^.cquantize);
  nc := cinfo^.out_color_components;
  width := cinfo^.output_width;
  range_limit := cinfo^.sample_range_limit;

  for row := 0 to pred(num_rows) do
  begin
    { Initialize output values to 0 so can process components separately }
    jzero_far( (output_buf)^[row],
               size_t(width * SIZEOF(JSAMPLE)));
    for ci := 0 to pred(nc) do
    begin
      input_ptr := JSAMPLE_PTR(@ input_buf^[row]^[ci]);
      output_ptr := JSAMPLE_PTR(output_buf^[row]);
      errorptr := FSERRORPTR(cquantize^.fserrors[ci]); { => entry before first column }
      if (cquantize^.on_odd_row) then
      begin
	{ work right to left in this row }
	Inc(input_ptr, (width-1) * JDIMENSION(nc)); { so point to rightmost pixel }
	Inc(output_ptr, width-1);
	dir := -1;
	dirnc := -nc;
	Inc(errorptr, (width+1)); { => entry after last column }
      end
      else
      begin
	{ work left to right in this row }
	dir := 1;
	dirnc := nc;
        {errorptr := cquantize^.fserrors[ci];}
      end;

      colorindex_ci := cquantize^.colorindex^[ci];

      colormap_ci := (cquantize^.sv_colormap)^[ci];
      { Preset error values: no error propagated to first pixel from left }
      cur := 0;
      { and no error propagated to row below yet }
      belowerr := 0;
      bpreverr := 0;

      for col := pred(width) downto 0 do
      begin
        prev_errorptr := errorptr;
	Inc(errorptr, dir);  { advance errorptr to current column }

	{ cur holds the error propagated from the previous pixel on the
	  current line.  Add the error propagated from the previous line
	  to form the complete error correction term for this pixel, and
	  round the error term (which is expressed * 16) to an integer.
	  RIGHT_SHIFT rounds towards minus infinity, so adding 8 is correct
	  for either sign of the error value.
	  Note: errorptr points to *previous* column's array entry. }

        cur := (cur + errorptr^ + 8) div 16;

	{ Form pixel value + error, and range-limit to 0..MAXJSAMPLE.
	  The maximum error is +- MAXJSAMPLE; this sets the required size
	  of the range_limit array. }

	Inc( cur, GETJSAMPLE(input_ptr^));
	cur := GETJSAMPLE(range_limit^[cur]);
	{ Select output value, accumulate into output code for this pixel }
	pixcode := GETJSAMPLE(colorindex_ci^[cur]);
	Inc(output_ptr^, JSAMPLE (pixcode));
	{ Compute actual representation error at this pixel }
	{ Note: we can do this even though we don't have the final }
	{ pixel code, because the colormap is orthogonal. }
	Dec(cur, GETJSAMPLE(colormap_ci^[pixcode]));
	{ Compute error fractions to be propagated to adjacent pixels.
	  Add these into the running sums, and simultaneously shift the
	  next-line error sums left by 1 column. }

	bnexterr := cur;
	delta := cur * 2;
	Inc(cur, delta);        { form error * 3 }
	prev_errorptr^ := jquant1_FSERROR (bpreverr + cur);
	Inc(cur, delta);        { form error * 5 }
	bpreverr := belowerr + cur;
	belowerr := bnexterr;
	Inc(cur, delta);        { form error * 7 }
	{ At this point cur contains the 7/16 error value to be propagated
	  to the next pixel on the current line, and all the errors for the
	  next line have been shifted over. We are therefore ready to move on. }

	Inc(input_ptr, dirnc);  { advance input ptr to next column }
	Inc(output_ptr, dir);   { advance output ptr to next column }

      end;
      { Post-loop cleanup: we must unload the final error value into the
        final fserrors[] entry.  Note we need not unload belowerr because
        it is for the dummy column before or after the actual array. }

      errorptr^ := jquant1_FSERROR (bpreverr); { unload prev err into array }
      { Nomssi : ?? }
    end;
    cquantize^.on_odd_row := not cquantize^.on_odd_row;
  end;
end;


{ Allocate workspace for Floyd-Steinberg errors. }

{LOCAL}
procedure alloc_fs_workspace (cinfo : j_decompress_ptr);
var
  cquantize : jquant1_my_cquantize_ptr;
  arraysize : size_t;
  i : int;
begin
  cquantize := jquant1_my_cquantize_ptr (cinfo^.cquantize);
  arraysize := size_t ((cinfo^.output_width + 2) * SIZEOF(jquant1_FSERROR));
  for i := 0 to pred(cinfo^.out_color_components) do
  begin
    cquantize^.fserrors[i] := jquant1_FS_ERROR_FIELD_PTR(
      cinfo^.mem^.alloc_large(j_common_ptr(cinfo), JPOOL_IMAGE, arraysize));
  end;
end;


{ Initialize for one-pass color quantization. }

{METHODDEF}
procedure start_pass_1_quant (cinfo : j_decompress_ptr;
                              is_pre_scan : boolean);
var
  cquantize : jquant1_my_cquantize_ptr;
  arraysize : size_t;
  i : int;
begin
  cquantize := jquant1_my_cquantize_ptr (cinfo^.cquantize);
  { Install my colormap. }
  cinfo^.colormap := cquantize^.sv_colormap;
  cinfo^.actual_number_of_colors := cquantize^.sv_actual;

  { Initialize for desired dithering mode. }
  case (cinfo^.dither_mode) of
  JDITHER_NONE:
    if (cinfo^.out_color_components = 3) then
      cquantize^.pub.color_quantize := color_quantize3
    else
      cquantize^.pub.color_quantize := color_quantize;
  JDITHER_ORDERED:
    begin
      if (cinfo^.out_color_components = 3) then
        cquantize^.pub.color_quantize := quantize3_ord_dither
      else
        cquantize^.pub.color_quantize := quantize_ord_dither;
      cquantize^.row_index := 0;    { initialize state for ordered dither }
      { If user changed to ordered dither from another mode,
        we must recreate the color index table with padding.
        This will cost extra space, but probably isn't very likely. }

      if (not cquantize^.is_padded) then
        create_colorindex(cinfo);
      { Create ordered-dither tables if we didn't already. }
      if (cquantize^.odither[0] = NIL) then
        create_odither_tables(cinfo);
    end;
  JDITHER_FS:
    begin
      cquantize^.pub.color_quantize := quantize_fs_dither;
      cquantize^.on_odd_row := FALSE; { initialize state for F-S dither }
      { Allocate Floyd-Steinberg workspace if didn't already. }
      if (cquantize^.fserrors[0] = NIL) then
        alloc_fs_workspace(cinfo);
      { Initialize the propagated errors to zero. }
      arraysize := size_t ((cinfo^.output_width + 2) * SIZEOF(jquant1_FSERROR));
      for i := 0 to pred(cinfo^.out_color_components) do
        jzero_far({far} pointer( cquantize^.fserrors[i] ), arraysize);
    end;
  else
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
  end;
end;


{ Finish up at the end of the pass. }

{METHODDEF}
procedure finish_pass_1_quant (cinfo : j_decompress_ptr);
begin
  { no work in 1-pass case }
end;


{ Switch to a new external colormap between output passes.
  Shouldn't get to this module! }

{METHODDEF}
procedure new_color_map_1_quant (cinfo : j_decompress_ptr);
begin
  ERREXIT(j_common_ptr(cinfo), JERR_MODE_CHANGE);
end;


{ Module initialization routine for 1-pass color quantization. }

{GLOBAL}
procedure jinit_1pass_quantizer (cinfo : j_decompress_ptr);
var
  cquantize : jquant1_my_cquantize_ptr;
begin
  cquantize := jquant1_my_cquantize_ptr(
     cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(jquant1_my_cquantizer)));
  cinfo^.cquantize := jpeg_color_quantizer_ptr(cquantize);
  cquantize^.pub.start_pass := start_pass_1_quant;
  cquantize^.pub.finish_pass := finish_pass_1_quant;
  cquantize^.pub.new_color_map := new_color_map_1_quant;
  cquantize^.fserrors[0] := NIL; { Flag FS workspace not allocated }
  cquantize^.odither[0] := NIL;	{ Also flag odither arrays not allocated }

  { Make sure my internal arrays won't overflow }
  if (cinfo^.out_color_components > MAX_Q_COMPS) then
    ERREXIT1(j_common_ptr(cinfo), JERR_QUANT_COMPONENTS, MAX_Q_COMPS);
  { Make sure colormap indexes can be represented by JSAMPLEs }
  if (cinfo^.desired_number_of_colors > (MAXJSAMPLE+1)) then
    ERREXIT1(j_common_ptr(cinfo), JERR_QUANT_MANY_COLORS, MAXJSAMPLE+1);

  { Create the colormap and color index table. }
  create_colormap(cinfo);
  create_colorindex(cinfo);

  { Allocate Floyd-Steinberg workspace now if requested.
    We do this now since it is FAR storage and may affect the memory
    manager's space calculations.  If the user changes to FS dither
    mode in a later pass, we will allocate the space then, and will
    possibly overrun the max_memory_to_use setting. }

  if (cinfo^.dither_mode = JDITHER_FS) then
    alloc_fs_workspace(cinfo);
end;

{== imjquant2 implementation ==}

{ This module implements the well-known Heckbert paradigm for color
  quantization.  Most of the ideas used here can be traced back to
  Heckbert's seminal paper
    Heckbert, Paul.  "Color Image Quantization for Frame Buffer Display",
    Proc. SIGGRAPH '82, Computer Graphics v.16 #3 (July 1982), pp 297-304.

  In the first pass over the image, we accumulate a histogram showing the
  usage count of each possible color.  To keep the histogram to a reasonable
  size, we reduce the precision of the input; typical practice is to retain
  5 or 6 bits per color, so that 8 or 4 different input values are counted
  in the same histogram cell.

  Next, the color-selection step begins with a box representing the whole
  color space, and repeatedly splits the "largest" remaining box until we
  have as many boxes as desired colors.  Then the mean color in each
  remaining box becomes one of the possible output colors.

  The second pass over the image maps each input pixel to the closest output
  color (optionally after applying a Floyd-Steinberg dithering correction).
  This mapping is logically trivial, but making it go fast enough requires
  considerable care.

  Heckbert-style quantizers vary a good deal in their policies for choosing
  the "largest" box and deciding where to cut it.  The particular policies
  used here have proved out well in experimental comparisons, but better ones
  may yet be found.

  In earlier versions of the IJG code, this module quantized in YCbCr color
  space, processing the raw upsampled data without a color conversion step.
  This allowed the color conversion math to be done only once per colormap
  entry, not once per pixel.  However, that optimization precluded other
  useful optimizations (such as merging color conversion with upsampling)
  and it also interfered with desired capabilities such as quantizing to an
  externally-supplied colormap.  We have therefore abandoned that approach.
  The present code works in the post-conversion color space, typically RGB.

  To improve the visual quality of the results, we actually work in scaled
  RGB space, giving G distances more weight than R, and R in turn more than
  B.  To do everything in integer math, we must use integer scale factors.
  The 2/3/1 scale factors used here correspond loosely to the relative
  weights of the colors in the NTSC grayscale equation.
  If you want to use this code to quantize a non-RGB color space, you'll
  probably need to change these scale factors. }

const
  R_SCALE = 2;          { scale R distances by this much }
  G_SCALE = 3;          { scale G distances by this much }
  B_SCALE = 1;          { and B by this much }

{ Relabel R/G/B as components 0/1/2, respecting the RGB ordering defined
  in jmorecfg.h.  As the code stands, it will do the right thing for R,G,B
  and B,G,R orders.  If you define some other weird order in jmorecfg.h,
  you'll get compile errors until you extend this logic.  In that case
  you'll probably want to tweak the histogram sizes too. }

{$ifdef RGB_RED_IS_0}
const
  C0_SCALE = R_SCALE;
  C1_SCALE = G_SCALE;
  C2_SCALE = B_SCALE;
{$else}
const
  C0_SCALE = B_SCALE;
  C1_SCALE = G_SCALE;
  C2_SCALE = R_SCALE;
{$endif}


{ First we have the histogram data structure and routines for creating it.

  The number of bits of precision can be adjusted by changing these symbols.
  We recommend keeping 6 bits for G and 5 each for R and B.
  If you have plenty of memory and cycles, 6 bits all around gives marginally
  better results; if you are short of memory, 5 bits all around will save
  some space but degrade the results.
  To maintain a fully accurate histogram, we'd need to allocate a "long"
  (preferably unsigned long) for each cell.  In practice this is overkill;
  we can get by with 16 bits per cell.  Few of the cell counts will overflow,
  and clamping those that do overflow to the maximum value will give close-
  enough results.  This reduces the recommended histogram size from 256Kb
  to 128Kb, which is a useful savings on PC-class machines.
  (In the second pass the histogram space is re-used for pixel mapping data;
  in that capacity, each cell must be able to store zero to the number of
  desired colors.  16 bits/cell is plenty for that too.)
  Since the JPEG code is intended to run in small memory model on 80x86
  machines, we can't just allocate the histogram in one chunk.  Instead
  of a true 3-D array, we use a row of pointers to 2-D arrays.  Each
  pointer corresponds to a C0 value (typically 2^5 = 32 pointers) and
  each 2-D array has 2^6*2^5 = 2048 or 2^6*2^6 = 4096 entries.  Note that
  on 80x86 machines, the pointer row is in near memory but the actual
  arrays are in far memory (same arrangement as we use for image arrays). }


const
  MAXNUMCOLORS = (MAXJSAMPLE+1);        { maximum size of colormap }

{ These will do the right thing for either R,G,B or B,G,R color order,
  but you may not like the results for other color orders. }

const
  HIST_C0_BITS = 5;             { bits of precision in R/B histogram }
  HIST_C1_BITS = 6;             { bits of precision in G histogram }
  HIST_C2_BITS = 5;             { bits of precision in B/R histogram }

{ Number of elements along histogram axes. }
const
  HIST_C0_ELEMS = (1 shl HIST_C0_BITS);
  HIST_C1_ELEMS = (1 shl HIST_C1_BITS);
  HIST_C2_ELEMS = (1 shl HIST_C2_BITS);

{ These are the amounts to shift an input value to get a histogram index. }
const
  C0_SHIFT = (BITS_IN_JSAMPLE-HIST_C0_BITS);
  C1_SHIFT = (BITS_IN_JSAMPLE-HIST_C1_BITS);
  C2_SHIFT = (BITS_IN_JSAMPLE-HIST_C2_BITS);


type                            { Nomssi }
  RGBptr = ^RGBtype;
  RGBtype = packed record
    r,g,b : JSAMPLE;
  end;
type
  histcell = UINT16;            { histogram cell; prefer an unsigned type }

type
  histptr = ^histcell {FAR};       { for pointers to histogram cells }

type
  hist1d = array[0..HIST_C2_ELEMS-1] of histcell; { typedefs for the array }
  {hist1d_ptr = ^hist1d;}
  hist1d_field = array[0..HIST_C1_ELEMS-1] of hist1d;
                                  { type for the 2nd-level pointers }
  hist2d = ^hist1d_field;
  hist2d_field = array[0..HIST_C0_ELEMS-1] of hist2d;
  hist3d = ^hist2d_field;	  { type for top-level pointer }


{ Declarations for Floyd-Steinberg dithering.

  Errors are accumulated into the array fserrors[], at a resolution of
  1/16th of a pixel count.  The error at a given pixel is propagated
  to its not-yet-processed neighbors using the standard F-S fractions,
 		...	(here)	7/16
 		3/16	5/16	1/16
  We work left-to-right on even rows, right-to-left on odd rows.

  We can get away with a single array (holding one row's worth of errors)
  by using it to store the current row's errors at pixel columns not yet
  processed, but the next row's errors at columns already processed.  We
  need only a few extra variables to hold the errors immediately around the
  current column.  (If we are lucky, those variables are in registers, but
  even if not, they're probably cheaper to access than array elements are.)

  The fserrors[] array has (#columns + 2) entries; the extra entry at
  each end saves us from special-casing the first and last pixels.
  Each entry is three values long, one value for each color component.

  Note: on a wide image, we might not have enough room in a PC's near data
  segment to hold the error array; so it is allocated with alloc_large. }


{$ifdef BITS_IN_JSAMPLE_IS_8}
type
  jquant2_FSERROR = INT16;              { 16 bits should be enough }
  jquant2_LOCFSERROR = int;             { use 'int' for calculation temps }
{$else}
type
  jquant2_FSERROR = INT32;              { may need more than 16 bits }
  jquant2_LOCFSERROR = INT32;           { be sure calculation temps are big enough }
{$endif}
type                            { Nomssi }
  RGB_FSERROR_PTR = ^RGB_FSERROR;
  RGB_FSERROR = packed record
    r,g,b : jquant2_FSERROR;
  end;
  LOCRGB_FSERROR = packed record
    r,g,b : jquant2_LOCFSERROR;
  end;

type
  FSERROR_PTR = ^jquant2_FSERROR;
  jquant2_jFSError = 0..(MaxInt div SIZEOF(RGB_FSERROR))-1;
  jquant2_FS_ERROR_FIELD = array[jquant2_jFSError] of RGB_FSERROR;
  jquant2_FS_ERROR_FIELD_PTR = ^jquant2_FS_ERROR_FIELD;{far}
                                { pointer to error array (in FAR storage!) }

type
  error_limit_array = array[-MAXJSAMPLE..MAXJSAMPLE] of int;
  { table for clamping the applied error }
  error_limit_ptr = ^error_limit_array;

{ Private subobject }
type
  jquant2_my_cquantize_ptr = ^jquant2_my_cquantizer;
  jquant2_my_cquantizer = record
    pub : jpeg_color_quantizer; { public fields }

    { Space for the eventually created colormap is stashed here }
    sv_colormap : JSAMPARRAY;	{ colormap allocated at init time }
    desired : int;              { desired # of colors = size of colormap }

    { Variables for accumulating image statistics }
    histogram : hist3d;         { pointer to the histogram }

    needs_zeroed : boolean;     { TRUE if next pass must zero histogram }

    { Variables for Floyd-Steinberg dithering }
    fserrors : jquant2_FS_ERROR_FIELD_PTR;        { accumulated errors }
    on_odd_row : boolean;       { flag to remember which row we are on }
    error_limiter : error_limit_ptr; { table for clamping the applied error }
  end;


{ Prescan some rows of pixels.
  In this module the prescan simply updates the histogram, which has been
  initialized to zeroes by start_pass.
  An output_buf parameter is required by the method signature, but no data
  is actually output (in fact the buffer controller is probably passing a
  NIL pointer). }

{METHODDEF}
procedure prescan_quantize (cinfo : j_decompress_ptr;
                            input_buf : JSAMPARRAY;
                            output_buf : JSAMPARRAY;
                            num_rows : int);
var
  cquantize : jquant2_my_cquantize_ptr;
  {register} ptr : RGBptr;
  {register} histp : histptr;
  {register} histogram : hist3d;
  row : int;
  col : JDIMENSION;
  width : JDIMENSION;
begin
  cquantize := jquant2_my_cquantize_ptr(cinfo^.cquantize);
  histogram := cquantize^.histogram;
  width := cinfo^.output_width;

  for row := 0 to pred(num_rows) do
  begin
    ptr := RGBptr(input_buf^[row]);
    for col := pred(width) downto 0 do
    begin
      { get pixel value and index into the histogram }
      histp := @(histogram^[GETJSAMPLE(ptr^.r) shr C0_SHIFT]^
                           [GETJSAMPLE(ptr^.g) shr C1_SHIFT]
			   [GETJSAMPLE(ptr^.b) shr C2_SHIFT]);
      { increment, check for overflow and undo increment if so. }
      Inc(histp^);
      if (histp^ <= 0) then
	Dec(histp^);
      Inc(ptr);
    end;
  end;
end;

{ Next we have the really interesting routines: selection of a colormap
  given the completed histogram.
  These routines work with a list of "boxes", each representing a rectangular
  subset of the input color space (to histogram precision). }

type
  box = record
  { The bounds of the box (inclusive); expressed as histogram indexes }
    c0min, c0max : int;
    c1min, c1max : int;
    c2min, c2max : int;
    { The volume (actually 2-norm) of the box }
    volume : INT32;
    { The number of nonzero histogram cells within this box }
    colorcount : long;
  end;

type
  jBoxList = 0..(MaxInt div SizeOf(box))-1;
  box_field = array[jBoxlist] of box;
  boxlistptr = ^box_field;
  boxptr = ^box;

{LOCAL}
function find_biggest_color_pop (boxlist : boxlistptr; numboxes : int) : boxptr;
{ Find the splittable box with the largest color population }
{ Returns NIL if no splittable boxes remain }
var
  boxp : boxptr ; {register}
  i : int;        {register}
  maxc : long;    {register}
  which : boxptr;
begin
  which := NIL;
  boxp := @(boxlist^[0]);
  maxc := 0;
  for i := 0 to pred(numboxes) do
  begin
    if (boxp^.colorcount > maxc) and (boxp^.volume > 0) then
    begin
      which := boxp;
      maxc := boxp^.colorcount;
    end;
    Inc(boxp);
  end;
  find_biggest_color_pop := which;
end;


{LOCAL}
function find_biggest_volume (boxlist : boxlistptr; numboxes : int) : boxptr;
{ Find the splittable box with the largest (scaled) volume }
{ Returns NULL if no splittable boxes remain }
var
  {register} boxp : boxptr;
  {register} i : int;
  {register} maxv : INT32;
  which : boxptr;
begin
  maxv := 0;
  which := NIL;
  boxp := @(boxlist^[0]);
  for i := 0 to pred(numboxes) do
  begin
    if (boxp^.volume > maxv) then
    begin
      which := boxp;
      maxv := boxp^.volume;
    end;
    Inc(boxp);
  end;
  find_biggest_volume := which;
end;


{LOCAL}
procedure update_box (cinfo : j_decompress_ptr; var boxp : box);
label
  have_c0min, have_c0max,
  have_c1min, have_c1max,
  have_c2min, have_c2max;
{ Shrink the min/max bounds of a box to enclose only nonzero elements, }
{ and recompute its volume and population }
var
  cquantize : jquant2_my_cquantize_ptr;
  histogram : hist3d;
  histp : histptr;
  c0,c1,c2 : int;
  c0min,c0max,c1min,c1max,c2min,c2max : int;
  dist0,dist1,dist2 : INT32;
  ccount : long;
begin
  cquantize := jquant2_my_cquantize_ptr(cinfo^.cquantize);
  histogram := cquantize^.histogram;

  c0min := boxp.c0min;  c0max := boxp.c0max;
  c1min := boxp.c1min;  c1max := boxp.c1max;
  c2min := boxp.c2min;  c2max := boxp.c2max;

  if (c0max > c0min) then
    for c0 := c0min to c0max do
      for c1 := c1min to c1max do
      begin
	histp := @(histogram^[c0]^[c1][c2min]);
	for c2 := c2min to c2max do
        begin
	  if (histp^ <> 0) then
          begin
            c0min := c0;
	    boxp.c0min := c0min;
	    goto have_c0min;
	  end;
          Inc(histp);
        end;
      end;
 have_c0min:
  if (c0max > c0min) then
    for c0 := c0max downto c0min do
      for c1 := c1min to c1max do
      begin
	histp := @(histogram^[c0]^[c1][c2min]);
	for c2 := c2min to c2max do
        begin
	  if ( histp^ <> 0) then
          begin
            c0max := c0;
	    boxp.c0max := c0;
	    goto have_c0max;
	  end;
          Inc(histp);
        end;
      end;
 have_c0max:
  if (c1max > c1min) then
    for c1 := c1min to c1max do
      for c0 := c0min to c0max do
      begin
	histp := @(histogram^[c0]^[c1][c2min]);
	for c2 := c2min to c2max do
        begin
	  if (histp^ <> 0) then
          begin
            c1min := c1;
	    boxp.c1min := c1;
	    goto have_c1min;
	  end;
          Inc(histp);
        end;
      end;
 have_c1min:
  if (c1max > c1min) then
    for c1 := c1max downto c1min do
      for c0 := c0min to c0max do
      begin
	histp := @(histogram^[c0]^[c1][c2min]);
	for c2 := c2min to c2max do
        begin
	  if (histp^ <> 0) then
          begin
            c1max := c1;
	    boxp.c1max := c1;
	    goto have_c1max;
	  end;
          Inc(histp);
        end;
      end;
 have_c1max:
  if (c2max > c2min) then
    for c2 := c2min to c2max do
      for c0 := c0min to c0max do
      begin
	histp := @(histogram^[c0]^[c1min][c2]);
	for c1 := c1min to c1max do
        begin
	  if (histp^ <> 0) then
          begin
	    c2min := c2;
	    boxp.c2min := c2min;
	    goto have_c2min;
	  end;
          Inc(histp, HIST_C2_ELEMS);
        end;
      end;
 have_c2min:
  if (c2max > c2min) then
    for c2 := c2max downto c2min do
      for c0 := c0min to c0max do
      begin
	histp := @(histogram^[c0]^[c1min][c2]);
	for c1 := c1min to c1max do
        begin
	  if (histp^ <> 0) then
          begin
	    c2max := c2;
	    boxp.c2max := c2max;
	    goto have_c2max;
	  end;
          Inc(histp, HIST_C2_ELEMS);
        end;
      end;
 have_c2max:

  { Update box volume.
    We use 2-norm rather than real volume here; this biases the method
    against making long narrow boxes, and it has the side benefit that
    a box is splittable iff norm > 0.
    Since the differences are expressed in histogram-cell units,
    we have to shift back to JSAMPLE units to get consistent distances;
    after which, we scale according to the selected distance scale factors.}

  dist0 := ((c0max - c0min) shl C0_SHIFT) * C0_SCALE;
  dist1 := ((c1max - c1min) shl C1_SHIFT) * C1_SCALE;
  dist2 := ((c2max - c2min) shl C2_SHIFT) * C2_SCALE;
  boxp.volume := dist0*dist0 + dist1*dist1 + dist2*dist2;

  { Now scan remaining volume of box and compute population }
  ccount := 0;
  for c0 := c0min to c0max do
    for c1 := c1min to c1max do
    begin
      histp := @(histogram^[c0]^[c1][c2min]);
      for c2 := c2min to c2max do
      begin
	if (histp^ <> 0) then
	  Inc(ccount);
        Inc(histp);
      end;
    end;
  boxp.colorcount := ccount;
end;


{LOCAL}
function median_cut (cinfo : j_decompress_ptr; boxlist : boxlistptr;
                     numboxes : int; desired_colors : int) : int;
{ Repeatedly select and split the largest box until we have enough boxes }
var
  n,lb : int;
  c0,c1,c2,cmax : int;
  {register} b1,b2 : boxptr;
begin
  while (numboxes < desired_colors) do
  begin
    { Select box to split.
      Current algorithm: by population for first half, then by volume. }

    if (numboxes*2 <= desired_colors) then
      b1 := find_biggest_color_pop(boxlist, numboxes)
    else
      b1 := find_biggest_volume(boxlist, numboxes);

    if (b1 = NIL) then          { no splittable boxes left! }
      break;
    b2 := @(boxlist^[numboxes]);	{ where new box will go }
    { Copy the color bounds to the new box. }
    b2^.c0max := b1^.c0max; b2^.c1max := b1^.c1max; b2^.c2max := b1^.c2max;
    b2^.c0min := b1^.c0min; b2^.c1min := b1^.c1min; b2^.c2min := b1^.c2min;
    { Choose which axis to split the box on.
      Current algorithm: longest scaled axis.
      See notes in update_box about scaling distances. }

    c0 := ((b1^.c0max - b1^.c0min) shl C0_SHIFT) * C0_SCALE;
    c1 := ((b1^.c1max - b1^.c1min) shl C1_SHIFT) * C1_SCALE;
    c2 := ((b1^.c2max - b1^.c2min) shl C2_SHIFT) * C2_SCALE;
    { We want to break any ties in favor of green, then red, blue last.
      This code does the right thing for R,G,B or B,G,R color orders only. }

{$ifdef RGB_RED_IS_0}
    cmax := c1; n := 1;
    if (c0 > cmax) then
    begin
      cmax := c0;
      n := 0;
    end;
    if (c2 > cmax) then
      n := 2;
{$else}
    cmax := c1;
    n := 1;
    if (c2 > cmax) then
    begin
      cmax := c2;
      n := 2;
    end;
    if (c0 > cmax) then
      n := 0;
{$endif}
    { Choose split point along selected axis, and update box bounds.
      Current algorithm: split at halfway point.
      (Since the box has been shrunk to minimum volume,
      any split will produce two nonempty subboxes.)
      Note that lb value is max for lower box, so must be < old max. }

    case n of
    0:begin
        lb := (b1^.c0max + b1^.c0min) div 2;
        b1^.c0max := lb;
        b2^.c0min := lb+1;
      end;
    1:begin
        lb := (b1^.c1max + b1^.c1min) div 2;
        b1^.c1max := lb;
        b2^.c1min := lb+1;
      end;
    2:begin
        lb := (b1^.c2max + b1^.c2min) div 2;
        b1^.c2max := lb;
        b2^.c2min := lb+1;
      end;
    end;
    { Update stats for boxes }
    update_box(cinfo, b1^);
    update_box(cinfo, b2^);
    Inc(numboxes);
  end;
  median_cut := numboxes;
end;


{LOCAL}
procedure compute_color (cinfo : j_decompress_ptr;
                         const boxp : box; icolor : int);
{ Compute representative color for a box, put it in colormap[icolor] }
var
  { Current algorithm: mean weighted by pixels (not colors) }
  { Note it is important to get the rounding correct! }
  cquantize : jquant2_my_cquantize_ptr;
  histogram : hist3d;
  histp : histptr;
  c0,c1,c2 : int;
  c0min,c0max,c1min,c1max,c2min,c2max : int;
  count : long;
  total : long;
  c0total : long;
  c1total : long;
  c2total : long;
begin
  cquantize := jquant2_my_cquantize_ptr(cinfo^.cquantize);
  histogram := cquantize^.histogram;
  total := 0;
  c0total := 0;
  c1total := 0;
  c2total := 0;

  c0min := boxp.c0min;  c0max := boxp.c0max;
  c1min := boxp.c1min;  c1max := boxp.c1max;
  c2min := boxp.c2min;  c2max := boxp.c2max;

  for c0 := c0min to c0max do
    for c1 := c1min to c1max do
    begin
      histp := @(histogram^[c0]^[c1][c2min]);
      for c2 := c2min to c2max do
      begin
	count := histp^;
        Inc(histp);
	if (count <> 0) then
        begin
	  Inc(total, count);
	  Inc(c0total, ((c0 shl C0_SHIFT) + ((1 shl C0_SHIFT) shr 1)) * count);
	  Inc(c1total, ((c1 shl C1_SHIFT) + ((1 shl C1_SHIFT) shr 1)) * count);
	  Inc(c2total, ((c2 shl C2_SHIFT) + ((1 shl C2_SHIFT) shr 1)) * count);
	end;
      end;
    end;

  cinfo^.colormap^[0]^[icolor] := JSAMPLE ((c0total + (total shr 1)) div total);
  cinfo^.colormap^[1]^[icolor] := JSAMPLE ((c1total + (total shr 1)) div total);
  cinfo^.colormap^[2]^[icolor] := JSAMPLE ((c2total + (total shr 1)) div total);
end;


{LOCAL}
procedure select_colors (cinfo : j_decompress_ptr; desired_colors : int);
{ Master routine for color selection }
var
  boxlist : boxlistptr;
  numboxes : int;
  i : int;
begin
  { Allocate workspace for box list }
  boxlist := boxlistptr(cinfo^.mem^.alloc_small(
    j_common_ptr(cinfo), JPOOL_IMAGE, desired_colors * SIZEOF(box)));
  { Initialize one box containing whole space }
  numboxes := 1;
  boxlist^[0].c0min := 0;
  boxlist^[0].c0max := MAXJSAMPLE shr C0_SHIFT;
  boxlist^[0].c1min := 0;
  boxlist^[0].c1max := MAXJSAMPLE shr C1_SHIFT;
  boxlist^[0].c2min := 0;
  boxlist^[0].c2max := MAXJSAMPLE shr C2_SHIFT;
  { Shrink it to actually-used volume and set its statistics }
  update_box(cinfo, boxlist^[0]);
  { Perform median-cut to produce final box list }
  numboxes := median_cut(cinfo, boxlist, numboxes, desired_colors);
  { Compute the representative color for each box, fill colormap }
  for i := 0 to pred(numboxes) do
    compute_color(cinfo, boxlist^[i], i);
  cinfo^.actual_number_of_colors := numboxes;
  {$IFDEF DEBUG}
  TRACEMS1(j_common_ptr(cinfo), 1, JTRC_QUANT_SELECTED, numboxes);
  {$ENDIF}
end;


{ These routines are concerned with the time-critical task of mapping input
  colors to the nearest color in the selected colormap.

  We re-use the histogram space as an "inverse color map", essentially a
  cache for the results of nearest-color searches.  All colors within a
  histogram cell will be mapped to the same colormap entry, namely the one
  closest to the cell's center.  This may not be quite the closest entry to
  the actual input color, but it's almost as good.  A zero in the cache
  indicates we haven't found the nearest color for that cell yet; the array
  is cleared to zeroes before starting the mapping pass.  When we find the
  nearest color for a cell, its colormap index plus one is recorded in the
  cache for future use.  The pass2 scanning routines call fill_inverse_cmap
  when they need to use an unfilled entry in the cache.

  Our method of efficiently finding nearest colors is based on the "locally
  sorted search" idea described by Heckbert and on the incremental distance
  calculation described by Spencer W. Thomas in chapter III.1 of Graphics
  Gems II (James Arvo, ed.  Academic Press, 1991).  Thomas points out that
  the distances from a given colormap entry to each cell of the histogram can
  be computed quickly using an incremental method: the differences between
  distances to adjacent cells themselves differ by a constant.  This allows a
  fairly fast implementation of the "brute force" approach of computing the
  distance from every colormap entry to every histogram cell.  Unfortunately,
  it needs a work array to hold the best-distance-so-far for each histogram
  cell (because the inner loop has to be over cells, not colormap entries).
  The work array elements have to be INT32s, so the work array would need
  256Kb at our recommended precision.  This is not feasible in DOS machines.

  To get around these problems, we apply Thomas' method to compute the
  nearest colors for only the cells within a small subbox of the histogram.
  The work array need be only as big as the subbox, so the memory usage
  problem is solved.  Furthermore, we need not fill subboxes that are never
  referenced in pass2; many images use only part of the color gamut, so a
  fair amount of work is saved.  An additional advantage of this
  approach is that we can apply Heckbert's locality criterion to quickly
  eliminate colormap entries that are far away from the subbox; typically
  three-fourths of the colormap entries are rejected by Heckbert's criterion,
  and we need not compute their distances to individual cells in the subbox.
  The speed of this approach is heavily influenced by the subbox size: too
  small means too much overhead, too big loses because Heckbert's criterion
  can't eliminate as many colormap entries.  Empirically the best subbox
  size seems to be about 1/512th of the histogram (1/8th in each direction).

  Thomas' article also describes a refined method which is asymptotically
  faster than the brute-force method, but it is also far more complex and
  cannot efficiently be applied to small subboxes.  It is therefore not
  useful for programs intended to be portable to DOS machines.  On machines
  with plenty of memory, filling the whole histogram in one shot with Thomas'
  refined method might be faster than the present code --- but then again,
  it might not be any faster, and it's certainly more complicated. }


{ log2(histogram cells in update box) for each axis; this can be adjusted }
const
  BOX_C0_LOG = (HIST_C0_BITS-3);
  BOX_C1_LOG = (HIST_C1_BITS-3);
  BOX_C2_LOG = (HIST_C2_BITS-3);

  BOX_C0_ELEMS = (1 shl BOX_C0_LOG); { # of hist cells in update box }
  BOX_C1_ELEMS = (1 shl BOX_C1_LOG);
  BOX_C2_ELEMS = (1 shl BOX_C2_LOG);

  BOX_C0_SHIFT = (C0_SHIFT + BOX_C0_LOG);
  BOX_C1_SHIFT = (C1_SHIFT + BOX_C1_LOG);
  BOX_C2_SHIFT = (C2_SHIFT + BOX_C2_LOG);


{ The next three routines implement inverse colormap filling.  They could
  all be folded into one big routine, but splitting them up this way saves
  some stack space (the mindist[] and bestdist[] arrays need not coexist)
  and may allow some compilers to produce better code by registerizing more
  inner-loop variables. }

{LOCAL}
function find_nearby_colors (cinfo : j_decompress_ptr;
                             minc0 : int; minc1 : int; minc2 : int;
		             var colorlist : array of JSAMPLE) : int;
{ Locate the colormap entries close enough to an update box to be candidates
  for the nearest entry to some cell(s) in the update box.  The update box
  is specified by the center coordinates of its first cell.  The number of
  candidate colormap entries is returned, and their colormap indexes are
  placed in colorlist[].
  This routine uses Heckbert's "locally sorted search" criterion to select
  the colors that need further consideration. }

var
  numcolors : int;
  maxc0, maxc1, maxc2 : int;
  centerc0, centerc1, centerc2 : int;
  i, x, ncolors : int;
  minmaxdist, min_dist, max_dist, tdist : INT32;
  mindist : array[0..MAXNUMCOLORS-1] of INT32;
  	{ min distance to colormap entry i }
begin
  numcolors := cinfo^.actual_number_of_colors;

  { Compute true coordinates of update box's upper corner and center.
    Actually we compute the coordinates of the center of the upper-corner
    histogram cell, which are the upper bounds of the volume we care about.
    Note that since ">>" rounds down, the "center" values may be closer to
    min than to max; hence comparisons to them must be "<=", not "<". }

  maxc0 := minc0 + ((1 shl BOX_C0_SHIFT) - (1 shl C0_SHIFT));
  centerc0 := (minc0 + maxc0) shr 1;
  maxc1 := minc1 + ((1 shl BOX_C1_SHIFT) - (1 shl C1_SHIFT));
  centerc1 := (minc1 + maxc1) shr 1;
  maxc2 := minc2 + ((1 shl BOX_C2_SHIFT) - (1 shl C2_SHIFT));
  centerc2 := (minc2 + maxc2) shr 1;

  { For each color in colormap, find:
     1. its minimum squared-distance to any point in the update box
        (zero if color is within update box);
     2. its maximum squared-distance to any point in the update box.
    Both of these can be found by considering only the corners of the box.
    We save the minimum distance for each color in mindist[];
    only the smallest maximum distance is of interest. }

  minmaxdist := long($7FFFFFFF);

  for i := 0 to pred(numcolors) do
  begin
    { We compute the squared-c0-distance term, then add in the other two. }
    x := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
    if (x < minc0) then
    begin
      tdist := (x - minc0) * C0_SCALE;
      min_dist := tdist*tdist;
      tdist := (x - maxc0) * C0_SCALE;
      max_dist := tdist*tdist;
    end
    else
      if (x > maxc0) then
      begin
        tdist := (x - maxc0) * C0_SCALE;
        min_dist := tdist*tdist;
        tdist := (x - minc0) * C0_SCALE;
        max_dist := tdist*tdist;
      end
      else
      begin
        { within cell range so no contribution to min_dist }
        min_dist := 0;
        if (x <= centerc0) then
        begin
          tdist := (x - maxc0) * C0_SCALE;
          max_dist := tdist*tdist;
        end
        else
        begin
          tdist := (x - minc0) * C0_SCALE;
          max_dist := tdist*tdist;
        end;
      end;

    x := GETJSAMPLE(cinfo^.colormap^[1]^[i]);
    if (x < minc1) then
    begin
      tdist := (x - minc1) * C1_SCALE;
      Inc(min_dist, tdist*tdist);
      tdist := (x - maxc1) * C1_SCALE;
      Inc(max_dist, tdist*tdist);
    end
    else
      if (x > maxc1) then
      begin
        tdist := (x - maxc1) * C1_SCALE;
        Inc(min_dist, tdist*tdist);
        tdist := (x - minc1) * C1_SCALE;
        Inc(max_dist, tdist*tdist);
      end
      else
      begin
        { within cell range so no contribution to min_dist }
        if (x <= centerc1) then
        begin
	  tdist := (x - maxc1) * C1_SCALE;
	  Inc(max_dist, tdist*tdist);
        end
        else
        begin
	  tdist := (x - minc1) * C1_SCALE;
	  Inc(max_dist, tdist*tdist);
        end
      end;

    x := GETJSAMPLE(cinfo^.colormap^[2]^[i]);
    if (x < minc2) then
    begin
      tdist := (x - minc2) * C2_SCALE;
      Inc(min_dist, tdist*tdist);
      tdist := (x - maxc2) * C2_SCALE;
      Inc(max_dist, tdist*tdist);
    end
    else
      if (x > maxc2) then
      begin
        tdist := (x - maxc2) * C2_SCALE;
        Inc(min_dist, tdist*tdist);
        tdist := (x - minc2) * C2_SCALE;
        Inc(max_dist, tdist*tdist);
      end
      else
      begin
        { within cell range so no contribution to min_dist }
        if (x <= centerc2) then
        begin
	  tdist := (x - maxc2) * C2_SCALE;
	  Inc(max_dist, tdist*tdist);
        end
        else
        begin
	  tdist := (x - minc2) * C2_SCALE;
	  Inc(max_dist, tdist*tdist);
        end;
      end;

    mindist[i] := min_dist;	{ save away the results }
    if (max_dist < minmaxdist) then
      minmaxdist := max_dist;
  end;

  { Now we know that no cell in the update box is more than minmaxdist
    away from some colormap entry.  Therefore, only colors that are
    within minmaxdist of some part of the box need be considered. }

  ncolors := 0;
  for i := 0 to pred(numcolors) do
  begin
    if (mindist[i] <= minmaxdist) then
    begin
      colorlist[ncolors] := JSAMPLE(i);
      Inc(ncolors);
    end;
  end;
  find_nearby_colors := ncolors;
end;


{LOCAL}
procedure find_best_colors (cinfo : j_decompress_ptr;
                            minc0 : int; minc1 : int; minc2 : int;
                            numcolors : int;
                            var colorlist : array of JSAMPLE;
                            var bestcolor : array of JSAMPLE);
{ Find the closest colormap entry for each cell in the update box,
  given the list of candidate colors prepared by find_nearby_colors.
  Return the indexes of the closest entries in the bestcolor[] array.
  This routine uses Thomas' incremental distance calculation method to
  find the distance from a colormap entry to successive cells in the box. }
const
  { Nominal steps between cell centers ("x" in Thomas article) }
  STEP_C0 = ((1 shl C0_SHIFT) * C0_SCALE);
  STEP_C1 = ((1 shl C1_SHIFT) * C1_SCALE);
  STEP_C2 = ((1 shl C2_SHIFT) * C2_SCALE);
var
  ic0, ic1, ic2 : int;
  i, icolor : int;
  {register} bptr : INT32PTR;     { pointer into bestdist[] array }
  cptr : JSAMPLE_PTR;              { pointer into bestcolor[] array }
  dist0, dist1 : INT32;         { initial distance values }
  {register} dist2 : INT32;	{ current distance in inner loop }
  xx0, xx1 : INT32;             { distance increments }
  {register} xx2 : INT32;
  inc0, inc1, inc2 : INT32;	{ initial values for increments }
  { This array holds the distance to the nearest-so-far color for each cell }
  bestdist : array[0..BOX_C0_ELEMS * BOX_C1_ELEMS * BOX_C2_ELEMS-1] of INT32;
begin
  { Initialize best-distance for each cell of the update box }
  for i := BOX_C0_ELEMS*BOX_C1_ELEMS*BOX_C2_ELEMS-1 downto 0 do
    bestdist[i] := $7FFFFFFF;

  { For each color selected by find_nearby_colors,
    compute its distance to the center of each cell in the box.
    If that's less than best-so-far, update best distance and color number. }


  for i := 0 to pred(numcolors) do
  begin
    icolor := GETJSAMPLE(colorlist[i]);
    { Compute (square of) distance from minc0/c1/c2 to this color }
    inc0 := (minc0 - GETJSAMPLE(cinfo^.colormap^[0]^[icolor])) * C0_SCALE;
    dist0 := inc0*inc0;
    inc1 := (minc1 - GETJSAMPLE(cinfo^.colormap^[1]^[icolor])) * C1_SCALE;
    Inc(dist0, inc1*inc1);
    inc2 := (minc2 - GETJSAMPLE(cinfo^.colormap^[2]^[icolor])) * C2_SCALE;
    Inc(dist0, inc2*inc2);
    { Form the initial difference increments }
    inc0 := inc0 * (2 * STEP_C0) + STEP_C0 * STEP_C0;
    inc1 := inc1 * (2 * STEP_C1) + STEP_C1 * STEP_C1;
    inc2 := inc2 * (2 * STEP_C2) + STEP_C2 * STEP_C2;
    { Now loop over all cells in box, updating distance per Thomas method }
    bptr := @bestdist[0];
    cptr := @bestcolor[0];
    xx0 := inc0;
    for ic0 := BOX_C0_ELEMS-1 downto 0 do
    begin
      dist1 := dist0;
      xx1 := inc1;
      for ic1 := BOX_C1_ELEMS-1 downto 0 do
      begin
	dist2 := dist1;
	xx2 := inc2;
	for ic2 := BOX_C2_ELEMS-1 downto 0 do
        begin
	  if (dist2 < bptr^) then
          begin
	    bptr^ := dist2;
	    cptr^ := JSAMPLE (icolor);
	  end;
	  Inc(dist2, xx2);
	  Inc(xx2, 2 * STEP_C2 * STEP_C2);
	  Inc(bptr);
	  Inc(cptr);
	end;
	Inc(dist1, xx1);
	Inc(xx1, 2 * STEP_C1 * STEP_C1);
      end;
      Inc(dist0, xx0);
      Inc(xx0, 2 * STEP_C0 * STEP_C0);
    end;
  end;
end;


{LOCAL}
procedure fill_inverse_cmap (cinfo : j_decompress_ptr;
                             c0 : int; c1 : int; c2 : int);
{ Fill the inverse-colormap entries in the update box that contains }
{ histogram cell c0/c1/c2.  (Only that one cell MUST be filled, but }
{ we can fill as many others as we wish.) }
var
  cquantize : jquant2_my_cquantize_ptr;
  histogram : hist3d;
  minc0, minc1, minc2 : int;    { lower left corner of update box }
  ic0, ic1, ic2 : int;
  {register} cptr : JSAMPLE_PTR;	{ pointer into bestcolor[] array }
  {register} cachep : histptr;	{ pointer into main cache array }
  { This array lists the candidate colormap indexes. }
  colorlist : array[0..MAXNUMCOLORS-1] of JSAMPLE;
  numcolors : int;		{ number of candidate colors }
  { This array holds the actually closest colormap index for each cell. }
  bestcolor : array[0..BOX_C0_ELEMS * BOX_C1_ELEMS * BOX_C2_ELEMS-1] of JSAMPLE;
begin
  cquantize := jquant2_my_cquantize_ptr (cinfo^.cquantize);
  histogram := cquantize^.histogram;

  { Convert cell coordinates to update box ID }
  c0 := c0 shr BOX_C0_LOG;
  c1 := c1 shr BOX_C1_LOG;
  c2 := c2 shr BOX_C2_LOG;

  { Compute true coordinates of update box's origin corner.
    Actually we compute the coordinates of the center of the corner
    histogram cell, which are the lower bounds of the volume we care about.}

  minc0 := (c0 shl BOX_C0_SHIFT) + ((1 shl C0_SHIFT) shr 1);
  minc1 := (c1 shl BOX_C1_SHIFT) + ((1 shl C1_SHIFT) shr 1);
  minc2 := (c2 shl BOX_C2_SHIFT) + ((1 shl C2_SHIFT) shr 1);

  { Determine which colormap entries are close enough to be candidates
    for the nearest entry to some cell in the update box. }

  numcolors := find_nearby_colors(cinfo, minc0, minc1, minc2, colorlist);

  { Determine the actually nearest colors. }
  find_best_colors(cinfo, minc0, minc1, minc2, numcolors, colorlist,
		   bestcolor);

  { Save the best color numbers (plus 1) in the main cache array }
  c0 := c0 shl BOX_C0_LOG;		{ convert ID back to base cell indexes }
  c1 := c1 shl BOX_C1_LOG;
  c2 := c2 shl BOX_C2_LOG;
  cptr := @(bestcolor[0]);
  for ic0 := 0 to pred(BOX_C0_ELEMS) do
    for ic1 := 0 to pred(BOX_C1_ELEMS) do
    begin
      cachep := @(histogram^[c0+ic0]^[c1+ic1][c2]);
      for ic2 := 0 to pred(BOX_C2_ELEMS) do
      begin
	cachep^ := histcell (GETJSAMPLE(cptr^) + 1);
        Inc(cachep);
        Inc(cptr);
      end;
    end;
end;


{ Map some rows of pixels to the output colormapped representation. }

{METHODDEF}
procedure pass2_no_dither (cinfo : j_decompress_ptr;
		           input_buf : JSAMPARRAY;
                           output_buf : JSAMPARRAY;
                           num_rows : int);
{ This version performs no dithering }
var
  cquantize : jquant2_my_cquantize_ptr;
  histogram : hist3d;
  {register} inptr : RGBptr;
             outptr : JSAMPLE_PTR;
  {register} cachep : histptr;
  {register} c0, c1, c2 : int;
  row : int;
  col : JDIMENSION;
  width : JDIMENSION;
begin
  cquantize := jquant2_my_cquantize_ptr (cinfo^.cquantize);
  histogram := cquantize^.histogram;
  width := cinfo^.output_width;

  for row := 0 to pred(num_rows) do
  begin
    inptr := RGBptr(input_buf^[row]);
    outptr := JSAMPLE_PTR(output_buf^[row]);
    for col := pred(width) downto 0 do
    begin
      { get pixel value and index into the cache }
      c0 := GETJSAMPLE(inptr^.r) shr C0_SHIFT;
      c1 := GETJSAMPLE(inptr^.g) shr C1_SHIFT;
      c2 := GETJSAMPLE(inptr^.b) shr C2_SHIFT;
      Inc(inptr);
      cachep := @(histogram^[c0]^[c1][c2]);
      { If we have not seen this color before, find nearest colormap entry }
      { and update the cache }
      if (cachep^ = 0) then
	fill_inverse_cmap(cinfo, c0,c1,c2);
      { Now emit the colormap index for this cell }
      outptr^ := JSAMPLE (cachep^ - 1);
      Inc(outptr);
    end;
  end;
end;


{METHODDEF}
procedure pass2_fs_dither (cinfo : j_decompress_ptr;
		           input_buf : JSAMPARRAY;
                           output_buf : JSAMPARRAY;
                           num_rows : int);
{ This version performs Floyd-Steinberg dithering }
var
  cquantize : jquant2_my_cquantize_ptr;
  histogram : hist3d;
  {register} cur : LOCRGB_FSERROR;	{ current error or pixel value }
  belowerr : LOCRGB_FSERROR; { error for pixel below cur }
  bpreverr : LOCRGB_FSERROR; { error for below/prev col }
  prev_errorptr,
  {register} errorptr : RGB_FSERROR_PTR;	{ => fserrors[] at column before current }
  inptr : RGBptr;		{ => current input pixel }
  outptr : JSAMPLE_PTR;		{ => current output pixel }
  cachep : histptr;
  dir : int;			{ +1 or -1 depending on direction }
  row : int;
  col : JDIMENSION;
  width : JDIMENSION;
  range_limit : range_limit_table_ptr;
  error_limit : error_limit_ptr;
  colormap0 : JSAMPROW;
  colormap1 : JSAMPROW;
  colormap2 : JSAMPROW;
  {register} pixcode : int;
  {register} bnexterr, delta : jquant2_LOCFSERROR;
begin
  cquantize := jquant2_my_cquantize_ptr (cinfo^.cquantize);
  histogram := cquantize^.histogram;
  width := cinfo^.output_width;
  range_limit := cinfo^.sample_range_limit;
  error_limit := cquantize^.error_limiter;
  colormap0 := cinfo^.colormap^[0];
  colormap1 := cinfo^.colormap^[1];
  colormap2 := cinfo^.colormap^[2];

  for row := 0 to pred(num_rows) do
  begin
    inptr := RGBptr(input_buf^[row]);
    outptr := JSAMPLE_PTR(output_buf^[row]);
    errorptr := RGB_FSERROR_PTR(cquantize^.fserrors); { => entry before first real column }
    if (cquantize^.on_odd_row) then
    begin
      { work right to left in this row }
      Inc(inptr, (width-1));     { so point to rightmost pixel }
      Inc(outptr, width-1);
      dir := -1;
      Inc(errorptr, (width+1)); { => entry after last column }
      cquantize^.on_odd_row := FALSE; { flip for next time }
    end
    else
    begin
      { work left to right in this row }
      dir := 1;
      cquantize^.on_odd_row := TRUE; { flip for next time }
    end;

    { Preset error values: no error propagated to first pixel from left }
    cur.r := 0;
    cur.g := 0;
    cur.b := 0;
    { and no error propagated to row below yet }
    belowerr.r := 0;
    belowerr.g := 0;
    belowerr.b := 0;
    bpreverr.r := 0;
    bpreverr.g := 0;
    bpreverr.b := 0;

    for col := pred(width) downto 0 do
    begin
      prev_errorptr := errorptr;
      Inc(errorptr, dir);	{ advance errorptr to current column }

      { curN holds the error propagated from the previous pixel on the
        current line.  Add the error propagated from the previous line
        to form the complete error correction term for this pixel, and
        round the error term (which is expressed * 16) to an integer.
        RIGHT_SHIFT rounds towards minus infinity, so adding 8 is correct
        for either sign of the error value.
        Note: prev_errorptr points to *previous* column's array entry. }

      { Nomssi Note: Borland Pascal SHR is unsigned }
      cur.r := (cur.r + errorptr^.r + 8) div 16;
      cur.g := (cur.g + errorptr^.g + 8) div 16;
      cur.b := (cur.b + errorptr^.b + 8) div 16;
      { Limit the error using transfer function set by init_error_limit.
        See comments with init_error_limit for rationale. }

      cur.r := error_limit^[cur.r];
      cur.g := error_limit^[cur.g];
      cur.b := error_limit^[cur.b];
      { Form pixel value + error, and range-limit to 0..MAXJSAMPLE.
        The maximum error is +- MAXJSAMPLE (or less with error limiting);
        this sets the required size of the range_limit array. }

      Inc(cur.r, GETJSAMPLE(inptr^.r));
      Inc(cur.g, GETJSAMPLE(inptr^.g));
      Inc(cur.b, GETJSAMPLE(inptr^.b));

      cur.r := GETJSAMPLE(range_limit^[cur.r]);
      cur.g := GETJSAMPLE(range_limit^[cur.g]);
      cur.b := GETJSAMPLE(range_limit^[cur.b]);
      { Index into the cache with adjusted pixel value }
      cachep := @(histogram^[cur.r shr C0_SHIFT]^
                            [cur.g shr C1_SHIFT][cur.b shr C2_SHIFT]);
      { If we have not seen this color before, find nearest colormap }
      { entry and update the cache }
      if (cachep^ = 0) then
	fill_inverse_cmap(cinfo, cur.r shr C0_SHIFT,
                                 cur.g shr C1_SHIFT,
                                 cur.b shr C2_SHIFT);
      { Now emit the colormap index for this cell }

      pixcode := cachep^ - 1;
      outptr^ := JSAMPLE (pixcode);

      { Compute representation error for this pixel }
      Dec(cur.r, GETJSAMPLE(colormap0^[pixcode]));
      Dec(cur.g, GETJSAMPLE(colormap1^[pixcode]));
      Dec(cur.b, GETJSAMPLE(colormap2^[pixcode]));

      { Compute error fractions to be propagated to adjacent pixels.
        Add these into the running sums, and simultaneously shift the
        next-line error sums left by 1 column. }

      bnexterr := cur.r;	{ Process component 0 }
      delta := cur.r * 2;
      Inc(cur.r, delta);		{ form error * 3 }
      prev_errorptr^.r := jquant2_FSERROR (bpreverr.r + cur.r);
      Inc(cur.r, delta);		{ form error * 5 }
      bpreverr.r := belowerr.r + cur.r;
      belowerr.r := bnexterr;
      Inc(cur.r, delta);		{ form error * 7 }
      bnexterr := cur.g;	{ Process component 1 }
      delta := cur.g * 2;
      Inc(cur.g, delta);		{ form error * 3 }
      prev_errorptr^.g := jquant2_FSERROR (bpreverr.g + cur.g);
      Inc(cur.g, delta);		{ form error * 5 }
      bpreverr.g := belowerr.g + cur.g;
      belowerr.g := bnexterr;
      Inc(cur.g, delta);		{ form error * 7 }
      bnexterr := cur.b;	{ Process component 2 }
      delta := cur.b * 2;
      Inc(cur.b, delta);		{ form error * 3 }
      prev_errorptr^.b := jquant2_FSERROR (bpreverr.b + cur.b);
      Inc(cur.b, delta);		{ form error * 5 }
      bpreverr.b := belowerr.b + cur.b;
      belowerr.b := bnexterr;
      Inc(cur.b, delta);		{ form error * 7 }

      { At this point curN contains the 7/16 error value to be propagated
        to the next pixel on the current line, and all the errors for the
        next line have been shifted over.  We are therefore ready to move on.}

      Inc(inptr, dir);		{ Advance pixel pointers to next column }
      Inc(outptr, dir);
    end;
    { Post-loop cleanup: we must unload the final error values into the
      final fserrors[] entry.  Note we need not unload belowerrN because
      it is for the dummy column before or after the actual array. }

    errorptr^.r := jquant2_FSERROR (bpreverr.r); { unload prev errs into array }
    errorptr^.g := jquant2_FSERROR (bpreverr.g);
    errorptr^.b := jquant2_FSERROR (bpreverr.b);
  end;
end;


{ Initialize the error-limiting transfer function (lookup table).
  The raw F-S error computation can potentially compute error values of up to
  +- MAXJSAMPLE.  But we want the maximum correction applied to a pixel to be
  much less, otherwise obviously wrong pixels will be created.  (Typical
  effects include weird fringes at color-area boundaries, isolated bright
  pixels in a dark area, etc.)  The standard advice for avoiding this problem
  is to ensure that the "corners" of the color cube are allocated as output
  colors; then repeated errors in the same direction cannot cause cascading
  error buildup.  However, that only prevents the error from getting
  completely out of hand; Aaron Giles reports that error limiting improves
  the results even with corner colors allocated.
  A simple clamping of the error values to about +- MAXJSAMPLE/8 works pretty
  well, but the smoother transfer function used below is even better.  Thanks
  to Aaron Giles for this idea. }

{LOCAL}
procedure init_error_limit (cinfo : j_decompress_ptr);
const
  STEPSIZE = ((MAXJSAMPLE+1) div 16);
{ Allocate and fill in the error_limiter table }
var
  cquantize : jquant2_my_cquantize_ptr;
  table : error_limit_ptr;
  inp, out : int;
begin
  cquantize := jquant2_my_cquantize_ptr (cinfo^.cquantize);
  table := error_limit_ptr (cinfo^.mem^.alloc_small
    (j_common_ptr (cinfo), JPOOL_IMAGE, (MAXJSAMPLE*2+1) * SIZEOF(int)));
  { not needed: Inc(table, MAXJSAMPLE);
                so can index -MAXJSAMPLE .. +MAXJSAMPLE }
  cquantize^.error_limiter := table;
  { Map errors 1:1 up to +- MAXJSAMPLE/16 }
  out := 0;
  for inp := 0 to pred(STEPSIZE) do
  begin
    table^[inp] := out;
    table^[-inp] := -out;
    Inc(out);
  end;
  { Map errors 1:2 up to +- 3*MAXJSAMPLE/16 }
  inp := STEPSIZE;       { Nomssi: avoid problems with Delphi2 optimizer }
  while (inp < STEPSIZE*3) do
  begin
    table^[inp] := out;
    table^[-inp] := -out;
    Inc(inp);
    if Odd(inp) then
      Inc(out);
  end;
  { Clamp the rest to final out value (which is (MAXJSAMPLE+1)/8) }
  inp := STEPSIZE*3;     { Nomssi: avoid problems with Delphi 2 optimizer }
  while inp <= MAXJSAMPLE do
  begin
    table^[inp] := out;
    table^[-inp] := -out;
    Inc(inp);
  end;
end;

{ Finish up at the end of each pass. }

{METHODDEF}
procedure finish_pass1 (cinfo : j_decompress_ptr);
var
  cquantize : jquant2_my_cquantize_ptr;
begin
  cquantize := jquant2_my_cquantize_ptr (cinfo^.cquantize);

  { Select the representative colors and fill in cinfo^.colormap }
  cinfo^.colormap := cquantize^.sv_colormap;
  select_colors(cinfo, cquantize^.desired);
  { Force next pass to zero the color index table }
  cquantize^.needs_zeroed := TRUE;
end;


{METHODDEF}
procedure finish_pass2 (cinfo : j_decompress_ptr);
begin
  { no work }
end;


{ Initialize for each processing pass. }

{METHODDEF}
procedure start_pass_2_quant (cinfo : j_decompress_ptr;
                              is_pre_scan : boolean);
var
  cquantize : jquant2_my_cquantize_ptr;
  histogram : hist3d;
  i : int;
var
  arraysize : size_t;
begin
  cquantize := jquant2_my_cquantize_ptr (cinfo^.cquantize);
  histogram := cquantize^.histogram;
  { Only F-S dithering or no dithering is supported. }
  { If user asks for ordered dither, give him F-S. }
  if (cinfo^.dither_mode <> JDITHER_NONE) then
    cinfo^.dither_mode := JDITHER_FS;

  if (is_pre_scan) then
  begin
    { Set up method pointers }
    cquantize^.pub.color_quantize := prescan_quantize;
    cquantize^.pub.finish_pass := finish_pass1;
    cquantize^.needs_zeroed := TRUE; { Always zero histogram }
  end
  else
  begin
    { Set up method pointers }
    if (cinfo^.dither_mode = JDITHER_FS) then
      cquantize^.pub.color_quantize := pass2_fs_dither
    else
      cquantize^.pub.color_quantize := pass2_no_dither;
    cquantize^.pub.finish_pass := finish_pass2;

    { Make sure color count is acceptable }
    i := cinfo^.actual_number_of_colors;
    if (i < 1) then
      ERREXIT1(j_common_ptr(cinfo), JERR_QUANT_FEW_COLORS, 1);
    if (i > MAXNUMCOLORS) then
      ERREXIT1(j_common_ptr(cinfo), JERR_QUANT_MANY_COLORS, MAXNUMCOLORS);

    if (cinfo^.dither_mode = JDITHER_FS) then
    begin
      arraysize := size_t ((cinfo^.output_width + 2) *
				   (3 * SIZEOF(jquant2_FSERROR)));
      { Allocate Floyd-Steinberg workspace if we didn't already. }
      if (cquantize^.fserrors = NIL) then
	cquantize^.fserrors := jquant2_FS_ERROR_FIELD_PTR (cinfo^.mem^.alloc_large
	  (j_common_ptr(cinfo), JPOOL_IMAGE, arraysize));
      { Initialize the propagated errors to zero. }
      jzero_far(cquantize^.fserrors, arraysize);
      { Make the error-limit table if we didn't already. }
      if (cquantize^.error_limiter = NIL) then
	init_error_limit(cinfo);
      cquantize^.on_odd_row := FALSE;
    end;

  end;
  { Zero the histogram or inverse color map, if necessary }
  if (cquantize^.needs_zeroed) then
  begin
    for i := 0 to pred(HIST_C0_ELEMS) do
    begin
      jzero_far( histogram^[i],
		HIST_C1_ELEMS*HIST_C2_ELEMS * SIZEOF(histcell));
    end;
    cquantize^.needs_zeroed := FALSE;
  end;
end;


{ Switch to a new external colormap between output passes. }

{METHODDEF}
procedure new_color_map_2_quant (cinfo : j_decompress_ptr);
var
  cquantize : jquant2_my_cquantize_ptr;
begin
  cquantize := jquant2_my_cquantize_ptr (cinfo^.cquantize);

  { Reset the inverse color map }
  cquantize^.needs_zeroed := TRUE;
end;


{ Module initialization routine for 2-pass color quantization. }


{GLOBAL}
procedure jinit_2pass_quantizer (cinfo : j_decompress_ptr);
var
  cquantize : jquant2_my_cquantize_ptr;
  i : int;
var
  desired : int;
begin
  cquantize := jquant2_my_cquantize_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(jquant2_my_cquantizer)));
  cinfo^.cquantize := jpeg_color_quantizer_ptr(cquantize);
  cquantize^.pub.start_pass := start_pass_2_quant;
  cquantize^.pub.new_color_map := new_color_map_2_quant;
  cquantize^.fserrors := NIL;	{ flag optional arrays not allocated }
  cquantize^.error_limiter := NIL;

  { Make sure jdmaster didn't give me a case I can't handle }
  if (cinfo^.out_color_components <> 3) then
    ERREXIT(j_common_ptr(cinfo), JERR_NOTIMPL);

  { Allocate the histogram/inverse colormap storage }
  cquantize^.histogram := hist3d (cinfo^.mem^.alloc_small
    (j_common_ptr (cinfo), JPOOL_IMAGE, HIST_C0_ELEMS * SIZEOF(hist2d)));
  for i := 0 to pred(HIST_C0_ELEMS) do
  begin
    cquantize^.histogram^[i] := hist2d (cinfo^.mem^.alloc_large
      (j_common_ptr (cinfo), JPOOL_IMAGE,
       HIST_C1_ELEMS*HIST_C2_ELEMS * SIZEOF(histcell)));
  end;
  cquantize^.needs_zeroed := TRUE; { histogram is garbage now }

  { Allocate storage for the completed colormap, if required.
    We do this now since it is FAR storage and may affect
    the memory manager's space calculations. }

  if (cinfo^.enable_2pass_quant) then
  begin
    { Make sure color count is acceptable }
    desired := cinfo^.desired_number_of_colors;
    { Lower bound on # of colors ... somewhat arbitrary as long as > 0 }
    if (desired < 8) then
      ERREXIT1(j_common_ptr (cinfo), JERR_QUANT_FEW_COLORS, 8);
    { Make sure colormap indexes can be represented by JSAMPLEs }
    if (desired > MAXNUMCOLORS) then
      ERREXIT1(j_common_ptr (cinfo), JERR_QUANT_MANY_COLORS, MAXNUMCOLORS);
    cquantize^.sv_colormap := cinfo^.mem^.alloc_sarray
      (j_common_ptr (cinfo),JPOOL_IMAGE, JDIMENSION(desired), JDIMENSION(3));
    cquantize^.desired := desired;
  end
  else
    cquantize^.sv_colormap := NIL;

  { Only F-S dithering or no dithering is supported. }
  { If user asks for ordered dither, give him F-S. }
  if (cinfo^.dither_mode <> JDITHER_NONE) then
    cinfo^.dither_mode := JDITHER_FS;

  { Allocate Floyd-Steinberg workspace if necessary.
    This isn't really needed until pass 2, but again it is FAR storage.
    Although we will cope with a later change in dither_mode,
    we do not promise to honor max_memory_to_use if dither_mode changes. }

  if (cinfo^.dither_mode = JDITHER_FS) then
  begin
    cquantize^.fserrors := jquant2_FS_ERROR_FIELD_PTR (cinfo^.mem^.alloc_large
      (j_common_ptr(cinfo), JPOOL_IMAGE,
       size_t ((cinfo^.output_width + 2) * (3 * SIZEOF(jquant2_FSERROR))) ) );
    { Might as well create the error-limiting table too. }
    init_error_limit(cinfo);
  end;
end;
{ QUANT_2PASS_SUPPORTED }

{== imjdmainct implementation ==}

{ Private buffer controller object }

type
  jdmainct_my_main_ptr = ^jdmainct_my_main_controller;
  jdmainct_my_main_controller = record
    pub : jpeg_d_main_controller; { public fields }

    { Pointer to allocated workspace (M or M+2 row groups). }
    buffer : array[0..MAX_COMPONENTS-1] of JSAMPARRAY;

    buffer_full : boolean;	{ Have we gotten an iMCU row from decoder? }
    rowgroup_ctr : JDIMENSION ;	{ counts row groups output to postprocessor }

    { Remaining fields are only used in the context case. }

    { These are the master pointers to the funny-order pointer lists. }
    xbuffer : array[0..2-1] of JSAMPIMAGE;	{ pointers to weird pointer lists }

    whichptr : int;			{ indicates which pointer set is now in use }
    context_state : int;		{ process_data state machine status }
    rowgroups_avail : JDIMENSION;	{ row groups available to postprocessor }
    iMCU_row_ctr : JDIMENSION;	{ counts iMCU rows to detect image top/bot }
  end; { my_main_controller; }


{ context_state values: }
const
  CTX_PREPARE_FOR_IMCU  = 0;	{ need to prepare for MCU row }
  CTX_PROCESS_IMCU      = 1;	{ feeding iMCU to postprocessor }
  CTX_POSTPONED_ROW     = 2;	{ feeding postponed row group }


{ Forward declarations }
{METHODDEF}
procedure jdmainct_process_data_simple_main(cinfo : j_decompress_ptr;
                                   output_buf : JSAMPARRAY;
	                           var out_row_ctr : JDIMENSION;
                                   out_rows_avail : JDIMENSION); forward;
{METHODDEF}
procedure process_data_context_main (cinfo : j_decompress_ptr;
                                     output_buf : JSAMPARRAY;
	                             var out_row_ctr : JDIMENSION;
                                     out_rows_avail : JDIMENSION); forward;

{$ifdef QUANT_2PASS_SUPPORTED}
{METHODDEF}
procedure process_data_crank_post (cinfo : j_decompress_ptr;
                                     output_buf : JSAMPARRAY;
	                             var out_row_ctr : JDIMENSION;
                                     out_rows_avail : JDIMENSION); forward;
{$endif}


{LOCAL}
procedure alloc_funny_pointers (cinfo : j_decompress_ptr);
{ Allocate space for the funny pointer lists.
  This is done only once, not once per pass. }
var
  main : jdmainct_my_main_ptr;
  ci, rgroup : int;
  M : int;
  compptr : jpeg_component_info_ptr;
  xbuf : JSAMPARRAY;
begin
  main := jdmainct_my_main_ptr (cinfo^.main);
  M := cinfo^.min_DCT_scaled_size;

  { Get top-level space for component array pointers.
    We alloc both arrays with one call to save a few cycles. }

  main^.xbuffer[0] := JSAMPIMAGE (
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                      cinfo^.num_components * 2 * SIZEOF(JSAMPARRAY)) );
  main^.xbuffer[1] := JSAMPIMAGE(@( main^.xbuffer[0]^[cinfo^.num_components] ));

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    rgroup := (compptr^.v_samp_factor * compptr^.DCT_scaled_size) div
      cinfo^.min_DCT_scaled_size; { height of a row group of component }
    { Get space for pointer lists --- M+4 row groups in each list.
      We alloc both pointer lists with one call to save a few cycles. }

    xbuf := JSAMPARRAY (
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				 2 * (rgroup * (M + 4)) * SIZEOF(JSAMPROW)) );
    Inc(JSAMPROW_PTR(xbuf), rgroup); { want one row group at negative offsets }
    main^.xbuffer[0]^[ci] := xbuf;
    Inc(JSAMPROW_PTR(xbuf), rgroup * (M + 4));
    main^.xbuffer[1]^[ci] := xbuf;
    Inc(compptr);
  end;
end;

{LOCAL}
procedure make_funny_pointers (cinfo : j_decompress_ptr);
{ Create the funny pointer lists discussed in the comments above.
  The actual workspace is already allocated (in main^.buffer),
  and the space for the pointer lists is allocated too.
  This routine just fills in the curiously ordered lists.
  This will be repeated at the beginning of each pass. }
var
  main : jdmainct_my_main_ptr;
  ci, i, rgroup : int;
  M : int;
  compptr : jpeg_component_info_ptr;
  buf, xbuf0, xbuf1 : JSAMPARRAY;
var
  help_xbuf0 : JSAMPARRAY;       { work around negative offsets }
begin
  main := jdmainct_my_main_ptr (cinfo^.main);
  M := cinfo^.min_DCT_scaled_size;

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    rgroup := (compptr^.v_samp_factor * compptr^.DCT_scaled_size) div
      cinfo^.min_DCT_scaled_size; { height of a row group of component }
    xbuf0 := main^.xbuffer[0]^[ci];
    xbuf1 := main^.xbuffer[1]^[ci];
    { First copy the workspace pointers as-is }
    buf := main^.buffer[ci];
    for i := 0 to pred(rgroup * (M + 2)) do
    begin
      xbuf0^[i] := buf^[i];
      xbuf1^[i] := buf^[i];
    end;
    { In the second list, put the last four row groups in swapped order }
    for i := 0 to pred(rgroup * 2) do
    begin
      xbuf1^[rgroup*(M-2) + i] := buf^[rgroup*M + i];
      xbuf1^[rgroup*M + i] := buf^[rgroup*(M-2) + i];
    end;
    { The wraparound pointers at top and bottom will be filled later
      (see set_wraparound_pointers, below).  Initially we want the "above"
      pointers to duplicate the first actual data line.  This only needs
      to happen in xbuffer[0]. }

    help_xbuf0 := xbuf0;
    Dec(JSAMPROW_PTR(help_xbuf0), rgroup);

    for i := 0 to pred(rgroup) do
    begin
      {xbuf0^[i - rgroup] := xbuf0^[0];}
      help_xbuf0^[i] := xbuf0^[0];
    end;
    Inc(compptr);
  end;
end;


{LOCAL}
procedure set_wraparound_pointers (cinfo : j_decompress_ptr);
{ Set up the "wraparound" pointers at top and bottom of the pointer lists.
  This changes the pointer list state from top-of-image to the normal state. }
var
  main : jdmainct_my_main_ptr;
  ci, i, rgroup : int;
  M : int;
  compptr : jpeg_component_info_ptr;
  xbuf0, xbuf1 : JSAMPARRAY;
var
  help_xbuf0,
  help_xbuf1 : JSAMPARRAY;       { work around negative offsets }
begin
  main := jdmainct_my_main_ptr (cinfo^.main);
  M := cinfo^.min_DCT_scaled_size;

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    rgroup := (compptr^.v_samp_factor * compptr^.DCT_scaled_size) div
      cinfo^.min_DCT_scaled_size; { height of a row group of component }
    xbuf0 := main^.xbuffer[0]^[ci];
    xbuf1 := main^.xbuffer[1]^[ci];

    help_xbuf0 := xbuf0;
    Dec(JSAMPROW_PTR(help_xbuf0), rgroup);
    help_xbuf1 := xbuf1;
    Dec(JSAMPROW_PTR(help_xbuf1), rgroup);

    for i := 0 to pred(rgroup) do
    begin
      {xbuf0^[i - rgroup] := xbuf0^[rgroup*(M+1) + i];
      xbuf1^[i - rgroup] := xbuf1^[rgroup*(M+1) + i];}

      help_xbuf0^[i] := xbuf0^[rgroup*(M+1) + i];
      help_xbuf1^[i] := xbuf1^[rgroup*(M+1) + i];

      xbuf0^[rgroup*(M+2) + i] := xbuf0^[i];
      xbuf1^[rgroup*(M+2) + i] := xbuf1^[i];
    end;
    Inc(compptr);
  end;
end;


{LOCAL}
procedure set_bottom_pointers (cinfo : j_decompress_ptr);
{ Change the pointer lists to duplicate the last sample row at the bottom
  of the image.  whichptr indicates which xbuffer holds the final iMCU row.
  Also sets rowgroups_avail to indicate number of nondummy row groups in row. }
var
  main : jdmainct_my_main_ptr;
  ci, i, rgroup, iMCUheight, rows_left : int;
  compptr : jpeg_component_info_ptr;
  xbuf : JSAMPARRAY;
begin
  main := jdmainct_my_main_ptr (cinfo^.main);

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Count sample rows in one iMCU row and in one row group }
    iMCUheight := compptr^.v_samp_factor * compptr^.DCT_scaled_size;
    rgroup := iMCUheight div cinfo^.min_DCT_scaled_size;
    { Count nondummy sample rows remaining for this component }
    rows_left := int (compptr^.downsampled_height mod JDIMENSION (iMCUheight));
    if (rows_left = 0) then
      rows_left := iMCUheight;
    { Count nondummy row groups.  Should get same answer for each component,
      so we need only do it once. }
    if (ci = 0) then
    begin
      main^.rowgroups_avail := JDIMENSION ((rows_left-1) div rgroup + 1);
    end;
    { Duplicate the last real sample row rgroup*2 times; this pads out the
      last partial rowgroup and ensures at least one full rowgroup of context. }

    xbuf := main^.xbuffer[main^.whichptr]^[ci];
    for i := 0 to pred(rgroup * 2) do
    begin
      xbuf^[rows_left + i] := xbuf^[rows_left-1];
    end;
    Inc(compptr);
  end;
end;


{ Initialize for a processing pass. }

{METHODDEF}
procedure jdmainct_start_pass_main (cinfo : j_decompress_ptr;
                           pass_mode : J_BUF_MODE);
var
  main : jdmainct_my_main_ptr;
begin
  main := jdmainct_my_main_ptr (cinfo^.main);

  case (pass_mode) of
  JBUF_PASS_THRU:
    begin
      if (cinfo^.upsample^.need_context_rows) then
      begin
        main^.pub.process_data := process_data_context_main;
        make_funny_pointers(cinfo); { Create the xbuffer[] lists }
        main^.whichptr := 0;	{ Read first iMCU row into xbuffer[0] }
        main^.context_state := CTX_PREPARE_FOR_IMCU;
        main^.iMCU_row_ctr := 0;
      end
      else
      begin
        { Simple case with no context needed }
        main^.pub.process_data := jdmainct_process_data_simple_main;
      end;
      main^.buffer_full := FALSE;	{ Mark buffer empty }
      main^.rowgroup_ctr := 0;
    end;
{$ifdef QUANT_2PASS_SUPPORTED}
  JBUF_CRANK_DEST:
    { For last pass of 2-pass quantization, just crank the postprocessor }
    main^.pub.process_data := process_data_crank_post;
{$endif}
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
  end;
end;


{ Process some data.
  This handles the simple case where no context is required. }

{METHODDEF}
procedure jdmainct_process_data_simple_main (cinfo : j_decompress_ptr;
			            output_buf : JSAMPARRAY;
                                    var out_row_ctr : JDIMENSION;
			            out_rows_avail : JDIMENSION);
var
  main : jdmainct_my_main_ptr;
  rowgroups_avail : JDIMENSION;
var
  main_buffer_ptr : JSAMPIMAGE;
begin
  main := jdmainct_my_main_ptr (cinfo^.main);
  main_buffer_ptr := JSAMPIMAGE(@(main^.buffer));

  { Read input data if we haven't filled the main buffer yet }
  if (not main^.buffer_full) then
  begin
    if (cinfo^.coef^.decompress_data (cinfo, main_buffer_ptr)=0) then
      exit;			{ suspension forced, can do nothing more }
    main^.buffer_full := TRUE;	{ OK, we have an iMCU row to work with }
  end;

  { There are always min_DCT_scaled_size row groups in an iMCU row. }
  rowgroups_avail := JDIMENSION (cinfo^.min_DCT_scaled_size);
  { Note: at the bottom of the image, we may pass extra garbage row groups
    to the postprocessor.  The postprocessor has to check for bottom
    of image anyway (at row resolution), so no point in us doing it too. }

  { Feed the postprocessor }
  cinfo^.post^.post_process_data (cinfo, main_buffer_ptr,
                                  main^.rowgroup_ctr, rowgroups_avail,
				  output_buf, out_row_ctr, out_rows_avail);

  { Has postprocessor consumed all the data yet? If so, mark buffer empty }
  if (main^.rowgroup_ctr >= rowgroups_avail) then
  begin
    main^.buffer_full := FALSE;
    main^.rowgroup_ctr := 0;
  end;
end;


{ Process some data.
  This handles the case where context rows must be provided. }

{METHODDEF}
procedure process_data_context_main (cinfo : j_decompress_ptr;
			             output_buf : JSAMPARRAY;
                                     var out_row_ctr : JDIMENSION;
			             out_rows_avail : JDIMENSION);
var
  main : jdmainct_my_main_ptr;
begin
  main := jdmainct_my_main_ptr (cinfo^.main);

  { Read input data if we haven't filled the main buffer yet }
  if (not main^.buffer_full) then
  begin
    if (cinfo^.coef^.decompress_data (cinfo,
			  main^.xbuffer[main^.whichptr])=0) then
      exit;			{ suspension forced, can do nothing more }
    main^.buffer_full := TRUE;	{ OK, we have an iMCU row to work with }
    Inc(main^.iMCU_row_ctr);	{ count rows received }
  end;

  { Postprocessor typically will not swallow all the input data it is handed
    in one call (due to filling the output buffer first).  Must be prepared
    to exit and restart.  This switch lets us keep track of how far we got.
    Note that each case falls through to the next on successful completion. }

  case (main^.context_state) of
  CTX_POSTPONED_ROW:
    begin
      { Call postprocessor using previously set pointers for postponed row }
      cinfo^.post^.post_process_data (cinfo, main^.xbuffer[main^.whichptr],
			  main^.rowgroup_ctr, main^.rowgroups_avail,
			  output_buf, out_row_ctr, out_rows_avail);
      if (main^.rowgroup_ctr < main^.rowgroups_avail) then
        exit;			{ Need to suspend }
      main^.context_state := CTX_PREPARE_FOR_IMCU;
      if (out_row_ctr >= out_rows_avail) then
        exit;			{ Postprocessor exactly filled output buf }
    end;
  end;
  case (main^.context_state) of
  CTX_POSTPONED_ROW,
  CTX_PREPARE_FOR_IMCU:  {FALLTHROUGH}
    begin
      { Prepare to process first M-1 row groups of this iMCU row }
      main^.rowgroup_ctr := 0;
      main^.rowgroups_avail := JDIMENSION (cinfo^.min_DCT_scaled_size - 1);
      { Check for bottom of image: if so, tweak pointers to "duplicate"
        the last sample row, and adjust rowgroups_avail to ignore padding rows. }

      if (main^.iMCU_row_ctr = cinfo^.total_iMCU_rows) then
        set_bottom_pointers(cinfo);
      main^.context_state := CTX_PROCESS_IMCU;

    end;
  end;
  case (main^.context_state) of
  CTX_POSTPONED_ROW,
  CTX_PREPARE_FOR_IMCU,  {FALLTHROUGH}
  CTX_PROCESS_IMCU:
    begin
      { Call postprocessor using previously set pointers }
      cinfo^.post^.post_process_data (cinfo, main^.xbuffer[main^.whichptr],
			  main^.rowgroup_ctr, main^.rowgroups_avail,
			  output_buf, out_row_ctr, out_rows_avail);
      if (main^.rowgroup_ctr < main^.rowgroups_avail) then
        exit;			{ Need to suspend }
      { After the first iMCU, change wraparound pointers to normal state }
      if (main^.iMCU_row_ctr = 1) then
        set_wraparound_pointers(cinfo);
      { Prepare to load new iMCU row using other xbuffer list }
      main^.whichptr := main^.whichptr xor 1;	{ 0=>1 or 1=>0 }
      main^.buffer_full := FALSE;
      { Still need to process last row group of this iMCU row, }
      { which is saved at index M+1 of the other xbuffer }
      main^.rowgroup_ctr := JDIMENSION (cinfo^.min_DCT_scaled_size + 1);
      main^.rowgroups_avail := JDIMENSION (cinfo^.min_DCT_scaled_size + 2);
      main^.context_state := CTX_POSTPONED_ROW;
    end;
  end;
end;


{ Process some data.
  Final pass of two-pass quantization: just call the postprocessor.
  Source data will be the postprocessor controller's internal buffer. }

{$ifdef QUANT_2PASS_SUPPORTED}

{METHODDEF}
procedure process_data_crank_post (cinfo : j_decompress_ptr;
			           output_buf : JSAMPARRAY;
                                   var out_row_ctr : JDIMENSION;
			           out_rows_avail : JDIMENSION);
var
  in_row_group_ctr : JDIMENSION;
begin
  in_row_group_ctr := 0;
  cinfo^.post^.post_process_data (cinfo, JSAMPIMAGE (NIL),
				     in_row_group_ctr,
                                     JDIMENSION(0),
				     output_buf,
                                     out_row_ctr,
                                     out_rows_avail);
end;

{$endif} { QUANT_2PASS_SUPPORTED }


{ Initialize main buffer controller. }

{GLOBAL}
procedure jinit_d_main_controller (cinfo : j_decompress_ptr;
                                   need_full_buffer : boolean);
var
  main : jdmainct_my_main_ptr;
  ci, rgroup, ngroups : int;
  compptr : jpeg_component_info_ptr;
begin
  main := jdmainct_my_main_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(jdmainct_my_main_controller)) );
  cinfo^.main := jpeg_d_main_controller_ptr(main);
  main^.pub.start_pass := jdmainct_start_pass_main;

  if (need_full_buffer)	then	{ shouldn't happen }
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);

  { Allocate the workspace.
    ngroups is the number of row groups we need.}

  if (cinfo^.upsample^.need_context_rows) then
  begin
    if (cinfo^.min_DCT_scaled_size < 2) then { unsupported, see comments above }
      ERREXIT(j_common_ptr(cinfo), JERR_NOTIMPL);
    alloc_funny_pointers(cinfo); { Alloc space for xbuffer[] lists }
    ngroups := cinfo^.min_DCT_scaled_size + 2;
  end
  else
  begin
    ngroups := cinfo^.min_DCT_scaled_size;
  end;

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    rgroup := (compptr^.v_samp_factor * compptr^.DCT_scaled_size) div
      cinfo^.min_DCT_scaled_size; { height of a row group of component }
    main^.buffer[ci] := cinfo^.mem^.alloc_sarray
			(j_common_ptr(cinfo), JPOOL_IMAGE,
			 compptr^.width_in_blocks * uInt(compptr^.DCT_scaled_size),
			 JDIMENSION (rgroup * ngroups));
    Inc(compptr);
  end;
end;

{== imjdmaster implementation ==}

{ Private state }

type
  jdmaster_my_master_ptr = ^my_decomp_master;
  my_decomp_master = record
    pub : jpeg_decomp_master; { public fields }

    pass_number : int;		{ # of passes completed }

    using_merged_upsample : boolean; { TRUE if using merged upsample/cconvert }

    { Saved references to initialized quantizer modules,
      in case we need to switch modes. }

    quantizer_1pass : jpeg_color_quantizer_ptr;
    quantizer_2pass : jpeg_color_quantizer_ptr;
  end;

{ Determine whether merged upsample/color conversion should be used.
  CRUCIAL: this must match the actual capabilities of jdmerge.c! }

{LOCAL}
function use_merged_upsample (cinfo : j_decompress_ptr) : boolean;
var
  compptr : jpeg_component_info_list_ptr;
begin
  compptr := cinfo^.comp_info;

{$ifdef UPSAMPLE_MERGING_SUPPORTED}
  { Merging is the equivalent of plain box-filter upsampling }
  if (cinfo^.do_fancy_upsampling) or (cinfo^.CCIR601_sampling) then
  begin
    use_merged_upsample := FALSE;
    exit;
  end;
  { jdmerge.c only supports YCC=>RGB color conversion }
  if (cinfo^.jpeg_color_space <> JCS_YCbCr) or (cinfo^.num_components <> 3)
  or (cinfo^.out_color_space <> JCS_RGB)
  or (cinfo^.out_color_components <> RGB_PIXELSIZE) then
  begin
    use_merged_upsample := FALSE;
    exit;
  end;

  { and it only handles 2h1v or 2h2v sampling ratios }
  if (compptr^[0].h_samp_factor <> 2) or
     (compptr^[1].h_samp_factor <> 1) or
     (compptr^[2].h_samp_factor <> 1) or
     (compptr^[0].v_samp_factor >  2) or
     (compptr^[1].v_samp_factor <> 1) or
     (compptr^[2].v_samp_factor <> 1) then
  begin
    use_merged_upsample := FALSE;
    exit;
  end;
  { furthermore, it doesn't work if we've scaled the IDCTs differently }
  if (compptr^[0].DCT_scaled_size <> cinfo^.min_DCT_scaled_size) or
     (compptr^[1].DCT_scaled_size <> cinfo^.min_DCT_scaled_size) or
     (compptr^[2].DCT_scaled_size <> cinfo^.min_DCT_scaled_size) then
  begin
    use_merged_upsample := FALSE;
    exit;
  end;
  { ??? also need to test for upsample-time rescaling, when & if supported }
  use_merged_upsample := TRUE;			{ by golly, it'll work... }
{$else}
  use_merged_upsample := FALSE;
{$endif}
end;


{ Compute output image dimensions and related values.
  NOTE: this is exported for possible use by application.
  Hence it mustn't do anything that can't be done twice.
  Also note that it may be called before the master module is initialized! }

{GLOBAL}
procedure jpeg_calc_output_dimensions (cinfo : j_decompress_ptr);
{ Do computations that are needed before master selection phase }
{$ifdef IDCT_SCALING_SUPPORTED}
var
  ci : int;
  compptr : jpeg_component_info_ptr;
{$endif}
var
  ssize : int;
begin
  { Prevent application from calling me at wrong times }
  if (cinfo^.global_state <> DSTATE_READY) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

{$ifdef IDCT_SCALING_SUPPORTED}

  { Compute actual output image dimensions and DCT scaling choices. }
  if (cinfo^.scale_num * 8 <= cinfo^.scale_denom) then
  begin
    { Provide 1/8 scaling }
    cinfo^.output_width := JDIMENSION (
      jdiv_round_up( long(cinfo^.image_width), long(8)) );
    cinfo^.output_height := JDIMENSION (
      jdiv_round_up( long(cinfo^.image_height), long(8)) );
    cinfo^.min_DCT_scaled_size := 1;
  end
  else
    if (cinfo^.scale_num * 4 <= cinfo^.scale_denom) then
    begin
      { Provide 1/4 scaling }
      cinfo^.output_width := JDIMENSION (
        jdiv_round_up( long (cinfo^.image_width), long(4)) );
      cinfo^.output_height := JDIMENSION (
        jdiv_round_up( long (cinfo^.image_height), long(4)) );
      cinfo^.min_DCT_scaled_size := 2;
    end
    else
      if (cinfo^.scale_num * 2 <= cinfo^.scale_denom) then
      begin
        { Provide 1/2 scaling }
        cinfo^.output_width := JDIMENSION (
          jdiv_round_up( long(cinfo^.image_width), long(2)) );
        cinfo^.output_height := JDIMENSION (
          jdiv_round_up( long(cinfo^.image_height), long(2)) );
        cinfo^.min_DCT_scaled_size := 4;
      end
      else
      begin
        { Provide 1/1 scaling }
        cinfo^.output_width := cinfo^.image_width;
        cinfo^.output_height := cinfo^.image_height;
        cinfo^.min_DCT_scaled_size := DCTSIZE;
      end;
  { In selecting the actual DCT scaling for each component, we try to
    scale up the chroma components via IDCT scaling rather than upsampling.
    This saves time if the upsampler gets to use 1:1 scaling.
    Note this code assumes that the supported DCT scalings are powers of 2. }

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    ssize := cinfo^.min_DCT_scaled_size;
    while (ssize < DCTSIZE) and
	  ((compptr^.h_samp_factor * ssize * 2 <=
	    cinfo^.max_h_samp_factor * cinfo^.min_DCT_scaled_size) and
	   (compptr^.v_samp_factor * ssize * 2 <=
	    cinfo^.max_v_samp_factor * cinfo^.min_DCT_scaled_size)) do
    begin
      ssize := ssize * 2;
    end;
    compptr^.DCT_scaled_size := ssize;
    Inc(compptr);
  end;

  { Recompute downsampled dimensions of components;
    application needs to know these if using raw downsampled data. }

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Size in samples, after IDCT scaling }
    compptr^.downsampled_width := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_width) *
		    long (compptr^.h_samp_factor * compptr^.DCT_scaled_size),
		    long (cinfo^.max_h_samp_factor * DCTSIZE)) );
    compptr^.downsampled_height := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_height) *
		    long (compptr^.v_samp_factor * compptr^.DCT_scaled_size),
		    long (cinfo^.max_v_samp_factor * DCTSIZE)) );
    Inc(compptr);
  end;

{$else} { !IDCT_SCALING_SUPPORTED }

  { Hardwire it to "no scaling" }
  cinfo^.output_width := cinfo^.image_width;
  cinfo^.output_height := cinfo^.image_height;
  { jdinput.c has already initialized DCT_scaled_size to DCTSIZE,
    and has computed unscaled downsampled_width and downsampled_height. }

{$endif} { IDCT_SCALING_SUPPORTED }

  { Report number of components in selected colorspace. }
  { Probably this should be in the color conversion module... }
  case (cinfo^.out_color_space) of
  JCS_GRAYSCALE:
    cinfo^.out_color_components := 1;
{$ifndef RGB_PIXELSIZE_IS_3}
  JCS_RGB:
    cinfo^.out_color_components := RGB_PIXELSIZE;
{$else}
  JCS_RGB,
{$endif} { else share code with YCbCr }
  JCS_YCbCr:
    cinfo^.out_color_components := 3;
  JCS_CMYK,
  JCS_YCCK:
    cinfo^.out_color_components := 4;
  else			{ else must be same colorspace as in file }
    cinfo^.out_color_components := cinfo^.num_components;
  end;
  if (cinfo^.quantize_colors) then
    cinfo^.output_components := 1
  else
    cinfo^.output_components := cinfo^.out_color_components;

  { See if upsampler will want to emit more than one row at a time }
  if (use_merged_upsample(cinfo)) then
    cinfo^.rec_outbuf_height := cinfo^.max_v_samp_factor
  else
    cinfo^.rec_outbuf_height := 1;
end;


{ Several decompression processes need to range-limit values to the range
  0..MAXJSAMPLE; the input value may fall somewhat outside this range
  due to noise introduced by quantization, roundoff error, etc.  These
  processes are inner loops and need to be as fast as possible.  On most
  machines, particularly CPUs with pipelines or instruction prefetch,
  a (subscript-check-less) C table lookup
 		x := sample_range_limit[x];
  is faster than explicit tests
 		if (x < 0)  x := 0;
 		else if (x > MAXJSAMPLE)  x := MAXJSAMPLE;
  These processes all use a common table prepared by the routine below.

  For most steps we can mathematically guarantee that the initial value
  of x is within MAXJSAMPLE+1 of the legal range, so a table running from
  -(MAXJSAMPLE+1) to 2*MAXJSAMPLE+1 is sufficient.  But for the initial
  limiting step (just after the IDCT), a wildly out-of-range value is
  possible if the input data is corrupt.  To avoid any chance of indexing
  off the end of memory and getting a bad-pointer trap, we perform the
  post-IDCT limiting thus:
 		x := range_limit[x & MASK];
  where MASK is 2 bits wider than legal sample data, ie 10 bits for 8-bit
  samples.  Under normal circumstances this is more than enough range and
  a correct output will be generated; with bogus input data the mask will
  cause wraparound, and we will safely generate a bogus-but-in-range output.
  For the post-IDCT step, we want to convert the data from signed to unsigned
  representation by adding CENTERJSAMPLE at the same time that we limit it.
  So the post-IDCT limiting table ends up looking like this:
    CENTERJSAMPLE,CENTERJSAMPLE+1,...,MAXJSAMPLE,
    MAXJSAMPLE (repeat 2*(MAXJSAMPLE+1)-CENTERJSAMPLE times),
    0          (repeat 2*(MAXJSAMPLE+1)-CENTERJSAMPLE times),
    0,1,...,CENTERJSAMPLE-1
  Negative inputs select values from the upper half of the table after
  masking.

  We can save some space by overlapping the start of the post-IDCT table
  with the simpler range limiting table.  The post-IDCT table begins at
  sample_range_limit + CENTERJSAMPLE.

  Note that the table is allocated in near data space on PCs; it's small
  enough and used often enough to justify this. }

{LOCAL}
procedure prepare_range_limit_table (cinfo : j_decompress_ptr);
{ Allocate and fill in the sample_range_limit table }
var
  table : range_limit_table_ptr;
  idct_table : JSAMPROW;
  i : int;
begin
  table := range_limit_table_ptr (
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
		(5 * (MAXJSAMPLE+1) + CENTERJSAMPLE) * SIZEOF(JSAMPLE)) );

  { First segment of "simple" table: limit[x] := 0 for x < 0 }
  MEMZERO(table, (MAXJSAMPLE+1) * SIZEOF(JSAMPLE));

  cinfo^.sample_range_limit := (table);
  { allow negative subscripts of simple table }
  { is noop, handled via type definition (Nomssi) }
  { Main part of "simple" table: limit[x] := x }
  for i := 0 to MAXJSAMPLE do
    table^[i] := JSAMPLE (i);
  idct_table := JSAMPROW(@ table^[CENTERJSAMPLE]);
                        { Point to where post-IDCT table starts }
  { End of simple table, rest of first half of post-IDCT table }
  for i := CENTERJSAMPLE to pred(2*(MAXJSAMPLE+1)) do
    idct_table^[i] := MAXJSAMPLE;
  { Second half of post-IDCT table }
  MEMZERO(@(idct_table^[2 * (MAXJSAMPLE+1)]),
	  (2 * (MAXJSAMPLE+1) - CENTERJSAMPLE) * SIZEOF(JSAMPLE));
  MEMCOPY(@(idct_table^[(4 * (MAXJSAMPLE+1) - CENTERJSAMPLE)]),
	  @cinfo^.sample_range_limit^[0], CENTERJSAMPLE * SIZEOF(JSAMPLE));

end;


{ Master selection of decompression modules.
  This is done once at jpeg_start_decompress time.  We determine
  which modules will be used and give them appropriate initialization calls.
  We also initialize the decompressor input side to begin consuming data.

  Since jpeg_read_header has finished, we know what is in the SOF
  and (first) SOS markers.  We also have all the application parameter
  settings. }

{LOCAL}
procedure master_selection (cinfo : j_decompress_ptr);
var
  master : jdmaster_my_master_ptr;
  use_c_buffer : boolean;
  samplesperrow : long;
  jd_samplesperrow : JDIMENSION;
var
  nscans : int;
begin
  master := jdmaster_my_master_ptr (cinfo^.master);

  { Initialize dimensions and other stuff }
  jpeg_calc_output_dimensions(cinfo);
  prepare_range_limit_table(cinfo);

  { Width of an output scanline must be representable as JDIMENSION. }
  samplesperrow := long(cinfo^.output_width) * long (cinfo^.out_color_components);
  jd_samplesperrow := JDIMENSION (samplesperrow);
  if (long(jd_samplesperrow) <> samplesperrow) then
    ERREXIT(j_common_ptr(cinfo), JERR_WIDTH_OVERFLOW);

  { Initialize my private state }
  master^.pass_number := 0;
  master^.using_merged_upsample := use_merged_upsample(cinfo);

  { Color quantizer selection }
  master^.quantizer_1pass := NIL;
  master^.quantizer_2pass := NIL;
  { No mode changes if not using buffered-image mode. }
  if (not cinfo^.quantize_colors) or (not cinfo^.buffered_image) then
  begin
    cinfo^.enable_1pass_quant := FALSE;
    cinfo^.enable_external_quant := FALSE;
    cinfo^.enable_2pass_quant := FALSE;
  end;
  if (cinfo^.quantize_colors) then
  begin
    if (cinfo^.raw_data_out) then
      ERREXIT(j_common_ptr(cinfo), JERR_NOTIMPL);
    { 2-pass quantizer only works in 3-component color space. }
    if (cinfo^.out_color_components <> 3) then
    begin
      cinfo^.enable_1pass_quant := TRUE;
      cinfo^.enable_external_quant := FALSE;
      cinfo^.enable_2pass_quant := FALSE;
      cinfo^.colormap := NIL;
    end
    else
      if (cinfo^.colormap <> NIL) then
      begin
        cinfo^.enable_external_quant := TRUE;
      end
      else
        if (cinfo^.two_pass_quantize) then
        begin
          cinfo^.enable_2pass_quant := TRUE;
        end
        else
        begin
          cinfo^.enable_1pass_quant := TRUE;
        end;

    if (cinfo^.enable_1pass_quant) then
    begin
{$ifdef QUANT_1PASS_SUPPORTED}
      jinit_1pass_quantizer(cinfo);
      master^.quantizer_1pass := cinfo^.cquantize;
{$else}
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
    end;

    { We use the 2-pass code to map to external colormaps. }
    if (cinfo^.enable_2pass_quant) or (cinfo^.enable_external_quant) then
    begin
{$ifdef QUANT_2PASS_SUPPORTED}
      jinit_2pass_quantizer(cinfo);
      master^.quantizer_2pass := cinfo^.cquantize;
{$else}
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
    end;
    { If both quantizers are initialized, the 2-pass one is left active;
      this is necessary for starting with quantization to an external map. }
  end;

  { Post-processing: in particular, color conversion first }
  if (not cinfo^.raw_data_out) then
  begin
    if (master^.using_merged_upsample) then
    begin
{$ifdef UPSAMPLE_MERGING_SUPPORTED}
      jinit_merged_upsampler(cinfo); { does color conversion too }
{$else}
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
    end
    else
    begin
      jinit_color_deconverter(cinfo);
      jinit_upsampler(cinfo);
    end;
    jinit_d_post_controller(cinfo, cinfo^.enable_2pass_quant);
  end;
  { Inverse DCT }
  jinit_inverse_dct(cinfo);
  { Entropy decoding: either Huffman or arithmetic coding. }
  if (cinfo^.arith_code) then
  begin
    ERREXIT(j_common_ptr(cinfo), JERR_ARITH_NOTIMPL);
  end
  else
  begin
    if (cinfo^.progressive_mode) then
    begin
{$ifdef D_PROGRESSIVE_SUPPORTED}
      jinit_phuff_decoder(cinfo);
{$else}
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
    end
    else
      jinit_huff_decoder(cinfo);
  end;

  { Initialize principal buffer controllers. }
  use_c_buffer := cinfo^.inputctl^.has_multiple_scans or cinfo^.buffered_image;
  jinit_d_coef_controller(cinfo, use_c_buffer);

  if (not cinfo^.raw_data_out) then
    jinit_d_main_controller(cinfo, FALSE { never need full buffer here });

  { We can now tell the memory manager to allocate virtual arrays. }
  cinfo^.mem^.realize_virt_arrays (j_common_ptr(cinfo));

  { Initialize input side of decompressor to consume first scan. }
  cinfo^.inputctl^.start_input_pass (cinfo);

{$ifdef D_MULTISCAN_FILES_SUPPORTED}
  { If jpeg_start_decompress will read the whole file, initialize
    progress monitoring appropriately.  The input step is counted
    as one pass. }

  if (cinfo^.progress <> NIL) and (not cinfo^.buffered_image) and
     (cinfo^.inputctl^.has_multiple_scans) then
  begin

    { Estimate number of scans to set pass_limit. }
    if (cinfo^.progressive_mode) then
    begin
      { Arbitrarily estimate 2 interleaved DC scans + 3 AC scans/component. }
      nscans := 2 + 3 * cinfo^.num_components;
    end
    else
    begin
      { For a nonprogressive multiscan file, estimate 1 scan per component. }
      nscans := cinfo^.num_components;
    end;
    cinfo^.progress^.pass_counter := Long(0);
    cinfo^.progress^.pass_limit := long (cinfo^.total_iMCU_rows) * nscans;
    cinfo^.progress^.completed_passes := 0;
    if cinfo^.enable_2pass_quant then
      cinfo^.progress^.total_passes := 3
    else
      cinfo^.progress^.total_passes := 2;
    { Count the input pass as done }
    Inc(master^.pass_number);
  end;
{$endif} { D_MULTISCAN_FILES_SUPPORTED }
end;


{ Per-pass setup.
  This is called at the beginning of each output pass.  We determine which
  modules will be active during this pass and give them appropriate
  start_pass calls.  We also set is_dummy_pass to indicate whether this
  is a "real" output pass or a dummy pass for color quantization.
  (In the latter case, jdapistd.c will crank the pass to completion.) }

{METHODDEF}
procedure prepare_for_output_pass (cinfo : j_decompress_ptr);
var
  master : jdmaster_my_master_ptr;
begin
  master := jdmaster_my_master_ptr (cinfo^.master);

  if (master^.pub.is_dummy_pass) then
  begin
{$ifdef QUANT_2PASS_SUPPORTED}
    { Final pass of 2-pass quantization }
    master^.pub.is_dummy_pass := FALSE;
    cinfo^.cquantize^.start_pass (cinfo, FALSE);
    cinfo^.post^.start_pass (cinfo, JBUF_CRANK_DEST);
    cinfo^.main^.start_pass (cinfo, JBUF_CRANK_DEST);
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif} { QUANT_2PASS_SUPPORTED }
  end
  else
  begin
    if (cinfo^.quantize_colors) and (cinfo^.colormap = NIL) then
    begin
      { Select new quantization method }
      if (cinfo^.two_pass_quantize) and (cinfo^.enable_2pass_quant) then
      begin
	cinfo^.cquantize := master^.quantizer_2pass;
	master^.pub.is_dummy_pass := TRUE;
      end
      else
        if (cinfo^.enable_1pass_quant) then
        begin
	  cinfo^.cquantize := master^.quantizer_1pass;
        end
        else
        begin
	  ERREXIT(j_common_ptr(cinfo), JERR_MODE_CHANGE);
        end;
    end;
    cinfo^.idct^.start_pass (cinfo);
    cinfo^.coef^.start_output_pass (cinfo);
    if (not cinfo^.raw_data_out) then
    begin
      if (not master^.using_merged_upsample) then
	cinfo^.cconvert^.start_pass (cinfo);
      cinfo^.upsample^.start_pass (cinfo);
      if (cinfo^.quantize_colors) then
	cinfo^.cquantize^.start_pass (cinfo, master^.pub.is_dummy_pass);
      if master^.pub.is_dummy_pass  then
        cinfo^.post^.start_pass (cinfo, JBUF_SAVE_AND_PASS)
      else
        cinfo^.post^.start_pass (cinfo, JBUF_PASS_THRU);
      cinfo^.main^.start_pass (cinfo, JBUF_PASS_THRU);
    end;
  end;

  { Set up progress monitor's pass info if present }
  if (cinfo^.progress <> NIL) then
  begin
    cinfo^.progress^.completed_passes := master^.pass_number;
    if master^.pub.is_dummy_pass then
      cinfo^.progress^.total_passes := master^.pass_number + 2
    else
      cinfo^.progress^.total_passes := master^.pass_number + 1;
    { In buffered-image mode, we assume one more output pass if EOI not
      yet reached, but no more passes if EOI has been reached. }

    if (cinfo^.buffered_image) and (not cinfo^.inputctl^.eoi_reached) then
    begin
      if cinfo^.enable_2pass_quant then
        Inc(cinfo^.progress^.total_passes, 2)
      else
        Inc(cinfo^.progress^.total_passes, 1);
    end;
  end;
end;


{ Finish up at end of an output pass. }

{METHODDEF}
procedure finish_output_pass (cinfo : j_decompress_ptr);
var
  master : jdmaster_my_master_ptr;
begin
  master := jdmaster_my_master_ptr (cinfo^.master);

  if (cinfo^.quantize_colors) then
    cinfo^.cquantize^.finish_pass (cinfo);
  Inc(master^.pass_number);
end;


{$ifdef D_MULTISCAN_FILES_SUPPORTED}

{ Switch to a new external colormap between output passes. }

{GLOBAL}
procedure jpeg_new_colormap (cinfo : j_decompress_ptr);
var
  master : jdmaster_my_master_ptr;
begin
  master := jdmaster_my_master_ptr (cinfo^.master);

  { Prevent application from calling me at wrong times }
  if (cinfo^.global_state <> DSTATE_BUFIMAGE) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  if (cinfo^.quantize_colors) and (cinfo^.enable_external_quant) and
     (cinfo^.colormap <> NIL) then
  begin
    { Select 2-pass quantizer for external colormap use }
    cinfo^.cquantize := master^.quantizer_2pass;
    { Notify quantizer of colormap change }
    cinfo^.cquantize^.new_color_map (cinfo);
    master^.pub.is_dummy_pass := FALSE; { just in case }
  end
  else
    ERREXIT(j_common_ptr(cinfo), JERR_MODE_CHANGE);
end;

{$endif} { D_MULTISCAN_FILES_SUPPORTED }


{ Initialize master decompression control and select active modules.
  This is performed at the start of jpeg_start_decompress. }

{GLOBAL}
procedure jinit_master_decompress (cinfo : j_decompress_ptr);
var
  master : jdmaster_my_master_ptr;
begin
  master := jdmaster_my_master_ptr (
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				  SIZEOF(my_decomp_master)) );
  cinfo^.master := jpeg_decomp_master_ptr(master);
  master^.pub.prepare_for_output_pass := prepare_for_output_pass;
  master^.pub.finish_output_pass := finish_output_pass;

  master^.pub.is_dummy_pass := FALSE;

  master_selection(cinfo);
end;

{== imjdapimin implementation ==}

procedure jpeg_create_decompress(cinfo : j_decompress_ptr);
begin
  jpeg_CreateDecompress(cinfo, JPEG_LIB_VERSION,
    size_t(sizeof(jpeg_decompress_struct)));
end;

{ Initialization of a JPEG decompression object.
  The error manager must already be set up (in case memory manager fails). }

{GLOBAL}
procedure jpeg_CreateDecompress (cinfo : j_decompress_ptr;
                                 version : int;
                                 structsize : size_t);
var
  i : int;
var
  err : jpeg_error_mgr_ptr;
  client_data : voidp;
begin
  { Guard against version mismatches between library and caller. }
  cinfo^.mem := NIL;		{ so jpeg_destroy knows mem mgr not called }
  if (version <> JPEG_LIB_VERSION) then
    ERREXIT2(j_common_ptr(cinfo), JERR_BAD_LIB_VERSION, JPEG_LIB_VERSION, version);
  if (structsize <> SIZEOF(jpeg_decompress_struct)) then
    ERREXIT2(j_common_ptr(cinfo), JERR_BAD_STRUCT_SIZE,
	     int(SIZEOF(jpeg_decompress_struct)), int(structsize));

  { For debugging purposes, we zero the whole master structure.
    But the application has already set the err pointer, and may have set
    client_data, so we have to save and restore those fields.
    Note: if application hasn't set client_data, tools like Purify may
    complain here. }
  begin
    err := cinfo^.err;
    client_data := cinfo^.client_data; { ignore Purify complaint here }
    MEMZERO(j_common_ptr(cinfo), SIZEOF(jpeg_decompress_struct));
    cinfo^.err := err;
    cinfo^.client_data := client_data;
  end;
  cinfo^.is_decompressor := TRUE;

  { Initialize a memory manager instance for this object }
  jinit_memory_mgr(j_common_ptr(cinfo));

  { Zero out pointers to permanent structures. }
  cinfo^.progress := NIL;
  cinfo^.src := NIL;

  for i := 0 to pred(NUM_QUANT_TBLS) do
    cinfo^.quant_tbl_ptrs[i] := NIL;

  for i := 0 to pred(NUM_HUFF_TBLS) do
  begin
    cinfo^.dc_huff_tbl_ptrs[i] := NIL;
    cinfo^.ac_huff_tbl_ptrs[i] := NIL;
  end;

  { Initialize marker processor so application can override methods
    for COM, APPn markers before calling jpeg_read_header.  }
  cinfo^.marker_list := NIL;
  jinit_marker_reader(cinfo);

  { And initialize the overall input controller. }
  jinit_input_controller(cinfo);

  { OK, I'm ready }
  cinfo^.global_state := DSTATE_START;
end;


{ Destruction of a JPEG decompression object }

{GLOBAL}
procedure jpeg_destroy_decompress (cinfo : j_decompress_ptr);
begin
  jpeg_destroy(j_common_ptr(cinfo)); { use common routine }
end;


{ Abort processing of a JPEG decompression operation,
  but don't destroy the object itself. }

{GLOBAL}
procedure jpeg_abort_decompress (cinfo : j_decompress_ptr);
begin
  jpeg_abort(j_common_ptr(cinfo)); { use common routine }
end;


{ Set default decompression parameters. }

{LOCAL}
procedure default_decompress_parms (cinfo : j_decompress_ptr);
var
  cid0 : int;
  cid1 : int;
  cid2 : int;
begin
  { Guess the input colorspace, and set output colorspace accordingly. }
  { (Wish JPEG committee had provided a real way to specify this...) }
  { Note application may override our guesses. }
  case (cinfo^.num_components) of
  1: begin
       cinfo^.jpeg_color_space := JCS_GRAYSCALE;
       cinfo^.out_color_space := JCS_GRAYSCALE;
     end;

  3: begin
       if (cinfo^.saw_JFIF_marker) then
       begin
         cinfo^.jpeg_color_space := JCS_YCbCr; { JFIF implies YCbCr }
       end
       else
         if (cinfo^.saw_Adobe_marker) then
         begin
           case (cinfo^.Adobe_transform) of
           0: cinfo^.jpeg_color_space := JCS_RGB;
           1: cinfo^.jpeg_color_space := JCS_YCbCr;
           else
             begin
	       WARNMS1(j_common_ptr(cinfo), JWRN_ADOBE_XFORM, cinfo^.Adobe_transform);
               cinfo^.jpeg_color_space := JCS_YCbCr; { assume it's YCbCr }
             end;
           end;
         end
         else
         begin
           { Saw no special markers, try to guess from the component IDs }
           cid0 := cinfo^.comp_info^[0].component_id;
           cid1 := cinfo^.comp_info^[1].component_id;
           cid2 := cinfo^.comp_info^[2].component_id;

           if (cid0 = 1) and (cid1 = 2) and (cid2 = 3) then
	     cinfo^.jpeg_color_space := JCS_YCbCr { assume JFIF w/out marker }
           else
             if (cid0 = 82) and (cid1 = 71) and (cid2 = 66) then
               cinfo^.jpeg_color_space := JCS_RGB { ASCII 'R', 'G', 'B' }
             else
             begin
               {$IFDEF DEBUG}
	       TRACEMS3(j_common_ptr(cinfo), 1, JTRC_UNKNOWN_IDS, cid0, cid1, cid2);
               {$ENDIF}
               cinfo^.jpeg_color_space := JCS_YCbCr; { assume it's YCbCr }
             end;
         end;
       { Always guess RGB is proper output colorspace. }
       cinfo^.out_color_space := JCS_RGB;
     end;

  4: begin
       if (cinfo^.saw_Adobe_marker) then
       begin
         case (cinfo^.Adobe_transform) of
         0: cinfo^.jpeg_color_space := JCS_CMYK;
         2: cinfo^.jpeg_color_space := JCS_YCCK;
         else
           begin
             WARNMS1(j_common_ptr(cinfo), JWRN_ADOBE_XFORM, cinfo^.Adobe_transform);
             cinfo^.jpeg_color_space := JCS_YCCK; { assume it's YCCK }
           end;
         end;
       end
       else
       begin
         { No special markers, assume straight CMYK. }
         cinfo^.jpeg_color_space := JCS_CMYK;
       end;
       cinfo^.out_color_space := JCS_CMYK;
     end;

  else
    begin
      cinfo^.jpeg_color_space := JCS_UNKNOWN;
      cinfo^.out_color_space := JCS_UNKNOWN;
    end;
  end;

  { Set defaults for other decompression parameters. }
  cinfo^.scale_num := 1;		{ 1:1 scaling }
  cinfo^.scale_denom := 1;
  cinfo^.output_gamma := 1.0;
  cinfo^.buffered_image := FALSE;
  cinfo^.raw_data_out := FALSE;
  cinfo^.dct_method := JDCT_DEFAULT;
  cinfo^.do_fancy_upsampling := TRUE;
  cinfo^.do_block_smoothing := TRUE;
  cinfo^.quantize_colors := FALSE;
  { We set these in case application only sets quantize_colors. }
  cinfo^.dither_mode := JDITHER_FS;
{$ifdef QUANT_2PASS_SUPPORTED}
  cinfo^.two_pass_quantize := TRUE;
{$else}
  cinfo^.two_pass_quantize := FALSE;
{$endif}
  cinfo^.desired_number_of_colors := 256;
  cinfo^.colormap := NIL;
  { Initialize for no mode change in buffered-image mode. }
  cinfo^.enable_1pass_quant := FALSE;
  cinfo^.enable_external_quant := FALSE;
  cinfo^.enable_2pass_quant := FALSE;
end;


{ Decompression startup: read start of JPEG datastream to see what's there.
  Need only initialize JPEG object and supply a data source before calling.

  This routine will read as far as the first SOS marker (ie, actual start of
  compressed data), and will save all tables and parameters in the JPEG
  object.  It will also initialize the decompression parameters to default
  values, and finally return JPEG_HEADER_OK.  On return, the application may
  adjust the decompression parameters and then call jpeg_start_decompress.
  (Or, if the application only wanted to determine the image parameters,
  the data need not be decompressed.  In that case, call jpeg_abort or
  jpeg_destroy to release any temporary space.)
  If an abbreviated (tables only) datastream is presented, the routine will
  return JPEG_HEADER_TABLES_ONLY upon reaching EOI.  The application may then
  re-use the JPEG object to read the abbreviated image datastream(s).
  It is unnecessary (but OK) to call jpeg_abort in this case.
  The JPEG_SUSPENDED return code only occurs if the data source module
  requests suspension of the decompressor.  In this case the application
  should load more source data and then re-call jpeg_read_header to resume
  processing.
  If a non-suspending data source is used and require_image is TRUE, then the
  return code need not be inspected since only JPEG_HEADER_OK is possible.

  This routine is now just a front end to jpeg_consume_input, with some
  extra error checking. }

{GLOBAL}
function jpeg_read_header (cinfo : j_decompress_ptr;
                           require_image : boolean) : int;
var
  retcode : int;
begin
  if (cinfo^.global_state <> DSTATE_START) and
     (cinfo^.global_state <> DSTATE_INHEADER) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  retcode := jpeg_consume_input(cinfo);

  case (retcode) of
  JPEG_REACHED_SOS:
    retcode := JPEG_HEADER_OK;
  JPEG_REACHED_EOI:
    begin
      if (require_image) then   { Complain if application wanted an image }
        ERREXIT(j_common_ptr(cinfo), JERR_NO_IMAGE);
      { Reset to start state; it would be safer to require the application to
        call jpeg_abort, but we can't change it now for compatibility reasons.
        A side effect is to free any temporary memory (there shouldn't be any). }

      jpeg_abort(j_common_ptr(cinfo)); { sets state := DSTATE_START }
      retcode := JPEG_HEADER_TABLES_ONLY;
    end;
  JPEG_SUSPENDED: ;    { no work }
  end;

  jpeg_read_header := retcode;
end;


{ Consume data in advance of what the decompressor requires.
  This can be called at any time once the decompressor object has
  been created and a data source has been set up.

  This routine is essentially a state machine that handles a couple
  of critical state-transition actions, namely initial setup and
  transition from header scanning to ready-for-start_decompress.
  All the actual input is done via the input controller's consume_input
  method. }

{GLOBAL}
function jpeg_consume_input (cinfo : j_decompress_ptr) : int;
var
  retcode : int;
begin
  retcode := JPEG_SUSPENDED;

  { NB: every possible DSTATE value should be listed in this switch }

  if (cinfo^.global_state) = DSTATE_START then
  begin {work around the FALLTHROUGH}
    { Start-of-datastream actions: reset appropriate modules }
    cinfo^.inputctl^.reset_input_controller (cinfo);
    { Initialize application's data source module }
    cinfo^.src^.init_source (cinfo);
    cinfo^.global_state := DSTATE_INHEADER;
  end;

  case (cinfo^.global_state) of
  DSTATE_START,
  DSTATE_INHEADER:
    begin
      retcode := cinfo^.inputctl^.consume_input (cinfo);
      if (retcode = JPEG_REACHED_SOS) then
      begin { Found SOS, prepare to decompress }
        { Set up default parameters based on header data }
        default_decompress_parms(cinfo);
        { Set global state: ready for start_decompress }
        cinfo^.global_state := DSTATE_READY;
      end;
    end;
  DSTATE_READY:
    { Can't advance past first SOS until start_decompress is called }
    retcode := JPEG_REACHED_SOS;

  DSTATE_PRELOAD,
  DSTATE_PRESCAN,
  DSTATE_SCANNING,
  DSTATE_RAW_OK,
  DSTATE_BUFIMAGE,
  DSTATE_BUFPOST,
  DSTATE_STOPPING:
    retcode := cinfo^.inputctl^.consume_input (cinfo);
  else
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  end;
  jpeg_consume_input := retcode;
end;


{ Have we finished reading the input file? }

{GLOBAL}
function jpeg_input_complete (cinfo : j_decompress_ptr) : boolean;
begin
  { Check for valid jpeg object }
  if (cinfo^.global_state < DSTATE_START) or
     (cinfo^.global_state > DSTATE_STOPPING) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  jpeg_input_complete := cinfo^.inputctl^.eoi_reached;
end;


{ Is there more than one scan? }

{GLOBAL}
function jpeg_has_multiple_scans (cinfo : j_decompress_ptr) : boolean;
begin
  { Only valid after jpeg_read_header completes }
  if (cinfo^.global_state < DSTATE_READY) or
     (cinfo^.global_state > DSTATE_STOPPING) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  jpeg_has_multiple_scans := cinfo^.inputctl^.has_multiple_scans;
end;


{ Finish JPEG decompression.

  This will normally just verify the file trailer and release temp storage.

  Returns FALSE if suspended.  The return value need be inspected only if
  a suspending data source is used. }

{GLOBAL}
function jpeg_finish_decompress (cinfo : j_decompress_ptr) : boolean;
begin
  if ((cinfo^.global_state = DSTATE_SCANNING) or
      (cinfo^.global_state = DSTATE_RAW_OK) and (not cinfo^.buffered_image)) then
  begin
    { Terminate final pass of non-buffered mode }
    if (cinfo^.output_scanline < cinfo^.output_height) then
      ERREXIT(j_common_ptr(cinfo), JERR_TOO_LITTLE_DATA);
    cinfo^.master^.finish_output_pass (cinfo);
    cinfo^.global_state := DSTATE_STOPPING;
  end
  else
    if (cinfo^.global_state = DSTATE_BUFIMAGE) then
    begin
      { Finishing after a buffered-image operation }
      cinfo^.global_state := DSTATE_STOPPING;
    end
    else
      if (cinfo^.global_state <> DSTATE_STOPPING) then
      begin
        { STOPPING := repeat call after a suspension, anything else is error }
        ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
      end;
  { Read until EOI }
  while (not cinfo^.inputctl^.eoi_reached) do
  begin
    if (cinfo^.inputctl^.consume_input (cinfo) = JPEG_SUSPENDED) then
    begin
      jpeg_finish_decompress := FALSE;	{ Suspend, come back later }
      exit;
    end;
  end;
  { Do final cleanup }
  cinfo^.src^.term_source (cinfo);
  { We can use jpeg_abort to release memory and reset global_state }
  jpeg_abort(j_common_ptr(cinfo));
  jpeg_finish_decompress := TRUE;
end;

{== imjdapistd implementation ==}

{ Forward declarations }
{LOCAL}
function output_pass_setup (cinfo : j_decompress_ptr) : boolean; forward;

{ Decompression initialization.
  jpeg_read_header must be completed before calling this.

  If a multipass operating mode was selected, this will do all but the
  last pass, and thus may take a great deal of time.

  Returns FALSE if suspended.  The return value need be inspected only if
  a suspending data source is used. }

{GLOBAL}
function jpeg_start_decompress (cinfo : j_decompress_ptr) : boolean;
var
  retcode : int;
begin
  if (cinfo^.global_state = DSTATE_READY) then
  begin
    { First call: initialize master control, select active modules }
    jinit_master_decompress(cinfo);
    if (cinfo^.buffered_image) then
    begin
      { No more work here; expecting jpeg_start_output next }
      cinfo^.global_state := DSTATE_BUFIMAGE;
      jpeg_start_decompress := TRUE;
      exit;
    end;
    cinfo^.global_state := DSTATE_PRELOAD;
  end;
  if (cinfo^.global_state = DSTATE_PRELOAD) then
  begin
    { If file has multiple scans, absorb them all into the coef buffer }
    if (cinfo^.inputctl^.has_multiple_scans) then
    begin
{$ifdef D_MULTISCAN_FILES_SUPPORTED}
      while TRUE do
      begin

	{ Call progress monitor hook if present }
	if (cinfo^.progress <> NIL) then
	  cinfo^.progress^.progress_monitor (j_common_ptr(cinfo));
	{ Absorb some more input }
	retcode := cinfo^.inputctl^.consume_input (cinfo);
	if (retcode = JPEG_SUSPENDED) then
        begin
          jpeg_start_decompress := FALSE;
          exit;
        end;
	if (retcode = JPEG_REACHED_EOI) then
	  break;
	{ Advance progress counter if appropriate }
	if (cinfo^.progress <> NIL) and
	   ((retcode = JPEG_ROW_COMPLETED) or (retcode = JPEG_REACHED_SOS)) then
        begin
          Inc(cinfo^.progress^.pass_counter);
	  if (cinfo^.progress^.pass_counter >= cinfo^.progress^.pass_limit) then
          begin
	    { jdmaster underestimated number of scans; ratchet up one scan }
	    Inc(cinfo^.progress^.pass_limit, long(cinfo^.total_iMCU_rows));
	  end;
	end;
      end;
{$else}
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif} { D_MULTISCAN_FILES_SUPPORTED }
    end;
    cinfo^.output_scan_number := cinfo^.input_scan_number;
  end
  else
    if (cinfo^.global_state <> DSTATE_PRESCAN) then
      ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  { Perform any dummy output passes, and set up for the final pass }
  jpeg_start_decompress := output_pass_setup(cinfo);
end;


{ Set up for an output pass, and perform any dummy pass(es) needed.
  Common subroutine for jpeg_start_decompress and jpeg_start_output.
  Entry: global_state := DSTATE_PRESCAN only if previously suspended.
  Exit: If done, returns TRUE and sets global_state for proper output mode.
        If suspended, returns FALSE and sets global_state := DSTATE_PRESCAN. }

{LOCAL}
function output_pass_setup (cinfo : j_decompress_ptr) : boolean;
var
  last_scanline : JDIMENSION;
begin
  if (cinfo^.global_state <> DSTATE_PRESCAN) then
  begin
    { First call: do pass setup }
    cinfo^.master^.prepare_for_output_pass (cinfo);
    cinfo^.output_scanline := 0;
    cinfo^.global_state := DSTATE_PRESCAN;
  end;
  { Loop over any required dummy passes }
  while (cinfo^.master^.is_dummy_pass) do
  begin
{$ifdef QUANT_2PASS_SUPPORTED}
    { Crank through the dummy pass }
    while (cinfo^.output_scanline < cinfo^.output_height) do
    begin
      { Call progress monitor hook if present }
      if (cinfo^.progress <> NIL) then
      begin
	cinfo^.progress^.pass_counter := long (cinfo^.output_scanline);
	cinfo^.progress^.pass_limit := long (cinfo^.output_height);
	cinfo^.progress^.progress_monitor (j_common_ptr(cinfo));
      end;
      { Process some data }
      last_scanline := cinfo^.output_scanline;
      cinfo^.main^.process_data (cinfo, JSAMPARRAY(NIL),
				 cinfo^.output_scanline, {var}
                                 JDIMENSION(0));
      if (cinfo^.output_scanline = last_scanline) then
      begin
	output_pass_setup := FALSE;	{ No progress made, must suspend }
        exit;
      end;
    end;
    { Finish up dummy pass, and set up for another one }
    cinfo^.master^.finish_output_pass (cinfo);
    cinfo^.master^.prepare_for_output_pass (cinfo);
    cinfo^.output_scanline := 0;
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif} { QUANT_2PASS_SUPPORTED }
  end;
  { Ready for application to drive output pass through
    jpeg_read_scanlines or jpeg_read_raw_data. }
  if cinfo^.raw_data_out then
    cinfo^.global_state := DSTATE_RAW_OK
   else
     cinfo^.global_state := DSTATE_SCANNING;
  output_pass_setup := TRUE;
end;


{ Read some scanlines of data from the JPEG decompressor.

  The return value will be the number of lines actually read.
  This may be less than the number requested in several cases,
  including bottom of image, data source suspension, and operating
  modes that emit multiple scanlines at a time.

  Note: we warn about excess calls to jpeg_read_scanlines() since
  this likely signals an application programmer error.  However,
  an oversize buffer (max_lines > scanlines remaining) is not an error. }

{GLOBAL}
function jpeg_read_scanlines (cinfo : j_decompress_ptr;
                              scanlines : JSAMPARRAY;
		              max_lines : JDIMENSION) : JDIMENSION;
var
  row_ctr : JDIMENSION;
begin
  if (cinfo^.global_state <> DSTATE_SCANNING) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  if (cinfo^.output_scanline >= cinfo^.output_height) then
  begin
    WARNMS(j_common_ptr(cinfo), JWRN_TOO_MUCH_DATA);
    jpeg_read_scanlines := 0;
    exit;
  end;

  { Call progress monitor hook if present }
  if (cinfo^.progress <> NIL) then
  begin
    cinfo^.progress^.pass_counter := long (cinfo^.output_scanline);
    cinfo^.progress^.pass_limit := long (cinfo^.output_height);
    cinfo^.progress^.progress_monitor (j_common_ptr(cinfo));
  end;

  { Process some data }
  row_ctr := 0;
  cinfo^.main^.process_data (cinfo, scanlines, {var}row_ctr, max_lines);
  Inc(cinfo^.output_scanline, row_ctr);
  jpeg_read_scanlines := row_ctr;
end;


{ Alternate entry point to read raw data.
  Processes exactly one iMCU row per call, unless suspended. }

{GLOBAL}
function jpeg_read_raw_data (cinfo : j_decompress_ptr;
                             data : JSAMPIMAGE;
		             max_lines : JDIMENSION) : JDIMENSION;
var
  lines_per_iMCU_row : JDIMENSION;
begin
  if (cinfo^.global_state <> DSTATE_RAW_OK) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  if (cinfo^.output_scanline >= cinfo^.output_height) then
  begin
    WARNMS(j_common_ptr(cinfo), JWRN_TOO_MUCH_DATA);
    jpeg_read_raw_data := 0;
    exit;
  end;

  { Call progress monitor hook if present }
  if (cinfo^.progress <> NIL) then
  begin
    cinfo^.progress^.pass_counter := long (cinfo^.output_scanline);
    cinfo^.progress^.pass_limit := long (cinfo^.output_height);
    cinfo^.progress^.progress_monitor (j_common_ptr(cinfo));
  end;

  { Verify that at least one iMCU row can be returned. }
  lines_per_iMCU_row := cinfo^.max_v_samp_factor * cinfo^.min_DCT_scaled_size;
  if (max_lines < lines_per_iMCU_row) then
    ERREXIT(j_common_ptr(cinfo), JERR_BUFFER_SIZE);

  { Decompress directly into user's buffer. }
  if (cinfo^.coef^.decompress_data (cinfo, data) = 0) then
  begin
    jpeg_read_raw_data := 0;			{ suspension forced, can do nothing more }
    exit;
  end;

  { OK, we processed one iMCU row. }
  Inc(cinfo^.output_scanline, lines_per_iMCU_row);
  jpeg_read_raw_data := lines_per_iMCU_row;
end;


{ Additional entry points for buffered-image mode. }

{$ifdef D_MULTISCAN_FILES_SUPPORTED}

{ Initialize for an output pass in buffered-image mode. }

{GLOBAL}
function jpeg_start_output (cinfo : j_decompress_ptr;
                            scan_number : int) : boolean;
begin
  if (cinfo^.global_state <> DSTATE_BUFIMAGE) and
     (cinfo^.global_state <> DSTATE_PRESCAN) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  { Limit scan number to valid range }
  if (scan_number <= 0) then
    scan_number := 1;
  if (cinfo^.inputctl^.eoi_reached) and
     (scan_number > cinfo^.input_scan_number) then
    scan_number := cinfo^.input_scan_number;
  cinfo^.output_scan_number := scan_number;
  { Perform any dummy output passes, and set up for the real pass }
  jpeg_start_output := output_pass_setup(cinfo);
end;


{ Finish up after an output pass in buffered-image mode.

  Returns FALSE if suspended.  The return value need be inspected only if
  a suspending data source is used. }

{GLOBAL}
function jpeg_finish_output (cinfo : j_decompress_ptr) : boolean;
begin
  if ((cinfo^.global_state = DSTATE_SCANNING) or
      (cinfo^.global_state = DSTATE_RAW_OK) and cinfo^.buffered_image) then
  begin
    { Terminate this pass. }
    { We do not require the whole pass to have been completed. }
    cinfo^.master^.finish_output_pass (cinfo);
    cinfo^.global_state := DSTATE_BUFPOST;
  end
  else
    if (cinfo^.global_state <> DSTATE_BUFPOST) then
    begin
      { BUFPOST := repeat call after a suspension, anything else is error }
      ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
    end;
  { Read markers looking for SOS or EOI }
  while (cinfo^.input_scan_number <= cinfo^.output_scan_number) and
	(not cinfo^.inputctl^.eoi_reached) do
  begin
    if (cinfo^.inputctl^.consume_input (cinfo) = JPEG_SUSPENDED) then
    begin
      jpeg_finish_output := FALSE;	{ Suspend, come back later }
      exit;
    end;
  end;
  cinfo^.global_state := DSTATE_BUFIMAGE;
  jpeg_finish_output := TRUE;
end;

{$endif} { D_MULTISCAN_FILES_SUPPORTED }

end.
