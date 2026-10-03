unit ImPasZLib;

{ -------------------------------------------------------------------- }

{$DEFINE MAX_MATCH_IS_258}

{ Compile with -DMAXSEG_64K if the alloc function cannot allocate more
  than 64k bytes at a time (needed on systems with 16-bit int). }

{$UNDEF MAXSEG_64K}
{$DEFINE UNALIGNED_OK}    { requires SizeOf(ush) = 2 ! }
{$UNDEF DYNAMIC_CRC_TABLE}
{$UNDEF FASTEST}
{$DEFINE Use32}
{$DEFINE patch112}        { apply patch from the zlib home page }

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

// Undef DEBUG mode for ImPasZLib. DEBUG is off by default, since it's much slower
// Comment this out if you want to debug ZLib.
{$UNDEF DEBUG}

{$IFNDEF DEBUG}
  {$RANGECHECKS OFF}
  {$OVERFLOWCHECKS OFF}
{$ENDIF}

{ -------------------------------------------------------------------- }

interface

uses
  SysUtils,
  StrUtils;

{== imzutil interface ==}

{
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

const
  ZLIB_VERSION : string[10] = '1.1.2';

{ Type declarations }

type
  {Byte   = usigned char;  8 bits}
  Bytef  = byte;
  charf  = byte;

  int    = integer;
  intf   = int;
  uInt   = cardinal;
  uIntf  = uInt;

  Long   = longint;
  uLong  = Cardinal;
  uLongf = uLong;

  voidp  = pointer;
  voidpf = voidp;
  pBytef = ^Bytef;
  pIntf  = ^intf;
  puIntf = ^uIntf;
  puLong = ^uLongf;

{$IF Defined(FPC)}
  ptr2int = PtrUInt;
{$ELSEIF CompilerVersion >= 20}
  ptr2int = NativeUInt;
{$ELSE}
  ptr2int = Cardinal;
{$IFEND}
{ a pointer to integer casting is used to do pointer arithmetic. }

type
  zByteArray = array[0..(MaxInt div SizeOf(Bytef))-1] of Bytef;
  pzByteArray = ^zByteArray;
type
  zIntfArray = array[0..(MaxInt div SizeOf(Intf))-1] of Intf;
  pzIntfArray = ^zIntfArray;
type
  zuIntArray = array[0..(MaxInt div SizeOf(uInt))-1] of uInt;
  PuIntArray = ^zuIntArray;

{ Type declarations - only for deflate }

type
  uch  = Byte;
  uchf = uch; { FAR }
  ush  = Word;
  ushf = ush;
  ulg  = LongInt;

  unsigned = uInt;

  pcharf = ^charf;
  puchf = ^uchf;
  pushf = ^ushf;

type
  zuchfArray = zByteArray;
  puchfArray = ^zuchfArray;
type
  zushfArray = array[0..(MaxInt div SizeOf(ushf))-1] of ushf;
  pushfArray = ^zushfArray;

procedure zmemcpy(destp : pBytef; sourcep : pBytef; len : uInt);
function zmemcmp(s1p, s2p : pBytef; len : uInt) : int;
procedure zmemzero(destp : pBytef; len : uInt);
procedure zcfree(opaque : voidpf; ptr : voidpf);
function zcalloc (opaque : voidpf; items : uInt; size : uInt) : voidpf;

{== impaszlib interface ==}

{ Original:
   zlib.h -- interface of the 'zlib' general purpose compression library
  version 1.1.0, Feb 24th, 1998

  Copyright (C) 1995-1998 Jean-loup Gailly and Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
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

  Jean-loup Gailly        Mark Adler
  jloup@gzip.org          madler@alumni.caltech.edu


  The data format used by the zlib library is described by RFCs (Request for
  Comments) 1950 to 1952 in the files ftp://ds.internic.net/rfc/rfc1950.txt
  (zlib format), rfc1951.txt (deflate format) and rfc1952.txt (gzip format).


  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

{ zconf.h -- configuration of the zlib compression library }
{ zutil.c -- target dependent utility functions for the compression library }

{ The 'zlib' compression library provides in-memory compression and
  decompression functions, including integrity checks of the uncompressed
  data.  This version of the library supports only one compression method
  (deflation) but other algorithms will be added later and will have the same
  stream interface.

     Compression can be done in a single step if the buffers are large
  enough (for example if an input file is mmap'ed), or can be done by
  repeated calls of the compression function.  In the latter case, the
  application must provide more input and/or consume the output
  (providing more output space) before each call.

     The library also supports reading and writing files in gzip (.gz) format
  with an interface similar to that of stdio.

     The library does not install any signal handler. The decoder checks
  the consistency of the compressed data, so the library should never
  crash even in case of corrupted input. }



{ Compile with -DMAXSEG_64K if the alloc function cannot allocate more
  than 64k bytes at a time (needed on systems with 16-bit int). }

{ Maximum value for memLevel in deflateInit2 }
const
  MAX_MEM_LEVEL = 9;
  DEF_MEM_LEVEL = 9; { if MAX_MEM_LEVEL > 8 }

{ Maximum value for windowBits in deflateInit2 and inflateInit2 }
const
  MAX_WBITS = 15; { 32K LZ77 window }

{ default windowBits for decompression. MAX_WBITS is for compression only }
const
  DEF_WBITS = MAX_WBITS;

{ The memory requirements for deflate are (in bytes):
            1 shl (windowBits+2)   +  1 shl (memLevel+9)
 that is: 128K for windowBits=15  +  128K for memLevel = 8  (default values)
 plus a few kilobytes for small objects. For example, if you want to reduce
 the default memory requirements from 256K to 128K, compile with
     DMAX_WBITS=14 DMAX_MEM_LEVEL=7
 Of course this will generally degrade compression (there's no free lunch).

 The memory requirements for inflate are (in bytes) 1 shl windowBits
 that is, 32K for windowBits=15 (default value) plus a few kilobytes
 for small objects. }


{ Huffman code lookup table entry--this entry is four bytes for machines
  that have 16-bit pointers (e.g. PC's in the small or medium model). }

type
  pInflate_huft = ^inflate_huft;
  inflate_huft = Record
    Exop,             { number of extra bits or operation }
    bits : Byte;      { number of bits in this code or subcode }
    {pad : uInt;}       { pad structure to a power of 2 (4 bytes for }
                      {  16-bit, 8 bytes for 32-bit int's) }
    base : uInt;      { literal, length base, or distance base }
                      { or table offset }
  End;

type
  huft_field = Array[0..(MaxInt div SizeOf(inflate_huft))-1] of inflate_huft;
  huft_ptr = ^huft_field;
type
  ppInflate_huft = ^pInflate_huft;

type
  inflate_codes_mode = ( { waiting for "i:"=input, "o:"=output, "x:"=nothing }
        START,    { x: set up for LEN }
        LEN,      { i: get length/literal/eob next }
        LENEXT,   { i: getting length extra (have base) }
        DIST,     { i: get distance next }
        DISTEXT,  { i: getting distance extra }
        COPY,     { o: copying bytes in window, waiting for space }
        LIT,      { o: got literal, waiting for output space }
        WASH,     { o: got eob, possibly still output waiting }
        ZEND,     { x: got eob and all data flushed }
        BADCODE); { x: got error }

{ inflate codes private state }
type
  pInflate_codes_state = ^inflate_codes_state;
  inflate_codes_state = record

    mode : inflate_codes_mode;        { current inflate_codes mode }

    { mode dependent information }
    len : uInt;
    sub : record                      { submode }
      Case Byte of
      0:(code : record                { if LEN or DIST, where in tree }
          tree : pInflate_huft;       { pointer into tree }
          need : uInt;                { bits needed }
         end);
      1:(lit : uInt);                 { if LIT, literal }
      2:(copy: record                 { if EXT or COPY, where and how much }
           get : uInt;                { bits to get for extra }
           dist : uInt;               { distance back to copy from }
         end);
    end;

    { mode independent information }
    lbits : Byte;                     { ltree bits decoded per branch }
    dbits : Byte;                     { dtree bits decoder per branch }
    ltree : pInflate_huft;            { literal/length/eob tree }
    dtree : pInflate_huft;            { distance tree }
  end;

type
  check_func = function(check : uLong;
                        buf : pBytef;
                        {const buf : array of byte;}
	                len : uInt) : uLong;
type
  inflate_block_mode =
     (ZTYPE,    { get type bits (3, including end bit) }
      LENS,     { get lengths for stored }
      STORED,   { processing stored block }
      TABLE,    { get table lengths }
      BTREE,    { get bit lengths tree for a dynamic block }
      DTREE,    { get length, distance trees for a dynamic block }
      CODES,    { processing fixed or dynamic block }
      DRY,      { output remaining window bytes }
      BLKDONE,  { finished last block, done }
      BLKBAD);  { got a data error--stuck here }

type
  pInflate_blocks_state = ^inflate_blocks_state;

{ inflate blocks semi-private state }
  inflate_blocks_state = record

    mode : inflate_block_mode;     { current inflate_block mode }

    { mode dependent information }
    sub : record                  { submode }
    case Byte of
    0:(left : uInt);              { if STORED, bytes left to copy }
    1:(trees : record             { if DTREE, decoding info for trees }
        table : uInt;               { table lengths (14 bits) }
        index : uInt;               { index into blens (or border) }
        blens : PuIntArray;         { bit lengths of codes }
        bb : uInt;                  { bit length tree depth }
        tb : pInflate_huft;         { bit length decoding tree }
      end);
    2:(decode : record            { if CODES, current state }
        tl : pInflate_huft;
        td : pInflate_huft;         { trees to free }
        codes : pInflate_codes_state;
      end);
    end;
    last : boolean;               { true if this block is the last block }

    { mode independent information }
    bitk : uInt;            { bits in bit buffer }
    bitb : uLong;           { bit buffer }
    hufts : huft_ptr; {pInflate_huft;}  { single malloc for tree space }
    window : pBytef;        { sliding window }
    zend : pBytef;          { one byte after sliding window }
    read : pBytef;          { window read pointer }
    write : pBytef;         { window write pointer }
    checkfn : check_func;   { check function }
    check : uLong;          { check on output }
  end;

type
  inflate_mode = (
      METHOD,   { waiting for method byte }
      FLAG,     { waiting for flag byte }
      DICT4,    { four dictionary check bytes to go }
      DICT3,    { three dictionary check bytes to go }
      DICT2,    { two dictionary check bytes to go }
      DICT1,    { one dictionary check byte to go }
      DICT0,    { waiting for inflateSetDictionary }
      BLOCKS,   { decompressing blocks }
      CHECK4,   { four check bytes to go }
      CHECK3,   { three check bytes to go }
      CHECK2,   { two check bytes to go }
      CHECK1,   { one check byte to go }
      DONE,     { finished check, done }
      BAD);     { got an error--stay here }

{ inflate private state }
type
  pInternal_state = ^internal_state; { or point to a deflate_state record }
  internal_state = record

     mode : inflate_mode;  { current inflate mode }

     { mode dependent information }
     sub : record          { submode }
       case byte of
       0:(method : uInt);  { if FLAGS, method byte }
       1:(check : record   { if CHECK, check values to compare }
           was : uLong;        { computed check value }
           need : uLong;       { stream check value }
          end);
       2:(marker : uInt);  { if BAD, inflateSync's marker bytes count }
     end;

     { mode independent information }
     nowrap : boolean;      { flag for no wrapper }
     wbits : uInt;          { log2(window size)  (8..15, defaults to 15) }
     blocks : pInflate_blocks_state;    { current inflate_blocks state }
   end;

type
  alloc_func = function(opaque : voidpf; items : uInt; size : uInt) : voidpf;
  free_func = procedure(opaque : voidpf; address : voidpf);

type
  z_streamp = ^z_stream;
  z_stream = record
    next_in : pBytef;     { next input byte }
    avail_in : uInt;      { number of bytes available at next_in }
    total_in : uLong;     { total nb of input bytes read so far }

    next_out : pBytef;    { next output byte should be put there }
    avail_out : uInt;     { remaining free space at next_out }
    total_out : uLong;    { total nb of bytes output so far }

    msg : string[255];         { last error message, '' if no error }
    state : pInternal_state; { not visible by applications }

    zalloc : alloc_func;  { used to allocate the internal state }
    zfree : free_func;    { used to free the internal state }
    opaque : voidpf;      { private data object passed to zalloc and zfree }

    data_type : int;      { best guess about the data type: ascii or binary }
    adler : uLong;        { adler32 value of the uncompressed data }
    reserved : uLong;     { reserved for future use }
  end;


{  The application must update next_in and avail_in when avail_in has
   dropped to zero. It must update next_out and avail_out when avail_out
   has dropped to zero. The application must initialize zalloc, zfree and
   opaque before calling the init function. All other fields are set by the
   compression library and must not be updated by the application.

   The opaque value provided by the application will be passed as the first
   parameter for calls of zalloc and zfree. This can be useful for custom
   memory management. The compression library attaches no meaning to the
   opaque value.

   zalloc must return Z_NULL if there is not enough memory for the object.
   On 16-bit systems, the functions zalloc and zfree must be able to allocate
   exactly 65536 bytes, but will not be required to allocate more than this
   if the symbol MAXSEG_64K is defined (see zconf.h). WARNING: On MSDOS,
   pointers returned by zalloc for objects of exactly 65536 bytes *must*
   have their offset normalized to zero. The default allocation function
   provided by this library ensures this (see zutil.c). To reduce memory
   requirements and avoid any allocation of 64K objects, at the expense of
   compression ratio, compile the library with -DMAX_WBITS=14 (see zconf.h).

   The fields total_in and total_out can be used for statistics or
   progress reports. After compression, total_in holds the total size of
   the uncompressed data and may be saved for use in the decompressor
   (particularly if the decompressor wants to decompress everything in
   a single step). }

const  { constants }
   Z_NO_FLUSH      = 0;
   Z_PARTIAL_FLUSH = 1;
   Z_SYNC_FLUSH    = 2;
   Z_FULL_FLUSH    = 3;
   Z_FINISH        = 4;
{ Allowed flush values; see deflate() below for details }

   Z_OK            = 0;
   Z_STREAM_END    = 1;
   Z_NEED_DICT     = 2;
   Z_ERRNO         = (-1);
   Z_STREAM_ERROR  = (-2);
   Z_DATA_ERROR    = (-3);
   Z_MEM_ERROR     = (-4);
   Z_BUF_ERROR     = (-5);
   Z_VERSION_ERROR = (-6);
{ Return codes for the compression/decompression functions. Negative
  values are errors, positive values are used for special but normal events.}

   Z_NO_COMPRESSION         = 0;
   Z_BEST_SPEED             = 1;
   Z_BEST_COMPRESSION       = 9;
   Z_DEFAULT_COMPRESSION    = (-1);
{ compression levels }

   Z_FILTERED            = 1;
   Z_HUFFMAN_ONLY        = 2;
   Z_DEFAULT_STRATEGY    = 0;
{ compression strategy; see deflateInit2() below for details }

   Z_BINARY   = 0;
   Z_ASCII    = 1;
   Z_UNKNOWN  = 2;
{ Possible values of the data_type field }

   Z_DEFLATED   = 8;
{ The deflate compression method (the only one supported in this version) }

   Z_NULL  = NIL;  { for initializing zalloc, zfree, opaque }


        { common constants }


{ The three kinds of block type }
const
  STORED_BLOCK = 0;
  STATIC_TREES = 1;
  DYN_TREES = 2;
{ The minimum and maximum match lengths }
const
  MIN_MATCH = 3;
  MAX_MATCH = 258;

const
  PRESET_DICT = $20; { preset dictionary flag in zlib header }


{$IFDEF DEBUG}
procedure Trace(x : string);
procedure Tracev(x : string);
procedure Tracevv(x : string);
procedure Tracevvv(x : string);
procedure Tracec(c : boolean; x : string);
procedure Tracecv(c : boolean; x : string);
{$ENDIF}

function zlibVersion : AnsiString;
{ The application can compare zlibVersion and ZLIB_VERSION for consistency.
  If the first character differs, the library code actually used is
  not compatible with the zlib.h header file used by the application.
  This check is automatically made by deflateInit and inflateInit. }

function zError(err : int) : string;
function ZALLOC (var strm : z_stream; items : uInt; size : uInt) : voidpf;
procedure ZFREE (var strm : z_stream; ptr : voidpf);
procedure TRY_FREE (var strm : z_stream; ptr : voidpf);

const
  z_errbase = Z_NEED_DICT;
  z_errmsg : Array[0..9] of string[21] = { indexed by 2-zlib_error }
           ('need dictionary',     { Z_NEED_DICT       2  }
            'stream end',          { Z_STREAM_END      1  }
            '',                    { Z_OK              0  }
            'file error',          { Z_ERRNO         (-1) }
            'stream error',        { Z_STREAM_ERROR  (-2) }
            'data error',          { Z_DATA_ERROR    (-3) }
            'insufficient memory', { Z_MEM_ERROR     (-4) }
            'buffer error',        { Z_BUF_ERROR     (-5) }
            'incompatible version',{ Z_VERSION_ERROR (-6) }
            '');
const
  z_verbose : int = 0;

function deflateInit_(var Stream: z_stream; Level: LongInt; const Version: AnsiString;
  Stream_size: LongInt): LongInt; overload;
function inflateInit_(var Stream: z_stream; const Version: AnsiString;
  Stream_size: Longint): LongInt; overload;

procedure z_error (m : string);

{== imzdeflate interface ==}

{ Orginal: deflate.h -- internal compression state
           deflate.c -- compress data using the deflation algorithm
  Copyright (C) 1995-1996 Jean-loup Gailly.

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}


{  ALGORITHM

       The "deflation" process depends on being able to identify portions
       of the input text which are identical to earlier input (within a
       sliding window trailing behind the input currently being processed).

       The most straightforward technique turns out to be the fastest for
       most input files: try all possible matches and select the longest.
       The key feature of this algorithm is that insertions into the string
       dictionary are very simple and thus fast, and deletions are avoided
       completely. Insertions are performed at each input character, whereas
       string matches are performed only when the previous match ends. So it
       is preferable to spend more time in matches to allow very fast string
       insertions and avoid deletions. The matching algorithm for small
       strings is inspired from that of Rabin & Karp. A brute force approach
       is used to find longer strings when a small match has been found.
       A similar algorithm is used in comic (by Jan-Mark Wams) and freeze
       (by Leonid Broukhis).
          A previous version of this file used a more sophisticated algorithm
       (by Fiala and Greene) which is guaranteed to run in linear amortized
       time, but has a larger average cost, uses more memory and is patented.
       However the F&G algorithm may be faster for some highly redundant
       files if the parameter max_chain_length (described below) is too large.

   ACKNOWLEDGEMENTS

       The idea of lazy evaluation of matches is due to Jan-Mark Wams, and
       I found it in 'freeze' written by Leonid Broukhis.
       Thanks to many people for bug reports and testing.

   REFERENCES

       Deutsch, L.P.,"'Deflate' Compressed Data Format Specification".
       Available in ftp.uu.net:/pub/archiving/zip/doc/deflate-1.1.doc

       A description of the Rabin and Karp algorithm is given in the book
          "Algorithms" by R. Sedgewick, Addison-Wesley, p252.

       Fiala,E.R., and Greene,D.H.
          Data Compression with Finite Windows, Comm.ACM, 32,4 (1989) 490-595}

function deflateInit_(strm : z_streamp;
                      level : int;
                      const version : AnsiString;
                      stream_size : int) : int; overload;


function deflateInit (var strm : z_stream; level : int) : int;

{  Initializes the internal stream state for compression. The fields
   zalloc, zfree and opaque must be initialized before by the caller.
   If zalloc and zfree are set to Z_NULL, deflateInit updates them to
   use default allocation functions.

     The compression level must be Z_DEFAULT_COMPRESSION, or between 0 and 9:
   1 gives best speed, 9 gives best compression, 0 gives no compression at
   all (the input data is simply copied a block at a time).
   Z_DEFAULT_COMPRESSION requests a default compromise between speed and
   compression (currently equivalent to level 6).

     deflateInit returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_STREAM_ERROR if level is not a valid compression level,
   Z_VERSION_ERROR if the zlib library version (zlib_version) is incompatible
   with the version assumed by the caller (ZLIB_VERSION).
   msg is set to null if there is no error message.  deflateInit does not
   perform any compression: this will be done by deflate(). }

function deflate (var strm : z_stream; flush : int) : int;

{ Performs one or both of the following actions:

  - Compress more input starting at next_in and update next_in and avail_in
    accordingly. If not all input can be processed (because there is not
    enough room in the output buffer), next_in and avail_in are updated and
    processing will resume at this point for the next call of deflate().

  - Provide more output starting at next_out and update next_out and avail_out
    accordingly. This action is forced if the parameter flush is non zero.
    Forcing flush frequently degrades the compression ratio, so this parameter
    should be set only when necessary (in interactive applications).
    Some output may be provided even if flush is not set.

  Before the call of deflate(), the application should ensure that at least
  one of the actions is possible, by providing more input and/or consuming
  more output, and updating avail_in or avail_out accordingly; avail_out
  should never be zero before the call. The application can consume the
  compressed output when it wants, for example when the output buffer is full
  (avail_out == 0), or after each call of deflate(). If deflate returns Z_OK
  and with zero avail_out, it must be called again after making room in the
  output buffer because there might be more output pending.

    If the parameter flush is set to Z_PARTIAL_FLUSH, the current compression
  block is terminated and flushed to the output buffer so that the
  decompressor can get all input data available so far. For method 9, a future
  variant on method 8, the current block will be flushed but not terminated.
  Z_SYNC_FLUSH has the same effect as partial flush except that the compressed
  output is byte aligned (the compressor can clear its internal bit buffer)
  and the current block is always terminated; this can be useful if the
  compressor has to be restarted from scratch after an interruption (in which
  case the internal state of the compressor may be lost).
    If flush is set to Z_FULL_FLUSH, the compression block is terminated, a
  special marker is output and the compression dictionary is discarded; this
  is useful to allow the decompressor to synchronize if one compressed block
  has been damaged (see inflateSync below).  Flushing degrades compression and
  so should be used only when necessary.  Using Z_FULL_FLUSH too often can
  seriously degrade the compression. If deflate returns with avail_out == 0,
  this function must be called again with the same value of the flush
  parameter and more output space (updated avail_out), until the flush is
  complete (deflate returns with non-zero avail_out).

    If the parameter flush is set to Z_FINISH, all pending input is processed,
  all pending output is flushed and deflate returns with Z_STREAM_END if there
  was enough output space; if deflate returns with Z_OK, this function must be
  called again with Z_FINISH and more output space (updated avail_out) but no
  more input data, until it returns with Z_STREAM_END or an error. After
  deflate has returned Z_STREAM_END, the only possible operations on the
  stream are deflateReset or deflateEnd.

    Z_FINISH can be used immediately after deflateInit if all the compression
  is to be done in a single step. In this case, avail_out must be at least
  0.1% larger than avail_in plus 12 bytes.  If deflate does not return
  Z_STREAM_END, then it must be called again as described above.

    deflate() may update data_type if it can make a good guess about
  the input data type (Z_ASCII or Z_BINARY). In doubt, the data is considered
  binary. This field is only for information purposes and does not affect
  the compression algorithm in any manner.

    deflate() returns Z_OK if some progress has been made (more input
  processed or more output produced), Z_STREAM_END if all input has been
  consumed and all output has been produced (only when flush is set to
  Z_FINISH), Z_STREAM_ERROR if the stream state was inconsistent (for example
  if next_in or next_out was NULL), Z_BUF_ERROR if no progress is possible. }

function deflateEnd (var strm : z_stream) : int;

{     All dynamically allocated data structures for this stream are freed.
   This function discards any unprocessed input and does not flush any
   pending output.

     deflateEnd returns Z_OK if success, Z_STREAM_ERROR if the
   stream state was inconsistent, Z_DATA_ERROR if the stream was freed
   prematurely (some input or output was discarded). In the error case,
   msg may be set but then points to a static string (which must not be
   deallocated). }


                        { Advanced functions }

{ The following functions are needed only in some special applications. }

function deflateInit2 (var strm : z_stream;
                       level : int;
                       method : int;
                       windowBits : int;
                       memLevel : int;
                       strategy : int) : int;

{  This is another version of deflateInit with more compression options. The
   fields next_in, zalloc, zfree and opaque must be initialized before by
   the caller.

     The method parameter is the compression method. It must be Z_DEFLATED in
   this version of the library. (Method 9 will allow a 64K history buffer and
   partial block flushes.)

     The windowBits parameter is the base two logarithm of the window size
   (the size of the history buffer).  It should be in the range 8..15 for this
   version of the library (the value 16 will be allowed for method 9). Larger
   values of this parameter result in better compression at the expense of
   memory usage. The default value is 15 if deflateInit is used instead.

     The memLevel parameter specifies how much memory should be allocated
   for the internal compression state. memLevel=1 uses minimum memory but
   is slow and reduces compression ratio; memLevel=9 uses maximum memory
   for optimal speed. The default value is 8. See zconf.h for total memory
   usage as a function of windowBits and memLevel.

     The strategy parameter is used to tune the compression algorithm. Use the
   value Z_DEFAULT_STRATEGY for normal data, Z_FILTERED for data produced by a
   filter (or predictor), or Z_HUFFMAN_ONLY to force Huffman encoding only (no
   string match).  Filtered data consists mostly of small values with a
   somewhat random distribution. In this case, the compression algorithm is
   tuned to compress them better. The effect of Z_FILTERED is to force more
   Huffman coding and less string matching; it is somewhat intermediate
   between Z_DEFAULT and Z_HUFFMAN_ONLY. The strategy parameter only affects
   the compression ratio but not the correctness of the compressed output even
   if it is not set appropriately.

     If next_in is not null, the library will use this buffer to hold also
   some history information; the buffer must either hold the entire input
   data, or have at least 1<<(windowBits+1) bytes and be writable. If next_in
   is null, the library will allocate its own history buffer (and leave next_in
   null). next_out need not be provided here but must be provided by the
   application for the next call of deflate().

     If the history buffer is provided by the application, next_in must
   must never be changed by the application since the compressor maintains
   information inside this buffer from call to call; the application
   must provide more input only by increasing avail_in. next_in is always
   reset by the library in this case.

      deflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was
   not enough memory, Z_STREAM_ERROR if a parameter is invalid (such as
   an invalid method). msg is set to null if there is no error message.
   deflateInit2 does not perform any compression: this will be done by
   deflate(). }

function deflateInit2_(var strm : z_stream;
                       level : int;
                       method : int;
                       windowBits : int;
                       memLevel : int;
                       strategy : int;
                       const version : AnsiString;
                       stream_size : int) : int;

function deflateSetDictionary (var strm : z_stream;
                               dictionary : pBytef; {const bytes}
			       dictLength : uint) : int;

{    Initializes the compression dictionary (history buffer) from the given
   byte sequence without producing any compressed output. This function must
   be called immediately after deflateInit or deflateInit2, before any call
   of deflate. The compressor and decompressor must use exactly the same
   dictionary (see inflateSetDictionary).
     The dictionary should consist of strings (byte sequences) that are likely
   to be encountered later in the data to be compressed, with the most commonly
   used strings preferably put towards the end of the dictionary. Using a
   dictionary is most useful when the data to be compressed is short and
   can be predicted with good accuracy; the data can then be compressed better
   than with the default empty dictionary. In this version of the library,
   only the last 32K bytes of the dictionary are used.
     Upon return of this function, strm->adler is set to the Adler32 value
   of the dictionary; the decompressor may later use this value to determine
   which dictionary has been used by the compressor. (The Adler32 value
   applies to the whole dictionary even if only a subset of the dictionary is
   actually used by the compressor.)

     deflateSetDictionary returns Z_OK if success, or Z_STREAM_ERROR if a
   parameter is invalid (such as NULL dictionary) or the stream state
   is inconsistent (for example if deflate has already been called for this
   stream). deflateSetDictionary does not perform any compression: this will
   be done by deflate(). }

function deflateCopy (dest : z_streamp;
                      source : z_streamp) : int;

{  Sets the destination stream as a complete copy of the source stream.  If
   the source stream is using an application-supplied history buffer, a new
   buffer is allocated for the destination stream.  The compressed output
   buffer is always application-supplied. It's the responsibility of the
   application to provide the correct values of next_out and avail_out for the
   next call of deflate.

     This function can be useful when several compression strategies will be
   tried, for example when there are several ways of pre-processing the input
   data with a filter. The streams that will be discarded should then be freed
   by calling deflateEnd.  Note that deflateCopy duplicates the internal
   compression state which can be quite large, so this strategy is slow and
   can consume lots of memory.

     deflateCopy returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_STREAM_ERROR if the source stream state was inconsistent
   (such as zalloc being NULL). msg is left unchanged in both source and
   destination. }

function deflateReset (var strm : z_stream) : int;

{   This function is equivalent to deflateEnd followed by deflateInit,
   but does not free and reallocate all the internal compression state.
   The stream will keep the same compression level and any other attributes
   that may have been set by deflateInit2.

      deflateReset returns Z_OK if success, or Z_STREAM_ERROR if the source
   stream state was inconsistent (such as zalloc or state being NIL). }

function deflateParams (var strm : z_stream; level : int; strategy : int) : int;

{    Dynamically update the compression level and compression strategy.
   This can be used to switch between compression and straight copy of
   the input data, or to switch to a different kind of input data requiring
   a different strategy. If the compression level is changed, the input
   available so far is compressed with the old level (and may be flushed);
   the new level will take effect only at the next call of deflate().

     Before the call of deflateParams, the stream state must be set as for
   a call of deflate(), since the currently available input may have to
   be compressed and flushed. In particular, strm->avail_out must be non-zero.

     deflateParams returns Z_OK if success, Z_STREAM_ERROR if the source
   stream state was inconsistent or if a parameter was invalid, Z_BUF_ERROR
   if strm->avail_out was zero. }

const
   deflate_copyright : string = ' deflate 1.1.2 Copyright 1995-1998 Jean-loup Gailly ';

{ If you use the zlib library in a product, an acknowledgment is welcome
  in the documentation of your product. If for some reason you cannot
  include such an acknowledgment, I would appreciate that you keep this
  copyright string in the executable of your product. }

{== imadler interface ==}

{
  adler32.c -- compute the Adler-32 checksum of a data stream
  Copyright (C) 1995-1998 Mark Adler

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

function adler32(adler : uLong; buf : pBytef; len : uInt) : uLong;

{    Update a running Adler-32 checksum with the bytes buf[0..len-1] and
   return the updated checksum. If buf is NIL, this function returns
   the required initial value for the checksum.
   An Adler-32 checksum is almost as reliable as a CRC32 but can be computed
   much faster. Usage example:

   var
     adler : uLong;
   begin
     adler := adler32(0, Z_NULL, 0);

     while (read_buffer(buffer, length) <> EOF) do
       adler := adler32(adler, buffer, length);

     if (adler <> original_adler) then
       error();
   end;
}

{== iminfblock interface ==}

{ infblock.h and
  infblock.c -- interpret and process block types to last block
  Copyright (C) 1995-1998 Mark Adler

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

function inflate_blocks_new(var z : z_stream;
                            c : check_func;  { check function }
                            w : uInt     { window size }
                            ) : pInflate_blocks_state;

function inflate_blocks (var s : inflate_blocks_state;
                         var z : z_stream;
                         r : int             { initial return code }
                         ) : int;

procedure inflate_blocks_reset (var s : inflate_blocks_state;
                                var z : z_stream;
                                c : puLong); { check value on output }


function inflate_blocks_free(s : pInflate_blocks_state;
                             var z : z_stream) : int;

procedure inflate_set_dictionary(var s : inflate_blocks_state;
                                 const d : array of byte;  { dictionary }
                                 n : uInt);         { dictionary length }

function inflate_blocks_sync_point(var s : inflate_blocks_state) : int;

{== iminfcodes interface ==}

{ infcodes.c -- process literals and length/distance pairs
  Copyright (C) 1995-1998 Mark Adler

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

function inflate_codes_new (bl : uInt;
                            bd : uInt;
                            tl : pInflate_huft;
                            td : pInflate_huft;
                            var z : z_stream): pInflate_codes_state;

function inflate_codes(var s : inflate_blocks_state;
                       var z : z_stream;
                       r : int) : int;

procedure inflate_codes_free(c : pInflate_codes_state;
                             var z : z_stream);

{== iminffast interface ==}

{
  inffast.h and
  inffast.c -- process literals and length/distance pairs fast
  Copyright (C) 1995-1998 Mark Adler

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

function inflate_fast( bl : uInt;
                       bd : uInt;
                       tl : pInflate_huft;
                       td : pInflate_huft;
                      var s : inflate_blocks_state;
                      var z : z_stream) : int;

{== iminftrees interface ==}

{ inftrees.h -- header to use inftrees.c
  inftrees.c -- generate Huffman trees for efficient decoding
  Copyright (C) 1995-1998 Mark Adler

  WARNING: this file should *not* be used by applications. It is
   part of the implementation of the compression library and is
   subject to change.

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

{ Maximum size of dynamic tree.  The maximum found in a long but non-
  exhaustive search was 1004 huft structures (850 for length/literals
  and 154 for distances, the latter actually the result of an
  exhaustive search).  The actual maximum is not known, but the
  value below is more than safe. }
const
  MANY = 1440;


{$ifdef DEBUG}
var
  inflate_hufts : uInt;
{$endif}

function inflate_trees_bits(
  var c : array of uIntf;  { 19 code lengths }
  var bb : uIntf;          { bits tree desired/actual depth }
  var tb : pinflate_huft;  { bits tree result }
  var hp : array of Inflate_huft;      { space for trees }
  var z : z_stream         { for messages }
    ) : int;

function inflate_trees_dynamic(
    nl : uInt;                    { number of literal/length codes }
    nd : uInt;                    { number of distance codes }
    var c : Array of uIntf;           { that many (total) code lengths }
    var bl : uIntf;               { literal desired/actual bit depth }
    var bd : uIntf;               { distance desired/actual bit depth }
var tl : pInflate_huft;           { literal/length tree result }
var td : pInflate_huft;           { distance tree result }
var hp : array of Inflate_huft;   { space for trees }
var z : z_stream                  { for messages }
     ) : int;

function inflate_trees_fixed (
    var bl : uInt;                { literal desired/actual bit depth }
    var bd : uInt;                { distance desired/actual bit depth }
    var tl : pInflate_huft;       { literal/length tree result }
    var td : pInflate_huft;       { distance tree result }
    var z : z_stream              { for memory allocation }
     ) : int;

{== iminfutil interface ==}

{ types and macros common to blocks and codes
  Copyright (C) 1995-1998 Mark Adler

   WARNING: this file should *not* be used by applications. It is
   part of the implementation of the compression library and is
   subject to change.

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

{ copy as much as possible from the sliding window to the output area }
function inflate_flush(var s : inflate_blocks_state;
                       var z : z_stream;
                       r : int) : int;

{ And'ing with mask[n] masks the lower n bits }
const
  inflate_mask : array[0..17-1] of uInt = (
    $0000,
    $0001, $0003, $0007, $000f, $001f, $003f, $007f, $00ff,
    $01ff, $03ff, $07ff, $0fff, $1fff, $3fff, $7fff, $ffff);

{procedure GRABBITS(j : int);}
{procedure DUMPBITS(j : int);}
{procedure NEEDBITS(j : int);}

{== imzinflate interface ==}

{  inflate.c -- zlib interface to inflate modules
   Copyright (C) 1995-1998 Mark Adler

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

function inflateInit(var z : z_stream) : int;

{    Initializes the internal stream state for decompression. The fields
   zalloc, zfree and opaque must be initialized before by the caller.  If
   zalloc and zfree are set to Z_NULL, inflateInit updates them to use default
   allocation functions.

     inflateInit returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_VERSION_ERROR if the zlib library version is incompatible
   with the version assumed by the caller.  msg is set to null if there is no
   error message. inflateInit does not perform any decompression: this will be
   done by inflate(). }

function inflateInit_(z : z_streamp;
                      const version : AnsiString;
                      stream_size : int) : int; overload;


function inflateInit2_(var z: z_stream;
                       w : int;
                       const version : AnsiString;
                       stream_size : int) : int;

function inflateInit2(var z: z_stream;
                       windowBits : int) : int;

{
     This is another version of inflateInit with an extra parameter. The
   fields next_in, avail_in, zalloc, zfree and opaque must be initialized
   before by the caller.

     The windowBits parameter is the base two logarithm of the maximum window
   size (the size of the history buffer).  It should be in the range 8..15 for
   this version of the library. The default value is 15 if inflateInit is used
   instead. If a compressed stream with a larger window size is given as
   input, inflate() will return with the error code Z_DATA_ERROR instead of
   trying to allocate a larger window.

      inflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
   memory, Z_STREAM_ERROR if a parameter is invalid (such as a negative
   memLevel). msg is set to null if there is no error message.  inflateInit2
   does not perform any decompression apart from reading the zlib header if
   present: this will be done by inflate(). (So next_in and avail_in may be
   modified, but next_out and avail_out are unchanged.)
}

function inflateEnd(var z : z_stream) : int;

{
   All dynamically allocated data structures for this stream are freed.
   This function discards any unprocessed input and does not flush any
   pending output.

     inflateEnd returns Z_OK if success, Z_STREAM_ERROR if the stream state
   was inconsistent. In the error case, msg may be set but then points to a
   static string (which must not be deallocated).
}

function inflateReset(var z : z_stream) : int;

{
   This function is equivalent to inflateEnd followed by inflateInit,
   but does not free and reallocate all the internal decompression state.
   The stream will keep attributes that may have been set by inflateInit2.

      inflateReset returns Z_OK if success, or Z_STREAM_ERROR if the source
   stream state was inconsistent (such as zalloc or state being NULL).
}

function inflate(var z : z_stream;
                 f : int) : int;
{
  inflate decompresses as much data as possible, and stops when the input
  buffer becomes empty or the output buffer becomes full. It may introduce
  some output latency (reading input without producing any output)
  except when forced to flush.

  The detailed semantics are as follows. inflate performs one or both of the
  following actions:

  - Decompress more input starting at next_in and update next_in and avail_in
    accordingly. If not all input can be processed (because there is not
    enough room in the output buffer), next_in is updated and processing
    will resume at this point for the next call of inflate().

  - Provide more output starting at next_out and update next_out and avail_out
    accordingly.  inflate() provides as much output as possible, until there
    is no more input data or no more space in the output buffer (see below
    about the flush parameter).

  Before the call of inflate(), the application should ensure that at least
  one of the actions is possible, by providing more input and/or consuming
  more output, and updating the next_* and avail_* values accordingly.
  The application can consume the uncompressed output when it wants, for
  example when the output buffer is full (avail_out == 0), or after each
  call of inflate(). If inflate returns Z_OK and with zero avail_out, it
  must be called again after making room in the output buffer because there
  might be more output pending.

    If the parameter flush is set to Z_SYNC_FLUSH, inflate flushes as much
  output as possible to the output buffer. The flushing behavior of inflate is
  not specified for values of the flush parameter other than Z_SYNC_FLUSH
  and Z_FINISH, but the current implementation actually flushes as much output
  as possible anyway.

    inflate() should normally be called until it returns Z_STREAM_END or an
  error. However if all decompression is to be performed in a single step
  (a single call of inflate), the parameter flush should be set to
  Z_FINISH. In this case all pending input is processed and all pending
  output is flushed; avail_out must be large enough to hold all the
  uncompressed data. (The size of the uncompressed data may have been saved
  by the compressor for this purpose.) The next operation on this stream must
  be inflateEnd to deallocate the decompression state. The use of Z_FINISH
  is never required, but can be used to inform inflate that a faster routine
  may be used for the single inflate() call.

     If a preset dictionary is needed at this point (see inflateSetDictionary
  below), inflate sets strm-adler to the adler32 checksum of the
  dictionary chosen by the compressor and returns Z_NEED_DICT; otherwise 
  it sets strm->adler to the adler32 checksum of all output produced
  so far (that is, total_out bytes) and returns Z_OK, Z_STREAM_END or
  an error code as described below. At the end of the stream, inflate()
  checks that its computed adler32 checksum is equal to that saved by the
  compressor and returns Z_STREAM_END only if the checksum is correct.

    inflate() returns Z_OK if some progress has been made (more input processed
  or more output produced), Z_STREAM_END if the end of the compressed data has
  been reached and all uncompressed output has been produced, Z_NEED_DICT if a
  preset dictionary is needed at this point, Z_DATA_ERROR if the input data was
  corrupted (input stream not conforming to the zlib format or incorrect
  adler32 checksum), Z_STREAM_ERROR if the stream structure was inconsistent
  (for example if next_in or next_out was NULL), Z_MEM_ERROR if there was not
  enough memory, Z_BUF_ERROR if no progress is possible or if there was not
  enough room in the output buffer when Z_FINISH is used. In the Z_DATA_ERROR
  case, the application may then call inflateSync to look for a good
  compression block.
}

function inflateSetDictionary(var z : z_stream;
                              dictionary : pBytef; {const array of byte}
                              dictLength : uInt) : int;

{
     Initializes the decompression dictionary from the given uncompressed byte
   sequence. This function must be called immediately after a call of inflate
   if this call returned Z_NEED_DICT. The dictionary chosen by the compressor
   can be determined from the Adler32 value returned by this call of
   inflate. The compressor and decompressor must use exactly the same
   dictionary (see deflateSetDictionary).

     inflateSetDictionary returns Z_OK if success, Z_STREAM_ERROR if a
   parameter is invalid (such as NULL dictionary) or the stream state is
   inconsistent, Z_DATA_ERROR if the given dictionary doesn't match the
   expected one (incorrect Adler32 value). inflateSetDictionary does not
   perform any decompression: this will be done by subsequent calls of
   inflate().
}

function inflateSync(var z : z_stream) : int;

{
  Skips invalid compressed data until a full flush point (see above the
  description of deflate with Z_FULL_FLUSH) can be found, or until all
  available input is skipped. No output is provided.

    inflateSync returns Z_OK if a full flush point has been found, Z_BUF_ERROR
  if no more input was provided, Z_DATA_ERROR if no flush point has been found,
  or Z_STREAM_ERROR if the stream structure was inconsistent. In the success
  case, the application may save the current current value of total_in which
  indicates where valid compressed data was found. In the error case, the
  application may repeatedly call inflateSync, providing more input each time,
  until success or end of the input data.
}

function inflateSyncPoint(var z : z_stream) : int;

{== imtrees interface ==}

{$T-}
{$define ORG_DEBUG}
{
  trees.c -- output deflated data using Huffman coding
  Copyright (C) 1995-1998 Jean-loup Gailly

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

{
 *  ALGORITHM
 *
 *      The "deflation" process uses several Huffman trees. The more
 *      common source values are represented by shorter bit sequences.
 *
 *      Each code tree is stored in a compressed form which is itself
 * a Huffman encoding of the lengths of all the code strings (in
 * ascending order by source values).  The actual code strings are
 * reconstructed from the lengths in the inflate process, as described
 * in the deflate specification.
 *
 *  REFERENCES
 *
 *      Deutsch, L.P.,"'Deflate' Compressed Data Format Specification".
 *      Available in ftp.uu.net:/pub/archiving/zip/doc/deflate-1.1.doc
 *
 *      Storer, James A.
 *          Data Compression:  Methods and Theory, pp. 49-50.
 *          Computer Science Press, 1988.  ISBN 0-7167-8156-5.
 *
 *      Sedgewick, R.
 *          Algorithms, p290.
 *          Addison-Wesley, 1983. ISBN 0-201-06672-6.
 }

{ ===========================================================================
  Internal compression state. }

const
  LENGTH_CODES = 29;
{ number of length codes, not counting the special END_BLOCK code }

  LITERALS = 256;
{ number of literal bytes 0..255 }

  L_CODES = (LITERALS+1+LENGTH_CODES);
{ number of Literal or Length codes, including the END_BLOCK code }

  D_CODES = 30;
{ number of distance codes }

  BL_CODES = 19;
{ number of codes used to transfer the bit lengths }

  HEAP_SIZE = (2*L_CODES+1);
{ maximum heap size }

  MAX_BITS = 15;
{ All codes must not exceed MAX_BITS bits }

const
  INIT_STATE =  42;
  BUSY_STATE =  113;
  FINISH_STATE = 666;
{ Stream status }


{ Data structure describing a single value and its code string. }
type
  ct_data_ptr = ^ct_data;
  ct_data = record
    fc : record
      case byte of
      0:(freq : ush);       { frequency count }
      1:(code : ush);       { bit string }
    end;
    dl : record
      case byte of
      0:(dad : ush);        { father node in Huffman tree }
      1:(len : ush);        { length of bit string }
    end;
  end;

{ Freq = fc.freq
 Code = fc.code
 Dad = dl.dad
 Len = dl.len }

type
  ltree_type = array[0..HEAP_SIZE-1] of ct_data;    { literal and length tree }
  dtree_type = array[0..2*D_CODES+1-1] of ct_data;  { distance tree }
  htree_type = array[0..2*BL_CODES+1-1] of ct_data;  { Huffman tree for bit lengths }
  { generic tree type }
  tree_type = array[0..(MaxInt div SizeOf(ct_data))-1] of ct_data;

  tree_ptr = ^tree_type;
  ltree_ptr = ^ltree_type;
  dtree_ptr = ^dtree_type;
  htree_ptr = ^htree_type;

type
  static_tree_desc_ptr = ^static_tree_desc;
  static_tree_desc =
         record
    {const} static_tree : tree_ptr;     { static tree or NIL }
    {const} extra_bits : pzIntfArray;   { extra bits for each code or NIL }
            extra_base : int;           { base index for extra_bits }
            elems : int;                { max number of elements in the tree }
            max_length : int;           { max bit length for the codes }
          end;

  tree_desc_ptr = ^tree_desc;
  tree_desc = record
    dyn_tree : tree_ptr;    { the dynamic tree }
    max_code : int;            { largest code with non zero frequency }
    stat_desc : static_tree_desc_ptr; { the corresponding static tree }
  end;

type
  Pos = ush;
  Posf = Pos; {FAR}
  IPos = uInt;

  pPosf = ^Posf;

  zPosfArray = array[0..(MaxInt div SizeOf(Posf))-1] of Posf;
  pzPosfArray = ^zPosfArray;

{ A Pos is an index in the character window. We use short instead of int to
  save space in the various tables. IPos is used only for parameter passing.}

type
  deflate_state_ptr = ^deflate_state;
  deflate_state = record
    strm : z_streamp;          { pointer back to this zlib stream }
    status : int;              { as the name implies }
    pending_buf : pzByteArray; { output still pending }
    pending_buf_size : ulg;    { size of pending_buf }
    pending_out : pBytef;      { next pending byte to output to the stream }
    pending : int;             { nb of bytes in the pending buffer }
    noheader : int;            { suppress zlib header and adler32 }
    data_type : Byte;          { UNKNOWN, BINARY or ASCII }
    method : Byte;             { STORED (for zip only) or DEFLATED }
    last_flush : int;          { value of flush param for previous deflate call }

                { used by deflate.pas: }

    w_size : uInt;             { LZ77 window size (32K by default) }
    w_bits : uInt;             { log2(w_size)  (8..16) }
    w_mask : uInt;             { w_size - 1 }

    window : pzByteArray;
    { Sliding window. Input bytes are read into the second half of the window,
      and move to the first half later to keep a dictionary of at least wSize
      bytes. With this organization, matches are limited to a distance of
      wSize-MAX_MATCH bytes, but this ensures that IO is always
      performed with a length multiple of the block size. Also, it limits
      the window size to 64K, which is quite useful on MSDOS.
      To do: use the user input buffer as sliding window. }

    window_size : ulg;
    { Actual size of window: 2*wSize, except when the user input buffer
      is directly used as sliding window. }

    prev : pzPosfArray;
    { Link to older string with same hash index. To limit the size of this
      array to 64K, this link is maintained only for the last 32K strings.
      An index in this array is thus a window index modulo 32K. }

    head : pzPosfArray;    { Heads of the hash chains or NIL. }

    ins_h : uInt;          { hash index of string to be inserted }
    hash_size : uInt;      { number of elements in hash table }
    hash_bits : uInt;      { log2(hash_size) }
    hash_mask : uInt;      { hash_size-1 }

    hash_shift : uInt;
    { Number of bits by which ins_h must be shifted at each input
      step. It must be such that after MIN_MATCH steps, the oldest
      byte no longer takes part in the hash key, that is:
        hash_shift * MIN_MATCH >= hash_bits     }

    block_start : long;
    { Window position at the beginning of the current output block. Gets
      negative when the window is moved backwards. }

    match_length : uInt;           { length of best match }
    prev_match : IPos;             { previous match }
    match_available : boolean;     { set if previous match exists }
    strstart : uInt;               { start of string to insert }
    match_start : uInt;            { start of matching string }
    lookahead : uInt;              { number of valid bytes ahead in window }

    prev_length : uInt;
    { Length of the best match at previous step. Matches not greater than this
      are discarded. This is used in the lazy match evaluation. }

    max_chain_length : uInt;
    { To speed up deflation, hash chains are never searched beyond this
      length.  A higher limit improves compression ratio but degrades the
      speed. }

    { moved to the end because Borland Pascal won't accept the following:
    max_lazy_match : uInt;
    max_insert_length : uInt absolute max_lazy_match;
    }

    level : int;    { compression level (1..9) }
    strategy : int; { favor or force Huffman coding}

    good_match : uInt;
    { Use a faster search when the previous match is longer than this }

    nice_match : int; { Stop searching when current match exceeds this }

                { used by trees.pas: }
    { Didn't use ct_data typedef below to supress compiler warning }
    dyn_ltree : ltree_type;    { literal and length tree }
    dyn_dtree : dtree_type;  { distance tree }
    bl_tree : htree_type;   { Huffman tree for bit lengths }

    l_desc : tree_desc;                { desc. for literal tree }
    d_desc : tree_desc;                { desc. for distance tree }
    bl_desc : tree_desc;               { desc. for bit length tree }

    bl_count : array[0..MAX_BITS+1-1] of ush;
    { number of codes at each bit length for an optimal tree }

    heap : array[0..2*L_CODES+1-1] of int; { heap used to build the Huffman trees }
    heap_len : int;                   { number of elements in the heap }
    heap_max : int;                   { element of largest frequency }
    { The sons of heap[n] are heap[2*n] and heap[2*n+1]. heap[0] is not used.
      The same heap array is used to build all trees. }

    depth : array[0..2*L_CODES+1-1] of uch;
    { Depth of each subtree used as tie breaker for trees of equal frequency }


    l_buf : puchfArray;       { buffer for literals or lengths }

    lit_bufsize : uInt;
    { Size of match buffer for literals/lengths.  There are 4 reasons for
      limiting lit_bufsize to 64K:
        - frequencies can be kept in 16 bit counters
        - if compression is not successful for the first block, all input
          data is still in the window so we can still emit a stored block even
          when input comes from standard input.  (This can also be done for
          all blocks if lit_bufsize is not greater than 32K.)
        - if compression is not successful for a file smaller than 64K, we can
          even emit a stored file instead of a stored block (saving 5 bytes).
          This is applicable only for zip (not gzip or zlib).
        - creating new Huffman trees less frequently may not provide fast
          adaptation to changes in the input data statistics. (Take for
          example a binary file with poorly compressible code followed by
          a highly compressible string table.) Smaller buffer sizes give
          fast adaptation but have of course the overhead of transmitting
          trees more frequently.
        - I can't count above 4 }


    last_lit : uInt;      { running index in l_buf }

    d_buf : pushfArray;
    { Buffer for distances. To simplify the code, d_buf and l_buf have
      the same number of elements. To use different lengths, an extra flag
      array would be necessary. }

    opt_len : ulg;        { bit length of current block with optimal trees }
    static_len : ulg;     { bit length of current block with static trees }
    compressed_len : ulg; { total bit length of compressed file }
    matches : uInt;       { number of string matches in current block }
    last_eob_len : int;   { bit length of EOB code for last block }

{$ifdef DEBUG}
    bits_sent : ulg;    { bit length of the compressed data }
{$endif}

    bi_buf : ush;
    { Output buffer. bits are inserted starting at the bottom (least
      significant bits). }

    bi_valid : int;
    { Number of valid bits in bi_buf.  All bits above the last valid bit
      are always zero. }

    case byte of
    0:(max_lazy_match : uInt);
    { Attempt to find a better match only when the current match is strictly
      smaller than this value. This mechanism is used only for compression
      levels >= 4. }

    1:(max_insert_length : uInt);
    { Insert new strings in the hash table only if the match length is not
      greater than this length. This saves time but degrades compression.
      max_insert_length is used only for compression levels <= 3. }
  end;

procedure _tr_init (var s : deflate_state);

function _tr_tally (var s : deflate_state;
                    dist : unsigned;
                    lc : unsigned) : boolean;

function _tr_flush_block (var s : deflate_state;
                          buf : pcharf;
                          stored_len : ulg;
			  eof : boolean) : ulg;

procedure _tr_align(var s : deflate_state);

procedure _tr_stored_block(var s : deflate_state;
                           buf : pcharf;
                           stored_len : ulg;
                           eof : boolean);

implementation

{== imzutil implementation ==}

procedure zmemcpy(destp : pBytef; sourcep : pBytef; len : uInt);
begin
  Move(sourcep^, destp^, len);
end;

function zmemcmp(s1p, s2p : pBytef; len : uInt) : int;
var
  j : uInt;
  source,
  dest : pBytef;
begin
  source := s1p;
  dest := s2p;
  for j := 0 to pred(len) do
  begin
    if (source^ <> dest^) then
    begin
      zmemcmp := 2*Ord(source^ > dest^)-1;
      exit;
    end;
    Inc(source);
    Inc(dest);
  end;
  zmemcmp := 0;
end;

procedure zmemzero(destp : pBytef; len : uInt);
begin
  FillChar(destp^, len, 0);
end;

procedure zcfree(opaque : voidpf; ptr : voidpf);
begin
  FreeMem(ptr);
end;

function zcalloc (opaque : voidpf; items : uInt; size : uInt) : voidpf;
var
  p : voidpf;
  memsize : uLong;
begin
  memsize := uLong(items) * size;
  GetMem(p, memsize);
  zcalloc := p;
end;

{== impaszlib implementation ==}

function deflateInit_(var Stream: z_stream; Level: LongInt; const Version: AnsiString;
  Stream_size: LongInt): LongInt;
begin
  Result := deflateInit_(@Stream, Level, Version, Stream_size);
end;

function inflateInit_(var Stream: z_stream; const Version: AnsiString;
  Stream_size: Longint): LongInt;
begin
  Result := inflateInit_(@Stream, Version, Stream_size);
end;

function zError(err : int) : string;
begin
  zError := string(z_errmsg[Z_NEED_DICT-err]);
end;

function zlibVersion : AnsiString;
begin
  zlibVersion := ZLIB_VERSION;
end;

procedure z_error(m : string);
begin
  //WriteLn(output, m);
  //Write('Zlib - Halt...');
  //ReadLn;
  raise Exception.Create('ZLib error: ' + string(m));
end;

{$IFDEF DEBUG}
procedure Trace(x : string);
begin
  //WriteLn(x);
end;

procedure Tracev(x : string);
begin
 if (z_verbose>0) then
   ; //WriteLn(x);
end;

procedure Tracevv(x : string);
begin
  if (z_verbose>1) then
    ; //WriteLn(x);
end;

procedure Tracevvv(x : string);
begin
  if (z_verbose>2) then
    ; //WriteLn(x);
end;

procedure Tracec(c : boolean; x : string);
begin
  if (z_verbose>0) and (c) then
    ; //WriteLn(x);
end;

procedure Tracecv(c : boolean; x : string);
begin
  if (z_verbose>1) and c then
    ; //WriteLn(x);
end;
{$ENDIF}

function ZALLOC (var strm : z_stream; items : uInt; size : uInt) : voidpf;
begin
  ZALLOC := strm.zalloc(strm.opaque, items, size);
end;

procedure ZFREE (var strm : z_stream; ptr : voidpf);
begin
  strm.zfree(strm.opaque, ptr);
end;

procedure TRY_FREE (var strm : z_stream; ptr : voidpf);
begin
  {if @strm <> Z_NULL then}
    strm.zfree(strm.opaque, ptr);
end;

{== imzdeflate implementation ==}

{  ===========================================================================
   Function prototypes. }

type
   block_state = (
    need_more,      { block not completed, need more input or more output }
    block_done,     { block flush performed }
    finish_started, { finish started, need only more output at next deflate }
    finish_done);   { finish done, accept no more input or output }

{ Compression function. Returns the block state after the call. }
type
  compress_func = function(var s : deflate_state; flush : int) : block_state;

procedure fill_window(var s : deflate_state); forward;
function deflate_stored(var s : deflate_state; flush : int) : block_state;   forward;
function deflate_fast(var s : deflate_state; flush : int) : block_state;   forward;
function deflate_slow(var s : deflate_state; flush : int) : block_state;   forward;
procedure lm_init(var s : deflate_state); forward;

procedure putShortMSB(var s : deflate_state; b : uInt); forward;
procedure  flush_pending (var strm : z_stream); forward;
function read_buf(strm : z_streamp;
                  buf : pBytef;
                  size : unsigned) : int; forward;
{$ifdef ASMV}
procedure match_init; { asm code initialization }
function longest_match(var deflate_state; cur_match : IPos) : uInt; forward;
{$else}
function longest_match(var s : deflate_state; cur_match : IPos) : uInt;
  forward;
{$endif}

{$ifdef DEBUG}
procedure check_match(var s : deflate_state;
                      start, match : IPos;
                      length : int); forward;
{$endif}

{  ==========================================================================
  local data }

const
  ZNIL = 0;
{ Tail of hash chains }

const
  TOO_FAR = 4096;
{ Matches of length 3 are discarded if their distance exceeds TOO_FAR }

const
  MIN_LOOKAHEAD = (MAX_MATCH+MIN_MATCH+1);
{ Minimum amount of lookahead, except at the end of the input file.
  See deflate.c for comments about the MIN_MATCH+1. }

{macro MAX_DIST(var s : deflate_state) : uInt;
begin
  MAX_DIST := (s.w_size - MIN_LOOKAHEAD);
end;
  In order to simplify the code, particularly on 16 bit machines, match
  distances are limited to MAX_DIST instead of WSIZE. }


{ Values for max_lazy_match, good_match and max_chain_length, depending on
  the desired pack level (0..9). The values given below have been tuned to
  exclude worst case performance for pathological files. Better values may be
  found for specific files. }

type
  config = record
   good_length : ush; { reduce lazy search above this match length }
   max_lazy : ush;    { do not perform lazy search above this match length }
   nice_length : ush; { quit search above this match length }
   max_chain : ush;
   func : compress_func;
  end;

const
  configuration_table : array[0..10-1] of config = (
{      good lazy nice chain }
{0} (good_length:0;  max_lazy:0;   nice_length:0;   max_chain:0;    func:deflate_stored),  { store only }
{1} (good_length:4;  max_lazy:4;   nice_length:8;   max_chain:4;    func:deflate_fast), { maximum speed, no lazy matches }
{2} (good_length:4;  max_lazy:5;   nice_length:16;  max_chain:8;    func:deflate_fast),
{3} (good_length:4;  max_lazy:6;   nice_length:32;  max_chain:32;   func:deflate_fast),

{4} (good_length:4;  max_lazy:4;   nice_length:16;  max_chain:16;   func:deflate_slow),  { lazy matches }
{5} (good_length:8;  max_lazy:16;  nice_length:32;  max_chain:32;   func:deflate_slow),
{6} (good_length:8;  max_lazy:16;  nice_length:128; max_chain:128;  func:deflate_slow),
{7} (good_length:8;  max_lazy:32;  nice_length:128; max_chain:256;  func:deflate_slow),
{8} (good_length:32; max_lazy:128; nice_length:258; max_chain:1024; func:deflate_slow),
{9} (good_length:32; max_lazy:258; nice_length:258; max_chain:4096; func:deflate_slow)); { maximum compression }

{ Note: the deflate() code requires max_lazy >= MIN_MATCH and max_chain >= 4
  For deflate_fast() (levels <= 3) good is ignored and lazy has a different
  meaning. }

const
  EQUAL = 0;
{ result of memcmp for equal strings }

{ ==========================================================================
  Update a hash value with the given input byte
  IN  assertion: all calls to to UPDATE_HASH are made with consecutive
     input characters, so that a running hash key can be computed from the
     previous key instead of complete recalculation each time.

macro UPDATE_HASH(s,h,c)
   h := (( (h) shl s^.hash_shift) xor (c)) and s^.hash_mask;
}

{ ===========================================================================
  Insert string str in the dictionary and set match_head to the previous head
  of the hash chain (the most recent string with same hash key). Return
  the previous length of the hash chain.
  If this file is compiled with -DFASTEST, the compression level is forced
  to 1, and no hash chains are maintained.
  IN  assertion: all calls to to INSERT_STRING are made with consecutive
     input characters and the first MIN_MATCH bytes of str are valid
     (except for the last MIN_MATCH-1 bytes of the input file). }

procedure INSERT_STRING(var s : deflate_state;
                        str : uInt;
                        var match_head : IPos);
begin
{$ifdef FASTEST}
   {UPDATE_HASH(s, s.ins_h, s.window[(str) + (MIN_MATCH-1)])}
    s.ins_h := ((s.ins_h shl s.hash_shift) xor
                 (s.window^[(str) + (MIN_MATCH-1)])) and s.hash_mask;
    match_head := s.head[s.ins_h]
    s.head[s.ins_h] := Pos(str);
{$else}
   {UPDATE_HASH(s, s.ins_h, s.window[(str) + (MIN_MATCH-1)])}
    s.ins_h := ((s.ins_h shl s.hash_shift) xor
                 (s.window^[(str) + (MIN_MATCH-1)])) and s.hash_mask;

    match_head := s.head^[s.ins_h];
    s.prev^[(str) and s.w_mask] := match_head;
    s.head^[s.ins_h] := Pos(str);
{$endif}
end;

{  =========================================================================
  Initialize the hash table (avoiding 64K overflow for 16 bit systems).
  prev[] will be initialized on the fly.

macro CLEAR_HASH(s)
    s^.head[s^.hash_size-1] := ZNIL;
    zmemzero(pBytef(s^.head), unsigned(s^.hash_size-1)*sizeof(s^.head^[0]));
}

{  ======================================================================== }

function deflateInit2_(var strm : z_stream;
                       level : int;
                       method : int;
                       windowBits : int;
                       memLevel : int;
                       strategy : int;
                       const version : AnsiString;
                       stream_size : int) : int;
var
  s : deflate_state_ptr;
  noheader : int;

  overlay : pushfArray;
  { We overlay pending_buf and d_buf+l_buf. This works since the average
    output size for (length,distance) codes is <= 24 bits. }
begin
  noheader := 0;
  if (version  =  '') or (version[1] <> ZLIB_VERSION[1]) or
     (stream_size <> sizeof(z_stream)) then
  begin
    deflateInit2_ := Z_VERSION_ERROR;
    exit;
  end;
  {
  if (strm = Z_NULL) then
  begin
    deflateInit2_ := Z_STREAM_ERROR;
    exit;
  end;
  }
  { SetLength(strm.msg, 255); }
  strm.msg := '';
  if not Assigned(strm.zalloc) then
  begin
    {$IFDEF FPC}  strm.zalloc := @zcalloc;  {$ELSE}
    strm.zalloc := zcalloc;
    {$ENDIF}
    strm.opaque := voidpf(0);
  end;
  if not Assigned(strm.zfree) then
    {$IFDEF FPC}  strm.zfree := @zcfree;  {$ELSE}
    strm.zfree := zcfree;
    {$ENDIF}

  if (level  =  Z_DEFAULT_COMPRESSION) then
    level := 6;
{$ifdef FASTEST}
    level := 1;
{$endif}

  if (windowBits < 0) then { undocumented feature: suppress zlib header }
  begin
    noheader := 1;
    windowBits := -windowBits;
  end;
  if (memLevel < 1) or (memLevel > MAX_MEM_LEVEL) or (method <> Z_DEFLATED)
    or (windowBits < 8) or (windowBits > 15) or (level < 0)
    or (level > 9) or (strategy < 0) or (strategy > Z_HUFFMAN_ONLY) then
  begin
    deflateInit2_ := Z_STREAM_ERROR;
    exit;
  end;

  s := deflate_state_ptr (ZALLOC(strm, 1, sizeof(deflate_state)));
  if (s = Z_NULL) then
  begin
    deflateInit2_ := Z_MEM_ERROR;
    exit;
  end;
  strm.state := pInternal_state(s);
  s^.strm := @strm;

  s^.noheader := noheader;
  s^.w_bits := windowBits;
  s^.w_size := 1 shl s^.w_bits;
  s^.w_mask := s^.w_size - 1;

  s^.hash_bits := memLevel + 7;
  s^.hash_size := 1 shl s^.hash_bits;
  s^.hash_mask := s^.hash_size - 1;
  s^.hash_shift :=  ((s^.hash_bits+MIN_MATCH-1) div MIN_MATCH);

  s^.window := pzByteArray (ZALLOC(strm, s^.w_size, 2*sizeof(Byte)));
  s^.prev   := pzPosfArray (ZALLOC(strm, s^.w_size, sizeof(Pos)));
  s^.head   := pzPosfArray (ZALLOC(strm, s^.hash_size, sizeof(Pos)));

  s^.lit_bufsize := 1 shl (memLevel + 6); { 16K elements by default }

  overlay := pushfArray (ZALLOC(strm, s^.lit_bufsize, sizeof(ush)+2));
  s^.pending_buf := pzByteArray (overlay);
  s^.pending_buf_size := ulg(s^.lit_bufsize) * (sizeof(ush)+Long(2));

  if (s^.window = Z_NULL) or (s^.prev = Z_NULL) or (s^.head = Z_NULL)
   or (s^.pending_buf = Z_NULL) then
  begin
    {ERR_MSG(Z_MEM_ERROR);}
    strm.msg := z_errmsg[z_errbase-Z_MEM_ERROR];
    deflateEnd (strm);
    deflateInit2_ := Z_MEM_ERROR;
    exit;
  end;
  s^.d_buf := pushfArray( @overlay^[s^.lit_bufsize div sizeof(ush)] );
  s^.l_buf := puchfArray( @s^.pending_buf^[(1+sizeof(ush))*s^.lit_bufsize] );

  s^.level := level;
  s^.strategy := strategy;
  s^.method := Byte(method);

  deflateInit2_ := deflateReset(strm);
end;

{  ========================================================================= }

function deflateInit2(var strm : z_stream;
                      level : int;
                      method : int;
                      windowBits : int;
                      memLevel : int;
                      strategy : int) : int;
{ a macro }
begin
  deflateInit2 := deflateInit2_(strm, level, method, windowBits,
                   memLevel, strategy, ZLIB_VERSION, sizeof(z_stream));
end;

{  ========================================================================= }

function deflateInit_(strm : z_streamp;
                      level : int;
                      const version : AnsiString;
                      stream_size : int) : int;
begin
  if (strm = Z_NULL) then
    deflateInit_ := Z_STREAM_ERROR
  else
    deflateInit_ := deflateInit2_(strm^, level, Z_DEFLATED, MAX_WBITS,
                   DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY, version, stream_size);
  { To do: ignore strm^.next_in if we use it as window }
end;

{  ========================================================================= }

function deflateInit(var strm : z_stream; level : int) : int;
{ deflateInit is a macro to allow checking the zlib version
  and the compiler's view of z_stream: }
begin
  deflateInit := deflateInit2_(strm, level, Z_DEFLATED, MAX_WBITS,
         DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY, ZLIB_VERSION, sizeof(z_stream));
end;

{  ======================================================================== }
function deflateSetDictionary (var strm : z_stream;
                               dictionary : pBytef;
                               dictLength : uInt) : int;
var
  s : deflate_state_ptr;
  length : uInt;
  n : uInt;
  hash_head : IPos;
var
  MAX_DIST : uInt;  {macro}
begin
  length := dictLength;
  hash_head := 0;

  if {(@strm  =  Z_NULL) or}
     (strm.state  =  Z_NULL) or (dictionary  =  Z_NULL)
    or (deflate_state_ptr(strm.state)^.status <> INIT_STATE) then
  begin
    deflateSetDictionary := Z_STREAM_ERROR;
    exit;
  end;

  s := deflate_state_ptr(strm.state);
  strm.adler := adler32(strm.adler, dictionary, dictLength);

  if (length < MIN_MATCH) then
  begin
    deflateSetDictionary := Z_OK;
    exit;
  end;
  MAX_DIST := (s^.w_size - MIN_LOOKAHEAD);
  if (length > MAX_DIST) then
  begin
    length := MAX_DIST;
{$ifndef USE_DICT_HEAD}
    Inc(dictionary, dictLength - length);  { use the tail of the dictionary }
{$endif}
  end;

  zmemcpy( pBytef(s^.window), dictionary, length);
  s^.strstart := length;
  s^.block_start := long(length);

  { Insert all strings in the hash table (except for the last two bytes).
    s^.lookahead stays null, so s^.ins_h will be recomputed at the next
    call of fill_window. }

  s^.ins_h := s^.window^[0];
  {UPDATE_HASH(s, s^.ins_h, s^.window[1]);}
  s^.ins_h := ((s^.ins_h shl s^.hash_shift) xor (s^.window^[1]))
              and s^.hash_mask;

  for n := 0 to length - MIN_MATCH do
  begin
    INSERT_STRING(s^, n, hash_head);
  end;
  {if (hash_head <> 0) then
    hash_head := 0;  - to make compiler happy }
  deflateSetDictionary := Z_OK;
end;

{  ======================================================================== }
function deflateReset (var strm : z_stream) : int;
var
  s : deflate_state_ptr;
begin
  if {(@strm = Z_NULL) or}
   (strm.state = Z_NULL)
   or (not Assigned(strm.zalloc)) or (not Assigned(strm.zfree)) then
  begin
    deflateReset := Z_STREAM_ERROR;
    exit;
  end;

  strm.total_out := 0;
  strm.total_in := 0;
  strm.msg := '';      { use zfree if we ever allocate msg dynamically }
  strm.data_type := Z_UNKNOWN;

  s := deflate_state_ptr(strm.state);
  s^.pending := 0;
  s^.pending_out := pBytef(s^.pending_buf);

  if (s^.noheader < 0) then
  begin
    s^.noheader := 0; { was set to -1 by deflate(..., Z_FINISH); }
  end;
  if s^.noheader <> 0 then
    s^.status := BUSY_STATE
  else
    s^.status := INIT_STATE;
  strm.adler := 1;
  s^.last_flush := Z_NO_FLUSH;

  _tr_init(s^);
  lm_init(s^);

  deflateReset := Z_OK;
end;

{  ======================================================================== }
function deflateParams(var strm : z_stream;
                       level : int;
                       strategy : int) : int;
var
  s : deflate_state_ptr;
  func : compress_func;
  err : int;
begin
  err := Z_OK;
  if {(@strm  =  Z_NULL) or} (strm.state  =  Z_NULL) then
  begin
    deflateParams := Z_STREAM_ERROR;
    exit;
  end;

  s := deflate_state_ptr(strm.state);

  if (level = Z_DEFAULT_COMPRESSION) then
  begin
    level := 6;
  end;
  if (level < 0) or (level > 9) or (strategy < 0)
  or (strategy > Z_HUFFMAN_ONLY) then
  begin
    deflateParams := Z_STREAM_ERROR;
    exit;
  end;
  func := configuration_table[s^.level].func;

  if (@func <> @configuration_table[level].func)
    and (strm.total_in <> 0) then
  begin
      { Flush the last buffer: }
      err := deflate(strm, Z_PARTIAL_FLUSH);
  end;
  if (s^.level <> level) then
  begin
    s^.level := level;
    s^.max_lazy_match   := configuration_table[level].max_lazy;
    s^.good_match       := configuration_table[level].good_length;
    s^.nice_match       := configuration_table[level].nice_length;
    s^.max_chain_length := configuration_table[level].max_chain;
  end;
  s^.strategy := strategy;
  deflateParams := err;
end;

{ =========================================================================
  Put a short in the pending buffer. The 16-bit value is put in MSB order.
  IN assertion: the stream state is correct and there is enough room in
  pending_buf. }

procedure putShortMSB (var s : deflate_state; b : uInt);
begin
  s.pending_buf^[s.pending] := Byte(b shr 8);
  Inc(s.pending);
  s.pending_buf^[s.pending] := Byte(b and $ff);
  Inc(s.pending);
end;

{ =========================================================================
  Flush as much pending output as possible. All deflate() output goes
  through this function so some applications may wish to modify it
  to avoid allocating a large strm^.next_out buffer and copying into it.
  (See also read_buf()). }

procedure flush_pending(var strm : z_stream);
var
  len : unsigned;
  s : deflate_state_ptr;
begin
  s := deflate_state_ptr(strm.state);
  len := s^.pending;

  if (len > strm.avail_out) then
    len := strm.avail_out;
  if (len = 0) then
    exit;

  zmemcpy(strm.next_out, s^.pending_out, len);
  Inc(strm.next_out, len);
  Inc(s^.pending_out, len);
  Inc(strm.total_out, len);
  Dec(strm.avail_out, len);
  Dec(s^.pending, len);
  if (s^.pending = 0) then
  begin
    s^.pending_out := pBytef(s^.pending_buf);
  end;
end;

{ ========================================================================= }
function deflate (var strm : z_stream; flush : int) : int;
var
  old_flush : int; { value of flush param for previous deflate call }
  s : deflate_state_ptr;
var
  header : uInt;
  level_flags : uInt;
var
  bstate : block_state;
begin
  if {(@strm = Z_NULL) or} (strm.state = Z_NULL)
    or (flush > Z_FINISH) or (flush < 0) then
  begin
    deflate := Z_STREAM_ERROR;
    exit;
  end;
  s := deflate_state_ptr(strm.state);

  if (strm.next_out = Z_NULL) or
     ((strm.next_in = Z_NULL) and (strm.avail_in <> 0)) or
     ((s^.status = FINISH_STATE) and (flush <> Z_FINISH)) then
  begin
    {ERR_RETURN(strm^, Z_STREAM_ERROR);}
    strm.msg := z_errmsg[z_errbase - Z_STREAM_ERROR];
    deflate := Z_STREAM_ERROR;
    exit;
  end;
  if (strm.avail_out = 0) then
  begin
    {ERR_RETURN(strm^, Z_BUF_ERROR);}
    strm.msg := z_errmsg[z_errbase - Z_BUF_ERROR];
    deflate := Z_BUF_ERROR;
    exit;
  end;

  s^.strm := @strm; { just in case }
  old_flush := s^.last_flush;
  s^.last_flush := flush;

  { Write the zlib header }
  if (s^.status = INIT_STATE) then
  begin

    header := (Z_DEFLATED + ((s^.w_bits-8) shl 4)) shl 8;
    level_flags := (s^.level-1) shr 1;

    if (level_flags > 3) then
      level_flags := 3;
    header := header or (level_flags shl 6);
    if (s^.strstart <> 0) then
      header := header or PRESET_DICT;
    Inc(header, 31 - (header mod 31));

    s^.status := BUSY_STATE;
    putShortMSB(s^, header);

    { Save the adler32 of the preset dictionary: }
    if (s^.strstart <> 0) then
    begin
      putShortMSB(s^, uInt(strm.adler shr 16));
      putShortMSB(s^, uInt(strm.adler and $ffff));
    end;
    strm.adler := long(1);
  end;

  { Flush as much pending output as possible }
  if (s^.pending <> 0) then
  begin
    flush_pending(strm);
    if (strm.avail_out = 0) then
    begin
      { Since avail_out is 0, deflate will be called again with
	more output space, but possibly with both pending and
	avail_in equal to zero. There won't be anything to do,
	but this is not an error situation so make sure we
	return OK instead of BUF_ERROR at next call of deflate: }

      s^.last_flush := -1;
      deflate := Z_OK;
      exit;
    end;

  { Make sure there is something to do and avoid duplicate consecutive
    flushes. For repeated and useless calls with Z_FINISH, we keep
    returning Z_STREAM_END instead of Z_BUFF_ERROR. }

  end
  else
    if (strm.avail_in = 0) and (flush <= old_flush)
      and (flush <> Z_FINISH) then
    begin
      {ERR_RETURN(strm^, Z_BUF_ERROR);}
      strm.msg := z_errmsg[z_errbase - Z_BUF_ERROR];
      deflate := Z_BUF_ERROR;
      exit;
    end;

  { User must not provide more input after the first FINISH: }
  if (s^.status = FINISH_STATE) and (strm.avail_in <> 0) then
  begin
    {ERR_RETURN(strm^, Z_BUF_ERROR);}
    strm.msg := z_errmsg[z_errbase - Z_BUF_ERROR];
    deflate := Z_BUF_ERROR;
    exit;
  end;

  { Start a new block or continue the current one. }
  if (strm.avail_in <> 0) or (s^.lookahead <> 0)
    or ((flush <> Z_NO_FLUSH) and (s^.status <> FINISH_STATE)) then
  begin
    bstate := configuration_table[s^.level].func(s^, flush);

    if (bstate = finish_started) or (bstate = finish_done) then
      s^.status := FINISH_STATE;

    if (bstate = need_more) or (bstate = finish_started) then
    begin
      if (strm.avail_out = 0) then
        s^.last_flush := -1; { avoid BUF_ERROR next call, see above }

      deflate := Z_OK;
      exit;
      { If flush != Z_NO_FLUSH && avail_out == 0, the next call
	of deflate should use the same flush parameter to make sure
	that the flush is complete. So we don't have to output an
	empty block here, this will be done at next call. This also
	ensures that for a very small output buffer, we emit at most
	 one empty block. }
    end;
    if (bstate = block_done) then
    begin
      if (flush = Z_PARTIAL_FLUSH) then
        _tr_align(s^)
      else
      begin  { FULL_FLUSH or SYNC_FLUSH }
        _tr_stored_block(s^, pcharf(NIL), Long(0), FALSE);
        { For a full flush, this empty block will be recognized
          as a special marker by inflate_sync(). }

        if (flush = Z_FULL_FLUSH) then
        begin
          {macro CLEAR_HASH(s);}             { forget history }
          s^.head^[s^.hash_size-1] := ZNIL;
          zmemzero(pBytef(s^.head), unsigned(s^.hash_size-1)*sizeof(s^.head^[0]));
        end;
      end;

      flush_pending(strm);
      if (strm.avail_out = 0) then
      begin
        s^.last_flush := -1; { avoid BUF_ERROR at next call, see above }
	deflate := Z_OK;
        exit;
      end;

    end;
  end;
  {$IFDEF DEBUG}
  Assert(strm.avail_out > 0, 'bug2');
  {$ENDIF}
  if (flush <> Z_FINISH) then
  begin
    deflate := Z_OK;
    exit;
  end;

  if (s^.noheader <> 0) then
  begin
    deflate := Z_STREAM_END;
    exit;
  end;

  { Write the zlib trailer (adler32) }
  putShortMSB(s^, uInt(strm.adler shr 16));
  putShortMSB(s^, uInt(strm.adler and $ffff));
  flush_pending(strm);
  { If avail_out is zero, the application will call deflate again
    to flush the rest. }

  s^.noheader := -1; { write the trailer only once! }
  if s^.pending <> 0 then
    deflate := Z_OK
  else
    deflate := Z_STREAM_END;
end;

{ ========================================================================= }
function deflateEnd (var strm : z_stream) : int;
var
  status : int;
  s : deflate_state_ptr;
begin
  if {(@strm = Z_NULL) or} (strm.state = Z_NULL) then
  begin
    deflateEnd := Z_STREAM_ERROR;
    exit;
  end;

  s := deflate_state_ptr(strm.state);
  status := s^.status;
  if (status <> INIT_STATE) and (status <> BUSY_STATE) and
     (status <> FINISH_STATE) then
  begin
    deflateEnd := Z_STREAM_ERROR;
    exit;
  end;

  { Deallocate in reverse order of allocations: }
  TRY_FREE(strm, s^.pending_buf);
  TRY_FREE(strm, s^.head);
  TRY_FREE(strm, s^.prev);
  TRY_FREE(strm, s^.window);

  ZFREE(strm, s);
  strm.state := Z_NULL;

  if status = BUSY_STATE then
    deflateEnd := Z_DATA_ERROR
  else
    deflateEnd := Z_OK;
end;

{ =========================================================================
  Copy the source state to the destination state.
  To simplify the source, this is not supported for 16-bit MSDOS (which
  doesn't have enough memory anyway to duplicate compression states). }


{ ========================================================================= }
function deflateCopy (dest, source : z_streamp) : int;
{$ifndef MAXSEG_64K}
var
  ds : deflate_state_ptr;
  ss : deflate_state_ptr;
  overlay : pushfArray;
{$endif}
begin
{$ifdef MAXSEG_64K}
  deflateCopy := Z_STREAM_ERROR;
  exit;
{$else}

  if (source = Z_NULL) or (dest = Z_NULL) or (source^.state = Z_NULL) then
  begin
    deflateCopy := Z_STREAM_ERROR;
    exit;
  end;
  ss := deflate_state_ptr(source^.state);
  dest^ := source^;

  ds := deflate_state_ptr( ZALLOC(dest^, 1, sizeof(deflate_state)) );
  if (ds = Z_NULL) then
  begin
    deflateCopy := Z_MEM_ERROR;
    exit;
  end;
  dest^.state := pInternal_state(ds);
  ds^ := ss^;
  ds^.strm := dest;

  ds^.window := pzByteArray ( ZALLOC(dest^, ds^.w_size, 2*sizeof(Byte)) );
  ds^.prev   := pzPosfArray ( ZALLOC(dest^, ds^.w_size, sizeof(Pos)) );
  ds^.head   := pzPosfArray ( ZALLOC(dest^, ds^.hash_size, sizeof(Pos)) );
  overlay := pushfArray ( ZALLOC(dest^, ds^.lit_bufsize, sizeof(ush)+2) );
  ds^.pending_buf := pzByteArray ( overlay );

  if (ds^.window = Z_NULL) or (ds^.prev = Z_NULL) or (ds^.head = Z_NULL)
     or (ds^.pending_buf = Z_NULL) then
  begin
    deflateEnd (dest^);
    deflateCopy := Z_MEM_ERROR;
    exit;
  end;
  { following zmemcpy do not work for 16-bit MSDOS }
  zmemcpy(pBytef(ds^.window), pBytef(ss^.window), ds^.w_size * 2 * sizeof(Byte));
  zmemcpy(pBytef(ds^.prev), pBytef(ss^.prev), ds^.w_size * sizeof(Pos));
  zmemcpy(pBytef(ds^.head), pBytef(ss^.head), ds^.hash_size * sizeof(Pos));
  zmemcpy(pBytef(ds^.pending_buf), pBytef(ss^.pending_buf), uInt(ds^.pending_buf_size));

  ds^.pending_out := @ds^.pending_buf^[ptr2int(ss^.pending_out) - ptr2int(ss^.pending_buf)];
  ds^.d_buf := pushfArray (@overlay^[ds^.lit_bufsize div sizeof(ush)] );
  ds^.l_buf := puchfArray (@ds^.pending_buf^[(1+sizeof(ush))*ds^.lit_bufsize]);

  ds^.l_desc.dyn_tree := tree_ptr(@ds^.dyn_ltree);
  ds^.d_desc.dyn_tree := tree_ptr(@ds^.dyn_dtree);
  ds^.bl_desc.dyn_tree := tree_ptr(@ds^.bl_tree);

  deflateCopy := Z_OK;
{$endif}
end;


{ ===========================================================================
  Read a new buffer from the current input stream, update the adler32
  and total number of bytes read.  All deflate() input goes through
  this function so some applications may wish to modify it to avoid
  allocating a large strm^.next_in buffer and copying from it.
  (See also flush_pending()). }

function read_buf(strm : z_streamp; buf : pBytef; size : unsigned) : int;
var
  len : unsigned;
begin
  len := strm^.avail_in;

  if (len > size) then
    len := size;
  if (len = 0) then
  begin
    read_buf := 0;
    exit;
  end;

  Dec(strm^.avail_in, len);

  if deflate_state_ptr(strm^.state)^.noheader = 0 then
  begin
    strm^.adler := adler32(strm^.adler, strm^.next_in, len);
  end;
  zmemcpy(buf, strm^.next_in, len);
  Inc(strm^.next_in, len);
  Inc(strm^.total_in, len);

  read_buf := int(len);
end;

{ ===========================================================================
  Initialize the "longest match" routines for a new zlib stream }

procedure lm_init (var s : deflate_state);
begin
  s.window_size := ulg( uLong(2)*s.w_size);

  {macro CLEAR_HASH(s);}
  s.head^[s.hash_size-1] := ZNIL;
  zmemzero(pBytef(s.head), unsigned(s.hash_size-1)*sizeof(s.head^[0]));

  { Set the default configuration parameters: }

  s.max_lazy_match   := configuration_table[s.level].max_lazy;
  s.good_match       := configuration_table[s.level].good_length;
  s.nice_match       := configuration_table[s.level].nice_length;
  s.max_chain_length := configuration_table[s.level].max_chain;

  s.strstart := 0;
  s.block_start := long(0);
  s.lookahead := 0;
  s.prev_length := MIN_MATCH-1;
  s.match_length := MIN_MATCH-1;
  s.match_available := FALSE;
  s.ins_h := 0;
{$ifdef ASMV}
  match_init; { initialize the asm code }
{$endif}
end;

{ ===========================================================================
  Set match_start to the longest match starting at the given string and
  return its length. Matches shorter or equal to prev_length are discarded,
  in which case the result is equal to prev_length and match_start is
  garbage.
  IN assertions: cur_match is the head of the hash chain for the current
    string (strstart) and its distance is <= MAX_DIST, and prev_length >= 1
  OUT assertion: the match length is not greater than s^.lookahead. }


{$ifndef ASMV}
{ For 80x86 and 680x0, an optimized version will be provided in match.asm or
  match.S. The code will be functionally equivalent. }

{$ifndef FASTEST}

function longest_match(var s : deflate_state;
                       cur_match : IPos  { current match }
                       ) : uInt;
label
  nextstep;
var
  chain_length : unsigned;    { max hash chain length }
  {register} scan : pBytef;   { current string }
  {register} match : pBytef;  { matched string }
  {register} len : int;       { length of current match }
  best_len : int;             { best match length so far }
  nice_match : int;           { stop if match long enough }
  limit : IPos;

  prev : pzPosfArray;
  wmask : uInt;
{$ifdef UNALIGNED_OK}
  {register} strend : pBytef;
  {register} scan_start : ush;
  {register} scan_end : ush;
{$else}
  {register} strend : pBytef;
  {register} scan_end1 : Byte;
  {register} scan_end : Byte;
{$endif}
var
  MAX_DIST : uInt;
begin
  chain_length := s.max_chain_length; { max hash chain length }
  scan := @(s.window^[s.strstart]);
  best_len := s.prev_length;              { best match length so far }
  nice_match := s.nice_match;             { stop if match long enough }


  MAX_DIST := s.w_size - MIN_LOOKAHEAD;
{In order to simplify the code, particularly on 16 bit machines, match
distances are limited to MAX_DIST instead of WSIZE. }

  if s.strstart > IPos(MAX_DIST) then
    limit := s.strstart - IPos(MAX_DIST)
  else
    limit := ZNIL;
  { Stop when cur_match becomes <= limit. To simplify the code,
    we prevent matches with the string of window index 0. }

  prev := s.prev;
  wmask := s.w_mask;

{$ifdef UNALIGNED_OK}
  { Compare two bytes at a time. Note: this is not always beneficial.
    Try with and without -DUNALIGNED_OK to check. }

  strend := pBytef(@(s.window^[s.strstart + MAX_MATCH - 1]));
  scan_start := pushf(scan)^;
  scan_end   := pushfArray(scan)^[best_len-1];   { fix }
{$else}
  strend := pBytef(@(s.window^[s.strstart + MAX_MATCH]));
  {$IFOPT R+} {$R-} {$DEFINE NoRangeCheck} {$ENDIF}
  scan_end1  := pzByteArray(scan)^[best_len-1];
  {$IFDEF NoRangeCheck} {$R+} {$UNDEF NoRangeCheck} {$ENDIF}
  scan_end   := pzByteArray(scan)^[best_len];
{$endif}

    { The code is optimized for HASH_BITS >= 8 and MAX_MATCH-2 multiple of 16.
      It is easy to get rid of this optimization if necessary. }
    {$IFDEF DEBUG}
    Assert((s.hash_bits >= 8) and (MAX_MATCH = 258), 'Code too clever');
    {$ENDIF}
    { Do not waste too much time if we already have a good match: }
    if (s.prev_length >= s.good_match) then
    begin
      chain_length := chain_length shr 2;
    end;

    { Do not look for matches beyond the end of the input. This is necessary
      to make deflate deterministic. }

    if (uInt(nice_match) > s.lookahead) then
      nice_match := s.lookahead;
    {$IFDEF DEBUG}
    Assert(ulg(s.strstart) <= s.window_size-MIN_LOOKAHEAD, 'need lookahead');
    {$ENDIF}
    repeat
        {$IFDEF DEBUG}
        Assert(cur_match < s.strstart, 'no future');
        {$ENDIF}
        match := @(s.window^[cur_match]);

        { Skip to next match if the match length cannot increase
          or if the match length is less than 2: }

{$undef DO_UNALIGNED_OK}
{$ifdef UNALIGNED_OK}
  {$ifdef MAX_MATCH_IS_258}
    {$define DO_UNALIGNED_OK}
  {$endif}
{$endif}

{$ifdef DO_UNALIGNED_OK}
        { This code assumes sizeof(unsigned short) = 2. Do not use
          UNALIGNED_OK if your compiler uses a different size. }
  {$IFOPT R+} {$R-} {$DEFINE NoRangeCheck} {$ENDIF}
        if (pushfArray(match)^[best_len-1] <> scan_end) or
           (pushf(match)^ <> scan_start) then
          goto nextstep; {continue;}
  {$IFDEF NoRangeCheck} {$R+} {$UNDEF NoRangeCheck} {$ENDIF}

        { It is not necessary to compare scan[2] and match[2] since they are
          always equal when the other bytes match, given that the hash keys
          are equal and that HASH_BITS >= 8. Compare 2 bytes at a time at
          strstart+3, +5, ... up to strstart+257. We check for insufficient
          lookahead only every 4th comparison; the 128th check will be made
          at strstart+257. If MAX_MATCH-2 is not a multiple of 8, it is
          necessary to put more guard bytes at the end of the window, or
          to check more often for insufficient lookahead. }
        {$IFDEF DEBUG}
        Assert(pzByteArray(scan)^[2] = pzByteArray(match)^[2], 'scan[2]?');
        {$ENDIF}
        Inc(scan);
        Inc(match);

        repeat
          Inc(scan,2); Inc(match,2); if (pushf(scan)^<>pushf(match)^) then break;
          Inc(scan,2); Inc(match,2); if (pushf(scan)^<>pushf(match)^) then break;
          Inc(scan,2); Inc(match,2); if (pushf(scan)^<>pushf(match)^) then break;
          Inc(scan,2); Inc(match,2); if (pushf(scan)^<>pushf(match)^) then break;
        until (ptr2int(scan) >= ptr2int(strend));
        { The funny "do while" generates better code on most compilers }

        { Here, scan <= window+strstart+257 }
        {$IFDEF DEBUG}
        {$ifopt R+} {$define RangeCheck} {$endif} {$R-}
        Assert(ptr2int(scan) <=
               ptr2int(@(s.window^[unsigned(s.window_size-1)])),
               'wild scan');
        {$ifdef RangeCheck} {$R+} {$undef RangeCheck} {$endif}
        {$ENDIF}
        if (scan^ = match^) then
          Inc(scan);

        len := (MAX_MATCH - 1) - int(ptr2int(strend)) + int(ptr2int(scan));
        scan := strend;
        Dec(scan, (MAX_MATCH-1));

{$else} { UNALIGNED_OK }

  {$IFOPT R+} {$R-} {$DEFINE NoRangeCheck} {$ENDIF}
        if (pzByteArray(match)^[best_len]   <> scan_end) or
           (pzByteArray(match)^[best_len-1] <> scan_end1) or
           (match^ <> scan^) then
          goto nextstep; {continue;}
  {$IFDEF NoRangeCheck} {$R+} {$UNDEF NoRangeCheck} {$ENDIF}
        Inc(match);
        if (match^ <> pzByteArray(scan)^[1]) then
          goto nextstep; {continue;}

        { The check at best_len-1 can be removed because it will be made
          again later. (This heuristic is not always a win.)
          It is not necessary to compare scan[2] and match[2] since they
          are always equal when the other bytes match, given that
          the hash keys are equal and that HASH_BITS >= 8. }

        Inc(scan, 2);
        Inc(match);
        {$IFDEF DEBUG}
        Assert( scan^ = match^, 'match[2]?');
        {$ENDIF}
        { We check for insufficient lookahead only every 8th comparison;
          the 256th check will be made at strstart+258. }

        repeat
          Inc(scan); Inc(match); if (scan^ <> match^) then break;
          Inc(scan); Inc(match); if (scan^ <> match^) then break;
          Inc(scan); Inc(match); if (scan^ <> match^) then break;
          Inc(scan); Inc(match); if (scan^ <> match^) then break;
          Inc(scan); Inc(match); if (scan^ <> match^) then break;
          Inc(scan); Inc(match); if (scan^ <> match^) then break;
          Inc(scan); Inc(match); if (scan^ <> match^) then break;
          Inc(scan); Inc(match); if (scan^ <> match^) then break;
        until (ptr2int(scan) >= ptr2int(strend));

        {$IFDEF DEBUG}
        Assert(ptr2int(scan) <=
               ptr2int(@(s.window^[unsigned(s.window_size-1)])),
               'wild scan');
        {$ENDIF}

        len := MAX_MATCH - int(ptr2int(strend) - ptr2int(scan));
        scan := strend;
        Dec(scan, MAX_MATCH);

{$endif} { UNALIGNED_OK }

        if (len > best_len) then
        begin
            s.match_start := cur_match;
            best_len := len;
            if (len >= nice_match) then
              break;
  {$IFOPT R+} {$R-} {$DEFINE NoRangeCheck} {$ENDIF}
{$ifdef UNALIGNED_OK}
            scan_end   := pzByteArray(scan)^[best_len-1];
{$else}
            scan_end1  := pzByteArray(scan)^[best_len-1];
            scan_end   := pzByteArray(scan)^[best_len];
{$endif}
  {$IFDEF NoRangeCheck} {$R+} {$UNDEF NoRangeCheck} {$ENDIF}
        end;
    nextstep:
      cur_match := prev^[cur_match and wmask];
      Dec(chain_length);
    until (cur_match <= limit) or (chain_length = 0);

    if (uInt(best_len) <= s.lookahead) then
      longest_match := uInt(best_len)
    else
      longest_match := s.lookahead;
end;
{$endif} { ASMV }

{$else} { FASTEST }
{ ---------------------------------------------------------------------------
  Optimized version for level = 1 only }

function longest_match(var s : deflate_state;
                       cur_match : IPos  { current match }
                       ) : uInt;
var
  {register} scan : pBytef;   { current string }
  {register} match : pBytef;  { matched string }
  {register} len : int;       { length of current match }
  {register} strend : pBytef;
begin
  scan := @s.window^[s.strstart];
  strend := @s.window^[s.strstart + MAX_MATCH];


    { The code is optimized for HASH_BITS >= 8 and MAX_MATCH-2 multiple of 16.
      It is easy to get rid of this optimization if necessary. }
    {$IFDEF DEBUG}
    Assert((s.hash_bits >= 8) and (MAX_MATCH = 258), 'Code too clever');

    Assert(ulg(s.strstart) <= s.window_size-MIN_LOOKAHEAD, 'need lookahead');

    Assert(cur_match < s.strstart, 'no future');
    {$ENDIF}
    match := s.window + cur_match;

    { Return failure if the match length is less than 2: }

    if (match[0] <> scan[0]) or (match[1] <> scan[1]) then
    begin
      longest_match := MIN_MATCH-1;
      exit;
    end;

    { The check at best_len-1 can be removed because it will be made
      again later. (This heuristic is not always a win.)
      It is not necessary to compare scan[2] and match[2] since they
      are always equal when the other bytes match, given that
      the hash keys are equal and that HASH_BITS >= 8. }

    scan += 2, match += 2;
    Assert(scan^ = match^, 'match[2]?');

    { We check for insufficient lookahead only every 8th comparison;
      the 256th check will be made at strstart+258. }

    repeat
      Inc(scan); Inc(match); if scan^<>match^ then break;
      Inc(scan); Inc(match); if scan^<>match^ then break;
      Inc(scan); Inc(match); if scan^<>match^ then break;
      Inc(scan); Inc(match); if scan^<>match^ then break;
      Inc(scan); Inc(match); if scan^<>match^ then break;
      Inc(scan); Inc(match); if scan^<>match^ then break;
      Inc(scan); Inc(match); if scan^<>match^ then break;
      Inc(scan); Inc(match); if scan^<>match^ then break;
    until (ptr2int(scan) >= ptr2int(strend));

    Assert(scan <= s.window+unsigned(s.window_size-1), 'wild scan');

    len := MAX_MATCH - int(strend - scan);

    if (len < MIN_MATCH) then
    begin
      return := MIN_MATCH - 1;
      exit;
    end;

    s.match_start := cur_match;
    if len <= s.lookahead then
      longest_match := len
    else
      longest_match := s.lookahead;
end;
{$endif} { FASTEST }

{$ifdef DEBUG}
{ ===========================================================================
  Check that the match at match_start is indeed a match. }

procedure check_match(var s : deflate_state;
                      start, match : IPos;
                      length : int);
begin
  exit;
  { check that the match is indeed a match }
  if (zmemcmp(pBytef(@s.window^[match]),
              pBytef(@s.window^[start]), length) <> EQUAL) then
  begin
    WriteLn(' start ',start,', match ',match ,' length ', length);
    repeat
      Write(AnsiChar(s.window^[match]), AnsiChar(s.window^[start]));
      Inc(match);
      Inc(start);
      Dec(length);
    Until (length = 0);
    z_error('invalid match');
  end;
  if (z_verbose > 1) then
  begin
    Write('\\[',start-match,',',length,']');
    repeat
       Write(AnsiChar(s.window^[start]));
       Inc(start);
       Dec(length);
    Until (length = 0);
  end;
end;
{$endif}

{ ===========================================================================
  Fill the window when the lookahead becomes insufficient.
  Updates strstart and lookahead.

  IN assertion: lookahead < MIN_LOOKAHEAD
  OUT assertions: strstart <= window_size-MIN_LOOKAHEAD
     At least one byte has been read, or avail_in = 0; reads are
     performed for at least two bytes (required for the zip translate_eol
     option -- not supported here). }

procedure fill_window(var s : deflate_state);
var
  {register} n, m : unsigned;
  {register} p : pPosf;
  more : unsigned;    { Amount of free space at the end of the window. }
  wsize : uInt;
begin
   wsize := s.w_size;
   repeat
     more := unsigned(s.window_size -ulg(s.lookahead) -ulg(s.strstart));

     { Deal with !@#$% 64K limit: }
     if (more = 0) and (s.strstart = 0) and (s.lookahead = 0) then
       more := wsize
     else
     if (more = unsigned(-1)) then
     begin
       { Very unlikely, but possible on 16 bit machine if strstart = 0
         and lookahead = 1 (input done one byte at time) }
       Dec(more);

       { If the window is almost full and there is insufficient lookahead,
         move the upper half to the lower one to make room in the upper half.}
     end
     else
       if (s.strstart >= wsize+ {MAX_DIST}(wsize-MIN_LOOKAHEAD)) then
       begin
         zmemcpy( pBytef(s.window), pBytef(@(s.window^[wsize])),
                 unsigned(wsize));
         Dec(s.match_start, wsize);
         Dec(s.strstart, wsize); { we now have strstart >= MAX_DIST }
         Dec(s.block_start, long(wsize));

         { Slide the hash table (could be avoided with 32 bit values
           at the expense of memory usage). We slide even when level = 0
           to keep the hash table consistent if we switch back to level > 0
           later. (Using level 0 permanently is not an optimal usage of
           zlib, so we don't care about this pathological case.) }

         n := s.hash_size;
         p := @s.head^[n];
         repeat
           Dec(p);
           m := p^;
           if (m >= wsize) then
             p^ := Pos(m-wsize)
           else
             p^ := Pos(ZNIL);
           Dec(n);
         Until (n=0);

         n := wsize;
{$ifndef FASTEST}
         p := @s.prev^[n];
         repeat
           Dec(p);
           m := p^;
           if (m >= wsize) then
             p^ := Pos(m-wsize)
           else
             p^:= Pos(ZNIL);
             { If n is not on any hash chain, prev^[n] is garbage but
               its value will never be used. }
           Dec(n);
         Until (n=0);
{$endif}
         Inc(more, wsize);
     end;
     if (s.strm^.avail_in = 0) then
       exit;

     {* If there was no sliding:
      *    strstart <= WSIZE+MAX_DIST-1 && lookahead <= MIN_LOOKAHEAD - 1 &&
      *    more == window_size - lookahead - strstart
      * => more >= window_size - (MIN_LOOKAHEAD-1 + WSIZE + MAX_DIST-1)
      * => more >= window_size - 2*WSIZE + 2
      * In the BIG_MEM or MMAP case (not yet supported),
      *   window_size == input_size + MIN_LOOKAHEAD  &&
      *   strstart + s->lookahead <= input_size => more >= MIN_LOOKAHEAD.
      * Otherwise, window_size == 2*WSIZE so more >= 2.
      * If there was sliding, more >= WSIZE. So in all cases, more >= 2. }

     {$IFDEF DEBUG}
     Assert(more >= 2, 'more < 2');
     {$ENDIF}

     n := read_buf(s.strm, pBytef(@(s.window^[s.strstart + s.lookahead])),
                  more);
     Inc(s.lookahead, n);

     { Initialize the hash value now that we have some input: }
     if (s.lookahead >= MIN_MATCH) then
     begin
       s.ins_h := s.window^[s.strstart];
       {UPDATE_HASH(s, s.ins_h, s.window[s.strstart+1]);}
       s.ins_h := ((s.ins_h shl s.hash_shift) xor s.window^[s.strstart+1])
                     and s.hash_mask;
{$ifdef MIN_MATCH <> 3}
       Call UPDATE_HASH() MIN_MATCH-3 more times
{$endif}
     end;
     { If the whole input has less than MIN_MATCH bytes, ins_h is garbage,
       but this is not important since only literal bytes will be emitted. }

   until (s.lookahead >= MIN_LOOKAHEAD) or (s.strm^.avail_in = 0);
end;

{ ===========================================================================
  Flush the current block, with given end-of-file flag.
  IN assertion: strstart is set to the end of the current match. }

procedure FLUSH_BLOCK_ONLY(var s : deflate_state; eof : boolean); {macro}
begin
  if (s.block_start >= Long(0)) then
    _tr_flush_block(s, pcharf(@s.window^[unsigned(s.block_start)]),
                    ulg(long(s.strstart) - s.block_start), eof)
  else
    _tr_flush_block(s, pcharf(Z_NULL),
                    ulg(long(s.strstart) - s.block_start), eof);

  s.block_start := s.strstart;
  flush_pending(s.strm^);
  {$IFDEF DEBUG}
  Tracev('[FLUSH]');
  {$ENDIF}
end;

{ Same but force premature exit if necessary.
macro FLUSH_BLOCK(var s : deflate_state; eof : boolean) : boolean;
var
  result : block_state;
begin
 FLUSH_BLOCK_ONLY(s, eof);
 if (s.strm^.avail_out = 0) then
 begin
   if eof then
     result := finish_started
   else
     result := need_more;
   exit;
 end;
end;
}

{ ===========================================================================
  Copy without compression as much as possible from the input stream, return
  the current block state.
  This function does not insert new strings in the dictionary since
  uncompressible data is probably not useful. This function is used
  only for the level=0 compression option.
  NOTE: this function should be optimized to avoid extra copying from
  window to pending_buf. }


function deflate_stored(var s : deflate_state; flush : int) : block_state;
{ Stored blocks are limited to 0xffff bytes, pending_buf is limited
  to pending_buf_size, and each stored block has a 5 byte header: }
var
  max_block_size : ulg;
  max_start : ulg;
begin
  max_block_size := $ffff;
  if (max_block_size > s.pending_buf_size - 5) then
    max_block_size := s.pending_buf_size - 5;

  { Copy as much as possible from input to output: }
  while TRUE do
  begin
    { Fill the window as much as possible: }
    if (s.lookahead <= 1) then
    begin
      {$IFDEF DEBUG}
      Assert( (s.strstart < s.w_size + {MAX_DIST}s.w_size-MIN_LOOKAHEAD) or
              (s.block_start >= long(s.w_size)), 'slide too late');
      {$ENDIF}
      fill_window(s);
      if (s.lookahead = 0) and (flush = Z_NO_FLUSH) then
      begin
        deflate_stored := need_more;
        exit;
      end;

      if (s.lookahead = 0) then
        break; { flush the current block }
    end;
    {$IFDEF DEBUG}
    Assert(s.block_start >= long(0), 'block gone');
    {$ENDIF}
    Inc(s.strstart, s.lookahead);
    s.lookahead := 0;

    { Emit a stored block if pending_buf will be full: }
    max_start := s.block_start + max_block_size;
    if (s.strstart = 0) or (ulg(s.strstart) >= max_start) then
    begin
      { strstart = 0 is possible when wraparound on 16-bit machine }
      s.lookahead := s.strstart - uInt(max_start);
      s.strstart := uInt(max_start);
      {FLUSH_BLOCK(s, FALSE);}
      FLUSH_BLOCK_ONLY(s, FALSE);
      if (s.strm^.avail_out = 0) then
      begin
        deflate_stored := need_more;
        exit;
      end;
    end;

    { Flush if we may have to slide, otherwise block_start may become
      negative and the data will be gone: }

    if (s.strstart - uInt(s.block_start) >= {MAX_DIST}
        s.w_size-MIN_LOOKAHEAD) then
    begin
      {FLUSH_BLOCK(s, FALSE);}
      FLUSH_BLOCK_ONLY(s, FALSE);
      if (s.strm^.avail_out = 0) then
      begin
        deflate_stored := need_more;
        exit;
      end;
    end;
  end;

  {FLUSH_BLOCK(s, flush = Z_FINISH);}
  FLUSH_BLOCK_ONLY(s, flush = Z_FINISH);
  if (s.strm^.avail_out = 0) then
  begin
    if flush = Z_FINISH then
      deflate_stored := finish_started
    else
      deflate_stored := need_more;
    exit;
  end;

  if flush = Z_FINISH then
    deflate_stored := finish_done
  else
    deflate_stored := block_done;
end;

{ ===========================================================================
  Compress as much as possible from the input stream, return the current
  block state.
  This function does not perform lazy evaluation of matches and inserts
  new strings in the dictionary only for unmatched strings or for short
  matches. It is used only for the fast compression options. }

function deflate_fast(var s : deflate_state; flush : int) : block_state;
var
  hash_head : IPos;     { head of the hash chain }
  bflush : boolean;     { set if current block must be flushed }
begin
  hash_head := ZNIL;
  while TRUE do
  begin
  { Make sure that we always have enough lookahead, except
    at the end of the input file. We need MAX_MATCH bytes
    for the next match, plus MIN_MATCH bytes to insert the
    string following the next match. }

    if (s.lookahead < MIN_LOOKAHEAD) then
    begin
      fill_window(s);
      if (s.lookahead < MIN_LOOKAHEAD) and (flush = Z_NO_FLUSH) then
      begin
        deflate_fast := need_more;
        exit;
      end;

      if (s.lookahead = 0) then
        break; { flush the current block }
    end;


    { Insert the string window[strstart .. strstart+2] in the
      dictionary, and set hash_head to the head of the hash chain: }

    if (s.lookahead >= MIN_MATCH) then
    begin
      INSERT_STRING(s, s.strstart, hash_head);
    end;

    { Find the longest match, discarding those <= prev_length.
      At this point we have always match_length < MIN_MATCH }
    if (hash_head <> ZNIL) and
       (s.strstart - hash_head <= (s.w_size-MIN_LOOKAHEAD){MAX_DIST}) then
    begin
      { To simplify the code, we prevent matches with the string
        of window index 0 (in particular we have to avoid a match
        of the string with itself at the start of the input file). }
      if (s.strategy <> Z_HUFFMAN_ONLY) then
      begin
        s.match_length := longest_match (s, hash_head);
      end;
      { longest_match() sets match_start }
    end;
    if (s.match_length >= MIN_MATCH) then
    begin
      {$IFDEF DEBUG}
      check_match(s, s.strstart, s.match_start, s.match_length);
      {$ENDIF}

      {_tr_tally_dist(s, s.strstart - s.match_start,
                        s.match_length - MIN_MATCH, bflush);}
      bflush := _tr_tally(s, s.strstart - s.match_start,
                        s.match_length - MIN_MATCH);

      Dec(s.lookahead, s.match_length);

      { Insert new strings in the hash table only if the match length
        is not too large. This saves time but degrades compression. }

{$ifndef FASTEST}
      if (s.match_length <= s.max_insert_length)
       and (s.lookahead >= MIN_MATCH) then
      begin
        Dec(s.match_length); { string at strstart already in hash table }
        repeat
          Inc(s.strstart);
          INSERT_STRING(s, s.strstart, hash_head);
          { strstart never exceeds WSIZE-MAX_MATCH, so there are
            always MIN_MATCH bytes ahead. }
          Dec(s.match_length);
        until (s.match_length = 0);
        Inc(s.strstart);
      end
      else
{$endif}

      begin
        Inc(s.strstart, s.match_length);
        s.match_length := 0;
        s.ins_h := s.window^[s.strstart];
        {UPDATE_HASH(s, s.ins_h, s.window[s.strstart+1]);}
        s.ins_h := (( s.ins_h shl s.hash_shift) xor
                     s.window^[s.strstart+1]) and s.hash_mask;
//if MIN_MATCH <> 3 then   { the linker removes this }
//begin
//          {Call UPDATE_HASH() MIN_MATCH-3 more times}
//end;

        { If lookahead < MIN_MATCH, ins_h is garbage, but it does not
          matter since it will be recomputed at next deflate call. }

      end;
    end
    else
    begin
      { No match, output a literal byte }
      {$IFDEF DEBUG}
      Tracevv(AnsiChar(s.window^[s.strstart]));
      {$ENDIF}
      {_tr_tally_lit (s, 0, s.window^[s.strstart], bflush);}
      bflush := _tr_tally (s, 0, s.window^[s.strstart]);

      Dec(s.lookahead);
      Inc(s.strstart);
    end;
    if bflush then
    begin  {FLUSH_BLOCK(s, FALSE);}
      FLUSH_BLOCK_ONLY(s, FALSE);
      if (s.strm^.avail_out = 0) then
      begin
        deflate_fast := need_more;
        exit;
      end;
    end;
  end;
  {FLUSH_BLOCK(s, flush = Z_FINISH);}
  FLUSH_BLOCK_ONLY(s, flush = Z_FINISH);
  if (s.strm^.avail_out = 0) then
  begin
    if flush = Z_FINISH then
      deflate_fast := finish_started
    else
      deflate_fast := need_more;
    exit;
  end;

  if flush = Z_FINISH then
    deflate_fast := finish_done
  else
    deflate_fast := block_done;
end;

{ ===========================================================================
  Same as above, but achieves better compression. We use a lazy
  evaluation for matches: a match is finally adopted only if there is
  no better match at the next window position. }

function deflate_slow(var s : deflate_state; flush : int) : block_state;
var
  hash_head : IPos;       { head of hash chain }
  bflush : boolean;       { set if current block must be flushed }
var
  max_insert : uInt;
begin
  hash_head := ZNIL;

  { Process the input block. }
  while TRUE do
  begin
    { Make sure that we always have enough lookahead, except
      at the end of the input file. We need MAX_MATCH bytes
      for the next match, plus MIN_MATCH bytes to insert the
      string following the next match. }

    if (s.lookahead < MIN_LOOKAHEAD) then
    begin
      fill_window(s);
      if (s.lookahead < MIN_LOOKAHEAD) and (flush = Z_NO_FLUSH) then
      begin
        deflate_slow := need_more;
        exit;
      end;

      if (s.lookahead = 0) then
        break; { flush the current block }
    end;

    { Insert the string window[strstart .. strstart+2] in the
      dictionary, and set hash_head to the head of the hash chain: }

    if (s.lookahead >= MIN_MATCH) then
    begin
      INSERT_STRING(s, s.strstart, hash_head);
    end;

    { Find the longest match, discarding those <= prev_length. }

    s.prev_length := s.match_length;
    s.prev_match := s.match_start;
    s.match_length := MIN_MATCH-1;

    if (hash_head <> ZNIL) and (s.prev_length < s.max_lazy_match) and
       (s.strstart - hash_head <= {MAX_DIST}(s.w_size-MIN_LOOKAHEAD)) then
    begin
        { To simplify the code, we prevent matches with the string
          of window index 0 (in particular we have to avoid a match
          of the string with itself at the start of the input file). }

        if (s.strategy <> Z_HUFFMAN_ONLY) then
        begin
          s.match_length := longest_match (s, hash_head);
        end;
        { longest_match() sets match_start }

        if (s.match_length <= 5) and ((s.strategy = Z_FILTERED) or
             ((s.match_length = MIN_MATCH) and
              (s.strstart - s.match_start > TOO_FAR))) then
        begin
            { If prev_match is also MIN_MATCH, match_start is garbage
              but we will ignore the current match anyway. }

            s.match_length := MIN_MATCH-1;
        end;
    end;
    { If there was a match at the previous step and the current
      match is not better, output the previous match: }

    if (s.prev_length >= MIN_MATCH)
      and (s.match_length <= s.prev_length) then
    begin
      max_insert := s.strstart + s.lookahead - MIN_MATCH;
      { Do not insert strings in hash table beyond this. }
      {$ifdef DEBUG}
      check_match(s, s.strstart-1, s.prev_match, s.prev_length);
      {$endif}

      {_tr_tally_dist(s, s->strstart -1 - s->prev_match,
	                s->prev_length - MIN_MATCH, bflush);}
      bflush := _tr_tally(s, s.strstart -1 - s.prev_match,
                           s.prev_length - MIN_MATCH);

      { Insert in hash table all strings up to the end of the match.
        strstart-1 and strstart are already inserted. If there is not
        enough lookahead, the last two strings are not inserted in
        the hash table. }

      Dec(s.lookahead, s.prev_length-1);
      Dec(s.prev_length, 2);
      repeat
        Inc(s.strstart);
        if (s.strstart <= max_insert) then
        begin
          INSERT_STRING(s, s.strstart, hash_head);
        end;
        Dec(s.prev_length);
      until (s.prev_length = 0);
      s.match_available := FALSE;
      s.match_length := MIN_MATCH-1;
      Inc(s.strstart);

      if (bflush) then  {FLUSH_BLOCK(s, FALSE);}
      begin
        FLUSH_BLOCK_ONLY(s, FALSE);
        if (s.strm^.avail_out = 0) then
        begin
          deflate_slow := need_more;
          exit;
        end;
      end;
    end
    else
      if (s.match_available) then
      begin
        { If there was no match at the previous position, output a
          single literal. If there was a match but the current match
          is longer, truncate the previous match to a single literal. }
        {$IFDEF DEBUG}
        Tracevv(AnsiChar(s.window^[s.strstart-1]));
        {$ENDIF}
        bflush := _tr_tally (s, 0, s.window^[s.strstart-1]);

        if bflush then
        begin
          FLUSH_BLOCK_ONLY(s, FALSE);
        end;
        Inc(s.strstart);
        Dec(s.lookahead);
        if (s.strm^.avail_out = 0) then
        begin
          deflate_slow := need_more;
          exit;
        end;
      end
      else
      begin
        { There is no previous match to compare with, wait for
          the next step to decide. }

        s.match_available := TRUE;
        Inc(s.strstart);
        Dec(s.lookahead);
      end;
  end;

  {$IFDEF DEBUG}
  Assert (flush <> Z_NO_FLUSH, 'no flush?');
  {$ENDIF}
  if (s.match_available) then
  begin
    {$IFDEF DEBUG}
    Tracevv(AnsiChar(s.window^[s.strstart-1]));
    bflush :=
    {$ENDIF}
      _tr_tally (s, 0, s.window^[s.strstart-1]);
    s.match_available := FALSE;
  end;
  {FLUSH_BLOCK(s, flush = Z_FINISH);}
  FLUSH_BLOCK_ONLY(s, flush = Z_FINISH);
  if (s.strm^.avail_out = 0) then
  begin
    if flush = Z_FINISH then
      deflate_slow := finish_started
    else
      deflate_slow := need_more;
    exit;
  end;
  if flush = Z_FINISH then
    deflate_slow := finish_done
  else
    deflate_slow := block_done;
end;

{== imadler implementation ==}

const
  BASE = uLong(65521); { largest prime smaller than 65536 }
  {NMAX = 5552; original code with unsigned 32 bit integer }
  { NMAX is the largest n such that 255n(n+1)/2 + (n+1)(BASE-1) <= 2^32-1 }
  NMAX = 3854;        { code with signed 32 bit integer }
  { NMAX is the largest n such that 255n(n+1)/2 + (n+1)(BASE-1) <= 2^31-1 }
  { The penalty is the time loss in the extra MOD-calls. }


{ ========================================================================= }

function adler32(adler : uLong; buf : pBytef; len : uInt) : uLong;
var
  s1, s2 : uLong;
  k : int;
begin
  s1 := adler and $ffff;
  s2 := (adler shr 16) and $ffff;

  if not Assigned(buf) then
  begin
    adler32 := uLong(1);
    exit;
  end;

  while (len > 0) do
  begin
    if len < NMAX then
      k := len
    else
      k := NMAX;
    Dec(len, k);
    {
    while (k >= 16) do
    begin
      DO16(buf);
      Inc(buf, 16);
      Dec(k, 16);
    end;
    if (k <> 0) then
    repeat
      Inc(s1, buf^);
      Inc(puf);
      Inc(s2, s1);
      Dec(k);
    until (k = 0);
    }
    while (k > 0) do
    begin
      Inc(s1, buf^);
      Inc(s2, s1);
      Inc(buf);
      Dec(k);
    end;
    s1 := s1 mod BASE;
    s2 := s2 mod BASE;
  end;
  adler32 := (s2 shl 16) or s1;
end;

{
#define DO1(buf,i)
  begin
    Inc(s1, buf[i]);
    Inc(s2, s1);
  end;
#define DO2(buf,i)  DO1(buf,i); DO1(buf,i+1);
#define DO4(buf,i)  DO2(buf,i); DO2(buf,i+2);
#define DO8(buf,i)  DO4(buf,i); DO4(buf,i+4);
#define DO16(buf)   DO8(buf,0); DO8(buf,8);
}

{== iminfblock implementation ==}

{ Tables for deflate from PKZIP's appnote.txt. }
Const
  border : Array [0..18] Of Word  { Order of the bit length code lengths }
    = (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);

{ Notes beyond the 1.93a appnote.txt:

   1. Distance pointers never point before the beginning of the output
      stream.
   2. Distance pointers can point back across blocks, up to 32k away.
   3. There is an implied maximum of 7 bits for the bit length table and
      15 bits for the actual data.
   4. If only one code exists, then it is encoded using one bit.  (Zero
      would be more efficient, but perhaps a little confusing.)  If two
      codes exist, they are coded using one bit each (0 and 1).
   5. There is no way of sending zero distance codes--a dummy must be
      sent if there are none.  (History: a pre 2.0 version of PKZIP would
      store blocks with no distance codes, but this was discovered to be
      too harsh a criterion.)  Valid only for 1.93a.  2.04c does allow
      zero distance codes, which is sent as one code of zero bits in
      length.
   6. There are up to 286 literal/length codes.  Code 256 represents the
      end-of-block.  Note however that the static length tree defines
      288 codes just to fill out the Huffman codes.  Codes 286 and 287
      cannot be used though, since there is no length base or extra bits
      defined for them.  Similarily, there are up to 30 distance codes.
      However, static trees define 32 codes (all 5 bits) to fill out the
      Huffman codes, but the last two had better not show up in the data.
   7. Unzip can check dynamic Huffman blocks for complete code sets.
      The exception is that a single code would not be complete (see #4).
   8. The five bits following the block type is really the number of
      literal codes sent minus 257.
   9. Length codes 8,16,16 are interpreted as 13 length codes of 8 bits
      (1+6+6).  Therefore, to output three times the length, you output
      three codes (1+1+1), whereas to output four times the same length,
      you only need two codes (1+3).  Hmm.
  10. In the tree reconstruction algorithm, Code = Code + Increment
      only if BitLength(i) is not zero.  (Pretty obvious.)
  11. Correction: 4 Bits: # of Bit Length codes - 4     (4 - 19)
  12. Note: length code 284 can represent 227-258, but length code 285
      really is 258.  The last length deserves its own, short code
      since it gets used a lot in very redundant files.  The length
      258 is special since 258 - 3 (the min match length) is 255.
  13. The literal/length and distance code bit lengths are read as a
      single stream of lengths.  It is possible (and advantageous) for
      a repeat code (16, 17, or 18) to go across the boundary between
      the two sets of lengths. }


procedure inflate_blocks_reset (var s : inflate_blocks_state;
                                var z : z_stream;
                                c : puLong); { check value on output }
begin
  if (c <> Z_NULL) then
    c^ := s.check;
  if (s.mode = BTREE) or (s.mode = DTREE) then
    ZFREE(z, s.sub.trees.blens);
  if (s.mode = CODES) then
    inflate_codes_free(s.sub.decode.codes, z);

  s.mode := ZTYPE;
  s.bitk := 0;
  s.bitb := 0;

  s.write := s.window;
  s.read := s.window;
  if Assigned(s.checkfn) then
  begin
    s.check := s.checkfn(uLong(0), pBytef(NIL), 0);
    z.adler := s.check;
  end;
  {$IFDEF DEBUG}
  Tracev('inflate:   blocks reset');
  {$ENDIF}
end;


function inflate_blocks_new(var z : z_stream;
                            c : check_func;  { check function }
                            w : uInt         { window size }
                            ) : pInflate_blocks_state;
var
  s : pInflate_blocks_state;
begin
  s := pInflate_blocks_state( ZALLOC(z,1, sizeof(inflate_blocks_state)) );
  if (s = Z_NULL) then
  begin
    inflate_blocks_new := s;
    exit;
  end;
  s^.hufts := huft_ptr( ZALLOC(z, sizeof(inflate_huft), MANY) );

  if (s^.hufts = Z_NULL) then
  begin
    ZFREE(z, s);
    inflate_blocks_new := Z_NULL;
    exit;
  end;

  s^.window := pBytef( ZALLOC(z, 1, w) );
  if (s^.window = Z_NULL) then
  begin
    ZFREE(z, s^.hufts);
    ZFREE(z, s);
    inflate_blocks_new := Z_NULL;
    exit;
  end;
  s^.zend := s^.window;
  Inc(s^.zend, w);
  s^.checkfn := c;
  s^.mode := ZTYPE;
  {$IFDEF DEBUG}  
  Tracev('inflate:   blocks allocated');
  {$ENDIF}
  inflate_blocks_reset(s^, z, Z_NULL);
  inflate_blocks_new := s;
end;


function inflate_blocks (var s : inflate_blocks_state;
                         var z : z_stream;
                         r : int) : int;           { initial return code }
label
  start_btree, start_dtree,
  start_blkdone, start_dry,
  start_codes;

var
  t : uInt;               { temporary storage }
  b : uLong;              { bit buffer }
  k : uInt;               { bits in bit buffer }
  p : pBytef;             { input data pointer }
  n : uInt;               { bytes available there }
  q : pBytef;             { output window write pointer }
  m : uInt;               { bytes to end of window or read pointer }
{ fixed code blocks }
var
  bl, bd : uInt;
  tl, td : pInflate_huft;
var
  h : pInflate_huft;
  i, j, c : uInt;
var
  cs : pInflate_codes_state;
begin
  { copy input/output information to locals }
  p := z.next_in;
  n := z.avail_in;
  b := s.bitb;
  k := s.bitk;
  q := s.write;
  if ptr2int(q) < ptr2int(s.read) then
    m := uInt(ptr2int(s.read)-ptr2int(q)-1)
  else
    m := uInt(ptr2int(s.zend)-ptr2int(q));

{ decompress an inflated block }


  { process input based on current state }
  while True do
  Case s.mode of
    ZTYPE:
      begin
        {NEEDBITS(3);}
        while (k < 3) do
        begin
          {NEEDBYTE;}
          if (n <> 0) then
            r :=Z_OK
          else
          begin
            {UPDATE}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;
            inflate_blocks := inflate_flush(s,z,r);
            exit;
          end;
          Dec(n);
          b := b or (uLong(p^) shl k);
          Inc(p);
          Inc(k, 8);
        end;

        t := uInt(b) and 7;
        s.last := boolean(t and 1);
        case (t shr 1) of
          0:                         { stored }
            begin
              {$IFDEF DEBUG}
              if s.last then
                Tracev('inflate:     stored block (last)')
              else
                Tracev('inflate:     stored block');
              {$ENDIF}
              {DUMPBITS(3);}
              b := b shr 3;
              Dec(k, 3);

              t := k and 7;                  { go to byte boundary }
              {DUMPBITS(t);}
              b := b shr t;
              Dec(k, t);

              s.mode := LENS;                { get length of stored block }
            end;
          1:                         { fixed }
            begin
              begin
                {$IFDEF DEBUG}
                if s.last then
                  Tracev('inflate:     fixed codes blocks (last)')
                else
                  Tracev('inflate:     fixed codes blocks');
                {$ENDIF}
                inflate_trees_fixed(bl, bd, tl, td, z);
                s.sub.decode.codes := inflate_codes_new(bl, bd, tl, td, z);
                if (s.sub.decode.codes = Z_NULL) then
                begin
                  r := Z_MEM_ERROR;
                  { update pointers and return }
                  s.bitb := b;
                  s.bitk := k;
                  z.avail_in := n;
                  Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                  z.next_in := p;
                  s.write := q;
                  inflate_blocks := inflate_flush(s,z,r);
                  exit;
                end;
              end;
              {DUMPBITS(3);}
              b := b shr 3;
              Dec(k, 3);

              s.mode := CODES;
            end;
          2:                         { dynamic }
            begin
              {$IFDEF DEBUG}
              if s.last then
                Tracev('inflate:     dynamic codes block (last)')
              else
                Tracev('inflate:     dynamic codes block');
              {$ENDIF}                
              {DUMPBITS(3);}
              b := b shr 3;
              Dec(k, 3);

              s.mode := TABLE;
            end;
          3:
            begin                   { illegal }
              {DUMPBITS(3);}
              b := b shr 3;
              Dec(k, 3);

              s.mode := BLKBAD;
              z.msg := 'invalid block type';
              r := Z_DATA_ERROR;
              { update pointers and return }
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
              z.next_in := p;
              s.write := q;
              inflate_blocks := inflate_flush(s,z,r);
              exit;
            end;
        end;
      end;
    LENS:
      begin
        {NEEDBITS(32);}
        while (k < 32) do
        begin
          {NEEDBYTE;}
          if (n <> 0) then
            r :=Z_OK
          else
          begin
            {UPDATE}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;
            inflate_blocks := inflate_flush(s,z,r);
            exit;
          end;
          Dec(n);
          b := b or (uLong(p^) shl k);
          Inc(p);
          Inc(k, 8);
        end;

        if (((not b) shr 16) and $ffff) <> (b and $ffff) then
        begin
          s.mode := BLKBAD;
          z.msg := 'invalid stored block lengths';
          r := Z_DATA_ERROR;
          { update pointers and return }
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_blocks := inflate_flush(s,z,r);
          exit;
        end;
        s.sub.left := uInt(b) and $ffff;
        k := 0;
        b := 0;                      { dump bits }
        {$IFDEF DEBUG}
        Tracev('inflate:       stored length '+IntToStr(s.sub.left));
        {$ENDIF}
        if s.sub.left <> 0 then
          s.mode := STORED
        else
          if s.last then
            s.mode := DRY
          else
            s.mode := ZTYPE;
      end;
    STORED:
      begin
        if (n = 0) then
        begin
          { update pointers and return }
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_blocks := inflate_flush(s,z,r);
          exit;
        end;
        {NEEDOUT}
        if (m = 0) then
        begin
          {WRAP}
          if (q = s.zend) and (s.read <> s.window) then
          begin
            q := s.window;
            if ptr2int(q) < ptr2int(s.read) then
              m := uInt(ptr2int(s.read)-ptr2int(q)-1)
            else
              m := uInt(ptr2int(s.zend)-ptr2int(q));
          end;

          if (m = 0) then
          begin
            {FLUSH}
            s.write := q;
            r := inflate_flush(s,z,r);
            q := s.write;
            if ptr2int(q) < ptr2int(s.read) then
              m := uInt(ptr2int(s.read)-ptr2int(q)-1)
            else
              m := uInt(ptr2int(s.zend)-ptr2int(q));

            {WRAP}
            if (q = s.zend) and (s.read <> s.window) then
            begin
              q := s.window;
              if ptr2int(q) < ptr2int(s.read) then
                m := uInt(ptr2int(s.read)-ptr2int(q)-1)
              else
                m := uInt(ptr2int(s.zend)-ptr2int(q));
            end;

            if (m = 0) then
            begin
              {UPDATE}
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
              z.next_in := p;
              s.write := q;
              inflate_blocks := inflate_flush(s,z,r);
              exit;
            end;
          end;
        end;
        r := Z_OK;

        t := s.sub.left;
        if (t > n) then
          t := n;
        if (t > m) then
          t := m;
        zmemcpy(q, p, t);
        Inc(p, t);  Dec(n, t);
        Inc(q, t);  Dec(m, t);
        Dec(s.sub.left, t);
        if (s.sub.left = 0) then
        begin
          {$IFDEF DEBUG}
          if (ptr2int(q) >= ptr2int(s.read)) then
            Tracev('inflate:       stored end '+
                IntToStr(z.total_out + ptr2int(q) - ptr2int(s.read)) + ' total out')
          else
            Tracev('inflate:       stored end '+
                    IntToStr(z.total_out + ptr2int(s.zend) - ptr2int(s.read) +
                    ptr2int(q) - ptr2int(s.window)) +  ' total out');
          {$ENDIF}
          if s.last then
            s.mode := DRY
          else
            s.mode := ZTYPE;
        end;
      end;
    TABLE:
      begin
        {NEEDBITS(14);}
        while (k < 14) do
        begin
          {NEEDBYTE;}
          if (n <> 0) then
            r :=Z_OK
          else
          begin
            {UPDATE}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;
            inflate_blocks := inflate_flush(s,z,r);
            exit;
          end;
          Dec(n);
          b := b or (uLong(p^) shl k);
          Inc(p);
          Inc(k, 8);
        end;

        t := uInt(b) and $3fff;
        s.sub.trees.table := t;
  {$ifndef PKZIP_BUG_WORKAROUND}
        if ((t and $1f) > 29) or (((t shr 5) and $1f) > 29) then
        begin
          s.mode := BLKBAD;
          z.msg := 'too many length or distance symbols';
          r := Z_DATA_ERROR;
          { update pointers and return }
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_blocks := inflate_flush(s,z,r);
          exit;
        end;
  {$endif}
        t := 258 + (t and $1f) + ((t shr 5) and $1f);
        s.sub.trees.blens := puIntArray( ZALLOC(z, t, sizeof(uInt)) );
        if (s.sub.trees.blens = Z_NULL) then
        begin
          r := Z_MEM_ERROR;
          { update pointers and return }
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_blocks := inflate_flush(s,z,r);
          exit;
        end;
        {DUMPBITS(14);}
        b := b shr 14;
        Dec(k, 14);

        s.sub.trees.index := 0;
        {$IFDEF DEBUG}
        Tracev('inflate:       table sizes ok');
        {$ENDIF}
        s.mode := BTREE;
        { fall trough case is handled by the while }
        { try GOTO for speed - Nomssi }
        goto start_btree;
      end;
    BTREE:
      begin
        start_btree:
        while (s.sub.trees.index < 4 + (s.sub.trees.table shr 10)) do
        begin
          {NEEDBITS(3);}
          while (k < 3) do
          begin
            {NEEDBYTE;}
            if (n <> 0) then
              r :=Z_OK
            else
            begin
              {UPDATE}
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
              z.next_in := p;
              s.write := q;
              inflate_blocks := inflate_flush(s,z,r);
              exit;
            end;
            Dec(n);
            b := b or (uLong(p^) shl k);
            Inc(p);
            Inc(k, 8);
          end;

          s.sub.trees.blens^[border[s.sub.trees.index]] := uInt(b) and 7;
          Inc(s.sub.trees.index);
          {DUMPBITS(3);}
          b := b shr 3;
          Dec(k, 3);
        end;
        while (s.sub.trees.index < 19) do
        begin
          s.sub.trees.blens^[border[s.sub.trees.index]] := 0;
          Inc(s.sub.trees.index);
        end;
        s.sub.trees.bb := 7;
        t := inflate_trees_bits(s.sub.trees.blens^, s.sub.trees.bb,
                                s.sub.trees.tb, s.hufts^, z);
        if (t <> Z_OK) then
        begin
          ZFREE(z, s.sub.trees.blens);
          r := t;
          if (r = Z_DATA_ERROR) then
            s.mode := BLKBAD;
          { update pointers and return }
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_blocks := inflate_flush(s,z,r);
          exit;
        end;
        s.sub.trees.index := 0;
        {$IFDEF DEBUG}
        Tracev('inflate:       bits tree ok');
        {$ENDIF}
        s.mode := DTREE;
        { fall through again }
        goto start_dtree;
      end;
    DTREE:
      begin
        start_dtree:
        while TRUE do
        begin
          t := s.sub.trees.table;
          if not (s.sub.trees.index < 258 +
                                     (t and $1f) + ((t shr 5) and $1f)) then
            break;
          t := s.sub.trees.bb;
          {NEEDBITS(t);}
          while (k < t) do
          begin
            {NEEDBYTE;}
            if (n <> 0) then
              r :=Z_OK
            else
            begin
              {UPDATE}
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
              z.next_in := p;
              s.write := q;
              inflate_blocks := inflate_flush(s,z,r);
              exit;
            end;
            Dec(n);
            b := b or (uLong(p^) shl k);
            Inc(p);
            Inc(k, 8);
          end;

          h := s.sub.trees.tb;
          Inc(h, uInt(b) and inflate_mask[t]);
          t := h^.Bits;
          c := h^.Base;

          if (c < 16) then
          begin
            {DUMPBITS(t);}
            b := b shr t;
            Dec(k, t);

            s.sub.trees.blens^[s.sub.trees.index] := c;
            Inc(s.sub.trees.index);
          end
          else { c = 16..18 }
          begin
            if c = 18 then
            begin
              i := 7;
              j := 11;
            end
            else
            begin
              i := c - 14;
              j := 3;
            end;
            {NEEDBITS(t + i);}
            while (k < t + i) do
            begin
              {NEEDBYTE;}
              if (n <> 0) then
                r :=Z_OK
              else
              begin
                {UPDATE}
                s.bitb := b;
                s.bitk := k;
                z.avail_in := n;
                Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
                z.next_in := p;
                s.write := q;
                inflate_blocks := inflate_flush(s,z,r);
                exit;
              end;
              Dec(n);
              b := b or (uLong(p^) shl k);
              Inc(p);
              Inc(k, 8);
            end;

            {DUMPBITS(t);}
            b := b shr t;
            Dec(k, t);

            Inc(j, uInt(b) and inflate_mask[i]);
            {DUMPBITS(i);}
            b := b shr i;
            Dec(k, i);

            i := s.sub.trees.index;
            t := s.sub.trees.table;
            if (i + j > 258 + (t and $1f) + ((t shr 5) and $1f)) or
               ((c = 16) and (i < 1)) then
            begin
              ZFREE(z, s.sub.trees.blens);
              s.mode := BLKBAD;
              z.msg := 'invalid bit length repeat';
              r := Z_DATA_ERROR;
              { update pointers and return }
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
              z.next_in := p;
              s.write := q;
              inflate_blocks := inflate_flush(s,z,r);
              exit;
            end;
            if c = 16 then
              c := s.sub.trees.blens^[i - 1]
            else
              c := 0;
            repeat
              s.sub.trees.blens^[i] := c;
              Inc(i);
              Dec(j);
            until (j=0);
            s.sub.trees.index := i;
          end;
        end; { while }
        s.sub.trees.tb := Z_NULL;
        begin
          bl := 9;         { must be <= 9 for lookahead assumptions }
          bd := 6;         { must be <= 9 for lookahead assumptions }
          t := s.sub.trees.table;
          t := inflate_trees_dynamic(257 + (t and $1f),
                  1 + ((t shr 5) and $1f),
                  s.sub.trees.blens^, bl, bd, tl, td, s.hufts^, z);
          ZFREE(z, s.sub.trees.blens);
          if (t <> Z_OK) then
          begin
            if (t = uInt(Z_DATA_ERROR)) then
              s.mode := BLKBAD;
            r := t;
            { update pointers and return }
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;
            inflate_blocks := inflate_flush(s,z,r);
            exit;
          end;
          {$IFDEF DEBUG}
          Tracev('inflate:       trees ok');
          {$ENDIF}          
          { c renamed to cs }
          cs := inflate_codes_new(bl, bd, tl, td, z);
          if (cs = Z_NULL) then
          begin
            r := Z_MEM_ERROR;
            { update pointers and return }
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;
            inflate_blocks := inflate_flush(s,z,r);
            exit;
          end;
          s.sub.decode.codes := cs;
        end;
        s.mode := CODES;
        { yet another falltrough }
        goto start_codes;
      end;
    CODES:
      begin
        start_codes:
        { update pointers }
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
        z.next_in := p;
        s.write := q;

        r := inflate_codes(s, z, r);
        if (r <> Z_STREAM_END) then
        begin
          inflate_blocks := inflate_flush(s, z, r);
          exit;
        end;
        r := Z_OK;
        inflate_codes_free(s.sub.decode.codes, z);
        { load local pointers }
        p := z.next_in;
        n := z.avail_in;
        b := s.bitb;
        k := s.bitk;
        q := s.write;
        if ptr2int(q) < ptr2int(s.read) then
          m := uInt(ptr2int(s.read)-ptr2int(q)-1)
        else
          m := uInt(ptr2int(s.zend)-ptr2int(q));
        {$IFDEF DEBUG}
        if (ptr2int(q) >= ptr2int(s.read)) then
          Tracev('inflate:       codes end '+
              IntToStr(z.total_out + ptr2int(q) - ptr2int(s.read)) + ' total out')
        else
          Tracev('inflate:       codes end '+
                  IntToStr(z.total_out + ptr2int(s.zend) - ptr2int(s.read) +
                  ptr2int(q) - ptr2int(s.window)) +  ' total out');
        {$ENDIF}
        if (not s.last) then
        begin
          s.mode := ZTYPE;
          continue; { break for switch statement in C-code }
        end;
        {$ifndef patch112}
        if (k > 7) then           { return unused byte, if any }
        begin
          {$IFDEF DEBUG}
          Assert(k < 16, 'inflate_codes grabbed too many bytes');
          {$ENDIF}
          Dec(k, 8);
          Inc(n);
          Dec(p);                    { can always return one }
        end;
        {$endif}
        s.mode := DRY;
        { another falltrough }
        goto start_dry;
      end;
    DRY:
      begin
        start_dry:
        {FLUSH}
        s.write := q;
        r := inflate_flush(s,z,r);
        q := s.write;

        { not needed anymore, we are done:
        if ptr2int(q) < ptr2int(s.read) then
          m := uInt(ptr2int(s.read)-ptr2int(q)-1)
        else
          m := uInt(ptr2int(s.zend)-ptr2int(q));
        }

        if (s.read <> s.write) then
        begin
          { update pointers and return }
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_blocks := inflate_flush(s,z,r);
          exit;
        end;
        s.mode := BLKDONE;
        goto start_blkdone;
      end;
    BLKDONE:
      begin
        start_blkdone:
        r := Z_STREAM_END;
        { update pointers and return }
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
        z.next_in := p;
        s.write := q;
        inflate_blocks := inflate_flush(s,z,r);
        exit;
      end;
    BLKBAD:
      begin
        r := Z_DATA_ERROR;
        { update pointers and return }
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
        z.next_in := p;
        s.write := q;
        inflate_blocks := inflate_flush(s,z,r);
        exit;
      end;
    else
    begin
      r := Z_STREAM_ERROR;
      { update pointers and return }
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_blocks := inflate_flush(s,z,r);
      exit;
    end;
  end; { Case s.mode of }

end;


function inflate_blocks_free(s : pInflate_blocks_state;
                             var z : z_stream) : int;
begin
  inflate_blocks_reset(s^, z, Z_NULL);
  ZFREE(z, s^.window);
  ZFREE(z, s^.hufts);
  ZFREE(z, s);
  {$IFDEF DEBUG}
  Trace('inflate:   blocks freed');
  {$ENDIF}  
  inflate_blocks_free := Z_OK;
end;


procedure inflate_set_dictionary(var s : inflate_blocks_state;
                                 const d : array of byte; { dictionary }
                                 n : uInt);         { dictionary length }
begin
  zmemcpy(s.window, pBytef(@d), n);
  s.write := s.window;
  Inc(s.write, n);
  s.read := s.write;
end;


{ Returns true if inflate is currently at the end of a block generated
  by Z_SYNC_FLUSH or Z_FULL_FLUSH.
  IN assertion: s <> Z_NULL }

function inflate_blocks_sync_point(var s : inflate_blocks_state) : int;
begin
  inflate_blocks_sync_point := int(s.mode = LENS);
end;

{== iminfcodes implementation ==}

function inflate_codes_new (bl : uInt;
                            bd : uInt;
                            tl : pInflate_huft;
                            td : pInflate_huft;
                            var z : z_stream): pInflate_codes_state;
var
 c : pInflate_codes_state;
begin
  c := pInflate_codes_state( ZALLOC(z,1,sizeof(inflate_codes_state)) );
  if (c <> Z_NULL) then
  begin
    c^.mode := START;
    c^.lbits := Byte(bl);
    c^.dbits := Byte(bd);
    c^.ltree := tl;
    c^.dtree := td;
    {$IFDEF DEBUG}
    Tracev('inflate:       codes new');
    {$ENDIF}
  end;
  inflate_codes_new := c;
end;


function inflate_codes(var s : inflate_blocks_state;
                       var z : z_stream;
                       r : int) : int;
var
  j : uInt;               { temporary storage }
  t : pInflate_huft;      { temporary pointer }
  e : uInt;               { extra bits or operation }
  b : uLong;              { bit buffer }
  k : uInt;               { bits in bit buffer }
  p : pBytef;             { input data pointer }
  n : uInt;               { bytes available there }
  q : pBytef;             { output window write pointer }
  m : uInt;               { bytes to end of window or read pointer }
  f : pBytef;             { pointer to copy strings from }
var
  c : pInflate_codes_state;
begin
  c := s.sub.decode.codes;  { codes state }

  { copy input/output information to locals }
  p := z.next_in;
  n := z.avail_in;
  b := s.bitb;
  k := s.bitk;
  q := s.write;
  if ptr2int(q) < ptr2int(s.read) then
    m := uInt(ptr2int(s.read)-ptr2int(q)-1)
  else
    m := uInt(ptr2int(s.zend)-ptr2int(q));

  { process input and output based on current state }
  while True do
  case (c^.mode) of
    { waiting for "i:"=input, "o:"=output, "x:"=nothing }
  START:         { x: set up for LEN }
    begin
{$ifndef SLOW}
      if (m >= 258) and (n >= 10) then
      begin
        {UPDATE}
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
        z.next_in := p;
        s.write := q;

        r := inflate_fast(c^.lbits, c^.dbits, c^.ltree, c^.dtree, s, z);
        {LOAD}
        p := z.next_in;
        n := z.avail_in;
        b := s.bitb;
        k := s.bitk;
        q := s.write;
        if ptr2int(q) < ptr2int(s.read) then
          m := uInt(ptr2int(s.read)-ptr2int(q)-1)
        else
          m := uInt(ptr2int(s.zend)-ptr2int(q));

        if (r <> Z_OK) then
        begin
          if (r = Z_STREAM_END) then
            c^.mode := WASH
          else
            c^.mode := BADCODE;
          continue;    { break for switch-statement in C }
        end;
      end;
{$endif} { not SLOW }
      c^.sub.code.need := c^.lbits;
      c^.sub.code.tree := c^.ltree;
      c^.mode := LEN;  { falltrough }
    end;
  LEN:           { i: get length/literal/eob next }
    begin
      j := c^.sub.code.need;
      {NEEDBITS(j);}
      while (k < j) do
      begin
        {NEEDBYTE;}
        if (n <> 0) then
          r :=Z_OK
        else
        begin
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_codes := inflate_flush(s,z,r);
          exit;
        end;
        Dec(n);
        b := b or (uLong(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      t := c^.sub.code.tree;
      Inc(t, uInt(b) and inflate_mask[j]);
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      Dec(k, t^.bits);

      e := uInt(t^.exop);
      if (e = 0) then            { literal }
      begin
        c^.sub.lit := t^.base;
       {$IFDEF DEBUG}
        if (t^.base >= $20) and (t^.base < $7f) then
          Tracevv('inflate:         literal '+AnsiChar(t^.base))
        else
          Tracevv('inflate:         literal '+IntToStr(t^.base));
        {$ENDIF}          
        c^.mode := LIT;
        continue;  { break switch statement }
      end;
      if (e and 16 <> 0) then            { length }
      begin
        c^.sub.copy.get := e and 15;
        c^.len := t^.base;
        c^.mode := LENEXT;
        continue;         { break C-switch statement }
      end;
      if (e and 64 = 0) then             { next table }
      begin
        c^.sub.code.need := e;
        c^.sub.code.tree := @huft_ptr(t)^[t^.base];
        continue;         { break C-switch statement }
      end;
      if (e and 32 <> 0) then            { end of block }
      begin
        {$IFDEF DEBUG}
        Tracevv('inflate:         end of block');
        {$ENDIF}        
        c^.mode := WASH;
        continue;         { break C-switch statement }
      end;
      c^.mode := BADCODE;        { invalid code }
      z.msg := 'invalid literal/length code';
      r := Z_DATA_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  LENEXT:        { i: getting length extra (have base) }
    begin
      j := c^.sub.copy.get;
      {NEEDBITS(j);}
      while (k < j) do
      begin
        {NEEDBYTE;}
        if (n <> 0) then
          r :=Z_OK
        else
        begin
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_codes := inflate_flush(s,z,r);
          exit;
        end;
        Dec(n);
        b := b or (uLong(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      Inc(c^.len, uInt(b and inflate_mask[j]));
      {DUMPBITS(j);}
      b := b shr j;
      Dec(k, j);

      c^.sub.code.need := c^.dbits;
      c^.sub.code.tree := c^.dtree;
      {$IFDEF DEBUG}
      Tracevv('inflate:         length '+IntToStr(c^.len));
      {$ENDIF}
      c^.mode := DIST;
      { falltrough }
    end;
  DIST:          { i: get distance next }
    begin
      j := c^.sub.code.need;
      {NEEDBITS(j);}
      while (k < j) do
      begin
        {NEEDBYTE;}
        if (n <> 0) then
          r :=Z_OK
        else
        begin
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_codes := inflate_flush(s,z,r);
          exit;
        end;
        Dec(n);
        b := b or (uLong(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      t := @huft_ptr(c^.sub.code.tree)^[uInt(b) and inflate_mask[j]];
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      Dec(k, t^.bits);

      e := uInt(t^.exop);
      if (e and 16 <> 0) then            { distance }
      begin
        c^.sub.copy.get := e and 15;
        c^.sub.copy.dist := t^.base;
        c^.mode := DISTEXT;
        continue;     { break C-switch statement }
      end;
      if (e and 64 = 0) then     { next table }
      begin
        c^.sub.code.need := e;
        c^.sub.code.tree := @huft_ptr(t)^[t^.base];
        continue;     { break C-switch statement }
      end;
      c^.mode := BADCODE;        { invalid code }
      z.msg := 'invalid distance code';
      r := Z_DATA_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  DISTEXT:       { i: getting distance extra }
    begin
      j := c^.sub.copy.get;
      {NEEDBITS(j);}
      while (k < j) do
      begin
        {NEEDBYTE;}
        if (n <> 0) then
          r :=Z_OK
        else
        begin
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_codes := inflate_flush(s,z,r);
          exit;
        end;
        Dec(n);
        b := b or (uLong(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      Inc(c^.sub.copy.dist, uInt(b) and inflate_mask[j]);
      {DUMPBITS(j);}
      b := b shr j;
      Dec(k, j);
      {$IFDEF DEBUG}
      Tracevv('inflate:         distance '+ IntToStr(c^.sub.copy.dist));
      {$ENDIF}
      c^.mode := COPY;
      { falltrough }
    end;
  COPY:          { o: copying bytes in window, waiting for space }
    begin
      f := q;
      Dec(f, c^.sub.copy.dist);
      if (uInt(ptr2int(q) - ptr2int(s.window)) < c^.sub.copy.dist) then
      begin
        f := s.zend;
        Dec(f, c^.sub.copy.dist - uInt(ptr2int(q) - ptr2int(s.window)));
      end;

      while (c^.len <> 0) do
      begin
        {NEEDOUT}
        if (m = 0) then
        begin
          {WRAP}
          if (q = s.zend) and (s.read <> s.window) then
          begin
            q := s.window;
            if ptr2int(q) < ptr2int(s.read) then
              m := uInt(ptr2int(s.read)-ptr2int(q)-1)
            else
              m := uInt(ptr2int(s.zend)-ptr2int(q));
          end;

          if (m = 0) then
          begin
            {FLUSH}
            s.write := q;
            r := inflate_flush(s,z,r);
            q := s.write;
            if ptr2int(q) < ptr2int(s.read) then
              m := uInt(ptr2int(s.read)-ptr2int(q)-1)
            else
              m := uInt(ptr2int(s.zend)-ptr2int(q));

            {WRAP}
            if (q = s.zend) and (s.read <> s.window) then
            begin
              q := s.window;
              if ptr2int(q) < ptr2int(s.read) then
                m := uInt(ptr2int(s.read)-ptr2int(q)-1)
              else
                m := uInt(ptr2int(s.zend)-ptr2int(q));
            end;

            if (m = 0) then
            begin
              {UPDATE}
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
              z.next_in := p;
              s.write := q;
              inflate_codes := inflate_flush(s,z,r);
              exit;
            end;
          end;
        end;
        r := Z_OK;

        {OUTBYTE( *f++)}
        q^ := f^;
        Inc(q);
        Inc(f);
        Dec(m);

        if (f = s.zend) then
          f := s.window;
        Dec(c^.len);
      end;
      c^.mode := START;
      { C-switch break; not needed }
    end;
  LIT:           { o: got literal, waiting for output space }
    begin
      {NEEDOUT}
      if (m = 0) then
      begin
        {WRAP}
        if (q = s.zend) and (s.read <> s.window) then
        begin
          q := s.window;
          if ptr2int(q) < ptr2int(s.read) then
            m := uInt(ptr2int(s.read)-ptr2int(q)-1)
          else
            m := uInt(ptr2int(s.zend)-ptr2int(q));
        end;

        if (m = 0) then
        begin
          {FLUSH}
          s.write := q;
          r := inflate_flush(s,z,r);
          q := s.write;
          if ptr2int(q) < ptr2int(s.read) then
            m := uInt(ptr2int(s.read)-ptr2int(q)-1)
          else
            m := uInt(ptr2int(s.zend)-ptr2int(q));

          {WRAP}
          if (q = s.zend) and (s.read <> s.window) then
          begin
            q := s.window;
            if ptr2int(q) < ptr2int(s.read) then
              m := uInt(ptr2int(s.read)-ptr2int(q)-1)
            else
              m := uInt(ptr2int(s.zend)-ptr2int(q));
          end;

          if (m = 0) then
          begin
            {UPDATE}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;
            inflate_codes := inflate_flush(s,z,r);
            exit;
          end;
        end;
      end;
      r := Z_OK;

      {OUTBYTE(c^.sub.lit);}
      q^ := c^.sub.lit;
      Inc(q);
      Dec(m);

      c^.mode := START;
      {break;}
    end;
  WASH:          { o: got eob, possibly more output }
    begin
      {$ifdef patch112}
      if (k > 7) then           { return unused byte, if any }
      begin
        {$IFDEF DEBUG}
        Assert(k < 16, 'inflate_codes grabbed too many bytes');
        {$ENDIF}
        Dec(k, 8);
        Inc(n);
        Dec(p);                    { can always return one }
      end;
      {$endif}
      {FLUSH}
      s.write := q;
      r := inflate_flush(s,z,r);
      q := s.write;
      if ptr2int(q) < ptr2int(s.read) then
        m := uInt(ptr2int(s.read)-ptr2int(q)-1)
      else
        m := uInt(ptr2int(s.zend)-ptr2int(q));

      if (s.read <> s.write) then
      begin
        {UPDATE}
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
        z.next_in := p;
        s.write := q;
        inflate_codes := inflate_flush(s,z,r);
        exit;
      end;
      c^.mode := ZEND;
      { falltrough }
    end;

  ZEND:
    begin
      r := Z_STREAM_END;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  BADCODE:       { x: got error }
    begin
      r := Z_DATA_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  else
    begin
      r := Z_STREAM_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  end;
{NEED_DUMMY_RETURN - Delphi2+ dumb compilers complain without this }
  inflate_codes := Z_STREAM_ERROR;
end;


procedure inflate_codes_free(c : pInflate_codes_state;
                             var z : z_stream);
begin
  ZFREE(z, c);
  {$IFDEF DEBUG}  
  Tracev('inflate:       codes free');
  {$ENDIF}
end;

{== iminffast implementation ==}

{ Called with number of bytes left to write in window at least 258
  (the maximum string length) and number of input bytes available
  at least ten.  The ten bytes are six bytes for the longest length/
  distance pair plus four bytes for overloading the bit buffer. }

function inflate_fast( bl : uInt;
                       bd : uInt;
                       tl : pInflate_huft;
                       td : pInflate_huft;
                      var s : inflate_blocks_state;
                      var z : z_stream) : int;

var
  t : pInflate_huft;      { temporary pointer }
  e : uInt;               { extra bits or operation }
  b : uLong;              { bit buffer }
  k : uInt;               { bits in bit buffer }
  p : pBytef;             { input data pointer }
  n : uInt;               { bytes available there }
  q : pBytef;             { output window write pointer }
  m : uInt;               { bytes to end of window or read pointer }
  ml : uInt;              { mask for literal/length tree }
  md : uInt;              { mask for distance tree }
  c : uInt;               { bytes to copy }
  d : uInt;               { distance back to copy from }
  r : pBytef;             { copy source pointer }
begin
  { load input, output, bit values (macro LOAD) }
  p := z.next_in;
  n := z.avail_in;
  b := s.bitb;
  k := s.bitk;
  q := s.write;
  if ptr2int(q) < ptr2int(s.read) then
    m := uInt(ptr2int(s.read)-ptr2int(q)-1)
  else
    m := uInt(ptr2int(s.zend)-ptr2int(q));

  { initialize masks }
  ml := inflate_mask[bl];
  md := inflate_mask[bd];

  { do until not enough input or output space for fast loop }
  repeat                      { assume called with (m >= 258) and (n >= 10) }
    { get literal/length code }
    {GRABBITS(20);}             { max bits for literal/length code }
    while (k < 20) do
    begin
      Dec(n);
      b := b or (uLong(p^) shl k);
      Inc(p);
      Inc(k, 8);
    end;

    t := @(huft_ptr(tl)^[uInt(b) and ml]);

    e := t^.exop;
    if (e = 0) then
    begin
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      Dec(k, t^.bits);
     {$IFDEF DEBUG}
      if (t^.base >= $20) and (t^.base < $7f) then
        Tracevv('inflate:         * literal '+AnsiChar(t^.base))
      else
        Tracevv('inflate:         * literal '+ IntToStr(t^.base));
      {$ENDIF}
      q^ := Byte(t^.base);
      Inc(q);
      Dec(m);
      continue;
    end;
    repeat
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      Dec(k, t^.bits);

      if (e and 16 <> 0) then
      begin
        { get extra bits for length }
        e := e and 15;
        c := t^.base + (uInt(b) and inflate_mask[e]);
        {DUMPBITS(e);}
        b := b shr e;
        Dec(k, e);
        {$IFDEF DEBUG}
        Tracevv('inflate:         * length ' + IntToStr(c));
        {$ENDIF}
        { decode distance base of block to copy }
        {GRABBITS(15);}           { max bits for distance code }
        while (k < 15) do
        begin
          Dec(n);
          b := b or (uLong(p^) shl k);
          Inc(p);
          Inc(k, 8);
        end;

        t := @huft_ptr(td)^[uInt(b) and md];
        e := t^.exop;
        repeat
          {DUMPBITS(t^.bits);}
          b := b shr t^.bits;
          Dec(k, t^.bits);

          if (e and 16 <> 0) then
          begin
            { get extra bits to add to distance base }
            e := e and 15;
            {GRABBITS(e);}         { get extra bits (up to 13) }
            while (k < e) do
            begin
              Dec(n);
              b := b or (uLong(p^) shl k);
              Inc(p);
              Inc(k, 8);
            end;

            d := t^.base + (uInt(b) and inflate_mask[e]);
            {DUMPBITS(e);}
            b := b shr e;
            Dec(k, e);

            {$IFDEF DEBUG}
            Tracevv('inflate:         * distance '+IntToStr(d));
            {$ENDIF}
            { do the copy }
            Dec(m, c);
            if (uInt(ptr2int(q) - ptr2int(s.window)) >= d) then     { offset before dest }
            begin                                  {  just copy }
              r := q;
              Dec(r, d);
              q^ := r^;  Inc(q); Inc(r); Dec(c); { minimum count is three, }
              q^ := r^;  Inc(q); Inc(r); Dec(c); { so unroll loop a little }
            end
            else                        { else offset after destination }
            begin
              e := d - uInt(ptr2int(q) - ptr2int(s.window)); { bytes from offset to end }
              r := s.zend;
              Dec(r, e);                  { pointer to offset }
              if (c > e) then             { if source crosses, }
              begin
                Dec(c, e);                { copy to end of window }
                repeat
                  q^ := r^;
                  Inc(q);
                  Inc(r);
                  Dec(e);
                until (e=0);
                r := s.window;           { copy rest from start of window }
              end;
            end;
            repeat                       { copy all or what's left }
              q^ := r^;
              Inc(q);
              Inc(r);
              Dec(c);
            until (c = 0);
            break;
          end
          else
            if (e and 64 = 0) then
            begin
              Inc(t, t^.base + (uInt(b) and inflate_mask[e]));
              e := t^.exop;
            end
          else
          begin
            z.msg := 'invalid distance code';
            {UNGRAB}
            c := z.avail_in-n;
            if (k shr 3) < c then
              c := k shr 3;
            Inc(n, c);
            Dec(p, c);
            Dec(k, c shl 3);
            {UPDATE}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;

            inflate_fast := Z_DATA_ERROR;
            exit;
          end;
        until FALSE;
        break;
      end;
      if (e and 64 = 0) then
      begin
         {t += t->base;
          e = (t += ((uInt)b & inflate_mask[e]))->exop;}

        Inc(t, t^.base + (uInt(b) and inflate_mask[e]));
        e := t^.exop;
        if (e = 0) then
        begin
          {DUMPBITS(t^.bits);}
          b := b shr t^.bits;
          Dec(k, t^.bits);

         {$IFDEF DEBUG}
          if (t^.base >= $20) and (t^.base < $7f) then
            Tracevv('inflate:         * literal '+AnsiChar(t^.base))
          else
            Tracevv('inflate:         * literal '+IntToStr(t^.base));
          {$ENDIF}            
          q^ := Byte(t^.base);
          Inc(q);
          Dec(m);
          break;
        end;
      end
      else
        if (e and 32 <> 0) then
        begin
          {$IFDEF DEBUG}
          Tracevv('inflate:         * end of block');
          {$ENDIF}
          {UNGRAB}
          c := z.avail_in-n;
          if (k shr 3) < c then
            c := k shr 3;
          Inc(n, c);
          Dec(p, c);
          Dec(k, c shl 3);
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_fast := Z_STREAM_END;
          exit;
        end
        else
        begin
          z.msg := 'invalid literal/length code';
          {UNGRAB}
          c := z.avail_in-n;
          if (k shr 3) < c then
            c := k shr 3;
          Inc(n, c);
          Dec(p, c);
          Dec(k, c shl 3);
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_fast := Z_DATA_ERROR;
          exit;
        end;
    until FALSE;
  until (m < 258) or (n < 10);

  { not enough input or output--restore pointers and return }
  {UNGRAB}
  c := z.avail_in-n;
  if (k shr 3) < c then
    c := k shr 3;
  Inc(n, c);
  Dec(p, c);
  Dec(k, c shl 3);
  {UPDATE}
  s.bitb := b;
  s.bitk := k;
  z.avail_in := n;
  Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
  z.next_in := p;
  s.write := q;
  inflate_fast := Z_OK;
end;

{== iminftrees implementation ==}

const
 inflate_copyright = 'inflate 1.1.2 Copyright 1995-1998 Mark Adler';

{
  If you use the zlib library in a product, an acknowledgment is welcome
  in the documentation of your product. If for some reason you cannot
  include such an acknowledgment, I would appreciate that you keep this
  copyright string in the executable of your product.
}


const
{ Tables for deflate from PKZIP's appnote.txt. }
  cplens : Array [0..30] Of uInt  { Copy lengths for literal codes 257..285 }
     = (3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
        35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0);
        { actually lengths - 2; also see note #13 above about 258 }

  invalid_code = 112;

  cplext : Array [0..30] Of uInt  { Extra bits for literal codes 257..285 }
     = (0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
        3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, invalid_code, invalid_code);

  cpdist : Array [0..29] Of uInt { Copy offsets for distance codes 0..29 }
     = (1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
        257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
        8193, 12289, 16385, 24577);

  cpdext : Array [0..29] Of uInt { Extra bits for distance codes }
     = (0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
        7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
        12, 12, 13, 13);

{  Huffman code decoding is performed using a multi-level table lookup.
   The fastest way to decode is to simply build a lookup table whose
   size is determined by the longest code.  However, the time it takes
   to build this table can also be a factor if the data being decoded
   is not very long.  The most common codes are necessarily the
   shortest codes, so those codes dominate the decoding time, and hence
   the speed.  The idea is you can have a shorter table that decodes the
   shorter, more probable codes, and then point to subsidiary tables for
   the longer codes.  The time it costs to decode the longer codes is
   then traded against the time it takes to make longer tables.

   This results of this trade are in the variables lbits and dbits
   below.  lbits is the number of bits the first level table for literal/
   length codes can decode in one step, and dbits is the same thing for
   the distance codes.  Subsequent tables are also less than or equal to
   those sizes.  These values may be adjusted either when all of the
   codes are shorter than that, in which case the longest code length in
   bits is used, or when the shortest code is *longer* than the requested
   table size, in which case the length of the shortest code in bits is
   used.

   There are two different values for the two tables, since they code a
   different number of possibilities each.  The literal/length table
   codes 286 possible values, or in a flat code, a little over eight
   bits.  The distance table codes 30 possible values, or a little less
   than five bits, flat.  The optimum values for speed end up being
   about one bit more than those, so lbits is 8+1 and dbits is 5+1.
   The optimum values may differ though from machine to machine, and
   possibly even between compilers.  Your mileage may vary. }


{ If BMAX needs to be larger than 16, then h and x[] should be uLong. }
const
  BMAX = 15;         { maximum bit length of any code }

{$DEFINE USE_PTR}

function huft_build(
var b : array of uIntf;    { code lengths in bits (all assumed <= BMAX) }
    n : uInt;              { number of codes (assumed <= N_MAX) }
    s : uInt;              { number of simple-valued codes (0..s-1) }
const d : array of uIntf;  { list of base values for non-simple codes }
{ array of word }
const e : array of uIntf;  { list of extra bits for non-simple codes }
{ array of byte }
  t : ppInflate_huft;     { result: starting table }
var m : uIntf;             { maximum lookup bits, returns actual }
var hp : array of inflate_huft;  { space for trees }
var hn : uInt;             { hufts used in space }
var v : array of uIntf     { working area: values in order of bit length }
   ) : int;
{ Given a list of code lengths and a maximum table size, make a set of
  tables to decode that set of codes.  Return Z_OK on success, Z_BUF_ERROR
  if the given code set is incomplete (the tables are still built in this
  case), Z_DATA_ERROR if the input is invalid (an over-subscribed set of
  lengths), or Z_MEM_ERROR if not enough memory. }
Var
  a : uInt;                     { counter for codes of length k }
  c : Array [0..BMAX] Of uInt;  { bit length count table }
  f : uInt;                     { i repeats in table every f entries }
  g : int;                      { maximum code length }
  h : int;                      { table level }
  i : uInt;  {register}         { counter, current code }
  j : uInt;  {register}         { counter }
  k : Int;   {register}         { number of bits in current code }
  l : int;			{ bits per table (returned in m) }
  mask : uInt;                  { (1 shl w) - 1, to avoid cc -O bug on HP }
  p : ^uIntf; {register}        { pointer into c[], b[], or v[] }
  q : pInflate_huft;            { points to current table }
  r : inflate_huft;             { table entry for structure assignment }
  u : Array [0..BMAX-1] Of pInflate_huft; { table stack }
  w : int;   {register}         { bits before this table = (l*h) }
  x : Array [0..BMAX] Of uInt;  { bit offsets, then code stack }
  {$IFDEF USE_PTR}
  xp : puIntf;                  { pointer into x }
  {$ELSE}
  xp : uInt;
  {$ENDIF}
  y : int;                      { number of dummy codes added }
  z : uInt;                     { number of entries in current table }
Begin
  { Generate counts for each bit length }
  FillChar(c,SizeOf(c),0) ;     { clear c[] }

  for i := 0 to n-1 do
    Inc (c[b[i]]);              { assume all entries <= BMAX }

  If (c[0] = n) Then            { null input--all zero length codes }
  Begin
    t^ := pInflate_huft(NIL);
    m := 0 ;
    huft_build := Z_OK ;
    Exit;
  End ;

  { Find minimum and maximum length, bound [m] by those }
  l := m;
  for j:=1 To BMAX do
    if (c[j] <> 0) then
      break;
  k := j ;                      { minimum code length }
  if (uInt(l) < j) then
    l := j;
  for i := BMAX downto 1 do
    if (c[i] <> 0) then
      break ;
  g := i ;                      { maximum code length }
  if (uInt(l) > i) then
     l := i;
  m := l;

  { Adjust last length count to fill out codes, if needed }
  y := 1 shl j ;
  while (j < i) do
  begin
    Dec(y, c[j]) ;
    if (y < 0) then
    begin
      huft_build := Z_DATA_ERROR;   { bad input: more codes than bits }
      exit;
    end ;
    Inc(j) ;
    y := y shl 1
  end;
  Dec (y, c[i]) ;
  if (y < 0) then
  begin
    huft_build := Z_DATA_ERROR;     { bad input: more codes than bits }
    exit;
  end;
  Inc(c[i], y);

  { Generate starting offsets into the value table FOR each length }
  {$IFDEF USE_PTR}
  x[1] := 0;
  j := 0;

  p := @c[1];
  xp := @x[2];

  dec(i);               { note that i = g from above }
  WHILE (i > 0) DO
  BEGIN
    inc(j, p^);
    xp^ := j;
    inc(p);
    inc(xp);
    dec(i);
  END;
  {$ELSE}
  x[1] := 0;
  j := 0 ;
  for i := 1 to g do
  begin
    x[i] := j;
    Inc(j, c[i]);
  end;
  {$ENDIF}

  { Make a table of values in order of bit lengths }
  for i := 0 to n-1 do
  begin
    j := b[i];
    if (j <> 0) then
    begin
      v[ x[j] ] := i;
      Inc(x[j]);
    end;
  end;
  n := x[g];                     { set n to length of v }

  { Generate the Huffman codes and for each, make the table entries }
  i := 0 ;
  x[0] := 0 ;                   { first Huffman code is zero }
  p := Addr(v) ;                { grab values in bit order }
  h := -1 ;                     { no tables yet--level -1 }
  w := -l ;                     { bits decoded = (l*h) }

  u[0] := pInflate_huft(NIL);   { just to keep compilers happy }
  q := pInflate_huft(NIL);      { ditto }
  z := 0 ;                      { ditto }

  { go through the bit lengths (k already is bits in shortest code) }
  while (k <= g) Do
  begin
    a := c[k] ;
    while (a<>0) Do
    begin
      Dec (a) ;
      { here i is the Huffman code of length k bits for value p^ }
      { make tables up to required level }
      while (k > w + l) do
      begin

        Inc (h) ;
        Inc (w, l);              { add bits already decoded }
                                 { previous table always l bits }
        { compute minimum size table less than or equal to l bits }

        { table size upper limit }
        z := g - w;
        If (z > uInt(l)) Then
          z := l;

        { try a k-w bit table }
        j := k - w;
        f := 1 shl j;
        if (f > a+1) Then        { too few codes for k-w bit table }
        begin
          Dec(f, a+1);           { deduct codes from patterns left }
          {$IFDEF USE_PTR}
          xp := Addr(c[k]);

          if (j < z) then
          begin
            Inc(j);
            while (j < z) do
            begin                { try smaller tables up to z bits }
              f := f shl 1;
              Inc (xp) ;
              If (f <= xp^) Then
                break;           { enough codes to use up j bits }
              Dec(f, xp^);       { else deduct codes from patterns }
              Inc(j);
            end;
          end;
          {$ELSE}
          xp := k;

          if (j < z) then
          begin
            Inc (j) ;
            While (j < z) Do
            begin                 { try smaller tables up to z bits }
              f := f * 2;
              Inc (xp) ;
              if (f <= c[xp]) then
                Break ;           { enough codes to use up j bits }
              Dec (f, c[xp]) ;      { else deduct codes from patterns }
              Inc (j);
            end;
          end;
          {$ENDIF}
        end;

        z := 1 shl j;            { table entries for j-bit table }

        { allocate new table }
        if (hn + z > MANY) then { (note: doesn't matter for fixed) }
        begin
          huft_build := Z_MEM_ERROR;     { not enough memory }
          exit;
        end;

        q := @hp[hn];
        u[h] := q;
        Inc(hn, z);

        { connect to last table, if there is one }
        if (h <> 0) then
        begin
          x[h] := i;             { save pattern for backing up }
          r.bits := Byte(l);     { bits to dump before this table }
          r.exop := Byte(j);     { bits in this table }
          j := i shr (w - l);
          {r.base := uInt( q - u[h-1] -j);}   { offset to this table }
          r.base := (ptr2int(q) - ptr2int(u[h-1]) ) div sizeof(q^) - j;
          huft_Ptr(u[h-1])^[j] := r;  { connect to last table }
        end
        else
          t^ := q;               { first table is returned result }
      end;

      { set up table entry in r }
      r.bits := Byte(k - w);

      { C-code: if (p >= v + n) - see ZUTIL.PAS for comments }

      if ptr2int(p)>=ptr2int(@(v[n])) then  { also works under DPMI ?? }
        r.exop := 128 + 64                  { out of values--invalid code }
      else
        if (p^ < s) then
        begin
          if (p^ < 256) then     { 256 is end-of-block code }
            r.exop := 0
          Else
            r.exop := 32 + 64;   { EOB_code; }
          r.base := p^;          { simple code is just the value }
          Inc(p);
        end
        Else
        begin
          r.exop := Byte(e[p^-s] + 16 + 64);  { non-simple--look up in lists }
          r.base := d[p^-s];
          Inc (p);
        end ;

      { fill code-like entries with r }
      f := 1 shl (k - w);
      j := i shr w;
      while (j < z) do
      begin
        huft_Ptr(q)^[j] := r;
        Inc(j, f);
      end;

      { backwards increment the k-bit code i }
      j := 1 shl (k-1) ;
      while (i and j) <> 0 do
      begin
        i := i xor j;         { bitwise exclusive or }
        j := j shr 1
      end ;
      i := i xor j;

      { backup over finished tables }
      mask := (1 shl w) - 1;   { needed on HP, cc -O bug }
      while ((i and mask) <> x[h]) do
      begin
        Dec(h);                { don't need to update q }
        Dec(w, l);
        mask := (1 shl w) - 1;
      end;

    end;

    Inc(k);
  end;

  { Return Z_BUF_ERROR if we were given an incomplete table }
  if (y <> 0) And (g <> 1) then
    huft_build := Z_BUF_ERROR
  else
    huft_build := Z_OK;
end; { huft_build}


function inflate_trees_bits(
  var c : array of uIntf;  { 19 code lengths }
  var bb : uIntf;          { bits tree desired/actual depth }
  var tb : pinflate_huft;  { bits tree result }
  var hp : array of Inflate_huft;      { space for trees }
  var z : z_stream         { for messages }
    ) : int;
var
  r : int;
  hn : uInt;          { hufts used in space }
  v : PuIntArray;     { work area for huft_build }
begin
  hn := 0;
  v := PuIntArray( ZALLOC(z, 19, sizeof(uInt)) );
  if (v = Z_NULL) then
  begin
    inflate_trees_bits := Z_MEM_ERROR;
    exit;
  end;

  r := huft_build(c, 19, 19, cplens, cplext,
                             {puIntf(Z_NULL), puIntf(Z_NULL),}
                  @tb, bb, hp, hn, v^);
  if (r = Z_DATA_ERROR) then
    z.msg := 'oversubscribed dynamic bit lengths tree'
  else
    if (r = Z_BUF_ERROR) or (bb = 0) then
    begin
      z.msg := 'incomplete dynamic bit lengths tree';
      r := Z_DATA_ERROR;
    end;
  ZFREE(z, v);
  inflate_trees_bits := r;
end;


function inflate_trees_dynamic(
    nl : uInt;                    { number of literal/length codes }
    nd : uInt;                    { number of distance codes }
    var c : Array of uIntf;           { that many (total) code lengths }
    var bl : uIntf;          { literal desired/actual bit depth }
    var bd : uIntf;          { distance desired/actual bit depth }
var tl : pInflate_huft;           { literal/length tree result }
var td : pInflate_huft;           { distance tree result }
var hp : array of Inflate_huft;   { space for trees }
var z : z_stream                  { for messages }
     ) : int;
var
  r : int;
  hn : uInt;          { hufts used in space }
  v : PuIntArray;     { work area for huft_build }
begin
  hn := 0;
  { allocate work area }
  v := PuIntArray( ZALLOC(z, 288, sizeof(uInt)) );
  if (v = Z_NULL) then
  begin
    inflate_trees_dynamic := Z_MEM_ERROR;
    exit;
  end;

  { build literal/length tree }
  r := huft_build(c, nl, 257, cplens, cplext, @tl, bl, hp, hn, v^);
  if (r <> Z_OK) or (bl = 0) then
  begin
    if (r = Z_DATA_ERROR) then
      z.msg := 'oversubscribed literal/length tree'
    else
      if (r <> Z_MEM_ERROR) then
      begin
        z.msg := 'incomplete literal/length tree';
        r := Z_DATA_ERROR;
      end;

    ZFREE(z, v);
    inflate_trees_dynamic := r;
    exit;
  end;

  { build distance tree }
  r := huft_build(puIntArray(@c[nl])^, nd, 0,
                  cpdist, cpdext, @td, bd, hp, hn, v^);
  if (r <> Z_OK) or ((bd = 0) and (nl > 257)) then
  begin
    if (r = Z_DATA_ERROR) then
      z.msg := 'oversubscribed literal/length tree'
    else
      if (r = Z_BUF_ERROR) then
      begin
{$ifdef PKZIP_BUG_WORKAROUND}
        r := Z_OK;
      end;
{$else}
        z.msg := 'incomplete literal/length tree';
        r := Z_DATA_ERROR;
      end
      else
        if (r <> Z_MEM_ERROR) then
        begin
          z.msg := 'empty distance tree with lengths';
          r := Z_DATA_ERROR;
        end;
    ZFREE(z, v);
    inflate_trees_dynamic := r;
    exit;
{$endif}
  end;

  { done }
  ZFREE(z, v);
  inflate_trees_dynamic := Z_OK;
end;

{$UNDEF BUILDFIXED}

{ build fixed tables only once--keep them here }
{$IFNDEF BUILDFIXED}
{ locals }
var
  fixed_built : Boolean = false;
const
  FIXEDH = 544;      { number of hufts used by fixed tables }
var
  fixed_mem : array[0..FIXEDH-1] of inflate_huft;
  fixed_bl : uInt;
  fixed_bd : uInt;
  fixed_tl : pInflate_huft;
  fixed_td : pInflate_huft;

{$ELSE}

{ inffixed.h -- table for decoding fixed codes }

const
  fixed_bl = uInt(9);
const
  fixed_bd = uInt(5);
const
  fixed_tl : array [0..288-1] of inflate_huft = (
    Exop,             { number of extra bits or operation }
    bits : Byte;      { number of bits in this code or subcode }
    {pad : uInt;}       { pad structure to a power of 2 (4 bytes for }
                      {  16-bit, 8 bytes for 32-bit int's) }
    base : uInt;      { literal, length base, or distance base }
                      { or table offset }

    ((96,7),256), ((0,8),80), ((0,8),16), ((84,8),115), ((82,7),31),
    ((0,8),112), ((0,8),48), ((0,9),192), ((80,7),10), ((0,8),96),
    ((0,8),32), ((0,9),160), ((0,8),0), ((0,8),128), ((0,8),64),
    ((0,9),224), ((80,7),6), ((0,8),88), ((0,8),24), ((0,9),144),
    ((83,7),59), ((0,8),120), ((0,8),56), ((0,9),208), ((81,7),17),
    ((0,8),104), ((0,8),40), ((0,9),176), ((0,8),8), ((0,8),136),
    ((0,8),72), ((0,9),240), ((80,7),4), ((0,8),84), ((0,8),20),
    ((85,8),227), ((83,7),43), ((0,8),116), ((0,8),52), ((0,9),200),
    ((81,7),13), ((0,8),100), ((0,8),36), ((0,9),168), ((0,8),4),
    ((0,8),132), ((0,8),68), ((0,9),232), ((80,7),8), ((0,8),92),
    ((0,8),28), ((0,9),152), ((84,7),83), ((0,8),124), ((0,8),60),
    ((0,9),216), ((82,7),23), ((0,8),108), ((0,8),44), ((0,9),184),
    ((0,8),12), ((0,8),140), ((0,8),76), ((0,9),248), ((80,7),3),
    ((0,8),82), ((0,8),18), ((85,8),163), ((83,7),35), ((0,8),114),
    ((0,8),50), ((0,9),196), ((81,7),11), ((0,8),98), ((0,8),34),
    ((0,9),164), ((0,8),2), ((0,8),130), ((0,8),66), ((0,9),228),
    ((80,7),7), ((0,8),90), ((0,8),26), ((0,9),148), ((84,7),67),
    ((0,8),122), ((0,8),58), ((0,9),212), ((82,7),19), ((0,8),106),
    ((0,8),42), ((0,9),180), ((0,8),10), ((0,8),138), ((0,8),74),
    ((0,9),244), ((80,7),5), ((0,8),86), ((0,8),22), ((192,8),0),
    ((83,7),51), ((0,8),118), ((0,8),54), ((0,9),204), ((81,7),15),
    ((0,8),102), ((0,8),38), ((0,9),172), ((0,8),6), ((0,8),134),
    ((0,8),70), ((0,9),236), ((80,7),9), ((0,8),94), ((0,8),30),
    ((0,9),156), ((84,7),99), ((0,8),126), ((0,8),62), ((0,9),220),
    ((82,7),27), ((0,8),110), ((0,8),46), ((0,9),188), ((0,8),14),
    ((0,8),142), ((0,8),78), ((0,9),252), ((96,7),256), ((0,8),81),
    ((0,8),17), ((85,8),131), ((82,7),31), ((0,8),113), ((0,8),49),
    ((0,9),194), ((80,7),10), ((0,8),97), ((0,8),33), ((0,9),162),
    ((0,8),1), ((0,8),129), ((0,8),65), ((0,9),226), ((80,7),6),
    ((0,8),89), ((0,8),25), ((0,9),146), ((83,7),59), ((0,8),121),
    ((0,8),57), ((0,9),210), ((81,7),17), ((0,8),105), ((0,8),41),
    ((0,9),178), ((0,8),9), ((0,8),137), ((0,8),73), ((0,9),242),
    ((80,7),4), ((0,8),85), ((0,8),21), ((80,8),258), ((83,7),43),
    ((0,8),117), ((0,8),53), ((0,9),202), ((81,7),13), ((0,8),101),
    ((0,8),37), ((0,9),170), ((0,8),5), ((0,8),133), ((0,8),69),
    ((0,9),234), ((80,7),8), ((0,8),93), ((0,8),29), ((0,9),154),
    ((84,7),83), ((0,8),125), ((0,8),61), ((0,9),218), ((82,7),23),
    ((0,8),109), ((0,8),45), ((0,9),186), ((0,8),13), ((0,8),141),
    ((0,8),77), ((0,9),250), ((80,7),3), ((0,8),83), ((0,8),19),
    ((85,8),195), ((83,7),35), ((0,8),115), ((0,8),51), ((0,9),198),
    ((81,7),11), ((0,8),99), ((0,8),35), ((0,9),166), ((0,8),3),
    ((0,8),131), ((0,8),67), ((0,9),230), ((80,7),7), ((0,8),91),
    ((0,8),27), ((0,9),150), ((84,7),67), ((0,8),123), ((0,8),59),
    ((0,9),214), ((82,7),19), ((0,8),107), ((0,8),43), ((0,9),182),
    ((0,8),11), ((0,8),139), ((0,8),75), ((0,9),246), ((80,7),5),
    ((0,8),87), ((0,8),23), ((192,8),0), ((83,7),51), ((0,8),119),
    ((0,8),55), ((0,9),206), ((81,7),15), ((0,8),103), ((0,8),39),
    ((0,9),174), ((0,8),7), ((0,8),135), ((0,8),71), ((0,9),238),
    ((80,7),9), ((0,8),95), ((0,8),31), ((0,9),158), ((84,7),99),
    ((0,8),127), ((0,8),63), ((0,9),222), ((82,7),27), ((0,8),111),
    ((0,8),47), ((0,9),190), ((0,8),15), ((0,8),143), ((0,8),79),
    ((0,9),254), ((96,7),256), ((0,8),80), ((0,8),16), ((84,8),115),
    ((82,7),31), ((0,8),112), ((0,8),48), ((0,9),193), ((80,7),10),
    ((0,8),96), ((0,8),32), ((0,9),161), ((0,8),0), ((0,8),128),
    ((0,8),64), ((0,9),225), ((80,7),6), ((0,8),88), ((0,8),24),
    ((0,9),145), ((83,7),59), ((0,8),120), ((0,8),56), ((0,9),209),
    ((81,7),17), ((0,8),104), ((0,8),40), ((0,9),177), ((0,8),8),
    ((0,8),136), ((0,8),72), ((0,9),241), ((80,7),4), ((0,8),84),
    ((0,8),20), ((85,8),227), ((83,7),43), ((0,8),116), ((0,8),52),
    ((0,9),201), ((81,7),13), ((0,8),100), ((0,8),36), ((0,9),169),
    ((0,8),4), ((0,8),132), ((0,8),68), ((0,9),233), ((80,7),8),
    ((0,8),92), ((0,8),28), ((0,9),153), ((84,7),83), ((0,8),124),
    ((0,8),60), ((0,9),217), ((82,7),23), ((0,8),108), ((0,8),44),
    ((0,9),185), ((0,8),12), ((0,8),140), ((0,8),76), ((0,9),249),
    ((80,7),3), ((0,8),82), ((0,8),18), ((85,8),163), ((83,7),35),
    ((0,8),114), ((0,8),50), ((0,9),197), ((81,7),11), ((0,8),98),
    ((0,8),34), ((0,9),165), ((0,8),2), ((0,8),130), ((0,8),66),
    ((0,9),229), ((80,7),7), ((0,8),90), ((0,8),26), ((0,9),149),
    ((84,7),67), ((0,8),122), ((0,8),58), ((0,9),213), ((82,7),19),
    ((0,8),106), ((0,8),42), ((0,9),181), ((0,8),10), ((0,8),138),
    ((0,8),74), ((0,9),245), ((80,7),5), ((0,8),86), ((0,8),22),
    ((192,8),0), ((83,7),51), ((0,8),118), ((0,8),54), ((0,9),205),
    ((81,7),15), ((0,8),102), ((0,8),38), ((0,9),173), ((0,8),6),
    ((0,8),134), ((0,8),70), ((0,9),237), ((80,7),9), ((0,8),94),
    ((0,8),30), ((0,9),157), ((84,7),99), ((0,8),126), ((0,8),62),
    ((0,9),221), ((82,7),27), ((0,8),110), ((0,8),46), ((0,9),189),
    ((0,8),14), ((0,8),142), ((0,8),78), ((0,9),253), ((96,7),256),
    ((0,8),81), ((0,8),17), ((85,8),131), ((82,7),31), ((0,8),113),
    ((0,8),49), ((0,9),195), ((80,7),10), ((0,8),97), ((0,8),33),
    ((0,9),163), ((0,8),1), ((0,8),129), ((0,8),65), ((0,9),227),
    ((80,7),6), ((0,8),89), ((0,8),25), ((0,9),147), ((83,7),59),
    ((0,8),121), ((0,8),57), ((0,9),211), ((81,7),17), ((0,8),105),
    ((0,8),41), ((0,9),179), ((0,8),9), ((0,8),137), ((0,8),73),
    ((0,9),243), ((80,7),4), ((0,8),85), ((0,8),21), ((80,8),258),
    ((83,7),43), ((0,8),117), ((0,8),53), ((0,9),203), ((81,7),13),
    ((0,8),101), ((0,8),37), ((0,9),171), ((0,8),5), ((0,8),133),
    ((0,8),69), ((0,9),235), ((80,7),8), ((0,8),93), ((0,8),29),
    ((0,9),155), ((84,7),83), ((0,8),125), ((0,8),61), ((0,9),219),
    ((82,7),23), ((0,8),109), ((0,8),45), ((0,9),187), ((0,8),13),
    ((0,8),141), ((0,8),77), ((0,9),251), ((80,7),3), ((0,8),83),
    ((0,8),19), ((85,8),195), ((83,7),35), ((0,8),115), ((0,8),51),
    ((0,9),199), ((81,7),11), ((0,8),99), ((0,8),35), ((0,9),167),
    ((0,8),3), ((0,8),131), ((0,8),67), ((0,9),231), ((80,7),7),
    ((0,8),91), ((0,8),27), ((0,9),151), ((84,7),67), ((0,8),123),
    ((0,8),59), ((0,9),215), ((82,7),19), ((0,8),107), ((0,8),43),
    ((0,9),183), ((0,8),11), ((0,8),139), ((0,8),75), ((0,9),247),
    ((80,7),5), ((0,8),87), ((0,8),23), ((192,8),0), ((83,7),51),
    ((0,8),119), ((0,8),55), ((0,9),207), ((81,7),15), ((0,8),103),
    ((0,8),39), ((0,9),175), ((0,8),7), ((0,8),135), ((0,8),71),
    ((0,9),239), ((80,7),9), ((0,8),95), ((0,8),31), ((0,9),159),
    ((84,7),99), ((0,8),127), ((0,8),63), ((0,9),223), ((82,7),27),
    ((0,8),111), ((0,8),47), ((0,9),191), ((0,8),15), ((0,8),143),
    ((0,8),79), ((0,9),255)
  );

const
  fixed_td : array[0..32-1] of inflate_huft = (
(Exop:80;bits:5;base:1),      (Exop:87;bits:5;base:257),   (Exop:83;bits:5;base:17),
(Exop:91;bits:5;base:4097),   (Exop:81;bits:5;base),       (Exop:89;bits:5;base:1025),
(Exop:85;bits:5;base:65),     (Exop:93;bits:5;base:16385), (Exop:80;bits:5;base:3),
(Exop:88;bits:5;base:513),    (Exop:84;bits:5;base:33),    (Exop:92;bits:5;base:8193),
(Exop:82;bits:5;base:9),      (Exop:90;bits:5;base:2049),  (Exop:86;bits:5;base:129),
(Exop:192;bits:5;base:24577), (Exop:80;bits:5;base:2),     (Exop:87;bits:5;base:385),
(Exop:83;bits:5;base:25),     (Exop:91;bits:5;base:6145),  (Exop:81;bits:5;base:7),
(Exop:89;bits:5;base:1537),   (Exop:85;bits:5;base:97),    (Exop:93;bits:5;base:24577),
(Exop:80;bits:5;base:4),      (Exop:88;bits:5;base:769),   (Exop:84;bits:5;base:49),
(Exop:92;bits:5;base:12289),  (Exop:82;bits:5;base:13),    (Exop:90;bits:5;base:3073),
(Exop:86;bits:5;base:193),    (Exop:192;bits:5;base:24577)
  );
{$ENDIF}

function inflate_trees_fixed(
var bl : uInt;               { literal desired/actual bit depth }
var bd : uInt;               { distance desired/actual bit depth }
var tl : pInflate_huft;      { literal/length tree result }
var td : pInflate_huft;      { distance tree result }
var  z : z_stream            { for memory allocation }
      ) : int;
type
  pFixed_table = ^fixed_table;
  fixed_table = array[0..288-1] of uIntf;
var
  k : int;                   { temporary variable }
  c : pFixed_table;          { length list for huft_build }
  v : PuIntArray;            { work area for huft_build }
var
  f : uInt;                  { number of hufts used in fixed_mem }
begin
  { build fixed tables if not already (multiple overlapped executions ok) }
  if not fixed_built then
  begin
    f := 0;

    { allocate memory }
    c := pFixed_table( ZALLOC(z, 288, sizeof(uInt)) );
    if (c = Z_NULL) then
    begin
      inflate_trees_fixed := Z_MEM_ERROR;
      exit;
    end;
    v := PuIntArray( ZALLOC(z, 288, sizeof(uInt)) );
    if (v = Z_NULL) then
    begin
      ZFREE(z, c);
      inflate_trees_fixed := Z_MEM_ERROR;
      exit;
    end;

    { literal table }
    for k := 0 to Pred(144) do
      c^[k] := 8;
    for k := 144 to Pred(256) do
      c^[k] := 9;
    for k := 256 to Pred(280) do
      c^[k] := 7;
    for k := 280 to Pred(288) do
      c^[k] := 8;
    fixed_bl := 9;
    huft_build(c^, 288, 257, cplens, cplext, @fixed_tl, fixed_bl,
               fixed_mem, f, v^);

    { distance table }
    for k := 0 to Pred(30) do
      c^[k] := 5;
    fixed_bd := 5;
    huft_build(c^, 30, 0, cpdist, cpdext, @fixed_td, fixed_bd,
               fixed_mem, f, v^);

    { done }
    ZFREE(z, v);
    ZFREE(z, c);
    fixed_built := True;
  end;
  bl := fixed_bl;
  bd := fixed_bd;
  tl := fixed_tl;
  td := fixed_td;
  inflate_trees_fixed := Z_OK;
end; { inflate_trees_fixed }

{== iminfutil implementation ==}

{ macros for bit input with no checking and for returning unused bytes }
procedure GRABBITS(j : int);
begin
  {while (k < j) do
  begin
    Dec(z^.avail_in);
    Inc(z^.total_in);
    b := b or (uLong(z^.next_in^) shl k);
    Inc(z^.next_in);
    Inc(k, 8);
  end;}
end;

procedure DUMPBITS(j : int);
begin
  {b := b shr j;
  Dec(k, j);}
end;

procedure NEEDBITS(j : int);
begin
 (*
          while (k < j) do
          begin
            {NEEDBYTE;}
            if (n <> 0) then
              r :=Z_OK
            else
            begin
              {UPDATE}
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, LongInt(p)-LongInt(z.next_in));
              z.next_in := p;
              s.write := q;
              result := inflate_flush(s,z,r);
              exit;
            end;
            Dec(n);
            b := b or (uLong(p^) shl k);
            Inc(p);
            Inc(k, 8);
          end;
 *)
end;

procedure NEEDOUT;
begin
 (*
  if (m = 0) then
  begin
    {WRAP}
    if (q = s.zend) and (s.read <> s.window) then
    begin
      q := s.window;
      if LongInt(q) < LongInt(s.read) then
        m := uInt(LongInt(s.read)-LongInt(q)-1)
      else
        m := uInt(LongInt(s.zend)-LongInt(q));
    end;

    if (m = 0) then
    begin
      {FLUSH}
      s.write := q;
      r := inflate_flush(s,z,r);
      q := s.write;
      if LongInt(q) < LongInt(s.read) then
        m := uInt(LongInt(s.read)-LongInt(q)-1)
      else
        m := uInt(LongInt(s.zend)-LongInt(q));

      {WRAP}
      if (q = s.zend) and (s.read <> s.window) then
      begin
        q := s.window;
        if LongInt(q) < LongInt(s.read) then
          m := uInt(LongInt(s.read)-LongInt(q)-1)
        else
          m := uInt(LongInt(s.zend)-LongInt(q));
      end;

      if (m = 0) then
      begin
        {UPDATE}
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, LongInt(p)-LongInt(z.next_in));
        z.next_in := p;
        s.write := q;
        result := inflate_flush(s,z,r);
        exit;
      end;
    end;
  end;
  r := Z_OK;
 *)
end;

{ copy as much as possible from the sliding window to the output area }
function inflate_flush(var s : inflate_blocks_state;
                       var z : z_stream;
                       r : int) : int;
var
  n : uInt;
  p : pBytef;
  q : pBytef;
begin
  { local copies of source and destination pointers }
  p := z.next_out;
  q := s.read;

  { compute number of bytes to copy as far as end of window }
  if ptr2int(q) <= ptr2int(s.write) then
    n := uInt(ptr2int(s.write) - ptr2int(q))
  else
    n := uInt(ptr2int(s.zend) - ptr2int(q));
  if (n > z.avail_out) then
    n := z.avail_out;
  if (n <> 0) and (r = Z_BUF_ERROR) then
    r := Z_OK;

  { update counters }
  Dec(z.avail_out, n);
  Inc(z.total_out, n);


  { update check information }
  if Assigned(s.checkfn) then
  begin
    s.check := s.checkfn(s.check, q, n);
    z.adler := s.check;
  end;

  { copy as far as end of window }
  zmemcpy(p, q, n);
  Inc(p, n);
  Inc(q, n);

  { see if more to copy at beginning of window }
  if (q = s.zend) then
  begin
    { wrap pointers }
    q := s.window;
    if (s.write = s.zend) then
      s.write := s.window;

    { compute bytes to copy }
    n := uInt(ptr2int(s.write) - ptr2int(q));
    if (n > z.avail_out) then
      n := z.avail_out;
    if (n <> 0) and (r = Z_BUF_ERROR) then
      r := Z_OK;

    { update counters }
    Dec( z.avail_out, n);
    Inc( z.total_out, n);

    { update check information }
    if Assigned(s.checkfn) then
    begin
      s.check := s.checkfn(s.check, q, n);
      z.adler := s.check;
    end;

    { copy }
    zmemcpy(p, q, n);
    Inc(p, n);
    Inc(q, n);
  end;


  { update pointers }
  z.next_out := p;
  s.read := q;

  { done }
  inflate_flush := r;
end;

{== imzinflate implementation ==}

function inflateReset(var z : z_stream) : int;
begin
  if (z.state = Z_NULL) then
  begin
    inflateReset :=  Z_STREAM_ERROR;
    exit;
  end;
  z.total_out := 0;
  z.total_in := 0;
  z.msg := '';
  if z.state^.nowrap then
    z.state^.mode := BLOCKS
  else
    z.state^.mode := METHOD;
  inflate_blocks_reset(z.state^.blocks^, z, Z_NULL);
  {$IFDEF DEBUG}
  Tracev('inflate: reset');
  {$ENDIF}
  inflateReset :=  Z_OK;
end;


function inflateEnd(var z : z_stream) : int;
begin
  if (z.state = Z_NULL) or not Assigned(z.zfree) then
  begin
    inflateEnd :=  Z_STREAM_ERROR;
    exit;
  end;
  if (z.state^.blocks <> Z_NULL) then
    inflate_blocks_free(z.state^.blocks, z);
  ZFREE(z, z.state);
  z.state := Z_NULL;
  {$IFDEF DEBUG}
  Tracev('inflate: end');
  {$ENDIF}
  inflateEnd :=  Z_OK;
end;


function inflateInit2_(var z: z_stream;
                       w : int;
                       const version : AnsiString;
                       stream_size : int) : int;
begin
  if (version = '') or (version[1] <> ZLIB_VERSION[1]) or
      (stream_size <> sizeof(z_stream)) then
  begin
    inflateInit2_ := Z_VERSION_ERROR;
    exit;
  end;
  { initialize state }
  { SetLength(strm.msg, 255); }
  z.msg := '';
  if not Assigned(z.zalloc) then
  begin
    {$IFDEF FPC}  z.zalloc := @zcalloc;  {$ELSE}
    z.zalloc := zcalloc;
    {$endif}
    z.opaque := voidpf(0);
  end;
  if not Assigned(z.zfree) then
    {$IFDEF FPC}  z.zfree := @zcfree;  {$ELSE}
    z.zfree := zcfree;
    {$ENDIF}

  z.state := pInternal_state( ZALLOC(z,1,sizeof(internal_state)) );
  if (z.state = Z_NULL) then
  begin
    inflateInit2_ := Z_MEM_ERROR;
    exit;
  end;

  z.state^.blocks := Z_NULL;

  { handle undocumented nowrap option (no zlib header or check) }
  z.state^.nowrap := FALSE;
  if (w < 0) then
  begin
    w := - w;
    z.state^.nowrap := TRUE;
  end;

  { set window size }
  if (w < 8) or (w > 15) then
  begin
    inflateEnd(z);
    inflateInit2_ := Z_STREAM_ERROR;
    exit;
  end;
  z.state^.wbits := uInt(w);

  { create inflate_blocks state }
  if z.state^.nowrap then
    z.state^.blocks := inflate_blocks_new(z, NIL, uInt(1) shl w)
  else
  {$IFDEF FPC}
    z.state^.blocks := inflate_blocks_new(z, @adler32, uInt(1) shl w);
  {$ELSE}
    z.state^.blocks := inflate_blocks_new(z, adler32, uInt(1) shl w);
  {$ENDIF}
  if (z.state^.blocks = Z_NULL) then
  begin
    inflateEnd(z);
    inflateInit2_ := Z_MEM_ERROR;
    exit;
  end;
  {$IFDEF DEBUG}
  Tracev('inflate: allocated');
  {$ENDIF}
  { reset state }
  inflateReset(z);
  inflateInit2_ :=  Z_OK;
end;

function inflateInit2(var z: z_stream; windowBits : int) : int;
begin
  inflateInit2 := inflateInit2_(z, windowBits, ZLIB_VERSION, sizeof(z_stream));
end;


function inflateInit(var z : z_stream) : int;
{ inflateInit is a macro to allow checking the zlib version
  and the compiler's view of z_stream:  }
begin
  inflateInit := inflateInit2_(z, DEF_WBITS, ZLIB_VERSION, sizeof(z_stream));
end;

function inflateInit_(z : z_streamp;
                      const version : AnsiString;
                      stream_size : int) : int;
begin
  { initialize state }
  if (z = Z_NULL) then
    inflateInit_ := Z_STREAM_ERROR
  else
    inflateInit_ := inflateInit2_(z^, DEF_WBITS, version, stream_size);
end;

function inflate(var z : z_stream;
                 f : int) : int;
var
  r : int;
  b : uInt;
begin
  if (z.state = Z_NULL) or (z.next_in = Z_NULL) then
  begin
    inflate := Z_STREAM_ERROR;
    exit;
  end;
  if f = Z_FINISH then
    f := Z_BUF_ERROR
  else
    f := Z_OK;
  r := Z_BUF_ERROR;
  while True do
  case (z.state^.mode) of
    BLOCKS:
      begin
        r := inflate_blocks(z.state^.blocks^, z, r);
        if (r = Z_DATA_ERROR) then
        begin
          z.state^.mode := BAD;
          z.state^.sub.marker := 0;       { can try inflateSync }
          continue;            { break C-switch }
        end;
        if (r = Z_OK) then
          r := f;
        if (r <> Z_STREAM_END) then
        begin
          inflate := r;
          exit;
        end;
        r := f;
        inflate_blocks_reset(z.state^.blocks^, z, @z.state^.sub.check.was);
        if (z.state^.nowrap) then
        begin
          z.state^.mode := DONE;
          continue;            { break C-switch }
        end;
        z.state^.mode := CHECK4;  { falltrough }
      end;
    CHECK4:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;

        {z.state^.sub.check.need := uLong(NEXTBYTE(z)) shl 24;}
        Dec(z.avail_in);
        Inc(z.total_in);
        z.state^.sub.check.need := uLong(z.next_in^) shl 24;
        Inc(z.next_in);

        z.state^.mode := CHECK3;   { falltrough }
      end;
    CHECK3:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;
        {Inc( z.state^.sub.check.need, uLong(NEXTBYTE(z)) shl 16);}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) shl 16);
        Inc(z.next_in);

        z.state^.mode := CHECK2;   { falltrough }
      end;
    CHECK2:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;

        {Inc( z.state^.sub.check.need, uLong(NEXTBYTE(z)) shl 8);}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) shl 8);
        Inc(z.next_in);

        z.state^.mode := CHECK1;   { falltrough }
      end;
    CHECK1:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;
        {Inc( z.state^.sub.check.need, uLong(NEXTBYTE(z)) );}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) );
        Inc(z.next_in);


        if (z.state^.sub.check.was <> z.state^.sub.check.need) then
        begin
          z.state^.mode := BAD;
          z.msg := 'incorrect data check';
          z.state^.sub.marker := 5;       { can't try inflateSync }
          continue;           { break C-switch }
        end;
        {$IFDEF DEBUG}
        Tracev('inflate: zlib check ok');
        {$ENDIF}
        z.state^.mode := DONE; { falltrough }
      end;
    DONE:
      begin
        inflate := Z_STREAM_END;
        exit;
      end;
    METHOD:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f; {}

        {z.state^.sub.method := NEXTBYTE(z);}
        Dec(z.avail_in);
        Inc(z.total_in);
        z.state^.sub.method := z.next_in^;
        Inc(z.next_in);

        if ((z.state^.sub.method and $0f) <> Z_DEFLATED) then
        begin
          z.state^.mode := BAD;
          z.msg := 'unknown compression method';
          z.state^.sub.marker := 5;       { can't try inflateSync }
          continue;  { break C-switch }
        end;
        if ((z.state^.sub.method shr 4) + 8 > z.state^.wbits) then
        begin
          z.state^.mode := BAD;
          z.msg := 'invalid window size';
          z.state^.sub.marker := 5;       { can't try inflateSync }
          continue; { break C-switch }
        end;
        z.state^.mode := FLAG;
        { fall trough }
      end;
    FLAG:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f; {}
        {b := NEXTBYTE(z);}
        Dec(z.avail_in);
        Inc(z.total_in);
        b := z.next_in^;
        Inc(z.next_in);

        if (((z.state^.sub.method shl 8) + b) mod 31) <> 0 then {% mod ?}
        begin
          z.state^.mode := BAD;
          z.msg := 'incorrect header check';
          z.state^.sub.marker := 5;       { can't try inflateSync }
          continue;      { break C-switch }
        end;
        {$IFDEF DEBUG}
        Tracev('inflate: zlib header ok');
        {$ENDIF}
        if ((b and PRESET_DICT) = 0) then
        begin
          z.state^.mode := BLOCKS;
	  continue;      { break C-switch }
        end;
        z.state^.mode := DICT4;
        { falltrough }
      end;
    DICT4:
      begin
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;

        {z.state^.sub.check.need := uLong(NEXTBYTE(z)) shl 24;}
        Dec(z.avail_in);
        Inc(z.total_in);
        z.state^.sub.check.need :=  uLong(z.next_in^) shl 24;
        Inc(z.next_in);

        z.state^.mode := DICT3;        { falltrough }
      end;
    DICT3:
      begin
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;
        {Inc(z.state^.sub.check.need, uLong(NEXTBYTE(z)) shl 16);}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) shl 16);
        Inc(z.next_in);

        z.state^.mode := DICT2;        { falltrough }
      end;
    DICT2:
      begin
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;

        {Inc(z.state^.sub.check.need, uLong(NEXTBYTE(z)) shl 8);}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) shl 8);
        Inc(z.next_in);

        z.state^.mode := DICT1;        { falltrough }
      end;
    DICT1:
      begin
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        { r := f;    ---  wird niemals benutzt }
        {Inc(z.state^.sub.check.need, uLong(NEXTBYTE(z)) );}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) );
        Inc(z.next_in);

        z.adler := z.state^.sub.check.need;
        z.state^.mode := DICT0;
        inflate := Z_NEED_DICT;
        exit;
      end;
    DICT0:
      begin
        z.state^.mode := BAD;
        z.msg := 'need dictionary';
        z.state^.sub.marker := 0;         { can try inflateSync }
        inflate := Z_STREAM_ERROR;
        exit;
      end;
    BAD:
      begin
        inflate := Z_DATA_ERROR;
        exit;
      end;
    else
      begin
        inflate := Z_STREAM_ERROR;
        exit;
      end;
  end;
  result := Z_STREAM_ERROR;
end;

function inflateSetDictionary(var z : z_stream;
                              dictionary : pBytef; {const array of byte}
                              dictLength : uInt) : int;
var
  length : uInt;
begin
  length := dictLength;

  if (z.state = Z_NULL) or (z.state^.mode <> DICT0) then
  begin
    inflateSetDictionary := Z_STREAM_ERROR;
    exit;
  end;
  if (adler32(Long(1), dictionary, dictLength) <> z.adler) then
  begin
    inflateSetDictionary := Z_DATA_ERROR;
    exit;
  end;
  z.adler := Long(1);

  if (length >= (uInt(1) shl z.state^.wbits)) then
  begin
    length := (1 shl z.state^.wbits)-1;
    Inc( dictionary, dictLength - length);
  end;
  inflate_set_dictionary(z.state^.blocks^, dictionary^, length);
  z.state^.mode := BLOCKS;
  inflateSetDictionary := Z_OK;
end;


function inflateSync(var z : z_stream) : int;
const
  mark : packed array[0..3] of byte = (0, 0, $ff, $ff);
var
  n : uInt;       { number of bytes to look at }
  p : pBytef;     { pointer to bytes }
  m : uInt;       { number of marker bytes found in a row }
  r, w : uLong;   { temporaries to save total_in and total_out }
begin
  { set up }
  if (z.state = Z_NULL) then
  begin
    inflateSync := Z_STREAM_ERROR;
    exit;
  end;
  if (z.state^.mode <> BAD) then
  begin
    z.state^.mode := BAD;
    z.state^.sub.marker := 0;
  end;
  n := z.avail_in;
  if (n = 0) then
  begin
    inflateSync := Z_BUF_ERROR;
    exit;
  end;
  p := z.next_in;
  m := z.state^.sub.marker;

  { search }
  while (n <> 0) and (m < 4) do
  begin
    if (p^ = mark[m]) then
      Inc(m)
    else
      if (p^ <> 0) then
        m := 0
      else
        m := 4 - m;
    Inc(p);
    Dec(n);
  end;

  { restore }
  Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
  z.next_in := p;
  z.avail_in := n;
  z.state^.sub.marker := m;


  { return no joy or set up to restart on a new block }
  if (m <> 4) then
  begin
    inflateSync := Z_DATA_ERROR;
    exit;
  end;
  r := z.total_in;
  w := z.total_out;
  inflateReset(z);
  z.total_in := r;
  z.total_out := w;
  z.state^.mode := BLOCKS;
  inflateSync := Z_OK;
end;


{
  returns true if inflate is currently at the end of a block generated
  by Z_SYNC_FLUSH or Z_FULL_FLUSH. This function is used by one PPP
  implementation to provide an additional safety check. PPP uses Z_SYNC_FLUSH
  but removes the length bytes of the resulting empty stored block. When
  decompressing, PPP checks that at the end of input packet, inflate is
  waiting for these length bytes.
}

function inflateSyncPoint(var z : z_stream) : int;
begin
  if (z.state = Z_NULL) or (z.state^.blocks = Z_NULL) then
  begin
    inflateSyncPoint := Z_STREAM_ERROR;
    exit;
  end;
  inflateSyncPoint := inflate_blocks_sync_point(z.state^.blocks^);
end;

{== imtrees implementation ==}

{ #define GEN_TREES_H }

{$ifndef GEN_TREES_H}
{ header created automatically with -DGEN_TREES_H }

const
  DIST_CODE_LEN = 512; { see definition of array dist_code below }

{ The static literal tree. Since the bit lengths are imposed, there is no
  need for the L_CODES extra codes used during heap construction. However
  The codes 286 and 287 are needed to build a canonical tree (see _tr_init
  below). }
var
  static_ltree : array[0..L_CODES+2-1] of ct_data = (
{ fc:(freq, code) dl:(dad,len) }
(fc:(freq: 12);dl:(len: 8)), (fc:(freq:140);dl:(len: 8)), (fc:(freq: 76);dl:(len: 8)),
(fc:(freq:204);dl:(len: 8)), (fc:(freq: 44);dl:(len: 8)), (fc:(freq:172);dl:(len: 8)),
(fc:(freq:108);dl:(len: 8)), (fc:(freq:236);dl:(len: 8)), (fc:(freq: 28);dl:(len: 8)),
(fc:(freq:156);dl:(len: 8)), (fc:(freq: 92);dl:(len: 8)), (fc:(freq:220);dl:(len: 8)),
(fc:(freq: 60);dl:(len: 8)), (fc:(freq:188);dl:(len: 8)), (fc:(freq:124);dl:(len: 8)),
(fc:(freq:252);dl:(len: 8)), (fc:(freq:  2);dl:(len: 8)), (fc:(freq:130);dl:(len: 8)),
(fc:(freq: 66);dl:(len: 8)), (fc:(freq:194);dl:(len: 8)), (fc:(freq: 34);dl:(len: 8)),
(fc:(freq:162);dl:(len: 8)), (fc:(freq: 98);dl:(len: 8)), (fc:(freq:226);dl:(len: 8)),
(fc:(freq: 18);dl:(len: 8)), (fc:(freq:146);dl:(len: 8)), (fc:(freq: 82);dl:(len: 8)),
(fc:(freq:210);dl:(len: 8)), (fc:(freq: 50);dl:(len: 8)), (fc:(freq:178);dl:(len: 8)),
(fc:(freq:114);dl:(len: 8)), (fc:(freq:242);dl:(len: 8)), (fc:(freq: 10);dl:(len: 8)),
(fc:(freq:138);dl:(len: 8)), (fc:(freq: 74);dl:(len: 8)), (fc:(freq:202);dl:(len: 8)),
(fc:(freq: 42);dl:(len: 8)), (fc:(freq:170);dl:(len: 8)), (fc:(freq:106);dl:(len: 8)),
(fc:(freq:234);dl:(len: 8)), (fc:(freq: 26);dl:(len: 8)), (fc:(freq:154);dl:(len: 8)),
(fc:(freq: 90);dl:(len: 8)), (fc:(freq:218);dl:(len: 8)), (fc:(freq: 58);dl:(len: 8)),
(fc:(freq:186);dl:(len: 8)), (fc:(freq:122);dl:(len: 8)), (fc:(freq:250);dl:(len: 8)),
(fc:(freq:  6);dl:(len: 8)), (fc:(freq:134);dl:(len: 8)), (fc:(freq: 70);dl:(len: 8)),
(fc:(freq:198);dl:(len: 8)), (fc:(freq: 38);dl:(len: 8)), (fc:(freq:166);dl:(len: 8)),
(fc:(freq:102);dl:(len: 8)), (fc:(freq:230);dl:(len: 8)), (fc:(freq: 22);dl:(len: 8)),
(fc:(freq:150);dl:(len: 8)), (fc:(freq: 86);dl:(len: 8)), (fc:(freq:214);dl:(len: 8)),
(fc:(freq: 54);dl:(len: 8)), (fc:(freq:182);dl:(len: 8)), (fc:(freq:118);dl:(len: 8)),
(fc:(freq:246);dl:(len: 8)), (fc:(freq: 14);dl:(len: 8)), (fc:(freq:142);dl:(len: 8)),
(fc:(freq: 78);dl:(len: 8)), (fc:(freq:206);dl:(len: 8)), (fc:(freq: 46);dl:(len: 8)),
(fc:(freq:174);dl:(len: 8)), (fc:(freq:110);dl:(len: 8)), (fc:(freq:238);dl:(len: 8)),
(fc:(freq: 30);dl:(len: 8)), (fc:(freq:158);dl:(len: 8)), (fc:(freq: 94);dl:(len: 8)),
(fc:(freq:222);dl:(len: 8)), (fc:(freq: 62);dl:(len: 8)), (fc:(freq:190);dl:(len: 8)),
(fc:(freq:126);dl:(len: 8)), (fc:(freq:254);dl:(len: 8)), (fc:(freq:  1);dl:(len: 8)),
(fc:(freq:129);dl:(len: 8)), (fc:(freq: 65);dl:(len: 8)), (fc:(freq:193);dl:(len: 8)),
(fc:(freq: 33);dl:(len: 8)), (fc:(freq:161);dl:(len: 8)), (fc:(freq: 97);dl:(len: 8)),
(fc:(freq:225);dl:(len: 8)), (fc:(freq: 17);dl:(len: 8)), (fc:(freq:145);dl:(len: 8)),
(fc:(freq: 81);dl:(len: 8)), (fc:(freq:209);dl:(len: 8)), (fc:(freq: 49);dl:(len: 8)),
(fc:(freq:177);dl:(len: 8)), (fc:(freq:113);dl:(len: 8)), (fc:(freq:241);dl:(len: 8)),
(fc:(freq:  9);dl:(len: 8)), (fc:(freq:137);dl:(len: 8)), (fc:(freq: 73);dl:(len: 8)),
(fc:(freq:201);dl:(len: 8)), (fc:(freq: 41);dl:(len: 8)), (fc:(freq:169);dl:(len: 8)),
(fc:(freq:105);dl:(len: 8)), (fc:(freq:233);dl:(len: 8)), (fc:(freq: 25);dl:(len: 8)),
(fc:(freq:153);dl:(len: 8)), (fc:(freq: 89);dl:(len: 8)), (fc:(freq:217);dl:(len: 8)),
(fc:(freq: 57);dl:(len: 8)), (fc:(freq:185);dl:(len: 8)), (fc:(freq:121);dl:(len: 8)),
(fc:(freq:249);dl:(len: 8)), (fc:(freq:  5);dl:(len: 8)), (fc:(freq:133);dl:(len: 8)),
(fc:(freq: 69);dl:(len: 8)), (fc:(freq:197);dl:(len: 8)), (fc:(freq: 37);dl:(len: 8)),
(fc:(freq:165);dl:(len: 8)), (fc:(freq:101);dl:(len: 8)), (fc:(freq:229);dl:(len: 8)),
(fc:(freq: 21);dl:(len: 8)), (fc:(freq:149);dl:(len: 8)), (fc:(freq: 85);dl:(len: 8)),
(fc:(freq:213);dl:(len: 8)), (fc:(freq: 53);dl:(len: 8)), (fc:(freq:181);dl:(len: 8)),
(fc:(freq:117);dl:(len: 8)), (fc:(freq:245);dl:(len: 8)), (fc:(freq: 13);dl:(len: 8)),
(fc:(freq:141);dl:(len: 8)), (fc:(freq: 77);dl:(len: 8)), (fc:(freq:205);dl:(len: 8)),
(fc:(freq: 45);dl:(len: 8)), (fc:(freq:173);dl:(len: 8)), (fc:(freq:109);dl:(len: 8)),
(fc:(freq:237);dl:(len: 8)), (fc:(freq: 29);dl:(len: 8)), (fc:(freq:157);dl:(len: 8)),
(fc:(freq: 93);dl:(len: 8)), (fc:(freq:221);dl:(len: 8)), (fc:(freq: 61);dl:(len: 8)),
(fc:(freq:189);dl:(len: 8)), (fc:(freq:125);dl:(len: 8)), (fc:(freq:253);dl:(len: 8)),
(fc:(freq: 19);dl:(len: 9)), (fc:(freq:275);dl:(len: 9)), (fc:(freq:147);dl:(len: 9)),
(fc:(freq:403);dl:(len: 9)), (fc:(freq: 83);dl:(len: 9)), (fc:(freq:339);dl:(len: 9)),
(fc:(freq:211);dl:(len: 9)), (fc:(freq:467);dl:(len: 9)), (fc:(freq: 51);dl:(len: 9)),
(fc:(freq:307);dl:(len: 9)), (fc:(freq:179);dl:(len: 9)), (fc:(freq:435);dl:(len: 9)),
(fc:(freq:115);dl:(len: 9)), (fc:(freq:371);dl:(len: 9)), (fc:(freq:243);dl:(len: 9)),
(fc:(freq:499);dl:(len: 9)), (fc:(freq: 11);dl:(len: 9)), (fc:(freq:267);dl:(len: 9)),
(fc:(freq:139);dl:(len: 9)), (fc:(freq:395);dl:(len: 9)), (fc:(freq: 75);dl:(len: 9)),
(fc:(freq:331);dl:(len: 9)), (fc:(freq:203);dl:(len: 9)), (fc:(freq:459);dl:(len: 9)),
(fc:(freq: 43);dl:(len: 9)), (fc:(freq:299);dl:(len: 9)), (fc:(freq:171);dl:(len: 9)),
(fc:(freq:427);dl:(len: 9)), (fc:(freq:107);dl:(len: 9)), (fc:(freq:363);dl:(len: 9)),
(fc:(freq:235);dl:(len: 9)), (fc:(freq:491);dl:(len: 9)), (fc:(freq: 27);dl:(len: 9)),
(fc:(freq:283);dl:(len: 9)), (fc:(freq:155);dl:(len: 9)), (fc:(freq:411);dl:(len: 9)),
(fc:(freq: 91);dl:(len: 9)), (fc:(freq:347);dl:(len: 9)), (fc:(freq:219);dl:(len: 9)),
(fc:(freq:475);dl:(len: 9)), (fc:(freq: 59);dl:(len: 9)), (fc:(freq:315);dl:(len: 9)),
(fc:(freq:187);dl:(len: 9)), (fc:(freq:443);dl:(len: 9)), (fc:(freq:123);dl:(len: 9)),
(fc:(freq:379);dl:(len: 9)), (fc:(freq:251);dl:(len: 9)), (fc:(freq:507);dl:(len: 9)),
(fc:(freq:  7);dl:(len: 9)), (fc:(freq:263);dl:(len: 9)), (fc:(freq:135);dl:(len: 9)),
(fc:(freq:391);dl:(len: 9)), (fc:(freq: 71);dl:(len: 9)), (fc:(freq:327);dl:(len: 9)),
(fc:(freq:199);dl:(len: 9)), (fc:(freq:455);dl:(len: 9)), (fc:(freq: 39);dl:(len: 9)),
(fc:(freq:295);dl:(len: 9)), (fc:(freq:167);dl:(len: 9)), (fc:(freq:423);dl:(len: 9)),
(fc:(freq:103);dl:(len: 9)), (fc:(freq:359);dl:(len: 9)), (fc:(freq:231);dl:(len: 9)),
(fc:(freq:487);dl:(len: 9)), (fc:(freq: 23);dl:(len: 9)), (fc:(freq:279);dl:(len: 9)),
(fc:(freq:151);dl:(len: 9)), (fc:(freq:407);dl:(len: 9)), (fc:(freq: 87);dl:(len: 9)),
(fc:(freq:343);dl:(len: 9)), (fc:(freq:215);dl:(len: 9)), (fc:(freq:471);dl:(len: 9)),
(fc:(freq: 55);dl:(len: 9)), (fc:(freq:311);dl:(len: 9)), (fc:(freq:183);dl:(len: 9)),
(fc:(freq:439);dl:(len: 9)), (fc:(freq:119);dl:(len: 9)), (fc:(freq:375);dl:(len: 9)),
(fc:(freq:247);dl:(len: 9)), (fc:(freq:503);dl:(len: 9)), (fc:(freq: 15);dl:(len: 9)),
(fc:(freq:271);dl:(len: 9)), (fc:(freq:143);dl:(len: 9)), (fc:(freq:399);dl:(len: 9)),
(fc:(freq: 79);dl:(len: 9)), (fc:(freq:335);dl:(len: 9)), (fc:(freq:207);dl:(len: 9)),
(fc:(freq:463);dl:(len: 9)), (fc:(freq: 47);dl:(len: 9)), (fc:(freq:303);dl:(len: 9)),
(fc:(freq:175);dl:(len: 9)), (fc:(freq:431);dl:(len: 9)), (fc:(freq:111);dl:(len: 9)),
(fc:(freq:367);dl:(len: 9)), (fc:(freq:239);dl:(len: 9)), (fc:(freq:495);dl:(len: 9)),
(fc:(freq: 31);dl:(len: 9)), (fc:(freq:287);dl:(len: 9)), (fc:(freq:159);dl:(len: 9)),
(fc:(freq:415);dl:(len: 9)), (fc:(freq: 95);dl:(len: 9)), (fc:(freq:351);dl:(len: 9)),
(fc:(freq:223);dl:(len: 9)), (fc:(freq:479);dl:(len: 9)), (fc:(freq: 63);dl:(len: 9)),
(fc:(freq:319);dl:(len: 9)), (fc:(freq:191);dl:(len: 9)), (fc:(freq:447);dl:(len: 9)),
(fc:(freq:127);dl:(len: 9)), (fc:(freq:383);dl:(len: 9)), (fc:(freq:255);dl:(len: 9)),
(fc:(freq:511);dl:(len: 9)), (fc:(freq:  0);dl:(len: 7)), (fc:(freq: 64);dl:(len: 7)),
(fc:(freq: 32);dl:(len: 7)), (fc:(freq: 96);dl:(len: 7)), (fc:(freq: 16);dl:(len: 7)),
(fc:(freq: 80);dl:(len: 7)), (fc:(freq: 48);dl:(len: 7)), (fc:(freq:112);dl:(len: 7)),
(fc:(freq:  8);dl:(len: 7)), (fc:(freq: 72);dl:(len: 7)), (fc:(freq: 40);dl:(len: 7)),
(fc:(freq:104);dl:(len: 7)), (fc:(freq: 24);dl:(len: 7)), (fc:(freq: 88);dl:(len: 7)),
(fc:(freq: 56);dl:(len: 7)), (fc:(freq:120);dl:(len: 7)), (fc:(freq:  4);dl:(len: 7)),
(fc:(freq: 68);dl:(len: 7)), (fc:(freq: 36);dl:(len: 7)), (fc:(freq:100);dl:(len: 7)),
(fc:(freq: 20);dl:(len: 7)), (fc:(freq: 84);dl:(len: 7)), (fc:(freq: 52);dl:(len: 7)),
(fc:(freq:116);dl:(len: 7)), (fc:(freq:  3);dl:(len: 8)), (fc:(freq:131);dl:(len: 8)),
(fc:(freq: 67);dl:(len: 8)), (fc:(freq:195);dl:(len: 8)), (fc:(freq: 35);dl:(len: 8)),
(fc:(freq:163);dl:(len: 8)), (fc:(freq: 99);dl:(len: 8)), (fc:(freq:227);dl:(len: 8))
);


{ The static distance tree. (Actually a trivial tree since all lens use
  5 bits.) }
  static_dtree : array[0..D_CODES-1] of ct_data = (
(fc:(freq: 0); dl:(len:5)), (fc:(freq:16); dl:(len:5)), (fc:(freq: 8); dl:(len:5)),
(fc:(freq:24); dl:(len:5)), (fc:(freq: 4); dl:(len:5)), (fc:(freq:20); dl:(len:5)),
(fc:(freq:12); dl:(len:5)), (fc:(freq:28); dl:(len:5)), (fc:(freq: 2); dl:(len:5)),
(fc:(freq:18); dl:(len:5)), (fc:(freq:10); dl:(len:5)), (fc:(freq:26); dl:(len:5)),
(fc:(freq: 6); dl:(len:5)), (fc:(freq:22); dl:(len:5)), (fc:(freq:14); dl:(len:5)),
(fc:(freq:30); dl:(len:5)), (fc:(freq: 1); dl:(len:5)), (fc:(freq:17); dl:(len:5)),
(fc:(freq: 9); dl:(len:5)), (fc:(freq:25); dl:(len:5)), (fc:(freq: 5); dl:(len:5)),
(fc:(freq:21); dl:(len:5)), (fc:(freq:13); dl:(len:5)), (fc:(freq:29); dl:(len:5)),
(fc:(freq: 3); dl:(len:5)), (fc:(freq:19); dl:(len:5)), (fc:(freq:11); dl:(len:5)),
(fc:(freq:27); dl:(len:5)), (fc:(freq: 7); dl:(len:5)), (fc:(freq:23); dl:(len:5))
);

{ Distance codes. The first 256 values correspond to the distances
  3 .. 258, the last 256 values correspond to the top 8 bits of
  the 15 bit distances. }
  _dist_code : array[0..DIST_CODE_LEN-1] of uch = (
 0,  1,  2,  3,  4,  4,  5,  5,  6,  6,  6,  6,  7,  7,  7,  7,  8,  8,  8,  8,
 8,  8,  8,  8,  9,  9,  9,  9,  9,  9,  9,  9, 10, 10, 10, 10, 10, 10, 10, 10,
10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13,
13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15,
15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,  0,  0, 16, 17,
18, 18, 19, 19, 20, 20, 20, 20, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22,
23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27,
27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
28, 28, 28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29
);

{ length code for each normalized match length (0 == MIN_MATCH) }
  _length_code : array[0..MAX_MATCH-MIN_MATCH+1-1] of uch = (
 0,  1,  2,  3,  4,  5,  6,  7,  8,  8,  9,  9, 10, 10, 11, 11, 12, 12, 12, 12,
13, 13, 13, 13, 14, 14, 14, 14, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16,
17, 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 18, 18, 19, 19, 19, 19,
19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22, 22, 22,
22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23,
23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26,
26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 28
);

  
{ First normalized length for each code (0 = MIN_MATCH) }
  base_length : array[0..LENGTH_CODES-1] of int = (
0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16, 20, 24, 28, 32, 40, 48, 56,
64, 80, 96, 112, 128, 160, 192, 224, 0
);


{ First normalized distance for each code (0 = distance of 1) }
  base_dist : array[0..D_CODES-1] of int = (
    0,     1,     2,     3,     4,     6,     8,    12,    16,    24,
   32,    48,    64,    96,   128,   192,   256,   384,   512,   768,
 1024,  1536,  2048,  3072,  4096,  6144,  8192, 12288, 16384, 24576
);
{$endif}

{ ===========================================================================
  Constants }

const
  MAX_BL_BITS = 7;
{ Bit length codes must not exceed MAX_BL_BITS bits }

const
  END_BLOCK = 256;
{ end of block literal code }

const
  REP_3_6 = 16;
{ repeat previous bit length 3-6 times (2 bits of repeat count) }

const
  REPZ_3_10 = 17;
{ repeat a zero length 3-10 times  (3 bits of repeat count) }

const
  REPZ_11_138 = 18;
{ repeat a zero length 11-138 times  (7 bits of repeat count) }

const
  extra_lbits : array[0..LENGTH_CODES-1] of int
    { extra bits for each length code }
   = (0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0);

const
  extra_dbits : array[0..D_CODES-1] of int
    { extra bits for each distance code }
   = (0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13);

const
  extra_blbits : array[0..BL_CODES-1] of int { extra bits for each bit length code }
   = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,3,7);

const
  bl_order : array[0..BL_CODES-1] of uch
   = (16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15);
{ The lengths of the bit length codes are sent in order of decreasing
  probability, to avoid transmitting the lengths for unused bit length codes.
 }

const
  Buf_size = (8 * 2*sizeof(uch));
{ Number of bits used within bi_buf. (bi_buf might be implemented on
  more than 16 bits on some systems.) }

{ ===========================================================================
  Local data. These are initialized only once. }


{$ifdef GEN_TREES_H)}
{ non ANSI compilers may not accept trees.h }

const
  DIST_CODE_LEN = 512; { see definition of array dist_code below }

var
  static_ltree : array[0..L_CODES+2-1] of ct_data;
{ The static literal tree. Since the bit lengths are imposed, there is no
  need for the L_CODES extra codes used during heap construction. However
  The codes 286 and 287 are needed to build a canonical tree (see _tr_init
  below). }

  static_dtree : array[0..D_CODES-1] of ct_data;
{ The static distance tree. (Actually a trivial tree since all codes use
  5 bits.) }

  _dist_code : array[0..DIST_CODE_LEN-1] of uch;
{ Distance codes. The first 256 values correspond to the distances
  3 .. 258, the last 256 values correspond to the top 8 bits of
  the 15 bit distances. }

  _length_code : array[0..MAX_MATCH-MIN_MATCH+1-1] of uch;
{ length code for each normalized match length (0 == MIN_MATCH) }

  base_length : array[0..LENGTH_CODES-1] of int;
{ First normalized length for each code (0 = MIN_MATCH) }

  base_dist : array[0..D_CODES-1] of int;
{ First normalized distance for each code (0 = distance of 1) }

{$endif} { GEN_TREES_H }

const
  static_l_desc :  static_tree_desc  =
      (static_tree: {tree_ptr}(@(static_ltree));  { pointer to array of ct_data }
       extra_bits: {pzIntfArray}(@(extra_lbits)); { pointer to array of int }
       extra_base: LITERALS+1;
       elems: L_CODES;
       max_length: MAX_BITS);

const
  static_d_desc : static_tree_desc  =
      (static_tree: {tree_ptr}(@(static_dtree));
       extra_bits: {pzIntfArray}(@(extra_dbits));
       extra_base : 0;
       elems: D_CODES;
       max_length: MAX_BITS);

const
  static_bl_desc : static_tree_desc =
      (static_tree: {tree_ptr}(NIL);
       extra_bits: {pzIntfArray}@(extra_blbits);
       extra_base : 0;
       elems: BL_CODES;
       max_length: MAX_BL_BITS);

{$ifdef GEN_TREES_H}
procedure gen_trees_header;
{$endif}

(*
{ ===========================================================================
  Output a short LSB first on the stream.
  IN assertion: there is enough room in pendingBuf. }

macro put_short(s, w)
begin
    {put_byte(s, (uch)((w) & 0xff));}
    s.pending_buf^[s.pending] := uch((w) and $ff);
    Inc(s.pending);

    {put_byte(s, (uch)((ush)(w) >> 8));}
    s.pending_buf^[s.pending] := uch(ush(w) shr 8);;
    Inc(s.pending);
end
*)

{ ===========================================================================
  Send a value on a given number of bits.
  IN assertion: length <= 16 and value fits in length bits. }

{$ifdef ORG_DEBUG}

procedure send_bits(var s : deflate_state;
                    value : int;   { value to send }
                    length : int); { number of bits }
begin
  {$ifdef DEBUG}
  Tracevv(' l '+IntToStr(length)+ ' v '+IntToStr(value));
  Assert((length > 0) and (length <= 15), 'invalid length');
  Inc(s.bits_sent, ulg(length));
  {$ENDIF}

  { If not enough room in bi_buf, use (valid) bits from bi_buf and
    (16 - bi_valid) bits from value, leaving (width - (16-bi_valid))
    unused bits in value. }
  {$IFOPT Q+} {$Q-} {$DEFINE NoOverflowCheck} {$ENDIF}
  {$IFOPT R+} {$R-} {$DEFINE NoRangeCheck} {$ENDIF}
  if (s.bi_valid > int(Buf_size) - length) then
  begin
    s.bi_buf := s.bi_buf or int(value shl s.bi_valid);
    {put_short(s, s.bi_buf);}
    s.pending_buf^[s.pending] := uch(s.bi_buf and $ff);
    Inc(s.pending);
    s.pending_buf^[s.pending] := uch(ush(s.bi_buf) shr 8);;
    Inc(s.pending);

    s.bi_buf := ush(value) shr (Buf_size - s.bi_valid);
    Inc(s.bi_valid, length - Buf_size);
  end
  else
  begin
    s.bi_buf := s.bi_buf or int(value shl s.bi_valid);
    Inc(s.bi_valid, length);
  end;
  {$IFDEF NoOverflowCheck} {$Q+} {$UNDEF NoOverflowCheck} {$ENDIF}
  {$IFDEF NoRangeCheck} {$Q+} {$UNDEF NoRangeCheck} {$ENDIF}
end;
{$endif} { DEBUG }

{ ===========================================================================
  Reverse the first len bits of a code, using straightforward code (a faster
  method would use a table)
  IN assertion: 1 <= len <= 15 }

function bi_reverse(code : unsigned;         { the value to invert }
                    len : int) : unsigned;   { its bit length }

var
  res : unsigned; {register}
begin
  res := 0;
  repeat
    res := res or (code and 1);
    code := code shr 1;
    res := res shl 1;
    Dec(len);
  until (len <= 0);
  bi_reverse := res shr 1;
end;

{ ===========================================================================
  Generate the codes for a given tree and bit counts (which need not be
  optimal).
  IN assertion: the array bl_count contains the bit length statistics for
  the given tree and the field len is set for all tree elements.
  OUT assertion: the field code is set for all tree elements of non
      zero code length. }

procedure gen_codes(tree : tree_ptr;  { the tree to decorate }
                    max_code : int;   { largest code with non zero frequency }
                    var bl_count : array of ushf);  { number of codes at each bit length }

var
  next_code : array[0..MAX_BITS+1-1] of ush; { next code value for each bit length }
  code : ush;              { running code value }
  bits : int;                  { bit index }
  n : int;                     { code index }
var
  len : int;
begin
  code := 0;

  { The distribution counts are first used to generate the code values
    without bit reversal. }

  for bits := 1 to MAX_BITS do
  begin
    code := ((code + bl_count[bits-1]) shl 1);
    next_code[bits] := code;
  end;
  { Check that the bit counts in bl_count are consistent. The last code
    must be all ones. }

  {$IFDEF DEBUG}
  Assert (code + bl_count[MAX_BITS]-1 = (1 shl MAX_BITS)-1,
          'inconsistent bit counts');
  Tracev(#13'gen_codes: max_code '+IntToStr(max_code));
  {$ENDIF}

  for n := 0 to max_code do
  begin
    len := tree^[n].dl.Len;
    if (len = 0) then
      continue;
    { Now reverse the bits }
    tree^[n].fc.Code := bi_reverse(next_code[len], len);
    Inc(next_code[len]);
    {$ifdef DEBUG}
    if (n>31) and (n<128) then
      Tracecv(tree <> tree_ptr(@static_ltree),
       (^M'n #'+IntToStr(n)+' '+AnsiChar(n)+' l '+IntToStr(len)+' c '+
         IntToStr(tree^[n].fc.Code)+' ('+IntToStr(next_code[len]-1)+')'))
    else
      Tracecv(tree <> tree_ptr(@static_ltree),
      (^M'n #'+IntToStr(n)+'   l '+IntToStr(len)+' c '+
         IntToStr(tree^[n].fc.Code)+' ('+IntToStr(next_code[len]-1)+')'));
    {$ENDIF}
  end;
end;

{ ===========================================================================
  Genererate the file trees.h describing the static trees. }
{$ifdef GEN_TREES_H}

macro SEPARATOR(i, last, width)
  if (i) = (last) then
    ( ^M');'^M^M
  else    \
    if (i) mod (width) = (width)-1 then
       ','^M
     else
       ', '

procedure gen_trees_header;
var
  header : system.text;
  i : int;
begin
  system.assign(header, 'trees.inc');
  {$I-}
  ReWrite(header);
  {$I+}
  Assert (IOresult <> 0, 'Can''t open trees.h');
  WriteLn(header,
    '{ header created automatically with -DGEN_TREES_H }'^M);

  WriteLn(header, 'local const ct_data static_ltree[L_CODES+2] := (');
  for i := 0 to L_CODES+2-1 do
  begin
    WriteLn(header, '((%3u),(%3u))%s', static_ltree[i].Code,
		static_ltree[i].Len, SEPARATOR(i, L_CODES+1, 5));
  end;

  WriteLn(header, 'local const ct_data static_dtree[D_CODES] := (');
  for i := 0 to D_CODES-1 do
  begin
    WriteLn(header, '((%2u),(%2u))%s', static_dtree[i].Code,
		static_dtree[i].Len, SEPARATOR(i, D_CODES-1, 5));
  end;

  WriteLn(header, 'const uch _dist_code[DIST_CODE_LEN] := (');
  for i := 0 to DIST_CODE_LEN-1 do
  begin
    WriteLn(header, '%2u%s', _dist_code[i],
		SEPARATOR(i, DIST_CODE_LEN-1, 20));
  end;

  WriteLn(header, 'const uch _length_code[MAX_MATCH-MIN_MATCH+1]= (');
  for i := 0 to MAX_MATCH-MIN_MATCH+1-1 do
  begin
    WriteLn(header, '%2u%s', _length_code[i],
		SEPARATOR(i, MAX_MATCH-MIN_MATCH, 20));
  end;

  WriteLn(header, 'local const int base_length[LENGTH_CODES] := (');
  for i := 0 to LENGTH_CODES-1 do
  begin
    WriteLn(header, '%1u%s', base_length[i],
		SEPARATOR(i, LENGTH_CODES-1, 20));
  end;

  WriteLn(header, 'local const int base_dist[D_CODES] := (');
  for i := 0 to D_CODES-1 do
  begin
    WriteLn(header, '%5u%s', base_dist[i],
		SEPARATOR(i, D_CODES-1, 10));
  end;

  close(header);
end;
{$endif} { GEN_TREES_H }


{ ===========================================================================
  Initialize the various 'constant' tables. }

procedure tr_static_init;

{$ifdef GEN_TREES_H}
const
  static_init_done : boolean = FALSE;
var
  n : int;        { iterates over tree elements }
  bits : int;     { bit counter }
  length : int;   { length value }
  code : int;     { code value }
  dist : int;     { distance index }
  bl_count : array[0..MAX_BITS+1-1] of ush;
    { number of codes at each bit length for an optimal tree }
begin
    if (static_init_done) then
      exit;

    { Initialize the mapping length (0..255) -> length code (0..28) }
    length := 0;
    for code := 0 to LENGTH_CODES-1-1 do
    begin
      base_length[code] := length;
      for n := 0 to (1 shl extra_lbits[code])-1 do
      begin
        _length_code[length] := uch(code);
        Inc(length);
      end;
    end;
    Assert (length = 256, 'tr_static_init: length <> 256');
    { Note that the length 255 (match length 258) can be represented
      in two different ways: code 284 + 5 bits or code 285, so we
      overwrite length_code[255] to use the best encoding: }

    _length_code[length-1] := uch(code);

    { Initialize the mapping dist (0..32K) -> dist code (0..29) }
    dist := 0;
    for code := 0 to 16-1 do
    begin
      base_dist[code] := dist;
      for n := 0 to (1 shl extra_dbits[code])-1 do
      begin
        _dist_code[dist] := uch(code);
        Inc(dist);
      end;
    end;
    Assert (dist = 256, 'tr_static_init: dist <> 256');
    dist := dist shr 7; { from now on, all distances are divided by 128 }
    for code := 16 to D_CODES-1 do
    begin
      base_dist[code] := dist shl 7;
      for n := 0 to (1 shl (extra_dbits[code]-7))-1 do
      begin
        _dist_code[256 + dist] := uch(code);
        Inc(dist);
      end;
    end;
    Assert (dist = 256, 'tr_static_init: 256+dist <> 512');

    { Construct the codes of the static literal tree }
    for bits := 0 to MAX_BITS do
      bl_count[bits] := 0;
    n := 0;
    while (n <= 143) do
    begin
      static_ltree[n].dl.Len := 8;
      Inc(n);
      Inc(bl_count[8]);
    end;
    while (n <= 255) do
    begin
      static_ltree[n].dl.Len := 9;
      Inc(n);
      Inc(bl_count[9]);
    end;
    while (n <= 279) do
    begin
      static_ltree[n].dl.Len := 7;
      Inc(n);
      Inc(bl_count[7]);
    end;
    while (n <= 287) do
    begin
      static_ltree[n].dl.Len := 8;
      Inc(n);
      Inc(bl_count[8]);
    end;

    { Codes 286 and 287 do not exist, but we must include them in the
      tree construction to get a canonical Huffman tree (longest code
      all ones)  }

    gen_codes(tree_ptr(@static_ltree), L_CODES+1, bl_count);

    { The static distance tree is trivial: }
    for n := 0 to D_CODES-1 do
    begin
      static_dtree[n].dl.Len := 5;
      static_dtree[n].fc.Code := bi_reverse(unsigned(n), 5);
    end;
    static_init_done := TRUE;

    gen_trees_header;  { save to include file }
{$else}
begin
{$endif} { GEN_TREES_H) }
end;

{ ===========================================================================
  Initialize a new block. }

procedure init_block(var s : deflate_state);
var
  n : int; { iterates over tree elements }
begin
  { Initialize the trees. }
  for n := 0 to L_CODES-1 do
    s.dyn_ltree[n].fc.Freq := 0;
  for n := 0 to D_CODES-1 do
    s.dyn_dtree[n].fc.Freq := 0;
  for n := 0 to BL_CODES-1 do
    s.bl_tree[n].fc.Freq := 0;

  s.dyn_ltree[END_BLOCK].fc.Freq := 1;
  s.static_len := Long(0);
  s.opt_len := Long(0);
  s.matches := 0;
  s.last_lit := 0;
end;

const
  SMALLEST = 1;
{ Index within the heap array of least frequent node in the Huffman tree }

{ ===========================================================================
  Initialize the tree data structures for a new zlib stream. }
procedure _tr_init(var s : deflate_state);
begin
  tr_static_init;

  s.compressed_len := Long(0);

  s.l_desc.dyn_tree := tree_ptr(@s.dyn_ltree);
  s.l_desc.stat_desc := @static_l_desc;

  s.d_desc.dyn_tree := tree_ptr(@s.dyn_dtree);
  s.d_desc.stat_desc := @static_d_desc;

  s.bl_desc.dyn_tree := tree_ptr(@s.bl_tree);
  s.bl_desc.stat_desc := @static_bl_desc;

  s.bi_buf := 0;
  s.bi_valid := 0;
  s.last_eob_len := 8; { enough lookahead for inflate }
{$ifdef DEBUG}
  s.bits_sent := Long(0);
{$endif}

  { Initialize the first block of the first file: }
  init_block(s);
end;

{ ===========================================================================
  Remove the smallest element from the heap and recreate the heap with
  one less element. Updates heap and heap_len.

macro pqremove(s, tree, top)
begin
    top := s.heap[SMALLEST];
    s.heap[SMALLEST] := s.heap[s.heap_len];
    Dec(s.heap_len);
    pqdownheap(s, tree, SMALLEST);
end
}

{ ===========================================================================
  Compares to subtrees, using the tree depth as tie breaker when
  the subtrees have equal frequency. This minimizes the worst case length.

macro smaller(tree, n, m, depth)
   ( (tree[n].Freq < tree[m].Freq) or
     ((tree[n].Freq = tree[m].Freq) and (depth[n] <= depth[m])) )
}

{ ===========================================================================
  Restore the heap property by moving down the tree starting at node k,
  exchanging a node with the smallest of its two sons if necessary, stopping
  when the heap property is re-established (each father smaller than its
  two sons). }

procedure pqdownheap(var s : deflate_state;
                     var tree : tree_type;   { the tree to restore }
                     k : int);          { node to move down }
var
  v : int;
  j : int;
begin
  v := s.heap[k];
  j := k shl 1;  { left son of k }
  while (j <= s.heap_len) do
  begin
    { Set j to the smallest of the two sons: }
    if (j < s.heap_len) and
       {smaller(tree, s.heap[j+1], s.heap[j], s.depth)}
      ( (tree[s.heap[j+1]].fc.Freq < tree[s.heap[j]].fc.Freq) or
        ((tree[s.heap[j+1]].fc.Freq = tree[s.heap[j]].fc.Freq) and
         (s.depth[s.heap[j+1]] <= s.depth[s.heap[j]])) ) then
    begin
      Inc(j);
    end;
    { Exit if v is smaller than both sons }
    if {(smaller(tree, v, s.heap[j], s.depth))}
     ( (tree[v].fc.Freq < tree[s.heap[j]].fc.Freq) or
       ((tree[v].fc.Freq = tree[s.heap[j]].fc.Freq) and
        (s.depth[v] <= s.depth[s.heap[j]])) ) then
      break;
    { Exchange v with the smallest son }
    s.heap[k] := s.heap[j];
    k := j;

    { And continue down the tree, setting j to the left son of k }
    j := j shl 1;
  end;
  s.heap[k] := v;
end;

{ ===========================================================================
  Compute the optimal bit lengths for a tree and update the total bit length
  for the current block.
  IN assertion: the fields freq and dad are set, heap[heap_max] and
     above are the tree nodes sorted by increasing frequency.
  OUT assertions: the field len is set to the optimal bit length, the
      array bl_count contains the frequencies for each bit length.
      The length opt_len is updated; static_len is also updated if stree is
      not null. }

procedure gen_bitlen(var s : deflate_state;
                     var desc : tree_desc);   { the tree descriptor }
var
  tree : tree_ptr;
  max_code : int;
  stree : tree_ptr; {const}
  extra : pzIntfArray; {const}
  base : int;
  max_length : int;
  h : int;              { heap index }
  n, m : int;           { iterate over the tree elements }
  bits : int;           { bit length }
  xbits : int;          { extra bits }
  f : ush;              { frequency }
  overflow : int;   { number of elements with bit length too large }
begin
  tree := desc.dyn_tree;
  max_code := desc.max_code;
  stree := desc.stat_desc^.static_tree;
  extra := desc.stat_desc^.extra_bits;
  base := desc.stat_desc^.extra_base;
  max_length := desc.stat_desc^.max_length;
  overflow := 0;

  for bits := 0 to MAX_BITS do
    s.bl_count[bits] := 0;

  { In a first pass, compute the optimal bit lengths (which may
    overflow in the case of the bit length tree). }

  tree^[s.heap[s.heap_max]].dl.Len := 0; { root of the heap }

  for h := s.heap_max+1 to HEAP_SIZE-1 do
  begin
    n := s.heap[h];
    bits := tree^[tree^[n].dl.Dad].dl.Len + 1;
    if (bits > max_length) then
    begin
      bits := max_length;
      Inc(overflow);
    end;
    tree^[n].dl.Len := ush(bits);
    { We overwrite tree[n].dl.Dad which is no longer needed }

    if (n > max_code) then
      continue; { not a leaf node }

    Inc(s.bl_count[bits]);
    xbits := 0;
    if (n >= base) then
      xbits := extra^[n-base];
    f := tree^[n].fc.Freq;
    Inc(s.opt_len, ulg(f) * (bits + xbits));
    if (stree <> NIL) then
      Inc(s.static_len, ulg(f) * (stree^[n].dl.Len + xbits));
  end;
  if (overflow = 0) then
    exit;
  {$ifdef DEBUG}
  Tracev(^M'bit length overflow');
  {$endif}
  { This happens for example on obj2 and pic of the Calgary corpus }

  { Find the first bit length which could increase: }
  repeat
    bits := max_length-1;
    while (s.bl_count[bits] = 0) do
      Dec(bits);
    Dec(s.bl_count[bits]);      { move one leaf down the tree }
    Inc(s.bl_count[bits+1], 2); { move one overflow item as its brother }
    Dec(s.bl_count[max_length]);
    { The brother of the overflow item also moves one step up,
      but this does not affect bl_count[max_length] }

    Dec(overflow, 2);
  until (overflow <= 0);

  { Now recompute all bit lengths, scanning in increasing frequency.
    h is still equal to HEAP_SIZE. (It is simpler to reconstruct all
    lengths instead of fixing only the wrong ones. This idea is taken
    from 'ar' written by Haruhiko Okumura.) }
  h := HEAP_SIZE;  { Delphi3: compiler warning w/o this }
  for bits := max_length downto 1 do
  begin
    n := s.bl_count[bits];
    while (n <> 0) do
    begin
      Dec(h);
      m := s.heap[h];
      if (m > max_code) then
        continue;
      if (tree^[m].dl.Len <> unsigned(bits)) then
      begin
        {$ifdef DEBUG}
        Trace('code '+IntToStr(m)+' bits '+IntToStr(tree^[m].dl.Len)
              +'.'+IntToStr(bits));
        {$ENDIF}
        Inc(s.opt_len, (long(bits) - long(tree^[m].dl.Len))
                        * long(tree^[m].fc.Freq) );
        tree^[m].dl.Len := ush(bits);
      end;
      Dec(n);
    end;
  end;
end;

{ ===========================================================================
  Construct one Huffman tree and assigns the code bit strings and lengths.
  Update the total bit length for the current block.
  IN assertion: the field freq is set for all tree elements.
  OUT assertions: the fields len and code are set to the optimal bit length
      and corresponding code. The length opt_len is updated; static_len is
      also updated if stree is not null. The field max_code is set. }

procedure build_tree(var s : deflate_state;
                     var desc : tree_desc); { the tree descriptor }

var
  tree : tree_ptr;
  stree : tree_ptr; {const}
  elems : int;
  n, m : int;          { iterate over heap elements }
  max_code : int;      { largest code with non zero frequency }
  node : int;          { new node being created }
begin
  tree := desc.dyn_tree;
  stree := desc.stat_desc^.static_tree;
  elems := desc.stat_desc^.elems;
  max_code := -1;

  { Construct the initial heap, with least frequent element in
    heap[SMALLEST]. The sons of heap[n] are heap[2*n] and heap[2*n+1].
    heap[0] is not used. }
  s.heap_len := 0;
  s.heap_max := HEAP_SIZE;

  for n := 0 to elems-1 do
  begin
    if (tree^[n].fc.Freq <> 0) then
    begin
      max_code := n;
      Inc(s.heap_len);
      s.heap[s.heap_len] := n;
      s.depth[n] := 0;
    end
    else
    begin
      tree^[n].dl.Len := 0;
    end;
  end;

  { The pkzip format requires that at least one distance code exists,
    and that at least one bit should be sent even if there is only one
    possible code. So to avoid special checks later on we force at least
    two codes of non zero frequency. }

  while (s.heap_len < 2) do
  begin
    Inc(s.heap_len);
    if (max_code < 2) then
    begin
      Inc(max_code);
      s.heap[s.heap_len] := max_code;
      node := max_code;
    end
    else
    begin
      s.heap[s.heap_len] := 0;
      node := 0;
    end;
    tree^[node].fc.Freq := 1;
    s.depth[node] := 0;
    Dec(s.opt_len);
    if (stree <> NIL) then
      Dec(s.static_len, stree^[node].dl.Len);
    { node is 0 or 1 so it does not have extra bits }
  end;
  desc.max_code := max_code;

  { The elements heap[heap_len/2+1 .. heap_len] are leaves of the tree,
    establish sub-heaps of increasing lengths: }

  for n := s.heap_len div 2 downto 1 do
    pqdownheap(s, tree^, n);

  { Construct the Huffman tree by repeatedly combining the least two
    frequent nodes. }

  node := elems;              { next internal node of the tree }
  repeat
    {pqremove(s, tree, n);}  { n := node of least frequency }
    n := s.heap[SMALLEST];
    s.heap[SMALLEST] := s.heap[s.heap_len];
    Dec(s.heap_len);
    pqdownheap(s, tree^, SMALLEST);

    m := s.heap[SMALLEST]; { m := node of next least frequency }

    Dec(s.heap_max);
    s.heap[s.heap_max] := n; { keep the nodes sorted by frequency }
    Dec(s.heap_max);
    s.heap[s.heap_max] := m;

    { Create a new node father of n and m }
    tree^[node].fc.Freq := tree^[n].fc.Freq + tree^[m].fc.Freq;
    { maximum }
    if (s.depth[n] >= s.depth[m]) then
      s.depth[node] := uch (s.depth[n] + 1)
    else
      s.depth[node] := uch (s.depth[m] + 1);

    tree^[m].dl.Dad := ush(node);
    tree^[n].dl.Dad := ush(node);
{$ifdef DUMP_BL_TREE}
    if (tree = tree_ptr(@s.bl_tree)) then
    begin
      WriteLn(#13'node ',node,'(',tree^[node].fc.Freq,') sons ',n,
              '(',tree^[n].fc.Freq,') ', m, '(',tree^[m].fc.Freq,')');
    end;
{$endif}
    { and insert the new node in the heap }
    s.heap[SMALLEST] := node;
    Inc(node);
    pqdownheap(s, tree^, SMALLEST);

  until (s.heap_len < 2);

  Dec(s.heap_max);
  s.heap[s.heap_max] := s.heap[SMALLEST];

  { At this point, the fields freq and dad are set. We can now
    generate the bit lengths. }

  gen_bitlen(s, desc);

  { The field len is now set, we can generate the bit codes }
  gen_codes (tree, max_code, s.bl_count);
end;

{ ===========================================================================
  Scan a literal or distance tree to determine the frequencies of the codes
  in the bit length tree. }

procedure scan_tree(var s : deflate_state;
                    var tree : array of ct_data;    { the tree to be scanned }
                    max_code : int);    { and its largest code of non zero frequency }
var
  n : int;                 { iterates over all tree elements }
  prevlen : int;           { last emitted length }
  curlen : int;            { length of current code }
  nextlen : int;           { length of next code }
  count : int;             { repeat count of the current code }
  max_count : int;         { max repeat count }
  min_count : int;         { min repeat count }
begin
  prevlen := -1;
  nextlen := tree[0].dl.Len;
  count := 0;
  max_count := 7;
  min_count := 4;

  if (nextlen = 0) then
  begin
    max_count := 138;
    min_count := 3;
  end;
  tree[max_code+1].dl.Len := ush($ffff); { guard }

  for n := 0 to max_code do
  begin
    curlen := nextlen;
    nextlen := tree[n+1].dl.Len;
    Inc(count);
    if (count < max_count) and (curlen = nextlen) then
      continue
    else
      if (count < min_count) then
        Inc(s.bl_tree[curlen].fc.Freq, count)
      else
        if (curlen <> 0) then
        begin
          if (curlen <> prevlen) then
            Inc(s.bl_tree[curlen].fc.Freq);
          Inc(s.bl_tree[REP_3_6].fc.Freq);
        end
        else
          if (count <= 10) then
            Inc(s.bl_tree[REPZ_3_10].fc.Freq)
          else
            Inc(s.bl_tree[REPZ_11_138].fc.Freq);

    count := 0;
    prevlen := curlen;
    if (nextlen = 0) then
    begin
      max_count := 138;
      min_count := 3;
    end
    else
      if (curlen = nextlen) then
      begin
        max_count := 6;
        min_count := 3;
      end
      else
      begin
        max_count := 7;
        min_count := 4;
      end;
  end;
end;

{ ===========================================================================
  Send a literal or distance tree in compressed form, using the codes in
  bl_tree. }

procedure send_tree(var s : deflate_state;
                    var tree : array of ct_data;    { the tree to be scanned }
                    max_code : int);    { and its largest code of non zero frequency }

var
  n : int;                { iterates over all tree elements }
  prevlen : int;          { last emitted length }
  curlen : int;           { length of current code }
  nextlen : int;          { length of next code }
  count : int;            { repeat count of the current code }
  max_count : int;        { max repeat count }
  min_count : int;        { min repeat count }
begin
  prevlen := -1;
  nextlen := tree[0].dl.Len;
  count := 0;
  max_count := 7;
  min_count := 4;

  { tree[max_code+1].dl.Len := -1; }  { guard already set }
  if (nextlen = 0) then
  begin
    max_count := 138;
    min_count := 3;
  end;

  for n := 0 to max_code do
  begin
    curlen := nextlen;
    nextlen := tree[n+1].dl.Len;
    Inc(count);
    if (count < max_count) and (curlen = nextlen) then
      continue
    else
      if (count < min_count) then
      begin
        repeat
          {$ifdef DEBUG}
          Tracevvv(#13'cd '+IntToStr(curlen));
          {$ENDIF}
          send_bits(s, s.bl_tree[curlen].fc.Code, s.bl_tree[curlen].dl.Len);
          Dec(count);
        until (count = 0);
      end
      else
        if (curlen <> 0) then
        begin
          if (curlen <> prevlen) then
          begin
            {$ifdef DEBUG}
            Tracevvv(#13'cd '+IntToStr(curlen));
            {$ENDIF}
            send_bits(s, s.bl_tree[curlen].fc.Code, s.bl_tree[curlen].dl.Len);
            Dec(count);
          end;
          {$IFDEF DEBUG}
          Assert((count >= 3) and (count <= 6), ' 3_6?');
          {$ENDIF}
          {$ifdef DEBUG}
          Tracevvv(#13'cd '+IntToStr(REP_3_6));
          {$ENDIF}
          send_bits(s, s.bl_tree[REP_3_6].fc.Code, s.bl_tree[REP_3_6].dl.Len);
          send_bits(s, count-3, 2);
        end
        else
          if (count <= 10) then
          begin
            {$ifdef DEBUG}
            Tracevvv(#13'cd '+IntToStr(REPZ_3_10));
            {$ENDIF}
            send_bits(s, s.bl_tree[REPZ_3_10].fc.Code, s.bl_tree[REPZ_3_10].dl.Len);
            send_bits(s, count-3, 3);
          end
          else
          begin
            {$ifdef DEBUG}
            Tracevvv(#13'cd '+IntToStr(REPZ_11_138));
            {$ENDIF}
            send_bits(s, s.bl_tree[REPZ_11_138].fc.Code, s.bl_tree[REPZ_11_138].dl.Len);
            send_bits(s, count-11, 7);
          end;
    count := 0;
    prevlen := curlen;
    if (nextlen = 0) then
    begin
      max_count := 138;
      min_count := 3;
    end
    else
      if (curlen = nextlen) then
      begin
        max_count := 6;
        min_count := 3;
      end
      else
      begin
        max_count := 7;
        min_count := 4;
      end;
  end;
end;

{ ===========================================================================
  Construct the Huffman tree for the bit lengths and return the index in
  bl_order of the last bit length code to send. }

function build_bl_tree(var s : deflate_state) : int;
var
  max_blindex : int;  { index of last bit length code of non zero freq }
begin
  { Determine the bit length frequencies for literal and distance trees }
  scan_tree(s, s.dyn_ltree, s.l_desc.max_code);
  scan_tree(s, s.dyn_dtree, s.d_desc.max_code);

  { Build the bit length tree: }
  build_tree(s, s.bl_desc);
  { opt_len now includes the length of the tree representations, except
    the lengths of the bit lengths codes and the 5+5+4 bits for the counts. }

  { Determine the number of bit length codes to send. The pkzip format
    requires that at least 4 bit length codes be sent. (appnote.txt says
    3 but the actual value used is 4.) }

  for max_blindex := BL_CODES-1 downto 3 do
  begin
    if (s.bl_tree[bl_order[max_blindex]].dl.Len <> 0) then
      break;
  end;
  { Update opt_len to include the bit length tree and counts }
  Inc(s.opt_len, 3*(max_blindex+1) + 5+5+4);
  {$ifdef DEBUG}
  Tracev(^M'dyn trees: dyn %ld, stat %ld {s.opt_len, s.static_len}');
  {$ENDIF}

  build_bl_tree := max_blindex;
end;

{ ===========================================================================
  Send the header for a block using dynamic Huffman trees: the counts, the
  lengths of the bit length codes, the literal tree and the distance tree.
  IN assertion: lcodes >= 257, dcodes >= 1, blcodes >= 4. }

procedure send_all_trees(var s : deflate_state;
                         lcodes : int;
                         dcodes : int;
                         blcodes : int); { number of codes for each tree }
var
  rank : int;                    { index in bl_order }
begin
  {$IFDEF DEBUG}
  Assert ((lcodes >= 257) and (dcodes >= 1) and (blcodes >= 4),
          'not enough codes');
  Assert ((lcodes <= L_CODES) and (dcodes <= D_CODES)
          and (blcodes <= BL_CODES), 'too many codes');
  Tracev(^M'bl counts: ');
  {$ENDIF}
  send_bits(s, lcodes-257, 5); { not +255 as stated in appnote.txt }
  send_bits(s, dcodes-1,   5);
  send_bits(s, blcodes-4,  4); { not -3 as stated in appnote.txt }
  for rank := 0 to blcodes-1 do
  begin
    {$ifdef DEBUG}
    Tracev(^M'bl code '+IntToStr(bl_order[rank]));
    {$ENDIF}
    send_bits(s, s.bl_tree[bl_order[rank]].dl.Len, 3);
  end;
  {$ifdef DEBUG}
  Tracev(^M'bl tree: sent '+IntToStr(s.bits_sent));
  {$ENDIF}

  send_tree(s, s.dyn_ltree, lcodes-1); { literal tree }
  {$ifdef DEBUG}
  Tracev(^M'lit tree: sent '+IntToStr(s.bits_sent));
  {$ENDIF}

  send_tree(s, s.dyn_dtree, dcodes-1); { distance tree }
  {$ifdef DEBUG}
  Tracev(^M'dist tree: sent '+IntToStr(s.bits_sent));
  {$ENDIF}
end;

{ ===========================================================================
  Flush the bit buffer and align the output on a byte boundary }

procedure bi_windup(var s : deflate_state);
begin
  if (s.bi_valid > 8) then
  begin
    {put_short(s, s.bi_buf);}
    s.pending_buf^[s.pending] := uch(s.bi_buf and $ff);
    Inc(s.pending);
    s.pending_buf^[s.pending] := uch(ush(s.bi_buf) shr 8);;
    Inc(s.pending);
  end
  else
    if (s.bi_valid > 0) then
    begin
      {put_byte(s, (Byte)s^.bi_buf);}
      s.pending_buf^[s.pending] := Byte(s.bi_buf);
      Inc(s.pending);
    end;
  s.bi_buf := 0;
  s.bi_valid := 0;
{$ifdef DEBUG}
  s.bits_sent := (s.bits_sent+7) and (not 7);
{$endif}
end;

{ ===========================================================================
  Copy a stored block, storing first the length and its
  one's complement if requested. }

procedure copy_block(var s : deflate_state;
                     buf : pcharf;      { the input data }
                     len : unsigned;    { its length }
                     header : boolean); { true if block header must be written }
begin
  bi_windup(s);        { align on byte boundary }
  s.last_eob_len := 8; { enough lookahead for inflate }

  if (header) then
  begin
    {put_short(s, (ush)len);}
    s.pending_buf^[s.pending] := uch(ush(len) and $ff);
    Inc(s.pending);
    s.pending_buf^[s.pending] := uch(ush(len) shr 8);;
    Inc(s.pending);
    {put_short(s, (ush)~len);}
    s.pending_buf^[s.pending] := uch(ush(not len) and $ff);
    Inc(s.pending);
    s.pending_buf^[s.pending] := uch(ush(not len) shr 8);;
    Inc(s.pending);

{$ifdef DEBUG}
    Inc(s.bits_sent, 2*16);
{$endif}
  end;
{$ifdef DEBUG}
  Inc(s.bits_sent, ulg(len shl 3));
{$endif}
  while (len <> 0) do
  begin
    Dec(len);
    {put_byte(s, *buf++);}
    s.pending_buf^[s.pending] := buf^;
    Inc(buf);
    Inc(s.pending);
  end;
end;


{ ===========================================================================
  Send a stored block }

procedure _tr_stored_block(var s : deflate_state;
                           buf : pcharf;     { input block }
                           stored_len : ulg; { length of input block }
                           eof : boolean);   { true if this is the last block for a file }

begin
  send_bits(s, (STORED_BLOCK shl 1)+ord(eof), 3);  { send block type }
  s.compressed_len := (s.compressed_len + 3 + 7) and ulg(not Long(7));
  Inc(s.compressed_len, (stored_len + 4) shl 3);

  copy_block(s, buf, unsigned(stored_len), TRUE); { with header }
end;

{ ===========================================================================
  Flush the bit buffer, keeping at most 7 bits in it. }

procedure bi_flush(var s : deflate_state);
begin
  if (s.bi_valid = 16) then
  begin
    {put_short(s, s.bi_buf);}
    s.pending_buf^[s.pending] := uch(s.bi_buf and $ff);
    Inc(s.pending);
    s.pending_buf^[s.pending] := uch(ush(s.bi_buf) shr 8);;
    Inc(s.pending);

    s.bi_buf := 0;
    s.bi_valid := 0;
  end
  else
   if (s.bi_valid >= 8) then
   begin
     {put_byte(s, (Byte)s^.bi_buf);}
     s.pending_buf^[s.pending] := Byte(s.bi_buf);
     Inc(s.pending);

     s.bi_buf := s.bi_buf shr 8;
     Dec(s.bi_valid, 8);
   end;
end;


{ ===========================================================================
  Send one empty static block to give enough lookahead for inflate.
  This takes 10 bits, of which 7 may remain in the bit buffer.
  The current inflate code requires 9 bits of lookahead. If the
  last two codes for the previous block (real code plus EOB) were coded
  on 5 bits or less, inflate may have only 5+3 bits of lookahead to decode
  the last real code. In this case we send two empty static blocks instead
  of one. (There are no problems if the previous block is stored or fixed.)
  To simplify the code, we assume the worst case of last real code encoded
  on one bit only. }

procedure _tr_align(var s : deflate_state);
begin
  send_bits(s, STATIC_TREES shl 1, 3);
  {$ifdef DEBUG}
  Tracevvv(#13'cd '+IntToStr(END_BLOCK));
  {$ENDIF}
  send_bits(s, static_ltree[END_BLOCK].fc.Code, static_ltree[END_BLOCK].dl.Len);
  Inc(s.compressed_len, Long(10)); { 3 for block type, 7 for EOB }
  bi_flush(s);
  { Of the 10 bits for the empty block, we have already sent
    (10 - bi_valid) bits. The lookahead for the last real code (before
    the EOB of the previous block) was thus at least one plus the length
    of the EOB plus what we have just sent of the empty static block. }
  if (1 + s.last_eob_len + 10 - s.bi_valid < 9) then
  begin
    send_bits(s, STATIC_TREES shl 1, 3);
    {$ifdef DEBUG}
    Tracevvv(#13'cd '+IntToStr(END_BLOCK));
    {$ENDIF}
    send_bits(s, static_ltree[END_BLOCK].fc.Code, static_ltree[END_BLOCK].dl.Len);
    Inc(s.compressed_len, Long(10));
    bi_flush(s);
  end;
  s.last_eob_len := 7;
end;

{ ===========================================================================
  Set the data type to ASCII or BINARY, using a crude approximation:
  binary if more than 20% of the bytes are <= 6 or >= 128, ascii otherwise.
  IN assertion: the fields freq of dyn_ltree are set and the total of all
  frequencies does not exceed 64K (to fit in an int on 16 bit machines). }

procedure set_data_type(var s : deflate_state);
var
  n : int;
  ascii_freq : unsigned;
  bin_freq : unsigned;
begin
  n := 0;
  ascii_freq := 0;
  bin_freq := 0;

  while (n < 7) do
  begin
    Inc(bin_freq, s.dyn_ltree[n].fc.Freq);
    Inc(n);
  end;
  while (n < 128) do
  begin
    Inc(ascii_freq, s.dyn_ltree[n].fc.Freq);
    Inc(n);
  end;
  while (n < LITERALS) do
  begin
    Inc(bin_freq, s.dyn_ltree[n].fc.Freq);
    Inc(n);
  end;
  if (bin_freq > (ascii_freq shr 2)) then
    s.data_type := Byte(Z_BINARY)
  else
    s.data_type := Byte(Z_ASCII);
end;

{ ===========================================================================
  Send the block data compressed using the given Huffman trees }

procedure compress_block(var s : deflate_state;
                         var ltree : array of ct_data;   { literal tree }
                         var dtree : array of ct_data);  { distance tree }
var
  dist : unsigned;      { distance of matched string }
  lc : int;             { match length or unmatched char (if dist == 0) }
  lx : unsigned;        { running index in l_buf }
  code : unsigned;      { the code to send }
  extra : int;          { number of extra bits to send }
begin
  lx := 0;
  if (s.last_lit <> 0) then
  repeat
    dist := s.d_buf^[lx];
    lc := s.l_buf^[lx];
    Inc(lx);
    if (dist = 0) then
    begin
      { send a literal byte }
      {$ifdef DEBUG}
      Tracevvv(#13'cd '+IntToStr(lc));
      Tracecv((lc > 31) and (lc < 128), ' '+AnsiChar(lc)+' ');
      {$ENDIF}
      send_bits(s, ltree[lc].fc.Code, ltree[lc].dl.Len);
    end
    else
    begin
      { Here, lc is the match length - MIN_MATCH }
      code := _length_code[lc];
      { send the length code }
      {$ifdef DEBUG}
      Tracevvv(#13'cd '+IntToStr(code+LITERALS+1));
      {$ENDIF}
      send_bits(s, ltree[code+LITERALS+1].fc.Code, ltree[code+LITERALS+1].dl.Len);
      extra := extra_lbits[code];
      if (extra <> 0) then
      begin
        Dec(lc, base_length[code]);
        send_bits(s, lc, extra);       { send the extra length bits }
      end;
      Dec(dist); { dist is now the match distance - 1 }
      {code := d_code(dist);}
      if (dist < 256) then
        code := _dist_code[dist]
      else
        code := _dist_code[256+(dist shr 7)];

      {$IFDEF DEBUG}
      Assert (code < D_CODES, 'bad d_code');
      {$ENDIF}

      { send the distance code }
      {$ifdef DEBUG}
      Tracevvv(#13'cd '+IntToStr(code));
      {$ENDIF}
      send_bits(s, dtree[code].fc.Code, dtree[code].dl.Len);
      extra := extra_dbits[code];
      if (extra <> 0) then
      begin
        Dec(dist, base_dist[code]);
        send_bits(s, dist, extra);   { send the extra distance bits }
      end;
    end; { literal or match pair ? }

    { Check that the overlay between pending_buf and d_buf+l_buf is ok: }
    {$IFDEF DEBUG}
    Assert(s.pending < s.lit_bufsize + 2*lx, 'pendingBuf overflow');
    {$ENDIF}
  until (lx >= s.last_lit);

  {$ifdef DEBUG}
  Tracevvv(#13'cd '+IntToStr(END_BLOCK));
  {$ENDIF}
  send_bits(s, ltree[END_BLOCK].fc.Code, ltree[END_BLOCK].dl.Len);
  s.last_eob_len := ltree[END_BLOCK].dl.Len;
end;


{ ===========================================================================
  Determine the best encoding for the current block: dynamic trees, static
  trees or store, and output the encoded block to the zip file. This function
  returns the total compressed length for the file so far. }

function _tr_flush_block (var s : deflate_state;
         buf : pcharf;         { input block, or NULL if too old }
         stored_len : ulg;     { length of input block }
         eof : boolean) : ulg; { true if this is the last block for a file }
var
  opt_lenb, static_lenb : ulg; { opt_len and static_len in bytes }
  max_blindex : int;  { index of last bit length code of non zero freq }
begin
  max_blindex := 0;

  { Build the Huffman trees unless a stored block is forced }
  if (s.level > 0) then
  begin
    { Check if the file is ascii or binary }
    if (s.data_type = Z_UNKNOWN) then
      set_data_type(s);

    { Construct the literal and distance trees }
    build_tree(s, s.l_desc);
    {$ifdef DEBUG}
    Tracev(^M'lit data: dyn %ld, stat %ld {s.opt_len, s.static_len}');
    {$ENDIF}

    build_tree(s, s.d_desc);
    {$ifdef DEBUG}
    Tracev(^M'dist data: dyn %ld, stat %ld {s.opt_len, s.static_len}');
    {$ENDIF}
    { At this point, opt_len and static_len are the total bit lengths of
      the compressed block data, excluding the tree representations. }

    { Build the bit length tree for the above two trees, and get the index
      in bl_order of the last bit length code to send. }
    max_blindex := build_bl_tree(s);

    { Determine the best encoding. Compute first the block length in bytes}
    opt_lenb := (s.opt_len+3+7) shr 3;
    static_lenb := (s.static_len+3+7) shr 3;

    {$ifdef DEBUG}
    Tracev(^M'opt %lu(%lu) stat %lu(%lu) stored %lu lit %u '+
	    '{opt_lenb, s.opt_len, static_lenb, s.static_len, stored_len,'+
	    's.last_lit}');
    {$ENDIF}

    if (static_lenb <= opt_lenb) then
      opt_lenb := static_lenb;

  end
  else
  begin
    {$IFDEF DEBUG}
    Assert(buf <> pcharf(NIL), 'lost buf');
    {$ENDIF}
    static_lenb := stored_len + 5;
    opt_lenb := static_lenb;        { force a stored block }
  end;

  { If compression failed and this is the first and last block,
    and if the .zip file can be seeked (to rewrite the local header),
    the whole file is transformed into a stored file:  }

{$ifdef STORED_FILE_OK}
{$ifdef FORCE_STORED_FILE}
  if eof and (s.compressed_len = Long(0)) then
  begin { force stored file }
{$else}
  if (stored_len <= opt_lenb) and eof and (s.compressed_len=Long(0))
     and seekable()) do
  begin
{$endif}
    { Since LIT_BUFSIZE <= 2*WSIZE, the input data must be there: }
    if (buf = pcharf(0)) then
      error ('block vanished');

    copy_block(buf, unsigned(stored_len), 0); { without header }
    s.compressed_len := stored_len shl 3;
    s.method := STORED;
  end
  else
{$endif} { STORED_FILE_OK }

{$ifdef FORCE_STORED}
  if (buf <> pcharf(0)) then
  begin { force stored block }
{$else}
  if (stored_len+4 <= opt_lenb) and (buf <> pcharf(0)) then
  begin
                     { 4: two words for the lengths }
{$endif}
    { The test buf <> NULL is only necessary if LIT_BUFSIZE > WSIZE.
      Otherwise we can't have processed more than WSIZE input bytes since
      the last block flush, because compression would have been
      successful. If LIT_BUFSIZE <= WSIZE, it is never too late to
      transform a block into a stored block. }

    _tr_stored_block(s, buf, stored_len, eof);

{$ifdef FORCE_STATIC}
  end
  else
    if (static_lenb >= 0) then
    begin { force static trees }
{$else}
  end
  else
    if (static_lenb = opt_lenb) then
    begin
{$endif}
      send_bits(s, (STATIC_TREES shl 1)+ord(eof), 3);
      compress_block(s, static_ltree, static_dtree);
      Inc(s.compressed_len, 3 + s.static_len);
    end
    else
    begin
      send_bits(s, (DYN_TREES shl 1)+ord(eof), 3);
      send_all_trees(s, s.l_desc.max_code+1, s.d_desc.max_code+1,
                     max_blindex+1);
      compress_block(s, s.dyn_ltree, s.dyn_dtree);
      Inc(s.compressed_len, 3 + s.opt_len);
    end;
  {$ifdef DEBUG}
  Assert (s.compressed_len = s.bits_sent, 'bad compressed size');
  {$ENDIF}
  init_block(s);

  if (eof) then
  begin
    bi_windup(s);
    Inc(s.compressed_len, 7);  { align on byte boundary }
  end;
  {$ifdef DEBUG}
  Tracev(#13'comprlen %lu(%lu) {s.compressed_len shr 3,'+
         's.compressed_len-7*ord(eof)}');
  {$ENDIF}

  _tr_flush_block := s.compressed_len shr 3;
end;


{ ===========================================================================
  Save the match info and tally the frequency counts. Return true if
  the current block must be flushed. }

function _tr_tally (var s : deflate_state;
   dist : unsigned;          { distance of matched string }
   lc : unsigned) : boolean; { match length-MIN_MATCH or unmatched char (if dist=0) }
var
  {$IFDEF DEBUG}
  MAX_DIST : ush;
  {$ENDIF}
  code : ush;
{$ifdef TRUNCATE_BLOCK}
var
  out_length : ulg;
  in_length : ulg;
  dcode : int;
{$endif}
begin
  s.d_buf^[s.last_lit] := ush(dist);
  s.l_buf^[s.last_lit] := uch(lc);
  Inc(s.last_lit);
  if (dist = 0) then
  begin
    { lc is the unmatched char }
    Inc(s.dyn_ltree[lc].fc.Freq);
  end
  else
  begin
    Inc(s.matches);
    { Here, lc is the match length - MIN_MATCH }
    Dec(dist);             { dist := match distance - 1 }

    {macro d_code(dist)}
    if (dist) < 256 then
      code := _dist_code[dist]
    else
      code := _dist_code[256+(dist shr 7)];
    {$IFDEF DEBUG}
{macro  MAX_DIST(s) <=> ((s)^.w_size-MIN_LOOKAHEAD)
   In order to simplify the code, particularly on 16 bit machines, match
   distances are limited to MAX_DIST instead of WSIZE. }
    MAX_DIST := ush(s.w_size-MIN_LOOKAHEAD);
    Assert((dist < ush(MAX_DIST)) and
           (ush(lc) <= ush(MAX_MATCH-MIN_MATCH)) and
           (ush(code) < ush(D_CODES)),  '_tr_tally: bad match');
    {$ENDIF}
    Inc(s.dyn_ltree[_length_code[lc]+LITERALS+1].fc.Freq);
    {s.dyn_dtree[d_code(dist)].Freq++;}
    Inc(s.dyn_dtree[code].fc.Freq);
  end;

{$ifdef TRUNCATE_BLOCK}
  { Try to guess if it is profitable to stop the current block here }
  if (s.last_lit and $1fff = 0) and (s.level > 2) then
  begin
    { Compute an upper bound for the compressed length }
    out_length := ulg(s.last_lit)*Long(8);
    in_length := ulg(long(s.strstart) - s.block_start);
    for dcode := 0 to D_CODES-1 do
    begin
      Inc(out_length, ulg(s.dyn_dtree[dcode].fc.Freq *
            (Long(5)+extra_dbits[dcode])) );
    end;
    out_length := out_length shr 3;
    {$ifdef DEBUG}
    Tracev(^M'last_lit %u, in %ld, out ~%ld(%ld%%) ');
          { s.last_lit, in_length, out_length,
           Long(100) - out_length*Long(100) div in_length)); }
    {$ENDIF}
    if (s.matches < s.last_lit div 2) and (out_length < in_length div 2) then
    begin
      _tr_tally := TRUE;
      exit;
    end;
  end;
{$endif}
  _tr_tally := (s.last_lit = s.lit_bufsize-1);
  { We avoid equality with lit_bufsize because of wraparound at 64K
    on 16 bit machines and because stored blocks are restricted to
    64K-1 bytes. }
end;

end.
