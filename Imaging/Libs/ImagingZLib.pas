{*******************************************************}
{                                                       }
{       Delphi Supplemental Components                  }
{       ZLIB Data Compression Interface Unit            }
{                                                       }
{       Copyright (c) 1997 Borland International        }
{       Copyright (c) 1998 Jacques Nomssi Nzali         }
{                                                       }
{*******************************************************}

{
  Modified for
  Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  https://imaginglib.sourceforge.io
}

{ All zlib (de)compression in Imaging goes through this unit. It selects
  the zlib implementation (backend) at compile time and provides buffer to
  buffer functions and compression streams, so the rest of
  the library doesn't depend on which zlib is in use.

  Backends:

  ZLIB_USE_BUNDLED - ImPasZLib unit, Pascal translation of zlib 1.1.2 bundled with
                     Imaging. Compiles everywhere (Delphi 7+, FPC) and needs no
                     external files, but it is the slowest one.
  ZLIB_USE_FPC     - paszlib package of Free Pascal (Pascal translation as well).
                     It is usually linked to FPC and Lazarus programs anyway so
                     there is no need to add another, almost identical, copy.
  ZLIB_USE_DELPHI  - ZLib unit of Delphi RTL: recent zlib compiled from C sources
                     and linked statically, can be 2x faster than ZLIB_USE_BUNDLED.
  ZLIB_USE_DYNLIB  - zlib shared library bound by ZLibDynLib unit: zlib1.dll,
                     libz.so.1, or libz.1.dylib (zlib-ng in compat mode works too).
                     Ignored on targets without dynamic libraries.

  Selection:

  One of the Pascal or compiled-in backends is always there, chosen
  automatically:
    - Free Pascal: ZLIB_USE_FPC
    - Delphi XE3 and newer: ZLIB_USE_DELPHI
    - older Delphi: ZLIB_USE_BUNDLED
  Define ZLIB_USE_BUNDLED to use the bundled one with all compilers.
  ZLIB_USE_FPC and ZLIB_USE_DELPHI are set by the automatic choice and are not
  meant to be defined by hand.

  ZLIB_USE_DYNLIB is an addition to that backend, not a replacement. When it
  is defined the library is looked for at program start (or loaded by
  TryLoadZLibLibrary). If it is found it does all compression and
  decompression, otherwise the compiled-in backend is used, so the program
  works without the library too. ZLibBackend tells which one is active.

  The defines are listed in ImagingOptions.inc, set them there, in project
  options, or in ImagingUserOptions.inc.
}

unit ImagingZLib;

{$I ImagingOptions.inc}

interface

{$IFNDEF HAS_DYNLIBS}
  {$UNDEF ZLIB_USE_DYNLIB}
{$ENDIF}

{$IFNDEF ZLIB_USE_BUNDLED}
  {$DEFINE NO_USER_ZLIB}
{$ENDIF}


{$IF Defined(FPC) and Defined(NO_USER_ZLIB)}
  { Automatically use FPC's PasZLib when compiling with FPC.}
  {$DEFINE ZLIB_USE_FPC}
{$IFEND}

{$IF Defined(DELPHI) and (CompilerVersion > 23) and Defined(NO_USER_ZLIB)}
  { Automatically use Delphi's ZLib when compiling with Delphi (recent versions include
    recentish ZLib compiled to objects, can be 2x faster than ZLIB_USE_BUNDLED).
    Needs Delphi XE2+ for deflateInit2 etc.}
  {$DEFINE ZLIB_USE_DELPHI}
{$IFEND}

{$IF not Defined(ZLIB_USE_FPC) and not Defined(ZLIB_USE_DELPHI)}
  {$DEFINE ZLIB_USE_BUNDLED}  // Fallback
{$IFEND}

uses
{$IFDEF ZLIB_USE_DYNLIB}
  { zlib dll/so/dylib, used when found at run time. Can also use zlib-ng compat.
    Listed first so that unqualified zlib names refer to the backend below,
    the library's ones are always qualified (ZLibDynLib.inflate). }
  ZLibDynLib,
{$ENDIF}
{$IF Defined(ZLIB_USE_BUNDLED)}
  { Use paszlib modified by me for Delphi and FPC. Fall-back solution. }
  ImPasZLib,
{$ELSEIF Defined(ZLIB_USE_FPC)}
  { Use FPC's paszlib }
  zbase, paszlib,
{$ELSEIF Defined(ZLIB_USE_DELPHI)}
  { Use ZLib unit shipped with Delphi }
  ZLib,
{$IFEND}
  ImagingTypes, SysUtils, Classes;

{$IF not Defined(TZStreamRec)}
type
  TZStreamRec = z_stream;
{$IFEND}

  { CompressBuf compresses data, buffer to buffer, in one call.
   In: InBuf = ptr to compressed data
       InBytes = number of bytes in InBuf
  Out: OutBuf = ptr to newly allocated buffer containing decompressed data
       OutBytes = number of bytes in OutBuf   }
  procedure CompressBuf(const InBuf: Pointer; InBytes: NativeInt;
    var OutBuf: Pointer; var OutBytes: NativeInt;
    CompressLevel: Integer = Z_DEFAULT_COMPRESSION;
    CompressStrategy: Integer = Z_DEFAULT_STRATEGY);

  { DecompressBuf decompresses data, buffer to buffer, in one call.
     In: InBuf = ptr to compressed data
         InBytes = number of bytes in InBuf
         OutEstimate = zero, or est. size of the decompressed data
    Out: OutBuf = ptr to newly allocated buffer containing decompressed data
         OutBytes = number of bytes in OutBuf   }
  procedure DecompressBuf(const InBuf: Pointer; InBytes: NativeInt;
    OutEstimate: NativeInt; var OutBuf: Pointer; var OutBytes: NativeInt);

type
  { State of decompression of one zlib stream, see ZInflateBegin.}
  TZInflater = record
    // Stream of the backend chosen when the stream starts, used until its end
    Backend: TObject;
    Active: Boolean;
    Finished: Boolean;
  end;

  { State of compression of one zlib stream, see ZDeflateBegin.}
  TZDeflater = record
    // Stream of the backend chosen when the stream starts, used until its end
    Backend: TObject;
    Active: Boolean;
    Finished: Boolean;
  end;

const
  { The largest number of bytes given to zlib in one call, its byte counts
    are 32-bit. Larger inputs and outputs are passed in more calls.}
  MaxZLibBlock = 1 shl 30;  // 1 GB

{ Starts decompression of a zlib stream. Data are passed in by ZInflateData,
  ZInflateEnd must be called at the end (also when an exception is raised).}
procedure ZInflateBegin(var Inflater: TZInflater);
{ Decompresses data from Input to Output. Consumed and Produced return
  the numbers of bytes read from Input and written to Output. Returns False
  when the end of the zlib stream has been reached (data after it are not
  read), True when more input or output space is needed. Raises
  EDecompressionError for damaged data.}
function ZInflateData(var Inflater: TZInflater; Input: Pointer; InputSize: NativeInt;
  Output: Pointer; OutputSize: NativeInt; out Consumed, Produced: NativeInt): Boolean;
{ Releases memory used by decompression.}
procedure ZInflateEnd(var Inflater: TZInflater);

{ Starts compression to a zlib stream with given level (0..9 or
  Z_DEFAULT_COMPRESSION) and strategy (Z_DEFAULT_STRATEGY, ...). Data are
  passed in by ZDeflateData, ZDeflateEnd must be called at the end (also when
  an exception is raised).}
procedure ZDeflateBegin(var Deflater: TZDeflater; Level, Strategy: Integer);
{ Compresses data from Input to Output. Consumed and Produced return
  the numbers of bytes read from Input and written to Output. With Finish set
  the rest of the stream is written out after the input (call it again with
  more output space while it returns True). Returns False when the whole
  stream has been written.}
function ZDeflateData(var Deflater: TZDeflater; Input: Pointer; InputSize: NativeInt;
  Output: Pointer; OutputSize: NativeInt; Finish: Boolean; out Consumed, Produced: NativeInt): Boolean;
{ Releases memory used by compression.}
procedure ZDeflateEnd(var Deflater: TZDeflater);

type
  { Abstract ancestor class }
  TCustomZLibStream = class(TStream)
  private
    FStrm: TStream;
    FStrmPos: Int64;
    FOnProgress: TNotifyEvent;
    FBuffer: array [Word] of Byte;
    // Numbers of uncompressed and compressed bytes processed so far, zlib's
    // own counters are 32-bit
    FTotalIn, FTotalOut: Int64;
  protected
    procedure Progress(Sender: TObject); dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  public
    constructor Create(Strm: TStream);
  end;

{ TCompressionStream compresses data on the fly as data is written to it, and
  stores the compressed data to another stream.

  TCompressionStream is write-only and strictly sequential. Reading from the
  stream will raise an exception. Using Seek to move the stream pointer
  will raise an exception.

  Output data is cached internally, written to the output stream only when
  the internal output buffer is full.  All pending output data is flushed
  when the stream is destroyed.

  The Position property returns the number of uncompressed bytes of
  data that have been written to the stream so far.

  CompressionRate returns the on-the-fly percentage by which the original
  data has been compressed:  (1 - (CompressedBytes / UncompressedBytes)) * 100
  If raw data size = 100 and compressed data size = 25, the CompressionRate
  is 75%

  The OnProgress event is called each time the output buffer is filled and
  written to the output stream.  This is useful for updating a progress
  indicator when you are writing a large chunk of data to the compression
  stream in a single call.}

  TCompressionLevel = (clNone, clFastest, clDefault, clMax);

  TCompressionStream = class(TCustomZlibStream)
  private
    FDeflater: TZDeflater;
    FOutLen: Integer; // Compressed bytes in FBuffer
    function GetCompressionRate: Single;
    procedure FlushBuffer;
  public
    constructor Create(CompressionLevel: TCompressionLevel; Dest: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;

{ TDecompressionStream decompresses data on the fly as data is read from it.

  Compressed data comes from a separate source stream.  TDecompressionStream
  is read-only and unidirectional; you can seek forward in the stream, but not
  backwards.  The special case of setting the stream position to zero is
  allowed.  Seeking forward decompresses data until the requested position in
  the uncompressed data has been reached.  Seeking backwards, seeking relative
  to the end of the stream, requesting the size of the stream, and writing to
  the stream will raise an exception.

  The Position property returns the number of bytes of uncompressed data that
  have been read from the stream so far.

  The OnProgress event is called each time the internal input buffer of
  compressed data is exhausted and the next block is read from the input stream.
  This is useful for updating a progress indicator when you are reading a
  large chunk of data from the decompression stream in a single call.}

  TDecompressionStream = class(TCustomZlibStream)
  private
    FInflater: TZInflater;
    FStrmStart: Int64;        // Position of compressed data in source stream
    FBufPos, FBufLen: Integer; // Unread compressed data in FBuffer
  public
    constructor Create(Source: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property OnProgress;
  end;

type
  EZlibError = class(Exception);
  ECompressionError = class(EZlibError);
  EDecompressionError = class(EZlibError);

type
  { zlib implementation compiled in, see Backends above.}
  TZLibBackend = (
    zbBundled,  // ImPasZLib
    zbFpc,      // paszlib package of Free Pascal
    zbDelphi,   // ZLib unit of Delphi RTL
    zbDynLib    // zlib shared library
  );

{ Returns zlib implementation in use: zbDynLib when the shared library is
  loaded, otherwise the backend chosen at compile time.}
function ZLibBackend: TZLibBackend;
{ Returns version of zlib reported by the backend, e.g. '1.3.1'.}
function GetZLibVersion: string;
{ Loads zlib library from the given file and uses it from now on (instead of
  the one found in the default locations). Returns False and falls back to
  the compiled-in backend when the library can't be loaded. Call it before
  any compression or decompression starts, the previously loaded library is
  unloaded. Always False in builds without ZLIB_USE_DYNLIB.}
function TryLoadZLibLibrary(const FileName: string): Boolean;

implementation

const
  // Version string used with the API calls. We use just very basic
  // parts supported since forever.
  MinZLibVersion = '1.1.2';

{$IFDEF ZLIB_USE_DELPHI}
  // Use Pascal mem allocation when using compiled C libs (shared library
  // always gets it)
  {$DEFINE USE_PASCAL_ALLOC}
const
  DEF_MEM_LEVEL = 9;
  MAX_WBITS = 15;
{$ENDIF}

const
  ZErrorMessages: array[0..9] of PAnsiChar = (
    'need dictionary',        // Z_NEED_DICT      (2)
    'stream end',             // Z_STREAM_END     (1)
    '',                       // Z_OK             (0)
    'file error',             // Z_ERRNO          (-1)
    'stream error',           // Z_STREAM_ERROR   (-2)
    'data error',             // Z_DATA_ERROR     (-3)
    'insufficient memory',    // Z_MEM_ERROR      (-4)
    'buffer error',           // Z_BUF_ERROR      (-5)
    'incompatible version',   // Z_VERSION_ERROR  (-6)
    '');

function zlibAllocMem(AppData: Pointer; Items, Size: Cardinal): Pointer; cdecl;
begin
  GetMem(Result, Items*Size);
end;

procedure zlibFreeMem(AppData, Block: Pointer); cdecl;
begin
  FreeMem(Block);
end;

function CCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise ECompressionError.Create('zlib: ' + ZErrorMessages[2 - code]);
end;

function DCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise EDecompressionError.Create('zlib: ' + ZErrorMessages[2 - code]);
end;

type
  { One zlib stream (decompression or compression) of a backend. The backends
    have their own z_stream records with different layouts, each class keeps
    its own one.}
  TZBackendStream = class
  public
    function BeginInflate: Integer; virtual; abstract;
    { Runs inflate for one block of input and output (at most MaxZLibBlock
      bytes each), Used and Made return the numbers of bytes read and written.}
    function RunInflate(Input: Pointer; InputSize: NativeInt; Output: Pointer;
                     OutputSize: NativeInt; out Used, Made: NativeInt): Integer; virtual; abstract;
    procedure EndInflate; virtual; abstract;
    function BeginDeflate(Level, Strategy: Integer): Integer; virtual; abstract;
    { Runs deflate for one block of input and output, see RunInflate.}
    function RunDeflate(Input: Pointer; InputSize: NativeInt; Output: Pointer;
                     OutputSize: NativeInt; Flush: Integer; out Used, Made: NativeInt): Integer; virtual; abstract;
    procedure EndDeflate; virtual; abstract;
  end;

  { Backend compiled in: ImPasZLib, FPC's paszlib, or ZLib unit of Delphi.}
  TStaticZStream = class(TZBackendStream)
  private
    FStream: TZStreamRec;
  public
    function BeginInflate: Integer; override;
    function RunInflate(Input: Pointer; InputSize: NativeInt; Output: Pointer;
                     OutputSize: NativeInt; out Used, Made: NativeInt): Integer; override;
    procedure EndInflate; override;
    function BeginDeflate(Level, Strategy: Integer): Integer; override;
    function RunDeflate(Input: Pointer; InputSize: NativeInt; Output: Pointer;
                     OutputSize: NativeInt; Flush: Integer; out Used, Made: NativeInt): Integer; override;
    procedure EndDeflate; override;
  end;

{$IFDEF ZLIB_USE_DYNLIB}
  { zlib shared library.}
  TDynLibZStream = class(TZBackendStream)
  private
    FStream: ZLibDynLib.z_stream;
  public
    function BeginInflate: Integer; override;
    function RunInflate(Input: Pointer; InputSize: NativeInt; Output: Pointer;
                     OutputSize: NativeInt; out Used, Made: NativeInt): Integer; override;
    procedure EndInflate; override;
    function BeginDeflate(Level, Strategy: Integer): Integer; override;
    function RunDeflate(Input: Pointer; InputSize: NativeInt; Output: Pointer;
                     OutputSize: NativeInt; Flush: Integer; out Used, Made: NativeInt): Integer; override;
    procedure EndDeflate; override;
  end;
{$ENDIF}

{ TStaticZStream }

function TStaticZStream.BeginInflate: Integer;
begin
{$IFDEF USE_PASCAL_ALLOC}
  FStream.zalloc := @zlibAllocMem;
  FStream.zfree := @zlibFreeMem;
{$ENDIF}
  Result := inflateInit_(FStream, MinZLibVersion, SizeOf(FStream));
end;

function TStaticZStream.RunInflate(Input: Pointer; InputSize: NativeInt; Output: Pointer;
  OutputSize: NativeInt; out Used, Made: NativeInt): Integer;
begin
  FStream.next_in := Input;
  FStream.avail_in := InputSize;
  FStream.next_out := Output;
  FStream.avail_out := OutputSize;
  Result := inflate(FStream, Z_NO_FLUSH);
  Used := InputSize - NativeInt(FStream.avail_in);
  Made := OutputSize - NativeInt(FStream.avail_out);
end;

procedure TStaticZStream.EndInflate;
begin
  inflateEnd(FStream);
end;

function TStaticZStream.BeginDeflate(Level, Strategy: Integer): Integer;
begin
{$IFDEF USE_PASCAL_ALLOC}
  FStream.zalloc := @zlibAllocMem;
  FStream.zfree := @zlibFreeMem;
{$ENDIF}
  Result := deflateInit2_(FStream, Level, Z_DEFLATED, MAX_WBITS, DEF_MEM_LEVEL,
                          Strategy, MinZLibVersion, SizeOf(FStream));
end;

function TStaticZStream.RunDeflate(Input: Pointer; InputSize: NativeInt; Output: Pointer;
  OutputSize: NativeInt; Flush: Integer; out Used, Made: NativeInt): Integer;
begin
  FStream.next_in := Input;
  FStream.avail_in := InputSize;
  FStream.next_out := Output;
  FStream.avail_out := OutputSize;
  Result := deflate(FStream, Flush);
  Used := InputSize - NativeInt(FStream.avail_in);
  Made := OutputSize - NativeInt(FStream.avail_out);
end;

procedure TStaticZStream.EndDeflate;
begin
  deflateEnd(FStream);
end;

{$IFDEF ZLIB_USE_DYNLIB}

{ TDynLibZStream }

function TDynLibZStream.BeginInflate: Integer;
begin
  FStream.zalloc := @zlibAllocMem;
  FStream.zfree := @zlibFreeMem;
  Result := ZLibDynLib.inflateInit_(FStream, MinZLibVersion, SizeOf(FStream));
end;

function TDynLibZStream.RunInflate(Input: Pointer; InputSize: NativeInt; Output: Pointer;
  OutputSize: NativeInt; out Used, Made: NativeInt): Integer;
begin
  FStream.next_in := Input;
  FStream.avail_in := InputSize;
  FStream.next_out := Output;
  FStream.avail_out := OutputSize;
  Result := ZLibDynLib.inflate(FStream, Z_NO_FLUSH);
  Used := InputSize - NativeInt(FStream.avail_in);
  Made := OutputSize - NativeInt(FStream.avail_out);
end;

procedure TDynLibZStream.EndInflate;
begin
  ZLibDynLib.inflateEnd(FStream);
end;

function TDynLibZStream.BeginDeflate(Level, Strategy: Integer): Integer;
begin
  FStream.zalloc := @zlibAllocMem;
  FStream.zfree := @zlibFreeMem;
  Result := ZLibDynLib.deflateInit2_(FStream, Level, Z_DEFLATED, MAX_WBITS, DEF_MEM_LEVEL,
                                     Strategy, MinZLibVersion, SizeOf(FStream));
end;

function TDynLibZStream.RunDeflate(Input: Pointer; InputSize: NativeInt; Output: Pointer;
  OutputSize: NativeInt; Flush: Integer; out Used, Made: NativeInt): Integer;
begin
  FStream.next_in := Input;
  FStream.avail_in := InputSize;
  FStream.next_out := Output;
  FStream.avail_out := OutputSize;
  Result := ZLibDynLib.deflate(FStream, Flush);
  Used := InputSize - NativeInt(FStream.avail_in);
  Made := OutputSize - NativeInt(FStream.avail_out);
end;

procedure TDynLibZStream.EndDeflate;
begin
  ZLibDynLib.deflateEnd(FStream);
end;

{$ENDIF}

{ Streaming interface }

{ Creates a stream of the backend in use: the shared library when it is loaded,
  otherwise the compiled-in one.}
function CreateBackendStream: TZBackendStream;
begin
{$IFDEF ZLIB_USE_DYNLIB}
  if IsZLibLibraryLoaded then
  begin
    Result := TDynLibZStream.Create;
    Exit;
  end;
{$ENDIF}
  Result := TStaticZStream.Create;
end;

function MinSize(A, B: NativeInt): NativeInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

procedure ZInflateBegin(var Inflater: TZInflater);
var
  Backend: TZBackendStream;
begin
  FillChar(Inflater, SizeOf(Inflater), 0);
  Backend := CreateBackendStream;
  try
    DCheck(Backend.BeginInflate);
  except
    Backend.Free;
    raise;
  end;
  Inflater.Backend := Backend;
  Inflater.Active := True;
end;

function ZInflateData(var Inflater: TZInflater; Input: Pointer; InputSize: NativeInt;
  Output: Pointer; OutputSize: NativeInt; out Consumed, Produced: NativeInt): Boolean;
var
  Backend: TZBackendStream;
  InBlock, OutBlock, Used, Made: NativeInt;
  Code: Integer;
  NoOutput: Byte;
begin
  Consumed := 0;
  Produced := 0;
  if Inflater.Finished then
  begin
    Result := False;
    Exit;
  end;
  // zlib needs valid output pointer even when there is no room in it
  if Output = nil then
    Output := @NoOutput;
  Assert(Inflater.Backend <> nil, 'ZInflateBegin not called');
  Backend := TZBackendStream(Inflater.Backend);

  // Input and output are passed in blocks, zlib counts are 32-bit. inflate is
  // called at least once, the end of stream (checksum) can be read also when
  // the output is full.
  repeat
    InBlock := MinSize(InputSize - Consumed, MaxZLibBlock);
    OutBlock := MinSize(OutputSize - Produced, MaxZLibBlock);
    Code := Backend.RunInflate(@PBuffer(Input)[Consumed], InBlock,
                            @PBuffer(Output)[Produced], OutBlock, Used, Made);
    Inc(Consumed, Used);
    Inc(Produced, Made);

    if Code = Z_STREAM_END then
    begin
      Inflater.Finished := True;
      Break;
    end
    else if Code = Z_BUF_ERROR then
      Break // No progress possible: more input or output space is needed
    else
      DCheck(Code);

    if (Used = 0) and (Made = 0) then
      Break;
  until Produced >= OutputSize;

  Result := not Inflater.Finished;
end;

procedure ZInflateEnd(var Inflater: TZInflater);
begin
  if Inflater.Active then
  begin
    Inflater.Active := False;
    TZBackendStream(Inflater.Backend).EndInflate;
    FreeAndNil(Inflater.Backend);
  end;
end;

procedure ZDeflateBegin(var Deflater: TZDeflater; Level, Strategy: Integer);
var
  Backend: TZBackendStream;
begin
  FillChar(Deflater, SizeOf(Deflater), 0);
  Backend := CreateBackendStream;
  try
    CCheck(Backend.BeginDeflate(Level, Strategy));
  except
    Backend.Free;
    raise;
  end;
  Deflater.Backend := Backend;
  Deflater.Active := True;
end;

function ZDeflateData(var Deflater: TZDeflater; Input: Pointer; InputSize: NativeInt;
  Output: Pointer; OutputSize: NativeInt; Finish: Boolean; out Consumed, Produced: NativeInt): Boolean;
var
  Backend: TZBackendStream;
  InBlock, OutBlock, Used, Made: NativeInt;
  Flush, Code: Integer;
  NoOutput: Byte;
begin
  Consumed := 0;
  Produced := 0;
  // zlib needs valid output pointer even when there is no room in it
  if Output = nil then
    Output := @NoOutput;
  Assert(Deflater.Backend <> nil, 'ZDeflateBegin not called');
  Backend := TZBackendStream(Deflater.Backend);

  // Input and output are passed in blocks, zlib counts are 32-bit
  while not Deflater.Finished and (Produced < OutputSize) do
  begin
    InBlock := MinSize(InputSize - Consumed, MaxZLibBlock);
    OutBlock := MinSize(OutputSize - Produced, MaxZLibBlock);
    // Stream can be finished only together with the last block of input
    if Finish and (Consumed + InBlock = InputSize) then
      Flush := Z_FINISH
    else
      Flush := Z_NO_FLUSH;

    Code := Backend.RunDeflate(@PBuffer(Input)[Consumed], InBlock,
                            @PBuffer(Output)[Produced], OutBlock, Flush, Used, Made);
    Inc(Consumed, Used);
    Inc(Produced, Made);

    if Code = Z_STREAM_END then
      Deflater.Finished := True
    else if Code = Z_BUF_ERROR then
      Break // No progress possible: input is used and nothing more to write now
    else
      CCheck(Code);

    if (Used = 0) and (Made = 0) then
      Break;
    // Without Finish only the given input is processed
    if not Finish and (Consumed = InputSize) and (Made < OutBlock) then
      Break;
  end;

  Result := not Deflater.Finished;
end;

procedure ZDeflateEnd(var Deflater: TZDeflater);
begin
  if Deflater.Active then
  begin
    Deflater.Active := False;
    TZBackendStream(Deflater.Backend).EndDeflate;
    FreeAndNil(Deflater.Backend);
  end;
end;

{ Buffer functions }

procedure CompressBuf(const InBuf: Pointer; InBytes: NativeInt;
  var OutBuf: Pointer; var OutBytes: NativeInt;
  CompressLevel, CompressStrategy: Integer);
var
  Deflater: TZDeflater;
  InPos, OutPos, Consumed, Produced: NativeInt;
begin
  // Estimates ~1.1 * InBytes as initial compressed size, grows the buffer when
  // the data are incompressible
  OutBytes := ((InBytes + (InBytes div 10) + 12) + 255) and not 255;
  GetMem(OutBuf, OutBytes);
  InPos := 0;
  OutPos := 0;

  try
    ZDeflateBegin(Deflater, CompressLevel, CompressStrategy);
    try
      while ZDeflateData(Deflater, @PBuffer(InBuf)[InPos], InBytes - InPos,
                        @PBuffer(OutBuf)[OutPos], OutBytes - OutPos, True, Consumed, Produced) do
      begin
        Inc(InPos, Consumed);
        Inc(OutPos, Produced);
        if OutPos = OutBytes then
        begin
          OutBytes := OutBytes + OutBytes div 4 + 256;
          ReallocMem(OutBuf, OutBytes);
        end;
      end;
      Inc(OutPos, Produced);
    finally
      ZDeflateEnd(Deflater);
    end;

    ReallocMem(OutBuf, OutPos);
    OutBytes := OutPos;
  except
    FreeMem(OutBuf);
    OutBuf := nil;
    raise
  end;
end;

procedure DecompressBuf(const InBuf: Pointer; InBytes: NativeInt;
  OutEstimate: NativeInt; var OutBuf: Pointer; var OutBytes: NativeInt);
var
  Inflater: TZInflater;
  InPos, OutPos, Consumed, Produced: NativeInt;
begin
  // Use the estimate if possible to avoid the reallocation loop
  if OutEstimate > 0 then
    OutBytes := OutEstimate
  else
    OutBytes := (InBytes + 255) and not 255;
  GetMem(OutBuf, OutBytes);
  InPos := 0;
  OutPos := 0;

  try
    ZInflateBegin(Inflater);
    try
      while ZInflateData(Inflater, @PBuffer(InBuf)[InPos], InBytes - InPos,
                        @PBuffer(OutBuf)[OutPos], OutBytes - OutPos, Consumed, Produced) do
      begin
        Inc(InPos, Consumed);
        Inc(OutPos, Produced);
        if OutPos = OutBytes then
        begin
          OutBytes := OutBytes + OutBytes div 2 + 256;
          ReallocMem(OutBuf, OutBytes);
        end
        else if (Consumed = 0) and (Produced = 0) then
          raise EDecompressionError.Create('zlib: unexpected end of data');
      end;
      Inc(OutPos, Produced);
    finally
      ZInflateEnd(Inflater);
    end;

    if OutPos <> OutBytes then
    begin
      ReallocMem(OutBuf, OutPos);
      OutBytes := OutPos;
    end;
  except
    FreeMem(OutBuf);
    OutBuf := nil;
    raise
  end;
end;

function ZLibBackend: TZLibBackend;
begin
{$IFDEF ZLIB_USE_DYNLIB}
  if IsZLibLibraryLoaded then
  begin
    Result := zbDynLib;
    Exit;
  end;
{$ENDIF}

{$IF Defined(ZLIB_USE_BUNDLED)}
  Result := zbBundled;
{$ELSEIF Defined(ZLIB_USE_FPC)}
  Result := zbFpc;
{$ELSE}
  Result := zbDelphi;
{$IFEND}
end;

function GetZLibVersion: string;
begin
{$IFDEF ZLIB_USE_DYNLIB}
  if IsZLibLibraryLoaded then
  begin
    Result := string(AnsiString(ZLibDynLib.zlibVersion));
    Exit;
  end;
{$ENDIF}

{$IF Defined(ZLIB_USE_FPC)}
  Result := string(paszlib.zlibVersion);
{$ELSE}
  Result := string(AnsiString(zlibVersion));
{$IFEND}
end;

function TryLoadZLibLibrary(const FileName: string): Boolean;
begin
{$IFDEF ZLIB_USE_DYNLIB}
  Result := LoadZLibLibraryFromFile(FileName);
{$ELSE}
  Result := False;
{$ENDIF}
end;

{ TCustomZLibStream }

constructor TCustomZLibStream.Create(Strm: TStream);
begin
  inherited Create;
  FStrm := Strm;
  FStrmPos := Strm.Position;
end;

procedure TCustomZLibStream.Progress(Sender: TObject);
begin
  if Assigned(FOnProgress) then FOnProgress(Sender);
end;

{ TCompressionStream }

constructor TCompressionStream.Create(CompressionLevel: TCompressionLevel;
  Dest: TStream);
const
  Levels: array [TCompressionLevel] of ShortInt =
    (Z_NO_COMPRESSION, Z_BEST_SPEED, Z_DEFAULT_COMPRESSION, Z_BEST_COMPRESSION);
begin
  inherited Create(Dest);
  // Strategy 0 is Z_DEFAULT_STRATEGY
  ZDeflateBegin(FDeflater, Levels[CompressionLevel], 0);
end;

destructor TCompressionStream.Destroy;
var
  Consumed, Produced: NativeInt;
begin
  try
    if FDeflater.Active then
    begin
      // Write the rest of the compressed stream
      if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
      while ZDeflateData(FDeflater, nil, 0, @FBuffer[FOutLen], SizeOf(FBuffer) - FOutLen,
                         True, Consumed, Produced) do
      begin
        Inc(FOutLen, Produced);
        if FOutLen < SizeOf(FBuffer) then
          raise ECompressionError.Create('zlib: no progress');
        FlushBuffer;
      end;
      Inc(FOutLen, Produced);
      if FOutLen > 0 then
        FlushBuffer;
    end;
  finally
    ZDeflateEnd(FDeflater);
  end;
  inherited Destroy;
end;

procedure TCompressionStream.FlushBuffer;
begin
  FStrm.WriteBuffer(FBuffer, FOutLen);
  Inc(FTotalOut, FOutLen);
  FOutLen := 0;
  FStrmPos := FStrm.Position;
  Progress(Self);
end;

function TCompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  // FPC warns about unset result, Delphi hints that the value is never used
  {$IFDEF FPC}Result := 0;{$ENDIF}
  raise ECompressionError.Create('Invalid stream operation');
end;

function TCompressionStream.Write(const Buffer; Count: Longint): Longint;
var
  Done, Consumed, Produced: NativeInt;
begin
  if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
  Done := 0;
  while Done < Count do
  begin
    ZDeflateData(FDeflater, @PBuffer(@Buffer)[Done], Count - Done, @FBuffer[FOutLen],
                 SizeOf(FBuffer) - FOutLen, False, Consumed, Produced);
    Inc(Done, Consumed);
    Inc(FOutLen, Produced);
    // Compressed data are written out when the buffer is full
    if FOutLen = SizeOf(FBuffer) then
      FlushBuffer
    else if (Consumed = 0) and (Produced = 0) then
      raise ECompressionError.Create('zlib: no progress');
  end;
  Inc(FTotalIn, Count);
  Result := Count;
end;

function TCompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  // Only the position (number of bytes written) can be read
  if (Offset = 0) and (Origin = soCurrent) then
    Result := FTotalIn
  else
    raise ECompressionError.Create('Invalid stream operation');
end;

function TCompressionStream.GetCompressionRate: Single;
begin
  if FTotalIn = 0 then
    Result := 0
  else
    Result := (1.0 - ((FTotalOut + FOutLen) / FTotalIn)) * 100.0;
end;

{ TDecompressionStream }

constructor TDecompressionStream.Create(Source: TStream);
begin
  inherited Create(Source);
  FStrmStart := FStrmPos;
  ZInflateBegin(FInflater);
end;

destructor TDecompressionStream.Destroy;
begin
  ZInflateEnd(FInflater);
  inherited Destroy;
end;

function TDecompressionStream.Read(var Buffer; Count: Longint): Longint;
var
  Done, Consumed, Produced: NativeInt;
begin
  if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
  Done := 0;

  // Reads until Count bytes are decompressed, the end of zlib stream (data after
  // it are not used) or the end of source stream
  while (Done < Count) and not FInflater.Finished do
  begin
    if FBufPos = FBufLen then
    begin
      FBufLen := FStrm.Read(FBuffer, SizeOf(FBuffer));
      FBufPos := 0;
      FStrmPos := FStrm.Position;
      if FBufLen <= 0 then
      begin
        FBufLen := 0;
        Break;
      end;
      Progress(Self);
    end;

    ZInflateData(FInflater, @FBuffer[FBufPos], FBufLen - FBufPos, @PBuffer(@Buffer)[Done],
                 Count - Done, Consumed, Produced);
    Inc(FBufPos, Consumed);
    Inc(Done, Produced);
    if (Consumed = 0) and (Produced = 0) and not FInflater.Finished and (FBufPos < FBufLen) then
      raise EDecompressionError.Create('zlib: no progress');
  end;

  Inc(FTotalOut, Done);
  Result := Done;
end;

function TDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  // FPC warns about unset result, Delphi hints that the value is never used
  {$IFDEF FPC}Result := 0;{$ENDIF}
  raise EDecompressionError.Create('Invalid stream operation');
end;

function TDecompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  Skip: Int64;
  Len: Integer;
  Buf: array [0..4095] of Byte;
begin
  if (Offset = 0) and (Origin = soBeginning) then
  begin
    // Start decompression again from the beginning of compressed data
    ZInflateEnd(FInflater);
    ZInflateBegin(FInflater);
    FBufPos := 0;
    FBufLen := 0;
    FStrm.Position := FStrmStart;
    FStrmPos := FStrmStart;
    FTotalOut := 0;
  end
  else if ((Offset >= 0) and (Origin = soCurrent)) or
          ((Offset >= FTotalOut) and (Origin = soBeginning)) then
  begin
    // Forward seek decompresses and skips data
    Skip := Offset;
    if Origin = soBeginning then
      Skip := Offset - FTotalOut;
    while Skip > 0 do
    begin
      Len := SizeOf(Buf);
      if Len > Skip then
        Len := Skip;
      ReadBuffer(Buf, Len);
      Dec(Skip, Len);
    end;
  end
  else
    raise EDecompressionError.Create('Invalid stream operation');

  Result := FTotalOut;
end;

end.
