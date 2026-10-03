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
  http://imaginglib.sourceforge.net

  You can choose which pascal zlib implementation will be
  used. IMPASZLIB and FPCPASZLIB are translations of zlib
  to pascal so they don't need any *.obj files.
  The others are interfaces to *.obj files (Windows) or
  *.so libraries (Linux).
    Default implementation is IMPASZLIB because it can be compiled
  by all supported compilers and works on all supported platforms.
    FPCPASZLIB is useful for Lazarus applications. FPC's zlib is linked
  to exe by default so there is no need to link additional (and almost identical) IMPASZLIB.
}

unit ImagingZLib;

{$I ImagingOptions.inc}

interface

{ $DEFINE IMPASZLIB}
{ $DEFINE ZLIB_DYNLIB}

{ $DEFINE FPCPASZLIB}
{ $DEFINE DELPHIZLIB}

{$IF not Defined(IMPASZLIB) and not Defined(ZLIB_DYNLIB)}
  {$DEFINE NO_USER_ZLIB}
{$IFEND}


{$IF Defined(FPC) and Defined(NO_USER_ZLIB)}
  { Automatically use FPC's PasZLib when compiling with FPC.}
  {$DEFINE FPCPASZLIB}
{$IFEND}

{$IF Defined(DELPHI) and (CompilerVersion > 23) and Defined(NO_USER_ZLIB)}
  { Automatically use Delphi's ZLib when compiling with Delphi (recent versions include
    recentish ZLib compiled to objects, can be 2x faster than IMPASZLIB).
    Needs Delphi XE2+ for deflateInit2 etc.}
  {$DEFINE DELPHIZLIB}
{$IFEND}

{$IF not Defined(FPCPASZLIB) and not Defined(DELPHIZLIB) and not Defined(ZLIB_DYNLIB)}
  {$DEFINE IMPASZLIB}  // Fallback
{$IFEND}

uses
{$IF Defined(IMPASZLIB)}
  { Use paszlib modified by me for Delphi and FPC. Fall-back solution. }
  ImPasZLib,
{$ELSEIF Defined(FPCPASZLIB)}
  { Use FPC's paszlib }
  zbase, paszlib,
{$ELSEIF Defined(DELPHIZLIB)}
  { Use ZLib unit shipped with Delphi }
  ZLib,
{$ELSEIF Defined(ZLIB_DYNLIB)}
  { Use zlib dll/so/dylib. Can also use zlib-ng compat. }
  ZLibDynLib,
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
  procedure CompressBuf(const InBuf: Pointer; InBytes: Integer;
    var OutBuf: Pointer; var OutBytes: Integer;
    CompressLevel: Integer = Z_DEFAULT_COMPRESSION;
    CompressStrategy: Integer = Z_DEFAULT_STRATEGY);

  { DecompressBuf decompresses data, buffer to buffer, in one call.
     In: InBuf = ptr to compressed data
         InBytes = number of bytes in InBuf
         OutEstimate = zero, or est. size of the decompressed data
    Out: OutBuf = ptr to newly allocated buffer containing decompressed data
         OutBytes = number of bytes in OutBuf   }
  procedure DecompressBuf(const InBuf: Pointer; InBytes: Integer;
    OutEstimate: Integer; var OutBuf: Pointer; var OutBytes: Integer);

type
  { Abstract ancestor class }
  TCustomZLibStream = class(TStream)
  private
    FStrm: TStream;
    FStrmPos: Integer;
    FOnProgress: TNotifyEvent;
    FZRec: TZStreamRec;
    FBuffer: array [Word] of Byte;
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
    function GetCompressionRate: Single;
  public
    constructor Create(CompressionLevel: TCompressionLevel; Dest: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
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
  public
    constructor Create(Source: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property OnProgress;
  end;

type
  EZlibError = class(Exception);
  ECompressionError = class(EZlibError);
  EDecompressionError = class(EZlibError);

implementation

const
  // Version string used with the API calls. We use just very basic
  // parts supported since forever.
  MinZLibVersion = '1.1.2';

{$IF not Defined(IMPASZLIB) and not Defined(FPCPASZLIB)}
  // Use Pascal mem allocation when using compiled C libs
  {$DEFINE USE_PASCAL_ALLOC}
{$IFEND}

{$IF Defined(DELPHIZLIB) or Defined(ZLIB_DYNLIB)}
const
  DEF_MEM_LEVEL = 9;
  MAX_WBITS = 15;
{$IFEND}

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

procedure CompressBuf(const InBuf: Pointer; InBytes: Integer;
  var OutBuf: Pointer; var OutBytes: Integer;
  CompressLevel, CompressStrategy: Integer);
var
  ZStream: TZStreamRec;
begin
  FillChar(ZStream, sizeof(ZStream), 0);
{$IFDEF USE_PASCAL_ALLOC}
  ZStream.zalloc := @zlibAllocMem;
  ZStream.zfree := @zlibFreeMem;
{$ENDIF}
  // Estimates ~1.1 * InBytes as initial compressed size: usually much slower,
  // if incompressible the deflate() loop will increase the buffer size incrementaly.
  // With newer ZLib we could use deflateBound() to get rid of the loop.

  OutBytes := ((InBytes + (InBytes div 10) + 12) + 255) and not 255;
  GetMem(OutBuf, OutBytes);

  try
    ZStream.next_in := InBuf;
    ZStream.avail_in := InBytes;
    ZStream.next_out := OutBuf;
    ZStream.avail_out := OutBytes;

    CCheck(deflateInit2_(ZStream, CompressLevel, Z_DEFLATED, MAX_WBITS,
      DEF_MEM_LEVEL, CompressStrategy, MinZLibVersion, SizeOf(z_stream)));

    try
      while CCheck(deflate(ZStream, Z_FINISH)) <> Z_STREAM_END do
      begin
        Inc(OutBytes, 256);
        ReallocMem(OutBuf, OutBytes);
        ZStream.next_out := Pointer(PtrUInt(OutBuf) + ZStream.total_out);
        ZStream.avail_out := 256;
      end;
    finally
      CCheck(deflateEnd(ZStream));
    end;
    ReallocMem(OutBuf, ZStream.total_out);
    OutBytes := ZStream.total_out;
  except
    zlibFreeMem(nil, OutBuf);
    raise
  end;
end;

procedure DecompressBuf(const InBuf: Pointer; InBytes: Integer;
  OutEstimate: Integer; var OutBuf: Pointer; var OutBytes: Integer);
var
  ZStream: TZStreamRec;
  BufInc: Integer;
begin
  FillChar(ZStream, sizeof(ZStream), 0);
{$IFDEF USE_PASCAL_ALLOC}
  ZStream.zalloc := @zlibAllocMem;
  ZStream.zfree := @zlibFreeMem;
{$ENDIF}
  BufInc := (InBytes + 255) and not 255;

  // Use the estimate if possible to avoid the reallocation loop.
  if OutEstimate = 0 then
    OutBytes := BufInc
  else
    OutBytes := OutEstimate;

  GetMem(OutBuf, OutBytes);
  try
    ZStream.next_in := InBuf;
    ZStream.avail_in := InBytes;
    ZStream.next_out := OutBuf;
    ZStream.avail_out := OutBytes;

    DCheck(inflateInit_(ZStream, MinZLibVersion, sizeof(ZStream)));
    try
      while DCheck(inflate(ZStream, Z_NO_FLUSH)) <> Z_STREAM_END do
      begin
        Inc(OutBytes, BufInc);
        ReallocMem(OutBuf, OutBytes);
        ZStream.next_out := Pointer(PtrUInt(OutBuf) + ZStream.total_out);
        ZStream.avail_out := BufInc;
      end;
    finally
      DCheck(inflateEnd(ZStream));
    end;

    if OutBytes <> ZStream.total_out then
    begin
      ReallocMem(OutBuf, ZStream.total_out);
      OutBytes := ZStream.total_out;
    end;
  except
    zlibFreeMem(nil, OutBuf);
    raise
  end;
end;

{ TCustomZLibStream }

constructor TCustomZLibStream.Create(Strm: TStream);
begin
  inherited Create;
  FStrm := Strm;
  FStrmPos := Strm.Position;
{$IFDEF USE_PASCAL_ALLOC}
  FZRec.zalloc := @zlibAllocMem;
  FZRec.zfree := @zlibFreeMem;
{$ENDIF}
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
  FZRec.next_out := @FBuffer;
  FZRec.avail_out := sizeof(FBuffer);
  CCheck(deflateInit_(FZRec, Levels[CompressionLevel], MinZLibVersion, sizeof(FZRec)));
end;

destructor TCompressionStream.Destroy;
begin
  FZRec.next_in := nil;
  FZRec.avail_in := 0;
  try
    if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
    while (CCheck(deflate(FZRec, Z_FINISH)) <> Z_STREAM_END)
      and (FZRec.avail_out = 0) do
    begin
      FStrm.WriteBuffer(FBuffer, sizeof(FBuffer));
      FZRec.next_out := @FBuffer;
      FZRec.avail_out := sizeof(FBuffer);
    end;
    if FZRec.avail_out < sizeof(FBuffer) then
      FStrm.WriteBuffer(FBuffer, sizeof(FBuffer) - FZRec.avail_out);
  finally
    deflateEnd(FZRec);
  end;
  inherited Destroy;
end;

function TCompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
  raise ECompressionError.Create('Invalid stream operation');
end;

function TCompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  FZRec.next_in := @Buffer;
  FZRec.avail_in := Count;
  if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
  while (FZRec.avail_in > 0) do
  begin
    CCheck(deflate(FZRec, 0));
    if FZRec.avail_out = 0 then
    begin
      FStrm.WriteBuffer(FBuffer, sizeof(FBuffer));
      FZRec.next_out := @FBuffer;
      FZRec.avail_out := sizeof(FBuffer);
      FStrmPos := FStrm.Position;
      Progress(Self);
    end;
  end;
  Result := Count;
end;

function TCompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if (Offset = 0) and (Origin = soFromCurrent) then
    Result := FZRec.total_in
  else
    raise ECompressionError.Create('Invalid stream operation');
end;

function TCompressionStream.GetCompressionRate: Single;
begin
  if FZRec.total_in = 0 then
    Result := 0
  else
    Result := (1.0 - (FZRec.total_out / FZRec.total_in)) * 100.0;
end;

{ TDecompressionStream }

constructor TDecompressionStream.Create(Source: TStream);
begin
  inherited Create(Source);
  FZRec.next_in := @FBuffer;
  FZRec.avail_in := 0;
  DCheck(inflateInit_(FZRec, MinZLibVersion, sizeof(FZRec)));
end;

destructor TDecompressionStream.Destroy;
begin
  inflateEnd(FZRec);
  inherited Destroy;
end;

function TDecompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  FZRec.next_out := @Buffer;
  FZRec.avail_out := Count;
  if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
  while (FZRec.avail_out > 0) do
  begin
    if FZRec.avail_in = 0 then
    begin
      FZRec.avail_in := FStrm.Read(FBuffer, sizeof(FBuffer));
      if FZRec.avail_in = 0 then
        begin
          Result := Count - Integer(FZRec.avail_out);
          Exit;
        end;
      FZRec.next_in := @FBuffer;
      FStrmPos := FStrm.Position;
      Progress(Self);
    end;
    CCheck(inflate(FZRec, 0));
  end;
  Result := Count;
end;

function TDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
  raise EDecompressionError.Create('Invalid stream operation');
end;

function TDecompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  I: Integer;
  Buf: array [0..4095] of Byte;
begin
  if (Offset = 0) and (Origin = soFromBeginning) then
  begin
    DCheck(inflateReset(FZRec));
    FZRec.next_in := @FBuffer;
    FZRec.avail_in := 0;
    FStrm.Position := 0;
    FStrmPos := 0;
  end
  else if ((Offset >= 0) and (Origin = soFromCurrent)) or
          (((Offset - Integer(FZRec.total_out)) > 0) and (Origin = soFromBeginning)) then
  begin
    if Origin = soFromBeginning then Dec(Offset, FZRec.total_out);
    if Offset > 0 then
    begin
      for I := 1 to Offset div sizeof(Buf) do
        ReadBuffer(Buf, sizeof(Buf));
      ReadBuffer(Buf, Offset mod sizeof(Buf));
    end;
  end
  else
    raise EDecompressionError.Create('Invalid stream operation');
  Result := FZRec.total_out;
end;

end.
