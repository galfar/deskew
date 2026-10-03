unit ZLibDynLib;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils;

const
  { Library names for different platforms }
{$IF Defined(MSWINDOWS)}
  SLibName = 'zlib1.dll';
{$ELSEIF Defined(DARWIN)}  // macOS
  SLibName = 'libz.1.dylib';
{$ELSE}  // Linux, BSD
  SLibName = 'libz.so.1';
{$IFEND}

  ZLIB_VERSION    = '1.2.3';

  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;
  Z_BLOCK         = 5;
  Z_TREES         = 6;

  Z_OK            =  0;
  Z_STREAM_END    =  1;
  Z_NEED_DICT     =  2;
  Z_ERRNO         = -1;
  Z_STREAM_ERROR  = -2;
  Z_DATA_ERROR    = -3;
  Z_MEM_ERROR     = -4;
  Z_BUF_ERROR     = -5;
  Z_VERSION_ERROR = -6;

  Z_NO_COMPRESSION       =  0;
  Z_BEST_SPEED           =  1;
  Z_BEST_COMPRESSION     =  9;
  Z_DEFAULT_COMPRESSION  = -1;

  Z_FILTERED            = 1;
  Z_HUFFMAN_ONLY        = 2;
  Z_RLE                 = 3;
  Z_FIXED               = 4;
  Z_DEFAULT_STRATEGY    = 0;

  Z_DEFLATED = 8;

type
{$IFDEF DCC}
  {$IF CompilerVersion <= 18.5}
    PtrUInt = Cardinal;
  {$ELSE}
    PtrUInt = NativeUInt;
  {$IFEND}
{$IFEND}

  z_size_t = PtrUInt;
  Int = Integer;
  uInt = Cardinal;
{$IFDEF MSWINDOWS}
  uLong = Cardinal;  // Windows: long is 32-bit even on Win64
{$ELSE}
  uLong = PtrUInt;   // Linux/Unix LP64: unsigned long is pointer-sized
                     // FPC: LongWord stays 32 bit
                     // Delphi: LongWord follows C, LongWord is 64 bit
{$ENDIF}

  alloc_func = function(opaque: Pointer; items, size: uInt): Pointer; cdecl;
  free_func = procedure(opaque: Pointer; address: Pointer); cdecl;

  // SizeOf(z_stream) must match between Pascal and DLL/SO,
  // data types must match between platforms, do not use packed record.
  // If not, expect some failures with Z_VERSION_ERROR result.
  z_stream = record
    next_in: PByte;         { next input byte }
    avail_in: uInt;         { number of bytes available at next_in }
    total_in: uLong;        { total number of input bytes read so far }

    next_out: PByte;        { next output byte will go here }
    avail_out: uInt;        { remaining free space at next_out }
    total_out: uLong;       { total number of bytes output so far }

    msg: PAnsiChar;         { last error message, NULL if no error }
    internal_state: Pointer; { not visible by applications }

    zalloc: alloc_func;     { used to allocate the internal state }
    zfree: free_func;       { used to free the internal state }
    opaque: Pointer;        { private data object passed to zalloc and zfree }

    data_type: Int;         { best guess about the data type: binary or text
                              for deflate, or the decoding state for inflate }
    adler: uLong;           { Adler-32 or CRC-32 value of the uncompressed data }
    reserved: uLong;        { reserved for future use }
  end;
  z_streamp = ^z_stream;

var
  { Just what we need from full ZLib API }
  zlibVersion: function: PAnsiChar; cdecl;
  deflateInit_: function(var strm: z_stream; level: Int;
    const version: PAnsiChar; stream_size: Int): Int; cdecl;
  deflate: function(var strm: z_stream; flush: Int): Int; cdecl;
  deflateEnd: function(var strm: z_stream): Int; cdecl;
  inflateInit_: function(var strm: z_stream; const version: PAnsiChar; stream_size: Int): Int; cdecl;
  inflate: function(var strm: z_stream; flush: Int): Int; cdecl;
  inflateEnd: function(var strm: z_stream): Int; cdecl;
  deflateInit2_: function(var strm: z_stream; level, method, windowBits,
    memLevel, strategy: Int; const version: PAnsiChar; stream_size: Int): Int; cdecl;
  deflateReset: function(var strm: z_stream): Int; cdecl;
  deflateBound: function(var strm: z_stream; sourceLen: uLong): uLong; cdecl;
  inflateInit2_: function(var strm: z_stream; windowBits: Int;
    const version: PAnsiChar; stream_size: Int): Int; cdecl;
  inflateReset: function(var strm: z_stream): Int; cdecl;
  compressBound: function(sourceLen: uLong): uLong; cdecl;

function LoadZLibLibrary: Boolean;
var LoadedZLibVersion: string;

implementation

{$IFDEF FPC}
uses dynlibs;
{$ELSE}
  {$IFDEF MSWINDOWS}
  uses Windows;
  {$ENDIF}
{$ENDIF}

var
  ZLibHandle: {$IFDEF FPC}TLibHandle{$ELSE}THandle{$ENDIF} = 0;

function GetZLibProc(const AProcName: PChar): Pointer;
begin
  Result := GetProcAddress(ZLibHandle, AProcName);
  if Result = nil then begin
    RaiseLastOSError;
  end;
end;

function LoadZLibLibrary: Boolean;
begin
  Result := False;

  if ZLibHandle = 0 then
  begin
    if ZLibHandle = 0 then
      ZLibHandle := LoadLibrary(SLibName);
  {$IF Defined(DARWIN)}
    if ZLibHandle = 0 then
      ZLibHandle := LoadLibrary('@executable_path/' + SLibName);
  {$IFEND}

    if ZLibHandle <> 0 then
    begin
      zlibVersion   := GetZLibProc('zlibVersion');
      deflateInit_  := GetZLibProc('deflateInit_');
      deflate       := GetZLibProc('deflate');
      deflateEnd    := GetZLibProc('deflateEnd');
      inflateInit_  := GetZLibProc('inflateInit_');
      inflate       := GetZLibProc('inflate');
      inflateEnd    := GetZLibProc('inflateEnd');
      deflateInit2_ := GetZLibProc('deflateInit2_');
      deflateReset  := GetZLibProc('deflateReset');
      deflateBound  := GetZLibProc('deflateBound');
      inflateInit2_ := GetZLibProc('inflateInit2_');
      inflateReset  := GetZLibProc('inflateReset');
      compressBound := GetZLibProc('compressBound');

      Result := Assigned(zlibVersion) and
        Assigned(deflateInit_) and Assigned(deflateInit2_) and
        Assigned(deflate) and Assigned(deflateEnd) and Assigned(inflateInit_) and
        Assigned(inflate) and Assigned(inflateEnd);

      LoadedZLibVersion := zlibVersion;
    end;
  end;
end;

procedure FreeZLibrary;
begin
  if ZLibHandle <> 0 then begin
    FreeLibrary(ZLibHandle);
    ZLibHandle := 0;
  end;
end;

initialization
  LoadZLibLibrary;
finalization
  FreeZLibrary;
end.
