{ Bindings for zlib shared library (zlib1.dll, libz.so.1, libz.1.dylib; zlib-ng
  in compat mode works too) loaded at run time. Only what ImagingZLib needs
  is bound. The library is looked for in the default locations when the unit
  is initialized, a missing library is not an error: check
  IsZLibLibraryLoaded. }
unit ZLibDynLib;

{$I ImagingOptions.inc}

{$IFNDEF HAS_DYNLIBS}
  {$MESSAGE FATAL 'dynamic libraries are not supported on this target'}
{$ENDIF}

interface

uses
  SysUtils;

const
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
{$ENDIF}

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
  deflate: function(var strm: z_stream; flush: Int): Int; cdecl;
  deflateEnd: function(var strm: z_stream): Int; cdecl;
  inflateInit_: function(var strm: z_stream; const version: PAnsiChar; stream_size: Int): Int; cdecl;
  inflate: function(var strm: z_stream; flush: Int): Int; cdecl;
  inflateEnd: function(var strm: z_stream): Int; cdecl;
  deflateInit2_: function(var strm: z_stream; level, method, windowBits,
    memLevel, strategy: Int; const version: PAnsiChar; stream_size: Int): Int; cdecl;

{ Looks for zlib library in the default locations (unless it is already
  loaded) and returns True if it is loaded.}
function LoadZLibLibrary: Boolean;
{ Loads zlib library from the given file (replaces already loaded library).}
function LoadZLibLibraryFromFile(const FileName: string): Boolean;
{ Returns True if zlib library is currently loaded.}
function IsZLibLibraryLoaded: Boolean;

implementation

{$IFDEF FPC}
uses dynlibs;
{$ELSE}
  {$IFDEF MSWINDOWS}
  uses Windows;
  {$ENDIF}
{$ENDIF}

const
{$IF Defined(MSWINDOWS)}
  LibNames: array[0..1] of string = ('zlib1.dll', 'zlib.dll');
{$ELSEIF Defined(DARWIN) or Defined(MACOS)}
  LibNames: array[0..3] of string = ('libz.1.dylib', 'libz.dylib',
    '@executable_path/libz.1.dylib', '@executable_path/libz.dylib');
{$ELSE}  // Linux, BSD
  LibNames: array[0..1] of string = ('libz.so.1', 'libz.so');
{$IFEND}

var
  LibHandle: {$IFDEF FPC}TLibHandle{$ELSE}THandle{$ENDIF} = 0;

procedure UnloadLib;
begin
  if LibHandle <> 0 then
  begin
    FreeLibrary(LibHandle);
    LibHandle := 0;
  end;

  zlibVersion := nil;
  deflate := nil;
  deflateEnd := nil;
  inflateInit_ := nil;
  inflate := nil;
  inflateEnd := nil;
  deflateInit2_ := nil;
end;

function TryLoad(const FileName: string): Boolean;

  function GetProc(const ProcName: string): Pointer;
  begin
    Result := GetProcAddress(LibHandle, PChar(ProcName));
  end;

begin
  Result := False;
  LibHandle := LoadLibrary(PChar(FileName));
  if LibHandle = 0 then
    Exit;

  zlibVersion := GetProc('zlibVersion');
  deflate := GetProc('deflate');
  deflateEnd := GetProc('deflateEnd');
  inflateInit_ := GetProc('inflateInit_');
  inflate := GetProc('inflate');
  inflateEnd := GetProc('inflateEnd');
  deflateInit2_ := GetProc('deflateInit2_');

  Result := Assigned(zlibVersion) and Assigned(deflate) and Assigned(deflateEnd) and
            Assigned(inflateInit_) and Assigned(inflate) and Assigned(inflateEnd) and
            Assigned(deflateInit2_);

  if not Result then
    UnloadLib;
end;

function LoadZLibLibrary: Boolean;
var
  I: Integer;
begin
  if LibHandle = 0 then
  begin
    for I := Low(LibNames) to High(LibNames) do
    begin
      if TryLoad(LibNames[I]) then
        Break;
    end;
  end;

  Result := LibHandle <> 0;
end;

function LoadZLibLibraryFromFile(const FileName: string): Boolean;
begin
  UnloadLib;
  Result := TryLoad(FileName);
end;

function IsZLibLibraryLoaded: Boolean;
begin
  Result := LibHandle <> 0;
end;

initialization
  // The library is optional, nothing happens when it is not found
  LoadZLibLibrary;
finalization
  UnloadLib;
end.
