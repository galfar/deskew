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

{ Bindings for TurboJPEG API of libjpeg-turbo shared library (DLL/SO/dylib)
  loaded at run time. Only what is needed to decode and encode whole images
  in memory is bound. libjpeg-turbo 2.0 or newer is required, older versions
  are refused. Nothing is loaded automatically, call LoadLibTurboJpeg
  (or LoadLibTurboJpegFromFile) and check the result. }
unit LibTurboJpegDynLib;

{$I ImagingOptions.inc}

{$IFNDEF HAS_DYNLIBS}
  {$MESSAGE FATAL 'dynamic libraries are not supported on this target'}
{$ENDIF}

interface

uses
  ImagingTypes;

const
  { Chrominance subsampling }
  TJSAMP_444  = 0;
  TJSAMP_422  = 1;
  TJSAMP_420  = 2;
  TJSAMP_GRAY = 3;

  { Pixel formats }
  TJPF_RGB  = 0;
  TJPF_BGR  = 1;
  TJPF_GRAY = 6;
  TJPF_CMYK = 11;

  { JPEG color spaces }
  TJCS_RGB   = 0;
  TJCS_YCbCr = 1;
  TJCS_GRAY  = 2;
  TJCS_CMYK  = 3;
  TJCS_YCCK  = 4;

  { Flags }
  TJFLAG_FASTUPSAMPLE = 256;
  TJFLAG_ACCURATEDCT  = 4096;
  TJFLAG_PROGRESSIVE  = 16384;

  { Error codes returned by tjGetErrorCode }
  TJERR_WARNING = 0;
  TJERR_FATAL   = 1;

type
  TTJHandle = Pointer;
  { C unsigned long }
{$IFDEF MSWINDOWS}
  TTJULong = UInt32;
{$ELSE}
  TTJULong = PtrUInt;
{$ENDIF}
  PPByte = ^PByte;

var
  tjInitDecompress: function: TTJHandle; cdecl;
  tjInitCompress: function: TTJHandle; cdecl;
  { All functions returning Int32 return 0 on success and -1 on error
    (also when only a warning was issued, see tjGetErrorCode). }
  tjDecompressHeader3: function(Handle: TTJHandle; JpegBuf: PByte; JpegSize: TTJULong;
    var Width, Height, JpegSubsamp, JpegColorspace: Int32): Int32; cdecl;
  tjDecompress2: function(Handle: TTJHandle; JpegBuf: PByte; JpegSize: TTJULong;
    DstBuf: PByte; Width, Pitch, Height, PixelFormat, Flags: Int32): Int32; cdecl;
  { JPEG data is placed to buffer allocated by the library (when JpegBuf^ is nil),
    to be released by tjFree. }
  tjCompress2: function(Handle: TTJHandle; SrcBuf: PByte; Width, Pitch, Height,
    PixelFormat: Int32; JpegBuf: PPByte; var JpegSize: TTJULong;
    JpegSubsamp, JpegQual, Flags: Int32): Int32; cdecl;
  tjFree: procedure(Buffer: PByte); cdecl;
  tjDestroy: function(Handle: TTJHandle): Int32; cdecl;
  { These two are not present before libjpeg-turbo 2.0. }
  tjGetErrorStr2: function(Handle: TTJHandle): PAnsiChar; cdecl;
  tjGetErrorCode: function(Handle: TTJHandle): Int32; cdecl;

{ Tries to load TurboJPEG library using the usual library names for the current
  platform. Returns True if the library with all needed functions is loaded
  (also when it was already loaded before). The search is done only once. }
function LoadLibTurboJpeg: Boolean;
{ Loads TurboJPEG library from the given file (replaces already loaded library). }
function LoadLibTurboJpegFromFile(const FileName: string): Boolean;
{ Returns True if TurboJPEG library is currently loaded. }
function IsLibTurboJpegLoaded: Boolean;

implementation

uses
{$IFDEF FPC}
  dynlibs,
{$ELSE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
{$ENDIF}
  SysUtils;

const
{$IF Defined(MSWINDOWS)}
  LibNames: array[0..1] of string = ('turbojpeg.dll', 'libturbojpeg.dll');
{$ELSEIF Defined(DARWIN) or Defined(MACOS)}
  LibNames: array[0..3] of string = ('libturbojpeg.0.dylib', 'libturbojpeg.dylib',
    '@executable_path/libturbojpeg.0.dylib', '@executable_path/libturbojpeg.dylib');
{$ELSE}
  LibNames: array[0..1] of string = ('libturbojpeg.so.0', 'libturbojpeg.so');
{$IFEND}

var
  LibHandle: {$IFDEF FPC}TLibHandle{$ELSE}THandle{$ENDIF} = 0;
  SearchDone: Boolean = False;

procedure UnloadLib;
begin
  if LibHandle <> 0 then
  begin
    FreeLibrary(LibHandle);
    LibHandle := 0;
  end;

  tjInitDecompress := nil;
  tjInitCompress := nil;
  tjDecompressHeader3 := nil;
  tjDecompress2 := nil;
  tjCompress2 := nil;
  tjFree := nil;
  tjDestroy := nil;
  tjGetErrorStr2 := nil;
  tjGetErrorCode := nil;
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

  tjInitDecompress := GetProc('tjInitDecompress');
  tjInitCompress := GetProc('tjInitCompress');
  tjDecompressHeader3 := GetProc('tjDecompressHeader3');
  tjDecompress2 := GetProc('tjDecompress2');
  tjCompress2 := GetProc('tjCompress2');
  tjFree := GetProc('tjFree');
  tjDestroy := GetProc('tjDestroy');
  tjGetErrorStr2 := GetProc('tjGetErrorStr2');
  tjGetErrorCode := GetProc('tjGetErrorCode');

  // Missing error functions mean libjpeg-turbo 1.x which is not supported
  Result := Assigned(tjInitDecompress) and Assigned(tjInitCompress) and
            Assigned(tjDecompressHeader3) and Assigned(tjDecompress2) and
            Assigned(tjCompress2) and Assigned(tjFree) and Assigned(tjDestroy) and
            Assigned(tjGetErrorStr2) and Assigned(tjGetErrorCode);

  if not Result then
    UnloadLib;
end;

function LoadLibTurboJpeg: Boolean;
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

function LoadLibTurboJpegFromFile(const FileName: string): Boolean;
begin
  UnloadLib;
  SearchDone := True;
  Result := TryLoad(FileName);
end;

function IsLibTurboJpegLoaded: Boolean;
begin
  Result := LibHandle <> 0;
end;

initialization

finalization
  UnloadLib;

end.
