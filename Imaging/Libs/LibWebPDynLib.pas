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

{ Bindings for libwebp shared library (DLL/SO/dylib) loaded at run time.
  Only the simple decoding and encoding API is bound. Nothing is loaded
  automatically, call LoadLibWebP (or LoadLibWebPFromFile) and check the result. }
unit LibWebPDynLib;

{$I ImagingOptions.inc}

{$IFNDEF HAS_DYNLIBS}
  {$MESSAGE FATAL 'dynamic libraries are not supported on this target'}
{$ENDIF}

interface

uses
  ImagingTypes;

const
  WEBP_DECODER_ABI_VERSION = $0209;

type
  TWebPSize = PtrUInt;  // size_t
  PPByte = ^PByte;

  { Features gathered from the bitstream by WebPGetFeatures. }
  TWebPBitstreamFeatures = record
    Width: Int32;
    Height: Int32;
    HasAlpha: Int32;
    HasAnimation: Int32;
    Format: Int32;        // 0 = undefined/mixed, 1 = lossy, 2 = lossless
    Pad: array[0..4] of UInt32;
  end;

var
  WebPGetDecoderVersion: function: Int32; cdecl;
  { Returns 0 (VP8_STATUS_OK) on success. }
  WebPGetFeaturesInternal: function(Data: PByte; DataSize: TWebPSize;
    var Features: TWebPBitstreamFeatures; Version: Int32): Int32; cdecl;
  { Decode into caller's buffer, return nil on failure. }
  WebPDecodeBGRAInto: function(Data: PByte; DataSize: TWebPSize;
    OutputBuffer: PByte; OutputBufferSize: TWebPSize; OutputStride: Int32): PByte; cdecl;
  WebPDecodeBGRInto: function(Data: PByte; DataSize: TWebPSize;
    OutputBuffer: PByte; OutputBufferSize: TWebPSize; OutputStride: Int32): PByte; cdecl;
  { Encoders return size of the data placed to Output (to be released
    by WebPFree) or 0 on failure. }
  WebPEncodeBGR: function(Bgr: PByte; Width, Height, Stride: Int32;
    QualityFactor: Single; Output: PPByte): TWebPSize; cdecl;
  WebPEncodeBGRA: function(Bgra: PByte; Width, Height, Stride: Int32;
    QualityFactor: Single; Output: PPByte): TWebPSize; cdecl;
  WebPEncodeLosslessBGR: function(Bgr: PByte; Width, Height, Stride: Int32;
    Output: PPByte): TWebPSize; cdecl;
  WebPEncodeLosslessBGRA: function(Bgra: PByte; Width, Height, Stride: Int32;
    Output: PPByte): TWebPSize; cdecl;
  WebPFree: procedure(Ptr: Pointer); cdecl;

{ Tries to load libwebp using the usual library names for the current platform.
  Returns True if the library with all needed functions is loaded (also
  when it was already loaded before). The search is done only once. }
function LoadLibWebP: Boolean;
{ Loads libwebp from the given file (replaces already loaded library). }
function LoadLibWebPFromFile(const FileName: string): Boolean;
{ Returns True if libwebp is currently loaded. }
function IsLibWebPLoaded: Boolean;
{ Version of loaded libwebp as "major.minor.revision" or empty string. }
function GetLibWebPVersion: string;

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
  LibNames: array[0..1] of string = ('libwebp.dll', 'libwebp-7.dll');
{$ELSEIF Defined(DARWIN) or Defined(MACOS)}
  LibNames: array[0..3] of string = ('libwebp.7.dylib', 'libwebp.dylib',
    '@executable_path/libwebp.7.dylib', '@executable_path/libwebp.dylib');
{$ELSE}
  LibNames: array[0..1] of string = ('libwebp.so.7', 'libwebp.so');
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

  WebPGetDecoderVersion := nil;
  WebPGetFeaturesInternal := nil;
  WebPDecodeBGRAInto := nil;
  WebPDecodeBGRInto := nil;
  WebPEncodeBGR := nil;
  WebPEncodeBGRA := nil;
  WebPEncodeLosslessBGR := nil;
  WebPEncodeLosslessBGRA := nil;
  WebPFree := nil;
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

  WebPGetDecoderVersion := GetProc('WebPGetDecoderVersion');
  WebPGetFeaturesInternal := GetProc('WebPGetFeaturesInternal');
  WebPDecodeBGRAInto := GetProc('WebPDecodeBGRAInto');
  WebPDecodeBGRInto := GetProc('WebPDecodeBGRInto');
  WebPEncodeBGR := GetProc('WebPEncodeBGR');
  WebPEncodeBGRA := GetProc('WebPEncodeBGRA');
  WebPEncodeLosslessBGR := GetProc('WebPEncodeLosslessBGR');
  WebPEncodeLosslessBGRA := GetProc('WebPEncodeLosslessBGRA');
  WebPFree := GetProc('WebPFree');

  Result := Assigned(WebPGetDecoderVersion) and Assigned(WebPGetFeaturesInternal) and
            Assigned(WebPDecodeBGRAInto) and Assigned(WebPDecodeBGRInto) and
            Assigned(WebPEncodeBGR) and Assigned(WebPEncodeBGRA) and
            Assigned(WebPEncodeLosslessBGR) and Assigned(WebPEncodeLosslessBGRA) and
            Assigned(WebPFree);

  if not Result then
    UnloadLib;
end;

function LoadLibWebP: Boolean;
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

function LoadLibWebPFromFile(const FileName: string): Boolean;
begin
  UnloadLib;
  SearchDone := True;
  Result := TryLoad(FileName);
end;

function IsLibWebPLoaded: Boolean;
begin
  Result := LibHandle <> 0;
end;

function GetLibWebPVersion: string;
var
  Ver: Int32;
begin
  Result := '';

  if LibHandle <> 0 then
  begin
    Ver := WebPGetDecoderVersion;
    Result := Format('%d.%d.%d', [(Ver shr 16) and $FF, (Ver shr 8) and $FF, Ver and $FF]);
  end;
end;

initialization

finalization
  UnloadLib;

end.
