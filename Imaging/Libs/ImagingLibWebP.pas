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

{ This unit selects the WebP codec used by Imaging and wraps it in a small
  API working with Imaging's image data and memory buffers.

  Two implementations are available and the choice is made at run time:

  - libwebp shared library (DLL/SO/dylib) is used when it is found. It decodes
    and encodes (lossy and lossless) and is the only option for Delphi older
    than 2009. Define WEBP_NO_DYNLIB to never look for it.

  - WebPDec, pure Pascal decoder by Xelitan (port of libwebp decoder).
    Used when libwebp is not found, decoding only. Needs FPC or Delphi 2009+.
    Define WEBP_NO_BUNDLED to leave it out.

  In Delphi older than 2009, if libwebp is not found, there is no WebP support.

  The defines are listed in ImagingOptions.inc, set them there, in project
  options, or in ImagingUserOptions.inc.

  Animated WebP: Only the first frame of animated WebP files is loaded. }
unit ImagingLibWebP;

{$I ImagingOptions.inc}

// LibWebPDynLib unit can be used only on targets with dynamic libraries
{$IF not Defined(WEBP_NO_DYNLIB) and Defined(HAS_DYNLIBS)}
  {$DEFINE WEBP_USE_DYNLIB}
{$IFEND}

{$IF not Defined(WEBP_NO_BUNDLED) and
  ((Defined(DCC) and (CompilerVersion >= 20)) or Defined(FPC))}
  {$DEFINE WEBP_USE_BUNDLED}
{$IFEND}

interface

uses
  ImagingTypes;

type
  { WebP codec implementation in use. }
  TWebPBackend = (
    wbNone,     // nothing available, WebP can't be loaded nor saved
    wbPascal,   // Pascal decoder, loading only
    wbLibWebP   // libwebp shared library, loading and saving
  );

  TWebPImageInfo = record
    Width: Integer;
    Height: Integer;
    HasAlpha: Boolean;
    { Only the first frame of animated files is loaded. }
    Animated: Boolean;
  end;

  TWebPSaveParams = record
    Quality: Integer;  // Quality of lossy compression, 0..100
    Lossless: Boolean;
  end;

{ Returns WebP implementation in use. Looks for libwebp (in default locations)
  when called for the first time. }
function WebPBackend: TWebPBackend;
function WebPCanLoad: Boolean;
function WebPCanSave: Boolean;

{ Loads libwebp from the given file and uses it from now on (instead of
  looking for it in the default locations) - updates the current backend.
  Returns False and falls back to the Pascal decoder (if available)
  when the library can't be loaded. }
function TryLoadWebPLibrary(const FileName: string): Boolean;

function WebPGetImageInfo(Data: Pointer; Size: Int64; out Info: TWebPImageInfo): Boolean;
{ Decodes WebP file stored in memory. New image is created in Image: ifA8R8G8B8
  if the file has alpha channel, ifR8G8B8 otherwise. For animated files the
  first frame is loaded (placed on the canvas of the animation). }
function WebPLoadImage(Data: Pointer; Size: Int64; var Image: TImageData;
  out Info: TWebPImageInfo): Boolean;
{ Encodes Image (ifR8G8B8 or ifA8R8G8B8) to WebP file in memory. Encoded data
  must be released by WebPFreeData. }
function WebPSaveImage(const Image: TImageData; const Params: TWebPSaveParams;
  out Data: Pointer; out Size: Int64): Boolean;
procedure WebPFreeData(Data: Pointer);

implementation

uses
{$IFDEF WEBP_USE_DYNLIB}
  LibWebPDynLib,
{$ENDIF}
{$IFDEF WEBP_USE_BUNDLED}
  WebPDec,
{$ENDIF}
  SysUtils, Imaging;

const
  RiffTag = $46464952;  // 'RIFF'
  WebPTag = $50424557;  // 'WEBP'
  VP8XTag = $58385056;  // 'VP8X'
  VP8Tag  = $20385056;  // 'VP8 '
  VP8LTag = $4C385056;  // 'VP8L'
  ALPHTag = $48504C41;  // 'ALPH'
  ANIMTag = $4D494E41;  // 'ANIM'
  ANMFTag = $464D4E41;  // 'ANMF'

  RiffHeaderSize = 12;
  ChunkHeaderSize = 8;
  VP8XChunkSize = 10;
  VP8XFlagAnimation = $02;
  VP8XFlagAlpha = $10;

var
  Backend: TWebPBackend = wbNone;
  BackendDetected: Boolean = False;

function ReadByte(Data: PBuffer; Offset: Int64): Byte;
begin
  Result := Data[Offset];
end;

function ReadUInt32(Data: PBuffer; Offset: Int64): UInt32;
begin
  Result := PUInt32(@Data[Offset])^;
end;

{ There is no 3 byte type to read and reading 4 bytes could reach
  beyond the end of the data. }
function ReadUInt24(Data: PBuffer; Offset: Int64): UInt32;
begin
  Result := Data[Offset] or (UInt32(Data[Offset + 1]) shl 8) or (UInt32(Data[Offset + 2]) shl 16);
end;

procedure WriteUInt32(Data: PBuffer; Offset: Int64; Value: UInt32);
begin
  PUInt32(@Data[Offset])^ := Value;
end;

procedure SelectBackend;
begin
  Backend := wbNone;
{$IFDEF WEBP_USE_BUNDLED}
  Backend := wbPascal;
{$ENDIF}
{$IFDEF WEBP_USE_DYNLIB}
  if IsLibWebPLoaded then
    Backend := wbLibWebP;
{$ENDIF}
  BackendDetected := True;
end;

function WebPBackend: TWebPBackend;
begin
  if not BackendDetected then
  begin
  {$IFDEF WEBP_USE_DYNLIB}
    LoadLibWebP;
  {$ENDIF}
    SelectBackend;
  end;
  Result := Backend;
end;

function WebPCanLoad: Boolean;
begin
  Result := WebPBackend <> wbNone;
end;

function WebPCanSave: Boolean;
begin
  Result := WebPBackend = wbLibWebP;
end;

function TryLoadWebPLibrary(const FileName: string): Boolean;
begin
{$IFDEF WEBP_USE_DYNLIB}
  Result := LoadLibWebPFromFile(FileName);
{$ELSE}
  Result := False;
{$ENDIF}
  SelectBackend;
end;

{$IFDEF WEBP_USE_BUNDLED}
{ Finds out what the Pascal decoder doesn't tell: presence of alpha channel
  and animation. Walks RIFF chunks up to the image data. }
function ParseWebPContainer(Data: PBuffer; Size: Int64; out Info: TWebPImageInfo): Boolean;
var
  Pos: Int64;
  Tag, ChunkSize: UInt32;
begin
  Result := False;
  FillChar(Info, SizeOf(Info), 0);

  if (Size < RiffHeaderSize + ChunkHeaderSize) or (ReadUInt32(Data, 0) <> RiffTag) or
     (ReadUInt32(Data, 8) <> WebPTag) then
  begin
    Exit;
  end;

  Pos := RiffHeaderSize;

  while Pos + ChunkHeaderSize <= Size do
  begin
    Tag := ReadUInt32(Data, Pos);
    ChunkSize := ReadUInt32(Data, Pos + 4);

    if Tag = VP8XTag then
    begin
      if Pos + ChunkHeaderSize + VP8XChunkSize > Size then
        Exit;

      if ReadByte(Data, Pos + 8) and VP8XFlagAlpha <> 0 then
        Info.HasAlpha := True;
      if ReadByte(Data, Pos + 8) and VP8XFlagAnimation <> 0 then
        Info.Animated := True;
      // Canvas size, the only size info for animated files
      Info.Width := 1 + ReadUInt24(Data, Pos + 12);
      Info.Height := 1 + ReadUInt24(Data, Pos + 15);
    end
    else if (Tag = ANIMTag) or (Tag = ANMFTag) then
    begin
      Info.Animated := True;
      Result := True;
      Exit;
    end
    else if Tag = ALPHTag then
    begin
      Info.HasAlpha := True;
    end
    else if Tag = VP8Tag then
    begin
      Result := True;
      Exit;
    end
    else if Tag = VP8LTag then
    begin
      // Signature byte, 14 bits width, 14 bits height, 1 bit alpha
      if (Pos + 13 > Size) or (ReadByte(Data, Pos + 8) <> $2F) then
        Exit;

      if ReadUInt32(Data, Pos + 9) and (1 shl 28) <> 0 then
        Info.HasAlpha := True;
      Result := True;
      Exit;
    end;

    // Chunks are padded to even size
    Pos := Pos + ChunkHeaderSize + ChunkSize + (ChunkSize and 1);
  end;
end;

function PascalGetImageInfo(Data: Pointer; Size: Int64; out Info: TWebPImageInfo): Boolean;
begin
  Result := ParseWebPContainer(Data, Size, Info);
  if Result and not Info.Animated then
    Result := WebPDec.WebPGetInfo(Data, Size, Info.Width, Info.Height);
end;

function PascalLoadImage(Data: Pointer; Size: Int64; var Image: TImageData;
  const Info: TWebPImageInfo): Boolean;
var
  Pixels: PByte;
  Width, Height: Integer;
  Format: TImageFormat;
begin
  Result := False;

  if Info.HasAlpha then
  begin
    Format := ifA8R8G8B8;
    Pixels := WebPDec.WebPDecodeBGRA(Data, Size, Width, Height);
  end
  else
  begin
    Format := ifR8G8B8;
    Pixels := WebPDec.WebPDecodeBGR(Data, Size, Width, Height);
  end;

  if Pixels = nil then
    Exit;

  try
    if (Width = Info.Width) and (Height = Info.Height) and
       NewImage(Width, Height, Format, Image) then
    begin
      Move(Pixels^, Image.Bits^, Image.Size);
      Result := True;
    end;
  finally
    WebPDec.WebPFree(Pixels);
  end;
end;
{$ENDIF}

{$IFDEF WEBP_USE_DYNLIB}
function LibGetImageInfo(Data: Pointer; Size: Int64; out Info: TWebPImageInfo): Boolean;
var
  Features: TWebPBitstreamFeatures;
begin
  FillChar(Info, SizeOf(Info), 0);
  FillChar(Features, SizeOf(Features), 0);
  Result := WebPGetFeaturesInternal(Data, Size, Features, WEBP_DECODER_ABI_VERSION) = 0;

  if Result then
  begin
    Info.Width := Features.Width;
    Info.Height := Features.Height;
    Info.HasAlpha := Features.HasAlpha <> 0;
    Info.Animated := Features.HasAnimation <> 0;
  end;
end;

function LibLoadImage(Data: Pointer; Size: Int64; var Image: TImageData;
  const Info: TWebPImageInfo): Boolean;
begin
  Result := False;

  if Info.HasAlpha then
  begin
    if NewImage(Info.Width, Info.Height, ifA8R8G8B8, Image) then
      Result := WebPDecodeBGRAInto(Data, Size, Image.Bits, Image.Size, Info.Width * 4) <> nil;
  end
  else
  begin
    if NewImage(Info.Width, Info.Height, ifR8G8B8, Image) then
      Result := WebPDecodeBGRInto(Data, Size, Image.Bits, Image.Size, Info.Width * 3) <> nil;
  end;

  if not Result then
    FreeImage(Image);
end;
{$ENDIF}

function WebPGetImageInfo(Data: Pointer; Size: Int64; out Info: TWebPImageInfo): Boolean;
begin
  Result := False;
  FillChar(Info, SizeOf(Info), 0);
  if (Data = nil) or (Size <= 0) then
    Exit;

{$IFDEF WEBP_USE_DYNLIB}
  if WebPBackend = wbLibWebP then
    Result := LibGetImageInfo(Data, Size, Info);
{$ENDIF}
{$IFDEF WEBP_USE_BUNDLED}
  if WebPBackend = wbPascal then
    Result := PascalGetImageInfo(Data, Size, Info);
{$ENDIF}
end;

function LoadStillImage(Data: Pointer; Size: Int64; var Image: TImageData;
  const Info: TWebPImageInfo): Boolean;
begin
  Result := False;
  if Info.Animated or (Info.Width <= 0) or (Info.Height <= 0) then
    Exit;

{$IFDEF WEBP_USE_DYNLIB}
  if WebPBackend = wbLibWebP then
    Result := LibLoadImage(Data, Size, Image, Info);
{$ENDIF}
{$IFDEF WEBP_USE_BUNDLED}
  if WebPBackend = wbPascal then
    Result := PascalLoadImage(Data, Size, Image, Info);
{$ENDIF}
end;

type
  { Placement of animation frame on the canvas. }
  TWebPFrameRect = record
    CanvasWidth, CanvasHeight: Integer;
    X, Y, Width, Height: Integer;
  end;

{ Animated WebP has no image that still image decoders could decode, all frames
  (even the first one) are inside ANMF chunks. This finds the first frame and
  builds a still WebP file from its data in newly allocated memory (FrameData,
  to be released by FreeMem). }
function ExtractFirstFrame(Data: PBuffer; Size: Int64; out FrameData: Pointer;
  out FrameSize: Int64; out Rect: TWebPFrameRect): Boolean;
const
  FrameHeaderSize = 16;
var
  Pos, PayloadPos, PayloadSize, SubPos, DestPos: Int64;
  Tag, ChunkSize: UInt32;
  HasAlphaChunk: Boolean;
  Dest: PBuffer;
begin
  Result := False;
  FrameData := nil;
  FrameSize := 0;
  FillChar(Rect, SizeOf(Rect), 0);

  if (Size < RiffHeaderSize + ChunkHeaderSize) or (ReadUInt32(Data, 0) <> RiffTag) or
     (ReadUInt32(Data, 8) <> WebPTag) then
    Exit;

  Pos := RiffHeaderSize;

  while Pos + ChunkHeaderSize <= Size do
  begin
    Tag := ReadUInt32(Data, Pos);
    ChunkSize := ReadUInt32(Data, Pos + 4);

    if (Tag = VP8XTag) and (Pos + ChunkHeaderSize + VP8XChunkSize <= Size) then
    begin
      Rect.CanvasWidth := 1 + ReadUInt24(Data, Pos + 12);
      Rect.CanvasHeight := 1 + ReadUInt24(Data, Pos + 15);
    end
    else if Tag = ANMFTag then
    begin
      // Frame header (offsets are stored divided by two) followed by image chunks
      if (ChunkSize <= FrameHeaderSize) or (Rect.CanvasWidth <= 0) or
         (Pos + ChunkHeaderSize + ChunkSize > Size) then
        Exit;

      Rect.X := 2 * ReadUInt24(Data, Pos + 8);
      Rect.Y := 2 * ReadUInt24(Data, Pos + 11);
      Rect.Width := 1 + ReadUInt24(Data, Pos + 14);
      Rect.Height := 1 + ReadUInt24(Data, Pos + 17);
      PayloadPos := Pos + ChunkHeaderSize + FrameHeaderSize;
      PayloadSize := Int64(ChunkSize) - FrameHeaderSize;

      // Lossy frame with separate alpha data needs extended file header
      HasAlphaChunk := False;
      SubPos := PayloadPos;

      while SubPos + ChunkHeaderSize <= PayloadPos + PayloadSize do
      begin
        if ReadUInt32(Data, SubPos) = ALPHTag then
          HasAlphaChunk := True;
        ChunkSize := ReadUInt32(Data, SubPos + 4);
        SubPos := SubPos + ChunkHeaderSize + ChunkSize + (ChunkSize and 1);
      end;

      FrameSize := RiffHeaderSize + PayloadSize;
      if HasAlphaChunk then
        FrameSize := FrameSize + ChunkHeaderSize + VP8XChunkSize;

      GetMem(FrameData, FrameSize);
      Dest := FrameData;
      WriteUInt32(Dest, 0, RiffTag);
      WriteUInt32(Dest, 4, FrameSize - 8);
      WriteUInt32(Dest, 8, WebPTag);
      DestPos := RiffHeaderSize;

      if HasAlphaChunk then
      begin
        WriteUInt32(Dest, DestPos, VP8XTag);
        WriteUInt32(Dest, DestPos + 4, VP8XChunkSize);
        WriteUInt32(Dest, DestPos + 8, VP8XFlagAlpha);      // flags + 3 reserved bytes
        WriteUInt32(Dest, DestPos + 12, Rect.Width - 1);    // 3 bytes, 4th overwritten below
        WriteUInt32(Dest, DestPos + 15, Rect.Height - 1);   // 3 bytes + 1 byte beyond the chunk
        DestPos := DestPos + ChunkHeaderSize + VP8XChunkSize;
      end;

      // Last WriteUInt32 above touched the first payload byte, payload goes over it
      Move(Data[PayloadPos], Dest[DestPos], PayloadSize);
      Result := True;
      Exit;
    end;

    Pos := Pos + ChunkHeaderSize + ChunkSize + (ChunkSize and 1);
  end;
end;

{ Loads the first frame of animated WebP. The frame can be smaller than the
  canvas and placed anywhere on it, the rest of the canvas is transparent then. }
function LoadFirstFrame(Data: Pointer; Size: Int64; var Image: TImageData;
  var Info: TWebPImageInfo): Boolean;
var
  FrameData: Pointer;
  FrameSize: Int64;
  Rect: TWebPFrameRect;
  FrameInfo: TWebPImageInfo;
  Frame: TImageData;
  Y, CopyWidth, CopyHeight: Integer;
begin
  Result := False;
  if not ExtractFirstFrame(Data, Size, FrameData, FrameSize, Rect) then
    Exit;

  InitImage(Frame);
  try
    if not WebPGetImageInfo(FrameData, FrameSize, FrameInfo) or
       not LoadStillImage(FrameData, FrameSize, Frame, FrameInfo) then
      Exit;

    if (Rect.X = 0) and (Rect.Y = 0) and (Frame.Width = Rect.CanvasWidth) and
       (Frame.Height = Rect.CanvasHeight) then
    begin
      // Frame covers the whole canvas, just hand it over
      Image := Frame;
      InitImage(Frame);
    end
    else
    begin
      if not ConvertImage(Frame, ifA8R8G8B8) or
         not NewImage(Rect.CanvasWidth, Rect.CanvasHeight, ifA8R8G8B8, Image) then
        Exit;

      FillChar(Image.Bits^, Image.Size, 0);

      CopyWidth := Frame.Width;
      if Rect.X + CopyWidth > Image.Width then
        CopyWidth := Image.Width - Rect.X;

      CopyHeight := Frame.Height;
      if Rect.Y + CopyHeight > Image.Height then
        CopyHeight := Image.Height - Rect.Y;

      if CopyWidth > 0 then
      begin
        for Y := 0 to CopyHeight - 1 do
        begin
          Move(PBuffer(Frame.Bits)[PtrInt(Y) * Frame.Width * 4],
               PBuffer(Image.Bits)[(PtrInt(Rect.Y + Y) * Image.Width + Rect.X) * 4],
               CopyWidth * 4);
        end;
      end;
    end;

    Info.Width := Image.Width;
    Info.Height := Image.Height;
    Info.HasAlpha := Image.Format = ifA8R8G8B8;
    Result := True;
  finally
    FreeImage(Frame);
    FreeMem(FrameData);
  end;
end;

function WebPLoadImage(Data: Pointer; Size: Int64; var Image: TImageData;
  out Info: TWebPImageInfo): Boolean;
begin
  Result := False;
  if not WebPGetImageInfo(Data, Size, Info) then
    Exit;

  if Info.Animated then
    Result := LoadFirstFrame(Data, Size, Image, Info)
  else
    Result := LoadStillImage(Data, Size, Image, Info);
end;

function WebPSaveImage(const Image: TImageData; const Params: TWebPSaveParams;
  out Data: Pointer; out Size: Int64): Boolean;
{$IFDEF WEBP_USE_DYNLIB}
var
  Output: PByte;
{$ENDIF}
begin
  Result := False;
  Data := nil;
  Size := 0;
{$IFDEF WEBP_USE_DYNLIB}
  if WebPBackend <> wbLibWebP then
    Exit;

  Output := nil;

  case Image.Format of
    ifA8R8G8B8:
      begin
        if Params.Lossless then
          Size := WebPEncodeLosslessBGRA(Image.Bits, Image.Width, Image.Height,
                                         Image.Width * 4, @Output)
        else
          Size := WebPEncodeBGRA(Image.Bits, Image.Width, Image.Height,
                                 Image.Width * 4, Params.Quality, @Output);
      end;
    ifR8G8B8:
      begin
        if Params.Lossless then
          Size := WebPEncodeLosslessBGR(Image.Bits, Image.Width, Image.Height,
                                        Image.Width * 3, @Output)
        else
          Size := WebPEncodeBGR(Image.Bits, Image.Width, Image.Height,
                                Image.Width * 3, Params.Quality, @Output);
      end;
  else
    Exit;
  end;

  Result := (Size > 0) and (Output <> nil);

  if Result then
    Data := Output
  else
  begin
    Size := 0;
    if Output <> nil then
      LibWebPDynLib.WebPFree(Output);
  end;
{$ENDIF}
end;

procedure WebPFreeData(Data: Pointer);
begin
{$IFDEF WEBP_USE_DYNLIB}
  if (Data <> nil) and IsLibWebPLoaded then
    LibWebPDynLib.WebPFree(Data);
{$ENDIF}
end;

end.
