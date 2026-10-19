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

{ This unit contains image format loader/saver for WebP images.
  WebP codec used for the actual encoding and decoding is selected
  in ImagingLibWebP unit. }
unit ImagingWebP;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, ImagingTypes, Imaging, ImagingUtility;

type
  { Class for loading and saving WebP images (lossy and lossless, with or
    without alpha channel).
    Loading works when libwebp shared library is found or when the Pascal
    decoder is compiled in (FPC, Delphi 2009+). Saving needs libwebp.
    Only the first frame of animated WebP files is loaded.
    Note that WebP has a maximum size of 16383 x 16383 pixel.}
  TWebPFileFormat = class(TImageFileFormat)
  protected
    FQuality: LongInt;
    FLossless: LongBool;
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
    procedure CheckOptionsValidity; override;
    { Updates loading and saving capabilities according to currently
      available WebP backend - can call it again after loading libwebp from
      a custom location (TryLoadWebPLibrary in ImagingLibWebP unit). }
    procedure RefreshBackend;
  published
    { Quality of lossy compression used when saving in range 0..100,
      higher value means larger file with better quality.
      Accessible trough ImagingWebPQuality option. }
    property Quality: LongInt read FQuality write FQuality;
    { If True images are saved using lossless compression.
      Accessible trough ImagingWebPLossless option. }
    property Lossless: LongBool read FLossless write FLossless;
  end;

implementation

uses
  ImagingLibWebP, ImagingExtFileFormats;

const
  SWebPFormatName = 'WebP Image';
  SWebPMasks = '*.webp';
  WebPSupportedFormats: TImageFormats = [ifR8G8B8, ifA8R8G8B8];
  WebPDefaultQuality = 85;
  WebPDefaultLossless = False;

resourcestring
  SWebPDecodingFailed = 'WebP image could not be decoded';

type
  { Start of every WebP file. }
  TWebPRiffHeader = packed record
    Riff: TChar4;     // 'RIFF'
    Size: UInt32;     // Size of the rest of the file
    WebP: TChar4;     // 'WEBP'
  end;

const
  RiffMagic: TChar4 = 'RIFF';
  WebPMagic: TChar4 = 'WEBP';

function IsWebPHeader(const Hdr: TWebPRiffHeader): Boolean;
begin
  Result := (Hdr.Riff = RiffMagic) and (Hdr.WebP = WebPMagic);
end;

{ TWebPFileFormat class implementation }

procedure TWebPFileFormat.Define;
begin
  FName := SWebPFormatName;
  FSupportedFormats := WebPSupportedFormats;
  RefreshBackend;

  FQuality := WebPDefaultQuality;
  FLossless := WebPDefaultLossless;

  AddMasks(SWebPMasks);
  RegisterOption(ImagingWebPQuality, @FQuality);
  RegisterOption(ImagingWebPLossless, @FLossless);
end;

procedure TWebPFileFormat.RefreshBackend;
begin
  FFeatures := [];
  // WebP backend is selected at this point (if not already) as WebPCanLoad
  // triggers backend setup.
  if WebPCanLoad then
    Include(FFeatures, ffLoad);
  if WebPCanSave then
    Include(FFeatures, ffSave);
end;

procedure TWebPFileFormat.CheckOptionsValidity;
begin
  if (FQuality < 0) or (FQuality > 100) then
    FQuality := WebPDefaultQuality;
end;

function TWebPFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Hdr: TWebPRiffHeader;
  Data: PBuffer;
  DataSize, RestSize: Int64;
  Info: TWebPImageInfo;
begin
  Result := False;
  SetLength(Images, 1);

  with GetIO do
  begin
    if (Read(Handle, @Hdr, SizeOf(Hdr)) <> SizeOf(Hdr)) or not IsWebPHeader(Hdr) then
      Exit;

    // Codecs want the whole file in memory. Size in the header says how much
    // to read so only this one image is consumed from the input.
    RestSize := Int64(Hdr.Size) - SizeOf(Hdr.WebP);

    if RestSize <= 0 then
      Exit;

    DataSize := SizeOf(Hdr) + RestSize;
    GetMem(Data, DataSize);

    try
      Move(Hdr, Data^, SizeOf(Hdr));
      // File can be shorter than the header says, codec deals with that
      RestSize := Read(Handle, @Data[SizeOf(Hdr)], RestSize);
      if RestSize < 0 then
        RestSize := 0;
      DataSize := SizeOf(Hdr) + RestSize;

      Result := WebPLoadImage(Data, DataSize, Images[0], Info);
      if not Result then
        RaiseImaging(SWebPDecodingFailed);
    finally
      FreeMem(Data);
    end;
  end;
end;

function TWebPFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
  Params: TWebPSaveParams;
  Data: Pointer;
  DataSize: Int64;
begin
  Result := False;

  if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  try
    FillChar(Params, SizeOf(Params), 0);
    Params.Quality := FQuality;
    Params.Lossless := FLossless;

    if WebPSaveImage(ImageToSave, Params, Data, DataSize) then
    try
      Result := (GetIO.Write(Handle, Data, DataSize) = DataSize);
    finally
      WebPFreeData(Data);
    end;
  finally
    if MustBeFreed then
      FreeImage(ImageToSave);
  end;
end;

procedure TWebPFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
begin
  if Info.HasAlphaChannel then
    ConvertImage(Image, ifA8R8G8B8)
  else
    ConvertImage(Image, ifR8G8B8);
end;

function TWebPFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Hdr: TWebPRiffHeader;
  ReadCount: Int64;
begin
  Result := False;
  if Handle <> nil then
  with GetIO do
  begin
    FillChar(Hdr, SizeOf(Hdr), 0);
    ReadCount := Read(Handle, @Hdr, SizeOf(Hdr));
    Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount = SizeOf(Hdr)) and IsWebPHeader(Hdr);
  end;
end;

initialization
  RegisterImageFileFormat(TWebPFileFormat);

end.
