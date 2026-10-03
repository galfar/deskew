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

{ This unit contains image format loader/saver for Jpeg images.
  JPEG codec library used for the actual encoding and decoding is selected
  in ImagingJpegLib unit.}
unit ImagingJpeg;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, ImagingTypes, Imaging, ImagingColors, ImagingUtility;

type
  { Class for loading/saving Jpeg images. Supports load/save of
    8 bit grayscale and 24 bit RGB images. Jpegs can be saved with optional
    progressive encoding.
    Based on IJG's JpegLib so doesn't support alpha channels and lossless
    coding.}
  TJpegFileFormat = class(TImageFileFormat)
  protected
    FQuality: LongInt;
    FProgressive: LongBool;
    FChromaSubsampling: LongInt;
    procedure SetJpegIO(const JpegIO: TIOFunctions); virtual;
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
  published
    { Controls Jpeg save compression quality. It is number in range 1..100.
      1 means small/ugly file, 100 means large/nice file. Accessible trough
      ImagingJpegQuality option.}
    property Quality: LongInt read FQuality write FQuality;
    { If True Jpeg images are saved in progressive format. Accessible trough
      ImagingJpegProgressive option.}
    property Progressive: LongBool read FProgressive write FProgressive;
    { Chroma subsampling used when saving color Jpeg images: 0 (4:2:0),
      1 (4:2:2), or 2 (4:4:4). Accessible trough ImagingJpegChromaSubsampling
      option.}
    property ChromaSubsampling: LongInt read FChromaSubsampling write FChromaSubsampling;
  end;

implementation

uses
  ImagingJpegLib;

const
  SJpegFormatName = 'Joint Photographic Experts Group Image';
  SJpegMasks      = '*.jpg,*.jpeg,*.jfif,*.jpe,*.jif';
  JpegSupportedFormats: TImageFormats = [ifR8G8B8, ifGray8];
  JpegDefaultQuality = 90;
  JpegDefaultProgressive = False;
  JpegDefaultChromaSubsampling = 0;

  { Values of ImagingJpegChromaSubsampling option }
  JpegChromaSubsamplings: array[0..2] of TJpegChromaSubsampling =
    (jss420, jss422, jss444);

const
  { Jpeg file identifiers.}
  JpegMagic: TChar2 = #$FF#$D8;

var
  JIO: TIOFunctions;

{ TJpegFileFormat class implementation }

procedure TJpegFileFormat.Define;
begin
  FName := SJpegFormatName;
  FFeatures := [ffLoad, ffSave];
  FSupportedFormats := JpegSupportedFormats;

  FQuality := JpegDefaultQuality;
  FProgressive := JpegDefaultProgressive;
  FChromaSubsampling := JpegDefaultChromaSubsampling;

  AddMasks(SJpegMasks);
  RegisterOption(ImagingJpegQuality, @FQuality);
  RegisterOption(ImagingJpegProgressive, @FProgressive);
  RegisterOption(ImagingJpegChromaSubsampling, @FChromaSubsampling);
end;

procedure TJpegFileFormat.CheckOptionsValidity;
begin
  // Check if option values are valid
  if not (FQuality in [1..100]) then
    FQuality := JpegDefaultQuality;
  if (FChromaSubsampling < Low(JpegChromaSubsamplings)) or
    (FChromaSubsampling > High(JpegChromaSubsamplings)) then
    FChromaSubsampling := JpegDefaultChromaSubsampling;
end;

function TJpegFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  I: Integer;
  JpegInfo: TJpegImageInfo;
  Col32: PColor32Rec;
  ResUnit: TResolutionUnit;
begin
  // Copy IO functions to global var used for JPEG library IO
  SetJpegIO(GetIO);
  SetLength(Images, 1);

  Result := JpegLoadImage(JIO, Handle, Images[0], JpegInfo);

  // Partially decoded (corrupted) images are returned as they are
  if Result and JpegInfo.Complete then
  with Images[0] do
  begin
    if JpegInfo.ColorSpace = jcsCMYK then
    begin
      Col32 := Bits;
      // Translate from CMYK to RGB
      for I := 0 to Width * Height - 1 do
      begin
        CMYKToRGB(255 - Col32.B, 255 - Col32.G, 255 - Col32.R, 255 - Col32.A,
          Col32.R, Col32.G, Col32.B);
        Col32.A := 255;
        Inc(Col32);
      end;
    end;

    // Store supported metadata, density unit: 0 - undef, 1 - inch, 2 - cm
    if JpegInfo.HasJFIF and (JpegInfo.DensityUnit > 0) and
      (JpegInfo.XDensity > 0) and (JpegInfo.YDensity > 0) then
    begin
      ResUnit := ruDpi;
      if JpegInfo.DensityUnit = 2 then
        ResUnit := ruDpcm;
      FMetadata.SetPhysicalPixelSize(ResUnit, JpegInfo.XDensity, JpegInfo.YDensity);
    end;
  end;
end;

function TJpegFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
  Params: TJpegSaveParams;
  XRes, YRes: Double;
begin
  Result := False;
  // Copy IO functions to global var used for JPEG library IO
  SetJpegIO(GetIO);

  // Makes image to save compatible with Jpeg saving capabilities
  if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  try
    FillChar(Params, SizeOf(Params), 0);
    Params.Quality := FQuality;
    Params.Progressive := FProgressive;
    Params.ChromaSubsampling := JpegChromaSubsamplings[FChromaSubsampling];

    // Save supported metadata
    if FMetadata.GetPhysicalPixelSize(ruDpcm, XRes, YRes, True) then
    begin
      Params.DensityUnit := 2; // Dots per cm
      Params.XDensity := Round(XRes);
      Params.YDensity := Round(YRes);
    end;

    Result := JpegSaveImage(JIO, Handle, ImageToSave, Params);
  finally
    if MustBeFreed then
      FreeImage(ImageToSave);
  end;
end;

procedure TJpegFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
begin
  if Info.HasGrayChannel then
    ConvertImage(Image, ifGray8)
  else
    ConvertImage(Image, ifR8G8B8);
end;

function TJpegFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  ReadCount: LongInt;
  ID: array[0..9] of AnsiChar;
begin
  Result := False;
  if Handle <> nil then
  with GetIO do
  begin
    FillChar(ID, SizeOf(ID), 0);
    ReadCount := Read(Handle, @ID, SizeOf(ID));
    Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount = SizeOf(ID)) and
      CompareMem(@ID, @JpegMagic, SizeOf(JpegMagic));
  end;
end;

procedure TJpegFileFormat.SetJpegIO(const JpegIO: TIOFunctions);
begin
  JIO := JpegIO;
end;

initialization
  RegisterImageFileFormat(TJpegFileFormat);

{
  File Notes:

 -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.77.1 ---------------------------------------------------
    - Able to read corrupted JPEG files - loads partial image
      and skips the corrupted parts (FPC and x86 Delphi).
    - Fixed reading of physical resolution metadata, could cause
      "divided by zero" later on for some files.

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - Fixed loading of some JPEGs with certain APPN markers (bug in JpegLib).
    - Fixed swapped Red-Blue order when loading Jpegs with
      jc.d.jpeg_color_space = JCS_RGB.
    - Added loading and saving of physical pixel size metadata.

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Changed the Jpeg error manager, messages were not properly formatted.

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - Fixed wrong color space setting in InitCompressor.
    - Fixed problem with progressive Jpegs in FPC (modified JpegLib,
      can't use FPC's PasJpeg in Windows).

  -- 0.25.0 Changes/Bug Fixes ---------------------------------
    - FPC's PasJpeg wasn't really used in last version, fixed.

  -- 0.24.1 Changes/Bug Fixes ---------------------------------
    - Fixed loading of CMYK jpeg images. Could cause heap corruption
      and loaded image looked wrong.

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Removed JFIF/EXIF detection from TestFormat. Found JPEGs
      with different headers (Lavc) which weren't recognized. 

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - MakeCompatible method moved to base class, put ConvertToSupported here.
      GetSupportedFormats removed, it is now set in constructor.
    - Made public properties for options registered to SetOption/GetOption
      functions.
    - Changed extensions to filename masks.
    - Changed SaveData, LoadData, and MakeCompatible methods according
      to changes in base class in Imaging unit.
    - Changes in TestFormat, now reads JFIF and EXIF signatures too.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - input position is now set correctly to the end of the image
      after loading is done. Loading of sequence of JPEG files stored in
      single stream works now
    - when loading and saving images in FPC with PASJPEG read and
      blue channels are swapped to have the same chanel order as IMJPEGLIB
    - you can now choose between IMJPEGLIB and PASJPEG implementations

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - added SetJpegIO method which is used by JNG image format
}
end.

