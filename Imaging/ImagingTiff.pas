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

{ This unit contains image format loader/saver for TIFF images.
  TIFF codec used for the actual reading and writing (PasTiff or libtiff)
  is selected in ImagingTiffBackend unit. }
unit ImagingTiff;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Imaging, ImagingTypes, ImagingUtility, ImagingIO;

type
  { TIFF (Tag Image File Format) loader/saver class.
    Loads pages of most TIFF files (all usual compressions and sample
    layouts, strips and tiles, classic TIFF and BigTIFF) into the data format
    closest to their contents. Saves the formats in TiffSupportedFormats.}
  TTiffFileFormat = class(TImageFileFormat)
  protected
    FCompression: Integer;
    FJpegQuality: Integer;
    FBigTiffWriteMode: Integer;
    FPredictor: Integer;
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    function GetConversionFormat(const Info: TImageFormatInfo): TImageFormat; override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
    procedure CheckOptionsValidity; override;
    { Updates loading and saving capabilities according to currently
      available TIFF backend - call it after loading libtiff from a custom
      location (TryLoadLibTiffLibrary in ImagingTiffBackend unit). }
    procedure RefreshBackend;
    { Specifies compression scheme used when saving TIFF images. Supported values
      are 0 (Uncompressed), 1 (LZW), 2 (PackBits RLE), 3 (Deflate - ZLib), 4 (JPEG),
      5 (CCITT Group 4 fax encoding - for binary images only).
      Default is 1 (LZW). Note that not all images can be stored with
      JPEG compression - these images will be saved with default compression if
      JPEG is set.}
    property Compression: Integer read FCompression write FCompression;
    { Controls compression quality when selected TIFF compression is Jpeg.
      It is number in range 1..100. 1 means small/ugly file,
      100 means large/nice file. Accessible trough ImagingTiffJpegQuality option.}
    property JpegQuality: Integer read FJpegQuality write FJpegQuality;
    { Controls writing of BigTIFF files:
      - IfNeeded(0) writes BigTIFF only when the file size for sure exceeds
        the classic TIFF 4 GiB limit (can only assure for uncompressed TIFFs).
      - IfSafer(1) also uses BigTIFF when compression makes the final file size uncertain.
      - Always(2) always writes BigTIFF files. }
    property BigTiffWriteMode: Integer read FBigTiffWriteMode write FBigTiffWriteMode;
    { Controls the horizontal predictor of LZW and Deflate compression:
      Auto(0) decides for each image by compressing a sample of its rows both
      ways, On(1) always uses it, Off(2) never. Never used for palette images,
      floating point and 1-bit data. Accessible trough ImagingTiffPredictor
      option. }
    property Predictor: Integer read FPredictor write FPredictor;
  end;

const
  TiffCompressionOptionNone        = 0;
  TiffCompressionOptionLzw         = 1;
  TiffCompressionOptionPackbitsRle = 2;
  TiffCompressionOptionDeflate     = 3;
  TiffCompressionOptionJpeg        = 4;
  TiffCompressionOptionGroup4      = 5;

  TiffBigTiffWriteModeIfNeeded = 0;
  TiffBigTiffWriteModeIfSafer  = 1;
  TiffBigTiffWriteModeAlways   = 2;

  TiffPredictorOptionAuto = 0;
  TiffPredictorOptionOn   = 1;
  TiffPredictorOptionOff  = 2;

  { Data formats TIFF images are saved in, others are converted. }
  TiffSupportedFormats: TImageFormats = [ifIndex8, ifGray8, ifA8Gray8,
    ifGray16, ifA16Gray16, ifGray32, ifR8G8B8, ifA8R8G8B8, ifR16G16B16,
    ifA16R16G16B16, ifR32F, ifA32R32G32B32F, ifR16F, ifA16R16G16B16F, ifBinary];

  { Read only metadata info - name of compression scheme (LZW, none, JPEG, G4, ...)
    used in last loaded TIFF. }
  SMetaTiffCompressionName = 'TiffCompressionName';
  { Original resolution unit of loaded TIFF. Type is UInt.
    RESUNIT_NONE                        = 1;       // no meaningful units
    RESUNIT_INCH                        = 2;       // english
    RESUNIT_CENTIMETER                  = 3;       // metric }
  SMetaTiffResolutionUnit = 'TiffResolutionUnit';
  { Value of Orientation tag of a TIFF page: how the rows and columns as
    stored are to be shown. Type is UInt. Pixels are loaded and saved as
    they are, the orientation is never applied. When saving, the value
    for the page (or page 0) is written, 1 if there is none.
    Values (stored 0th row / 0th column, what shows the image upright):
      1 = top / left: as stored (the default)
      2 = top / right: mirror horizontally
      3 = bottom / right: rotate 180 degrees
      4 = bottom / left: mirror vertically
      5 = left / top: transpose (mirror along the top left to bottom right diagonal)
      6 = right / top: rotate 90 degrees clockwise
      7 = right / bottom: transverse (mirror along the top right to bottom left diagonal)
      8 = left / bottom: rotate 90 degrees counterclockwise
    Values are the same as of EXIF Orientation tag. }
  SMetaTiffOrientation = 'TiffOrientation';

implementation

uses
  ImagingTiffBackend, ImagingExtFileFormats;

const
  STiffFormatName = 'Tagged Image File Format';
  STiffMasks      = '*.tif,*.tiff';
  TiffDefaultCompression = TiffCompressionOptionLzw;
  TiffDefaultJpegQuality = 90;

  // Classic TIFF
  TiffBEMagic:   TChar4 = 'MM'#0#42;
  TiffLEMagic:   TChar4 = 'II'#42#0;  // II*
  // BigTIFF with 64bit offsets
  TiffBEMagic64: TChar4 = 'MM'#0#43;
  TiffLEMagic64: TChar4 = 'II'#43#0;  // II+

  // Values of TIFF tags
  CompressionNone     = 1;
  CompressionCcittRle = 2;
  CompressionCcittFax3 = 3;
  CompressionGroup4   = 4;
  CompressionLzw      = 5;
  CompressionOldJpeg  = 6;
  CompressionJpeg     = 7;
  CompressionAdobeDeflate = 8;
  CompressionDeflate  = 32946;
  CompressionPackBits = 32773;
  PhotometricMinIsBlack = 1;
  PhotometricRgb      = 2;
  PhotometricPalette  = 3;
  SampleFormatUInt    = 1;
  SampleFormatFloat   = 3;
  ResUnitNone         = 1;
  ResUnitInch         = 2;
  ResUnitCentimeter   = 3;

  { Compression tag values for ImagingTiffCompression option values. }
  OptionCompressions: array[TiffCompressionOptionNone..TiffCompressionOptionGroup4] of Integer = (
    CompressionNone, CompressionLzw, CompressionPackBits, CompressionAdobeDeflate,
    CompressionJpeg, CompressionGroup4);

  { Data formats of pixel layouts the backend delivers. }
  LayoutFormats: array[TTiffDataLayout] of TImageFormat = (
    ifUnknown, ifBinary, ifGray8, ifGray16, ifGray32, ifA8Gray8, ifA16Gray16,
    ifIndex8, ifR8G8B8, ifR16G16B16, ifA8R8G8B8, ifA16R16G16B16,
    ifR16F, ifR32F, ifA16R16G16B16F, ifA32R32G32B32F);

{ Name of compression scheme stored to SMetaTiffCompressionName metadata.
  The names are compared by applications (Deskew), keep them as they are. }
function CompressionName(Compression: Integer): string;
begin
  case Compression of
    CompressionNone: Result := 'None';
    CompressionLzw:  Result := 'LZW';
    CompressionJpeg: Result := 'JPEG';
    CompressionPackBits: Result := 'Packbits RLE';
    CompressionAdobeDeflate, CompressionDeflate: Result := 'Deflate';
    CompressionGroup4: Result := 'CCITT Group 4 Fax';
    CompressionOldJpeg: Result := 'Old JPEG';
    CompressionCcittRle, CompressionCcittFax3: Result := 'CCITT';
  else
    Result := 'Unknown';
  end;
end;

{ Decides between classic TIFF and BigTIFF before writing starts, see
  ImagingTiffBigTiffWriteMode option. }
function DecideBigTiff(const Images: TDynImageDataArray; FirstIndex, LastIndex: Integer;
  Compression, BigTiffWriteMode: Integer): Boolean;
const
  { BigTIFF/ClassicTIFF split: 32-bit offset field, this is the exact byte
    count beyond which an offset cannot be represented in classic mode.
    However, the classic TIFF file can still be much bigger - think 2 GB strip
    starting at the last possible offset. }
  ClassicTiffMaxFileSize: Int64 = Int64(4) * 1024 * 1024 * 1024; // 4 GiB
  // Allowance for IFD/tag/header overhead - 8K is enough for a lot of metadata per page
  PageOverhead = 8192;
var
  I: Integer;
  RawTotal, EstimatedTotal: Int64;
begin
  Result := BigTiffWriteMode = TiffBigTiffWriteModeAlways;
  if Result then
    Exit;

  RawTotal := 0;
  for I := FirstIndex to LastIndex do
    Inc(RawTotal, Images[I].Size);

  // For both IfNeeded and IfSafer modes: we know for sure we'll get a file > 4 GB
  EstimatedTotal := RawTotal + Int64(LastIndex - FirstIndex + 1) * PageOverhead;
  Result := (Compression = TiffCompressionOptionNone) and (EstimatedTotal > ClassicTiffMaxFileSize);
  if Result then
    Exit;

  if BigTiffWriteMode = TiffBigTiffWriteModeIfSafer then
  begin
    // Conservative estimates for compression: for JPEG assume 3x reduction
    // in data size, for others assume worst case of incompressible data
    if Compression = TiffCompressionOptionJpeg then
      EstimatedTotal := RawTotal div 3
    else
      EstimatedTotal := RawTotal + RawTotal div 10;
    Result := EstimatedTotal > ClassicTiffMaxFileSize;
  end;
end;

{ Decides about the horizontal predictor of a page by compressing a sample
  of its rows with and without it: the whole image when it is small, else
  bands of rows from the top, middle and bottom (an image often starts with
  a margin of paper, sky or no-data that is not like the rest). }
function PredictorHelps(const Image: TImageData; RowBytes: NativeInt;
  const Params: TTiffSaveParams): Boolean;
const
  SampleBytes = 256 * 1024;
  BandCount = 3;
var
  Sample: TDynByteArray;
  BandRows, Band, FirstRow: Integer;
  BandBytes: NativeInt;
begin
  if NativeInt(Image.Height) * RowBytes <= SampleBytes then
  begin
    Result := TiffPredictorHelps(Params, Image.Bits, Image.Height);
    Exit;
  end;

  BandRows := SampleBytes div BandCount div RowBytes;
  if BandRows < 1 then
    BandRows := 1;
  BandBytes := NativeInt(BandRows) * RowBytes;
  SetLength(Sample, BandCount * BandBytes);
  for Band := 0 to BandCount - 1 do
  begin
    FirstRow := Int64(Image.Height - BandRows) * Band div (BandCount - 1);
    Move(PBuffer(Image.Bits)[NativeInt(FirstRow) * RowBytes], Sample[Band * BandBytes], BandBytes);
  end;
  Result := TiffPredictorHelps(Params, Pointer(Sample), BandCount * BandRows);
end;

{ Swaps the first and third channel of Count pixels from Src to Dst:
  Imaging's BGR order to TIFF's RGB. }
procedure SwapRedBlue(Src, Dst: PByte; Count: NativeInt; const Info: TImageFormatInfo);
var
  I: NativeInt;
  ChannelSize, Bpp: Integer;
  SrcBuf, DstBuf: PBuffer;
begin
  Bpp := Info.BytesPerPixel;
  ChannelSize := Bpp div Info.ChannelCount;
  Move(Src^, Dst^, Count * Bpp);
  SrcBuf := PBuffer(Src);
  DstBuf := PBuffer(Dst);
  for I := 0 to Count - 1 do
  begin
    Move(SrcBuf[I * Bpp], DstBuf[I * Bpp + 2 * ChannelSize], ChannelSize);
    Move(SrcBuf[I * Bpp + 2 * ChannelSize], DstBuf[I * Bpp], ChannelSize);
  end;
end;

{
  TTiffFileFormat implementation
}

procedure TTiffFileFormat.Define;
begin
  inherited;
  FName := STiffFormatName;
  FCompression := TiffDefaultCompression;
  FJpegQuality := TiffDefaultJpegQuality;
  FBigTiffWriteMode := TiffBigTiffWriteModeIfNeeded;
  FPredictor := TiffPredictorOptionAuto;
  FSupportedFormats := TiffSupportedFormats;

  AddMasks(STiffMasks);
  RegisterOption(ImagingTiffCompression, @FCompression);
  RegisterOption(ImagingTiffJpegQuality, @FJpegQuality);
  RegisterOption(ImagingTiffBigTiffWriteMode, @FBigTiffWriteMode);
  RegisterOption(ImagingTiffPredictor, @FPredictor);
  RefreshBackend;
end;

procedure TTiffFileFormat.RefreshBackend;
begin
  // Without any TIFF codec (Delphi before 2009 without libtiff found) the
  // format is registered but can't load or save
  if TiffBackend <> tbNone then
    FFeatures := [ffLoad, ffSave, ffMultiImage, ffHugeImages]
  else
    FFeatures := [];
end;

procedure TTiffFileFormat.CheckOptionsValidity;
begin
  if (FCompression < TiffCompressionOptionNone) or (FCompression > TiffCompressionOptionGroup4) then
    FCompression := TiffDefaultCompression;
  FJpegQuality := ClampInt(FJpegQuality, 1, 100);
  if (FBigTiffWriteMode < TiffBigTiffWriteModeIfNeeded) or (FBigTiffWriteMode > TiffBigTiffWriteModeAlways) then
    FBigTiffWriteMode := TiffBigTiffWriteModeIfNeeded;
  if (FPredictor < TiffPredictorOptionAuto) or (FPredictor > TiffPredictorOptionOff) then
    FPredictor := TiffPredictorOptionAuto;
end;

function TTiffFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Magic: TChar4;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Magic, SizeOf(Magic));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount >= SizeOf(Magic)) and (
      (Magic = TiffBEMagic) or (Magic = TiffLEMagic) or
      (Magic = TiffBEMagic64) or (Magic = TiffLEMagic64)
    );
  end;
end;

function TTiffFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Reader: TTiffFileReader;
  I, Count: Integer;
  Info: TImageFormatInfo;
  Format: TImageFormat;

  procedure LoadMetadata(const Page: TTiffPageDesc; PageIndex: Integer);
  var
    ResUnit: TResolutionUnit;
  begin
    FMetadata.SetMetaItem(SMetaTiffResolutionUnit, Page.ResolutionUnit, PageIndex);
    if Page.HasResolution and (Page.ResolutionUnit <> ResUnitNone) and
      (Page.XResolution >= 0.1) and (Page.YResolution >= 0.1) then
    begin
      ResUnit := ruDpi;
      if Page.ResolutionUnit = ResUnitCentimeter then
        ResUnit := ruDpcm;
      FMetadata.SetPhysicalPixelSize(ResUnit, Page.XResolution, Page.YResolution, False, PageIndex);
    end;

    FMetadata.SetMetaItem(SMetaTiffCompressionName, CompressionName(Page.Compression), PageIndex);
    FMetadata.SetMetaItem(SMetaTiffOrientation, Page.Orientation, PageIndex);
  end;

begin
  Reader := TiffOpenReader(GetIO, Handle);
  try
    Count := Reader.PageCount;
    if OnlyFirstLevel and (Count > 1) then
      Count := 1;
    SetLength(Images, Count);

    for I := 0 to Count - 1 do
    begin
      Reader.SelectPage(I);
      Format := LayoutFormats[Reader.Layout];
      if Format = ifUnknown then
        RaiseImaging('TIFF page %d is not supported: %s', [I, Reader.UnsupportedReason]);

      NewImage(Reader.Page.Width, Reader.Page.Height, Format, Images[I]);
      GetImageFormatInfo(Format, Info);
      Reader.ReadPixels(Images[I].Bits, Info.GetPixelsSize(Format, Images[I].Width, 1));

      // Rows are in TIFF's RGB order which is Imaging's BGR
      if Info.IsIndexed then
        Reader.ReadPalette(Images[I].Palette)
      else if not Info.HasGrayChannel and (Info.ChannelCount >= 3) then
        SwapChannels(Images[I], ChannelRed, ChannelBlue);

      LoadMetadata(Reader.Page, I);
    end;
  finally
    Reader.Free;
  end;

  Result := True;
end;

function TTiffFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
const
  // Rows handed over to the writer at once are about this large
  GroupBytes = 1024 * 1024;
var
  Writer: TTiffFileWriter;
  I, Y, GroupRows, Rows: Integer;
  ImageToSave: TImageData;
  MustBeFreed, SwapRB: Boolean;
  Info: TImageFormatInfo;
  Params: TTiffSaveParams;
  RowBytes: NativeInt;
  Group: TDynByteArray;
  Src: PByte;

  procedure SaveMetadata(PageIndex: Integer);
  var
    XRes, YRes: Double;
    ResUnit: TResolutionUnit;
  begin
    XRes := -1;
    YRes := -1;
    ResUnit := ruDpcm;
    Params.ResolutionUnit := ResUnitCentimeter;

    // Check if DPI resolution unit is requested to be used (e.g. to
    // use the same unit when just resaving files)
    if FMetadata.HasMetaItemForSaving(SMetaTiffResolutionUnit) and
      (FMetadata.MetaItemsForSaving[SMetaTiffResolutionUnit] = ResUnitInch) then
    begin
      ResUnit := ruDpi;
      Params.ResolutionUnit := ResUnitInch;
    end;

    // First try to find phys. size for current TIFF page index. If not found then
    // try size for main image (index 0).
    if not FMetadata.GetPhysicalPixelSize(ResUnit, XRes, YRes, True, PageIndex) then
      FMetadata.GetPhysicalPixelSize(ResUnit, XRes, YRes, True, 0);

    if (XRes > 0) and (YRes > 0) then
    begin
      Params.XResolution := XRes;
      Params.YResolution := YRes;
    end
    else
      Params.ResolutionUnit := 0;

    // Orientation of the page, else of the main image, else as stored
    Params.Orientation := 1;
    if FMetadata.HasMetaItemForSaving(SMetaTiffOrientation, PageIndex) then
      Params.Orientation := FMetadata.MetaItemsForSavingMulti[SMetaTiffOrientation, PageIndex]
    else if FMetadata.HasMetaItemForSaving(SMetaTiffOrientation, 0) then
      Params.Orientation := FMetadata.MetaItemsForSavingMulti[SMetaTiffOrientation, 0];
    if (Params.Orientation < 1) or (Params.Orientation > 8) then
      Params.Orientation := 1;
  end;

begin
  Writer := TiffCreateWriter(GetIO, Handle,
                             DecideBigTiff(Images, FFirstIdx, FLastIdx, FCompression, FBigTiffWriteMode));
  try
    for I := FFirstIdx to FLastIdx do
    begin
      if not MakeCompatible(Images[I], ImageToSave, MustBeFreed) then
        Continue;

      try
        GetImageFormatInfo(ImageToSave.Format, Info);

        FillChar(Params, SizeOf(Params), 0);
        Params.Width := ImageToSave.Width;
        Params.Height := ImageToSave.Height;
        Params.SamplesPerPixel := Info.ChannelCount;
        Params.BitsPerSample := Info.BytesPerPixel div Info.ChannelCount * 8;
        if Info.Format = ifBinary then
        begin
          Params.BitsPerSample := 1;
          Params.SamplesPerPixel := 1;
        end;
        Params.SampleFormat := Iff(Info.IsFloatingPoint, SampleFormatFloat, SampleFormatUInt);
        Params.JpegQuality := FJpegQuality;

        if Info.IsIndexed then
        begin
          Params.Photometric := PhotometricPalette;
          Params.Palette := ImageToSave.Palette;
        end
        else if Info.HasGrayChannel or (Info.ChannelCount = 1) then
          Params.Photometric := PhotometricMinIsBlack
        else
          Params.Photometric := PhotometricRgb;

        // Check if selected compression scheme can be used for current image
        Params.Compression := OptionCompressions[FCompression];
        if (Params.Compression = CompressionJpeg) and ((Params.BitsPerSample <> 8) or
          not (Params.SamplesPerPixel in [1, 3]) or Info.IsIndexed or Info.IsFloatingPoint) then
        begin
          Params.Compression := CompressionLzw;
        end;
        if (Params.Compression = CompressionGroup4) and (Info.Format <> ifBinary) then
          Params.Compression := CompressionLzw;
        // If some compression is selected for binary image and it's not Fax then
        // select it automatically - better compression ratios
        if (Info.Format = ifBinary) and (Params.Compression <> CompressionNone) then
          Params.Compression := CompressionGroup4;

        SaveMetadata(I);
        RowBytes := Info.GetPixelsSize(Info.Format, ImageToSave.Width, 1);

        if TiffPredictorUsable(Params) then
        begin
          case FPredictor of
            TiffPredictorOptionOn:   Params.Predictor := True;
            TiffPredictorOptionAuto: Params.Predictor := PredictorHelps(ImageToSave, RowBytes, Params);
          end;
        end;

        Writer.BeginPage(Params);

        // Imaging's BGR order is swapped in a scratch buffer, the image
        // given by the caller is not touched
        SwapRB := (Params.Photometric = PhotometricRgb);
        GroupRows := 1;
        if RowBytes < GroupBytes then
          GroupRows := GroupBytes div RowBytes;
        if SwapRB then
          SetLength(Group, NativeInt(GroupRows) * RowBytes);

        Y := 0;
        while Y < ImageToSave.Height do
        begin
          Rows := Min(GroupRows, ImageToSave.Height - Y);
          Src := @PBuffer(ImageToSave.Bits)[NativeInt(Y) * RowBytes];
          if SwapRB then
          begin
            SwapRedBlue(Src, Pointer(Group), NativeInt(Rows) * ImageToSave.Width, Info);
            Src := Pointer(Group);
          end;
          Writer.WriteRows(Src, Rows);
          Inc(Y, Rows);
        end;

        Writer.EndPage;
      finally
        if MustBeFreed then
          FreeImage(ImageToSave);
      end;
    end;

    Writer.Finish;
  finally
    Writer.Free;
  end;

  Result := True;
end;

function TTiffFileFormat.GetConversionFormat(const Info: TImageFormatInfo): TImageFormat;
begin
  if Info.RBSwapFormat in GetSupportedFormats then
    Result := Info.RBSwapFormat
  else if Info.IsFloatingPoint then
    Result := IffFormat(Info.ChannelCount = 1, ifR32F, ifA32R32G32B32F)
  else if Info.HasGrayChannel then
    Result := IffFormat(Info.HasAlphaChannel, ifA16Gray16, ifGray32)
  else
    Result := IffFormat(Info.HasAlphaChannel, ifA8R8G8B8, ifR8G8B8);
end;

initialization
  RegisterImageFileFormat(TTiffFileFormat);

end.
