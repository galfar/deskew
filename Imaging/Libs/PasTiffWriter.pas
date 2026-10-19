{
  PasTiff - pure Pascal TIFF reader/writer
  by Marek Mauder
  https://github.com/galfar/imaginglib
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
}

{ TIFF writer: classic TIFF and BigTIFF files with one or more pages, pixel
  data in strips.

  The writer is streaming. Rows are handed over in any portions, collected
  into strips, compressed and written out as each strip fills up; only one
  strip is ever held in memory, so images larger than memory can be written.
  The directory of a page is written after its data, when all strip offsets
  are known, and the previous directory (or the file header) is then linked
  to it. Nothing already written is changed except those link fields.

  Rows must be given the way TIFF stores them - the writer converts nothing:
    - 1 bit per pixel packed most significant bit first, or 8/16/32-bit
      samples in the CPU's byte order,
    - samples of a pixel together, color first then alpha: gray, gray+alpha,
      R,G,B, R,G,B,A, or a palette index,
    - rows padded to whole bytes.
  Supported are the layouts Vampyre Imaging Library saves to TIFF plus 16-bit
  palette (see CheckParams). Files are little-endian. Tiles, separate planes
  and appending to existing files are not supported.

  Reference: libtiff's tif_write.c and tif_dirwrite.c. }
unit PasTiffWriter;

{$I PasTiff.inc}

interface

uses
  SysUtils, Classes, PasTiffTypes, PasTiffDirectory, PasTiffCodecs;

type
  { Description of a page to write. Start with TiffInitPageParams. }
  TTiffPageParams = record
    Width, Height: Integer;
    BitsPerSample: Integer;       // 1, 8, 16 or 32
    SamplesPerPixel: Integer;     // color samples plus optional alpha
    SampleFormat: Integer;        // TiffSampleFormatUInt or TiffSampleFormatFloat
    { TiffPhotometricMinIsBlack, MinIsWhite (data must already be in that
      sense), Rgb or Palette. }
    Photometric: Integer;
    { Meaning of the sample beyond the color ones, if there is one:
      TiffExtraSampleUnassAlpha (default) or TiffExtraSampleAssocAlpha. }
    ExtraSample: Integer;
    { TiffCompressionNone, PackBits, Lzw, AdobeDeflate, CcittFax4 (1-bit
      data only) or Jpeg (8-bit gray and RGB only). }
    Compression: Integer;
    { Horizontal differencing before LZW or Deflate. Helps a lot with
      photographs and scans in 8 and 16 bits; ignored where it cannot be used. }
    Predictor: Boolean;
    { 0 lets the writer choose (strips of about 256 KiB of raw data). }
    RowsPerStrip: Integer;
    { JPEG only: quality 1..100, and chroma subsampling of RGB images,
      2x2 (default), 2x1 or 1x1. }
    JpegQuality: Integer;
    JpegSubsampleH, JpegSubsampleV: Integer;
    { Resolution is written if ResolutionUnit is not 0 (TiffResUnit*). }
    ResolutionUnit: Integer;
    XResolution, YResolution: Double;
    { For TiffPhotometricPalette: 3 * 2^BitsPerSample 16-bit values, all reds
      first, then greens, then blues; 65535 is full intensity. }
    ColorMap: TTiffWordArray;
  end;

  TTiffWriter = class
  private
    FStream: TStream;
    FOwnsStream: Boolean;
    FContainer: TTiffContainer;
    FLinkOffset: Int64;          // where the offset of the next directory goes
    FPageCount: Integer;
    FInPage: Boolean;
    FParams: TTiffPageParams;
    FDir: TTiffDirectory;
    FSubDirs: array of record
      Tag: Word;
      Dir: TTiffDirectory;
    end;
    FRowBytes: NativeInt;
    FRowsPerStrip: Integer;
    FRowsWritten: Integer;       // rows of the page handed over so far
    FStripRows: Integer;         // rows waiting in FStrip
    FStrip: TBytes;
    FWork: TBytes;               // copy of a strip for predictor / byte swapping
    FEncoded: TTiffOutBuffer;
    FOffsets, FByteCounts: TTiffInt64Array;
    FStripCount: Integer;
    FJpegTables: TBytes;
    FUsePredictor: Boolean;
    procedure CheckParams(const P: TTiffPageParams);
    procedure WriteStrip(Data: PByte; Rows: Integer);
    procedure BuildDirectory;
  public
    { Starts a TIFF at the current position of the stream. BigTIFF is needed
      for files over 4 GiB, see TiffChooseBigTiff. }
    constructor Create(AStream: TStream; ABigTiff: Boolean; AOwnsStream: Boolean = False);
    { Finishes the file. A page that was begun but not ended is discarded
      (its data stays in the file but no directory refers to it). }
    destructor Destroy; override;

    { Starts a new page. Raises ETiffUnsupported if the writer cannot store
      the described layout. }
    procedure BeginPage(const Params: TTiffPageParams);
    { Hands over the next RowCount rows of the current page. }
    procedure WriteRows(Buffer: Pointer; RowCount: Integer);
    { Finishes the current page: all its rows must have been written. }
    procedure EndPage;

    { Tags of the page being written. Any additional tags (description,
      software, ICC profile...) can be set between BeginPage and EndPage.
      Tags describing the pixel data are set by the writer in EndPage and
      override what is here. }
    property PageDirectory: TTiffDirectory read FDir;
    { Attaches a sub-directory to the page being written, referenced by tag
      PointerTag (TiffTagExifIFD, TiffTagGpsIFD). Dir is written in EndPage
      and must stay alive until then; the writer does not free it. }
    procedure AddSubDirectory(PointerTag: Word; Dir: TTiffDirectory);

    property Container: TTiffContainer read FContainer;
    property PageCount: Integer read FPageCount;
    { Size of a row of the current page in bytes. }
    property RowBytes: NativeInt read FRowBytes;
  end;

  { When to write BigTIFF instead of classic TIFF; same choices as option
    ImagingTiffBigTiffWriteMode of Vampyre Imaging Library. }
  TTiffBigTiffMode = (
    tbmIfNeeded,   // only when the file is certain to exceed 4 GiB
    tbmIfSafer,    // also when compression makes the final size uncertain
    tbmAlways);

{ Fills Params with defaults: 8-bit gray, no compression, no resolution. }
procedure TiffInitPageParams(out Params: TTiffPageParams);

{ Decides between classic TIFF and BigTIFF before writing starts.
  RawDataSize is the size of uncompressed pixel data of all pages. Classic
  TIFF cannot address data beyond 4 GiB and the size of compressed data is
  not known in advance, so:
    tbmIfNeeded - BigTIFF only for uncompressed data that will not fit,
    tbmIfSafer  - also when compressed data might not fit, assuming the worst
                  case for lossless compression (10% growth) and a 3:1 ratio
                  for JPEG. }
function TiffChooseBigTiff(Mode: TTiffBigTiffMode; RawDataSize: Int64;
  PageCount: Integer; Compression: Integer): Boolean;

implementation

uses
  PasTiffFax, PasTiffJpeg;

const
  { Raw size of strips chosen by the writer. libtiff aims at 8 KiB, a value
    from 1992; larger strips compress better and cost less overhead. }
  DefaultStripBytes = 256 * 1024;

procedure TiffInitPageParams(out Params: TTiffPageParams);
begin
  Params.ColorMap := nil;
  FillChar(Params, SizeOf(Params), 0);
  Params.BitsPerSample := 8;
  Params.SamplesPerPixel := 1;
  Params.SampleFormat := TiffSampleFormatUInt;
  Params.Photometric := TiffPhotometricMinIsBlack;
  Params.ExtraSample := TiffExtraSampleUnassAlpha;
  Params.Compression := TiffCompressionNone;
  Params.JpegQuality := 90;
  Params.JpegSubsampleH := 2;
  Params.JpegSubsampleV := 2;
end;

function TiffChooseBigTiff(Mode: TTiffBigTiffMode; RawDataSize: Int64;
  PageCount: Integer; Compression: Integer): Boolean;
const
  ClassicLimit: Int64 = Int64(4) * 1024 * 1024 * 1024;
  // Allowance for directories and tag values of one page
  PageOverhead = 8192;
var
  Estimate: Int64;
begin
  Result := Mode = tbmAlways;
  if Result then
    Exit;
  if Compression = TiffCompressionNone then
  begin
    Result := RawDataSize + Int64(PageCount) * PageOverhead > ClassicLimit;
    Exit;
  end;
  if Mode = tbmIfSafer then
  begin
    if Compression = TiffCompressionJpeg then
      Estimate := RawDataSize div 3
    else
      Estimate := RawDataSize + RawDataSize div 10;
    Result := Estimate + Int64(PageCount) * PageOverhead > ClassicLimit;
  end;
end;

{ TTiffWriter }

constructor TTiffWriter.Create(AStream: TStream; ABigTiff: Boolean; AOwnsStream: Boolean);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  FContainer := TTiffContainer.Create(AStream);
  FContainer.WriteHeader(ABigTiff);
  FLinkOffset := FContainer.FirstDirLinkOffset;
end;

destructor TTiffWriter.Destroy;
begin
  FDir.Free;
  FContainer.Free;
  if FOwnsStream then
    FStream.Free;
  inherited;
end;

{ Checks that the page can be written; the list of accepted layouts is in
  the unit header. }
procedure TTiffWriter.CheckParams(const P: TTiffPageParams);
var
  ColorSamples: Integer;
  Ok: Boolean;

  procedure Fail(const Msg: string);
  begin
    raise ETiffUnsupported.Create('TIFF writer: ' + Msg);
  end;

begin
  if (P.Width <= 0) or (P.Height <= 0) then
    raise ETiffError.Create('TIFF writer: invalid image size');
  ColorSamples := TiffColorSampleCount(P.Photometric);
  if not (P.Photometric in [TiffPhotometricMinIsWhite, TiffPhotometricMinIsBlack,
    TiffPhotometricRgb, TiffPhotometricPalette]) then
    Fail(Format('photometric interpretation %d is not supported', [P.Photometric]));
  if (P.SamplesPerPixel <> ColorSamples) and (P.SamplesPerPixel <> ColorSamples + 1) then
    Fail(Format('%d samples per pixel', [P.SamplesPerPixel]));

  if P.SampleFormat = TiffSampleFormatFloat then
    Ok := ((P.BitsPerSample = 16) or (P.BitsPerSample = 32)) and
      (P.Photometric in [TiffPhotometricMinIsBlack, TiffPhotometricRgb])
  else if P.SampleFormat = TiffSampleFormatUInt then
    case P.BitsPerSample of
      1: Ok := (P.SamplesPerPixel = 1) and (P.Photometric <> TiffPhotometricPalette) and
           (P.Photometric <> TiffPhotometricRgb);
      8, 16: Ok := True;
      32: Ok := (P.SamplesPerPixel = 1) and (P.Photometric = TiffPhotometricMinIsBlack);
    else
      Ok := False;
    end
  else
    Ok := False;
  if not Ok then
    Fail(Format('%d-bit samples of format %d with photometric interpretation %d',
      [P.BitsPerSample, P.SampleFormat, P.Photometric]));

  if P.Photometric = TiffPhotometricPalette then
  begin
    if P.SamplesPerPixel <> 1 then
      Fail('palette images cannot have alpha');
    if Length(P.ColorMap) <> 3 * (1 shl P.BitsPerSample) then
      raise ETiffError.Create('TIFF writer: color map does not have 3 * 2^BitsPerSample entries');
  end;

  case P.Compression of
    TiffCompressionNone, TiffCompressionPackBits, TiffCompressionLzw: ;
    TiffCompressionAdobeDeflate, TiffCompressionDeflate:
      if not TiffDeflateAvailable then
        Fail('Deflate compression is not compiled in');
    TiffCompressionCcittFax4:
      if P.BitsPerSample <> 1 then
        Fail('CCITT Group 4 compression needs 1-bit data');
    TiffCompressionJpeg:
      begin
        if not TiffJpegAvailable then
          Fail('JPEG compression is not compiled in');
        if (P.BitsPerSample <> 8) or (P.SampleFormat <> TiffSampleFormatUInt) or
           not (((P.Photometric = TiffPhotometricMinIsBlack) and (P.SamplesPerPixel = 1)) or
                ((P.Photometric = TiffPhotometricRgb) and (P.SamplesPerPixel = 3))) then
          Fail('JPEG compression needs 8-bit gray or RGB data');
        if not (((P.JpegSubsampleH = 2) and (P.JpegSubsampleV in [1, 2])) or
                ((P.JpegSubsampleH = 1) and (P.JpegSubsampleV = 1))) then
          Fail(Format('JPEG chroma subsampling %dx%d', [P.JpegSubsampleH, P.JpegSubsampleV]));
      end;
  else
    Fail(Format('compression scheme %s', [TiffCompressionName(P.Compression)]));
  end;
end;

procedure TTiffWriter.BeginPage(const Params: TTiffPageParams);
var
  Rows, Unit_: Integer;
begin
  if FInPage then
    raise ETiffError.Create('TIFF writer: previous page was not ended');
  CheckParams(Params);
  FParams := Params;
  FRowBytes := TiffMulSize((Int64(Params.Width) * Params.SamplesPerPixel * Params.BitsPerSample + 7) div 8, 1);

  Rows := Params.RowsPerStrip;
  if Rows <= 0 then
  begin
    Rows := DefaultStripBytes div FRowBytes;
    if Rows < 1 then
      Rows := 1;
  end;
  if Params.Compression = TiffCompressionJpeg then
  begin
    // A strip is a JPEG image of its own. All but the last must be made of
    // whole MCUs (blocks of 8 or 16 rows), otherwise the decoder's padding
    // rows would have nowhere to go: "RowsPerStrip must be multiple of
    // 8 * vertical subsampling" (Technical Note 2, libtiff enforces it too).
    Unit_ := 8;
    if Params.SamplesPerPixel = 3 then
      Unit_ := 8 * Params.JpegSubsampleV;
    Rows := ((Rows + Unit_ - 1) div Unit_) * Unit_;
  end;
  if Rows > Params.Height then
    Rows := Params.Height;
  FRowsPerStrip := Rows;

  FUsePredictor := Params.Predictor and (Params.SampleFormat = TiffSampleFormatUInt) and
    (Params.BitsPerSample >= 8) and
    (Params.Compression in [TiffCompressionLzw, TiffCompressionAdobeDeflate]);
  // TiffCompressionDeflate (32946) is above 255 and cannot be in the set
  if Params.Predictor and (Params.Compression = TiffCompressionDeflate) and
     (Params.SampleFormat = TiffSampleFormatUInt) and (Params.BitsPerSample >= 8) then
    FUsePredictor := True;

  FStrip := nil;
  SetLength(FStrip, TiffMulSize(FRowBytes, FRowsPerStrip));
  FStripRows := 0;
  FRowsWritten := 0;
  FStripCount := 0;
  SetLength(FOffsets, (Params.Height - 1) div FRowsPerStrip + 1);
  SetLength(FByteCounts, Length(FOffsets));
  FJpegTables := nil;
  FSubDirs := nil;
  FreeAndNil(FDir);
  FDir := TTiffDirectory.Create;
  FInPage := True;
end;

{ Compresses and writes one strip. Data is not modified. }
procedure TTiffWriter.WriteStrip(Data: PByte; Rows: Integer);
var
  Size: NativeInt;
  Stride, Components: Integer;
  Offset: Int64;
begin
  Size := FRowBytes * Rows;

  // Predictor works in place and needs a copy that can be changed. Byte
  // order needs nothing: samples are in the CPU's order, which is the
  // little-endian order of the file.
  if FUsePredictor then
  begin
    if Length(FWork) < Size then
    begin
      FWork := nil;
      SetLength(FWork, Size);
    end;
    Move(Data^, FWork[0], Size);
    Data := @FWork[0];
    TiffPredictorEncode(Data, FRowBytes, Rows, FParams.BitsPerSample, FParams.SamplesPerPixel);
  end;

  if FParams.Compression = TiffCompressionNone then
    Offset := FContainer.Append(Data, Size)
  else
  begin
    FEncoded.Clear;
    case FParams.Compression of
      TiffCompressionPackBits:
        TiffPackBitsEncode(Data, FRowBytes, Rows, FEncoded);
      TiffCompressionLzw:
        TiffLzwEncode(Data, Size, FEncoded);
      TiffCompressionAdobeDeflate, TiffCompressionDeflate:
        TiffDeflateEncode(Data, Size, FEncoded);
      TiffCompressionCcittFax4:
        TiffFax4Encode(Data, FParams.Width, Rows, FEncoded);
      TiffCompressionJpeg:
        begin
          Components := FParams.SamplesPerPixel;
          Stride := FParams.JpegSubsampleH;
          TiffJpegEncode(Components, Data, FParams.Width, Rows, FParams.JpegQuality,
            Stride, FParams.JpegSubsampleV, FJpegTables, FEncoded);
        end;
    end;
    Offset := FContainer.Append(@FEncoded.Data[0], FEncoded.Size);
    Size := FEncoded.Size;
  end;

  FOffsets[FStripCount] := Offset;
  FByteCounts[FStripCount] := Size;
  Inc(FStripCount);
end;

procedure TTiffWriter.WriteRows(Buffer: Pointer; RowCount: Integer);
var
  Src: PByte;
  N: Integer;
begin
  if not FInPage then
    raise ETiffError.Create('TIFF writer: no page was begun');
  if (RowCount < 0) or (RowCount > FParams.Height - FRowsWritten) then
    raise ETiffError.Create('TIFF writer: more rows than the page has');
  Src := Buffer;
  while RowCount > 0 do
  begin
    if (FStripRows = 0) and (RowCount >= FRowsPerStrip) then
    begin
      // A whole strip is at hand: no need to copy it to the strip buffer
      WriteStrip(Src, FRowsPerStrip);
      N := FRowsPerStrip;
    end
    else
    begin
      N := FRowsPerStrip - FStripRows;
      if N > RowCount then
        N := RowCount;
      Move(Src^, FStrip[FStripRows * FRowBytes], N * FRowBytes);
      Inc(FStripRows, N);
      // Strip is complete, or it is the last, shorter strip of the page
      if (FStripRows = FRowsPerStrip) or (FRowsWritten + N = FParams.Height) then
      begin
        WriteStrip(@FStrip[0], FStripRows);
        FStripRows := 0;
      end;
    end;
    Inc(Src, N * FRowBytes);
    Inc(FRowsWritten, N);
    Dec(RowCount, N);
  end;
end;

procedure TTiffWriter.AddSubDirectory(PointerTag: Word; Dir: TTiffDirectory);
begin
  if not FInPage then
    raise ETiffError.Create('TIFF writer: no page was begun');
  SetLength(FSubDirs, Length(FSubDirs) + 1);
  FSubDirs[High(FSubDirs)].Tag := PointerTag;
  FSubDirs[High(FSubDirs)].Dir := Dir;
end;

{ Sets all tags that describe pixel data of the page. }
procedure TTiffWriter.BuildDirectory;
var
  Bits: array of Int64;
  Map: array of Int64;
  I, ColorSamples, Photometric: Integer;
  OffsetType: Word;
  Large: Boolean;
  RefBlackWhite: array[0..5] of array[0..1] of Cardinal;
begin
  Photometric := FParams.Photometric;
  // JPEG stores RGB as YCbCr, and the tags must say what is in the stream
  if (FParams.Compression = TiffCompressionJpeg) and (FParams.SamplesPerPixel = 3) then
    Photometric := TiffPhotometricYCbCr;

  FDir.SetInt(TiffTagImageWidth, ttLong, FParams.Width);
  FDir.SetInt(TiffTagImageLength, ttLong, FParams.Height);
  SetLength(Bits, FParams.SamplesPerPixel);
  for I := 0 to High(Bits) do
    Bits[I] := FParams.BitsPerSample;
  FDir.SetIntArray(TiffTagBitsPerSample, ttShort, Bits);
  FDir.SetInt(TiffTagCompression, ttShort, FParams.Compression);
  FDir.SetInt(TiffTagPhotometric, ttShort, Photometric);
  FDir.SetInt(TiffTagSamplesPerPixel, ttShort, FParams.SamplesPerPixel);
  FDir.SetInt(TiffTagRowsPerStrip, ttLong, FRowsPerStrip);
  FDir.SetInt(TiffTagPlanarConfig, ttShort, TiffPlanarContig);

  // Offsets are LONG where possible so that the widest range of readers
  // understands them; LONG8 only exists in BigTIFF and is used when needed
  Large := False;
  for I := 0 to FStripCount - 1 do
    if (FOffsets[I] > $FFFFFFFF) or (FByteCounts[I] > $FFFFFFFF) then
      Large := True;
  if Large then OffsetType := ttLong8 else OffsetType := ttLong;
  FDir.SetIntArray(TiffTagStripOffsets, OffsetType, FOffsets);
  FDir.SetIntArray(TiffTagStripByteCounts, OffsetType, FByteCounts);

  if FParams.ResolutionUnit <> 0 then
  begin
    FDir.SetRational(TiffTagXResolution, FParams.XResolution);
    FDir.SetRational(TiffTagYResolution, FParams.YResolution);
    FDir.SetInt(TiffTagResolutionUnit, ttShort, FParams.ResolutionUnit);
  end;

  if FUsePredictor then
    FDir.SetInt(TiffTagPredictor, ttShort, TiffPredictorHorizontal)
  else
    FDir.Remove(TiffTagPredictor);

  if Photometric = TiffPhotometricPalette then
  begin
    SetLength(Map, Length(FParams.ColorMap));
    for I := 0 to High(Map) do
      Map[I] := FParams.ColorMap[I];
    FDir.SetIntArray(TiffTagColorMap, ttShort, Map);
  end;

  ColorSamples := TiffColorSampleCount(FParams.Photometric);
  if FParams.SamplesPerPixel > ColorSamples then
    FDir.SetInt(TiffTagExtraSamples, ttShort, FParams.ExtraSample)
  else
    FDir.Remove(TiffTagExtraSamples);

  // SampleFormat defaults to unsigned integer and is written only for floats
  if FParams.SampleFormat <> TiffSampleFormatUInt then
  begin
    for I := 0 to High(Bits) do
      Bits[I] := FParams.SampleFormat;
    FDir.SetIntArray(TiffTagSampleFormat, ttShort, Bits);
  end
  else
    FDir.Remove(TiffTagSampleFormat);

  if FParams.Compression = TiffCompressionJpeg then
  begin
    if Length(FJpegTables) > 0 then
      FDir.SetBytes(TiffTagJpegTables, ttUndefined, @FJpegTables[0], Length(FJpegTables));
    if Photometric = TiffPhotometricYCbCr then
    begin
      FDir.SetIntArray(TiffTagYCbCrSubSampling, ttShort, [FParams.JpegSubsampleH, FParams.JpegSubsampleV]);
      // JPEG uses the full 0..255 range for luma and chroma. The TIFF default
      // of this tag for YCbCr says the same, but libtiff writes it out
      // explicitly and some readers expect it.
      for I := 0 to 5 do
      begin
        RefBlackWhite[I][1] := 1;
        if I = 0 then
          RefBlackWhite[I][0] := 0
        else if Odd(I) then
          RefBlackWhite[I][0] := 255
        else
          RefBlackWhite[I][0] := 128;
      end;
      FDir.SetBytes(TiffTagReferenceBlackWhite, ttRational, @RefBlackWhite, 6);
    end;
  end;
end;

procedure TTiffWriter.EndPage;
var
  I: Integer;
  SubOffset, DirOffset, NextLink, Dummy: Int64;
begin
  if not FInPage then
    raise ETiffError.Create('TIFF writer: no page was begun');
  if FRowsWritten <> FParams.Height then
    raise ETiffError.CreateFmt('TIFF writer: page has %d rows but %d were written',
      [FParams.Height, FRowsWritten]);

  BuildDirectory;
  // Sub-directories go first so that their offsets can be stored in the page
  for I := 0 to High(FSubDirs) do
  begin
    SubOffset := FContainer.WriteDirectory(FSubDirs[I].Dir, Dummy);
    if FContainer.IsBigTiff then
      FDir.SetInt(FSubDirs[I].Tag, ttIFD8, SubOffset)
    else
      FDir.SetInt(FSubDirs[I].Tag, ttLong, SubOffset);
  end;
  DirOffset := FContainer.WriteDirectory(FDir, NextLink);
  // Now that the page is complete, make it reachable
  FContainer.WriteOffsetAt(FLinkOffset, DirOffset);
  FLinkOffset := NextLink;

  Inc(FPageCount);
  FInPage := False;
  FSubDirs := nil;
  FStrip := nil;
  FWork := nil;
end;

end.
