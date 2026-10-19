{
  PasTiff - pure Pascal TIFF reader/writer
  by Marek Mauder
  https://github.com/galfar/imaginglib
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
}

{ TIFF reader: pages of a file, their description, and their pixel data as
  decoded strips/tiles or as rows.

  What "decoded" means here is the same as for libtiff's TIFFReadEncodedStrip
  and TIFFReadEncodedTile: data is decompressed, the predictor is undone, and
  16/24/32/64-bit samples are in the CPU's byte order. Nothing else is
  touched: samples keep their size and meaning (palette indices stay indices,
  MinIsWhite stays inverted, CMYK stays CMYK, separate planes stay separate).
  Turning that into a handful of usual pixel layouts is the job of
  PasTiffPixels.pas. Two exceptions, both described by DataInfo:
    - FillOrder: bits of decoded data are always most significant bit first,
    - JPEG compressed YCbCr is delivered as RGB (the JPEG decoder converts).

  Terms: a "chunk" is a strip or a tile. With PlanarConfiguration=2 every
  sample has its own chunks; chunks of plane 0 come first, then plane 1...

  Reference: libtiff's tif_dirread.c (TIFFReadDirectory and the fix-ups of
  broken files it does), tif_read.c, tif_strip.c and tif_tile.c. }
unit PasTiffReader;

{$I PasTiff.inc}

interface

uses
  SysUtils, Classes, PasTiffTypes, PasTiffDirectory, PasTiffCodecs, PasTiffPixels;

const
  { Most pages (directories in the main chain) PasTiff lists in one file. }
  TiffMaxPages = 65536;

type
  TTiffReader = class
  private
    FStream: TStream;
    FOwnsStream: Boolean;
    FContainer: TTiffContainer;
    FPageOffsets: TTiffInt64Array;
    FPageIndex: Integer;
    FDir: TTiffDirectory;
    FPage: TTiffPageInfo;
    FData: TTiffPageInfo;
    FUnsupported: string;
    FTolerateDataErrors: Boolean;
    FDataErrors: Integer;

    { Layout of the current page }
    FOffsets, FByteCounts: TTiffInt64Array;
    FPlanes: Integer;            // sets of chunks: SamplesPerPixel if separate, else 1
    FChunksPerPlane: Integer;
    FTilesAcross, FTilesDown: Integer;
    FPacked: Boolean;            // subsampled YCbCr stored in blocks, see SetupLayout
    FDataRows: Integer;          // rows of data per plane, Height unless FPacked
    FDataRowsPerStrip: Integer;
    FRowBytes: NativeInt;        // bytes of one data row of the whole image (per plane)
    FTileRowBytes: NativeInt;
    FTileDataRows: Integer;

    { Buffers reused between calls }
    FRaw: TBytes;                // compressed chunk
    FChunkBuf: TBytes;           // decoded tile for ReadRows
    FBand: TBytes;               // decoded band (strip or row of tiles) for ReadRows
    FJpegTables: TBytes;         // JPEGTables tag of the current page
    FBandIndex, FBandPlane: Integer;

    { Strips being decoded in pieces, one per plane (see StreamRows) }
    FOpen: array of record
      Chunk: Integer;            // -1 if none
      RowsDone: Integer;         // data rows of the strip decoded so far
      Raw: TBytes;
      Decoder: TTiffChunkDecoder;
    end;
    FScratch: TBytes;            // rows that are decoded only to be skipped
    FCanStream: Boolean;

    procedure EnumeratePages;
    procedure ParsePage;
    procedure SetupLayout;
    procedure LoadChunkTables;
    procedure FixByteCounts(HasByteCounts: Boolean);
    procedure ChopSingleStrip;
    function RowBytesFor(WidthPx: Integer): NativeInt;
    function ChunkDataRows(Index: Integer): Integer;
    procedure CheckPage;
    procedure DataError(const Msg: string);
    procedure DecodeBand(Plane, Band: Integer; Dest: PByte);
    function AvailableBytes(Index: Integer): Int64;
    procedure PostDecode(Buffer: PByte; RowB: NativeInt; Rows: Integer);
    procedure CloseOpenStrips;
    procedure StreamRows(Plane, Band, Offs, Count: Integer; Dest: PByte);
    function GetPageCount: Integer;
    function GetIsBigTiff: Boolean;
    function GetIsBigEndian: Boolean;
  public
    { Opens TIFF at the current position of the stream and selects the first
      page. Raises ETiffFormatError if it is not a TIFF or has no readable page. }
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False);
    destructor Destroy; override;

    { Makes page Index (0 based) current. Fails only when the directory itself
      cannot be read or lacks the basic tags; a page whose data PasTiff cannot
      decode can still be selected and its tags examined (see UnsupportedReason). }
    procedure SelectPage(Index: Integer);

    { Number of chunks of the current page. }
    function ChunkCount: Integer;
    { Size of decoded data of chunk Index. Strips at the bottom of the image
      can be shorter than the others, tiles always have full size. }
    function ChunkSize(Index: Integer): NativeInt;
    { Size of the largest chunk. }
    function MaxChunkSize: NativeInt;
    { Decodes chunk Index into Buffer, which must have room for
      ChunkSize(Index) bytes. Raises ETiffUnsupported if the page cannot be
      decoded by PasTiff, ETiffFormatError if the data is damaged. }
    procedure ReadChunk(Index: Integer; Buffer: Pointer; BufferSize: NativeInt);
    { Decodes RowCount rows starting with FirstRow into Buffer, which must hold
      RowCount * RowBytes bytes. Works the same for strips and tiles. With
      separate planes, Plane selects the sample.
      Rows are "data rows", see RowCount. }
    procedure ReadRows(FirstRow, RowCount: Integer; Buffer: Pointer; Plane: Integer = 0);

    { Reads the whole current page converted to one of the layouts of
      PasTiffPixels. Converter must have been initialized with DataInfo of
      this page. Buffer receives Page.Height rows, Stride bytes apart, each
      Converter.DstRowBytes long. }
    procedure ReadPixels(var Converter: TTiffRowConverter; Buffer: Pointer; Stride: NativeInt);

    { Number of rows ReadRows can deliver. This is the image height except for
      subsampled YCbCr that is not JPEG compressed, where one data row holds
      the blocks of YCbCrSubsampleV image rows. }
    property RowCount: Integer read FDataRows;
    { Bytes of one row from ReadRows. }
    property RowBytes: NativeInt read FRowBytes;
    { Number of planes for ReadRows: SamplesPerPixel for separate planes, else 1. }
    property PlaneCount: Integer read FPlanes;

    property PageCount: Integer read GetPageCount;
    property PageIndex: Integer read FPageIndex;
    { Current page as described by its tags, with defaults filled in. }
    property Page: TTiffPageInfo read FPage;
    { Description of what ReadChunk and ReadRows return for the current page.
      Differs from Page where decoding already changed the data: FillOrder is
      always MSB first, Predictor is none, JPEG compressed YCbCr is RGB.
      This is the record to hand to PasTiffPixels. }
    property DataInfo: TTiffPageInfo read FData;
    { All tags of the current page. }
    property Directory: TTiffDirectory read FDir;
    property Container: TTiffContainer read FContainer;
    property IsBigTiff: Boolean read GetIsBigTiff;
    property IsBigEndian: Boolean read GetIsBigEndian;
    { Empty if the current page can be decoded, otherwise the reason why not
      (unsupported compression, sample layout...). }
    property UnsupportedReason: string read FUnsupported;
    { By default damaged or missing pixel data raises ETiffFormatError.
      When this is set, what can be decoded is returned, the rest of the chunk
      is filled with zeros and DataErrors is incremented: useful to rescue as
      much as possible from truncated files. }
    property TolerateDataErrors: Boolean read FTolerateDataErrors write FTolerateDataErrors;
    property DataErrors: Integer read FDataErrors;
  end;

{ True if PasTiff can decompress the compression scheme (in this build). }
function TiffCanDecompress(Compression: Integer): Boolean;

implementation

uses
  PasTiffFax, PasTiffJpeg;

const
  { Partial reads of strips decoding to more than this go through the
    resumable decoders (see StreamRows); smaller strips are decoded whole and
    kept for the following reads. }
  StreamThreshold = 1024 * 1024;

const
  { A single uncompressed strip larger than ChopThreshold is read as if it
    were made of strips of about ChopStripSize bytes, see ChopSingleStrip. }
  ChopThreshold = 4 * 1024 * 1024;
  ChopStripSize = 1024 * 1024;
  { Most chunks for which missing offsets are padded, as libtiff's limit of
    1000000 for growing strip arrays beyond what the file really has. }
  MaxPaddedChunks = 1000000;

function TiffCanDecompress(Compression: Integer): Boolean;
begin
  case Compression of
    TiffCompressionNone, TiffCompressionPackBits, TiffCompressionLzw,
    TiffCompressionCcittRle, TiffCompressionCcittRleW,
    TiffCompressionCcittFax3, TiffCompressionCcittFax4:
      Result := True;
    TiffCompressionAdobeDeflate, TiffCompressionDeflate:
      Result := TiffDeflateAvailable;
    TiffCompressionJpeg:
      Result := TiffJpegAvailable;
  else
    Result := False;
  end;
end;

function MinInt64(A, B: Int64): Int64; inline;
begin
  if A < B then Result := A else Result := B;
end;

procedure SortInt64(var A: TTiffInt64Array; L, R: Integer);
var
  I, J: Integer;
  P, T: Int64;
begin
  while L < R do
  begin
    I := L;
    J := R;
    P := A[(L + R) shr 1];
    repeat
      while A[I] < P do Inc(I);
      while A[J] > P do Dec(J);
      if I <= J then
      begin
        T := A[I]; A[I] := A[J]; A[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    // Recurse into the smaller part to keep the stack shallow
    if J - L < R - I then
    begin
      SortInt64(A, L, J);
      L := I;
    end
    else
    begin
      SortInt64(A, I, R);
      R := J;
    end;
  end;
end;

{ TTiffReader }

constructor TTiffReader.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  FPageIndex := -1;
  FBandIndex := -1;
  try
    FContainer := TTiffContainer.Create(AStream);
    FContainer.ReadHeader;
    EnumeratePages;
    SelectPage(0);
  except
    // Destructor is called automatically, but must not free a stream
    // the caller still owns the reference to when construction fails
    FOwnsStream := False;
    raise;
  end;
end;

destructor TTiffReader.Destroy;
begin
  CloseOpenStrips;
  FDir.Free;
  FContainer.Free;
  if FOwnsStream then
    FStream.Free;
  inherited;
end;

function TTiffReader.GetPageCount: Integer;
begin
  Result := Length(FPageOffsets);
end;

function TTiffReader.GetIsBigTiff: Boolean;
begin
  Result := FContainer.IsBigTiff;
end;

function TTiffReader.GetIsBigEndian: Boolean;
begin
  Result := FContainer.ByteOrder = tboBigEndian;
end;

{ Collects offsets of all directories in the main chain. Only the links are
  read here, tags of a page are read when it is selected. }
procedure TTiffReader.EnumeratePages;
var
  Sorted: TTiffInt64Array;   // offsets seen so far, ascending
  Count, L, H, M, I: Integer;
  Offset, Next: Int64;
  Seen: Boolean;
begin
  Count := 0;
  Offset := FContainer.FirstDirOffset;
  while (Offset <> 0) and (Count < TiffMaxPages) do
  begin
    // A chain that leads back to a directory already visited would never end.
    // Such files exist (libtiff's test_ifd_loop_*.tif, CVE-2022-x reports) and
    // old libtiff versions hang on them. libtiff now keeps a list of visited
    // offsets (_TIFFCheckDirNumberAndOffset) and so do we: the chain is cut
    // where it loops and pages before that stay usable.
    L := 0;
    H := Count - 1;
    Seen := False;
    while L <= H do
    begin
      M := (L + H) shr 1;
      if Sorted[M] < Offset then
        L := M + 1
      else if Sorted[M] > Offset then
        H := M - 1
      else
      begin
        Seen := True;
        Break;
      end;
    end;
    if Seen then
      Break;

    try
      Next := FContainer.ReadDirectoryLink(Offset);
    except
      on ETiffFormatError do
      begin
        // Link to a directory that is not there: the chain ends with the
        // previous page. With no page at all the file is unusable.
        if Count = 0 then
          raise;
        Break;
      end;
    end;

    if Count = Length(FPageOffsets) then
    begin
      SetLength(FPageOffsets, Count * 2 + 4);
      SetLength(Sorted, Length(FPageOffsets));
    end;
    FPageOffsets[Count] := Offset;
    for I := Count - 1 downto L do
      Sorted[I + 1] := Sorted[I];
    Sorted[L] := Offset;
    Inc(Count);
    Offset := Next;
  end;
  SetLength(FPageOffsets, Count);
  if Count = 0 then
    raise ETiffFormatError.Create('TIFF file has no image directory');
end;

procedure TTiffReader.SelectPage(Index: Integer);
var
  NewDir: TTiffDirectory;
begin
  if (Index < 0) or (Index >= Length(FPageOffsets)) then
    raise ETiffError.CreateFmt('TIFF page index %d is out of range (file has %d pages)',
      [Index, Length(FPageOffsets)]);
  NewDir := FContainer.ReadDirectory(FPageOffsets[Index]);
  FDir.Free;
  FDir := NewDir;
  FPageIndex := Index;
  FBandIndex := -1;
  CloseOpenStrips;
  FUnsupported := '';
  FOffsets := nil;
  FByteCounts := nil;
  ParsePage;
  SetupLayout;
  if FUnsupported = '' then
    LoadChunkTables;
  // JPEG strips are decoded by a library that takes whole images, and tiles
  // are small: only strips of the other schemes are worth decoding in pieces
  FCanStream := (FUnsupported = '') and not FPage.IsTiled and
    (FPage.Compression <> TiffCompressionJpeg) and (MaxChunkSize > StreamThreshold);
end;

procedure TTiffReader.CheckPage;
begin
  if FUnsupported <> '' then
    raise ETiffUnsupported.Create(FUnsupported);
end;

{ Fills FPage from the directory: TIFF defaults for missing tags plus the
  repairs libtiff's TIFFReadDirectory makes for commonly broken files. }
procedure TTiffReader.ParsePage;
var
  V: Int64;
  Values: TTiffInt64Array;
  Floats: TTiffDoubleArray;
  I, ColorSamples, MapSize: Integer;
  HasPhotometric: Boolean;
  Tag: PTiffTag;

  procedure Unsupported(const Msg: string);
  begin
    if FUnsupported = '' then
      FUnsupported := Msg;
  end;

  function RequiredSize(TagNo: Word; const Name: string): Integer;
  begin
    if not FDir.TryGetInt(TagNo, V) then
      raise ETiffFormatError.CreateFmt('TIFF directory is missing required tag %s', [Name]);
    if (V <= 0) or (V > High(Integer)) then
      raise ETiffFormatError.CreateFmt('Invalid value %d of TIFF tag %s', [V, Name]);
    Result := V;
  end;

  { Value of a per-sample tag (one value for each sample). PasTiff, like
    libtiff ("Cannot handle different values per sample"), needs them all
    equal. Writers often store just one value for all samples, which is fine. }
  function PerSample(TagNo: Word; Default: Integer; const Name: string): Integer;
  var
    J: Integer;
  begin
    Result := Default;
    Values := FDir.GetIntArray(TagNo);
    if Length(Values) = 0 then
      Exit;
    for J := 1 to High(Values) do
      if (J < FPage.SamplesPerPixel) and (Values[J] <> Values[0]) then
        Unsupported(Name + ' differs between samples');
    if (Values[0] < 0) or (Values[0] > 65535) then
      raise ETiffFormatError.CreateFmt('Invalid value %d of TIFF tag %s', [Values[0], Name]);
    Result := Values[0];
  end;

begin
  FPage.ColorMap := nil;   // release before the record is zeroed
  FillChar(FPage, SizeOf(FPage), 0);

  FPage.Width := RequiredSize(TiffTagImageWidth, 'ImageWidth');
  FPage.Height := RequiredSize(TiffTagImageLength, 'ImageLength');

  V := FDir.GetInt(TiffTagSamplesPerPixel, 1);
  if (V < 1) or (V > 65535) then
    raise ETiffFormatError.CreateFmt('Invalid SamplesPerPixel %d', [V]);
  FPage.SamplesPerPixel := V;

  FPage.BitsPerSample := PerSample(TiffTagBitsPerSample, 1, 'BitsPerSample');
  if (FPage.BitsPerSample < 1) or (FPage.BitsPerSample > 64) then
    Unsupported(Format('%d bits per sample', [FPage.BitsPerSample]));

  FPage.SampleFormat := PerSample(TiffTagSampleFormat, TiffSampleFormatUInt, 'SampleFormat');
  // "Void" means the writer does not say what the samples are; libtiff and
  // everybody else then treat them as unsigned integers.
  if FPage.SampleFormat = TiffSampleFormatVoid then
    FPage.SampleFormat := TiffSampleFormatUInt;
  if not (FPage.SampleFormat in [TiffSampleFormatUInt, TiffSampleFormatInt, TiffSampleFormatFloat]) then
    Unsupported(Format('Sample format %d', [FPage.SampleFormat]));

  FPage.Compression := FDir.GetInt(TiffTagCompression, TiffCompressionNone);

  HasPhotometric := FDir.TryGetInt(TiffTagPhotometric, V);
  if HasPhotometric then
    FPage.Photometric := V
  else
    // Photometric is a required tag but some files lack it. libtiff's RGBA
    // reader guesses from the number of samples; for fax data the only
    // sensible meaning is MinIsWhite.
    case FPage.Compression of
      TiffCompressionCcittRle, TiffCompressionCcittFax3, TiffCompressionCcittFax4,
      TiffCompressionCcittRleW:
        FPage.Photometric := TiffPhotometricMinIsWhite;
    else
      if FPage.SamplesPerPixel >= 3 then
        FPage.Photometric := TiffPhotometricRgb
      else
        FPage.Photometric := TiffPhotometricMinIsBlack;
    end;

  FPage.PlanarConfig := FDir.GetInt(TiffTagPlanarConfig, TiffPlanarContig);
  // With one sample per pixel both configurations are the same thing
  if (FPage.PlanarConfig <> TiffPlanarSeparate) or (FPage.SamplesPerPixel = 1) then
    FPage.PlanarConfig := TiffPlanarContig;

  FPage.FillOrder := FDir.GetInt(TiffTagFillOrder, TiffFillOrderMsbFirst);
  if FPage.FillOrder <> TiffFillOrderLsbFirst then
    FPage.FillOrder := TiffFillOrderMsbFirst;

  V := FDir.GetInt(TiffTagOrientation, TiffOrientationTopLeft);
  if (V < 1) or (V > 8) then
    V := TiffOrientationTopLeft;
  FPage.Orientation := V;

  FPage.Predictor := FDir.GetInt(TiffTagPredictor, TiffPredictorNone);

  // Extra samples: everything beyond what the photometric interpretation
  // needs. The count comes from SamplesPerPixel rather than from the
  // ExtraSamples tag, because files with e.g. RGB + 1 sample and no (or a
  // wrong) ExtraSamples tag are common; libtiff repairs this the same way
  // ("Sum of Photometric type-related color channels and ExtraSamples doesn't
  // match SamplesPerPixel. Defining non-color channels as ExtraSamples").
  // Samples the tag does not describe are "unspecified".
  ColorSamples := TiffColorSampleCount(FPage.Photometric);
  if (ColorSamples > 0) and (FPage.SamplesPerPixel > ColorSamples) then
  begin
    FPage.ExtraSampleCount := FPage.SamplesPerPixel - ColorSamples;
    Values := FDir.GetIntArray(TiffTagExtraSamples);
    for I := 0 to TiffMaxExtraSamples - 1 do
      if (I < Length(Values)) and (Values[I] >= 0) and (Values[I] <= 2) then
        FPage.ExtraSamples[I] := Values[I]
      else
        FPage.ExtraSamples[I] := TiffExtraSampleUnspecified;
  end;

  // Storage layout. TileWidth present means tiles (TIFFTAG_TILEWIDTH is what
  // sets TIFF_ISTILED in libtiff).
  FPage.IsTiled := FDir.HasTag(TiffTagTileWidth);
  if FPage.IsTiled then
  begin
    FPage.TileWidth := RequiredSize(TiffTagTileWidth, 'TileWidth');
    FPage.TileHeight := RequiredSize(TiffTagTileLength, 'TileLength');
  end
  else
  begin
    // Default RowsPerStrip is 2^32-1, meaning the whole image is one strip.
    // Values larger than the image height mean the same.
    V := FDir.GetInt(TiffTagRowsPerStrip, FPage.Height);
    if (V <= 0) or (V > FPage.Height) then
      V := FPage.Height;
    FPage.RowsPerStrip := V;
  end;

  FPage.ResolutionUnit := FDir.GetInt(TiffTagResolutionUnit, TiffResUnitInch);
  FPage.HasResolution := FDir.TryGetFloat(TiffTagXResolution, FPage.XResolution) and
    FDir.TryGetFloat(TiffTagYResolution, FPage.YResolution);

  if FPage.Photometric = TiffPhotometricPalette then
  begin
    Tag := FDir.Find(TiffTagColorMap);
    if (Tag = nil) or (FPage.BitsPerSample > 16) or
      (Tag.Count < 3 * (Int64(1) shl FPage.BitsPerSample)) then
    begin
      // "Verify Palette image has a Colormap" in libtiff: without one the
      // data is taken for what it most likely is.
      if (FPage.BitsPerSample >= 8) and (FPage.SamplesPerPixel = 3) then
        FPage.Photometric := TiffPhotometricRgb
      else if FPage.BitsPerSample >= 8 then
        FPage.Photometric := TiffPhotometricMinIsBlack
      else
        raise ETiffFormatError.Create('TIFF directory is missing required tag ColorMap');
    end
    else
    begin
      MapSize := 3 * (1 shl FPage.BitsPerSample);
      SetLength(FPage.ColorMap, MapSize);
      for I := 0 to MapSize - 1 do
        FPage.ColorMap[I] := Word(Tag.GetInt(I) and $FFFF);
    end;
  end;

  // YCbCr. Note the default subsampling of 2x2: a YCbCr file without the
  // YCbCrSubSampling tag is subsampled.
  FPage.YCbCrSubsampleH := 2;
  FPage.YCbCrSubsampleV := 2;
  Values := FDir.GetIntArray(TiffTagYCbCrSubSampling);
  if Length(Values) >= 2 then
  begin
    FPage.YCbCrSubsampleH := Values[0];
    FPage.YCbCrSubsampleV := Values[1];
  end;
  FPage.YCbCrCoefficients[0] := 0.299;   // ITU-R BT.601 luma weights
  FPage.YCbCrCoefficients[1] := 0.587;
  FPage.YCbCrCoefficients[2] := 0.114;
  Floats := FDir.GetFloatArray(TiffTagYCbCrCoefficients);
  if Length(Floats) >= 3 then
    for I := 0 to 2 do
      FPage.YCbCrCoefficients[I] := Floats[I];
  // ReferenceBlackWhite default for YCbCr is full range luma and chroma
  // centered at half the range (libtiff's TIFFGetFieldDefaulted)
  if FPage.BitsPerSample <= 16 then
  begin
    V := (Int64(1) shl FPage.BitsPerSample) - 1;
    FPage.ReferenceBlackWhite[0] := 0;
    FPage.ReferenceBlackWhite[1] := V;
    FPage.ReferenceBlackWhite[2] := (V + 1) div 2;
    FPage.ReferenceBlackWhite[3] := V;
    FPage.ReferenceBlackWhite[4] := (V + 1) div 2;
    FPage.ReferenceBlackWhite[5] := V;
  end;
  Floats := FDir.GetFloatArray(TiffTagReferenceBlackWhite);
  if Length(Floats) >= 6 then
    for I := 0 to 5 do
      FPage.ReferenceBlackWhite[I] := Floats[I];

  FPage.T4Options := FDir.GetInt(TiffTagT4Options, 0) and $FFFFFFFF;
  FPage.T6Options := FDir.GetInt(TiffTagT6Options, 0) and $FFFFFFFF;
  FPage.InkSet := FDir.GetInt(TiffTagInkSet, TiffInkSetCmyk);

  case FPage.Compression of
    TiffCompressionCcittRle, TiffCompressionCcittRleW,
    TiffCompressionCcittFax3, TiffCompressionCcittFax4:
      // libtiff: "Bits/sample must be 1 for Group 3/4 encoding/decoding"
      if (FPage.BitsPerSample <> 1) or
         ((FPage.SamplesPerPixel <> 1) and (FPage.PlanarConfig = TiffPlanarContig)) then
        Unsupported('CCITT compression needs 1 bit per pixel');
    TiffCompressionJpeg:
      begin
        // libtiff can be built for 12-bit JPEG as well; the JPEG library
        // behind PasTiff handles 8 bits only
        if FPage.BitsPerSample <> 8 then
          Unsupported(Format('JPEG compression with %d bits per sample', [FPage.BitsPerSample]));
        if (FPage.PlanarConfig = TiffPlanarContig) and not (FPage.SamplesPerPixel in [1, 3, 4]) then
          Unsupported(Format('JPEG compression with %d samples per pixel', [FPage.SamplesPerPixel]));
      end;
  end;
  FJpegTables := nil;
  if FPage.Compression = TiffCompressionJpeg then
    FJpegTables := FDir.GetBytes(TiffTagJpegTables);
  if not TiffCanDecompress(FPage.Compression) then
    Unsupported(Format('Compression scheme %s is not supported',
      [TiffCompressionName(FPage.Compression)]));
  if not TiffPredictorSupported(FPage.Predictor, FPage.BitsPerSample, FPage.SampleFormat) then
    Unsupported(Format('Predictor %d is not supported with %d-bit samples',
      [FPage.Predictor, FPage.BitsPerSample]));
end;

{ Bytes of one data row of an area WidthPx pixels wide (the image or a tile). }
function TTiffReader.RowBytesFor(WidthPx: Integer): NativeInt;
var
  Samples: Int64;
begin
  if FPacked then
    // See SetupLayout. Blocks at the right edge are always complete.
    Samples := Int64((WidthPx + FPage.YCbCrSubsampleH - 1) div FPage.YCbCrSubsampleH) *
      (FPage.YCbCrSubsampleH * FPage.YCbCrSubsampleV + 2)
  else if FPage.PlanarConfig = TiffPlanarContig then
    Samples := Int64(WidthPx) * FPage.SamplesPerPixel
  else
    Samples := WidthPx;
  // Rows are padded to whole bytes
  Result := TiffMulSize((Samples * FPage.BitsPerSample + 7) div 8, 1);
end;

{ Derives sizes of rows, strips and tiles of decoded data and fills FData. }
procedure TTiffReader.SetupLayout;
var
  V: Int64;

  procedure Unsupported(const Msg: string);
  begin
    if FUnsupported = '' then
      FUnsupported := Msg;
  end;

begin
  if FPage.PlanarConfig = TiffPlanarSeparate then
    FPlanes := FPage.SamplesPerPixel
  else
    FPlanes := 1;

  // Subsampled YCbCr that is not JPEG compressed has its own way of storing
  // pixels (TIFF 6.0 section 21): data is a sequence of blocks, one for each
  // group of SubsampleH x SubsampleV pixels, each holding the H*V luma
  // samples of the group followed by one Cb and one Cr. A "row" of such data
  // covers SubsampleV image rows. libtiff returns strips in this form too
  // (TIFFVStripSize has the same arithmetic) and leaves unpacking to
  // tif_getimage.c; here it is done in PasTiffPixels.
  // JPEG compressed data never has this form in PasTiff, the JPEG decoder
  // upsamples and converts to RGB.
  FPacked := (FPage.Photometric = TiffPhotometricYCbCr) and
    (FPage.Compression <> TiffCompressionJpeg) and
    (FPage.PlanarConfig = TiffPlanarContig) and
    ((FPage.YCbCrSubsampleH <> 1) or (FPage.YCbCrSubsampleV <> 1));
  if FPacked then
  begin
    if not (FPage.YCbCrSubsampleH in [1, 2, 4]) or not (FPage.YCbCrSubsampleV in [1, 2, 4]) then
      raise ETiffFormatError.CreateFmt('Invalid YCbCr subsampling %dx%d',
        [FPage.YCbCrSubsampleH, FPage.YCbCrSubsampleV]);
    if FPage.SamplesPerPixel <> 3 then
      Unsupported('Subsampled YCbCr with other than 3 samples per pixel');
    if FPage.Predictor <> TiffPredictorNone then
      Unsupported('Predictor with subsampled YCbCr');
  end;

  FRowBytes := RowBytesFor(FPage.Width);
  if FPacked then
    FDataRows := (FPage.Height + FPage.YCbCrSubsampleV - 1) div FPage.YCbCrSubsampleV
  else
    FDataRows := FPage.Height;

  FTilesAcross := 0;
  FTilesDown := 0;
  FTileRowBytes := 0;
  FTileDataRows := 0;
  FDataRowsPerStrip := 0;
  if FPage.IsTiled then
  begin
    FTilesAcross := (FPage.Width - 1) div FPage.TileWidth + 1;
    FTilesDown := (FPage.Height - 1) div FPage.TileHeight + 1;
    V := Int64(FTilesAcross) * FTilesDown;
    FTileRowBytes := RowBytesFor(FPage.TileWidth);
    FTileDataRows := FPage.TileHeight;
    if FPacked then
    begin
      if (FPage.TileWidth mod FPage.YCbCrSubsampleH <> 0) or
         (FPage.TileHeight mod FPage.YCbCrSubsampleV <> 0) then
        Unsupported('Tile size is not a multiple of YCbCr subsampling');
      FTileDataRows := FPage.TileHeight div FPage.YCbCrSubsampleV;
    end;
    // Checks that a decoded tile fits memory at all
    TiffMulSize(FTileRowBytes, FTileDataRows);
  end
  else
  begin
    FDataRowsPerStrip := FPage.RowsPerStrip;
    if FPacked then
    begin
      if (FPage.RowsPerStrip mod FPage.YCbCrSubsampleV <> 0) and
         (FPage.RowsPerStrip < FPage.Height) then
        Unsupported('RowsPerStrip is not a multiple of YCbCr subsampling');
      FDataRowsPerStrip := (FPage.RowsPerStrip + FPage.YCbCrSubsampleV - 1) div FPage.YCbCrSubsampleV;
    end;
    V := (Int64(FDataRows) - 1) div FDataRowsPerStrip + 1;
    TiffMulSize(FRowBytes, FDataRowsPerStrip);
  end;
  if V > High(Integer) div FPlanes then
    raise ETiffFormatError.Create('TIFF page has too many strips or tiles');
  FChunksPerPlane := V;

  FData := FPage;
  FData.FillOrder := TiffFillOrderMsbFirst;
  FData.Predictor := TiffPredictorNone;
  if (FPage.Compression = TiffCompressionJpeg) and (FPage.Photometric = TiffPhotometricYCbCr) then
  begin
    FData.Photometric := TiffPhotometricRgb;
    FData.YCbCrSubsampleH := 1;
    FData.YCbCrSubsampleV := 1;
  end;
end;

{ Reads offsets and sizes of all chunks and repairs what can be repaired. }
procedure TTiffReader.LoadChunkTables;
var
  Expected: Int64;
  OffsetsTag, CountsTag: Word;
  I, Have: Integer;
  Counts: TTiffInt64Array;
begin
  Expected := Int64(FChunksPerPlane) * FPlanes;

  // Tiled files should use TileOffsets/TileByteCounts and stripped ones
  // StripOffsets/StripByteCounts, but there are tiled files with the strip
  // tags (libtiff treats the two pairs as one field), so whichever is
  // present is used.
  if FPage.IsTiled = FDir.HasTag(TiffTagTileOffsets) then
  begin
    if FPage.IsTiled then OffsetsTag := TiffTagTileOffsets else OffsetsTag := TiffTagStripOffsets;
  end
  else
    if FPage.IsTiled then OffsetsTag := TiffTagStripOffsets else OffsetsTag := TiffTagTileOffsets;
  if OffsetsTag = TiffTagTileOffsets then
    CountsTag := TiffTagTileByteCounts
  else
    CountsTag := TiffTagStripByteCounts;
  if not FDir.HasTag(CountsTag) then
    if CountsTag = TiffTagTileByteCounts then
      CountsTag := TiffTagStripByteCounts
    else
      CountsTag := TiffTagTileByteCounts;

  FOffsets := FDir.GetIntArray(OffsetsTag);
  Have := Length(FOffsets);
  if Have = 0 then
    raise ETiffFormatError.Create('TIFF directory is missing required tag StripOffsets');
  // Fewer offsets than chunks: libtiff pads the array with zeros (and warns),
  // so the image can be read as far as its data goes. The padding is limited,
  // otherwise a tiny file claiming a huge image would make us allocate
  // gigabytes for the tables.
  if Have < Expected then
  begin
    if Expected > MaxPaddedChunks then
      raise ETiffFormatError.CreateFmt('TIFF page needs %d strips or tiles but has only %d',
        [Expected, Have]);
  end;
  SetLength(FOffsets, Expected);
  for I := Have to Expected - 1 do
    FOffsets[I] := 0;

  Counts := FDir.GetIntArray(CountsTag);
  Have := Length(Counts);
  SetLength(FByteCounts, Expected);
  for I := 0 to Expected - 1 do
    if I < Have then
      FByteCounts[I] := Counts[I]
    else
      FByteCounts[I] := 0;

  FixByteCounts(Have > 0);
  ChopSingleStrip;
end;

{ Replaces StripByteCounts that are missing or cannot be right by estimates.

  Conditions are those of libtiff's TIFFReadDirectory:
   - the tag is missing ("Some manufacturers violate the spec by not giving
     the size of the strips"),
   - a single strip whose byte count is zero, or reaches beyond the end of
     the file, or is smaller than the uncompressed image ("Plexus (and others)
     sometimes give a value of zero for a tag when they don't know what the
     correct value is"),
   - uncompressed data in more than two strips where the first two counts
     differ ("Some vendors fill StripByteCount array with absolutely wrong
     values (it can be equal to StripOffset array, for example)").

  The estimate differs from libtiff's EstimateStripByteCounts for compressed
  data: libtiff gives every strip all the file space not taken by the
  directory, we give each chunk the space up to the next chunk (or the end of
  the file). It is never smaller than the real size and it also works when
  the tag is missing in a file with many strips, which libtiff refuses. }
procedure TTiffReader.FixByteCounts(HasByteCounts: Boolean);
var
  Bad, Uncompressed: Boolean;
  I, N, L, H, M: Integer;
  Sorted: TTiffInt64Array;
  FileSize, Next: Int64;
begin
  N := Length(FOffsets);
  FileSize := FContainer.Size;
  Uncompressed := FPage.Compression = TiffCompressionNone;

  Bad := not HasByteCounts;
  if not Bad and (FChunksPerPlane = 1) and not FPage.IsTiled then
    Bad := ((FByteCounts[0] = 0) and (FOffsets[0] <> 0)) or
      (Uncompressed and (FOffsets[0] <= FileSize) and (FByteCounts[0] > FileSize - FOffsets[0])) or
      (Uncompressed and (FByteCounts[0] < ChunkSize(0)));
  if not Bad and Uncompressed and (FPage.PlanarConfig = TiffPlanarContig) and (N > 2) then
    Bad := (FByteCounts[0] <> FByteCounts[1]) and (FByteCounts[0] <> 0) and (FByteCounts[1] <> 0);
  if not Bad then
    Exit;

  if Uncompressed then
  begin
    for I := 0 to N - 1 do
    begin
      FByteCounts[I] := ChunkSize(I);
      if (FOffsets[I] > 0) and (FOffsets[I] < FileSize) and (FByteCounts[I] > FileSize - FOffsets[I]) then
        FByteCounts[I] := FileSize - FOffsets[I];
    end;
    Exit;
  end;

  Sorted := Copy(FOffsets, 0, N);
  SortInt64(Sorted, 0, N - 1);
  for I := 0 to N - 1 do
  begin
    FByteCounts[I] := 0;
    if (FOffsets[I] <= 0) or (FOffsets[I] >= FileSize) then
      Continue;
    // First offset greater than this one
    L := 0;
    H := N;
    while L < H do
    begin
      M := (L + H) shr 1;
      if Sorted[M] <= FOffsets[I] then
        L := M + 1
      else
        H := M;
    end;
    if L < N then
      Next := MinInt64(Sorted[L], FileSize)
    else
      Next := FileSize;
    FByteCounts[I] := Next - FOffsets[I];
  end;
end;

{ "Some manufacturers make life difficult by writing large amounts of
  uncompressed data as a single strip. This is contrary to the
  recommendations of the spec." (libtiff, ChopUpSingleUncompressedStrip)

  Reading such an image as one chunk needs a buffer for the whole image. As
  the data is not compressed, any group of rows can be read on its own, so
  the page is presented as if it had strips of about ChopStripSize bytes.
  RowsPerStrip of Page changes accordingly, as it does in libtiff (which
  chops into 8 KiB strips; our threshold and strip size are larger, and unlike
  libtiff this also works for separate planes). }
procedure TTiffReader.ChopSingleStrip;
var
  Plane, S, Strips, Rows: Integer;
  StripBytes: Int64;
  NewOffsets, NewCounts: TTiffInt64Array;
begin
  if FPage.IsTiled or (FPage.Compression <> TiffCompressionNone) or
     (FChunksPerPlane <> 1) or (Int64(FRowBytes) * FDataRows <= ChopThreshold) then
    Exit;
  // Only when the data is really all there, damaged files keep their one strip
  for Plane := 0 to FPlanes - 1 do
    if (FOffsets[Plane] <= 0) or (FByteCounts[Plane] < Int64(FRowBytes) * FDataRows) then
      Exit;

  Rows := ChopStripSize div FRowBytes;
  if Rows < 1 then
    Rows := 1;
  Strips := (FDataRows - 1) div Rows + 1;
  StripBytes := Int64(Rows) * FRowBytes;
  SetLength(NewOffsets, Strips * FPlanes);
  SetLength(NewCounts, Strips * FPlanes);
  for Plane := 0 to FPlanes - 1 do
    for S := 0 to Strips - 1 do
    begin
      NewOffsets[Plane * Strips + S] := FOffsets[Plane] + S * StripBytes;
      if S < Strips - 1 then
        NewCounts[Plane * Strips + S] := StripBytes
      else
        NewCounts[Plane * Strips + S] := Int64(FDataRows - S * Rows) * FRowBytes;
    end;
  FOffsets := NewOffsets;
  FByteCounts := NewCounts;
  FChunksPerPlane := Strips;
  FDataRowsPerStrip := Rows;
  if FPacked then
    FPage.RowsPerStrip := Rows * FPage.YCbCrSubsampleV
  else
    FPage.RowsPerStrip := Rows;
  FData.RowsPerStrip := FPage.RowsPerStrip;
end;

function TTiffReader.ChunkCount: Integer;
begin
  Result := FChunksPerPlane * FPlanes;
end;

{ Number of data rows in strip Index (the last strip of a plane can be shorter). }
function TTiffReader.ChunkDataRows(Index: Integer): Integer;
var
  First: Int64;
begin
  if FPage.IsTiled then
    Result := FTileDataRows
  else
  begin
    First := Int64(Index mod FChunksPerPlane) * FDataRowsPerStrip;
    Result := FDataRowsPerStrip;
    if Result > FDataRows - First then
      Result := FDataRows - First;
  end;
end;

function TTiffReader.ChunkSize(Index: Integer): NativeInt;
begin
  if (Index < 0) or (Index >= ChunkCount) then
    raise ETiffError.CreateFmt('TIFF strip/tile index %d is out of range', [Index]);
  if FPage.IsTiled then
    Result := FTileRowBytes * FTileDataRows
  else
    Result := FRowBytes * ChunkDataRows(Index);
end;

function TTiffReader.MaxChunkSize: NativeInt;
begin
  // The first chunk is never shorter than any other
  Result := ChunkSize(0);
end;

procedure TTiffReader.DataError(const Msg: string);
begin
  if not FTolerateDataErrors then
    raise ETiffFormatError.Create(Msg);
  Inc(FDataErrors);
end;

procedure TTiffReader.ReadChunk(Index: Integer; Buffer: Pointer; BufferSize: NativeInt);
var
  Need, Got, RowB: NativeInt;
  Offset, Count, Available: Int64;
  Rows, ChunkWidth, MaxRows: Integer;
  Tables: PByte;
begin
  CheckPage;
  Need := ChunkSize(Index);
  if BufferSize < Need then
    raise ETiffError.CreateFmt('Buffer for TIFF strip/tile %d is too small', [Index]);
  if FPage.IsTiled then
  begin
    RowB := FTileRowBytes;
    ChunkWidth := FPage.TileWidth;
  end
  else
  begin
    RowB := FRowBytes;
    ChunkWidth := FPage.Width;
  end;
  Rows := ChunkDataRows(Index);

  Got := 0;
  Offset := FOffsets[Index];
  Count := FByteCounts[Index];
  Available := AvailableBytes(Index);

  if (Available <= 0) or (Available < Count) and (FPage.Compression <> TiffCompressionNone) and
     not FTolerateDataErrors then
  begin
    if Available <= 0 then
      DataError(Format('TIFF strip/tile %d has no data in the file', [Index]))
    else
      DataError(Format('TIFF strip/tile %d reaches beyond the end of the file', [Index]));
  end
  else if FPage.Compression = TiffCompressionNone then
  begin
    // Uncompressed data is read straight to the caller's buffer. A strip can
    // have more bytes than needed (padding), the excess is ignored.
    Got := MinInt64(Available, Need);
    FContainer.ReadAt(Offset, Buffer, Got);
    if FPage.FillOrder = TiffFillOrderLsbFirst then
      TiffReverseBits(Buffer, Got);
  end
  else
  begin
    if Length(FRaw) < Available then
    begin
      FRaw := nil;
      SetLength(FRaw, Available);
    end;
    FContainer.ReadAt(Offset, @FRaw[0], Available);
    // FillOrder=2 means bits of every byte of the file data are reversed.
    // libtiff undoes that on the raw (still compressed) data for all codecs
    // except the CCITT ones, which read bits in either order, and JPEG, where
    // FillOrder has no meaning. Our CCITT decoder reads one order only, so
    // its input is reversed here too.
    if (FPage.FillOrder = TiffFillOrderLsbFirst) and (FPage.Compression <> TiffCompressionJpeg) then
      TiffReverseBits(@FRaw[0], Available);
    case FPage.Compression of
      TiffCompressionPackBits:
        Got := TiffPackBitsDecode(@FRaw[0], Available, Buffer, Need);
      TiffCompressionLzw:
        Got := TiffLzwDecode(@FRaw[0], Available, Buffer, Need);
      TiffCompressionAdobeDeflate, TiffCompressionDeflate:
        Got := TiffDeflateDecode(@FRaw[0], Available, Buffer, Need);
      TiffCompressionCcittRle, TiffCompressionCcittRleW,
      TiffCompressionCcittFax3, TiffCompressionCcittFax4:
        Got := TiffFaxDecode(FPage.Compression, FPage.T4Options, @FRaw[0], Available,
          Buffer, ChunkWidth, Rows);
      TiffCompressionJpeg:
        begin
          Tables := nil;
          if Length(FJpegTables) > 0 then
            Tables := @FJpegTables[0];
          if FPage.IsTiled then
            MaxRows := FPage.TileHeight
          else
            MaxRows := FPage.RowsPerStrip;
          Got := TiffJpegDecode(FPage.Photometric, FPage.SamplesPerPixel div FPlanes,
            Tables, Length(FJpegTables), @FRaw[0], Available, Buffer, ChunkWidth, Rows, MaxRows);
          // JPEG data needs neither predictor nor byte swapping
          if Got < Need then
            DataError(Format('TIFF strip/tile %d is damaged: JPEG data could not be decoded', [Index]));
          Exit;
        end;
    end;
  end;

  if Got < Need then
  begin
    if Available > 0 then
      DataError(Format('TIFF strip/tile %d is damaged: only %d of %d bytes could be decoded',
        [Index, Int64(Got), Int64(Need)]));
    FillChar((PByte(Buffer) + Got)^, Need - Got, 0);
  end;

  PostDecode(Buffer, RowB, Rows);
end;

{ How much of chunk Index really is in the file. Checked before any buffer is
  sized by the byte count: StripByteCounts comes from the file and must not be
  able to make us allocate more than the file can hold. }
function TTiffReader.AvailableBytes(Index: Integer): Int64;
var
  Offset: Int64;
begin
  Result := 0;
  Offset := FOffsets[Index];
  if (Offset > 0) and (Offset < FContainer.Size) then
    Result := MinInt64(FByteCounts[Index], FContainer.Size - Offset);
end;

{ Predictor and byte order of Rows decoded rows. Samples of 16, 24, 32 and
  64 bits are stored as integers (or floats) in the byte order of the file;
  other sizes are bit strings with no byte order (libtiff: _TIFFSwab16BitData
  and friends are installed as post-decode methods only for these sizes).
  Both work row by row, so a strip can be finished in pieces. }
procedure TTiffReader.PostDecode(Buffer: PByte; RowB: NativeInt; Rows: Integer);
var
  Stride, BytesPerSample: Integer;
begin
  BytesPerSample := 0;
  if FPage.BitsPerSample in [16, 24, 32, 64] then
    BytesPerSample := FPage.BitsPerSample div 8;
  if FPage.PlanarConfig = TiffPlanarContig then
    Stride := FPage.SamplesPerPixel
  else
    Stride := 1;
  if FPage.Predictor <> TiffPredictorNone then
    TiffPredictorDecode(FPage.Predictor, Buffer, RowB, Rows, FPage.BitsPerSample, Stride,
      FContainer.NeedsSwap)
  else if FContainer.NeedsSwap and (BytesPerSample > 0) then
    TiffSwapBytes(Buffer, BytesPerSample, (RowB * Rows) div BytesPerSample);
end;

procedure TTiffReader.CloseOpenStrips;
var
  I: Integer;
begin
  for I := 0 to High(FOpen) do
    FOpen[I].Decoder.Free;
  FOpen := nil;
end;

{ Count rows of strip Band of a plane starting at row Offs of the strip,
  decoded straight to Dest.

  Large strips are not decoded whole for this (images of gigabytes written as
  one strip are common, Imaging and libtiff both write them): the strip stays
  open with its decoder, and a read that continues where the previous one
  stopped just decodes further. Rows skipped are decoded to a small scratch
  buffer; going back restarts the strip. Uncompressed rows are simply read
  from the file. Raw data of the open strip is held in memory, which for
  compressed data is usually a fraction of the decoded size. }
procedure TTiffReader.StreamRows(Plane, Band, Offs, Count: Integer; Dest: PByte);
var
  Index, N, ScratchRows: Integer;
  Available, Pos: Int64;
  Need, Got: NativeInt;
  Data: PByte;
  Skip: Boolean;
begin
  Index := Plane * FChunksPerPlane + Band;
  Available := AvailableBytes(Index);

  if FPage.Compression = TiffCompressionNone then
  begin
    Need := FRowBytes * Count;
    Got := 0;
    Pos := Int64(Offs) * FRowBytes;
    if Available <= 0 then
      DataError(Format('TIFF strip/tile %d has no data in the file', [Index]))
    else if Pos < Available then
    begin
      Got := MinInt64(Available - Pos, Need);
      FContainer.ReadAt(FOffsets[Index] + Pos, Dest, Got);
      if FPage.FillOrder = TiffFillOrderLsbFirst then
        TiffReverseBits(Dest, Got);
    end;
    if Got < Need then
    begin
      if Available > 0 then
        DataError(Format('TIFF strip/tile %d is damaged: only %d of %d bytes could be read',
          [Index, Int64(Got), Int64(Need)]));
      FillChar(Dest[Got], Need - Got, 0);
    end;
    PostDecode(Dest, FRowBytes, Count);
    Exit;
  end;

  if Length(FOpen) <> FPlanes then
  begin
    CloseOpenStrips;
    SetLength(FOpen, FPlanes);
    for N := 0 to FPlanes - 1 do
      FOpen[N].Chunk := -1;
  end;

  with FOpen[Plane] do
  begin
    if (Chunk <> Index) or (Offs < RowsDone) then
    begin
      // Start the strip (again)
      Chunk := -1;
      if (Available <= 0) or (Available < FByteCounts[Index]) and not FTolerateDataErrors then
      begin
        if Available <= 0 then
          DataError(Format('TIFF strip/tile %d has no data in the file', [Index]))
        else
          DataError(Format('TIFF strip/tile %d reaches beyond the end of the file', [Index]));
        Available := 0;
      end;
      Raw := nil;
      SetLength(Raw, Available);
      if Available > 0 then
      begin
        FContainer.ReadAt(FOffsets[Index], @Raw[0], Available);
        // See ReadChunk about FillOrder
        if FPage.FillOrder = TiffFillOrderLsbFirst then
          TiffReverseBits(@Raw[0], Available);
      end;
      if Decoder = nil then
        case FPage.Compression of
          TiffCompressionPackBits: Decoder := TTiffPackBitsDecoder.Create;
          TiffCompressionLzw: Decoder := TTiffLzwDecoder.Create;
          TiffCompressionAdobeDeflate, TiffCompressionDeflate: Decoder := TTiffDeflateDecoder.Create;
        else
          Decoder := TTiffFaxDecoder.Create(FPage.Compression, FPage.T4Options, FPage.Width);
        end;
      if Available > 0 then
        Decoder.Start(@Raw[0], Available)
      else
        Decoder.Start(nil, 0);
      Chunk := Index;
      RowsDone := 0;
    end;

    // Rows between the end of the previous read and Offs, then the wanted ones
    ScratchRows := StreamThreshold div FRowBytes;
    if ScratchRows < 1 then
      ScratchRows := 1;
    while RowsDone < Offs + Count do
    begin
      Skip := RowsDone < Offs;
      if Skip then
      begin
        N := Offs - RowsDone;
        if N > ScratchRows then
          N := ScratchRows;
        if Length(FScratch) < N * FRowBytes then
        begin
          FScratch := nil;
          SetLength(FScratch, N * FRowBytes);
        end;
        Data := @FScratch[0];
      end
      else
      begin
        N := Offs + Count - RowsDone;
        Data := Dest + NativeInt(RowsDone - Offs) * FRowBytes;
      end;

      Need := FRowBytes * N;
      Got := Decoder.Decode(Data, Need);
      if Got < Need then
      begin
        // Damaged data: an error, or zeros with TolerateDataErrors (then also
        // for the rest of the strip, the decoder has nothing more to give)
        if not Skip then
          DataError(Format('TIFF strip/tile %d is damaged: rows from %d on could not be decoded',
            [Index, RowsDone + Got div FRowBytes]));
        FillChar(Data[Got], Need - Got, 0);
      end;
      if not Skip then
        PostDecode(Data, FRowBytes, N);
      Inc(RowsDone, N);
    end;
  end;
end;

{ Decodes one band (a strip, or a row of tiles put side by side) of a plane
  to Dest, which holds the band's rows of FRowBytes each. }
procedure TTiffReader.DecodeBand(Plane, Band: Integer; Dest: PByte);
var
  Col, Row, Rows, First: Integer;
  TileSize, CopyBytes: NativeInt;
begin
  First := Plane * FChunksPerPlane;
  if not FPage.IsTiled then
  begin
    ReadChunk(First + Band, Dest, ChunkSize(First + Band));
    Exit;
  end;

  // Tiles of a band are joined at byte boundaries, so a tile row must end at
  // one. The specification guarantees it (tile width is a multiple of 16).
  if (FTilesAcross > 1) and not FPacked and
     ((Int64(FPage.TileWidth) * FPage.BitsPerSample * (FPage.SamplesPerPixel div FPlanes)) mod 8 <> 0) then
    raise ETiffUnsupported.Create('Tile rows do not end at byte boundary');

  TileSize := FTileRowBytes * FTileDataRows;
  if Length(FChunkBuf) < TileSize then
  begin
    FChunkBuf := nil;
    SetLength(FChunkBuf, TileSize);
  end;
  // Tiles at the bottom and right edges are stored in full size, the part
  // outside the image is padding and is dropped here.
  Rows := FTileDataRows;
  if Rows > FDataRows - Band * FTileDataRows then
    Rows := FDataRows - Band * FTileDataRows;
  for Col := 0 to FTilesAcross - 1 do
  begin
    ReadChunk(First + Band * FTilesAcross + Col, @FChunkBuf[0], TileSize);
    CopyBytes := FTileRowBytes;
    if CopyBytes > FRowBytes - Col * FTileRowBytes then
      CopyBytes := FRowBytes - Col * FTileRowBytes;
    for Row := 0 to Rows - 1 do
      Move(FChunkBuf[Row * FTileRowBytes], (Dest + Row * FRowBytes + Col * FTileRowBytes)^, CopyBytes);
  end;
end;

procedure TTiffReader.ReadRows(FirstRow, RowCount: Integer; Buffer: Pointer; Plane: Integer);
var
  Dest: PByte;
  Row, Remaining, BandRows, Band, BandFirst, RowsInBand, Offs, N: Integer;
begin
  CheckPage;
  if (FirstRow < 0) or (RowCount < 0) or (FirstRow > FDataRows - RowCount) then
    raise ETiffError.CreateFmt('TIFF rows %d..%d are out of range', [FirstRow, FirstRow + RowCount - 1]);
  if (Plane < 0) or (Plane >= FPlanes) then
    raise ETiffError.CreateFmt('TIFF plane %d is out of range', [Plane]);

  if FPage.IsTiled then
    BandRows := FTileDataRows
  else
    BandRows := FDataRowsPerStrip;
  Dest := Buffer;
  Row := FirstRow;
  Remaining := RowCount;
  while Remaining > 0 do
  begin
    Band := Row div BandRows;
    BandFirst := Band * BandRows;
    RowsInBand := BandRows;
    if RowsInBand > FDataRows - BandFirst then
      RowsInBand := FDataRows - BandFirst;
    Offs := Row - BandFirst;
    N := RowsInBand - Offs;
    if N > Remaining then
      N := Remaining;

    if (Offs = 0) and (N = RowsInBand) then
      // Whole band is wanted: decode straight to the caller's buffer
      DecodeBand(Plane, Band, Dest)
    else if FCanStream then
      StreamRows(Plane, Band, Offs, N, Dest)
    else
    begin
      // Part of a band: decode it to a buffer that is kept, callers reading
      // row by row then get the following rows without decoding again
      if (FBandIndex <> Band) or (FBandPlane <> Plane) then
      begin
        FBandIndex := -1;
        if Length(FBand) < RowsInBand * FRowBytes then
        begin
          FBand := nil;
          SetLength(FBand, RowsInBand * FRowBytes);
        end;
        DecodeBand(Plane, Band, @FBand[0]);
        FBandIndex := Band;
        FBandPlane := Plane;
      end;
      Move(FBand[Offs * FRowBytes], Dest^, N * FRowBytes);
    end;

    Inc(Dest, N * FRowBytes);
    Inc(Row, N);
    Dec(Remaining, N);
  end;
end;

procedure TTiffReader.ReadPixels(var Converter: TTiffRowConverter; Buffer: Pointer; Stride: NativeInt);
const
  // Rows are converted in groups of about this many bytes of decoded data
  BatchBytes = 1024 * 1024;
var
  Temp: TBytes;
  Planes: array of Pointer;
  Batch, Row, N, P, Written, Done: Integer;
  Dest: PByte;
begin
  CheckPage;
  if (Converter.SrcRowBytes <> FRowBytes) or (Converter.PlaneCount <> FPlanes) then
    raise ETiffError.Create('Row converter was not initialized for the current TIFF page');

  // Nothing to convert and rows are laid out as the caller wants them:
  // decode straight to the destination
  if Converter.PassThrough and (Stride = FRowBytes) and (FPlanes = 1) then
  begin
    ReadRows(0, FDataRows, Buffer);
    Exit;
  end;

  // Whole strips or rows of tiles at a time where possible, so ReadRows
  // does not have to keep partial bands
  if FPage.IsTiled then
    Batch := FTileDataRows
  else
    Batch := FDataRowsPerStrip;
  if Int64(Batch) * FRowBytes > BatchBytes then
  begin
    Batch := BatchBytes div FRowBytes;
    if Batch < 1 then
      Batch := 1;
  end;
  SetLength(Temp, TiffMulSize(TiffMulSize(Batch, FRowBytes), FPlanes));
  SetLength(Planes, FPlanes);
  for P := 0 to FPlanes - 1 do
    Planes[P] := @Temp[P * Batch * FRowBytes];

  Dest := Buffer;
  Written := 0;
  Row := 0;
  while Row < FDataRows do
  begin
    N := Batch;
    if N > FDataRows - Row then
      N := FDataRows - Row;
    for P := 0 to FPlanes - 1 do
      ReadRows(Row, N, Planes[P], P);
    Done := Converter.ConvertRows(Planes, Dest, Stride, N, FPage.Height - Written);
    Inc(Written, Done);
    Inc(Dest, Done * Stride);
    Inc(Row, N);
  end;
end;

end.
