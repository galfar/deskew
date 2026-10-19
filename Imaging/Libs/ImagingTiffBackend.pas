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

{ This unit selects the TIFF codec used by Imaging and wraps it in a small
  API that works with pages, pixel rows and Imaging's IO functions.
  ImagingTiff.pas builds the file format on top of it; no libtiff or PasTiff
  types are visible outside this unit.

  Two implementations are available:

  - PasTiff, pure Pascal TIFF reader and writer (PasTiff*.pas units here).
    Default, needs FPC or Delphi 2009+. Define TIFF_NO_BUNDLED to leave it
    out (libtiff only then).

  - libtiff 4.0+ shared library (DLL/SO/dylib), opt-in with define
    TIFF_USE_LIBTIFF_DYNLIB. When compiled in and found at run time it does
    all reading and writing, PasTiff is the fallback. It is the only option
    for Delphi older than 2009 and is used there without the define.

  The defines are listed in ImagingOptions.inc, set them there, in project
  options, or in ImagingUserOptions.inc.

  Pixels of a page are always delivered in one of the layouts of
  TTiffDataLayout, the same with both backends: libtiff only decodes strips
  and tiles, conversion of the sample layout to these is done by
  PasTiffPixels in both cases. Delphi older than 2009 has no PasTiffPixels,
  so pages are read there only when their layout is already one of these
  (plus MinIsWhite gray), everything else goes through libtiff's
  TIFFReadRGBAImage (8-bit RGBA). Pages that only libtiff decodes but
  PasTiffPixels cannot convert (CIE L*a*b*, LogLuv, old-style JPEG) go that
  way too. }
unit ImagingTiffBackend;

{$I ImagingOptions.inc}

{$IF not Defined(TIFF_NO_BUNDLED) and
  ((Defined(DCC) and (CompilerVersion >= 20)) or Defined(FPC))}
  {$DEFINE TIFF_USE_PASTIFF}
{$IFEND}

// libtiff is the only backend for old Delphi (and with TIFF_NO_BUNDLED)
{$IFNDEF TIFF_USE_PASTIFF}
  {$DEFINE TIFF_USE_LIBTIFF_DYNLIB}
{$ENDIF}

// LibTiffDynLib unit can be used only on targets with dynamic libraries
{$IF Defined(TIFF_USE_LIBTIFF_DYNLIB) and Defined(HAS_DYNLIBS)}
  {$DEFINE TIFF_USE_LIBTIFF}
{$IFEND}

interface

uses
  SysUtils, ImagingTypes, Imaging;

type
  { TIFF codec implementation in use. }
  TTiffBackend = (
    tbNone,     // nothing available, TIFF can't be loaded nor saved
    tbPasTiff,  // PasTiff, pure Pascal
    tbLibTiff   // libtiff shared library
  );

  ETiffBackendError = class(Exception);

  { Pixel layouts a page is delivered in. Samples are in TIFF order (R, G,
    B, A), 16/32-bit samples in the CPU's byte order, rows padded to whole
    bytes. Same order as TTiffPixelLayout of PasTiffPixels. }
  TTiffDataLayout = (
    tdlUnknown,        // page can't be loaded
    tdlBilevel,        // 1 bit per pixel, most significant bit first, 0 = black
    tdlGray8,
    tdlGray16,
    tdlGray32,         // 32-bit unsigned integer
    tdlGrayAlpha8,
    tdlGrayAlpha16,
    tdlIndexed8,       // palette from ReadPalette
    tdlRgb8,
    tdlRgb16,
    tdlRgba8,
    tdlRgba16,
    tdlGrayFloat16,    // IEEE half float
    tdlGrayFloat32,
    tdlRgbaFloat16,
    tdlRgbaFloat32);

  { Description of a page as stored in the file (tag values, with defaults
    for missing tags). }
  TTiffPageDesc = record
    Width, Height: Integer;
    BitsPerSample: Integer;
    SamplesPerPixel: Integer;
    SampleFormat: Integer;
    Photometric: Integer;
    PlanarConfig: Integer;
    Compression: Integer;     // value of Compression tag
    Orientation: Integer;     // 1..8, pixels are never reoriented
    HasResolution: Boolean;
    XResolution, YResolution: Double;
    ResolutionUnit: Integer;  // 1 none, 2 inch, 3 centimeter
  end;

  { Reads pages of a TIFF file. Create with TiffOpenReader. }
  TTiffFileReader = class
  protected
    FPageCount: Integer;
    FPage: TTiffPageDesc;
    FLayout: TTiffDataLayout;
    FUnsupportedReason: string;
  public
    { Makes page Index (0 based) current. }
    procedure SelectPage(Index: Integer); virtual; abstract;
    { Reads all rows of the current page in Layout into Dst, Stride bytes
      apart. Damaged pixel data does not fail: what can be decoded is
      returned, the rest is zeros. }
    procedure ReadPixels(Dst: Pointer; Stride: NativeInt); virtual; abstract;
    { Palette of the current page for tdlIndexed8, returns number of colors
      written to Pal (at most 256, alpha is 255). }
    function ReadPalette(Pal: PPalette32): Integer; virtual; abstract;

    property PageCount: Integer read FPageCount;
    property Page: TTiffPageDesc read FPage;
    { Layout ReadPixels delivers for the current page. }
    property Layout: TTiffDataLayout read FLayout;
    { Why the current page can't be loaded when Layout is tdlUnknown. }
    property UnsupportedReason: string read FUnsupportedReason;
  end;

  { Description of a page to write. Data given to WriteRows is in TIFF
    layout (see TTiffDataLayout): 1-bit bilevel, or 8/16/32-bit samples
    gray, gray+alpha, R,G,B, R,G,B,A, or 8-bit palette index. }
  TTiffSaveParams = record
    Width, Height: Integer;
    BitsPerSample: Integer;   // 1, 8, 16 or 32
    SamplesPerPixel: Integer; // color samples plus optional alpha
    SampleFormat: Integer;    // 1 unsigned integer, 3 floating point
    Photometric: Integer;     // 1 MinIsBlack, 2 RGB, 3 palette
    { Value of TIFF Compression tag: 1 none, 5 LZW, 32773 PackBits,
      8 Deflate, 7 JPEG (8-bit gray and RGB only), 4 Group 4 (1-bit only). }
    Compression: Integer;
    JpegQuality: Integer;     // 1..100
    { Horizontal differencing before LZW or Deflate. Used only where
      TiffPredictorUsable allows it. }
    Predictor: Boolean;
    { Value of Orientation tag, 1..8 (see SMetaTiffOrientation in ImagingTiff). }
    Orientation: Integer;
    { Resolution is written if ResolutionUnit is 2 (inch) or 3 (cm). }
    ResolutionUnit: Integer;
    XResolution, YResolution: Double;
    { For Photometric 3, 256 entries. }
    Palette: PPalette32;
  end;

  { Writes pages to a TIFF file. Create with TiffCreateWriter, call Finish
    after the last page, then free it. Freeing without Finish (after an
    error) only releases the writer, the output is not a complete file. }
  TTiffFileWriter = class
  public
    procedure BeginPage(const Params: TTiffSaveParams); virtual; abstract;
    { Hands over the next RowCount rows of the current page. }
    procedure WriteRows(Buffer: Pointer; RowCount: Integer); virtual; abstract;
    procedure EndPage; virtual; abstract;
    { Ends the file: data still buffered is written and the output is
      positioned after the last byte of the file. Imaging takes that
      position as the end of a file saved to memory, and the writers leave
      it where they last linked a directory. }
    procedure Finish; virtual; abstract;
  end;

{ Returns TIFF implementation in use. Looks for libtiff (if compiled in)
  when called for the first time. }
function TiffBackend: TTiffBackend;
{ Makes the given backend current (e.g. PasTiff although libtiff is loaded).
  Returns False and keeps the current one if it is not available. }
function SelectTiffBackend(Backend: TTiffBackend): Boolean;
{ Loads libtiff from the given file (instead of looking for it in the default
  locations) and uses it from now on. Returns False and falls back to PasTiff
  (if available) when the library can't be loaded or libtiff is not compiled in. }
function TryLoadLibTiffLibrary(const FileName: string): Boolean;

{ True if the horizontal predictor can be used for a page: LZW or Deflate
  compression and unsigned integer samples of 8, 16 or 32 bits that are not
  palette indices (differences of indices mean nothing and only make the data
  harder to compress). }
function TiffPredictorUsable(const Params: TTiffSaveParams): Boolean;
{ Tells whether the horizontal predictor makes the page smaller. Sample holds
  SampleRows rows of the page in the layout given to WriteRows (Imaging's
  B, G, R order is fine, it does not change the result noticeably); they are
  compressed with and without the predictor and the sizes compared, so the
  sample should be small (Imaging uses about 256 KB) and representative.
  Without PasTiff, which has the encoders (Delphi older than 2009), color
  pages are said to profit and gray ones not: photos and color scans mostly
  do, gray pages are often document scans, which do not. }
function TiffPredictorHelps(const Params: TTiffSaveParams; Sample: Pointer;
  SampleRows: Integer): Boolean;

{ Opens TIFF file for reading at the current position of Handle. First page
  is selected. Raises an exception when the file can't be read. }
function TiffOpenReader(const IO: TIOFunctions; Handle: TImagingHandle): TTiffFileReader;
{ Starts a new TIFF file at the current position of Handle. BigTIFF is
  needed for files over 4 GiB. }
function TiffCreateWriter(const IO: TIOFunctions; Handle: TImagingHandle;
  BigTiff: Boolean): TTiffFileWriter;

implementation

uses
{$IFDEF TIFF_USE_PASTIFF}
  Classes, PasTiffTypes, PasTiffCodecs, PasTiffReader, PasTiffWriter, PasTiffPixels,
{$ENDIF}
{$IFDEF TIFF_USE_LIBTIFF}
  LibTiffDynLib,
{$ENDIF}
  ImagingIO, ImagingUtility;

const
  // TIFF tag values used here (same in PasTiff and libtiff)
  CompressionNone = 1;
  CompressionLzw = 5;
  CompressionOldJpeg = 6;
  CompressionJpeg = 7;
  CompressionAdobeDeflate = 8;
  PhotometricMinIsWhite = 0;
  PhotometricMinIsBlack = 1;
  PhotometricRgb = 2;
  PhotometricPalette = 3;
  PhotometricYCbCr = 6;
  PlanarContig = 1;
  PredictorHorizontal = 2;
  SampleFormatUInt = 1;
  SampleFormatFloat = 3;
  SampleFormatVoid = 4;  // undefined, read as unsigned integer like libtiff does

  { Raw size of strips written. libtiff aims at 8 KiB, a value from 1992;
    larger strips compress better and cost less overhead. Same as PasTiff. }
  DefaultStripBytes = 256 * 1024;

var
  Backend: TTiffBackend = tbNone;
  BackendDetected: Boolean = False;

function TiffBackend: TTiffBackend;
begin
  if not BackendDetected then
  begin
    BackendDetected := True;
    Backend := tbNone;
  {$IFDEF TIFF_USE_PASTIFF}
    Backend := tbPasTiff;
  {$ENDIF}
  {$IFDEF TIFF_USE_LIBTIFF}
    if LoadLibTiff then
      Backend := tbLibTiff;
  {$ENDIF}
  end;
  Result := Backend;
end;

function SelectTiffBackend(Backend: TTiffBackend): Boolean;
begin
  TiffBackend;
  case Backend of
  {$IFDEF TIFF_USE_PASTIFF}
    tbPasTiff: Result := True;
  {$ENDIF}
  {$IFDEF TIFF_USE_LIBTIFF}
    tbLibTiff: Result := IsLibTiffLoaded;
  {$ENDIF}
  else
    Result := False;
  end;

  if Result then
    ImagingTiffBackend.Backend := Backend;
end;

function TryLoadLibTiffLibrary(const FileName: string): Boolean;
begin
  TiffBackend;
{$IFDEF TIFF_USE_LIBTIFF}
  Result := LoadLibTiffFromFile(FileName);
  if Result then
    Backend := tbLibTiff
  else
    Backend := {$IFDEF TIFF_USE_PASTIFF}tbPasTiff{$ELSE}tbNone{$ENDIF};
{$ELSE}
  Result := False;
{$ENDIF}
end;

{ Bytes of one row of Width pixels with Bits per sample and Samples samples. }
function RowSize(Width, Bits, Samples: Integer): NativeInt;
begin
  Result := (NativeInt(Width) * Bits * Samples + 7) div 8;
end;

{ Rows per strip of about DefaultStripBytes, a multiple of 16 so that JPEG
  with chroma subsampling can be used. }
function ChooseRowsPerStrip(const Params: TTiffSaveParams): Integer;
var
  Bytes: NativeInt;
begin
  Bytes := RowSize(Params.Width, Params.BitsPerSample, Params.SamplesPerPixel);
  Result := 16;
  if Bytes > 0 then
    Result := ((DefaultStripBytes div Bytes + 15) div 16) * 16;
  if Result < 16 then
    Result := 16;
end;

function IsJpegForRgb(const Params: TTiffSaveParams): Boolean;
begin
  Result := (Params.Compression = CompressionJpeg) and (Params.Photometric = PhotometricRgb);
end;

function TiffPredictorUsable(const Params: TTiffSaveParams): Boolean;
begin
  Result := ((Params.Compression = CompressionLzw) or (Params.Compression = CompressionAdobeDeflate)) and
    (Params.SampleFormat = SampleFormatUInt) and (Params.BitsPerSample in [8, 16, 32]) and
    (Params.Photometric <> PhotometricPalette);
end;

function TiffPredictorHelps(const Params: TTiffSaveParams; Sample: Pointer;
  SampleRows: Integer): Boolean;
{$IFDEF TIFF_USE_PASTIFF}
var
  Data: TDynByteArray;
  Size: NativeInt;
  Plain, Predicted: NativeInt;

  function Compressed: NativeInt;
  var
    Output: TTiffOutBuffer;
  begin
    Output.Clear;
    if Params.Compression = CompressionLzw then
      TiffLzwEncode(Pointer(Data), Size, Output)
    else
      TiffDeflateEncode(Pointer(Data), Size, Output);
    Result := Output.Size;
  end;

begin
  Result := False;
  if not TiffPredictorUsable(Params) or (SampleRows <= 0) then
    Exit;

  Size := RowSize(Params.Width, Params.BitsPerSample, Params.SamplesPerPixel) * SampleRows;
  SetLength(Data, Size);
  Move(Sample^, Data[0], Size);
  Plain := Compressed;
  TiffPredictorEncode(Pointer(Data), RowSize(Params.Width, Params.BitsPerSample, Params.SamplesPerPixel),
                      SampleRows, Params.BitsPerSample, Params.SamplesPerPixel);
  Predicted := Compressed;
  Result := Predicted < Plain;
end;
{$ELSE}
begin
  Result := TiffPredictorUsable(Params) and (Params.Photometric = PhotometricRgb);
end;
{$ENDIF}

{$IFDEF TIFF_USE_PASTIFF}

{ PasTiff backend }

type
  TPasTiffFileReader = class(TTiffFileReader)
  private
    FStream: TImagingIOStream;
    FReader: TTiffReader;
    FConverter: TTiffRowConverter;
  public
    constructor Create(const IO: TIOFunctions; Handle: TImagingHandle);
    destructor Destroy; override;
    procedure SelectPage(Index: Integer); override;
    procedure ReadPixels(Dst: Pointer; Stride: NativeInt); override;
    function ReadPalette(Pal: PPalette32): Integer; override;
  end;

  TPasTiffFileWriter = class(TTiffFileWriter)
  private
    FStream: TImagingIOStream;
    FWriter: TTiffWriter;
    FStart: Int64;
  public
    constructor Create(const IO: TIOFunctions; Handle: TImagingHandle; BigTiff: Boolean);
    destructor Destroy; override;
    procedure BeginPage(const Params: TTiffSaveParams); override;
    procedure WriteRows(Buffer: Pointer; RowCount: Integer); override;
    procedure EndPage; override;
    procedure Finish; override;
  end;

procedure PageInfoToDesc(const Info: TTiffPageInfo; out Desc: TTiffPageDesc);
begin
  Desc.Width := Info.Width;
  Desc.Height := Info.Height;
  Desc.BitsPerSample := Info.BitsPerSample;
  Desc.SamplesPerPixel := Info.SamplesPerPixel;
  Desc.SampleFormat := Info.SampleFormat;
  Desc.Photometric := Info.Photometric;
  Desc.PlanarConfig := Info.PlanarConfig;
  Desc.Compression := Info.Compression;
  Desc.Orientation := Info.Orientation;
  Desc.HasResolution := Info.HasResolution;
  Desc.XResolution := Info.XResolution;
  Desc.YResolution := Info.YResolution;
  Desc.ResolutionUnit := Info.ResolutionUnit;
end;

{ Layout of a page for the converter, tdlUnknown if it can't convert it. }
function InitConverter(var Converter: TTiffRowConverter; const Info: TTiffPageInfo): TTiffDataLayout;
begin
  if Converter.Init(Info) then
    Result := TTiffDataLayout(Ord(Converter.Layout))
  else
    Result := tdlUnknown;
end;

function ConverterPalette(const Info: TTiffPageInfo; Pal: PPalette32): Integer;
var
  Colors: array[0..255, 0..2] of Byte;
  I: Integer;
begin
  Result := TiffGetPalette8(Info, @Colors[0, 0], 256);
  for I := 0 to Result - 1 do
  begin
    Pal[I].A := 255;
    Pal[I].R := Colors[I, 0];
    Pal[I].G := Colors[I, 1];
    Pal[I].B := Colors[I, 2];
  end;
end;

constructor TPasTiffFileReader.Create(const IO: TIOFunctions; Handle: TImagingHandle);
begin
  inherited Create;
  FStream := TImagingIOStream.Create(IO, Handle);
  FReader := TTiffReader.Create(FStream);
  // Rescue as much as possible from damaged files, as libtiff does
  FReader.TolerateDataErrors := True;
  FPageCount := FReader.PageCount;
  SelectPage(0);
end;

destructor TPasTiffFileReader.Destroy;
begin
  FReader.Free;
  FStream.Free;
  inherited;
end;

procedure TPasTiffFileReader.SelectPage(Index: Integer);
begin
  FReader.SelectPage(Index);
  PageInfoToDesc(FReader.Page, FPage);
  FLayout := tdlUnknown;
  FUnsupportedReason := FReader.UnsupportedReason;
  if FUnsupportedReason = '' then
  begin
    FLayout := InitConverter(FConverter, FReader.DataInfo);
    if FLayout = tdlUnknown then
      FUnsupportedReason := 'sample layout';
  end;
end;

procedure TPasTiffFileReader.ReadPixels(Dst: Pointer; Stride: NativeInt);
begin
  if FLayout = tdlUnknown then
    raise ETiffBackendError.Create('TIFF page not supported: ' + FReader.UnsupportedReason);
  FReader.ReadPixels(FConverter, Dst, Stride);
end;

function TPasTiffFileReader.ReadPalette(Pal: PPalette32): Integer;
begin
  Result := ConverterPalette(FReader.DataInfo, Pal);
end;

constructor TPasTiffFileWriter.Create(const IO: TIOFunctions; Handle: TImagingHandle;
  BigTiff: Boolean);
begin
  inherited Create;
  FStream := TImagingIOStream.Create(IO, Handle);
  FStart := FStream.Position;
  FWriter := TTiffWriter.Create(FStream, BigTiff);
end;

destructor TPasTiffFileWriter.Destroy;
begin
  FWriter.Free;
  FStream.Free;
  inherited;
end;

procedure TPasTiffFileWriter.BeginPage(const Params: TTiffSaveParams);
var
  Page: TTiffPageParams;
  I: Integer;
begin
  TiffInitPageParams(Page);
  Page.Width := Params.Width;
  Page.Height := Params.Height;
  Page.BitsPerSample := Params.BitsPerSample;
  Page.SamplesPerPixel := Params.SamplesPerPixel;
  Page.SampleFormat := Params.SampleFormat;
  Page.Photometric := Params.Photometric;
  Page.Compression := Params.Compression;
  Page.JpegQuality := Params.JpegQuality;
  Page.RowsPerStrip := ChooseRowsPerStrip(Params);
  Page.Predictor := Params.Predictor and TiffPredictorUsable(Params);

  if Params.ResolutionUnit in [TiffResUnitInch, TiffResUnitCentimeter] then
  begin
    Page.ResolutionUnit := Params.ResolutionUnit;
    Page.XResolution := Params.XResolution;
    Page.YResolution := Params.YResolution;
  end;

  if Params.Photometric = PhotometricPalette then
  begin
    SetLength(Page.ColorMap, 3 * 256);
    for I := 0 to 255 do
    begin
      // 8-bit to 16-bit by repeating the byte, 255 becomes 65535
      Page.ColorMap[I] := Params.Palette[I].R * 257;
      Page.ColorMap[256 + I] := Params.Palette[I].G * 257;
      Page.ColorMap[512 + I] := Params.Palette[I].B * 257;
    end;
  end;

  FWriter.BeginPage(Page);
  // Not a tag describing the pixel data, the writer keeps it as given
  FWriter.PageDirectory.SetInt(TiffTagOrientation, ttShort, Params.Orientation);
end;

procedure TPasTiffFileWriter.WriteRows(Buffer: Pointer; RowCount: Integer);
begin
  FWriter.WriteRows(Buffer, RowCount);
end;

procedure TPasTiffFileWriter.EndPage;
begin
  FWriter.EndPage;
end;

procedure TPasTiffFileWriter.Finish;
begin
  // TTiffWriter writes everything in EndPage, only the position is left
  FStream.Position := FStart + FWriter.Container.Size;
end;

{$ENDIF}

{$IFDEF TIFF_USE_LIBTIFF}

{ libtiff backend }

type
  { What libtiff's IO callbacks get as their handle. Offsets libtiff asks
    for are from the start of the TIFF, which need not be the start of the
    stream (Base). }
  TTiffIOWrapper = record
    IO: TIOFunctions;
    Handle: TImagingHandle;
    Base: Int64;
    Writing: Boolean;
    { End of written data, relative to Base. }
    WrittenEnd: Int64;
  end;
  PTiffIOWrapper = ^TTiffIOWrapper;

{$IFDEF TIFF_USE_PASTIFF}
  TSingles = array[0..5] of Single;
  PSingles = ^TSingles;
{$ENDIF}

  TLibTiffFileReader = class(TTiffFileReader)
  private
    FIOWrapper: TTiffIOWrapper;
    FTiff: PTIFF;
    { Read through TIFFReadRGBAImage instead of strips and tiles. }
    FUseRgba: Boolean;
    { Reduced path: gray to be inverted after reading. }
    FInvert: Boolean;
    FRed, FGreen, FBlue: PWordArray;
    FColorCount: Integer;
  {$IFDEF TIFF_USE_PASTIFF}
    FInfo: TTiffPageInfo;
    FConverter: TTiffRowConverter;
  {$ENDIF}
    function TagWord(Tag: UInt32; Default: Word): Word;
    function TagLong(Tag: UInt32; Default: UInt32): UInt32;
    function RequiredTagLong(Tag: UInt32; const Name: string): UInt32;
    function TagFloat(Tag: UInt32; out Value: Double): Boolean;
    function NativeLayout: TTiffDataLayout;
    procedure ReadChunkRows(Dst: Pointer; Stride: NativeInt);
    procedure ReadRgba(Dst: Pointer; Stride: NativeInt);
  public
    constructor Create(const IO: TIOFunctions; Handle: TImagingHandle);
    destructor Destroy; override;
    procedure SelectPage(Index: Integer); override;
    procedure ReadPixels(Dst: Pointer; Stride: NativeInt); override;
    function ReadPalette(Pal: PPalette32): Integer; override;
  end;

  TLibTiffFileWriter = class(TTiffFileWriter)
  private
    FIOWrapper: TTiffIOWrapper;
    FTiff: PTIFF;
    FRow: Integer;
    FRowBytes: NativeInt;
    FRowCopy: TDynByteArray;
  public
    constructor Create(const IO: TIOFunctions; Handle: TImagingHandle; BigTiff: Boolean);
    destructor Destroy; override;
    procedure BeginPage(const Params: TTiffSaveParams); override;
    procedure WriteRows(Buffer: Pointer; RowCount: Integer); override;
    procedure EndPage; override;
    procedure Finish; override;
  end;

threadvar
  LastLibTiffError: string;

procedure LibTiffError(const Module, Message: AnsiString);
begin
  LastLibTiffError := string(Module + ': ' + Message);
end;

procedure ClearLibTiffError;
begin
  LastLibTiffError := '';
  SetLibTiffMessageHandlers(LibTiffError, nil);
end;

procedure RaiseLibTiffError(const Msg: string);
begin
  if LastLibTiffError <> '' then
    raise ETiffBackendError.Create(Msg + ': ' + LastLibTiffError)
  else
    raise ETiffBackendError.Create(Msg);
end;

{ IO callbacks are called from C code: exceptions must not escape them,
  failures are reported to libtiff by the result. }

function TIFFReadProc(Handle: thandle_t; Buffer: Pointer; Size: tmsize_t): tmsize_t; cdecl;
var
  Wrapper: PTiffIOWrapper;
begin
  Wrapper := PTiffIOWrapper(Handle);
  try
    Result := Wrapper.IO.Read(Wrapper.Handle, Buffer, Size);
  except
    Result := -1;
  end;
end;

function TIFFWriteProc(Handle: thandle_t; Buffer: Pointer; Size: tmsize_t): tmsize_t; cdecl;
var
  Wrapper: PTiffIOWrapper;
  Pos: Int64;
begin
  Wrapper := PTiffIOWrapper(Handle);
  try
    Result := Wrapper.IO.Write(Wrapper.Handle, Buffer, Size);
    Pos := Wrapper.IO.Tell(Wrapper.Handle) - Wrapper.Base;
    if Pos > Wrapper.WrittenEnd then
      Wrapper.WrittenEnd := Pos;
  except
    Result := -1;
  end;
end;

function TIFFSizeProc(Handle: thandle_t): toff_t; cdecl;
var
  Wrapper: PTiffIOWrapper;
begin
  Wrapper := PTiffIOWrapper(Handle);
  try
    // When writing, the end of what was written: the output may be a memory
    // buffer larger than the file
    if Wrapper.Writing then
      Result := Wrapper.WrittenEnd
    else
      Result := ImagingIO.GetInputSize(Wrapper.IO, Wrapper.Handle) - Wrapper.Base;
  except
    Result := 0;
  end;
end;

function TIFFSeekProc(Handle: thandle_t; Offset: toff_t; Where: Integer): toff_t; cdecl;
const
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;
var
  Wrapper: PTiffIOWrapper;
begin
  Wrapper := PTiffIOWrapper(Handle);
  try
    case Where of
      SEEK_CUR:
        Result := Wrapper.IO.Seek(Wrapper.Handle, Offset, smFromCurrent);
      SEEK_END:
        if Wrapper.Writing then
          Result := Wrapper.IO.Seek(Wrapper.Handle, Wrapper.Base + Wrapper.WrittenEnd + Offset, smFromBeginning)
        else
          Result := Wrapper.IO.Seek(Wrapper.Handle, Offset, smFromEnd);
    else
      Result := Wrapper.IO.Seek(Wrapper.Handle, Wrapper.Base + Offset, smFromBeginning);
    end;
    Result := Result - Wrapper.Base;
  except
    Result := -1;
  end;
end;

function TIFFCloseProc(Handle: thandle_t): Integer; cdecl;
begin
  Result := 0;
end;

function TIFFNoMapProc(Handle: thandle_t; var Base: tdata_t; var Size: toff_t): Integer; cdecl;
begin
  Result := 0;
end;

procedure TIFFNoUnmapProc(Handle: thandle_t; Base: tdata_t; Size: toff_t); cdecl;
begin
end;

function OpenLibTiff(var Wrapper: TTiffIOWrapper; const IO: TIOFunctions;
  Handle: TImagingHandle; Mode: PAnsiChar): PTIFF;
begin
  Wrapper.IO := IO;
  Wrapper.Handle := Handle;
  Wrapper.Base := IO.Tell(Handle);
  Wrapper.WrittenEnd := 0;
  Wrapper.Writing := Mode^ = 'w';
  ClearLibTiffError;
  Result := TIFFClientOpen('Imaging', Mode, @Wrapper, TIFFReadProc,
    TIFFWriteProc, TIFFSeekProc, TIFFCloseProc, TIFFSizeProc,
    TIFFNoMapProc, TIFFNoUnmapProc);
  if Result = nil then
    RaiseLibTiffError('libtiff cannot open TIFF');
end;

constructor TLibTiffFileReader.Create(const IO: TIOFunctions; Handle: TImagingHandle);
begin
  inherited Create;
  FTiff := OpenLibTiff(FIOWrapper, IO, Handle, 'r');
  FPageCount := TIFFNumberOfDirectories(FTiff);
  SelectPage(0);
end;

destructor TLibTiffFileReader.Destroy;
begin
  if FTiff <> nil then
    TIFFClose(FTiff);
  inherited;
end;

{ Values of tags of C type uint16, uint32 and float in the current
  directory. TIFFGetFieldDefaulted gives the default of the TIFF
  specification for tags that have one (BitsPerSample, Compression...);
  Default is for tags without, when the file does not have them. }

function TLibTiffFileReader.TagWord(Tag: UInt32; Default: Word): Word;
var
  Value: Word;
begin
  if TIFFGetFieldDefaulted(FTiff, Tag, @Value) = 1 then
    Result := Value
  else
    Result := Default;
end;

function TLibTiffFileReader.TagLong(Tag: UInt32; Default: UInt32): UInt32;
var
  Value: UInt32;
begin
  if TIFFGetFieldDefaulted(FTiff, Tag, @Value) = 1 then
    Result := Value
  else
    Result := Default;
end;

function TLibTiffFileReader.RequiredTagLong(Tag: UInt32; const Name: string): UInt32;
begin
  Result := TagLong(Tag, 0);
  if Result = 0 then
    RaiseLibTiffError(Format('TIFF page has no valid %s', [Name]));
end;

function TLibTiffFileReader.TagFloat(Tag: UInt32; out Value: Double): Boolean;
var
  Single32: Single;
begin
  Result := TIFFGetField(FTiff, Tag, @Single32) = 1;
  if Result then
    Value := Single32
  else
    Value := 0;
end;

procedure TLibTiffFileReader.SelectPage(Index: Integer);
var
  ExtraCount: Word;
  ExtraTypes: PWordArray;
{$IFDEF TIFF_USE_PASTIFF}
  SubsampleH, SubsampleV: Word;
  Floats: PSingles;
  I: Integer;
{$ENDIF}
begin
  ClearLibTiffError;
  if TIFFSetDirectory(FTiff, Index) = 0 then
    RaiseLibTiffError(Format('libtiff cannot read TIFF page %d', [Index]));

  FPage.Width := RequiredTagLong(TIFFTAG_IMAGEWIDTH, 'ImageWidth');
  FPage.Height := RequiredTagLong(TIFFTAG_IMAGELENGTH, 'ImageLength');
  FPage.BitsPerSample := TagWord(TIFFTAG_BITSPERSAMPLE, 1);
  FPage.SamplesPerPixel := TagWord(TIFFTAG_SAMPLESPERPIXEL, 1);
  FPage.SampleFormat := TagWord(TIFFTAG_SAMPLEFORMAT, SampleFormatUInt);
  FPage.Compression := TagWord(TIFFTAG_COMPRESSION, CompressionNone);
  // No default in the specification; libtiff itself guesses one for files
  // without the tag when it reads the directory
  FPage.Photometric := TagWord(TIFFTAG_PHOTOMETRIC, PhotometricMinIsBlack);
  FPage.PlanarConfig := TagWord(TIFFTAG_PLANARCONFIG, PlanarContig);
  FPage.Orientation := TagWord(TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
  FPage.ResolutionUnit := TagWord(TIFFTAG_RESOLUTIONUNIT, RESUNIT_INCH);
  FPage.HasResolution := TagFloat(TIFFTAG_XRESOLUTION, FPage.XResolution) and
                         TagFloat(TIFFTAG_YRESOLUTION, FPage.YResolution);

  FRed := nil;
  FGreen := nil;
  FBlue := nil;
  FColorCount := 0;
  if (FPage.Photometric = PhotometricPalette) and (FPage.BitsPerSample <= 16) and
    (TIFFGetField(FTiff, TIFFTAG_COLORMAP, @FRed, @FGreen, @FBlue) = 1) then
  begin
    FColorCount := 1 shl FPage.BitsPerSample;
  end;

  // Count and array of types; the array belongs to libtiff
  ExtraTypes := nil;
  if (TIFFGetFieldDefaulted(FTiff, TIFFTAG_EXTRASAMPLES, @ExtraCount, @ExtraTypes) <> 1) or
    (ExtraTypes = nil) then
  begin
    ExtraCount := 0;
  end;

  // libtiff's JPEG codec converts YCbCr to RGB when asked. Must be set before
  // any size is queried, it changes strip/tile sizes.
  if (FPage.Compression = CompressionJpeg) and (FPage.Photometric = PhotometricYCbCr) then
    TIFFSetField(FTiff, TIFFTAG_JPEGCOLORMODE, JPEGCOLORMODE_RGB);

  FInvert := False;
  FUseRgba := False;
  FLayout := tdlUnknown;

  // Old-style JPEG delivers whatever its tables say, leave it to libtiff
  if FPage.Compression <> CompressionOldJpeg then
  begin
  {$IFDEF TIFF_USE_PASTIFF}
    // Description of the decoded data as PasTiff's DataInfo would give it,
    // what libtiff decodes is the same (see TiffDiff in PasTiff tests)
    FInfo.ColorMap := nil;
    FillChar(FInfo, SizeOf(FInfo), 0);
    FInfo.Width := FPage.Width;
    FInfo.Height := FPage.Height;
    FInfo.BitsPerSample := FPage.BitsPerSample;
    FInfo.SamplesPerPixel := FPage.SamplesPerPixel;
    FInfo.SampleFormat := FPage.SampleFormat;
    FInfo.Compression := FPage.Compression;
    FInfo.Photometric := FPage.Photometric;
    FInfo.PlanarConfig := FPage.PlanarConfig;
    FInfo.FillOrder := TiffFillOrderMsbFirst;   // libtiff reverses bits when needed
    FInfo.Predictor := TiffPredictorNone;       // and undoes the predictor
    FInfo.Orientation := FPage.Orientation;
    FInfo.ResolutionUnit := FPage.ResolutionUnit;
    FInfo.ExtraSampleCount := ExtraCount;
    if FInfo.ExtraSampleCount > TiffMaxExtraSamples then
      FInfo.ExtraSampleCount := TiffMaxExtraSamples;
    for I := 0 to FInfo.ExtraSampleCount - 1 do
      FInfo.ExtraSamples[I] := ExtraTypes[I];
    FInfo.IsTiled := TIFFIsTiled(FTiff) <> 0;

    if FColorCount > 0 then
    begin
      SetLength(FInfo.ColorMap, 3 * FColorCount);
      for I := 0 to FColorCount - 1 do
      begin
        FInfo.ColorMap[I] := FRed[I];
        FInfo.ColorMap[FColorCount + I] := FGreen[I];
        FInfo.ColorMap[2 * FColorCount + I] := FBlue[I];
      end;
    end;

    FInfo.InkSet := TagWord(TIFFTAG_INKSET, TiffInkSetCmyk);

    // The YCbCr tags all have defaults in libtiff: subsampling 2x2,
    // ITU-R BT.601 coefficients, full range reference black and white
    FInfo.YCbCrSubsampleH := 2;
    FInfo.YCbCrSubsampleV := 2;
    if TIFFGetFieldDefaulted(FTiff, TIFFTAG_YCBCRSUBSAMPLING, @SubsampleH, @SubsampleV) = 1 then
    begin
      FInfo.YCbCrSubsampleH := SubsampleH;
      FInfo.YCbCrSubsampleV := SubsampleV;
    end;
    Floats := nil;
    if (TIFFGetFieldDefaulted(FTiff, TIFFTAG_YCBCRCOEFFICIENTS, @Floats) = 1) and (Floats <> nil) then
    begin
      for I := 0 to 2 do
        FInfo.YCbCrCoefficients[I] := Floats[I];
    end;
    if (TIFFGetFieldDefaulted(FTiff, TIFFTAG_REFERENCEBLACKWHITE, @Floats) = 1) and (Floats <> nil) then
    begin
      for I := 0 to 5 do
        FInfo.ReferenceBlackWhite[I] := Floats[I];
    end;

    if (FPage.Compression = CompressionJpeg) and (FPage.Photometric = PhotometricYCbCr) then
    begin
      FInfo.Photometric := PhotometricRgb;
      FInfo.YCbCrSubsampleH := 1;
      FInfo.YCbCrSubsampleV := 1;
    end;

    FLayout := InitConverter(FConverter, FInfo);
    // Subsampled YCbCr in tiles is too rare to assemble its blocks here
    if FInfo.IsTiled and (FConverter.RowsPerSrcRow > 1) then
      FLayout := tdlUnknown;
  {$ELSE}
    FLayout := NativeLayout;
  {$ENDIF}
  end;

  if FLayout = tdlUnknown then
  begin
    FUseRgba := True;
    FLayout := tdlRgba8;
  end;
end;

{ Reduced path for compilers without PasTiffPixels: layouts that need no
  conversion, MinIsWhite gray is inverted. }
function TLibTiffFileReader.NativeLayout: TTiffDataLayout;
var
  Bits, Samples, Photometric: Integer;
begin
  Result := tdlUnknown;
  Bits := FPage.BitsPerSample;
  Samples := FPage.SamplesPerPixel;
  Photometric := FPage.Photometric;
  if (FPage.Compression = CompressionJpeg) and (Photometric = PhotometricYCbCr) then
    Photometric := PhotometricRgb;  // JPEGCOLORMODE_RGB
  if (FPage.PlanarConfig <> PlanarContig) and (Samples > 1) then
    Exit;

  if FPage.SampleFormat in [SampleFormatUInt, SampleFormatVoid] then
  begin
    case Photometric of
      PhotometricMinIsWhite, PhotometricMinIsBlack:
        begin
          if (Bits = 1) and (Samples = 1) then Result := tdlBilevel
          else if (Bits = 8) and (Samples = 1) then Result := tdlGray8
          else if (Bits = 8) and (Samples = 2) then Result := tdlGrayAlpha8
          else if (Bits = 16) and (Samples = 1) then Result := tdlGray16
          else if (Bits = 16) and (Samples = 2) then Result := tdlGrayAlpha16
          else if (Bits = 32) and (Samples = 1) then Result := tdlGray32;
          // Only the color sample is to be inverted, not alpha
          FInvert := (Photometric = PhotometricMinIsWhite) and (Samples = 1) and (Bits <= 16);
          if (Photometric = PhotometricMinIsWhite) and not FInvert then
            Result := tdlUnknown;
        end;
      PhotometricRgb:
        begin
          if (Bits = 8) and (Samples = 3) then Result := tdlRgb8
          else if (Bits = 8) and (Samples = 4) then Result := tdlRgba8
          else if (Bits = 16) and (Samples = 3) then Result := tdlRgb16
          else if (Bits = 16) and (Samples = 4) then Result := tdlRgba16;
        end;
      PhotometricPalette:
        if (Bits = 8) and (Samples = 1) and (FColorCount > 0) then
          Result := tdlIndexed8;
    end;
  end
  else if FPage.SampleFormat = SampleFormatFloat then
  begin
    if Photometric in [PhotometricMinIsBlack, PhotometricRgb] then
    begin
      if (Bits = 16) and (Samples = 1) then Result := tdlGrayFloat16
      else if (Bits = 32) and (Samples = 1) then Result := tdlGrayFloat32
      else if (Bits = 16) and (Samples = 4) then Result := tdlRgbaFloat16
      else if (Bits = 32) and (Samples = 4) then Result := tdlRgbaFloat32;
    end;
  end;
end;

procedure TLibTiffFileReader.ReadPixels(Dst: Pointer; Stride: NativeInt);
begin
  if FUseRgba then
    ReadRgba(Dst, Stride)
  else
    ReadChunkRows(Dst, Stride);
end;

{ Pages libtiff decodes but nothing here can convert: libtiff's RGBA
  interface, 8 bits per sample. Pixels come as 32-bit ABGR values, that is
  bytes R, G, B, A in memory. }
procedure TLibTiffFileReader.ReadRgba(Dst: Pointer; Stride: NativeInt);
var
  Raster: TDynByteArray;
  Row: Integer;
  RowBytes: NativeInt;
begin
  RowBytes := NativeInt(FPage.Width) * 4;
  SetLength(Raster, RowBytes * FPage.Height);
  ClearLibTiffError;
  // Asking for the orientation the page has keeps its pixels as stored
  if TIFFReadRGBAImageOriented(FTiff, FPage.Width, FPage.Height, Pointer(Raster),
    FPage.Orientation, 0) = 0 then
  begin
    RaiseLibTiffError('libtiff cannot read TIFF page');
  end;

  for Row := 0 to FPage.Height - 1 do
    Move(Raster[NativeInt(Row) * RowBytes], PBuffer(Dst)[NativeInt(Row) * Stride], RowBytes);
end;

{ Reads strips or tiles band by band (a band is a strip, or a row of tiles)
  and converts the rows of each band into Dst. }
procedure TLibTiffFileReader.ReadChunkRows(Dst: Pointer; Stride: NativeInt);
const
  // A strip decoding to more than this is read by scanlines
  ScanlineThreshold = 16 * 1024 * 1024;
  ScanlineBand = 256;
var
  Planes, Plane, BandRows, BandDataRows, DataRows, DataRowsDone, RowsDone,
    Band, ChunksPerPlane, TilesAcross, TileColumn, Row, ChunkHeight, ChunkWidth,
    RowsConverted: Integer;
  RowsPerSrcRow: Integer;
  RowBytes, TileRowBytes, ChunkBytes, CopyBytes, Offset: NativeInt;
  Tiled, ByScanline: Boolean;
  Bands: array of TDynByteArray;
  Tile: TDynByteArray;
  Src: array of Pointer;
  StripRows: UInt32;
  ChunkIndex: UInt32;

  procedure InvertRows(Count: Integer);
  var
    I: Integer;
    J: NativeInt;
    RowData: PBuffer;
    Sample: PWord;
  begin
    for I := 0 to Count - 1 do
    begin
      RowData := @PBuffer(Dst)[NativeInt(RowsDone + I) * Stride];
      if FPage.BitsPerSample = 16 then
      begin
        Sample := PWord(RowData);
        for J := 0 to NativeInt(FPage.Width) - 1 do
        begin
          Sample^ := not Sample^;
          Inc(Sample);
        end;
      end
      else
        for J := 0 to RowBytes - 1 do
          RowData[J] := not RowData[J];
    end;
  end;

begin
  Tiled := TIFFIsTiled(FTiff) <> 0;
  Tile := nil;
  if FPage.PlanarConfig = PlanarContig then
    Planes := 1
  else
    Planes := FPage.SamplesPerPixel;

{$IFDEF TIFF_USE_PASTIFF}
  RowsPerSrcRow := FConverter.RowsPerSrcRow;
  RowBytes := FConverter.SrcRowBytes;
{$ELSE}
  RowsPerSrcRow := 1;
  RowBytes := RowSize(FPage.Width, FPage.BitsPerSample, FPage.SamplesPerPixel div Planes);
{$ENDIF}
  DataRows := (FPage.Height + RowsPerSrcRow - 1) div RowsPerSrcRow;

  if Tiled then
  begin
    ChunkWidth := RequiredTagLong(TIFFTAG_TILEWIDTH, 'TileWidth');
    ChunkHeight := RequiredTagLong(TIFFTAG_TILELENGTH, 'TileLength');
    TilesAcross := (FPage.Width + ChunkWidth - 1) div ChunkWidth;
    TileRowBytes := RowSize(ChunkWidth, FPage.BitsPerSample, FPage.SamplesPerPixel div Planes);
    ChunkBytes := TIFFTileSize(FTiff);
    SetLength(Tile, ChunkBytes);
    ChunksPerPlane := TilesAcross * ((FPage.Height + ChunkHeight - 1) div ChunkHeight);
    BandDataRows := ChunkHeight;
    ByScanline := False;
  end
  else
  begin
    // Default (2^32 - 1) and values over the height mean one strip
    StripRows := TagLong(TIFFTAG_ROWSPERSTRIP, High(UInt32));
    ChunkHeight := FPage.Height;
    if (StripRows > 0) and (StripRows < UInt32(FPage.Height)) then
      ChunkHeight := StripRows;
    TilesAcross := 1;
    TileRowBytes := 0;
    ChunkBytes := 0;
    BandDataRows := (ChunkHeight + RowsPerSrcRow - 1) div RowsPerSrcRow;
    ChunksPerPlane := (DataRows + BandDataRows - 1) div BandDataRows;
    // Huge single strips are read by scanlines, libtiff decodes them
    // progressively then. Not for subsampled YCbCr, whose scanlines libtiff
    // does not deliver as blocks.
    ByScanline := (NativeInt(BandDataRows) * RowBytes > ScanlineThreshold) and (RowsPerSrcRow = 1);
    if ByScanline then
      BandDataRows := ScanlineBand;
  end;

  if RowBytes <= 0 then
    Exit;

  SetLength(Bands, Planes);
  SetLength(Src, Planes);
  for Plane := 0 to Planes - 1 do
    SetLength(Bands[Plane], NativeInt(BandDataRows) * RowBytes);

  DataRowsDone := 0;
  RowsDone := 0;
  Band := 0;
  while (DataRowsDone < DataRows) and (RowsDone < FPage.Height) do
  begin
    BandRows := Min(BandDataRows, DataRows - DataRowsDone);

    for Plane := 0 to Planes - 1 do
    begin
      // Damaged chunks give zeros, as PasTiff does with TolerateDataErrors
      FillChar(Bands[Plane][0], Length(Bands[Plane]), 0);
      ClearLibTiffError;

      if ByScanline then
      begin
        for Row := 0 to BandRows - 1 do
          TIFFReadScanline(FTiff, @Bands[Plane][NativeInt(Row) * RowBytes], DataRowsDone + Row, Plane);
      end
      else if Tiled then
      begin
        for TileColumn := 0 to TilesAcross - 1 do
        begin
          FillChar(Tile[0], Length(Tile), 0);
          ChunkIndex := UInt32(Plane) * UInt32(ChunksPerPlane) + UInt32(Band) * UInt32(TilesAcross) +
                        UInt32(TileColumn);
          TIFFReadEncodedTile(FTiff, ChunkIndex, Pointer(Tile), ChunkBytes);
          Offset := NativeInt(TileColumn) * TileRowBytes;
          CopyBytes := RowBytes - Offset;
          if CopyBytes > TileRowBytes then
            CopyBytes := TileRowBytes;
          for Row := 0 to BandRows - 1 do
          begin
            Move(Tile[NativeInt(Row) * TileRowBytes], Bands[Plane][NativeInt(Row) * RowBytes + Offset],
                 CopyBytes);
          end;
        end;
      end
      else
      begin
        ChunkIndex := UInt32(Plane) * UInt32(ChunksPerPlane) + UInt32(Band);
        TIFFReadEncodedStrip(FTiff, ChunkIndex, Pointer(Bands[Plane]), NativeInt(BandRows) * RowBytes);
      end;
      Src[Plane] := Pointer(Bands[Plane]);
    end;

  {$IFDEF TIFF_USE_PASTIFF}
    RowsConverted := FConverter.ConvertRows(Src, @PBuffer(Dst)[NativeInt(RowsDone) * Stride], Stride,
                                            BandRows, FPage.Height - RowsDone);
  {$ELSE}
    RowsConverted := Min(BandRows, FPage.Height - RowsDone);
    for Row := 0 to RowsConverted - 1 do
    begin
      Move(Bands[0][NativeInt(Row) * RowBytes], PBuffer(Dst)[NativeInt(RowsDone + Row) * Stride],
           RowBytes);
    end;
    if FInvert then
      InvertRows(RowsConverted);
  {$ENDIF}

    Inc(RowsDone, RowsConverted);
    Inc(DataRowsDone, BandRows);
    Inc(Band);
  end;
end;

function TLibTiffFileReader.ReadPalette(Pal: PPalette32): Integer;
var
  I: Integer;
begin
{$IFDEF TIFF_USE_PASTIFF}
  if not FUseRgba then
  begin
    Result := ConverterPalette(FInfo, Pal);
    Exit;
  end;
{$ENDIF}
  Result := Min(FColorCount, 256);
  for I := 0 to Result - 1 do
  begin
    Pal[I].A := 255;
    Pal[I].R := FRed[I] shr 8;
    Pal[I].G := FGreen[I] shr 8;
    Pal[I].B := FBlue[I] shr 8;
  end;
end;

constructor TLibTiffFileWriter.Create(const IO: TIOFunctions; Handle: TImagingHandle;
  BigTiff: Boolean);
const
  Modes: array[Boolean] of PAnsiChar = ('w', 'w8');
begin
  inherited Create;
  FTiff := OpenLibTiff(FIOWrapper, IO, Handle, Modes[BigTiff]);
end;

destructor TLibTiffFileWriter.Destroy;
begin
  if FTiff <> nil then
    TIFFClose(FTiff);
  inherited;
end;

procedure TLibTiffFileWriter.BeginPage(const Params: TTiffSaveParams);
var
  Red, Green, Blue: array[Byte] of Word;
  I: Integer;
  Photometric: Integer;
  ExtraType: Word;
begin
  ClearLibTiffError;
  Photometric := Params.Photometric;
  // JPEG compresses RGB as YCbCr (smaller files), libtiff's JPEG codec
  // converts the RGB rows it is given
  if IsJpegForRgb(Params) then
    Photometric := PhotometricYCbCr;

  TIFFSetField(FTiff, TIFFTAG_IMAGEWIDTH, Params.Width);
  TIFFSetField(FTiff, TIFFTAG_IMAGELENGTH, Params.Height);
  TIFFSetField(FTiff, TIFFTAG_PHOTOMETRIC, Photometric);
  TIFFSetField(FTiff, TIFFTAG_PLANARCONFIG, PlanarContig);
  TIFFSetField(FTiff, TIFFTAG_ORIENTATION, Params.Orientation);
  TIFFSetField(FTiff, TIFFTAG_BITSPERSAMPLE, Params.BitsPerSample);
  TIFFSetField(FTiff, TIFFTAG_SAMPLESPERPIXEL, Params.SamplesPerPixel);
  TIFFSetField(FTiff, TIFFTAG_SAMPLEFORMAT, Params.SampleFormat);
  if ((Photometric = PhotometricMinIsBlack) and (Params.SamplesPerPixel = 2)) or
    (Params.SamplesPerPixel = 4) then
  begin
    ExtraType := EXTRASAMPLE_UNASSALPHA;
    TIFFSetField(FTiff, TIFFTAG_EXTRASAMPLES, 1, @ExtraType);
  end;
  TIFFSetField(FTiff, TIFFTAG_COMPRESSION, Params.Compression);
  if Params.Compression = CompressionJpeg then
  begin
    TIFFSetField(FTiff, TIFFTAG_JPEGQUALITY, Params.JpegQuality);
    if Photometric = PhotometricYCbCr then
      TIFFSetField(FTiff, TIFFTAG_JPEGCOLORMODE, JPEGCOLORMODE_RGB);
  end;
  if Params.Predictor and TiffPredictorUsable(Params) then
    TIFFSetField(FTiff, TIFFTAG_PREDICTOR, PredictorHorizontal);
  TIFFSetField(FTiff, TIFFTAG_ROWSPERSTRIP, ChooseRowsPerStrip(Params));

  if Params.ResolutionUnit in [RESUNIT_INCH, RESUNIT_CENTIMETER] then
  begin
    TIFFSetField(FTiff, TIFFTAG_RESOLUTIONUNIT, Params.ResolutionUnit);
    // Resolution tags are 32-bit float, but a float vararg is passed as
    // double in C, which is what libtiff reads
    TIFFSetField(FTiff, TIFFTAG_XRESOLUTION, Params.XResolution);
    TIFFSetField(FTiff, TIFFTAG_YRESOLUTION, Params.YResolution);
  end;

  if Photometric = PhotometricPalette then
  begin
    for I := 0 to 255 do
    begin
      Red[I] := Params.Palette[I].R * 257;
      Green[I] := Params.Palette[I].G * 257;
      Blue[I] := Params.Palette[I].B * 257;
    end;
    TIFFSetField(FTiff, TIFFTAG_COLORMAP, @Red[0], @Green[0], @Blue[0]);
  end;

  FRow := 0;
  FRowBytes := RowSize(Params.Width, Params.BitsPerSample, Params.SamplesPerPixel);
  SetLength(FRowCopy, FRowBytes);
end;

procedure TLibTiffFileWriter.WriteRows(Buffer: Pointer; RowCount: Integer);
var
  I: Integer;
begin
  for I := 0 to RowCount - 1 do
  begin
    // libtiff before 4.5 applies the predictor in place, in the buffer it
    // is given (and swaps bytes there for big-endian files)
    Move(PBuffer(Buffer)[NativeInt(I) * FRowBytes], FRowCopy[0], FRowBytes);
    if TIFFWriteScanline(FTiff, Pointer(FRowCopy), FRow, 0) < 0 then
      RaiseLibTiffError('libtiff cannot write TIFF row');
    Inc(FRow);
  end;
end;

procedure TLibTiffFileWriter.EndPage;
begin
  if TIFFWriteDirectory(FTiff) = 0 then
    RaiseLibTiffError('libtiff cannot write TIFF directory');
end;

procedure TLibTiffFileWriter.Finish;
begin
  // TIFFClose flushes what libtiff still buffers
  TIFFClose(FTiff);
  FTiff := nil;
  FIOWrapper.IO.Seek(FIOWrapper.Handle, FIOWrapper.Base + FIOWrapper.WrittenEnd, smFromBeginning);
end;

{$ENDIF}

function TiffOpenReader(const IO: TIOFunctions; Handle: TImagingHandle): TTiffFileReader;
begin
  case TiffBackend of
  {$IFDEF TIFF_USE_PASTIFF}
    tbPasTiff: Result := TPasTiffFileReader.Create(IO, Handle);
  {$ENDIF}
  {$IFDEF TIFF_USE_LIBTIFF}
    tbLibTiff: Result := TLibTiffFileReader.Create(IO, Handle);
  {$ENDIF}
  else
    raise ETiffBackendError.Create('No TIFF codec available');
  end;
end;

function TiffCreateWriter(const IO: TIOFunctions; Handle: TImagingHandle;
  BigTiff: Boolean): TTiffFileWriter;
begin
  case TiffBackend of
  {$IFDEF TIFF_USE_PASTIFF}
    tbPasTiff: Result := TPasTiffFileWriter.Create(IO, Handle, BigTiff);
  {$ENDIF}
  {$IFDEF TIFF_USE_LIBTIFF}
    tbLibTiff: Result := TLibTiffFileWriter.Create(IO, Handle, BigTiff);
  {$ENDIF}
  else
    raise ETiffBackendError.Create('No TIFF codec available');
  end;
end;

end.
