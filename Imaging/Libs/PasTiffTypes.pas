{
  PasTiff - pure Pascal TIFF reader/writer
  by Marek Mauder
  https://github.com/galfar/imaginglib
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
}

{ Basic types and constants shared by all PasTiff units: tag numbers, tag data
  types, values of the important tags, exceptions, and TTiffPageInfo which
  describes pixel data of one page (IFD).

  Only tags PasTiff itself interprets have constants here. Any other tag can be
  read and written by its number through TTiffDirectory (PasTiffDirectory.pas).

  References: TIFF 6.0 specification (1992), Adobe TIFF Technical Notes 1-3,
  BigTIFF design (2007), libtiff's tiff.h. }
unit PasTiffTypes;

{$I PasTiff.inc}

interface

uses
  SysUtils;

type
  { Base class of all exceptions raised by PasTiff. }
  ETiffError = class(Exception);
  { The file is damaged or violates the format in a way that cannot be worked around. }
  ETiffFormatError = class(ETiffError);
  { The file is (or may be) valid but uses a feature PasTiff does not implement. }
  ETiffUnsupported = class(ETiffError);
  { Reading from or writing to the underlying stream failed. }
  ETiffIOError = class(ETiffError);

  TTiffInt64Array = array of Int64;
  TTiffDoubleArray = array of Double;
  TTiffWordArray = array of Word;

const
  { Data types of tag values (the "field type" of an IFD entry).
    13 comes from Adobe's PageMaker technote, 16-18 from BigTIFF,
    129 from EXIF 3.0. }
  ttByte      = 1;   // 8-bit unsigned
  ttAscii     = 2;   // 8-bit bytes with terminating NUL
  ttShort     = 3;   // 16-bit unsigned
  ttLong      = 4;   // 32-bit unsigned
  ttRational  = 5;   // two LONGs: numerator, denominator
  ttSByte     = 6;   // 8-bit signed
  ttUndefined = 7;   // 8-bit untyped data
  ttSShort    = 8;   // 16-bit signed
  ttSLong     = 9;   // 32-bit signed
  ttSRational = 10;  // two SLONGs
  ttFloat     = 11;  // 32-bit IEEE float
  ttDouble    = 12;  // 64-bit IEEE float
  ttIFD       = 13;  // 32-bit unsigned offset of a sub-directory
  ttLong8     = 16;  // BigTIFF: 64-bit unsigned
  ttSLong8    = 17;  // BigTIFF: 64-bit signed
  ttIFD8      = 18;  // BigTIFF: 64-bit unsigned offset of a sub-directory
  ttUtf8      = 129; // EXIF 3.0: UTF-8 text with terminating NUL

  { Tags (baseline and extensions that describe the image data) }
  TiffTagNewSubfileType    = 254;
  TiffTagSubfileType       = 255;
  TiffTagImageWidth        = 256;
  TiffTagImageLength       = 257;
  TiffTagBitsPerSample     = 258;
  TiffTagCompression       = 259;
  TiffTagPhotometric       = 262;
  TiffTagFillOrder         = 266;
  TiffTagDocumentName      = 269;
  TiffTagImageDescription  = 270;
  TiffTagMake              = 271;
  TiffTagModel             = 272;
  TiffTagStripOffsets      = 273;
  TiffTagOrientation       = 274;
  TiffTagSamplesPerPixel   = 277;
  TiffTagRowsPerStrip      = 278;
  TiffTagStripByteCounts   = 279;
  TiffTagMinSampleValue    = 280;
  TiffTagMaxSampleValue    = 281;
  TiffTagXResolution       = 282;
  TiffTagYResolution       = 283;
  TiffTagPlanarConfig      = 284;
  TiffTagT4Options         = 292;  // Group3Options in TIFF 5.0
  TiffTagT6Options         = 293;  // Group4Options in TIFF 5.0
  TiffTagResolutionUnit    = 296;
  TiffTagPageNumber        = 297;
  TiffTagSoftware          = 305;
  TiffTagDateTime          = 306;
  TiffTagArtist            = 315;
  TiffTagPredictor         = 317;
  TiffTagColorMap          = 320;
  TiffTagTileWidth         = 322;
  TiffTagTileLength        = 323;
  TiffTagTileOffsets       = 324;
  TiffTagTileByteCounts    = 325;
  TiffTagSubIFD            = 330;
  TiffTagInkSet            = 332;
  TiffTagExtraSamples      = 338;
  TiffTagSampleFormat      = 339;
  TiffTagJpegTables        = 347;
  TiffTagYCbCrCoefficients = 529;
  TiffTagYCbCrSubSampling  = 530;
  TiffTagYCbCrPositioning  = 531;
  TiffTagReferenceBlackWhite = 532;
  TiffTagCopyright         = 33432;
  TiffTagExifIFD           = 34665;
  TiffTagIccProfile        = 34675;
  TiffTagGpsIFD            = 34853;
  TiffTagInteropIFD        = 40965; // lives in the Exif directory

  { Compression }
  TiffCompressionNone       = 1;
  TiffCompressionCcittRle   = 2;      // CCITT modified Huffman RLE
  TiffCompressionCcittFax3  = 3;      // CCITT Group 3 (T.4)
  TiffCompressionCcittFax4  = 4;      // CCITT Group 4 (T.6)
  TiffCompressionLzw        = 5;
  TiffCompressionOldJpeg    = 6;      // TIFF 6.0 JPEG, obsolete and not supported
  TiffCompressionJpeg       = 7;      // JPEG as defined in Technical Note 2
  TiffCompressionAdobeDeflate = 8;    // Deflate, official code
  TiffCompressionCcittRleW  = 32771;  // CCITT RLE, rows are word aligned
  TiffCompressionPackBits   = 32773;
  TiffCompressionDeflate    = 32946;  // Deflate, legacy code used before 8 was assigned

  { PhotometricInterpretation }
  TiffPhotometricMinIsWhite = 0;
  TiffPhotometricMinIsBlack = 1;
  TiffPhotometricRgb        = 2;
  TiffPhotometricPalette    = 3;
  TiffPhotometricMask       = 4;
  TiffPhotometricSeparated  = 5;      // usually CMYK, see InkSet
  TiffPhotometricYCbCr      = 6;
  TiffPhotometricCieLab     = 8;

  { PlanarConfiguration }
  TiffPlanarContig   = 1;   // samples of a pixel are stored together: RGBRGB...
  TiffPlanarSeparate = 2;   // every sample has its own set of strips/tiles

  { FillOrder }
  TiffFillOrderMsbFirst = 1;   // first pixel is in the most significant bit (default)
  TiffFillOrderLsbFirst = 2;

  { SampleFormat }
  TiffSampleFormatUInt   = 1;
  TiffSampleFormatInt    = 2;
  TiffSampleFormatFloat  = 3;
  TiffSampleFormatVoid   = 4;  // undefined, treated as unsigned integer

  { ExtraSamples }
  TiffExtraSampleUnspecified = 0;
  TiffExtraSampleAssocAlpha  = 1;  // premultiplied alpha
  TiffExtraSampleUnassAlpha  = 2;  // straight alpha

  { ResolutionUnit }
  TiffResUnitNone       = 1;
  TiffResUnitInch       = 2;
  TiffResUnitCentimeter = 3;

  { Predictor }
  TiffPredictorNone       = 1;
  TiffPredictorHorizontal = 2;   // difference to the previous pixel
  TiffPredictorFloat      = 3;   // floating point predictor (Technical Note 3)

  { Orientation: where row 0 / column 0 of the stored data are on screen }
  TiffOrientationTopLeft  = 1;

  { T4Options bits }
  TiffT4Options2D           = 1;  // 2-dimensional coding
  TiffT4OptionsUncompressed = 2;  // uncompressed mode may be used
  TiffT4OptionsFillBits     = 4;  // EOLs are byte aligned
  { T6Options bits }
  TiffT6OptionsUncompressed = 2;

  { InkSet }
  TiffInkSetCmyk = 1;

  { Most extra samples PasTiff keeps the meaning of. }
  TiffMaxExtraSamples = 8;

type
  { Description of pixel data of one page, gathered from the page's directory
    with TIFF defaults filled in. This is all that the pixel layer
    (PasTiffPixels.pas) needs, so it can be filled from another TIFF decoder too. }
  TTiffPageInfo = record
    Width, Height: Integer;
    { Bits per sample. All samples of a pixel must have the same size. }
    BitsPerSample: Integer;
    { Number of samples per pixel including extra samples. }
    SamplesPerPixel: Integer;
    SampleFormat: Integer;      // TiffSampleFormat*
    Compression: Integer;       // TiffCompression*
    Photometric: Integer;       // TiffPhotometric*
    PlanarConfig: Integer;      // TiffPlanar*
    FillOrder: Integer;         // TiffFillOrder*
    Orientation: Integer;       // 1..8, reported but never applied
    Predictor: Integer;         // TiffPredictor*
    { Samples beyond those required by Photometric. }
    ExtraSampleCount: Integer;
    ExtraSamples: array[0..TiffMaxExtraSamples - 1] of Word;  // TiffExtraSample*
    { Storage layout }
    IsTiled: Boolean;
    RowsPerStrip: Integer;      // strips only; never larger than Height
    TileWidth, TileHeight: Integer;
    { Resolution, valid if HasResolution }
    HasResolution: Boolean;
    XResolution, YResolution: Double;
    ResolutionUnit: Integer;    // TiffResUnit*
    { Palette for TiffPhotometricPalette: 3 * 2^BitsPerSample 16-bit values,
      all reds first, then greens, then blues (as stored in the file). }
    ColorMap: TTiffWordArray;
    { YCbCr parameters (with defaults when the tags are missing) }
    YCbCrSubsampleH, YCbCrSubsampleV: Integer;
    YCbCrCoefficients: array[0..2] of Double;     // luma weights of R, G, B
    ReferenceBlackWhite: array[0..5] of Double;   // black/white pairs for Y, Cb, Cr
    { CCITT options }
    T4Options, T6Options: Cardinal;
    InkSet: Integer;
  end;

{ Size in bytes of one value of the given tag data type, 0 for unknown types. }
function TiffTypeSize(DataType: Word): Integer;
{ Size of the unit that has to be byte-swapped for the given type:
  equals TiffTypeSize except for rationals, which are pairs of 32-bit values. }
function TiffTypeSwapUnit(DataType: Word): Integer;
{ Human readable name of compression scheme: None, LZW, JPEG, ... }
function TiffCompressionName(Compression: Integer): string;

{ Number of samples Photometric itself needs (1 for gray and palette, 3 for RGB
  and YCbCr, 4 for CMYK); samples above this count are extra samples.
  Returns 0 for photometric interpretations PasTiff does not know. }
function TiffColorSampleCount(Photometric: Integer): Integer;

{ Reverses byte order of Count units of UnitSize (2, 3, 4 or 8) bytes in place. }
procedure TiffSwapBytes(Data: Pointer; UnitSize: Integer; Count: NativeInt);
{ Reverses bit order in each of Count bytes (FillOrder conversion). }
procedure TiffReverseBits(Data: Pointer; Count: NativeInt);

{ Floating point values from files can be anything, including infinities,
  NaNs and numbers that overflow in the next multiplication - and operations
  on those raise floating point exceptions in Pascal. These two functions
  look at the bits only and never raise.

  TiffSafeDouble returns the value if it is an ordinary number with magnitude
  below 1e30, otherwise 0. }
function TiffSafeDouble(P: Pointer): Double;
{ Converts a 64-bit float to the bits of a 32-bit one. Values beyond the
  range of Single become infinity, NaN stays NaN. }
function TiffDoubleToSingleBits(P: Pointer): Cardinal;

{ Overflow-checked A * B for sizes read from files. Both must be >= 0.
  Raises ETiffFormatError when the product does not fit NativeInt, which also
  rejects images too large for 32-bit targets. }
function TiffMulSize(A, B: Int64): NativeInt;

implementation

function TiffTypeSize(DataType: Word): Integer;
begin
  case DataType of
    ttByte, ttAscii, ttSByte, ttUndefined, ttUtf8: Result := 1;
    ttShort, ttSShort: Result := 2;
    ttLong, ttSLong, ttFloat, ttIFD: Result := 4;
    ttRational, ttSRational, ttDouble, ttLong8, ttSLong8, ttIFD8: Result := 8;
  else
    Result := 0;
  end;
end;

function TiffTypeSwapUnit(DataType: Word): Integer;
begin
  case DataType of
    ttRational, ttSRational: Result := 4;
  else
    Result := TiffTypeSize(DataType);
  end;
end;

function TiffCompressionName(Compression: Integer): string;
begin
  case Compression of
    TiffCompressionNone: Result := 'None';
    TiffCompressionCcittRle, TiffCompressionCcittRleW: Result := 'CCITT RLE';
    TiffCompressionCcittFax3: Result := 'CCITT Group 3 Fax';
    TiffCompressionCcittFax4: Result := 'CCITT Group 4 Fax';
    TiffCompressionLzw: Result := 'LZW';
    TiffCompressionOldJpeg: Result := 'Old JPEG';
    TiffCompressionJpeg: Result := 'JPEG';
    TiffCompressionAdobeDeflate, TiffCompressionDeflate: Result := 'Deflate';
    TiffCompressionPackBits: Result := 'PackBits';
    32809: Result := 'ThunderScan';
    32766: Result := 'NeXT';
    32908, 32909: Result := 'Pixar';
    34676, 34677: Result := 'SGI LogLuv';
    34661: Result := 'JBIG';
    34712: Result := 'JPEG 2000';
    34887, 50000..50002: Result := 'LERC/ZSTD/WebP';
    34925: Result := 'LZMA';
  else
    Result := 'Unknown (' + IntToStr(Compression) + ')';
  end;
end;

function TiffColorSampleCount(Photometric: Integer): Integer;
begin
  case Photometric of
    TiffPhotometricMinIsWhite, TiffPhotometricMinIsBlack,
    TiffPhotometricPalette, TiffPhotometricMask: Result := 1;
    TiffPhotometricRgb, TiffPhotometricYCbCr, TiffPhotometricCieLab: Result := 3;
    TiffPhotometricSeparated: Result := 4;
  else
    Result := 0;
  end;
end;

procedure TiffSwapBytes(Data: Pointer; UnitSize: Integer; Count: NativeInt);
var
  P: PByte;
  I: NativeInt;
  T: Byte;
begin
  P := Data;
  case UnitSize of
    2:
      for I := 0 to Count - 1 do
      begin
        T := P[0]; P[0] := P[1]; P[1] := T;
        Inc(P, 2);
      end;
    3:
      // 24-bit samples; libtiff swaps these too (TIFFSwabArrayOfTriples)
      for I := 0 to Count - 1 do
      begin
        T := P[0]; P[0] := P[2]; P[2] := T;
        Inc(P, 3);
      end;
    4:
      for I := 0 to Count - 1 do
      begin
        T := P[0]; P[0] := P[3]; P[3] := T;
        T := P[1]; P[1] := P[2]; P[2] := T;
        Inc(P, 4);
      end;
    8:
      for I := 0 to Count - 1 do
      begin
        T := P[0]; P[0] := P[7]; P[7] := T;
        T := P[1]; P[1] := P[6]; P[6] := T;
        T := P[2]; P[2] := P[5]; P[5] := T;
        T := P[3]; P[3] := P[4]; P[4] := T;
        Inc(P, 8);
      end;
  end;
end;

var
  BitRevTable: array[Byte] of Byte;

procedure InitBitRevTable;
var
  I, B, R: Integer;
begin
  for I := 0 to 255 do
  begin
    R := 0;
    for B := 0 to 7 do
      if (I and (1 shl B)) <> 0 then
        R := R or (1 shl (7 - B));
    BitRevTable[I] := R;
  end;
end;

procedure TiffReverseBits(Data: Pointer; Count: NativeInt);
var
  P: PByte;
  I: NativeInt;
begin
  P := Data;
  for I := 0 to Count - 1 do
    P[I] := BitRevTable[P[I]];
end;

function TiffSafeDouble(P: Pointer): Double;
var
  Bits: Int64;
  Exponent: Integer;
begin
  Move(P^, Bits, 8);
  Exponent := Integer((Bits shr 52) and $7FF) - 1023;
  // Exponent 1024 is infinity or NaN; 2^100 is about 1.3e30
  if Exponent >= 100 then
    Result := 0
  else
    Move(Bits, Result, 8);
end;

function TiffDoubleToSingleBits(P: Pointer): Cardinal;
var
  Bits: Int64;
  Exponent: Integer;
  Sign: Cardinal;
  S: Single;
  D: Double;
begin
  Move(P^, Bits, 8);
  Exponent := Integer((Bits shr 52) and $7FF) - 1023;
  Sign := Cardinal(Bits shr 63) shl 31;
  if Exponent = 1024 then
  begin
    // Infinity or NaN: keep which one it is
    if (Bits and $000FFFFFFFFFFFFF) = 0 then
      Result := Sign or $7F800000
    else
      Result := Sign or $7FC00000;
  end
  else if Exponent > 127 then
    Result := Sign or $7F800000   // too large for Single
  else if Exponent < -126 then
    Result := Sign                // too small: zero (no denormals)
  else
  begin
    Move(Bits, D, 8);
    S := D;
    Move(S, Result, 4);
  end;
end;

function TiffMulSize(A, B: Int64): NativeInt;
begin
  if (A < 0) or (B < 0) then
    raise ETiffFormatError.Create('Negative size in TIFF data');
  if (A <> 0) and (B > High(NativeInt) div A) then
    raise ETiffFormatError.Create('Image data size is too large');
  Result := NativeInt(A * B);
end;

initialization
  InitBitRevTable;

end.
