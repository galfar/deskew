{
  PasTiff - pure Pascal TIFF reader/writer
  by Marek Mauder
  https://github.com/galfar/imaginglib
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
}

{ Conversion of decoded TIFF rows to a small set of usual pixel layouts.

  TIFF allows any number of bits per sample, several meanings of the samples
  and two ways of arranging them. This unit turns whatever a page has into
  one of the layouts of TTiffPixelLayout, losing as little as possible:
  samples are widened to 8 or 16 bits (never narrowed below 16), palettes of
  more than 256 colors, CMYK and YCbCr become RGB, separate planes are
  interleaved, MinIsWhite is inverted so that 0 is always black.

  It is independent of TTiffReader: the input is a page description
  (TTiffPageInfo) and rows of decoded data in the form TIFF decoders deliver
  them (TTiffReader.ReadRows, or libtiff's TIFFReadEncodedStrip and
  TIFFReadScanline). It can therefore be used with libtiff as well, in place
  of libtiff's own TIFFReadRGBAImage which only produces 8-bit RGBA.

  Layouts that already are one of the targets pass through untouched
  (PassThrough is True and the caller may decode straight into its own
  buffer). Everything else goes through two generic stages instead of a
  routine per format as in libtiff's tif_getimage.c:
    1. unpack: read samples of any width, from one or several planes, and
       scale them to 8 or 16 bits,
    2. photometric: inversion, palette lookup, CMYK and YCbCr to RGB.
  That is slower than specialized loops, which is fine for layouts that are
  rare. Sample order of the output is that of TIFF: R, G, B, A.

  Not supported (Init returns False): CIE L*a*b*, LogLuv, YCbCr other than
  8-bit, floating point other than gray and RGB, integer samples over 32 bits. }
unit PasTiffPixels;

{$I PasTiff.inc}

interface

uses
  SysUtils, PasTiffTypes;

type
  TTiffPixelLayout = (
    tplUnknown,
    tplBilevel,        // 1 bit per pixel, most significant bit first, 0 = black
    tplGray8,
    tplGray16,
    tplGray32,         // 32-bit unsigned integer
    tplGrayAlpha8,
    tplGrayAlpha16,
    tplIndexed8,       // palette index, palette comes from TiffGetPalette8
    tplRgb8,
    tplRgb16,
    tplRgba8,
    tplRgba16,
    tplGrayFloat16,    // IEEE half float
    tplGrayFloat32,
    tplRgbaFloat16,
    tplRgbaFloat32);

  TTiffConvertKind = (tckNone, tckBilevelInvert, tckInteger, tckFloat, tckYCbCr);
  TTiffColorOp = (tcoNone, tcoPalette16, tcoCmyk);

  { Converts rows of one page. Call Init once, then ConvertRows for any
    groups of rows. The record is self-contained and can be copied. }
  TTiffRowConverter = record
  public
    { Layout ConvertRows produces. }
    Layout: TTiffPixelLayout;
    { True if the decoded rows already are in Layout: ConvertRows then just
      copies and callers can skip it. }
    PassThrough: Boolean;
    Width: Integer;
    { Size of one input row (of one plane) and of one output row in bytes. }
    SrcRowBytes: NativeInt;
    DstRowBytes: NativeInt;
    { Number of planes ConvertRows expects in SrcPlanes: 1, or
      SamplesPerPixel for PlanarConfiguration=2. }
    PlaneCount: Integer;
    { Image rows produced by one input row: 1, or the vertical subsampling
      factor for subsampled YCbCr, whose rows hold blocks of pixels. }
    RowsPerSrcRow: Integer;
    { Output has alpha that came from the file (not a filled-in opaque one). }
    HasAlpha: Boolean;
    { That alpha is premultiplied ("associated" in TIFF). Colors are passed
      on as stored either way. }
    AlphaPremultiplied: Boolean;

    { Prepares conversion for the page. Returns False if the page's pixel
      format is not supported. }
    function Init(const APage: TTiffPageInfo): Boolean;
    { Converts SrcRowCount input rows. SrcPlanes holds pointers to the first
      row of each plane (rows follow each other SrcRowBytes apart). Output
      rows are written DstStride bytes apart, at most MaxDstRows of them
      (the last input row of subsampled YCbCr can hold more image rows than
      the image has left). Returns number of rows written. }
    function ConvertRows(const SrcPlanes: array of Pointer; Dst: Pointer;
      DstStride: NativeInt; SrcRowCount, MaxDstRows: Integer): Integer;
  private
    FPage: TTiffPageInfo;
    FKind: TTiffConvertKind;
    FOp: TTiffColorOp;
    FBits: Integer;            // bits per input sample
    FSamples: Integer;         // samples per pixel in the input
    FPlanar: Boolean;
    FChannels: Integer;        // samples per pixel that are read
    FTargetBits: Integer;      // 8 or 16: sample size after unpacking
    FScale: Boolean;           // scale samples to the full range (not for palette indices)
    FInvert: Boolean;          // MinIsWhite: invert color samples
    FSigned: Boolean;
    FColorChannels: Integer;   // of FChannels, the rest is alpha
    FPacked: Boolean;          // subsampled YCbCr in blocks
    FTemp: TBytes;
    { YCbCr to RGB tables, 16.16 fixed point }
    FYTab, FCrR, FCbB, FCrG, FCbG: array[Byte] of Integer;
    function GetSample(Row: PByte; Index: NativeInt): Cardinal;
    procedure UnpackRow(const Rows: array of PByte; Dst: PByte);
    procedure ConvertFloatRow(const Rows: array of PByte; Dst: PByte);
    procedure InitYCbCr;
    procedure YCbCrPixel(Y, Cb, Cr: Byte; Dst: PByte); inline;
  end;

{ Bytes per pixel of a layout, 0 for tplBilevel and tplUnknown. }
function TiffLayoutBytesPerPixel(Layout: TTiffPixelLayout): Integer;

{ Palette of a page with PhotometricInterpretation=Palette as 8-bit RGB
  triples in Dst (room for 3 * Count bytes). Count is at most 256 and is
  cut to the number of colors the page has. Returns number of colors written. }
function TiffGetPalette8(const Page: TTiffPageInfo; Dst: PByte; Count: Integer): Integer;

implementation

const
  HalfOne = $3C00;   // 1.0 as IEEE half float

function TiffLayoutBytesPerPixel(Layout: TTiffPixelLayout): Integer;
const
  Sizes: array[TTiffPixelLayout] of Integer = (0, 0, 1, 2, 4, 2, 4, 1, 3, 6, 4, 8, 2, 4, 8, 16);
begin
  Result := Sizes[Layout];
end;

{ TIFF color maps hold 16-bit values, 65535 being full intensity. Some old
  writers stored 8-bit values (0..255) instead; such a palette would come out
  almost black. libtiff's checkcmap ("Assuming 8-bit colormap") detects them
  by all values being below 256, and so do we. }
function ColorMapIs8Bit(const Page: TTiffPageInfo): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to High(Page.ColorMap) do
    if Page.ColorMap[I] >= 256 then
    begin
      Result := False;
      Exit;
    end;
end;

function TiffGetPalette8(const Page: TTiffPageInfo; Dst: PByte; Count: Integer): Integer;
var
  I, Colors: Integer;
  Shift: Integer;
begin
  Colors := Length(Page.ColorMap) div 3;
  if Count > Colors then
    Count := Colors;
  if Count > 256 then
    Count := 256;
  if ColorMapIs8Bit(Page) then
    Shift := 0
  else
    Shift := 8;
  // Color map is stored as all reds, then all greens, then all blues
  for I := 0 to Count - 1 do
  begin
    Dst[I * 3] := Page.ColorMap[I] shr Shift;
    Dst[I * 3 + 1] := Page.ColorMap[Colors + I] shr Shift;
    Dst[I * 3 + 2] := Page.ColorMap[2 * Colors + I] shr Shift;
  end;
  Result := Count;
end;

{ TTiffRowConverter }

function TTiffRowConverter.Init(const APage: TTiffPageInfo): Boolean;
var
  ColorSamples: Integer;
  Alpha, IsUInt: Boolean;
  RowSamples: Int64;

  function IntLayout(Gray: Boolean): TTiffPixelLayout;
  begin
    if Gray then
    begin
      if FTargetBits = 8 then
        if Alpha then Result := tplGrayAlpha8 else Result := tplGray8
      else
        if Alpha then Result := tplGrayAlpha16 else Result := tplGray16;
    end
    else
      if FTargetBits = 8 then
        if Alpha then Result := tplRgba8 else Result := tplRgb8
      else
        if Alpha then Result := tplRgba16 else Result := tplRgb16;
  end;

begin
  Result := False;
  FPage := APage;
  Layout := tplUnknown;
  PassThrough := False;
  Width := APage.Width;
  FBits := APage.BitsPerSample;
  FSamples := APage.SamplesPerPixel;
  FPlanar := (APage.PlanarConfig = TiffPlanarSeparate) and (FSamples > 1);
  if FPlanar then PlaneCount := FSamples else PlaneCount := 1;
  RowsPerSrcRow := 1;
  FKind := tckNone;
  FOp := tcoNone;
  FScale := True;
  FInvert := False;
  FPacked := False;
  FTemp := nil;
  FSigned := APage.SampleFormat = TiffSampleFormatInt;
  IsUInt := APage.SampleFormat = TiffSampleFormatUInt;
  if FBits <= 8 then FTargetBits := 8 else FTargetBits := 16;
  if (Width <= 0) or (FBits < 1) or (FSamples < 1) then
    Exit;

  // The first sample beyond those of the color model is taken for alpha,
  // whatever ExtraSamples says about it: plenty of RGBA files declare it as
  // "unspecified" (libtiff's RGBA reader has a workaround for exactly that).
  // Further extra samples are dropped.
  ColorSamples := TiffColorSampleCount(APage.Photometric);
  Alpha := (ColorSamples > 0) and (FSamples > ColorSamples);
  HasAlpha := Alpha;
  AlphaPremultiplied := Alpha and (APage.ExtraSamples[0] = TiffExtraSampleAssocAlpha);
  FColorChannels := ColorSamples;
  if Alpha then FChannels := ColorSamples + 1 else FChannels := ColorSamples;

  if APage.SampleFormat = TiffSampleFormatFloat then
  begin
    if not ((FBits = 16) or (FBits = 32) or (FBits = 64)) then
      Exit;
    FKind := tckFloat;
    case APage.Photometric of
      TiffPhotometricMinIsBlack, TiffPhotometricMinIsWhite:
        begin
          if FSamples <> 1 then
            Exit;
          if FBits = 16 then Layout := tplGrayFloat16 else Layout := tplGrayFloat32;
          PassThrough := FBits <> 64;
        end;
      TiffPhotometricRgb:
        begin
          if FSamples < 3 then
            Exit;
          if FBits = 16 then Layout := tplRgbaFloat16 else Layout := tplRgbaFloat32;
          PassThrough := (FBits <> 64) and (FSamples = 4) and not FPlanar;
        end;
    else
      Exit;
    end;
  end
  else
  begin
    if FBits > 32 then
      Exit;
    FKind := tckInteger;
    case APage.Photometric of
      TiffPhotometricPalette:
        begin
          if Length(APage.ColorMap) < 3 * (Int64(1) shl FBits) then
            Exit;
          FChannels := 1;
          FColorChannels := 1;
          HasAlpha := False;
          if FBits <= 8 then
          begin
            Layout := tplIndexed8;
            FScale := False;
            PassThrough := (FBits = 8) and (FSamples = 1);
          end
          else if FBits <= 16 then
          begin
            // More than 256 colors: no indexed layout can hold it, colors
            // are looked up (color map values are 16-bit, nothing is lost)
            Layout := tplRgb16;
            FScale := False;
            FOp := tcoPalette16;
          end
          else
            Exit;
        end;
      TiffPhotometricMinIsWhite, TiffPhotometricMinIsBlack, TiffPhotometricMask:
        begin
          FInvert := APage.Photometric = TiffPhotometricMinIsWhite;
          if (FBits = 1) and (FSamples = 1) then
          begin
            Layout := tplBilevel;
            if FInvert then
              FKind := tckBilevelInvert
            else
              PassThrough := True;
          end
          else if (FBits = 32) and (FSamples = 1) and IsUInt and not FInvert then
          begin
            Layout := tplGray32;
            PassThrough := True;
          end
          else
          begin
            Layout := IntLayout(True);
            PassThrough := (FBits = FTargetBits) and (FSamples = FChannels) and
              not FInvert and not FPlanar and IsUInt;
          end;
        end;
      TiffPhotometricRgb:
        begin
          if FSamples < 3 then
            Exit;
          Layout := IntLayout(False);
          PassThrough := (FBits = FTargetBits) and (FSamples = FChannels) and
            not FPlanar and IsUInt;
        end;
      TiffPhotometricSeparated:
        begin
          // Only CMYK; other ink sets would need the inks' colors
          if (FSamples < 4) or (APage.InkSet <> TiffInkSetCmyk) then
            Exit;
          FChannels := 4;
          FColorChannels := 4;
          HasAlpha := False;
          Alpha := False;
          Layout := IntLayout(False);
          FOp := tcoCmyk;
        end;
      TiffPhotometricYCbCr:
        begin
          if (FBits <> 8) or (FSamples < 3) then
            Exit;
          FKind := tckYCbCr;
          FChannels := 3;
          HasAlpha := False;
          Layout := tplRgb8;
          FPacked := not FPlanar and
            ((APage.YCbCrSubsampleH <> 1) or (APage.YCbCrSubsampleV <> 1));
          // Separate planes of different sizes are not handled
          if FPlanar and ((APage.YCbCrSubsampleH <> 1) or (APage.YCbCrSubsampleV <> 1)) then
            Exit;
          if FPacked then
          begin
            if not (APage.YCbCrSubsampleH in [1, 2, 4]) or not (APage.YCbCrSubsampleV in [1, 2, 4]) or
               (FSamples <> 3) then
              Exit;
            RowsPerSrcRow := APage.YCbCrSubsampleV;
          end;
          InitYCbCr;
        end;
    else
      Exit;
    end;
  end;

  // Row sizes; same arithmetic as in TTiffReader and libtiff's TIFFScanlineSize
  if FPacked then
    RowSamples := Int64((Width + APage.YCbCrSubsampleH - 1) div APage.YCbCrSubsampleH) *
      (APage.YCbCrSubsampleH * APage.YCbCrSubsampleV + 2)
  else if FPlanar then
    RowSamples := Width
  else
    RowSamples := Int64(Width) * FSamples;
  SrcRowBytes := TiffMulSize((RowSamples * FBits + 7) div 8, 1);
  if Layout = tplBilevel then
    DstRowBytes := (NativeInt(Width) + 7) div 8
  else
    DstRowBytes := TiffMulSize(Width, TiffLayoutBytesPerPixel(Layout));

  // Unpacked samples go to a temporary row when a color operation follows
  if (FKind = tckInteger) and (FOp <> tcoNone) or (FKind = tckYCbCr) and not FPacked then
    SetLength(FTemp, TiffMulSize(Width, FChannels * (FTargetBits div 8)));
  Result := True;
end;

{ Sample number Index of a row, for any sample size. Samples of 16, 24 and
  32 bits are integers in the CPU's byte order (decoders swap them); all
  other sizes are bit fields packed most significant bit first, a row always
  starting at a byte boundary. }
function TTiffRowConverter.GetSample(Row: PByte; Index: NativeInt): Cardinal;
var
  BitPos: Int64;
  P: PByte;
  Acc: UInt64;
  Have, Shift: Integer;
begin
  case FBits of
    8: Result := Row[Index];
    16: Result := PWord(Row)[Index];
    32: Result := PCardinal(Row)[Index];
    24:
      begin
        // Little-endian like the other sizes
        P := Row + Index * 3;
        Result := (Cardinal(P[2]) shl 16) or (Cardinal(P[1]) shl 8) or P[0];
      end;
  else
    begin
      BitPos := Int64(Index) * FBits;
      P := Row + (BitPos shr 3);
      Shift := BitPos and 7;
      Acc := 0;
      Have := 0;
      // The bytes read here all belong to the row: the last bit needed is
      // the last bit of the sample
      while Have < Shift + FBits do
      begin
        Acc := (Acc shl 8) or P^;
        Inc(P);
        Inc(Have, 8);
      end;
      Result := Cardinal((Acc shr (Have - Shift - FBits)) and ((UInt64(1) shl FBits) - 1));
    end;
  end;
end;

{ Stage 1: reads FChannels samples of every pixel and stores them as 8 or
  16-bit values, scaled to the full range and with MinIsWhite undone. }
procedure TTiffRowConverter.UnpackRow(const Rows: array of PByte; Dst: PByte);
var
  X, C: Integer;
  V, MaxSrc, MaxDst: Int64;
  D16: PWord;
begin
  MaxSrc := (Int64(1) shl FBits) - 1;
  MaxDst := (Int64(1) shl FTargetBits) - 1;
  D16 := PWord(Dst);
  for X := 0 to Width - 1 do
    for C := 0 to FChannels - 1 do
    begin
      if FPlanar then
        V := GetSample(Rows[C], X)
      else
        V := GetSample(Rows[0], NativeInt(X) * FSamples + C);
      // Signed integers are shifted to the unsigned range, keeping their order
      if FSigned then
        V := V xor (Int64(1) shl (FBits - 1));
      if FScale then
      begin
        if FBits < FTargetBits then
          // Like libtiff's gray maps: value * max / source max, so that
          // the largest source value becomes full intensity
          V := (V * MaxDst) div MaxSrc
        else if FBits > FTargetBits then
          V := V shr (FBits - FTargetBits);
        if FInvert and (C < FColorChannels) then
          V := MaxDst - V;
      end;
      if FTargetBits = 8 then
      begin
        Dst^ := Byte(V);
        Inc(Dst);
      end
      else
      begin
        D16^ := Word(V);
        Inc(D16);
      end;
    end;
end;

{ Floating point gray and RGB. 64-bit floats are narrowed to 32 bits, RGB
  without alpha gets an opaque one. }
procedure TTiffRowConverter.ConvertFloatRow(const Rows: array of PByte; Dst: PByte);
var
  X, C, DstChannels: Integer;
  Src: PByte;
  Index: NativeInt;
begin
  if Layout in [tplGrayFloat16, tplGrayFloat32] then
    DstChannels := 1
  else
    DstChannels := 4;
  for X := 0 to Width - 1 do
    for C := 0 to DstChannels - 1 do
    begin
      if C < FSamples then
      begin
        if FPlanar then
        begin
          Src := Rows[C];
          Index := X;
        end
        else
        begin
          Src := Rows[0];
          Index := NativeInt(X) * FSamples + C;
        end;
        case FBits of
          16: PWord(Dst)^ := PWord(Src)[Index];
          32: PCardinal(Dst)^ := PCardinal(Src)[Index];
          // Not a plain assignment: that raises an exception for values
          // outside the range of Single
          64: PCardinal(Dst)^ := TiffDoubleToSingleBits(@PDouble(Src)[Index]);
        end;
      end
      else if FBits = 16 then
        PWord(Dst)^ := HalfOne
      else
        PSingle(Dst)^ := 1.0;
      if FBits = 16 then Inc(Dst, 2) else Inc(Dst, 4);
    end;
end;

{ Tables for YCbCr to RGB, after libtiff's TIFFYCbCrToRGBInit (tif_color.c):
    R = Y + (2 - 2*LumaRed) * Cr
    B = Y + (2 - 2*LumaBlue) * Cb
    G = (Y - LumaRed*R - LumaBlue*B) / LumaGreen
  where Y, Cb, Cr are first mapped from the stored codes to nominal ranges
  with the ReferenceBlackWhite pairs (headroom/footroom coding). }
procedure TTiffRowConverter.InitYCbCr;
const
  One = 65536.0;
var
  I: Integer;
  LumaR, LumaG, LumaB, F1, F2, F3, F4, Cb, Cr, Y: Double;

  function Code2V(Code: Integer; Black, White: Double; const Range: Double): Double;
  var
    Span: Double;
  begin
    // Reference values are sample codes; anything far outside is garbage
    if Abs(Black) > 65536 then Black := 0;
    if Abs(White) > 65536 then White := 255;
    Span := White - Black;
    if Abs(Span) < 1e-3 then
      Span := 1;
    Result := (Code - Black) * Range / Span;
  end;

begin
  LumaR := FPage.YCbCrCoefficients[0];
  LumaG := FPage.YCbCrCoefficients[1];
  LumaB := FPage.YCbCrCoefficients[2];
  // Luma weights are fractions that add up to 1. Nonsense values (the tags
  // come from the file) are replaced by the standard ones so that the
  // arithmetic below stays in range.
  if (LumaR < 0) or (LumaR > 1) or (LumaG <= 0.01) or (LumaG > 1) or (LumaB < 0) or (LumaB > 1) then
  begin
    LumaR := 0.299;
    LumaG := 0.587;
    LumaB := 0.114;
  end;
  F1 := 2 - 2 * LumaR;
  F2 := LumaR * F1 / LumaG;
  F3 := 2 - 2 * LumaB;
  F4 := LumaB * F3 / LumaG;
  for I := 0 to 255 do
  begin
    Y := Code2V(I, FPage.ReferenceBlackWhite[0], FPage.ReferenceBlackWhite[1], 255);
    Cb := Code2V(I, FPage.ReferenceBlackWhite[2], FPage.ReferenceBlackWhite[3], 127);
    Cr := Code2V(I, FPage.ReferenceBlackWhite[4], FPage.ReferenceBlackWhite[5], 127);
    // Clamped like in libtiff so that odd reference values cannot overflow
    if Y < -4096 then Y := -4096 else if Y > 4096 then Y := 4096;
    if Cb < -4096 then Cb := -4096 else if Cb > 4096 then Cb := 4096;
    if Cr < -4096 then Cr := -4096 else if Cr > 4096 then Cr := 4096;
    FYTab[I] := Round(Y * One);
    FCrR[I] := Round(F1 * Cr * One);
    FCbB[I] := Round(F3 * Cb * One);
    FCrG[I] := Round(-F2 * Cr * One);
    FCbG[I] := Round(-F4 * Cb * One);
  end;
end;

procedure TTiffRowConverter.YCbCrPixel(Y, Cb, Cr: Byte; Dst: PByte);
var
  V: Integer;
begin
  V := (FYTab[Y] + FCrR[Cr] + 32768) div 65536;
  if V < 0 then V := 0 else if V > 255 then V := 255;
  Dst[0] := V;
  V := (FYTab[Y] + FCbG[Cb] + FCrG[Cr] + 32768) div 65536;
  if V < 0 then V := 0 else if V > 255 then V := 255;
  Dst[1] := V;
  V := (FYTab[Y] + FCbB[Cb] + 32768) div 65536;
  if V < 0 then V := 0 else if V > 255 then V := 255;
  Dst[2] := V;
end;

function TTiffRowConverter.ConvertRows(const SrcPlanes: array of Pointer; Dst: Pointer;
  DstStride: NativeInt; SrcRowCount, MaxDstRows: Integer): Integer;
var
  Rows: array of PByte;
  Row, P, X, I, SubH, SubV, BX, BY, Blocks, MapColors, MapShift: Integer;
  Out_, S, D: PByte;
  S16, D16: PWord;
  K, MaxV: Int64;
  Index: Cardinal;
begin
  Result := 0;
  if Length(SrcPlanes) < PlaneCount then
    raise ETiffError.Create('Not enough planes given to TIFF row converter');
  SetLength(Rows, PlaneCount);
  Out_ := Dst;
  MapColors := Length(FPage.ColorMap) div 3;
  MapShift := 0;
  if (FOp = tcoPalette16) and ColorMapIs8Bit(FPage) then
    MapShift := 8;

  for Row := 0 to SrcRowCount - 1 do
  begin
    if Result >= MaxDstRows then
      Break;
    for P := 0 to PlaneCount - 1 do
      Rows[P] := PByte(SrcPlanes[P]) + Row * SrcRowBytes;

    if PassThrough then
      Move(Rows[0]^, Out_^, DstRowBytes)
    else
      case FKind of
        tckBilevelInvert:
          for X := 0 to DstRowBytes - 1 do
            Out_[X] := not Rows[0][X];

        tckFloat:
          ConvertFloatRow(Rows, Out_);

        tckInteger:
          case FOp of
            tcoNone:
              UnpackRow(Rows, Out_);
            tcoPalette16:
              begin
                UnpackRow(Rows, @FTemp[0]);
                S16 := @FTemp[0];
                D16 := PWord(Out_);
                for X := 0 to Width - 1 do
                begin
                  Index := S16[X];
                  D16[0] := FPage.ColorMap[Index] shl MapShift;
                  D16[1] := FPage.ColorMap[MapColors + Integer(Index)] shl MapShift;
                  D16[2] := FPage.ColorMap[2 * MapColors + Integer(Index)] shl MapShift;
                  Inc(D16, 3);
                end;
              end;
            tcoCmyk:
              begin
                // The usual naive conversion without color management, as in
                // libtiff's RGBA reader: C, M, Y are scaled by the amount of
                // light that black lets through
                UnpackRow(Rows, @FTemp[0]);
                MaxV := (Int64(1) shl FTargetBits) - 1;
                if FTargetBits = 8 then
                begin
                  S := @FTemp[0];
                  D := Out_;
                  for X := 0 to Width - 1 do
                  begin
                    K := MaxV - S[3];
                    D[0] := (K * (MaxV - S[0])) div MaxV;
                    D[1] := (K * (MaxV - S[1])) div MaxV;
                    D[2] := (K * (MaxV - S[2])) div MaxV;
                    Inc(S, 4);
                    Inc(D, 3);
                  end;
                end
                else
                begin
                  S16 := @FTemp[0];
                  D16 := PWord(Out_);
                  for X := 0 to Width - 1 do
                  begin
                    K := MaxV - S16[3];
                    D16[0] := (K * (MaxV - S16[0])) div MaxV;
                    D16[1] := (K * (MaxV - S16[1])) div MaxV;
                    D16[2] := (K * (MaxV - S16[2])) div MaxV;
                    Inc(S16, 4);
                    Inc(D16, 3);
                  end;
                end;
              end;
          end;

        tckYCbCr:
          if not FPacked then
          begin
            UnpackRow(Rows, @FTemp[0]);
            S := @FTemp[0];
            for X := 0 to Width - 1 do
            begin
              YCbCrPixel(S[0], S[1], S[2], Out_ + X * 3);
              Inc(S, 3);
            end;
          end
          else
          begin
            // One input row is a row of blocks; each block has SubH * SubV
            // luma samples (row by row) followed by Cb and Cr shared by all
            // its pixels, and becomes SubV image rows.
            SubH := FPage.YCbCrSubsampleH;
            SubV := FPage.YCbCrSubsampleV;
            Blocks := (Width + SubH - 1) div SubH;
            S := Rows[0];
            for I := 0 to Blocks - 1 do
            begin
              for BY := 0 to SubV - 1 do
                if Result + BY < MaxDstRows then
                  for BX := 0 to SubH - 1 do
                    if I * SubH + BX < Width then
                      YCbCrPixel(S[BY * SubH + BX], S[SubH * SubV], S[SubH * SubV + 1],
                        Out_ + BY * DstStride + (I * SubH + BX) * 3);
              Inc(S, SubH * SubV + 2);
            end;
          end;
      end;

    if FPacked then
    begin
      I := RowsPerSrcRow;
      if I > MaxDstRows - Result then
        I := MaxDstRows - Result;
      Inc(Result, I);
      Inc(Out_, I * DstStride);
    end
    else
    begin
      Inc(Result);
      Inc(Out_, DstStride);
    end;
  end;
end;

end.
