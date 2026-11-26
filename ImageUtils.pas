{
  Deskew
  by Marek Mauder
  https://galfar.vevb.net/deskew
  https://github.com/galfar/deskew
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
}

{
  Unit with various image processing functions. Some are taken from
  Imaging extensions.
}
unit ImageUtils;

{$I ImagingOptions.inc}

interface

uses
  Types,
  Math,
  SysUtils,
  Classes,
  ImagingTypes,
  Imaging,
  ImagingFormats,
  ImagingUtility;

function IsImageDataEqual(const Image1, Image2: TImageData): Boolean;

{ Thresholding using Otsu's method (which chooses the threshold
  to minimize the intraclass variance of the black and white pixels!).
  Function returns calculated threshold level value [0..255].
  If AContentRect is specified, the threshold calculation is performed only
  within this rectangle. Otherwise, the whole image is processed.}
function OtsuThresholding(var Image: TImageData; AContentRect: PRect = nil): Integer;

{ Image is converted to binary using given threshold level value [0..255].
  If AContentRect is specified, the threshold calculation is performed only
  within this rectangle. Otherwise, the whole image is processed.}
procedure BinarizeImage(var Image: TImageData; Threshold: Integer; AContentRect: PRect = nil);

{ Draws a frame of given thickness and color along the edges of ARect,
  inside the rectangle or, if Outside is True, around it. Parts of the frame
  outside the image are left out. Used to mark areas in the work image. }
procedure DrawRectFrame(var Image: TImageData; const ARect: TRect; Thickness: Integer;
  Color: TColor32; Outside: Boolean);

{ Finds the area of 8bit grayscale image with actual content (text, pictures, ...)
  using given threshold level value [0..255] to tell the content from the background.
  Dark areas connected to the edges (scanner bed around the page, shadows)
  and sparse dark pixels (dust, noise) are not considered to be content.
  If ASearchRect is specified, only this part of the image is examined.
  Returns False when there is no content to be found (blank page). }
function DetectContentRect(const Image: TImageData; Threshold: Integer;
  out ContentRect: TRect; ASearchRect: PRect = nil): Boolean;

type
  TResamplingFilter = (
    rfNearest,
    rfLinear,
    rfCubic,
    rfLanczos
  );

const
  SupportedRotationFormats: set of TImageFormat = [ifGray8, ifR8G8B8, ifA8R8G8B8];

{ Rotates (CCW direction) image with a background (of outside "void" areas) of specified color.
  The image is resized to fit the whole rotated image if FitRotated = True,
  otherwise it's clipped to the original size (useful to avoid "voids" for small rotations
  e.g. deskewing scanned documents).
  With
  With FitRotated = True, the corners of the rotated image will always have BackgroundColor
  (except 90/180/270 angle). }
procedure RotateImage(var Image: TImageData; Angle: Double; BackgroundColor: TColor32;
  ResamplingFilter: TResamplingFilter; FitRotated: Boolean);

{ Returns the format (one of SupportedRotationFormats) the image must be converted
  to before it is rotated with the given background color: the closest one
  to the format of the image, with alpha when the image or the background has it
  and with color when the background is not gray. }
function GetRotationFormat(const Image: TImageData; BackgroundColor: TColor32): TImageFormat;

implementation

uses
  ImagingCanvases,
  Utils;

function IsImageDataEqual(const Image1, Image2: TImageData): Boolean;
var
  FormatInfo: TImageFormatInfo;
begin
  // 1. No pixel data?
  if not Assigned(Image1.Bits) and not Assigned(Image2.Bits) then
    Exit(True); // Both are effectively empty and thus "equal" in that sense
  if not Assigned(Image1.Bits) or not Assigned(Image2.Bits) then
    Exit(False); // One is empty, the other is not

  // 2. Check basic properties
  if Image1.Width <> Image2.Width then
    Exit(False);
  if Image1.Height <> Image2.Height then
    Exit(False);
  if Image1.Format <> Image2.Format then
    Exit(False);
  if Image1.Size <> Image2.Size then
    Exit(False);

  // If dimensions are zero, they are equal at this point
  if (Image1.Width = 0) or (Image1.Height = 0) then
    Exit(True);

  // 3. Compare Palettes (applicable for ifIndex8)
  if Assigned(Image1.Palette) and Assigned(Image2.Palette) then
  begin
    GetImageFormatInfo(Image1.Format, FormatInfo);
    // If both have palettes, compare their content
    if not CompareMem(Image1.Palette, Image2.Palette, SizeOf(Image1.Palette[0]) * FormatInfo.PaletteEntries)  then
      Exit(False);
  end
  else if Assigned(Image1.Palette) or Assigned(Image2.Palette) then
  begin
    // One has a palette, the other doesn't. They are not equal.
    Exit(False);
  end;

  // 4. Compare pixel data
  if not CompareMem(Image1.Bits, Image2.Bits, Image1.Size) then
    Exit(False);

  // All checks passed
  Result := True;
end;

function OtsuThresholding(var Image: TImageData; AContentRect: PRect): Integer;
var
  Counts: array[Byte] of Int64;
  // Share of each level in the rect
  Histogram: array[Byte] of Double;
  NumPixelsInRect: Int64;
  Level, Max, Min, I, J, X, Y: Integer;
  PixPtr: PByte;
  Mean: Single;
  Mu, Omega, LevelMean, LargestMu: Single;
  EffectiveRect, ImageBounds: TRect;
begin
  Assert(Image.Format = ifGray8, 'OtsuThresholding requires an 8-bit grayscale image.');
  ImageBounds := Rect(0, 0, Image.Width, Image.Height);
  EffectiveRect := NullRect;

  // Determine the effective rectangle for processing
  if Assigned(AContentRect) then
  begin
    if not IsRectEmpty(AContentRect^) then
      IntersectRect(EffectiveRect, AContentRect^, ImageBounds)
  end
  else
    EffectiveRect := ImageBounds;

  NumPixelsInRect := Int64(RectWidth(EffectiveRect)) * RectHeight(EffectiveRect);
  if NumPixelsInRect <= 0 then
    Exit(128); // Default threshold if rect is empty

  FillChar(Counts, SizeOf(Counts), 0);
  FillChar(Histogram, SizeOf(Histogram), 0);
  Level := 0;

  // Count the pixels of each level within the EffectiveRect
  for Y := EffectiveRect.Top to EffectiveRect.Bottom - 1 do
  begin
    PixPtr := @PBuffer(Image.Bits)[NativeInt(Y) * Image.Width + EffectiveRect.Left];
    for X := EffectiveRect.Left to EffectiveRect.Right - 1 do
    begin
      Inc(Counts[PixPtr^]);
      Inc(PixPtr);
    end;
  end;

  // Range of the levels found (the rect is not empty, so there is at least one)
  Min := 0;
  while Counts[Min] = 0 do
    Inc(Min);
  Max := 255;
  while Counts[Max] = 0 do
    Dec(Max);

  // Normalize histogram based on pixels in the effective rectangle
  for I := Min to Max do // Iterate only over the range of pixel values found
  begin
    // Divided as a Double like the counts before, thresholds stay the same on all CPUs
    Histogram[I] := Counts[I];
    Histogram[I] := Histogram[I] / NumPixelsInRect;
  end;

  // Compute image mean using the histogram of the EffectiveRect
  Mean := 0.0;
  for I := Min to Max do
    Mean := Mean + (I + 1) * Histogram[I];

  // Now finally compute threshold level based on the histogram of EffectiveRect
  LargestMu := 0.0;

  // Iterate from Min to Max pixel values found in the rect
  for I := Min to Max do
  begin
    Omega := 0.0;
    LevelMean := 0.0;

    for J := Min to I - 1 do
    begin
      Omega := Omega + Histogram[J];
      LevelMean := LevelMean + (J + 1) * Histogram[J];
    end;

    Mu := Sqr(Mean * Omega - LevelMean);
    Omega := Omega * (1.0 - Omega);

    if (Omega > 1E-6) and (Omega < (1.0 - 1E-6)) then // Check if Omega is not too close to 0 or 1
      Mu := Mu / Omega
    else
      Mu := 0.0;

    if Mu > LargestMu then
    begin
      LargestMu := Mu;
      Level := I;
    end;
  end;

  // If Max = Min (solid color rectangle), Level might remain 0 or Min.
  // If no clear separation, Level might be at one of the extremes.
  // Add a check: if level is 0 and Max > 0, or level is 255 and Min < 255,
  // it might indicate a poor threshold. For now, use as calculated.
  // If Min = Max, Level will likely be Min.
  if Min = Max then Level := Min;

  Result := Level;
end;

procedure DrawRectFrame(var Image: TImageData; const ARect: TRect; Thickness: Integer;
  Color: TColor32; Outside: Boolean);
var
  Canvas: TImagingCanvas;
  R: TRect;
  T: Integer;
begin
  T := Thickness;
  R := ARect;
  if Outside then
    InflateRect(R, T, T);

  // Canvas clips the edges to the image, empty ones are skipped
  Canvas := TImagingCanvas.CreateForData(@Image);
  try
    Canvas.FillColor32 := Color;
    Canvas.FillRect(Rect(R.Left, R.Top, R.Right, R.Top + T));
    Canvas.FillRect(Rect(R.Left, R.Bottom - T, R.Right, R.Bottom));
    Canvas.FillRect(Rect(R.Left, R.Top + T, R.Left + T, R.Bottom - T));
    Canvas.FillRect(Rect(R.Right - T, R.Top + T, R.Right, R.Bottom - T));
  finally
    Canvas.Free;
  end;
end;

procedure BinarizeImage(var Image: TImageData; Threshold: Integer; AContentRect: PRect);
var
  X, Y: Integer;
  EffectiveRect, ImageBounds: TRect;
  PixPtr: PByte;
begin
  Assert(Image.Format = ifGray8);
  ImageBounds := Rect(0, 0, Image.Width, Image.Height);

  if Assigned(AContentRect) and (RectWidth(AContentRect^) > 0) and (RectHeight(AContentRect^) > 0) then
    IntersectRect(EffectiveRect, AContentRect^, ImageBounds)
  else
    EffectiveRect := ImageBounds;

  // Do thresholding using given level, only within EffectiveRect
  for Y := EffectiveRect.Top to EffectiveRect.Bottom - 1 do
  begin
    PixPtr := @PBuffer(Image.Bits)[NativeInt(Y) * Image.Width + EffectiveRect.Left];
    for X := EffectiveRect.Left to EffectiveRect.Right - 1 do
    begin
      if PixPtr^ >= Threshold then
        PixPtr^ := 255
      else
        PixPtr^ := 0;
      Inc(PixPtr);
    end;
  end;
end;

function DetectContentRect(const Image: TImageData; Threshold: Integer;
  out ContentRect: TRect; ASearchRect: PRect): Boolean;
const
  // Rows and columns with less content pixels than this fraction of their
  // length are considered empty (dust, noise)
  NoiseFloorFraction = 0.002;
  MinNoiseFloor = 2;
  // Content starts with this many non-empty rows/columns in a row
  MinContentRun = 3;
  // Less content pixels than this is no content at all
  MinContentPixels = 32;
  // Found area is padded by this fraction of the smaller side of the searched area
  PaddingFraction = 0.01;
var
  Bounds: TRect;
  W, H, X, Y, OpenCount, RunLeft, RunRight, Padding: Integer;
  FirstRow, LastRow, FirstCol, LastCol: Integer;
  ContentPixels: Int64;
  Row: PBuffer;
  RowCounts, ColCounts, TopRuns, BottomRuns: array of Integer;
  ColOpen: array of Boolean;

  function GetRow(Y: Integer): PBuffer;
  begin
    Result := @PBuffer(Image.Bits)[NativeInt(Bounds.Top + Y) * Image.Width + Bounds.Left];
  end;

  // Returns index where the first (or last) run of non-empty items starts,
  // -1 if there is none.
  function FindContentEdge(const Counts: array of Integer; NoiseFloor: Integer; FromEnd: Boolean): Integer;
  var
    I, Idx, Run, Len: Integer;
  begin
    Result := -1;
    Len := Length(Counts);
    Run := 0;
    for I := 0 to Len - 1 do
    begin
      if FromEnd then
        Idx := Len - 1 - I
      else
        Idx := I;

      if Counts[Idx] >= NoiseFloor then
      begin
        Inc(Run);
        if (Run >= MinContentRun) or (Run >= Len) then
        begin
          if FromEnd then
            Result := Idx + Run - 1
          else
            Result := Idx - Run + 1;
          Exit;
        end;
      end
      else
        Run := 0;
    end;
  end;

begin
  Assert(Image.Format = ifGray8);
  Result := False;
  ContentRect := NullRect;

  Bounds := Rect(0, 0, Image.Width, Image.Height);
  if Assigned(ASearchRect) and not IsRectEmpty(ASearchRect^) then
    if not IntersectRect(Bounds, ASearchRect^, Rect(0, 0, Image.Width, Image.Height)) then
      Exit;

  W := RectWidth(Bounds);
  H := RectHeight(Bounds);
  if (W <= 0) or (H <= 0) then
    Exit;

  SetLength(RowCounts, H);
  SetLength(ColCounts, W);
  SetLength(TopRuns, W);
  SetLength(BottomRuns, W);
  SetLength(ColOpen, W);

  // Find dark runs connected to the top edge in each column ...
  for X := 0 to W - 1 do
    ColOpen[X] := True;

  OpenCount := W;
  Y := 0;

  while (Y < H) and (OpenCount > 0) do
  begin
    Row := GetRow(Y);
    for X := 0 to W - 1 do
      if ColOpen[X] then
      begin
        if Row[X] < Threshold then
          TopRuns[X] := Y + 1
        else
        begin
          ColOpen[X] := False;
          Dec(OpenCount);
        end;
      end;
    Inc(Y);
  end;

  // ... and to the bottom edge (unless the whole column is dark already)
  OpenCount := 0;
  for X := 0 to W - 1 do
  begin
    ColOpen[X] := TopRuns[X] < H;
    if ColOpen[X] then
      Inc(OpenCount);
  end;

  Y := H - 1;

  while (Y >= 0) and (OpenCount > 0) do
  begin
    Row := GetRow(Y);
    for X := 0 to W - 1 do
      if ColOpen[X] then
      begin
        if Row[X] < Threshold then
          BottomRuns[X] := H - Y
        else
        begin
          ColOpen[X] := False;
          Dec(OpenCount);
        end;
      end;
    Dec(Y);
  end;

  // Count content pixels in rows and columns: dark ones that are not
  // connected to any edge in their row or column.
  ContentPixels := 0;

  for Y := 0 to H - 1 do
  begin
    Row := GetRow(Y);

    RunLeft := 0;
    while (RunLeft < W) and (Row[RunLeft] < Threshold) do
      Inc(RunLeft);
    RunRight := 0;
    while (RunRight < W - RunLeft) and (Row[W - 1 - RunRight] < Threshold) do
      Inc(RunRight);

    for X := RunLeft to W - RunRight - 1 do
      if (Row[X] < Threshold) and (Y >= TopRuns[X]) and (Y < H - BottomRuns[X]) then
      begin
        Inc(RowCounts[Y]);
        Inc(ColCounts[X]);
        Inc(ContentPixels);
      end;
  end;

  if ContentPixels < MinContentPixels then
    Exit;

  // Content area starts and ends where rows and columns stop being empty
  FirstRow := FindContentEdge(RowCounts, Max(MinNoiseFloor, Round(W * NoiseFloorFraction)), False);
  LastRow := FindContentEdge(RowCounts, Max(MinNoiseFloor, Round(W * NoiseFloorFraction)), True);
  FirstCol := FindContentEdge(ColCounts, Max(MinNoiseFloor, Round(H * NoiseFloorFraction)), False);
  LastCol := FindContentEdge(ColCounts, Max(MinNoiseFloor, Round(H * NoiseFloorFraction)), True);

  if (FirstRow < 0) or (FirstCol < 0) then
    Exit;

  Padding := Max(2, Round(Min(W, H) * PaddingFraction));
  ContentRect := Rect(
    Max(Bounds.Left, Bounds.Left + FirstCol - Padding),
    Max(Bounds.Top, Bounds.Top + FirstRow - Padding),
    Min(Bounds.Right, Bounds.Left + LastCol + 1 + Padding),
    Min(Bounds.Bottom, Bounds.Top + LastRow + 1 + Padding));
  Result := True;
end;

function GetRotationFormat(const Image: TImageData; BackgroundColor: TColor32): TImageFormat;
var
  FormatInfo: TImageFormatInfo;
  Background: TColor32Rec;
begin
  Result := Image.Format;
  Background.Color := BackgroundColor;

  // Rotation is optimized for Gray8, RGB24, and ARGB32 formats at this time
  if not (Result in SupportedRotationFormats) then
  begin
    GetImageFormatInfo(Image.Format, FormatInfo);

    if Image.Format = ifIndex8 then
    begin
      if PaletteHasAlpha(Image.Palette, FormatInfo.PaletteEntries) then
        Result := ifA8R8G8B8
      else if PaletteIsGrayScale(Image.Palette, FormatInfo.PaletteEntries) then
        Result := ifGray8
      else
        Result := ifR8G8B8;
    end
    else if FormatInfo.HasAlphaChannel then
      Result := ifA8R8G8B8
    else if (Image.Format = ifBinary) or FormatInfo.HasGrayChannel then
      Result := ifGray8
    else
      Result := ifR8G8B8;
  end;

  if Background.A <> $FF then
  begin
    // User explicitly requested some alpha in background color
    Result := ifA8R8G8B8;
  end
  else if (Result = ifGray8) and not ((Background.R = Background.G) and (Background.B = Background.G)) then
  begin
    // Some non-grayscale background for gray image was requested
    Result := ifR8G8B8;
  end;
end;

{$REGION 'Rotation support: types, helpers and pixel loops of RotateImage'}

const
  // Number of sub-pixel positions with precomputed filter weights. With 128 the
  // position is at most 1/254 pixel off; the table is small enough (3.6 KB) to stay in cache.
  FilterTableSize = 128;
  MaxFilterTablePos = FilterTableSize - 1;
  MaxKernelRadius = 3;
  // Source positions are stepped along destination rows in 32.32 fixed point.
  // They are kept positive by a bias of CoordBias pixels, so the integer part is
  // a plain shift in both compilers (shr of Int64 is logical in Delphi).
  FixedShift = 32;
  FixedOne = Int64(1) shl FixedShift;
  CoordBias = 1 shl 30;
  FixedBias = Int64(CoordBias) shl FixedShift;
  // Weights of all the filters are integers with 16 fractional bits (every row
  // of the cubic and Lanczos table sums to exactly WeightOne). Sums of a row of
  // taps fit into Integer, the weighted sums of the rows need Int64.
  WeightShift = 16;
  WeightOne = 1 shl WeightShift;

type
  TFilterWeightTable = array[-MaxKernelRadius..MaxKernelRadius, 0..FilterTableSize] of Integer;

  // Center of the source and destination images and the sine and cosine
  // of the backward rotation (from destination to source), nearest filter
  TRotationGeometry = record
    SrcWidthHalf, SrcHeightHalf: Single;
    DstWidthHalf, DstHeightHalf: Single;
    BackwardSin, BackwardCos: Single;
  end;

  // Source position of the first pixel of each destination row, and how much
  // it moves with every next pixel of the row (linear, cubic and Lanczos filters)
  TRowStepping = record
    BackwardSin, BackwardCos: Double;
    SrcWidthHalf, SrcHeightHalf: Double;
    DstWidthHalf, DstHeightHalf: Double;
    StepX, StepY: Int64;
  end;

  // Everything the pixel loops of RotateImage need. The loops and their helpers
  // are unit-level routines, not nested in RotateImage: Delphi does not inline
  // nested routines that use the variables of the outer one, so every pixel
  // made several calls there. The loops copy what they use into local variables.
  TRotationParams = record
    Geometry: TRotationGeometry;
    Stepping: TRowStepping;
    SrcBits: PBuffer;
    SrcWidth, SrcHeight: Integer;
    DstBits: PByte;
    DstWidth, DstHeight: Integer;
    Bpp: Integer;
    BackColor32: TColor32Rec;
    // Cubic and Lanczos filters only
    KernelWidth: Integer;
    WeightTable: TFilterWeightTable;
  end;

// Position in the source image where destination pixel [DstX, DstY] comes from
procedure CalcSourceCoordinates(const G: TRotationGeometry; DstX, DstY: Integer;
  out SrcX, SrcY: Single); inline;
var
  SrcCoordX, SrcCoordY: Single;
  DstCoordX, DstCoordY: Single;
begin
  DstCoordX := DstX - G.DstWidthHalf;
  DstCoordY := G.DstHeightHalf - DstY;

  SrcCoordX := G.BackwardCos * DstCoordX - G.BackwardSin * DstCoordY;
  SrcCoordY := G.BackwardSin * DstCoordX + G.BackwardCos * DstCoordY;

  SrcX := SrcCoordX + G.SrcWidthHalf;
  SrcY := G.SrcHeightHalf - SrcCoordY;
end;

// Source position (biased fixed point) of the first pixel of destination row Y.
// Same as CalcSourceCoordinates for DstX = 0, in Double.
procedure CalcRowStart(const S: TRowStepping; Y: Integer; out FX, FY: Int64); inline;
var
  DstCoordX, DstCoordY: Double;
begin
  DstCoordX := -S.DstWidthHalf;
  DstCoordY := S.DstHeightHalf - Y;
  FX := FixedBias + Round((S.BackwardCos * DstCoordX - S.BackwardSin * DstCoordY +
    S.SrcWidthHalf) * FixedOne);
  FY := FixedBias + Round((S.SrcHeightHalf - S.BackwardSin * DstCoordX -
    S.BackwardCos * DstCoordY) * FixedOne);
end;

// Integer part (floor) of a biased fixed point position
function FixedToInt(F: Int64): Integer; inline;
begin
  Result := Integer(F shr FixedShift) - CoordBias;
end;

// Fractional part of a fixed point position
function FixedFrac(F: Int64): Cardinal; inline;
begin
  Result := Cardinal(F and $FFFFFFFF);
end;

function GetPixelColor24(Bits: PBuffer; Width, Height, X, Y: Integer;
  const BackColor: TColor24Rec): TColor24Rec; inline;
begin
  if (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
    Result := PColor24Rec(@Bits[(NativeInt(Y) * Width + X) * 3])^
  else
    Result := BackColor;
end;

function GetPixelColor8(Bits: PBuffer; Width, Height, X, Y: Integer; BackColor: Byte): Byte; inline;
begin
  if (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
    Result := Bits[NativeInt(Y) * Width + X]
  else
    Result := BackColor;
end;

function GetPixelColor32(Bits: PBuffer; Width, Height, X, Y: Integer;
  const BackColor: TColor32Rec): TColor32Rec; inline;
begin
  if (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
    Result := PColor32Rec(@Bits[(NativeInt(Y) * Width + X) * 4])^
  else
    Result := BackColor;
end;

// Bilinear interpolation of four neighbors. With fx and fy in [0, 1) the position
// between them (fx from the left ones, fy from the top ones) it is
//   Top    = TopLeft    * (1 - fx) + TopRight    * fx
//   Bottom = BottomLeft * (1 - fx) + BottomRight * fx
//   Result = Round(Top  * (1 - fy) + Bottom * fy)
// which is the same as
//   (1 - fx) * (1 - fy) * TopLeft    + fx * (1 - fy) * TopRight +
//   (1 - fx) * fy       * BottomLeft + fx * fy       * BottomRight
// Here the weights are integers in fixed point: Wx = fx * WeightOne and
// Wy = fy * WeightOne (WeightOne = 2^16 stands for 1). Never above 255.
function Blend(TopLeft, TopRight, BottomLeft, BottomRight: Integer; Wx, Wy: Integer): Byte; inline;
var
  Top, Bottom: Integer;
begin
  // Top and Bottom are WeightOne times the real value (at most 255 * 2^16)
  Top :=    TopLeft    * (WeightOne - Wx) + TopRight    * Wx;
  Bottom := BottomLeft * (WeightOne - Wx) + BottomRight * Wx;

  // Their blend is WeightOne^2 = 2^32 times the result: adding half of that
  // and shifting by 32 bits rounds it to the nearest integer
  Result := Byte(
    (Int64(Top) * (WeightOne - Wy) + Int64(Bottom) * Wy +
    (Int64(1) shl (2 * WeightShift - 1))) shr (2 * WeightShift)
  );
end;

procedure RotateNearest(const P: TRotationParams);
var
  G: TRotationGeometry;
  SrcBits: PBuffer;
  SrcWidth, SrcHeight, Bpp, X, Y, NearestX, NearestY: Integer;
  SrcX, SrcY: Single;
  DstByte: PByte;
  BackColor32: TColor32Rec;
begin
  G := P.Geometry;
  SrcBits := P.SrcBits;
  SrcWidth := P.SrcWidth;
  SrcHeight := P.SrcHeight;
  Bpp := P.Bpp;
  BackColor32 := P.BackColor32;
  DstByte := P.DstBits;

  for Y := 0 to P.DstHeight - 1 do
    for X := 0 to P.DstWidth - 1 do
    begin
      CalcSourceCoordinates(G, X, Y, SrcX, SrcY);
      // Bounds must be checked after the rounding, coordinate just below
      // the width or height would end up outside of the image otherwise
      NearestX := Round(SrcX);
      NearestY := Round(SrcY);

      if (NearestX >= 0) and (NearestY >= 0) and (NearestX < SrcWidth) and (NearestY < SrcHeight) then
      begin
        if Bpp = 3 then
          PColor24Rec(DstByte)^ := PColor24Rec(@SrcBits[(NativeInt(NearestY) * SrcWidth + NearestX) * 3])^
        else if Bpp = 1 then
          DstByte^ := SrcBits[NativeInt(NearestY) * SrcWidth + NearestX]
        else
          PColor32Rec(DstByte)^ := PColor32Rec(@SrcBits[(NativeInt(NearestY) * SrcWidth + NearestX) * 4])^;
      end
      else
        CopyPixel(@BackColor32, DstByte, Bpp);

      Inc(DstByte, Bpp);
    end;
end;

// The three linear loops below are the same apart from the pixel format.
// For every destination pixel the source position [X0 + Wx, Y0 + Wy] is
// stepped from the previous one. When all four neighbors are inside the source
// they are read through one pointer (unsigned compares check both ends at once).
// Within one pixel around the source the neighbors outside are the background,
// further out the destination pixel is the background.

procedure RotateLinear8(const P: TRotationParams);
var
  SrcBits, Src: PBuffer;
  SrcWidth, SrcHeight, X, Y, X0, Y0, Wx, Wy: Integer;
  FX, FY, StepX, StepY: Int64;
  Dst: PByte;
  Back: Byte;
begin
  SrcBits := P.SrcBits;
  SrcWidth := P.SrcWidth;
  SrcHeight := P.SrcHeight;
  StepX := P.Stepping.StepX;
  StepY := P.Stepping.StepY;
  Back := P.BackColor32.B;
  Dst := P.DstBits;

  for Y := 0 to P.DstHeight - 1 do
  begin
    CalcRowStart(P.Stepping, Y, FX, FY);
    for X := 0 to P.DstWidth - 1 do
    begin
      X0 := FixedToInt(FX);
      Y0 := FixedToInt(FY);
      Wx := FixedFrac(FX) shr (32 - WeightShift);
      Wy := FixedFrac(FY) shr (32 - WeightShift);

      if (Cardinal(X0) < Cardinal(SrcWidth - 1)) and (Cardinal(Y0) < Cardinal(SrcHeight - 1)) then
      begin
        Src := @SrcBits[NativeInt(Y0) * SrcWidth + X0];
        Dst^ := Blend(Src[0], Src[1], Src[SrcWidth], Src[SrcWidth + 1], Wx, Wy);
      end
      else if (Cardinal(X0 + 1) < Cardinal(SrcWidth + 1)) and (Cardinal(Y0 + 1) < Cardinal(SrcHeight + 1)) then
        Dst^ := Blend(
          GetPixelColor8(SrcBits, SrcWidth, SrcHeight, X0,     Y0,     Back),
          GetPixelColor8(SrcBits, SrcWidth, SrcHeight, X0 + 1, Y0,     Back),
          GetPixelColor8(SrcBits, SrcWidth, SrcHeight, X0,     Y0 + 1, Back),
          GetPixelColor8(SrcBits, SrcWidth, SrcHeight, X0 + 1, Y0 + 1, Back), Wx, Wy)
      else
        Dst^ := Back;

      Inc(Dst);
      Inc(FX, StepX);
      Inc(FY, StepY);
    end;
  end;
end;

procedure RotateLinear24(const P: TRotationParams);
var
  SrcBits: PBuffer;
  SrcWidth, SrcHeight, Stride, X, Y, X0, Y0, Wx, Wy: Integer;
  FX, FY, StepX, StepY: Int64;
  Dst: PColor24Rec;
  TL, TR, BL, BR: PColor24Rec;
  C11, C21, C12, C22, Back: TColor24Rec;
begin
  SrcBits := P.SrcBits;
  SrcWidth := P.SrcWidth;
  SrcHeight := P.SrcHeight;
  Stride := SrcWidth * 3;
  StepX := P.Stepping.StepX;
  StepY := P.Stepping.StepY;
  Back := P.BackColor32.Color24Rec;
  Dst := PColor24Rec(P.DstBits);

  for Y := 0 to P.DstHeight - 1 do
  begin
    CalcRowStart(P.Stepping, Y, FX, FY);
    for X := 0 to P.DstWidth - 1 do
    begin
      X0 := FixedToInt(FX);
      Y0 := FixedToInt(FY);
      Wx := FixedFrac(FX) shr (32 - WeightShift);
      Wy := FixedFrac(FY) shr (32 - WeightShift);

      if (Cardinal(X0) < Cardinal(SrcWidth - 1)) and (Cardinal(Y0) < Cardinal(SrcHeight - 1)) then
      begin
        TL := @SrcBits[(NativeInt(Y0) * SrcWidth + X0) * 3];
        TR := PColor24Rec(PByte(TL) + 3);
        BL := PColor24Rec(PByte(TL) + Stride);
        BR := PColor24Rec(PByte(TL) + Stride + 3);
        Dst.B := Blend(TL.B, TR.B, BL.B, BR.B, Wx, Wy);
        Dst.G := Blend(TL.G, TR.G, BL.G, BR.G, Wx, Wy);
        Dst.R := Blend(TL.R, TR.R, BL.R, BR.R, Wx, Wy);
      end
      else if (Cardinal(X0 + 1) < Cardinal(SrcWidth + 1)) and (Cardinal(Y0 + 1) < Cardinal(SrcHeight + 1)) then
      begin
        C11 := GetPixelColor24(SrcBits, SrcWidth, SrcHeight, X0,     Y0,     Back);
        C21 := GetPixelColor24(SrcBits, SrcWidth, SrcHeight, X0 + 1, Y0,     Back);
        C12 := GetPixelColor24(SrcBits, SrcWidth, SrcHeight, X0,     Y0 + 1, Back);
        C22 := GetPixelColor24(SrcBits, SrcWidth, SrcHeight, X0 + 1, Y0 + 1, Back);
        Dst.B := Blend(C11.B, C21.B, C12.B, C22.B, Wx, Wy);
        Dst.G := Blend(C11.G, C21.G, C12.G, C22.G, Wx, Wy);
        Dst.R := Blend(C11.R, C21.R, C12.R, C22.R, Wx, Wy);
      end
      else
        Dst^ := Back;

      Inc(Dst);
      Inc(FX, StepX);
      Inc(FY, StepY);
    end;
  end;
end;

procedure RotateLinear32(const P: TRotationParams);
var
  SrcBits: PBuffer;
  SrcWidth, SrcHeight, Stride, X, Y, X0, Y0, Wx, Wy: Integer;
  FX, FY, StepX, StepY: Int64;
  Dst: PColor32Rec;
  TL, TR, BL, BR: PColor32Rec;
  C11, C21, C12, C22, Back: TColor32Rec;
begin
  SrcBits := P.SrcBits;
  SrcWidth := P.SrcWidth;
  SrcHeight := P.SrcHeight;
  Stride := SrcWidth * 4;
  StepX := P.Stepping.StepX;
  StepY := P.Stepping.StepY;
  Back := P.BackColor32;
  Dst := PColor32Rec(P.DstBits);

  for Y := 0 to P.DstHeight - 1 do
  begin
    CalcRowStart(P.Stepping, Y, FX, FY);
    for X := 0 to P.DstWidth - 1 do
    begin
      X0 := FixedToInt(FX);
      Y0 := FixedToInt(FY);
      Wx := FixedFrac(FX) shr (32 - WeightShift);
      Wy := FixedFrac(FY) shr (32 - WeightShift);

      if (Cardinal(X0) < Cardinal(SrcWidth - 1)) and (Cardinal(Y0) < Cardinal(SrcHeight - 1)) then
      begin
        TL := @SrcBits[(NativeInt(Y0) * SrcWidth + X0) * 4];
        TR := PColor32Rec(PByte(TL) + 4);
        BL := PColor32Rec(PByte(TL) + Stride);
        BR := PColor32Rec(PByte(TL) + Stride + 4);
        Dst.B := Blend(TL.B, TR.B, BL.B, BR.B, Wx, Wy);
        Dst.G := Blend(TL.G, TR.G, BL.G, BR.G, Wx, Wy);
        Dst.R := Blend(TL.R, TR.R, BL.R, BR.R, Wx, Wy);
        Dst.A := Blend(TL.A, TR.A, BL.A, BR.A, Wx, Wy);
      end
      else if (Cardinal(X0 + 1) < Cardinal(SrcWidth + 1)) and (Cardinal(Y0 + 1) < Cardinal(SrcHeight + 1)) then
      begin
        C11 := GetPixelColor32(SrcBits, SrcWidth, SrcHeight, X0,     Y0,     Back);
        C21 := GetPixelColor32(SrcBits, SrcWidth, SrcHeight, X0 + 1, Y0,     Back);
        C12 := GetPixelColor32(SrcBits, SrcWidth, SrcHeight, X0,     Y0 + 1, Back);
        C22 := GetPixelColor32(SrcBits, SrcWidth, SrcHeight, X0 + 1, Y0 + 1, Back);
        Dst.B := Blend(C11.B, C21.B, C12.B, C22.B, Wx, Wy);
        Dst.G := Blend(C11.G, C21.G, C12.G, C22.G, Wx, Wy);
        Dst.R := Blend(C11.R, C21.R, C12.R, C22.R, Wx, Wy);
        Dst.A := Blend(C11.A, C21.A, C12.A, C22.A, Wx, Wy);
      end
      else
        Dst^ := Back;

      Inc(Dst);
      Inc(FX, StepX);
      Inc(FY, StepY);
    end;
  end;
end;

// Weights of the filter taps for FilterTableSize + 1 sub-pixel positions in
// WeightShift-bit integers. Position Pos is for the distance Pos / MaxFilterTablePos
// between the source position and the next pixel up from it (its ceiling);
// tap J is the pixel J further from that one.
procedure PrecomputeFilterWeights(var P: TRotationParams; FilterFunction: TFilterFunction);
var
  I, J, Largest, Sum: Integer;
  Weights: array[-MaxKernelRadius..MaxKernelRadius] of Single;
  WeightSum: Single;
  Fraction: Single;
begin
  FillChar(P.WeightTable, SizeOf(P.WeightTable), 0);

  for I := 0 to FilterTableSize do
  begin
    Fraction := I / MaxFilterTablePos;
    WeightSum := 0;
    for J := -P.KernelWidth to P.KernelWidth do
    begin
      Weights[J] := FilterFunction(J + Fraction);
      WeightSum := WeightSum + Weights[J];
    end;

    // Weights of some filters (Lanczos) do not add up to 1, flat areas
    // would get darker the sub-pixel position -> we need to normalize the weights
    // so that all the taps must add up to 1.
    Sum := 0;
    Largest := 0;
    for J := -P.KernelWidth to P.KernelWidth do
    begin
      if WeightSum <> 0 then
        Weights[J] := Weights[J] / WeightSum;
      P.WeightTable[J, I] := Round(Weights[J] * WeightOne);
      Inc(Sum, P.WeightTable[J, I]);
      if Abs(P.WeightTable[J, I]) > Abs(P.WeightTable[Largest, I]) then
        Largest := J;
    end;

    // Rounding must not break that: what is missing goes to the largest weight
    Inc(P.WeightTable[Largest, I], WeightOne - Sum);
  end;
end;

// Weighted sum of one color channel to a byte: rounded, clamped (cubic and
// Lanczos have negative weights)
function FilterSumToByte(Sum: Int64): Byte; inline;
begin
  if Sum <= 0 then
    Result := 0
  else
  begin
    Sum := (Sum + (Int64(1) shl (2 * WeightShift - 1))) shr (2 * WeightShift);
    if Sum > 255 then
      Result := 255
    else
      Result := Byte(Sum);
  end;
end;

// Color of the destination pixel that comes from source position [X, Y]
// (fixed point, biased), cubic and Lanczos filters. Taps of the kernel outside
// of the source image take the background color.
function FilterPixel(const P: TRotationParams; FX, FY: Int64): TColor32Rec;
var
  CeilX, CeilY, LoX, HiX, LoY, HiY, I, J, KernelWidth, Bpp, SrcWidth, SrcHeight: Integer;
  FracX, FracY: Cardinal;
  PosX, PosY: Integer;
  Wx, Wy: array[-MaxKernelRadius..MaxKernelRadius] of Integer;
  InsideX, InsideY: Integer;
  RowB, RowG, RowR, RowA: Integer;
  SumB, SumG, SumR, SumA, Outside: Int64;
  Src: PByte;
  BackColor32: TColor32Rec;
begin
  KernelWidth := P.KernelWidth;
  Bpp := P.Bpp;
  SrcWidth := P.SrcWidth;
  SrcHeight := P.SrcHeight;
  BackColor32 := P.BackColor32;

  // Ceiling of the position and its distance from it as the table position
  CeilX := FixedToInt(FX);
  CeilY := FixedToInt(FY);
  FracX := FixedFrac(FX);
  FracY := FixedFrac(FY);
  PosX := 0;
  PosY := 0;
  if FracX <> 0 then
  begin
    Inc(CeilX);
    PosX := ((FixedOne - FracX) * MaxFilterTablePos + (Int64(1) shl (FixedShift - 1))) shr FixedShift;
  end;
  if FracY <> 0 then
  begin
    Inc(CeilY);
    PosY := ((FixedOne - FracY) * MaxFilterTablePos + (Int64(1) shl (FixedShift - 1))) shr FixedShift;
  end;

  if (CeilX < 0) or (CeilX > SrcWidth) or (CeilY < 0) or (CeilY > SrcHeight) then
    Exit(BackColor32);

  // Taps inside of the source image
  LoX := Max(-KernelWidth, -CeilX);
  HiX := Min(KernelWidth, SrcWidth - CeilX - 1);
  LoY := Max(-KernelWidth, -CeilY);
  HiY := Min(KernelWidth, SrcHeight - CeilY - 1);

  for I := -KernelWidth to KernelWidth do
  begin
    Wx[I] := P.WeightTable[I, PosX];
    Wy[I] := P.WeightTable[I, PosY];
  end;

  SumB := 0;
  SumG := 0;
  SumR := 0;
  SumA := 0;

  // One tap loop for each pixel format
  for I := LoY to HiY do
  begin
    Src := @P.SrcBits[(NativeInt(CeilY + I) * SrcWidth + CeilX + LoX) * Bpp];
    case Bpp of
      1:
        begin
          RowB := 0;
          for J := LoX to HiX do
          begin
            Inc(RowB, Src^ * Wx[J]);
            Inc(Src);
          end;
          Inc(SumB, Int64(RowB) * Wy[I]);
        end;
      3:
        begin
          RowB := 0;
          RowG := 0;
          RowR := 0;
          for J := LoX to HiX do
          begin
            Inc(RowB, PColor24Rec(Src).B * Wx[J]);
            Inc(RowG, PColor24Rec(Src).G * Wx[J]);
            Inc(RowR, PColor24Rec(Src).R * Wx[J]);
            Inc(Src, 3);
          end;
          Inc(SumB, Int64(RowB) * Wy[I]);
          Inc(SumG, Int64(RowG) * Wy[I]);
          Inc(SumR, Int64(RowR) * Wy[I]);
        end;
    else
      RowB := 0;
      RowG := 0;
      RowR := 0;
      RowA := 0;
      for J := LoX to HiX do
      begin
        Inc(RowB, PColor32Rec(Src).B * Wx[J]);
        Inc(RowG, PColor32Rec(Src).G * Wx[J]);
        Inc(RowR, PColor32Rec(Src).R * Wx[J]);
        Inc(RowA, PColor32Rec(Src).A * Wx[J]);
        Inc(Src, 4);
      end;
      Inc(SumB, Int64(RowB) * Wy[I]);
      Inc(SumG, Int64(RowG) * Wy[I]);
      Inc(SumR, Int64(RowR) * Wy[I]);
      Inc(SumA, Int64(RowA) * Wy[I]);
    end;
  end;

  // Taps outside of the source: their weights are all the weights
  // (WeightOne squared) minus those of the rectangle of taps inside
  if (LoX > -KernelWidth) or (HiX < KernelWidth) or (LoY > -KernelWidth) or (HiY < KernelWidth) then
  begin
    InsideX := 0;
    for J := LoX to HiX do
      Inc(InsideX, Wx[J]);
    InsideY := 0;
    for I := LoY to HiY do
      Inc(InsideY, Wy[I]);
    Outside := (Int64(1) shl (2 * WeightShift)) - Int64(InsideX) * InsideY;

    Inc(SumB, BackColor32.B * Outside);
    Inc(SumG, BackColor32.G * Outside);
    Inc(SumR, BackColor32.R * Outside);
    Inc(SumA, BackColor32.A * Outside);
  end;

  Result.B := FilterSumToByte(SumB);
  Result.G := FilterSumToByte(SumG);
  Result.R := FilterSumToByte(SumR);
  Result.A := FilterSumToByte(SumA);
end;

// Cubic and Lanczos filters
procedure RotateFiltered(const P: TRotationParams);
var
  Bpp, X, Y: Integer;
  FX, FY, StepX, StepY: Int64;
  DstByte: PByte;
  Pixel32: TColor32Rec;
begin
  Bpp := P.Bpp;
  StepX := P.Stepping.StepX;
  StepY := P.Stepping.StepY;
  DstByte := P.DstBits;

  for Y := 0 to P.DstHeight - 1 do
  begin
    CalcRowStart(P.Stepping, Y, FX, FY);
    for X := 0 to P.DstWidth - 1 do
    begin
      Pixel32 := FilterPixel(P, FX, FY);
      CopyPixel(@Pixel32, DstByte, Bpp);
      Inc(DstByte, Bpp);
      Inc(FX, StepX);
      Inc(FY, StepY);
    end;
  end;
end;

{$ENDREGION}

procedure RotateImage(var Image: TImageData; Angle: Double; BackgroundColor: TColor32;
  ResamplingFilter: TResamplingFilter; FitRotated: Boolean);
const
  FloatEps = 1E-6;
var
  SrcWidth, SrcHeight: Integer;
  SrcWidthHalf, SrcHeightHalf, DstWidthHalf, DstHeightHalf: Single;
  DstWidth, DstHeight: Integer;
  AngleRad, ForwardSin, ForwardCos, BackwardSin, BackwardCos: Single;
  AngleRadD, BackwardSinD, BackwardCosD: Double;
  SrcImage, DstImage: TImageData;
  FormatInfo: TImageFormatInfo;
  Filter: TSamplingFilter;
  Params: TRotationParams;

  function TryMultipleOf90Rotation: Boolean;
  var
    OrigBits: Pointer;
  begin
    OrigBits := Image.Bits;

    if SameValue(Angle, 90, FloatEps) then
      Imaging.RotateImageMul90(Image, 90)
    else if SameValue(Angle, 180, FloatEps) then
      Imaging.RotateImageMul90(Image, 180)
    else if SameValue(Angle, 270, FloatEps) then
      Imaging.RotateImageMul90(Image, 270);

    Result := (Image.Bits <> OrigBits);
  end;

begin
  Assert(Image.Format in SupportedRotationFormats);
  GetImageFormatInfo(Image.Format, FormatInfo);

  while Angle >= 360 do
    Angle := Angle - 360;
  while Angle < 0 do
    Angle := Angle + 360;

  if SameValue(Angle, 0, FloatEps) or SameValue(Angle, 360, FloatEps) then
    Exit;

  // For 90/180/270 degrees rotation we just call Imaging.RotateImageMul90.
  // Note that "FitRotated" parameter has no effect in this case.
  if TryMultipleOf90Rotation then
    Exit;

  AngleRad := Angle * PI / 180;
  SinCos(AngleRad, ForwardSin, ForwardCos);
  SinCos(-AngleRad, BackwardSin, BackwardCos);
  // Double for stepping the source positions along destination rows
  AngleRadD := Angle * PI / 180;
  SinCos(-AngleRadD, BackwardSinD, BackwardCosD);

  SrcImage := Image;
  SrcWidth := SrcImage.Width;
  SrcHeight := SrcImage.Height;
  SrcWidthHalf := (SrcWidth - 1) / 2;
  SrcHeightHalf := (SrcHeight - 1) / 2;

  if FitRotated then
  begin
    // Encompass the whole area of rotated image => bounding box
    DstWidth  := Ceil(Abs(SrcWidth * ForwardCos) + Abs(SrcHeight * ForwardSin));
    DstHeight := Ceil(Abs(SrcWidth * ForwardSin) + Abs(SrcHeight * ForwardCos));

    if ResamplingFilter <> rfNearest then
    begin
      // Account a bit for antialiased edges of the rotated image
      Inc(DstWidth);
      Inc(DstHeight);
    end;
  end
  else
  begin
    // Keep the size of input (effectively cropping part of the "voids" added by
    // framing the rotated image)
    DstWidth := SrcWidth;
    DstHeight := SrcHeight;
  end;

  // Prevent zero or negative dimensions if input was tiny and angle produced near-zero span
  if DstWidth <= 0 then DstWidth := 1;
  if DstHeight <= 0 then DstHeight := 1;

  DstWidthHalf := (DstWidth - 1) / 2;
  DstHeightHalf := (DstHeight - 1) / 2;

  InitImage(DstImage);
  NewImage(DstWidth, DstHeight, SrcImage.Format, DstImage);

  Params.Geometry.SrcWidthHalf := SrcWidthHalf;
  Params.Geometry.SrcHeightHalf := SrcHeightHalf;
  Params.Geometry.DstWidthHalf := DstWidthHalf;
  Params.Geometry.DstHeightHalf := DstHeightHalf;
  Params.Geometry.BackwardSin := BackwardSin;
  Params.Geometry.BackwardCos := BackwardCos;
  // Along a destination row the source position moves by (cos, -sin) per pixel
  Params.Stepping.BackwardSin := BackwardSinD;
  Params.Stepping.BackwardCos := BackwardCosD;
  Params.Stepping.SrcWidthHalf := SrcWidthHalf;
  Params.Stepping.SrcHeightHalf := SrcHeightHalf;
  Params.Stepping.DstWidthHalf := DstWidthHalf;
  Params.Stepping.DstHeightHalf := DstHeightHalf;
  Params.Stepping.StepX := Round(BackwardCosD * FixedOne);
  Params.Stepping.StepY := -Round(BackwardSinD * FixedOne);
  Params.SrcBits := SrcImage.Bits;
  Params.SrcWidth := SrcWidth;
  Params.SrcHeight := SrcHeight;
  Params.DstBits := DstImage.Bits;
  Params.DstWidth := DstWidth;
  Params.DstHeight := DstHeight;
  Params.Bpp := FormatInfo.BytesPerPixel;
  Params.BackColor32 := TColor32Rec(BackgroundColor);

  if ResamplingFilter = rfNearest then
    RotateNearest(Params)
  else if ResamplingFilter = rfLinear then
  begin
    case Params.Bpp of
      1: RotateLinear8(Params);
      3: RotateLinear24(Params);
    else
      RotateLinear32(Params);
    end;
  end
  else
  begin
    case ResamplingFilter of
      rfCubic: Filter := sfCatmullRom;
      rfLanczos: Filter := sfLanczos;
    else
      Assert(False);
      Filter := sfCatmullRom;
    end;

    Params.KernelWidth := Ceil(ImagingFormats.SamplingFilterRadii[Filter]);
    Assert(Params.KernelWidth <= MaxKernelRadius);
    PrecomputeFilterWeights(Params, ImagingFormats.SamplingFilterFunctions[Filter]);

    RotateFiltered(Params);
  end;

  FreeImage(SrcImage);
  Image := DstImage;
end;

end.
