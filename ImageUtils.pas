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

// Use precomputed weights for bicubic and Lanczos filters
{$DEFINE USE_FILTER_TABLE}

const
  // Number of sub-pixel positions with precomputed filter weights
  FilterTableSize = 32;
  MaxFilterTablePos = FilterTableSize - 1;
  MaxKernelRadius = 3;

type
  TFilterWeightTable = array[-MaxKernelRadius..MaxKernelRadius, 0..FilterTableSize] of Single;

  // Center of the source and destination images and the sine and cosine
  // of the backward rotation (from destination to source)
  TRotationGeometry = record
    SrcWidthHalf, SrcHeightHalf: Single;
    DstWidthHalf, DstHeightHalf: Single;
    BackwardSin, BackwardCos: Single;
  end;

  // Everything the pixel loops of RotateImage need. The loops and their helpers
  // are unit-level routines, not nested in RotateImage: Delphi does not inline
  // nested routines that use the variables of the outer one, so every pixel
  // made several calls there. The loops copy what they use into local variables.
  TRotationParams = record
    Geometry: TRotationGeometry;
    SrcBits: PBuffer;
    SrcWidth, SrcHeight: Integer;
    DstBits: PByte;
    DstWidth, DstHeight: Integer;
    Bpp: Integer;
    BackColor32: TColor32Rec;
    // Cubic and Lanczos filters only
    FilterFunction: TFilterFunction;
    KernelWidth: Integer;
    WeightTable: TFilterWeightTable;
  end;

function FastFloor(X: Single): Integer; inline;
begin
  Result := Trunc(X + 65536.0) - 65536;
end;

function FastCeil(X: Single): Integer; inline;
begin
  Result := 65536 - Trunc(65536.0 - X);
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

procedure GetBilinearPixelCoords(X, Y: Single;
  out HorzWeight, VertWeight: Single;
  out TopLeftPt, BottomLeftPt, TopRightPt, BottomRightPt: TPoint); inline;
begin
  // Fields are set one by one, Point() of Classes unit is not inlined
  TopLeftPt.X := FastFloor(X);
  TopLeftPt.Y := FastFloor(Y);

  HorzWeight := X - TopLeftPt.X;
  VertWeight := Y - TopLeftPt.Y;

  BottomLeftPt.X  := TopLeftPt.X;
  BottomLeftPt.Y  := TopLeftPt.Y + 1;
  TopRightPt.X    := TopLeftPt.X + 1;
  TopRightPt.Y    := TopLeftPt.Y;
  BottomRightPt.X := TopLeftPt.X + 1;
  BottomRightPt.Y := TopLeftPt.Y + 1;
end;

// Clamped here and not with ClampToByte: FPC does not inline that one so deep
// in the inlined calls, and the call per channel made the loops much slower
function InterpolateBytes(HorzWeight, VertWeight: Single; C11, C12, C21, C22: Byte): Byte; inline;
var
  Value: LongInt;
begin
  Value := Trunc(
             (1 - HorzWeight) * (1 - VertWeight) * C11 +
             (1 - HorzWeight) * VertWeight       * C12 +
             HorzWeight       * (1 - VertWeight) * C21 +
             HorzWeight       * VertWeight       * C22 + 0.5);
  if Value > 255 then
    Result := 255
  else if Value < 0 then
    Result := 0
  else
    Result := Value;
end;

function Bilinear24(Bits: PBuffer; Width, Height: Integer; X, Y: Single;
  const BackColor: TColor24Rec): TColor24Rec; inline;
var
  TopLeftPt, BottomLeftPt, TopRightPt, BottomRightPt: TPoint;
  HorzWeight, VertWeight: Single;
  TopLeftColor, TopRightColor, BottomLeftColor, BottomRightColor: TColor24Rec;
begin
  GetBilinearPixelCoords(X, Y,
    HorzWeight, VertWeight,
    TopLeftPt, BottomLeftPt, TopRightPt, BottomRightPt);

  TopLeftColor     := GetPixelColor24(Bits, Width, Height, TopLeftPt.X,     TopLeftPt.Y,     BackColor);
  BottomLeftColor  := GetPixelColor24(Bits, Width, Height, BottomLeftPt.X,  BottomLeftPt.Y,  BackColor);
  TopRightColor    := GetPixelColor24(Bits, Width, Height, TopRightPt.X,    TopRightPt.Y,    BackColor);
  BottomRightColor := GetPixelColor24(Bits, Width, Height, BottomRightPt.X, BottomRightPt.Y, BackColor);

  Result.R := InterpolateBytes(HorzWeight, VertWeight,
    TopLeftColor.R, BottomLeftColor.R, TopRightColor.R, BottomRightColor.R);
  Result.G := InterpolateBytes(HorzWeight, VertWeight,
    TopLeftColor.G, BottomLeftColor.G, TopRightColor.G, BottomRightColor.G);
  Result.B := InterpolateBytes(HorzWeight, VertWeight,
    TopLeftColor.B, BottomLeftColor.B, TopRightColor.B, BottomRightColor.B);
end;

function Bilinear8(Bits: PBuffer; Width, Height: Integer; X, Y: Single;
  BackColor: Byte): Byte; inline;
var
  TopLeftPt, BottomLeftPt, TopRightPt, BottomRightPt: TPoint;
  HorzWeight, VertWeight: Single;
  TopLeftColor, TopRightColor, BottomLeftColor, BottomRightColor: Byte;
begin
  GetBilinearPixelCoords(X, Y,
    HorzWeight, VertWeight,
    TopLeftPt, BottomLeftPt, TopRightPt, BottomRightPt);

  TopLeftColor     := GetPixelColor8(Bits, Width, Height, TopLeftPt.X,     TopLeftPt.Y,     BackColor);
  BottomLeftColor  := GetPixelColor8(Bits, Width, Height, BottomLeftPt.X,  BottomLeftPt.Y,  BackColor);
  TopRightColor    := GetPixelColor8(Bits, Width, Height, TopRightPt.X,    TopRightPt.Y,    BackColor);
  BottomRightColor := GetPixelColor8(Bits, Width, Height, BottomRightPt.X, BottomRightPt.Y, BackColor);

  Result := InterpolateBytes(HorzWeight, VertWeight,
    TopLeftColor, BottomLeftColor, TopRightColor, BottomRightColor);
end;

function Bilinear32(Bits: PBuffer; Width, Height: Integer; X, Y: Single;
  const BackColor: TColor32Rec): TColor32Rec; inline;
var
  TopLeftPt, BottomLeftPt, TopRightPt, BottomRightPt: TPoint;
  HorzWeight, VertWeight: Single;
  TopLeftColor, TopRightColor, BottomLeftColor, BottomRightColor: TColor32Rec;
begin
  GetBilinearPixelCoords(X, Y,
    HorzWeight, VertWeight,
    TopLeftPt, BottomLeftPt, TopRightPt, BottomRightPt);

  TopLeftColor     := GetPixelColor32(Bits, Width, Height, TopLeftPt.X,     TopLeftPt.Y,     BackColor);
  BottomLeftColor  := GetPixelColor32(Bits, Width, Height, BottomLeftPt.X,  BottomLeftPt.Y,  BackColor);
  TopRightColor    := GetPixelColor32(Bits, Width, Height, TopRightPt.X,    TopRightPt.Y,    BackColor);
  BottomRightColor := GetPixelColor32(Bits, Width, Height, BottomRightPt.X, BottomRightPt.Y, BackColor);

  Result.A := InterpolateBytes(HorzWeight, VertWeight,
      TopLeftColor.A, BottomLeftColor.A, TopRightColor.A, BottomRightColor.A);
  Result.R := InterpolateBytes(HorzWeight, VertWeight,
    TopLeftColor.R, BottomLeftColor.R, TopRightColor.R, BottomRightColor.R);
  Result.G := InterpolateBytes(HorzWeight, VertWeight,
    TopLeftColor.G, BottomLeftColor.G, TopRightColor.G, BottomRightColor.G);
  Result.B := InterpolateBytes(HorzWeight, VertWeight,
    TopLeftColor.B, BottomLeftColor.B, TopRightColor.B, BottomRightColor.B);
end;

{$IFDEF USE_FILTER_TABLE}
procedure PrecomputeFilterWeights(var P: TRotationParams);
var
  I, J: Integer;
  Weight, Sum: Single;
  Fraction: Single;
begin
  FillMemoryByte(@P.WeightTable, SizeOf(P.WeightTable), 0);

  for I := 0 to FilterTableSize do
  begin
    Fraction := I / (FilterTableSize - 1);
    Sum := 0;
    for J := -P.KernelWidth to P.KernelWidth do
    begin
      Weight := P.FilterFunction(J + Fraction);
      P.WeightTable[J, I] := Weight;
      Sum := Sum + Weight;
    end;

    // Weights of some filters (Lanczos) do not add up to 1, flat areas
    // would get darker the sub-pixel position -> we need to normalize the weights
    // so that all the taps must add up to 1.
    if Sum <> 0 then
      for J := -P.KernelWidth to P.KernelWidth do
        P.WeightTable[J, I] := P.WeightTable[J, I] / Sum;
  end;
end;
{$ENDIF}

// Color of the destination pixel that comes from source position [X, Y],
// cubic and Lanczos filters
function FilterPixel(const P: TRotationParams; X, Y: Single): TColor32Rec;
type
  TBufferEntry = record
    B, G, R, A: Single;
  end;
const
  EmptyBufferEntry: TBufferEntry = (B: 0; G: 0; R: 0; A: 0);
var
  HorzEntry, VertEntry: TBufferEntry;
  LoX, HiX, LoY, HiY: Integer;
  I, J: Integer;
  WeightHorz, WeightVert: Single;
  CeilX, CeilY: Integer;
{$IFDEF USE_FILTER_TABLE}
  XFilterTablePos, YFilterTablePos: Integer;
{$ELSE}
  FracXS, FracYS: Single;
  SumX, SumY, Norm: Single;
{$ENDIF}
  SrcPixel: PColor32Rec;
  ClipRect: TRect;
  Edge: Boolean;
  KernelWidth, Bpp: Integer;
  BackColor32: TColor32Rec;
begin
  KernelWidth := P.KernelWidth;
  Bpp := P.Bpp;
  BackColor32 := P.BackColor32;
  // Fields are set one by one, Rect() of Classes unit is not inlined
  ClipRect.Left := 0;
  ClipRect.Top := 0;
  ClipRect.Right := P.SrcWidth;
  ClipRect.Bottom := P.SrcHeight;
  Edge := False;

  CeilX := FastCeil(X);
  CeilY := FastCeil(Y);

  with ClipRect do
  begin
    if not ((CeilX < Left) or (CeilX > Right) or (CeilY < Top) or (CeilY > Bottom)) then
    begin
      Edge := False;

      if CeilX - KernelWidth < Left then
      begin
        LoX := Left - CeilX;
        Edge := True;
      end
      else
        LoX := -KernelWidth;

      if CeilX + KernelWidth >= Right then
      begin
        HiX := Right - CeilX - 1;
        Edge := True;
      end
      else
        HiX := KernelWidth;

      if CeilY - KernelWidth < Top then
      begin
        LoY := Top - CeilY;
        Edge := True;
      end
      else
        LoY := -KernelWidth;

      if CeilY + KernelWidth >= Bottom then
      begin
        HiY := Bottom - CeilY - 1;
        Edge := True;
      end
      else
        HiY := KernelWidth;
    end
    else
      Exit(BackColor32);
  end;

{$IFDEF USE_FILTER_TABLE}
  XFilterTablePos := Round((CeilX - X) * MaxFilterTablePos);
  YFilterTablePos := Round((CeilY - Y) * MaxFilterTablePos);
{$ELSE}
  FracXS := CeilX - X;
  FracYS := CeilY - Y;

  // Same normalization as in PrecomputeFilterWeights: weights of all the taps
  // (including those on the background) must add up to 1
  SumX := 0;
  SumY := 0;
  for I := -KernelWidth to KernelWidth do
  begin
    SumX := SumX + P.FilterFunction(I + FracXS);
    SumY := SumY + P.FilterFunction(I + FracYS);
  end;
{$ENDIF}

  VertEntry := EmptyBufferEntry;

  for I := LoY to HiY do
  begin
  {$IFDEF USE_FILTER_TABLE}
    WeightVert := P.WeightTable[I, YFilterTablePos];
  {$ELSE}
    WeightVert := P.FilterFunction(I + FracYS);
  {$ENDIF}

    SrcPixel := PColor32Rec(@P.SrcBits[(LoX + CeilX + NativeInt(I + CeilY) * P.SrcWidth) * Bpp]);

    if WeightVert <> 0 then
    begin
      HorzEntry := EmptyBufferEntry;
      for J := LoX to HiX do
      begin
      {$IFDEF USE_FILTER_TABLE}
         WeightHorz := P.WeightTable[J, XFilterTablePos];
      {$ELSE}
         WeightHorz := P.FilterFunction(J + FracXS);
      {$ENDIF}

        HorzEntry.B := HorzEntry.B + SrcPixel.B * WeightHorz;
        if Bpp > 1 then
        begin
          HorzEntry.R := HorzEntry.R + SrcPixel.R * WeightHorz;
          HorzEntry.G := HorzEntry.G + SrcPixel.G * WeightHorz;
          if Bpp > 3 then
            HorzEntry.A := HorzEntry.A + SrcPixel.A * WeightHorz;
        end;

        Inc(PByte(SrcPixel), Bpp);
      end;

      VertEntry.A := VertEntry.A + HorzEntry.A * WeightVert;
      VertEntry.R := VertEntry.R + HorzEntry.R * WeightVert;
      VertEntry.G := VertEntry.G + HorzEntry.G * WeightVert;
      VertEntry.B := VertEntry.B + HorzEntry.B * WeightVert;
    end;
  end;

  if Edge then
  begin
    for I := -KernelWidth to KernelWidth do
    begin
    {$IFDEF USE_FILTER_TABLE}
      WeightVert := P.WeightTable[I, YFilterTablePos];
    {$ELSE}
      WeightVert := P.FilterFunction(I + FracYS);
    {$ENDIF}

      if WeightVert <> 0 then
      begin
        HorzEntry := EmptyBufferEntry;
        for J := -KernelWidth to KernelWidth do
        begin
          if (J < LoX) or (J > HiX) or (I < LoY) or (I > HiY) then
          begin
          {$IFDEF USE_FILTER_TABLE}
            WeightHorz := P.WeightTable[J, XFilterTablePos];
          {$ELSE}
             WeightHorz := P.FilterFunction(J + FracXS);
          {$ENDIF}

            HorzEntry.A := HorzEntry.A + BackColor32.A * WeightHorz;
            HorzEntry.R := HorzEntry.R + BackColor32.R * WeightHorz;
            HorzEntry.G := HorzEntry.G + BackColor32.G * WeightHorz;
            HorzEntry.B := HorzEntry.B + BackColor32.B * WeightHorz;
          end;
        end;

        VertEntry.A := VertEntry.A + HorzEntry.A * WeightVert;
        VertEntry.R := VertEntry.R + HorzEntry.R * WeightVert;
        VertEntry.G := VertEntry.G + HorzEntry.G * WeightVert;
        VertEntry.B := VertEntry.B + HorzEntry.B * WeightVert;
      end;
    end
  end;

{$IFNDEF USE_FILTER_TABLE}
  if SumX * SumY <> 0 then
  begin
    Norm := 1 / (SumX * SumY);
    VertEntry.A := VertEntry.A * Norm;
    VertEntry.R := VertEntry.R * Norm;
    VertEntry.G := VertEntry.G * Norm;
    VertEntry.B := VertEntry.B * Norm;
  end;
{$ENDIF}

  with Result do
  begin
    A := ClampToByte(Trunc(VertEntry.A + 0.5));
    R := ClampToByte(Trunc(VertEntry.R + 0.5));
    G := ClampToByte(Trunc(VertEntry.G + 0.5));
    B := ClampToByte(Trunc(VertEntry.B + 0.5));
  end;
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

// RGB 24 bit path of the linear filter
procedure RotateLinear24(const P: TRotationParams);
var
  G: TRotationGeometry;
  SrcBits: PBuffer;
  SrcWidth, SrcHeight, X, Y: Integer;
  SrcX, SrcY: Single;
  DstPixel24: PColor24Rec;
  BackColor24: TColor24Rec;
begin
  G := P.Geometry;
  SrcBits := P.SrcBits;
  SrcWidth := P.SrcWidth;
  SrcHeight := P.SrcHeight;
  BackColor24 := P.BackColor32.Color24Rec;
  DstPixel24 := PColor24Rec(P.DstBits);

  for Y := 0 to P.DstHeight - 1 do
    for X := 0 to P.DstWidth - 1 do
    begin
      CalcSourceCoordinates(G, X, Y, SrcX, SrcY);

      if (SrcX >= -1) and (SrcY >= -1) and (SrcX <= SrcWidth) and (SrcY <= SrcHeight) then
        DstPixel24^ := Bilinear24(SrcBits, SrcWidth, SrcHeight, SrcX, SrcY, BackColor24)
      else
        DstPixel24^ := BackColor24;

      Inc(DstPixel24);
    end;
end;

// A bit more generic 8 and 32 bit path of the linear filter
procedure RotateLinear(const P: TRotationParams);
var
  G: TRotationGeometry;
  SrcBits: PBuffer;
  SrcWidth, SrcHeight, Bpp, X, Y: Integer;
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

      if (SrcX >= -1) and (SrcY >= -1) and (SrcX <= SrcWidth) and (SrcY <= SrcHeight) then
      begin
        if Bpp = 1 then
          DstByte^ := Bilinear8(SrcBits, SrcWidth, SrcHeight, SrcX, SrcY, BackColor32.B)
        else
          PColor32Rec(DstByte)^ := Bilinear32(SrcBits, SrcWidth, SrcHeight, SrcX, SrcY, BackColor32)
      end
      else
        CopyPixel(@BackColor32, DstByte, Bpp);

      Inc(DstByte, Bpp);
    end;
end;

// Cubic and Lanczos filters
procedure RotateFiltered(const P: TRotationParams);
var
  G: TRotationGeometry;
  Bpp, X, Y: Integer;
  SrcX, SrcY: Single;
  DstByte: PByte;
  Pixel32: TColor32Rec;
begin
  G := P.Geometry;
  Bpp := P.Bpp;
  DstByte := P.DstBits;

  for Y := 0 to P.DstHeight - 1 do
    for X := 0 to P.DstWidth - 1 do
    begin
      CalcSourceCoordinates(G, X, Y, SrcX, SrcY);
      Pixel32 := FilterPixel(P, SrcX, SrcY);
      CopyPixel(@Pixel32, DstByte, Bpp);
      Inc(DstByte, Bpp);
    end;
end;

procedure RotateImage(var Image: TImageData; Angle: Double; BackgroundColor: TColor32;
  ResamplingFilter: TResamplingFilter; FitRotated: Boolean);
const
  FloatEps = 1E-6;
var
  SrcWidth, SrcHeight: Integer;
  SrcWidthHalf, SrcHeightHalf, DstWidthHalf, DstHeightHalf: Single;
  DstWidth, DstHeight: Integer;
  AngleRad, ForwardSin, ForwardCos, BackwardSin, BackwardCos: Single;
  SrcImage, DstImage: TImageData;
  FormatInfo: TImageFormatInfo;
  Filter: TSamplingFilter;
  FilterRadius: Single;
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
    if SrcImage.Format = ifR8G8B8 then
      RotateLinear24(Params)
    else
      RotateLinear(Params);
  end
  else
  begin
    case ResamplingFilter of
      rfCubic: Filter := sfCatmullRom;
      rfLanczos: Filter := sfLanczos;
    else
      Assert(False);
    end;

    Params.FilterFunction := ImagingFormats.SamplingFilterFunctions[Filter];
    FilterRadius := ImagingFormats.SamplingFilterRadii[Filter];

    Params.KernelWidth := FastCeil(FilterRadius);
  {$IFDEF USE_FILTER_TABLE}
    PrecomputeFilterWeights(Params);
  {$ENDIF}

    RotateFiltered(Params);
  end;

  FreeImage(SrcImage);
  Image := DstImage;
end;

end.
