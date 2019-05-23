{
  Deskew
  by Marek Mauder
  http://galfar.vevb.net/deskew

  The contents of this file are used with permission, subject to the Mozilla
  Public License Version 1.1 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
}

{
  Unit with various image processing functions. Some are taken from
  Imaging extensions.
}
unit ImageUtils;

{$I ImagingOptions.inc}

interface

uses
  SysUtils,
  Classes,
  ImagingTypes,
  Imaging,
  ImagingFormats;

type
  TResamplingFilter = (
    rfNearest,
    rfLinear,
    rfCubic,
    rfLanczos
  );

{ Thresholding using Otsu's method (which chooses the threshold
  to minimize the intraclass variance of the black and white pixels!).
  Functions returns calculated threshold level value [0..255].
  If BinarizeImage is True then the Image is automatically converted to binary using
  computed threshold level.}
function OtsuThresholding(var Image: TImageData; BinarizeImage: Boolean = False): Integer;

const
  SupportedRotationFormats: set of TImageFormat = [ifGray8, ifR8G8B8, ifA8R8G8B8];

{ Rotates image with a background (of outside "void" areas) of specified color. The image is resized to fit
  the whole rotated image. }
procedure RotateImage(var Image: TImageData; Angle: Double; BackgroundColor: TColor32; ResamplingFilter: TResamplingFilter);

implementation

uses
  Math,
  ImagingUtility;

function OtsuThresholding(var Image: TImageData; BinarizeImage: Boolean): Integer;
var
  Histogram: array[Byte] of Single;
  Level, Max, Min, I, J, NumPixels: Integer;
  Pix: PByte;
  Mean, Variance: Single;
  Mu, Omega, LevelMean, LargestMu: Single;
begin
  Assert(Image.Format = ifGray8);

  FillChar(Histogram, SizeOf(Histogram), 0);
  Min := 255;
  Max := 0;
  Level := 0;
  NumPixels := Image.Width * Image.Height;
  Pix := Image.Bits;

  // Compute histogram and determine min and max pixel values
  for I := 0 to NumPixels - 1 do
  begin
    Histogram[Pix^] := Histogram[Pix^] + 1.0;
    if Pix^ < Min then
      Min := Pix^;
    if Pix^ > Max then
      Max := Pix^;
    Inc(Pix);
  end;

  // Normalize histogram
  for I := 0 to 255 do
    Histogram[I] := Histogram[I] / NumPixels;

  // Compute image mean and variance
  Mean := 0.0;
  Variance := 0.0;
  for I := 0 to 255 do
    Mean := Mean + (I + 1) * Histogram[I];
  for I := 0 to 255 do
    Variance := Variance + Sqr(I + 1 - Mean) * Histogram[I];

  // Now finally compute threshold level
  LargestMu := 0;

  for I := 0 to 255 do
  begin
    Omega := 0.0;
    LevelMean := 0.0;

    for J := 0 to I - 1 do
    begin
      Omega := Omega + Histogram[J];
      LevelMean := LevelMean + (J + 1) * Histogram[J];
    end;

    Mu := Sqr(Mean * Omega - LevelMean);
    Omega := Omega * (1.0 - Omega);

    if Omega > 0.0 then
      Mu := Mu / Omega
    else
      Mu := 0;

    if Mu > LargestMu then
    begin
      LargestMu := Mu;
      Level := I;
    end;
  end;

  if BinarizeImage then
  begin
    // Do thresholding using computed level
    Pix := Image.Bits;
    for I := 0 to Image.Width * Image.Height - 1 do
    begin
      if Pix^ >= Level then
        Pix^ := 255
      else
        Pix^ := 0;
      Inc(Pix);
    end;
  end;

  Result := Level;
end;

procedure RotateImage(var Image: TImageData; Angle: Double; BackgroundColor: TColor32; ResamplingFilter: TResamplingFilter);
type
  TBufferEntry = record
    B, G, R, A: Single;
  end;

const
  EmptyBufferEntry: TBufferEntry = (B: 0; G: 0; R: 0; A: 0);
  TableSize = 32;
  MaxTablePos = TableSize - 1;
  MaxKernelRadius = 3;

type
  PKernelEntry = ^TKernelEntry;
  TKernelEntry = array[-MaxKernelRadius..MaxKernelRadius] of Single;

var
  SrcWidth, SrcHeight: Integer;
  SrcWidthHalf, SrcHeightHalf, DstWidthHalf, DstHeightHalf: Single;
  DstWidth, DstHeight: Integer;
  AngleRad, ForwardSin, ForwardCos, BackwardSin, BackwardCos, SrcX, SrcY: Single;
  TopLeft, TopRight, BottomLeft, BottomRight: TFloatPoint;
  SrcImage, DstImage: TImageData;
  FormatInfo: TImageFormatInfo;
  Bpp: Integer;
  X, Y: Integer;
  DstPixel24: PColor24Rec;
  BackColor24: TColor24Rec;
  BackColor32, Pixel32: TColor32Rec;
  DstByte: PByte;
  Filter: TSamplingFilter;
  FilterFunction: TFilterFunction;
  FilterRadius: Single;
  KernelWidth: Integer;
  WeightTable: array[-MaxKernelRadius..MaxKernelRadius, 0..TableSize] of Single;

  function FastFloor(X: Single): Integer; inline;
  begin
    Result := Trunc(X + 65536.0) - 65536;
  end;

  function FastCeil(X: Single): Integer; inline;
  begin
    Result := 65536 - Trunc(65536.0 - X);
  end;

  function GetPixelColor24(X, Y: Integer): TColor24Rec; inline;
  begin
    if (X >= 0) and (X < SrcWidth) and (Y >= 0) and (Y < SrcHeight) then
      Result := PColor24RecArray(SrcImage.Bits)[Y * SrcWidth + X]
    else
      Result := BackColor24;
  end;

  function GetPixelColor8(X, Y: Integer): Byte; inline;
  begin
    if (X >= 0) and (X < SrcWidth) and (Y >= 0) and (Y < SrcHeight) then
      Result := PByteArray(SrcImage.Bits)[Y * SrcWidth + X]
    else
      Result := BackColor32.B;
  end;

  function GetPixelColor32(X, Y: Integer): TColor32Rec; inline;
  begin
    if (X >= 0) and (X < SrcWidth) and (Y >= 0) and (Y < SrcHeight) then
      Result := PColor32RecArray(SrcImage.Bits)[Y * SrcWidth + X]
    else
      Result := BackColor32;
  end;

  procedure GetBilinearPixelCoords(X, Y: Single;
    out HorzWeight, VertWeight: Single;
    out TopLeftPt, BottomLeftPt, TopRightPt, BottomRightPt: TPoint); inline;
  begin
    TopLeftPt := Point(FastFloor(X), FastFloor(Y));

    HorzWeight := X - TopLeftPt.x;
    VertWeight := Y - TopLeftPt.y;

    BottomLeftPt  := Point(TopLeftPt.x,     TopLeftPt.y + 1);
    TopRightPt    := Point(TopLeftPt.x + 1, TopLeftPt.y);
    BottomRightPt := Point(TopLeftPt.x + 1, TopLeftPt.y + 1);
  end;

  function InterpolateBytes(HorzWeight, VertWeight: Single; C11, C12, C21, C22: Byte): Byte; inline;
  begin
    Result := ClampToByte(Trunc(
                (1 - HorzWeight) * (1 - VertWeight) * C11 +
                (1 - HorzWeight) * VertWeight       * C12 +
                HorzWeight       * (1 - VertWeight) * C21 +
                HorzWeight       * VertWeight       * C22));
  end;

  function Bilinear24(X, Y: Single): TColor24Rec; inline;
  var
    TopLeftPt, BottomLeftPt, TopRightPt, BottomRightPt: TPoint;
    HorzWeight, VertWeight: Single;
    TopLeftColor, TopRightColor, BottomLeftColor, BottomRightColor: TColor24Rec;
  begin
    GetBilinearPixelCoords(X, Y,
      HorzWeight, VertWeight,
      TopLeftPt, BottomLeftPt, TopRightPt, BottomRightPt);

    TopLeftColor     := GetPixelColor24(TopLeftPt.X,     TopLeftPt.Y);
    BottomLeftColor  := GetPixelColor24(BottomLeftPt.X,  BottomLeftPt.Y);
    TopRightColor    := GetPixelColor24(TopRightPt.X,    TopRightPt.Y);
    BottomRightColor := GetPixelColor24(BottomRightPt.X, BottomRightPt.Y);

    Result.R := InterpolateBytes(HorzWeight, VertWeight,
      TopLeftColor.R, BottomLeftColor.R, TopRightColor.R, BottomRightColor.R);
    Result.G := InterpolateBytes(HorzWeight, VertWeight,
      TopLeftColor.G, BottomLeftColor.G, TopRightColor.G, BottomRightColor.G);
    Result.B := InterpolateBytes(HorzWeight, VertWeight,
      TopLeftColor.B, BottomLeftColor.B, TopRightColor.B, BottomRightColor.B);
  end;

  function Bilinear8(X, Y: Single): Byte; inline;
  var
    TopLeftPt, BottomLeftPt, TopRightPt, BottomRightPt: TPoint;
    HorzWeight, VertWeight: Single;
    TopLeftColor, TopRightColor, BottomLeftColor, BottomRightColor: Byte;
  begin
    GetBilinearPixelCoords(X, Y,
      HorzWeight, VertWeight,
      TopLeftPt, BottomLeftPt, TopRightPt, BottomRightPt);

    TopLeftColor     := GetPixelColor8(TopLeftPt.X,     TopLeftPt.Y);
    BottomLeftColor  := GetPixelColor8(BottomLeftPt.X,  BottomLeftPt.Y);
    TopRightColor    := GetPixelColor8(TopRightPt.X,    TopRightPt.Y);
    BottomRightColor := GetPixelColor8(BottomRightPt.X, BottomRightPt.Y);

    Result := InterpolateBytes(HorzWeight, VertWeight,
      TopLeftColor, BottomLeftColor, TopRightColor, BottomRightColor);
  end;

  function Bilinear32(X, Y: Single): TColor32Rec; inline;
  var
    TopLeftPt, BottomLeftPt, TopRightPt, BottomRightPt: TPoint;
    HorzWeight, VertWeight: Single;
    TopLeftColor, TopRightColor, BottomLeftColor, BottomRightColor: TColor32Rec;
  begin
    GetBilinearPixelCoords(X, Y,
      HorzWeight, VertWeight,
      TopLeftPt, BottomLeftPt, TopRightPt, BottomRightPt);

    TopLeftColor     := GetPixelColor32(TopLeftPt.X,     TopLeftPt.Y);
    BottomLeftColor  := GetPixelColor32(BottomLeftPt.X,  BottomLeftPt.Y);
    TopRightColor    := GetPixelColor32(TopRightPt.X,    TopRightPt.Y);
    BottomRightColor := GetPixelColor32(BottomRightPt.X, BottomRightPt.Y);

    Result.A := InterpolateBytes(HorzWeight, VertWeight,
        TopLeftColor.A, BottomLeftColor.A, TopRightColor.A, BottomRightColor.A);
    Result.R := InterpolateBytes(HorzWeight, VertWeight,
      TopLeftColor.R, BottomLeftColor.R, TopRightColor.R, BottomRightColor.R);
    Result.G := InterpolateBytes(HorzWeight, VertWeight,
      TopLeftColor.G, BottomLeftColor.G, TopRightColor.G, BottomRightColor.G);
    Result.B := InterpolateBytes(HorzWeight, VertWeight,
      TopLeftColor.B, BottomLeftColor.B, TopRightColor.B, BottomRightColor.B);
  end;

  procedure PrecomputeFilterWeights;
  var
    I, J: Integer;
    Weight: Single;
    Fraction: Single;
  begin
    FillMemoryByte(@WeightTable, SizeOf(WeightTable), 0);

    for I := 0 to TableSize do
    begin
      Fraction := I / (TableSize - 1);
      for J := -KernelWidth to KernelWidth do
      begin
        Weight := FilterFunction(J + Fraction);
        WeightTable[J, I] := Weight;
      end;
    end;
  end;

  function FilterPixel(X, Y: Single; Bpp: Integer): TColor32Rec; inline;
  var
    PHorzKernel, PVertKernel: PKernelEntry;
    HorzEntry, VertEntry: TBufferEntry;
    LoX, HiX, LoY, HiY: Integer;
    I, J: Integer;
    WeightHorz, WeightVert: Single;
    CeilX, CeilY: Integer;
    XFilterTablePos, YFilterTablePos: Integer;
    FracXS, FracYS: Single;
    SrcPixel: PColor32Rec;
    ClipRect: TRect;
    Edge: Boolean;
  begin
    ClipRect := Rect(0, 0, SrcWidth, SrcHeight);
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

    FracXS := CeilX - X;
    FracYS := CeilY - Y;

    XFilterTablePos := Round((CeilX - X) * MaxTablePos);
    YFilterTablePos := Round((CeilY - Y) * MaxTablePos);

    VertEntry := EmptyBufferEntry;

    for I := LoY to HiY do
    begin
      WeightVert := WeightTable[I, YFilterTablePos];
      //WeightVert := FilterFunction(I + FracYS);

      SrcPixel := PColor32Rec(@PByteArray(SrcImage.Bits)[(LoX + CeilX + (I + CeilY) * SrcWidth) * Bpp]);

      if WeightVert <> 0 then
      begin
        HorzEntry := EmptyBufferEntry;
        for J := LoX to HiX do
        begin
          WeightHorz := WeightTable[J, XFilterTablePos];
          //WeightHorz := FilterFunction(J + FracXS);

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
        WeightVert := WeightTable[I, YFilterTablePos];
        //WeightVert := FilterFunction(I + FracYS);

        if WeightVert <> 0 then
        begin
          HorzEntry := EmptyBufferEntry;
          for J := -KernelWidth to KernelWidth do
          begin
            if (J < LoX) or (J > HiX) or (I < LoY) or (I > HiY) then
            begin
              WeightHorz := WeightTable[J, XFilterTablePos];
              //WeightHorz := FilterFunction(J + FracXS);

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

    with Result do
    begin
      A := ClampToByte(Trunc(VertEntry.A + 0.5));
      R := ClampToByte(Trunc(VertEntry.R + 0.5));
      G := ClampToByte(Trunc(VertEntry.G + 0.5));
      B := ClampToByte(Trunc(VertEntry.B + 0.5));
    end;
  end;

  function RotatePoint(X, Y: Single): TFloatPoint; inline;
  begin
    Result.X := ForwardCos * X - ForwardSin * Y;
    Result.Y := ForwardSin * X + ForwardCos * Y;
  end;

  function Max4(X1, X2, X3, X4: Single): Single; inline;
  begin
    Result := Math.Max(Math.Max(X1, X2), Math.Max(X3, X4));
  end;

  function Min4(X1, X2, X3, X4: Single): Single; inline;
  begin
    Result := Math.Min(Math.Min(X1, X2), Math.Min(X3, X4));
  end;

  procedure CalcSourceCoordinates(DstX, DstY: Integer; out SrcX, SrcY: Single); inline;
  var
    SrcCoordX, SrcCoordY: Single;
    DstCoordX, DstCoordY: Single;
  begin
    DstCoordX := DstX - DstWidthHalf;
    DstCoordY := DstHeightHalf - DstY;

    SrcCoordX := BackwardCos * DstCoordX - BackwardSin * DstCoordY;
    SrcCoordY := BackwardSin * DstCoordX + BackwardCos * DstCoordY;

    SrcX := SrcCoordX + SrcWidthHalf;
    SrcY := SrcHeightHalf - SrcCoordY;
  end;

begin
  Assert(Image.Format in SupportedRotationFormats);
  GetImageFormatInfo(Image.Format, FormatInfo);

  while Angle >= 360 do
    Angle := Angle - 360;
  while Angle < 0 do
    Angle := Angle + 360;

  if (Angle = 0) or (Abs(Angle) = 360) then
    Exit;

  AngleRad := Angle * PI / 180;
  SinCos(AngleRad, ForwardSin, ForwardCos);
  SinCos(-AngleRad, BackwardSin, BackwardCos);

  SrcImage := Image;
  SrcWidth := SrcImage.Width;
  SrcHeight := SrcImage.Height;
  SrcWidthHalf := (SrcWidth - 1) / 2;
  SrcHeightHalf := (SrcHeight - 1) / 2;

  // Calculate width and height of the rotated image
  TopLeft := RotatePoint(-SrcWidthHalf, SrcHeightHalf);
  TopRight := RotatePoint(SrcWidthHalf, SrcHeightHalf);
  BottomLeft := RotatePoint(-SrcWidthHalf, -SrcHeightHalf);
  BottomRight := RotatePoint(SrcWidthHalf, -SrcHeightHalf);

  DstWidth  := Ceil(Max4(TopLeft.X, TopRight.X, BottomLeft.X, BottomRight.X) -
                    Min4(TopLeft.X, TopRight.X, BottomLeft.X, BottomRight.X));
  DstHeight := Ceil(Max4(TopLeft.Y, TopRight.Y, BottomLeft.Y, BottomRight.Y) -
                    Min4(TopLeft.Y, TopRight.Y, BottomLeft.Y, BottomRight.Y));

  if ResamplingFilter <> rfNearest then
  begin
    // Account a bit for antialiased edges of the rotated image
    Inc(DstWidth);
    Inc(DstHeight);
  end;

  DstWidthHalf := (DstWidth - 1) / 2;
  DstHeightHalf := (DstHeight - 1) / 2;

  InitImage(DstImage);
  NewImage(DstWidth, DstHeight, SrcImage.Format, DstImage);

  Bpp := FormatInfo.BytesPerPixel;
  DstByte := DstImage.Bits;
  BackColor32 := TColor32Rec(BackgroundColor);

  if ResamplingFilter = rfNearest then
  begin
    for Y := 0 to DstHeight - 1 do
      for X := 0 to DstWidth - 1 do
      begin
        CalcSourceCoordinates(X, Y, SrcX, SrcY);

        if (SrcX >= 0) and (SrcY >= 0) and (SrcX <= SrcWidth - 1) and (SrcY <= SrcHeight - 1) then
        begin
          if Bpp = 3 then
            PColor24Rec(DstByte)^ := PColor24RecArray(SrcImage.Bits)[Round(SrcY) * SrcWidth + Round(SrcX)]
          else if Bpp = 1 then
            DstByte^ := PByteArray(SrcImage.Bits)[Round(SrcY) * SrcWidth + Round(SrcX)]
          else
            PColor32Rec(DstByte)^ := PColor32RecArray(SrcImage.Bits)[Round(SrcY) * SrcWidth + Round(SrcX)];
        end
        else
          CopyPixel(@BackColor32, DstByte, Bpp);

        Inc(DstByte, Bpp);
      end;
  end
  else if ResamplingFilter = rfLinear then
  begin
    if SrcImage.Format = ifR8G8B8 then
    begin
      DstPixel24 := DstImage.Bits;
      BackColor24 := TColor32Rec(BackgroundColor).Color24Rec;

      // RGB 24bit path
      for Y := 0 to DstHeight - 1 do
        for X := 0 to DstWidth - 1 do
        begin
          CalcSourceCoordinates(X, Y, SrcX, SrcY);

          if (SrcX >= -1) and (SrcY >= -1) and (SrcX <= SrcWidth) and (SrcY <= SrcHeight) then
            DstPixel24^ := Bilinear24(SrcX, SrcY)
          else
            DstPixel24^ := BackColor24;

          Inc(DstPixel24);
        end;
    end
    else
    begin
      // A bit more generic 8+32bit path
      for Y := 0 to DstHeight - 1 do
        for X := 0 to DstWidth - 1 do
        begin
          CalcSourceCoordinates(X, Y, SrcX, SrcY);

          if (SrcX >= -1) and (SrcY >= -1) and (SrcX <= SrcWidth) and (SrcY <= SrcHeight) then
          begin
            if Bpp = 1 then
              DstByte^ := Bilinear8(SrcX, SrcY)
            else
              PColor32Rec(DstByte)^ := Bilinear32(SrcX, SrcY)
          end
          else
            CopyPixel(@BackColor32, DstByte, Bpp);

          Inc(DstByte, Bpp);
        end;
    end;
  end
  else
  begin
    case ResamplingFilter of
      rfCubic: Filter := sfCatmullRom;
      rfLanczos: Filter := sfLanczos;
    else
      Assert(False);
    end;

    FilterFunction := ImagingFormats.SamplingFilterFunctions[Filter];
    FilterRadius := ImagingFormats.SamplingFilterRadii[Filter];
    KernelWidth := FastCeil(FilterRadius);

    PrecomputeFilterWeights;

    for Y := 0 to DstHeight - 1 do
      for X := 0 to DstWidth - 1 do
      begin
        CalcSourceCoordinates(X, Y, SrcX, SrcY);
        Pixel32 := FilterPixel(SrcX, SrcY, Bpp);
        CopyPixel(@Pixel32, DstByte, Bpp);
        Inc(DstByte, Bpp);
      end;
  end;

  FreeImage(SrcImage);
  Image := DstImage;
end;

end.
