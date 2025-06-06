unit TestImageUtils;

interface

uses
  Types, Classes, SysUtils,
  DeskewTestUtils,
  ImagingTypes, ImagingClasses, ImagingColors, ImagingUtility,
  ImageUtils;

type
  TTestImageUtils = class(TDeskewTestCase)
  private
    FTestImage: TSingleImage;

    // Helper to create a simple two-value image within a rectangle
    procedure CreateTwoValueRect(AImage: TSingleImage; const ARect: TRect;
      Val1: Byte; Val2: Byte; IsHorizontalSplit: Boolean);

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOtsu_WholeImage_SimpleSplit;
    procedure TestOtsu_WholeImage_SolidColor;
    procedure TestOtsu_ContentRect_SimpleSplit;
    procedure TestOtsu_ContentRect_SolidColor;
    procedure TestOtsu_ContentRect_IgnoresOutside;
    procedure TestOtsu_ContentRect_Invalid;
    procedure TestOtsu_ContentRect_Clipped;

    procedure TestBinarize_WholeImage_SimpleSplit;
    procedure TestBinarize_ContentRect_LeavesOutsideUnchanged;
  end;

  TTestImageRotation = class(TDeskewTestCase)
  private const
    IMG_WIDTH  =  50;
    IMG_HEIGHT = 100; // Non-square for dimension checks

    C8_WHITE = 255;
    C8_GRAY1: Byte = 60;
    C8_GRAY2: Byte = 120;
    C8_GRAY3: Byte = 180;
    C8_GRAY4: Byte = 240;

    C32_WHITE: TColor32 = pcWhite;
    C32_RED: TColor32 = pcRed;
    C32_GREEN: TColor32 = pcGreen;
    C32_BLUE: TColor32 = pcBlue;
    C32_YELLOW: TColor32 = pcYellow;
  private
    FOrigImage: TSingleImage;
    FTestImage: TSingleImage;

    // Helper to create a distinct pattern - colored quadrants
    procedure CreateQuadrantPattern(AImage: TSingleImage);

    procedure CalcRotatedImageSize(CurrentWidth, CurrentHeight: Integer;
      AngleDeg: Integer; Filter: TResamplingFilter;
      out ExpectedWidth, ExpectedHeight: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Rotate0Degrees_NoChange;
    procedure Test_Rotate90Degrees_Gray8_Nearest;
    procedure Test_Rotate45Degrees_FitFalse_Gray8_Nearest;
    procedure Test_Rotate180Degrees_Gray8_Nearest;
    procedure Test_FitTrue_Dimensions_Gray8;
    procedure Test_BackgroundColor_Gray8_Linear;
    procedure Test_Rotate90Degrees_FitTrue_RGB24_Nearest;
  end;

implementation

uses
  Math;

type
  TImageHelper = class helper for TBaseImage
  public
    procedure Fill(AValue: Byte); overload;
    procedure FillRect(const ARect: TRect;  AValue: Byte); overload;
    function GetPixelByte(X, Y: Integer): Byte;
  end;

procedure TImageHelper.Fill(AValue: Byte);
begin
  Self.Fill(@AValue);
end;

procedure TImageHelper.FillRect(const ARect: TRect;  AValue: Byte);
begin
  Self.FillRect(ARect, @AValue);
end;

function TImageHelper.GetPixelByte(X, Y: Integer): Byte;
begin
  Assert(Valid and (Format = ifGray8));
  Assert((X >= 0) and (X < Width) and (Y >= 0) and (Y < Height));
  Result := PByteArray(Scanline[Y])[X];
end;

procedure TTestImageUtils.CreateTwoValueRect(AImage: TSingleImage; const ARect: TRect;
  Val1: Byte; Val2: Byte; IsHorizontalSplit: Boolean);
var
  HalfWay: Integer;
  Rect1, Rect2: TRect;
  ClippedRect: TRect;
begin
  Assert((AImage <> nil) and AImage.Valid and (AImage.Format = ifGray8));

  IntersectRect(ClippedRect, ARect, AImage.BoundsRect);
  Assert(not IsRectEmpty(ClippedRect));

  if IsHorizontalSplit then // Split horizontally, two vertical bands
  begin
    HalfWay := ClippedRect.Left + ClippedRect.Width div 2;
    Rect1 := Rect(ClippedRect.Left, ClippedRect.Top, HalfWay, ClippedRect.Bottom);
    Rect2 := Rect(HalfWay, ClippedRect.Top, ClippedRect.Right, ClippedRect.Bottom);
  end
  else // Split vertically, two horizontal bands
  begin
    HalfWay := ClippedRect.Top + ClippedRect.Height div 2;
    Rect1 := Rect(ClippedRect.Left, ClippedRect.Top, ClippedRect.Right, HalfWay);
    Rect2 := Rect(ClippedRect.Left, HalfWay, ClippedRect.Right, ClippedRect.Bottom);
  end;

  AImage.FillRect(Rect1, Val1);
  AImage.FillRect(Rect2, Val2);
end;

procedure TTestImageUtils.SetUp;
const
  FillColor: Byte = 255;
begin
  FTestImage := TSingleImage.CreateFromParams(128, 128, ifGray8);
  FTestImage.FillRect(FTestImage.BoundsRect, @FillColor); // Fill with white
end;

procedure TTestImageUtils.TearDown;
begin
  //FTestImage.SaveToFile('test-' + FTestName + '.png');
  FTestImage.Free;
end;

procedure TTestImageUtils.TestOtsu_WholeImage_SimpleSplit;
var
  Threshold: Integer;
begin
  CreateTwoValueRect(FTestImage, FTestImage.BoundsRect, 50, 200, True); // Split 50/200
  Threshold := OtsuThresholding(FTestImage.ImageDataPointer^);
  AssertTrue('Threshold should be between 50 and 200 (but is ' + IntToStr(Threshold) + ')',
    (Threshold > 50) and (Threshold < 200));
end;

procedure TTestImageUtils.TestOtsu_WholeImage_SolidColor;
var
  Threshold: Integer;
begin
  FTestImage.Fill(150);
  Threshold := OtsuThresholding(FTestImage.ImageDataPointer^, nil);
  AssertEquals('Threshold for solid color should be the color itself', 150, Threshold);
end;

procedure TTestImageUtils.TestOtsu_ContentRect_SimpleSplit;
var
  Threshold: Integer;
  ContentRect: TRect;
begin
  FTestImage.Fill(255);
  ContentRect := Rect(10, 10, 90, 90);
  CreateTwoValueRect(FTestImage, ContentRect, 30, 180, False); // Content 30/180
  Threshold := OtsuThresholding(FTestImage.ImageDataPointer^, @ContentRect);
  AssertTrue('Threshold (' + IntToStr(Threshold) + ') for content rect not between 30 and 180', (Threshold > 30) and (Threshold < 180));
end;

procedure TTestImageUtils.TestOtsu_ContentRect_SolidColor;
var
  Threshold: Integer;
  ContentRect: TRect;
begin
  FTestImage.Fill(255);
  ContentRect := Rect(20, 20, 80, 80);
  FTestImage.FillRect(ContentRect, 100); // Content solid 100
  Threshold := OtsuThresholding(FTestImage.ImageDataPointer^, @ContentRect);
  AssertEquals('Threshold for solid content rect should be the color itself', 100, Threshold);
end;

procedure TTestImageUtils.TestOtsu_ContentRect_IgnoresOutside;
var
  Threshold: Integer;
  ContentRect: TRect;
begin
  // Outside content rect: very dark and very light split
  CreateTwoValueRect(FTestImage, FTestImage.BoundsRect, 10, 240, True);

  // Content rect with a mid-range solid color
  ContentRect := Rect(25, 25, 75, 75);
  FTestImage.FillRect(ContentRect, 120);

  Threshold := OtsuThresholding(FTestImage.ImageDataPointer^, @ContentRect);
  AssertEquals('Threshold should be based on content rect (120), not outside junk', 120, Threshold);
end;

procedure TTestImageUtils.TestOtsu_ContentRect_Invalid;
var
  Threshold: Integer;
  EmptyRect, InvalidRect: TRect;
begin
  EmptyRect := Rect(10, 10, 10, 20); // Width = 0
  Threshold := OtsuThresholding(FTestImage.ImageDataPointer^, @EmptyRect);
  AssertEquals('Threshold for empty width rect should be default (128)', 128, Threshold);

  EmptyRect := Rect(10, 10, 20, 10); // Height = 0
  Threshold := OtsuThresholding(FTestImage.ImageDataPointer^, @EmptyRect);
  AssertEquals('Threshold for empty height rect should be default (128)', 128, Threshold);

  InvalidRect := Rect(50, 50, 40, 60); // Left > Right
  Threshold := OtsuThresholding(FTestImage.ImageDataPointer^, @InvalidRect);
  AssertEquals('Threshold for invalid rect (L>R) should be default (128)', 128, Threshold);

  InvalidRect := Rect(50, 50, 60, 40); // Top > Bottom
  Threshold := OtsuThresholding(FTestImage.ImageDataPointer^, @InvalidRect);
  AssertEquals('Threshold for invalid rect (T>B) should be default (128)', 128, Threshold);
end;

procedure TTestImageUtils.TestOtsu_ContentRect_Clipped;
var
  Threshold: Integer;
  OutsideRect: TRect;
begin
  // Create a small image
  FTestImage.CreateFromParams(20, 20, ifGray8);
  FTestImage.Fill(100); // Solid color

  // Define a rect that is largely outside the image
  OutsideRect := Rect(10, 10, 30, 30); // Valid part is (10,10,20,20)
  FTestImage.FillRect(Rect(10, 10, 20, 20), 50); // Fill the valid part with a different color

  Threshold := OtsuThresholding(FTestImage.ImageDataPointer^, @OutsideRect);
  // Since the valid part (10,10)-(19,19) is solid 50, threshold should be 50.
  AssertEquals('Threshold for clipped rect (solid 50) should be 50', 50, Threshold);
end;

procedure TTestImageUtils.TestBinarize_WholeImage_SimpleSplit;
var
  X, Y: Integer;
  RowPtr: PByte;
begin
  CreateTwoValueRect(FTestImage, FTestImage.BoundsRect, 50, 200, True);
  BinarizeImage(FTestImage.ImageDataPointer^, 100, nil);
  AssertTrue('Data format stays', ifGray8 = FTestImage.Format);

  for Y := 0 to FTestImage.Height - 1 do
  begin
    RowPtr := FTestImage.Scanline[Y];
    for X := 0 to FTestImage.Width - 1 do
    begin
      // Original value was 50 on the left and 200 on the right
      if X < FTestImage.Width div 2 then
        AssertEquals(Format('Pixel (%d,%d) originally 50 should be 0', [X, Y]), 0, RowPtr[X])
      else
        AssertEquals(Format('Pixel (%d,%d) originally 200 should be 255', [X, Y]), 255, RowPtr[X]);
    end;
  end;
end;

procedure TTestImageUtils.TestBinarize_ContentRect_LeavesOutsideUnchanged;
const
  OutsideColor: Byte = 150;
  InsideColor1: Byte = 40;
  InsideColor2: Byte = 210;
var
  X, Y: Integer;
  ContentRect: TRect;
  RowPtr: PByte;
begin
  FTestImage.Fill(OutsideColor);
  ContentRect := Rect(20, 20, 80, 80);
  CreateTwoValueRect(FTestImage, ContentRect, InsideColor1, InsideColor2, True);

  BinarizeImage(FTestImage.ImageDataPointer^, 128, @ContentRect);

  // Check pixels outside content rect remained OutsideColor
  for Y := 0 to FTestImage.Height - 1 do
  begin
    RowPtr := FTestImage.Scanline[Y];
    for X := 0 to FTestImage.Width - 1 do
    begin
      if PtInRect(ContentRect, Point(X,Y)) then
      begin // Inside content rect, check binarization
        if X < ContentRect.Left + ContentRect.Width div 2 then // Originally InsideColor1
           AssertTrue(Format('Pixel (%d,%d) binarized from %d', [X, Y, InsideColor1]), (RowPtr[X] = 0) or (RowPtr[X] = 255))
        else // Originally InsideColor2
           AssertTrue(Format('Pixel (%d,%d) binarized from %d', [X, Y, InsideColor2]), (RowPtr[X] = 0) or (RowPtr[X] = 255));
      end
      else // Outside content rect
      begin
        AssertEquals(Format('Pixel (%d,%d) outside CR should be unchanged', [X, Y]), OutsideColor, RowPtr[X]);
      end;
    end;
  end;
end;

procedure TTestImageRotation.CreateQuadrantPattern(AImage: TSingleImage);
var
  W, H, HalfW, HalfH: Integer;
begin
  Assert((AImage <> nil) and AImage.Valid);

  W := AImage.Width;
  H := AImage.Height;
  HalfW := W div 2;
  HalfH := H div 2;

  if AImage.Format = ifGray8 then
  begin
    AImage.FillRect(Rect(0, 0, HalfW, HalfH), @C8_GRAY1);  // Top-Left
    AImage.FillRect(Rect(HalfW, 0, W, HalfH), @C8_GRAY2);  // Top-Right
    AImage.FillRect(Rect(0, HalfH, HalfW, H), @C8_GRAY3);  // Bottom-Left
    AImage.FillRect(Rect(HalfW, HalfH, W, H), @C8_GRAY4);  // Bottom-Right
  end
  else if AImage.Format = ifR8G8B8 then
  begin
    AImage.FillRect(Rect(0, 0, HalfW, HalfH), @C32_RED);     // Top-Left: Red
    AImage.FillRect(Rect(HalfW, 0, W, HalfH), @C32_GREEN);   // Top-Right: Green
    AImage.FillRect(Rect(0, HalfH, HalfW, H), @C32_BLUE);    // Bottom-Left: Blue
    AImage.FillRect(Rect(HalfW, HalfH, W, H), @C32_YELLOW);  // Bottom-Right: Yellow
  end;
end;

procedure TTestImageRotation.CalcRotatedImageSize(CurrentWidth, CurrentHeight: Integer;
  AngleDeg: Integer; Filter: TResamplingFilter;
  out ExpectedWidth, ExpectedHeight: Integer);
var
  AngleRad, CosA, SinA: Double;
begin
  AngleRad := DegToRad(AngleDeg);
  SinCos(AngleRad, SinA, CosA);

  // Expected dimensions formula (for bounding box of rotated rectangle)
  ExpectedWidth := Ceil(Abs(CurrentWidth * CosA) + Abs(CurrentHeight * SinA));
  ExpectedHeight := Ceil(Abs(CurrentWidth * SinA) + Abs(CurrentHeight * CosA));

  // Set dimensions exactly for multiples of 90 degrees, ceil() + trig functions
  // woould cause one-off errors.
  if AngleDeg = 180 then
  begin
    ExpectedWidth := CurrentWidth;
    ExpectedHeight := CurrentHeight;
  end
  else if AngleDeg mod 90 = 0 then
  begin
    ExpectedWidth := CurrentHeight;
    ExpectedHeight := CurrentWidth;
  end;

  if Filter <> rfNearest then
  begin
    // RotateImage adds +1, if not nearest for antialiasing and angles not multiples of 90 degrees,
    // simulate that for comparison.
    if AngleDeg mod 90 <> 0 then
    begin
      ExpectedWidth := ExpectedWidth + 1;
      ExpectedHeight := ExpectedHeight + 1;
    end;
  end;
end;

procedure TTestImageRotation.SetUp;
begin
  // Default to Gray8 for many tests, specific tests can change format
  FOrigImage := TSingleImage.CreateFromParams(IMG_WIDTH, IMG_HEIGHT, ifGray8);
  FOrigImage.Fill(C8_WHITE);
  FTestImage := TSingleImage.CreateFromImage(FOrigImage);
end;

procedure TTestImageRotation.TearDown;
begin
  //FOrigImage.SaveToFile('orig-' + FTestName + '.png');
  //FTestImage.SaveToFile('test-' + FTestName + '.png');

  FOrigImage.Free;
  FTestImage.Free;
end;

procedure TTestImageRotation.Test_Rotate0Degrees_NoChange;
begin
  CreateQuadrantPattern(FOrigImage);
  FTestImage.Assign(FOrigImage);

  RotateImage(FTestImage.ImageDataPointer^, 0.0, 0, rfNearest, True);
  AssertEquals('Width should not change for 0 degree rotation', FOrigImage.Width, FTestImage.Width);
  AssertEquals('Height should not change for 0 degree rotation', FOrigImage.Height, FTestImage.Height);

  // Pixel-perfect check (difficult with interpolation, best for nearest or no-op)
  AssertTrue('Image content should be identical after 0 degree rotation',
    IsImageDataEqual(FOrigImage.ImageDataPointer^, FTestImage.ImageDataPointer^));
end;

procedure TTestImageRotation.Test_Rotate90Degrees_Gray8_Nearest;
var
  ExpectedW, ExpectedH: Integer;
begin
  CreateQuadrantPattern(FOrigImage);
  FTestImage.Assign(FOrigImage);

  RotateImage(FTestImage.ImageDataPointer^, 90.0, 0, rfNearest, True);

  ExpectedW := IMG_HEIGHT; // Original Height becomes new Width
  ExpectedH := IMG_WIDTH;  // Original Width becomes new Height
  AssertEquals('Width after 90 deg rotation', ExpectedW, FTestImage.Width);
  AssertEquals('Height after 90 deg rotation', ExpectedH, FTestImage.Height);

  // Check corners of the new image based on original quadrants
  // Original Top-Left (COLOR_GRAY1) should be new Bottom-Left
  AssertEquals('Bottom-Left pixel (orig Top-Left)', C8_GRAY1, FTestImage.GetPixelByte(0, FTestImage.Height - 1));
  // Original Top-Right (COLOR_GRAY2) should be new Top-Left
  AssertEquals('Top-Left pixel (orig Top-Right)', C8_GRAY2, FTestImage.GetPixelByte(0, 0));
  // Original Bottom-Left (COLOR_GRAY3) should be new Bottom-Right
  AssertEquals('Bottom-Right pixel (orig Bottom-Left)', C8_GRAY3, FTestImage.GetPixelByte(FTestImage.Width - 1, FTestImage.Height - 1));
  // Original Bottom-Right (COLOR_GRAY4) should be new Top-Right
  AssertEquals('Top-Right pixel (orig Bottom-Right)', C8_GRAY4, FTestImage.GetPixelByte(FTestImage.Width - 1, 0));
end;

procedure TTestImageRotation.Test_Rotate45Degrees_FitFalse_Gray8_Nearest;
begin
  CreateQuadrantPattern(FOrigImage);
  FTestImage.Assign(FOrigImage);

  RotateImage(FTestImage.ImageDataPointer^, 45.0, pcWhite, rfNearest, False);

  // Rotated image dimensions stay the same as the original un-rotated image
  AssertEquals('Width after rotation (Fit=False)', IMG_WIDTH, FTestImage.Width);
  AssertEquals('Height after rotation (Fit=False)', IMG_HEIGHT, FTestImage.Height);

  // Test a point that maps from original (14,40) [COLOR_GRAY1] to (10,50) in destination
  AssertEquals('Pixel @(10,50) (orig (14,40) -> COLOR_GRAY1)', C8_GRAY1, FTestImage.GetPixelByte(10, 50));

  // Test corners - they should be background color due to clipping
  AssertEquals('Top-Left (should be background)', C8_WHITE, FTestImage.GetPixelByte(0, 0));
  AssertEquals('Top-Right (should be background)', C8_WHITE, FTestImage.GetPixelByte(IMG_WIDTH - 1, 0));
  AssertEquals('Bottom-Left (should be background)', C8_WHITE, FTestImage.GetPixelByte(0, IMG_HEIGHT - 1));
  AssertEquals('Bottom-Right (should be background)', C8_WHITE, FTestImage.GetPixelByte(IMG_WIDTH - 1, IMG_HEIGHT - 1));
end;

procedure TTestImageRotation.Test_Rotate180Degrees_Gray8_Nearest;
begin
  CreateQuadrantPattern(FOrigImage);
  FTestImage.Assign(FOrigImage);

  RotateImage(FTestImage.ImageDataPointer^, 180.0, pcGreen, rfNearest, True);

  AssertEquals('Width after 180 deg rotation', IMG_WIDTH, FTestImage.Width);
  AssertEquals('Height after 180 deg rotation', IMG_HEIGHT, FTestImage.Height);

  // Original Top-Left (COLOR_GRAY1) should be new Bottom-Right
  AssertEquals('Bottom-Right pixel (orig Top-Left)', C8_GRAY1, FTestImage.GetPixelByte(IMG_WIDTH - 1, IMG_HEIGHT - 1));
  // Original Bottom-Right (COLOR_GRAY4) should be new Top-Left
  AssertEquals('Top-Left pixel (orig Bottom-Right)', C8_GRAY4, FTestImage.GetPixelByte(0, 0));
end;

procedure TTestImageRotation.Test_FitTrue_Dimensions_Gray8;
const
  Angles: array[1..12] of Integer = (90, 180, 270, -1, 1, 20, 45, 75, 111, 193, 217, 333);
var
  ExpectedW, ExpectedH: Integer;
  AngleStr: string;
  I: Integer;
begin
  FOrigImage.Fill(C8_WHITE);

  for I := Low(Angles) to High(Angles) do
  begin
    FTestImage.Assign(FOrigImage);
    AngleStr := IntToStr(Angles[I]);

    FTestImage.Assign(FOrigImage);
    // Use rfNearest for precise dimension check
    RotateImage(FTestImage.ImageDataPointer^, Angles[I], 0, rfNearest, True);
    CalcRotatedImageSize(IMG_WIDTH, IMG_HEIGHT, Angles[I], rfNearest,
                         ExpectedW, ExpectedH);

    AssertEquals('Width after ' + AngleStr  + ' deg rotation (nearest)', ExpectedW, FTestImage.Width);
    AssertEquals('Height after ' + AngleStr  + ' deg rotation (nearest)', ExpectedH, FTestImage.Height);

    FTestImage.Assign(FOrigImage);
    // Use some interpolation, mostly used and can have different size after rotation
    RotateImage(FTestImage.ImageDataPointer^, Angles[I], 0, rfCubic, True);
    CalcRotatedImageSize(IMG_WIDTH, IMG_HEIGHT, Angles[I], rfCubic,
                         ExpectedW, ExpectedH);

    AssertEquals('Width after ' + AngleStr  + ' deg rotation (cubic)', ExpectedW, FTestImage.Width);
    AssertEquals('Height after ' + AngleStr  + ' deg rotation (cubic)', ExpectedH, FTestImage.Height);
  end;
end;

procedure TTestImageRotation.Test_BackgroundColor_Gray8_Linear;
const
  Angles: array[1..5] of Integer = (1, 45, 75, 233, 359);
var
  BGColor: TColor32;
  BGColorByte: Byte;
  NewW, NewH, I: Integer;
  AngleStr: string;
begin
  // After rotation it will be white object on black background
  FOrigImage.Fill(C8_WHITE);
  BGColor := pcBlack;
  BGColorByte := 0;

  for I := Low(Angles) to High(Angles) do
  begin
    FTestImage.Assign(FOrigImage);
    AngleStr := IntToStr(Angles[I]);

    RotateImage(FTestImage.ImageDataPointer^, Angles[I], BGColor, rfLinear, True);

    NewW := FTestImage.Width;
    NewH := FTestImage.Height;

    // Check corners exactly, even for 1 degree rotations the interpolation won't affect
    // background in corners.
    AssertEquals('Top-Left @' + AngleStr, BGColorByte, FTestImage.GetPixelByte(0, 0));
    AssertEquals('Top-Right @' + AngleStr, BGColorByte, FTestImage.GetPixelByte(NewW - 1, 0));
    AssertEquals('Bottom-Left @' + AngleStr, BGColorByte, FTestImage.GetPixelByte(0, NewH - 1));
    AssertEquals('Bottom-Right @' + AngleStr, BGColorByte, FTestImage.GetPixelByte(NewW - 1, NewH - 1));
  end;
end;

procedure TTestImageRotation.Test_Rotate90Degrees_FitTrue_RGB24_Nearest;
var
  ExpectedW, ExpectedH: Integer;
  PixelValue: TColor24Rec;
  ExpectedColor_OrigTL, ExpectedColor_OrigTR, ExpectedColor_OrigBL, ExpectedColor_OrigBR: TColor24Rec;
begin
  FOrigImage.CreateFromParams(IMG_WIDTH, IMG_HEIGHT, ifR8G8B8);
  CreateQuadrantPattern(FOrigImage);
  FTestImage.Assign(FOrigImage);

  RotateImage(FTestImage.ImageDataPointer^, 45.0, pcBlack, rfLinear, True);

  CalcRotatedImageSize(IMG_WIDTH, IMG_HEIGHT, 45, rfLinear,
                       ExpectedW, ExpectedH);

  AssertEquals('Width after rotation (Fit=True, RGB24)', ExpectedW, FTestImage.Width);
  AssertEquals('Height after rotation (Fit=True, RGB24)', ExpectedH, FTestImage.Height);

  // From CreateQuadrantPattern for RGB24:
  ExpectedColor_OrigTL := TColor32Rec(C32_RED).Color24Rec;
  ExpectedColor_OrigTR := TColor32Rec(C32_GREEN).Color24Rec;
  ExpectedColor_OrigBL := TColor32Rec(C32_BLUE).Color24Rec;
  ExpectedColor_OrigBR := TColor32Rec(C32_YELLOW).Color24Rec;

  // Original Top-Left (Red) should be new Bottom-Left
  PixelValue := PColor24Rec(FTestImage.PixelPointer[ExpectedW div 4, ExpectedH div 2])^;
  AssertEquals('Orig Top-Left [Red] at new pos', ExpectedColor_OrigTL, PixelValue);
  // Original Top-Right (Green) should be new Top-Left
  PixelValue := PColor24Rec(FTestImage.PixelPointer[ExpectedW div 2, ExpectedH div 4])^;
  AssertEquals('Orig Top-Right [Green] at new pos', ExpectedColor_OrigTR, PixelValue);
  // Original Bottom-Left (Blue) should be new Bottom-Right
  PixelValue := PColor24Rec(FTestImage.PixelPointer[ExpectedW div 2, ExpectedH div 4 * 3])^;
  AssertEquals('Orig Bottom-Left [Blue] at new pos', ExpectedColor_OrigBL, PixelValue);
  // Original Bottom-Right (White) should be new Top-Right
  PixelValue := PColor24Rec(FTestImage.PixelPointer[ExpectedW div 4 * 3, ExpectedH div 2])^;
  AssertEquals('Orig Bottom-Right [Yellow] at new pos', ExpectedColor_OrigBR, PixelValue);
end;

initialization
  RegisterTest(TTestImageUtils);
  RegisterTest(TTestImageRotation);
end.
