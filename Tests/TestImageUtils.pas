unit TestImageUtils;

interface

uses
  Types, Classes, SysUtils,
  DeskewTestUtils,
  ImagingTypes, ImagingClasses,
  ImageUtils;

type
  TTestImageUtils = class(TDeskewTestCase)
  private
    FTestImage: TSingleImage;

    // Helper to fill a rectangle in an image with a specific color
    procedure FillRectInImage(AImage: TSingleImage; const ARect: TRect; AColor: Byte);
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

implementation

procedure TTestImageUtils.FillRectInImage(AImage: TSingleImage; const ARect: TRect; AColor: Byte);
var
  X, Y: Integer;
  RowPtr: PByte;
  ClippedRect: TRect;
begin
  Assert((AImage <> nil) and AImage.Valid and (AImage.Format = ifGray8));

  IntersectRect(ClippedRect, ARect, AImage.BoundsRect);
  Assert(not IsRectEmpty(ClippedRect));

  for Y := ClippedRect.Top to ClippedRect.Bottom - 1 do
  begin
    RowPtr := AImage.Scanline[Y];
    for X := ClippedRect.Left to ClippedRect.Right - 1 do
    begin
      RowPtr[X] := AColor;
    end;
  end;
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
  FillRectInImage(AImage, Rect1, Val1);
  FillRectInImage(AImage, Rect2, Val2);
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
  FillRectInImage(FTestImage, FTestImage.BoundsRect, 150);
  Threshold := OtsuThresholding(FTestImage.ImageDataPointer^, nil);
  AssertEquals('Threshold for solid color should be the color itself', 150, Threshold);
end;

procedure TTestImageUtils.TestOtsu_ContentRect_SimpleSplit;
var
  Threshold: Integer;
  ContentRect: TRect;
begin
  FillRectInImage(FTestImage, FTestImage.BoundsRect, 255); // White background
  ContentRect := Rect(10, 10, 90, 90);
  CreateTwoValueRect(FTestImage, ContentRect, 30, 180, False); // Content 30/180
  Threshold := OtsuThresholding(FTestImage.ImageDataPointer^, @ContentRect);
  AssertTrue('Threshold ('+IntToStr(Threshold)+') for content rect not between 30 and 180', (Threshold > 30) and (Threshold < 180));
end;

procedure TTestImageUtils.TestOtsu_ContentRect_SolidColor;
var
  Threshold: Integer;
  ContentRect: TRect;
begin
  FillRectInImage(FTestImage, FTestImage.BoundsRect, 255); // White background
  ContentRect := Rect(20, 20, 80, 80);
  FillRectInImage(FTestImage, ContentRect, 100); // Content solid 100
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
  FillRectInImage(FTestImage, ContentRect, 120);

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
  FTestImage.Resize(20, 20, ImagingTypes.rfNearest);
  FillRectInImage(FTestImage, FTestImage.BoundsRect, 100); // Solid color

  // Define a rect that is largely outside the image
  OutsideRect := Rect(10, 10, 30, 30); // Valid part is (10,10,20,20)
  FillRectInImage(FTestImage, Rect(10, 10, 20, 20), 50); // Fill the valid part with a different color

  Threshold := OtsuThresholding(FTestImage.ImageDataPointer^, @OutsideRect);
  // Since the valid part (10,10)-(19,19) is solid 50, threshold should be 50.
  AssertEquals('Threshold for clipped rect (solid 50) should be 50', 50, Threshold);
end;

procedure TTestImageUtils.TestBinarize_WholeImage_SimpleSplit;
var
  Threshold, X, Y: Integer;
  RowPtr: PByte;
begin
  CreateTwoValueRect(FTestImage, FTestImage.BoundsRect, 50, 200, True);
  Threshold := 100;
  BinarizeImage(FTestImage.ImageDataPointer^, 100, nil);
  AssertTrue('Data format stays', ifGray8 = FTestImage.Format);

  for Y := 0 to FTestImage.Height - 1 do
  begin
    RowPtr := FTestImage.Scanline[Y];
    for X := 0 to FTestImage.Width - 1 do
    begin
      // Original value was 50 on the left and 200 on the right
      if X < FTestImage.Width div 2 then
        AssertEquals('Pixel ('+IntToStr(X)+','+IntToStr(Y)+') originally 50 should be 0', 0, RowPtr[X])
      else
        AssertEquals('Pixel ('+IntToStr(X)+','+IntToStr(Y)+') originally 200 should be 255', 255, RowPtr[X]);
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
  FillRectInImage(FTestImage, FTestImage.BoundsRect, OutsideColor);
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
           AssertTrue('Pixel ('+IntToStr(X)+','+IntToStr(Y)+') binarized from '+IntToStr(InsideColor1), (RowPtr[X] = 0) or (RowPtr[X] = 255))
        else // Originally InsideColor2
           AssertTrue('Pixel ('+IntToStr(X)+','+IntToStr(Y)+') binarized from '+IntToStr(InsideColor2), (RowPtr[X] = 0) or (RowPtr[X] = 255));
      end
      else // Outside content rect
      begin
        AssertEquals('Pixel ('+IntToStr(X)+','+IntToStr(Y)+') outside CR should be unchanged', OutsideColor, RowPtr[X]);
      end;
    end;
  end;
end;

initialization
  RegisterTest(TTestImageUtils);
end.
