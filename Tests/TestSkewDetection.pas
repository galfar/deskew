unit TestSkewDetection;

interface

uses
  Types, Classes, SysUtils,
  DeskewTestUtils,
  ImagingTypes, Imaging, ImagingClasses, ImagingUtility,
  Utils,
  ImageUtils,
  RotationDetector;

type
  { Base for tests working with synthetic page images: white 8bit grayscale
    page with black "text" and other stuff drawn on it. }
  TPageImageTestCase = class(TDeskewTestCase)
  protected const
    PageWidth = 600;
    PageHeight = 800;
    Threshold = 128;
    ColorBlack = 0;
    ColorWhite = 255;
  protected
    FImage: TSingleImage;
    FRandSeed: Cardinal;

    procedure SetUp; override;
    procedure TearDown; override;

    // Deterministic pseudo random numbers in range [0, Range)
    function NextRandom(Range: Integer): Integer;
    // Draws lines of glyph-like blocks with base lines skewed by given angle.
    // Positive angle: lines go down towards the right side of the page.
    procedure DrawTextLines(Angle: Double; const Area: TRect; LineHeight: Integer = 24);
    // Draws thin straight line across the area skewed by given angle
    procedure DrawRule(Angle: Double; const Area: TRect; Y: Integer);
    procedure DrawDots(Count: Integer);
    // Makes everything outside of the page area black (like a scanner bed)
    procedure DrawBedAround(const PageArea: TRect);
  end;

  TTestSkewDetection = class(TPageImageTestCase)
  private
    FStats: TCalcSkewAngleStats;

    function Detect(MaxAngle: Double = 10; AngleStep: Double = 0.1; DetectionArea: PRect = nil): Double;
    procedure AssertAngle(const AMessage: string; Expected, Actual: Double; Tolerance: Double = 0.1);
    procedure AssertOutcome(const AMessage: string; Expected: TSkewDetectionOutcome);
  published
    procedure TestTextAngles;
    procedure TestStats;
    procedure TestBlankPage;
    procedure TestFewPixels;
    procedure TestRandomDots;
    procedure TestSkewFarOutsideRange;
    procedure TestSkewJustOutsideRange;
    procedure TestLongLinesDoNotOutvoteText;
    procedure TestDetectionArea;
    procedure TestTooLargeAccumulator;
  end;

  TTestContentDetection = class(TPageImageTestCase)
  private
    FContentRect: TRect;

    function DetectContent(SearchRect: PRect = nil): Boolean;
    // Checks that the found content rect contains Expected rect and is not
    // larger than it by more than the tolerance on any side.
    procedure AssertContentRect(const AMessage: string; const Expected: TRect; Tolerance: Integer = 16);
  published
    procedure TestBlankPage;
    procedure TestTextBlock;
    procedure TestDustIsIgnored;
    procedure TestEmptyPageOnBed;
    procedure TestTextPageOnBed;
    procedure TestSearchRect;
  end;

implementation

uses
  Math;

{ TPageImageTestCase }

procedure TPageImageTestCase.SetUp;
begin
  FImage := TSingleImage.CreateFromParams(PageWidth, PageHeight, ifGray8);
  FImage.Fill(ColorWhite);
  FRandSeed := 12345;
end;

procedure TPageImageTestCase.TearDown;
begin
  FImage.Free;
end;

function TPageImageTestCase.NextRandom(Range: Integer): Integer;
begin
  // Numerical Recipes LCG, upper bits are random enough for drawing
  FRandSeed := Cardinal((Int64(FRandSeed) * 1664525 + 1013904223) and $FFFFFFFF);
  Result := Integer((FRandSeed shr 8) mod Cardinal(Range));
end;

procedure TPageImageTestCase.DrawTextLines(Angle: Double; const Area: TRect; LineHeight: Integer);
var
  X, Y, GlyphWidth, GlyphHeight, BaseLine, CenterX: Integer;
  Slope: Double;
begin
  Slope := Tan(DegToRad(Angle));
  CenterX := (Area.Left + Area.Right) div 2;
  Y := Area.Top + LineHeight;

  while Y <= Area.Bottom do
  begin
    X := Area.Left;
    while X < Area.Right - 10 do
    begin
      GlyphWidth := 5 + NextRandom(6);
      GlyphHeight := 10 + 4 * NextRandom(2);
      BaseLine := Y + Round((X - CenterX) * Slope);
      FImage.FillRect(Rect(X, BaseLine - GlyphHeight, X + GlyphWidth - 2, BaseLine), ColorBlack);
      Inc(X, GlyphWidth);
      if NextRandom(6) = 0 then
        Inc(X, 8); // space between words
    end;
    Inc(Y, LineHeight);
  end;
end;

procedure TPageImageTestCase.DrawRule(Angle: Double; const Area: TRect; Y: Integer);
var
  X, CenterX, LineY: Integer;
  Slope: Double;
begin
  Slope := Tan(DegToRad(Angle));
  CenterX := (Area.Left + Area.Right) div 2;
  for X := Area.Left to Area.Right - 1 do
  begin
    LineY := Y + Round((X - CenterX) * Slope);
    FImage.FillRect(Rect(X, LineY - 2, X + 1, LineY), ColorBlack);
  end;
end;

procedure TPageImageTestCase.DrawDots(Count: Integer);
var
  I, X, Y: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    X := NextRandom(FImage.Width);
    Y := NextRandom(FImage.Height);
    FImage.FillRect(Rect(X, Y, X + 1, Y + 1), ColorBlack);
  end;
end;

procedure TPageImageTestCase.DrawBedAround(const PageArea: TRect);
begin
  FImage.FillRect(Rect(0, 0, FImage.Width, PageArea.Top), ColorBlack);
  FImage.FillRect(Rect(0, PageArea.Bottom, FImage.Width, FImage.Height), ColorBlack);
  FImage.FillRect(Rect(0, 0, PageArea.Left, FImage.Height), ColorBlack);
  FImage.FillRect(Rect(PageArea.Right, 0, FImage.Width, FImage.Height), ColorBlack);
end;

{ TTestSkewDetection }

function TTestSkewDetection.Detect(MaxAngle, AngleStep: Double; DetectionArea: PRect): Double;
begin
  FillChar(FStats, SizeOf(FStats), 0);
  Result := CalcRotationAngle(MaxAngle, AngleStep, Threshold, FImage.Width, FImage.Height,
    FImage.Bits, DetectionArea, @FStats);
end;

procedure TTestSkewDetection.AssertAngle(const AMessage: string; Expected, Actual: Double; Tolerance: Double);
begin
  AssertTrue(Format('%s: expected angle %.3f but was %.3f', [AMessage, Expected, Actual]),
    Abs(Expected - Actual) <= Tolerance);
end;

procedure TTestSkewDetection.AssertOutcome(const AMessage: string; Expected: TSkewDetectionOutcome);
begin
  AssertTrue(Format('%s: expected outcome %d but was %d (confidence %.3f)',
    [AMessage, Ord(Expected), Ord(FStats.Outcome), FStats.Confidence]), Expected = FStats.Outcome);
end;

procedure TTestSkewDetection.TestTextAngles;
const
  Angles: array[0..5] of Double = (0, 0.3, 2, -2, 5.5, -9);
var
  I: Integer;
  Angle: Double;
begin
  for I := Low(Angles) to High(Angles) do
  begin
    FImage.Fill(ColorWhite);
    DrawTextLines(Angles[I], Rect(60, 60, 540, 740));

    Angle := Detect;

    AssertAngle('Text at ' + FloatToStr(Angles[I]), Angles[I], Angle);
    AssertOutcome('Text at ' + FloatToStr(Angles[I]), sdoDetected);
    AssertTrue('Confidence above minimum', FStats.Confidence >= MinDetectionConfidence);
    AssertAngle('Peak angle', Angles[I], FStats.PeakAngle, 0.15);
  end;
end;

procedure TTestSkewDetection.TestStats;
var
  Area: TRect;
begin
  DrawTextLines(1, Rect(60, 60, 540, 740));

  Detect;
  // Last row cannot be tested (needs a row below)
  AssertTrue('Pixel count: page', FStats.PixelCount = Int64(PageWidth) * (PageHeight - 1));
  AssertTrue('Tested pixels', (FStats.TestedPixels > 1000) and (FStats.TestedPixels < FStats.PixelCount));
  // Each tested pixel votes once for each angle step
  AssertTrue('Accumulated counts', FStats.AccumulatedCounts = FStats.TestedPixels * 200);
  AssertTrue('Accumulator size', FStats.AccumulatorSize = 2 * (PageWidth + PageHeight - 1) * 200);
  AssertTrue('Best count', (FStats.BestCount > 100) and (FStats.BestCount <= PageWidth));
  AssertTrue('Lines at peak', FStats.LinesAtPeak = 20);
  AssertTrue('Sharpness', (FStats.Sharpness > 0.5) and (FStats.Sharpness <= 1));
  AssertTrue('Confidence', (FStats.Confidence > 0.9) and (FStats.Confidence <= 1));

  Area := Rect(100, 100, 300, 500);
  Detect(10, 0.1, @Area);
  AssertTrue('Pixel count: area', FStats.PixelCount = 200 * 400);
end;

procedure TTestSkewDetection.TestBlankPage;
var
  Angle: Double;
begin
  // Nothing at all on the page: there used to be -MaxAngle returned
  Angle := Detect;

  AssertEquals('Angle of blank page', 0.0, Angle);
  AssertOutcome('Blank page', sdoNoContent);
  AssertEquals('Confidence', 0.0, FStats.Confidence);
  AssertTrue('No tested pixels', FStats.TestedPixels = 0);
  AssertTrue('No lines', FStats.BestCount = 0);
end;

procedure TTestSkewDetection.TestFewPixels;
begin
  DrawDots(8);
  Detect;
  AssertOutcome('Few dots', sdoNoContent);
  AssertEquals('Confidence', 0.0, FStats.Confidence);
end;

procedure TTestSkewDetection.TestRandomDots;
begin
  // Sparse dust
  DrawDots(500);
  Detect;
  AssertOutcome('Dust', sdoLowConfidence);
  AssertTrue('Dust confidence', FStats.Confidence < MinDetectionConfidence);

  // Dense noise
  DrawDots(40000);
  Detect;
  AssertOutcome('Noise', sdoLowConfidence);
  AssertTrue('Noise confidence', FStats.Confidence < MinDetectionConfidence);
end;

procedure TTestSkewDetection.TestSkewFarOutsideRange;
begin
  DrawTextLines(20, Rect(60, 100, 540, 700));
  Detect(5);
  AssertTrue('Outcome is not "detected"', FStats.Outcome <> sdoDetected);

  // Larger max angle finds it
  AssertAngle('Larger range', 20, Detect(25));
  AssertOutcome('Larger range', sdoDetected);
end;

procedure TTestSkewDetection.TestSkewJustOutsideRange;
begin
  DrawTextLines(5.3, Rect(60, 60, 540, 740));
  Detect(5);
  AssertOutcome('Just outside', sdoAtRangeLimit);
  AssertAngle('Peak at the range limit', 4.9, FStats.PeakAngle, 0.001);

  AssertAngle('Larger range', 5.3, Detect(10));
  AssertOutcome('Larger range', sdoDetected);
end;

procedure TTestSkewDetection.TestLongLinesDoNotOutvoteText;
var
  Angle: Double;
begin
  // Text is skewed by 2 degrees and there are a few long straight lines
  // going in a different direction (like edges of pages in a book scan).
  // These lines are much stronger than any text line.
  DrawTextLines(2, Rect(100, 100, 500, 700));
  DrawRule(-3, Rect(10, 0, 590, 800), 50);
  DrawRule(-3, Rect(10, 0, 590, 800), 60);
  DrawRule(-3, Rect(10, 0, 590, 800), 750);

  Angle := Detect;

  AssertAngle('Angle of text', 2, Angle);
  AssertAngle('Peak angle', 2, FStats.PeakAngle, 0.15);
  AssertOutcome('Text with long lines', sdoDetected);
  AssertTrue('Strongest lines are not the text', FStats.LinesAtPeak < 20);
end;

procedure TTestSkewDetection.TestDetectionArea;
var
  Area: TRect;
begin
  // Upper half of the page is skewed differently than the lower one
  DrawTextLines(-4, Rect(60, 40, 540, 380));
  DrawTextLines(3, Rect(60, 420, 540, 760));

  Area := Rect(40, 20, 560, 390);
  AssertAngle('Upper half', -4, Detect(10, 0.1, @Area));
  AssertOutcome('Upper half', sdoDetected);

  Area := Rect(40, 410, 560, 780);
  AssertAngle('Lower half', 3, Detect(10, 0.1, @Area));
  AssertOutcome('Lower half', sdoDetected);
end;

procedure TTestSkewDetection.TestTooLargeAccumulator;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    // 2 * (600 + 799) distances * 1 800 000 angles
    Detect(90, 0.0001);
  except
    on E: Exception do
      Raised := Pos('too large', E.Message) > 0;
  end;
  AssertTrue('Exception raised', Raised);
end;

{ TTestContentDetection }

function TTestContentDetection.DetectContent(SearchRect: PRect): Boolean;
begin
  Result := DetectContentRect(FImage.ImageDataPointer^, Threshold, FContentRect, SearchRect);
end;

procedure TTestContentDetection.AssertContentRect(const AMessage: string; const Expected: TRect; Tolerance: Integer);
begin
  AssertTrue(
    Format('%s: content rect [%d,%d,%d,%d] does not match [%d,%d,%d,%d] (tolerance %d)',
      [AMessage, FContentRect.Left, FContentRect.Top, FContentRect.Right, FContentRect.Bottom,
       Expected.Left, Expected.Top, Expected.Right, Expected.Bottom, Tolerance]),
    (FContentRect.Left <= Expected.Left) and (FContentRect.Left >= Expected.Left - Tolerance) and
    (FContentRect.Top <= Expected.Top) and (FContentRect.Top >= Expected.Top - Tolerance) and
    (FContentRect.Right >= Expected.Right) and (FContentRect.Right <= Expected.Right + Tolerance) and
    (FContentRect.Bottom >= Expected.Bottom) and (FContentRect.Bottom <= Expected.Bottom + Tolerance));
end;

procedure TTestContentDetection.TestBlankPage;
begin
  AssertFalse('Blank page', DetectContent);
  AssertEquals('Null rect', NullRect, FContentRect);
end;

procedure TTestContentDetection.TestTextBlock;
begin
  FImage.FillRect(Rect(200, 300, 400, 500), ColorBlack);
  AssertTrue('Solid block', DetectContent);
  AssertContentRect('Solid block', Rect(200, 300, 400, 500));

  FImage.Fill(ColorWhite);
  DrawTextLines(0, Rect(100, 150, 500, 650));
  AssertTrue('Text', DetectContent);
  // First base line is one line height below the top of the area, glyphs are up to 14 px tall
  AssertContentRect('Text', Rect(100, 150 + 24 - 10, 480, 630), 24);
end;

procedure TTestContentDetection.TestDustIsIgnored;
begin
  DrawDots(60);
  AssertFalse('Only dust', DetectContent);

  FImage.FillRect(Rect(200, 300, 400, 500), ColorBlack);
  AssertTrue('Block and dust', DetectContent);
  AssertContentRect('Block and dust', Rect(200, 300, 400, 500));
end;

procedure TTestContentDetection.TestEmptyPageOnBed;
begin
  // Empty white page on black scanner bed
  DrawBedAround(Rect(50, 40, 560, 770));
  AssertFalse('Empty page on bed', DetectContent);

  // Bed only on some sides
  FImage.Fill(ColorWhite);
  FImage.FillRect(Rect(0, 0, PageWidth, 30), ColorBlack);
  FImage.FillRect(Rect(0, 0, 45, PageHeight), ColorBlack);
  AssertFalse('Empty page in the corner of bed', DetectContent);

  // All black
  FImage.Fill(ColorBlack);
  AssertFalse('All black', DetectContent);
end;

procedure TTestContentDetection.TestTextPageOnBed;
begin
  DrawTextLines(0, Rect(150, 150, 450, 650));
  DrawBedAround(Rect(50, 40, 560, 770));
  AssertTrue('Text page on bed', DetectContent);
  AssertContentRect('Text page on bed', Rect(150, 150 + 24 - 10, 430, 630), 24);
end;

procedure TTestContentDetection.TestSearchRect;
var
  SearchRect: TRect;
begin
  FImage.FillRect(Rect(100, 100, 200, 200), ColorBlack);
  FImage.FillRect(Rect(350, 500, 500, 700), ColorBlack);

  AssertTrue('Whole page', DetectContent);
  AssertContentRect('Whole page', Rect(100, 100, 500, 700));

  SearchRect := Rect(300, 400, 600, 800);
  AssertTrue('Lower right part', DetectContent(@SearchRect));
  AssertContentRect('Lower right part', Rect(350, 500, 500, 700));

  // Blocks touching the edge of search area are not content (like the scanner bed)
  SearchRect := Rect(150, 150, 300, 400);
  AssertFalse('Block reaching over the edge', DetectContent(@SearchRect));

  SearchRect := Rect(220, 220, 340, 480);
  AssertFalse('Empty part', DetectContent(@SearchRect));

  // Search area outside of the image
  SearchRect := Rect(700, 900, 800, 1000);
  AssertFalse('Outside', DetectContent(@SearchRect));
end;

initialization
  RegisterTest(TTestSkewDetection);
  RegisterTest(TTestContentDetection);
end.
