unit TestCmdLineArgs;

interface

uses
  Types, Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFramework,
{$ENDIF}
  Utils,
  ImagingUtility,
  CmdLineOptions;

type
  TTestCmdLineOptions= class(TTestCase)
  private
    FCmdOptions: TCmdLineOptions;

{$IFNDEF FPC}
    procedure AssertTrue(const Msg: string; const ACondition: Boolean;
      AddrOfError: Pointer = nil); overload;
    procedure AssertTrue(const ACondition: Boolean); overload;
    procedure AssertFalse(const Msg: string; const ACondition: Boolean;
      AddrOfError: Pointer = nil); overload;

    procedure AssertEquals(const AMessage: string; Expected, Actual: string); overload;
    procedure AssertEquals(Expected, Actual: string); overload;
{$ENDIF}

    procedure AssertEquals(const AMessage: string; Expected, Actual: Double); overload;
    procedure AssertEquals(const AMessage: string; const Expected, Actual: TFloatRect); overload;
    procedure AssertEquals(const AMessage: string; const Expected, Actual: TRect); overload;

    procedure AssertParseSuccesAndEmptyError(const ParseResult: Boolean; const AMessage: string = '');
    procedure AssertParseFailAndErrorContains(const ParseResult: Boolean; const ErrorMsgPart: string;
      const AMessage: string = '');

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Test basics
    procedure TestParsing;
    procedure TestDefaults;

    // Test input and output
    procedure TestInputOnly;
    procedure TestInputOutput;

    // Test numerical options
    procedure TestMaxAngle;
    procedure TestAngleStep;
    procedure TestSkipAngle;
    procedure TestDpiOverride;

    // Test more complex params
    procedure TestThreshold;
    procedure TestResamplingFilter;
    procedure TestOutputFormat;

    // Test flags
    procedure TestOperationalFlags;
    procedure TestInfoFlags;

    // Test content area
    procedure TestContentRect;
    procedure TestContentMargins;
    procedure TestCalcDetectionRect;
  end;

implementation

uses
  Math, StrUtils,
  ImageUtils, ImagingTypes, Imaging;

const
  Epsilon = 0.001;

{$IFNDEF FPC}
procedure TTestCmdLineOptions.AssertTrue(const Msg: string; const ACondition: Boolean;
  AddrOfError: Pointer);
begin
  CheckTrue(ACondition, Msg);
end;

procedure TTestCmdLineOptions.AssertTrue(const ACondition: Boolean);
begin
  CheckTrue(ACondition);
end;

procedure TTestCmdLineOptions.AssertFalse(const Msg: string; const ACondition: Boolean;
  AddrOfError: Pointer);
begin
  CheckFalse(ACondition, Msg);
end;

procedure TTestCmdLineOptions.AssertEquals(const AMessage: string; Expected, Actual: string);
begin
  CheckEquals(Expected, Actual, AMessage);
end;

procedure TTestCmdLineOptions.AssertEquals(Expected, Actual: string);
begin
  CheckEquals(Expected, Actual);
end;

{$ENDIF}

procedure TTestCmdLineOptions.AssertEquals(const AMessage: string; Expected, Actual: Double);
begin
{$IFDEF FPC}
  AssertTrue(ComparisonMsg(AMessage, FloatToStr(Expected), FloatToStr(Actual)),
    SameValue(Expected, Actual, Epsilon), CallerAddr);
{$ELSE}
  CheckEquals(Expected, Actual, Epsilon, AMessage);
{$ENDIF}
end;

procedure TTestCmdLineOptions.AssertEquals(const AMessage: string; const Expected, Actual: TFloatRect);
const
  SCompare = '"%s" expected: <%g,%g,%g,%g> but was: <%g,%g,%g,%g>';

  function AreFloatRectsEqual(const R1, R2: TFloatRect): Boolean;
  begin
    Result := SameValue(R1.Left, R2.Left, Epsilon) and
              SameValue(R1.Top, R2.Top, Epsilon) and
              SameValue(R1.Right, R2.Right, Epsilon) and
              SameValue(R1.Bottom, R2.Bottom, Epsilon);
  end;

begin
  AssertTrue(
    Format(SCompare, [AMessage,
      Expected.Left, Expected.Top, Expected.Right, Expected.Bottom,
      Actual.Left, Actual.Top, Actual.Right, Actual.Bottom]),
    AreFloatRectsEqual(Expected, Actual), CallerAddr);
end;

procedure TTestCmdLineOptions.AssertEquals(const AMessage: string; const Expected, Actual: TRect);
const
  SCompare = '"%s" expected: <%d,%d,%d,%d> but was: <%d,%d,%d,%d>';
begin
  AssertTrue(
    Format(SCompare, [AMessage,
      Expected.Left, Expected.Top, Expected.Right, Expected.Bottom,
      Actual.Left, Actual.Top, Actual.Right, Actual.Bottom]),
    EqualRect(Expected, Actual), CallerAddr);
end;

procedure TTestCmdLineOptions.AssertParseSuccesAndEmptyError(const ParseResult: Boolean; const AMessage: string);
var
  Prefix: string;
begin
  Prefix := Iff(AMessage = '', '', AMessage + ': ');
  AssertTrue(Prefix + 'Parse, ErrorMsg: ' + FCmdOptions.ErrorMessage, ParseResult, CallerAddr);
  AssertTrue(Prefix + 'IsValid', FCmdOptions.IsValid, CallerAddr);
  AssertTrue(Prefix + 'Error Message Empty', '' = FCmdOptions.ErrorMessage, CallerAddr);
end;

procedure TTestCmdLineOptions.AssertParseFailAndErrorContains(const ParseResult: Boolean; const ErrorMsgPart: string;
  const AMessage: string = '');
var
  Prefix: string;
begin
  Prefix := Iff(AMessage = '', '', AMessage + ': ');
  AssertFalse(Prefix + 'Parse, ErrorMsg: ' + FCmdOptions.ErrorMessage, ParseResult, CallerAddr);
  AssertFalse(Prefix + 'IsValid', FCmdOptions.IsValid, CallerAddr);
  AssertTrue(Prefix + 'Error Message Contains "' + ErrorMsgPart + '" in "' + FCmdOptions.ErrorMessage + '"',
             ContainsText(FCmdOptions.ErrorMessage, ErrorMsgPart),
             CallerAddr);
end;

procedure TTestCmdLineOptions.SetUp;
begin
  FCmdOptions := TCmdLineOptions.Create;
end;

procedure TTestCmdLineOptions.TearDown;
begin
  FCmdOptions.Free;
end;

procedure TTestCmdLineOptions.TestParsing;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-z', '123', 'in.jpg']);
  AssertParseFailAndErrorContains(ParseResult, 'Unknown parameter: -z', 'Unknown parameter');

  ParseResult := FCmdOptions.Parse(['in.jpg', '-a']);
  AssertParseFailAndErrorContains(ParseResult, 'Missing value for parameter: -a', 'Missing value');

  ParseResult := FCmdOptions.Parse(['in.jpg', '-a', '1']);
  AssertParseSuccesAndEmptyError(ParseResult, 'Param after input');
end;

procedure TTestCmdLineOptions.TestDefaults;
begin
  // Check default values set in the constructor - No parsing needed
  AssertEquals('Default MaxAngle', DefaultMaxAngle, FCmdOptions.MaxAngle);
  AssertEquals('Default SkipAngle', DefaultSkipAngle, FCmdOptions.SkipAngle);
  AssertEquals('Default Threshold Level', DefaultThreshold, FCmdOptions.ThresholdLevel);
  AssertTrue('Default Threshold Method', tmOtsu = FCmdOptions.ThresholdingMethod);
  AssertTrue('Default Resampling Filter', rfLinear = FCmdOptions.ResamplingFilter);
  AssertEquals('Default Background Color', $FF000000, FCmdOptions.BackgroundColor);
  AssertTrue('Default Output Format', ifUnknown = FCmdOptions.ForcedOutputFormat);
  AssertEquals('Default DPI Override', 0, FCmdOptions.DpiOverride);
  AssertEquals('Default JPEG Quality', -1, FCmdOptions.JpegCompressionQuality);
  AssertEquals('Default TIFF Compression', -1, FCmdOptions.TiffCompressionScheme);
  AssertTrue('Default OperationalFlags empty', FCmdOptions.OperationalFlags = []);
  AssertEquals('Default Input File Empty', '', FCmdOptions.InputFileName);
  AssertEquals('Default Output File Empty', '', FCmdOptions.OutputFileName);
  AssertEquals('Default Error Message Empty', '', FCmdOptions.ErrorMessage);

  AssertFalse('Default ShowStats', FCmdOptions.ShowDetectionStats);
  AssertFalse('Default ShowParams', FCmdOptions.ShowParams);
  AssertFalse('Default ShowTimings', FCmdOptions.ShowTimings);
  AssertFalse('Default SaveWorkImage', FCmdOptions.SaveWorkImage);

  AssertFalse('Default IsValid (not parsed)', FCmdOptions.IsValid);
end;

procedure TTestCmdLineOptions.TestInputOnly;
var
  ParseResult: Boolean;
  Input: string;
  ExpectedOutput: string;
begin
  Input := 'input.jpg';
  ExpectedOutput := EnsureTrailingPathDelimiter(ExtractFileDir(Input)) +
                   'deskewed-' + ChangeFileExt(ExtractFileName(Input), '.png');

  ParseResult := FCmdOptions.Parse([Input]);
  AssertParseSuccesAndEmptyError(ParseResult, 'Just input');

  AssertEquals('Input File Name Set', Input, FCmdOptions.InputFileName);
  AssertEquals('Output File Name Derived', ExpectedOutput, FCmdOptions.OutputFileName);

  ParseResult := FCmdOptions.Parse(['"input with space.png"']);
  AssertParseSuccesAndEmptyError(ParseResult, 'Input with space');
  AssertEquals('"input with space.png"', FCmdOptions.InputFileName);

  // Check bad inputs
  ParseResult := FCmdOptions.Parse(['-o', 'out.png']);
  AssertParseFailAndErrorContains(ParseResult, 'No input file given');

  ParseResult := FCmdOptions.Parse(['in1.png', 'in2.png']);
  AssertParseFailAndErrorContains(ParseResult, 'Multiple input files specified (in2.png, in1.png)');
end;

procedure TTestCmdLineOptions.TestInputOutput;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-o', 'output.tif', 'input.jpg']);
  AssertParseSuccesAndEmptyError(ParseResult);

  AssertEquals('Input File Name Set', 'input.jpg', FCmdOptions.InputFileName);
  AssertEquals('Output File Name Set', 'output.tif', FCmdOptions.OutputFileName);

  ParseResult := FCmdOptions.Parse(['-o', '../dir/output.tif', '../other-dir/input.jpg']);
  AssertParseSuccesAndEmptyError(ParseResult, 'Folders');
  AssertEquals('../other-dir/input.jpg', FCmdOptions.InputFileName);
  AssertEquals('../dir/output.tif', FCmdOptions.OutputFileName);

  ParseResult := FCmdOptions.Parse(['-o', '"output with space.png"', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, 'Output with space');
  AssertEquals('"output with space.png"', FCmdOptions.OutputFileName);
end;

procedure TTestCmdLineOptions.TestMaxAngle;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-a', '5.7', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult);
  AssertEquals('MaxAngle Set', 5.7, FCmdOptions.MaxAngle);

  ParseResult := FCmdOptions.Parse(['-a', 'not_a_number', 'in.jpg']);
  AssertParseFailAndErrorContains(ParseResult, 'Invalid value for max angle parameter: not_a_number', 'Invalid value');
end;

procedure TTestCmdLineOptions.TestAngleStep;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-d', '0.01', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult);
  AssertEquals('AngleStep Set', 0.01, FCmdOptions.AngleStep);
end;

procedure TTestCmdLineOptions.TestSkipAngle;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-l', '0.5', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult);
  AssertEquals('SkipAngle Set', 0.5, FCmdOptions.SkipAngle);
end;

procedure TTestCmdLineOptions.TestDpiOverride;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-p', '300', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult);
  AssertEquals('DPI Override Set', 300, FCmdOptions.DpiOverride);
end;

procedure TTestCmdLineOptions.TestThreshold;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-t', '99', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, 'Threshold explicit');
  AssertTrue('Threshold Method Explicit', tmExplicit = FCmdOptions.ThresholdingMethod);
  AssertEquals('Threshold Level Set', 99, FCmdOptions.ThresholdLevel);

  ParseResult := FCmdOptions.Parse(['-t', 'a', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, 'Threshold auto');
  AssertTrue('Threshold Method Auto', tmOtsu = FCmdOptions.ThresholdingMethod);

  ParseResult := FCmdOptions.Parse(['-t', 'x', 'in.jpg']);
  AssertParseFailAndErrorContains(ParseResult, 'Invalid value for treshold parameter: x', 'Invalid value - char');

  ParseResult := FCmdOptions.Parse(['-t', '55.55', 'in.jpg']);
  AssertParseFailAndErrorContains(ParseResult, 'Invalid value for treshold parameter: 55.55', 'Invalid value - float');
end;

procedure TTestCmdLineOptions.TestResamplingFilter;
const
  Names: array[TResamplingFilter] of string = ('nearest', 'linear', 'cubic', 'lanczos');
var
  ParseResult: Boolean;
  Filter: TResamplingFilter;
begin
  for Filter := Low(TResamplingFilter) to High(TResamplingFilter) do
  begin
    ParseResult := FCmdOptions.Parse(['-q', Names[Filter], 'in.png']);
    AssertParseSuccesAndEmptyError(ParseResult, 'Filter parse: ' + Names[Filter]);
    AssertTrue('Filter value: ' + Names[Filter], Filter = FCmdOptions.ResamplingFilter);
  end;

  ParseResult := FCmdOptions.Parse(['-q', 'ujo', 'in.jpg']);
  AssertParseFailAndErrorContains(ParseResult, 'Invalid value for resampling filter parameter: ujo', 'Invalid filter');
end;

procedure TTestCmdLineOptions.TestOutputFormat;
const
  Names: array[1..4] of string = ('b1', 'g8', 'rgb24', 'rgba32');
  Formats: array[1..4] of TImageFormat = (ifBinary, ifGray8, ifR8G8B8, ifA8R8G8B8);
var
  ParseResult: Boolean;
  I: Integer;
begin
  for I := Low(Formats) to High(Formats) do
  begin
    ParseResult := FCmdOptions.Parse(['-f', Names[I], 'in.png']);
    AssertParseSuccesAndEmptyError(ParseResult, 'Format parse: ' + Names[I]);
    AssertTrue('Format value: ' + Names[I], Formats[I] = FCmdOptions.ForcedOutputFormat);
  end;

  ParseResult := FCmdOptions.Parse(['-f', 'g64', 'in.jpg']);
  AssertParseFailAndErrorContains(ParseResult, 'Invalid value for format parameter: g64', 'Invalid format');
end;

procedure TTestCmdLineOptions.TestOperationalFlags;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-g', 'cd', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, '2 oper flags');
  AssertTrue('CropToInput set', ofCropToInput in FCmdOptions.OperationalFlags);
  AssertTrue('DetectOnly set', ofDetectOnly in FCmdOptions.OperationalFlags);

  ParseResult := FCmdOptions.Parse(['-g', 'd', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, '1 oper flag');
  AssertTrue('CropToInput !set', not (ofCropToInput in FCmdOptions.OperationalFlags));
  AssertTrue('DetectOnly set', ofDetectOnly in FCmdOptions.OperationalFlags);
end;

procedure TTestCmdLineOptions.TestInfoFlags;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-s', 'sptw', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, '4 stats flags');
  AssertTrue('Info: detection', FCmdOptions.ShowDetectionStats);
  AssertTrue('Info: params', FCmdOptions.ShowParams);
  AssertTrue('Info: timings', FCmdOptions.ShowTimings);
  AssertTrue('Info: work-image', FCmdOptions.SaveWorkImage);

  ParseResult := FCmdOptions.Parse(['-s', 't', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, '1 stats flag');
  AssertFalse('Info: !detection', FCmdOptions.ShowDetectionStats);
  AssertFalse('Info: !params', FCmdOptions.ShowParams);
  AssertTrue('Info: timings only', FCmdOptions.ShowTimings);
  AssertFalse('Info: !work-image', FCmdOptions.SaveWorkImage);
end;

procedure TTestCmdLineOptions.TestContentRect;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-r', '10,20,190,80', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, '4 values');
  AssertEquals('ContentRect', FloatRect(10, 20, 190, 80), FCmdOptions.ContentRect);
  AssertTrue('Unit: pixels (Default)', suPixels = FCmdOptions.ContentSizeUnit);

  ParseResult := FCmdOptions.Parse(['-r', '10', 'in.png']);
  AssertParseFailAndErrorContains(ParseResult, 'Invalid definition of content rectangle', '1 value');
  ParseResult := FCmdOptions.Parse(['-r', '1,1,1,1,1', 'in.png']);
  AssertParseFailAndErrorContains(ParseResult, 'Invalid definition of content rectangle', '5 values, no unit');
  ParseResult := FCmdOptions.Parse(['-r', '1,1,1,1,nm', 'in.png']);
  AssertParseFailAndErrorContains(ParseResult, 'Invalid definition of content rectangle', '5 values, bad unit');
  ParseResult := FCmdOptions.Parse(['-r', '1,1,%', 'in.png']);
  AssertParseFailAndErrorContains(ParseResult, 'Invalid definition of content rectangle', '3 values, unit');

  ParseResult := FCmdOptions.Parse(['-r', '0.1,0.2,19.0,8.5,%', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, '4 float values + unit');
  AssertEquals('ContentRect float + unit', FloatRect(0.1, 0.2, 19.0, 8.5), FCmdOptions.ContentRect);
  AssertTrue('Unit: Pct', suPercent = FCmdOptions.ContentSizeUnit);

  AssertTrue('Parse: cm', FCmdOptions.Parse(['-r', '0.13,0,190,80,cm', 'in.png']));
  AssertTrue('Unit: cm', suCm = FCmdOptions.ContentSizeUnit);
  AssertTrue('Parse: mm', FCmdOptions.Parse(['-r', '0,0,190,80,mm', 'in.png']));
  AssertTrue('Unit: mm', suMm = FCmdOptions.ContentSizeUnit);
  AssertTrue('Parse: inch', FCmdOptions.Parse(['-r', '0,0,190,80.731,in', 'in.png']));
  AssertTrue('Unit: inch', suInch = FCmdOptions.ContentSizeUnit);
  AssertTrue('Parse: pixels', FCmdOptions.Parse(['-r', '0,0,190,80,px', 'in.png']));
  AssertTrue('Unit: pixels', suPixels = FCmdOptions.ContentSizeUnit);

  ParseResult := FCmdOptions.Parse(['-m', '10', '-r', '10,10,100,100', 'in.png']);
  AssertParseFailAndErrorContains(ParseResult, 'Cannot accept content rectangle when content margins are already defined', 'margins given');
end;

procedure TTestCmdLineOptions.TestContentMargins;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-m', '100,120,140,80', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, '4 values');
  AssertEquals('ContentMargins', FloatRect(100, 120, 140, 80), FCmdOptions.ContentMargins);
  AssertTrue('Unit: pixels (Default)', suPixels = FCmdOptions.ContentSizeUnit);

  ParseResult := FCmdOptions.Parse(['-m', '10,12,14,8,mm', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, '4 values + unit');
  AssertEquals('ContentMargins + unit', FloatRect(10, 12, 14, 8), FCmdOptions.ContentMargins);
  AssertTrue('Unit: mm', suMm = FCmdOptions.ContentSizeUnit);

  ParseResult := FCmdOptions.Parse(['-m', '100', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, '1 value');
  AssertEquals('ContentMargins 1 value', FloatRect(100, 100, 100, 100), FCmdOptions.ContentMargins);

  ParseResult := FCmdOptions.Parse(['-m', '15.5,%', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, '1 value + unit');
  AssertEquals('ContentMargins 1 value', FloatRect(15.5, 15.5, 15.5, 15.5), FCmdOptions.ContentMargins);
  AssertTrue('Unit: pct', suPercent = FCmdOptions.ContentSizeUnit);

  ParseResult := FCmdOptions.Parse(['-m', '100, 200', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, '2 values');
  AssertEquals('ContentMargins 2 values', FloatRect(100, 200, 100, 200), FCmdOptions.ContentMargins);

  ParseResult := FCmdOptions.Parse(['-m', '1,2,cm', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, '2 values + unit');
  AssertEquals('ContentMargins 2 values + unit', FloatRect(1, 2, 1, 2), FCmdOptions.ContentMargins);
  AssertTrue('Unit: cm', suCm = FCmdOptions.ContentSizeUnit);

  ParseResult := FCmdOptions.Parse(['-m', '1,1,1,1,1', 'in.png']);
  AssertParseFailAndErrorContains(ParseResult, 'Invalid definition of content margins', '5 values, no unit');
  ParseResult := FCmdOptions.Parse(['-m', '1,1,1', 'in.png']);
  AssertParseFailAndErrorContains(ParseResult, 'Invalid definition of content margins', '3 values, no unit');
  ParseResult := FCmdOptions.Parse(['-m', '1,1,xibalba', 'in.png']);
  AssertParseFailAndErrorContains(ParseResult, 'Invalid definition of content margins', '3 values, bad unit');
  ParseResult := FCmdOptions.Parse(['-m', 'mm,1,1', 'in.png']);
  AssertParseFailAndErrorContains(ParseResult, 'Invalid definition of content margins', 'unit first');
  ParseResult := FCmdOptions.Parse(['-m', '1,1,1,1,1,in', 'in.png']);
  AssertParseFailAndErrorContains(ParseResult, 'Invalid definition of content margins', '6 values');

  ParseResult := FCmdOptions.Parse(['-r', '10,10,100,100', '-m', '10', 'in.png']);
  AssertParseFailAndErrorContains(ParseResult, 'Cannot accept content margins when content rectangle is already defined', 'content rect given');
end;

procedure TTestCmdLineOptions.TestCalcDetectionRect;
var
  FinalRect, ImageBounds: TRect;
  Metadata: TMetadata;
begin
  Metadata := TMetadata.Create;
  ImageBounds := Rect(0, 0, 500, 1000);

  AssertTrue(FCmdOptions.Parse(['in.png']));
  AssertTrue(FCmdOptions.CalcContentRectForImage(ImageBounds, Metadata, FinalRect));
  AssertEquals('No content reduction', ImageBounds, FinalRect);

  // Page Margins
  AssertTrue(FCmdOptions.Parse(['-m', '100,120,140,80', 'in.png']));
  AssertTrue(FCmdOptions.CalcContentRectForImage(ImageBounds, Metadata, FinalRect));
  AssertEquals('M 4x px', Rect(100, 120, 360, 920), FinalRect);

  AssertTrue(FCmdOptions.Parse(['-m', '100', 'in.png']));
  AssertTrue(FCmdOptions.CalcContentRectForImage(ImageBounds, Metadata, FinalRect));
  AssertEquals('M 1x px', Rect(100, 100, 400, 900), FinalRect);

  AssertTrue(FCmdOptions.Parse(['-m', '10,%', 'in.png']));
  AssertTrue(FCmdOptions.CalcContentRectForImage(ImageBounds, Metadata, FinalRect));
  AssertEquals('M 1x %', Rect(50, 100, 450, 900), FinalRect);

  AssertTrue(FCmdOptions.Parse(['-m', '1,20,%', 'in.png']));
  AssertTrue(FCmdOptions.CalcContentRectForImage(ImageBounds, Metadata, FinalRect));
  AssertEquals('M 2x %', Rect(5, 200, 495, 800), FinalRect);

  // Page Rect
  AssertTrue(FCmdOptions.Parse(['-r', '100,120,440,800', 'in.png']));
  AssertTrue(FCmdOptions.CalcContentRectForImage(ImageBounds, Metadata, FinalRect));
  AssertEquals('R 4x px', Rect(100, 120, 440, 800), FinalRect);

  AssertTrue(FCmdOptions.Parse(['-r', '10,20,90,80,%', 'in.png']));
  AssertTrue(FCmdOptions.CalcContentRectForImage(ImageBounds, Metadata, FinalRect));
  AssertEquals('R 4x %', Rect(50, 200, 450, 800), FinalRect);

  // Failures
  AssertTrue(FCmdOptions.Parse(['-m', '1,in', 'in.png']));
  AssertFalse('Missing DPI', FCmdOptions.CalcContentRectForImage(ImageBounds, Metadata, FinalRect));

  AssertTrue(FCmdOptions.Parse(['-r', '-10,-20,-90,-80', 'in.png']));
  AssertFalse('Rect off the image', FCmdOptions.CalcContentRectForImage(ImageBounds, Metadata, FinalRect));

  AssertTrue(FCmdOptions.Parse(['-m', '50.1,%', 'in.png']));
  AssertFalse('Margins too big', FCmdOptions.CalcContentRectForImage(ImageBounds, Metadata, FinalRect));

  // With physical sizes
  Metadata.SetPhysicalPixelSize(TResolutionUnit.ruDpi, 50, 100);  // Img is 10x10 inches
  AssertTrue(FCmdOptions.Parse(['-m', '1,in', 'in.png']));
  AssertTrue(FCmdOptions.CalcContentRectForImage(ImageBounds, Metadata, FinalRect));
  AssertEquals('M 1x in', Rect(50, 100, 450, 900), FinalRect);

  Metadata.SetPhysicalPixelSize(TResolutionUnit.ruDpcm, 50, 100);  // Img is 10x10 cm
  AssertTrue(FCmdOptions.Parse(['-m', '1,cm', 'in.png']));
  AssertTrue(FCmdOptions.CalcContentRectForImage(ImageBounds, Metadata, FinalRect));
  AssertEquals('M 1x cm', Rect(50, 100, 450, 900), FinalRect);

  AssertTrue(FCmdOptions.Parse(['-m', '1,mm', 'in.png']));
  AssertTrue(FCmdOptions.CalcContentRectForImage(ImageBounds, Metadata, FinalRect));
  AssertEquals('M 1x mm', Rect(5, 10, 495, 990), FinalRect);

  Metadata.SetPhysicalPixelSize(TResolutionUnit.ruSizeInMicroMeters, 200, 100); // Img is 10x10 cm
  AssertTrue(FCmdOptions.Parse(['-m', '10,mm', 'in.png']));
  AssertTrue(FCmdOptions.CalcContentRectForImage(ImageBounds, Metadata, FinalRect));
  AssertEquals('M 1x mm', Rect(50, 100, 450, 900), FinalRect);

  Metadata.Free;
end;

initialization

  RegisterTest(TTestCmdLineOptions{$IFNDEF FPC}.Suite{$ENDIF});
end.

