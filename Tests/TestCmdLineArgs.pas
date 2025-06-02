unit TestCmdLineArgs;

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFramework,
{$ENDIF}
  ImagingUtility,
  Utils,
  CmdLineOptions;

type
  TTestCmdLineOptions= class(TTestCase)
  private
    FCmdOptions: TCmdLineOptions;

{$IFNDEF FPC}
    procedure AssertTrue(const Msg: string; const ACondition: Boolean;
      AddrOfError: Pointer = nil); overload;
    procedure AssertTrue(const ACondition: Boolean; AddrOfError: Pointer = nil); overload;
    procedure AssertFalse(const Msg: string; const ACondition: Boolean;
      AddrOfError: Pointer = nil); overload;

    procedure AssertEquals(const AMessage: string; Expected, Actual: string); overload;
{$ENDIF}

    procedure AssertEquals(const AMessage: string; Expected, Actual: Double); overload;
    procedure AssertEquals(const AMessage: string; const Expected, Actual: TFloatRect); overload;
    procedure AssertParseSuccesAndEmptyError(const ParseResult: Boolean; const AMessage: string = '');
    procedure AssertParseFailAndErrorContains(const ParseResult: Boolean; const ErrorMsgPart: string;
      const AMessage: string = '');

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Test Default Values
    procedure TestDefaults;

    // Test Basic Argument Parsing
    procedure TestInputOnly;
    procedure TestInputOutput;

    // Test Numerical Options
    procedure TestMaxAngle;
    procedure TestThresholdExplicit;
    procedure TestThresholdAuto;
    procedure TestSkipAngle;
    procedure TestDpiOverride;

    // Test Flags
    procedure TestInfoFlags;

    // Test methods for content rect
    procedure TestContentRect;
    procedure TestContentMargins;
  end;

implementation

uses
  Math, StrUtils, ImageUtils, ImagingTypes;

const
  Epsilon = 0.001;

{$IFNDEF FPC}
procedure TTestCmdLineOptions.AssertTrue(const Msg: string; const ACondition: Boolean;
  AddrOfError: Pointer);
begin
  CheckTrue(ACondition, Msg);
end;

procedure TTestCmdLineOptions.AssertTrue(const ACondition: Boolean;
  AddrOfError: Pointer);
begin
  AssertTrue('', ACondition, AddrOfError);
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
{$ENDIF}

procedure TTestCmdLineOptions.AssertEquals(const AMessage: string; Expected, Actual: Double);
begin
  CheckEquals(Expected, Actual, Epsilon, AMessage);
end;

procedure TTestCmdLineOptions.AssertEquals(const AMessage: string; const Expected, Actual: TFloatRect);
const
  SCompare = '"%s" expected: <%g,%g,%g,%g> but was: <%g,%g,%g,%g>';
var
  Msg: string;

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
  AssertParseSuccesAndEmptyError(ParseResult);

  AssertEquals('Input File Name Set', Input, FCmdOptions.InputFileName);
  AssertEquals('Output File Name Derived', ExpectedOutput, FCmdOptions.OutputFileName);

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
  CheckEquals('../other-dir/input.jpg', FCmdOptions.InputFileName);
  CheckEquals('../dir/output.tif', FCmdOptions.OutputFileName);

  ParseResult := FCmdOptions.Parse(['-o', '"output with space.png"', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult, 'Output with space');
  CheckEquals('"output with space.png"', FCmdOptions.OutputFileName);
end;

procedure TTestCmdLineOptions.TestMaxAngle;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-a', '5.7', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult);
  AssertEquals('MaxAngle Set', 5.7, FCmdOptions.MaxAngle);
end;

procedure TTestCmdLineOptions.TestThresholdExplicit;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-t', '99', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult);
  AssertTrue('Threshold Method Explicit', tmExplicit = FCmdOptions.ThresholdingMethod);
  AssertEquals('Threshold Level Set', 99, FCmdOptions.ThresholdLevel);
end;

procedure TTestCmdLineOptions.TestThresholdAuto;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-t', 'a', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult);
  AssertTrue('Threshold Method Auto', tmOtsu = FCmdOptions.ThresholdingMethod);
  AssertEquals('Threshold Level Default with Otsu', DefaultThreshold, FCmdOptions.ThresholdLevel);
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

initialization

  RegisterTest(TTestCmdLineOptions{$IFNDEF FPC}.Suite{$ENDIF});
end.

