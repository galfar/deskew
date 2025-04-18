unit TestCmdLineArgs;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  ImagingUtility,
  CmdLineOptions;

type

  TTestCmdLineOptions= class(TTestCase)
  private
    FCmdOptions: TCmdLineOptions;

    class procedure AssertEquals(const AMessage: string; Expected, Actual: Double); overload;
    class procedure AssertEquals(const AMessage: string; const Expected, Actual: TFloatRect); overload;
    procedure AssertParseSuccesAndEmptyError(const ParseResult: Boolean; const AMessage: string = '');
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

    // Test methods for content rect
    procedure TestContentRect_LTRB;
  end;

implementation

uses
  Math, ImageUtils, ImagingTypes;

const
  Epsilon = 0.001;

class procedure TTestCmdLineOptions.AssertEquals(const AMessage: string; Expected, Actual: Double);
begin
  AssertTrue(ComparisonMsg(AMessage, FloatToStr(Expected), FloatToStr(Actual)),
    SameValue(Expected, Actual, Epsilon), CallerAddr);
end;

class procedure TTestCmdLineOptions.AssertEquals(const AMessage: string; const Expected, Actual: TFloatRect);
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
  AssertTrue(Prefix + 'Parse Success', ParseResult, CallerAddr);
  AssertTrue(Prefix + 'IsValid', FCmdOptions.IsValid, CallerAddr);
  AssertTrue(Prefix + 'Error Message Empty', '' = FCmdOptions.ErrorMessage, CallerAddr);
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
  AssertFalse('Default ShowStats', FCmdOptions.ShowStats);
  AssertFalse('Default ShowParams', FCmdOptions.ShowParams);
  AssertFalse('Default ShowTimings', FCmdOptions.ShowTimings);
  AssertTrue('Default OperationalFlags empty', FCmdOptions.OperationalFlags = []);
  AssertEquals('Default Input File Empty', '', FCmdOptions.InputFileName);
  AssertEquals('Default Output File Empty', '', FCmdOptions.OutputFileName);
  AssertEquals('Default Error Message Empty', '', FCmdOptions.ErrorMessage);
  AssertFalse('Default IsValid (no input)', FCmdOptions.IsValid);
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

procedure TTestCmdLineOptions.TestContentRect_LTRB;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-r', '10,20,190,80', 'in.png']);
  AssertParseSuccesAndEmptyError(ParseResult);

  AssertEquals('ContentRect', FloatRect(10, 20, 190, 80), FCmdOptions.ContentRect);
  AssertTrue('ContentRect Unit Pixels (Default)', suPixels = FCmdOptions.ContentSizeUnit);
end;

initialization

  RegisterTest(TTestCmdLineOptions);
end.

