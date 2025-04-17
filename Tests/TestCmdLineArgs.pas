unit TestCmdLineArgs;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  CmdLineOptions;

type

  TTestCmdLineOptions= class(TTestCase)
  private
    FCmdOptions: TCmdLineOptions;
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
  end;

implementation

uses
  ImageUtils, ImagingTypes;

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
  AssertEquals('Default MaxAngle', DefaultMaxAngle, FCmdOptions.MaxAngle, 0.001);
  AssertEquals('Default SkipAngle', DefaultSkipAngle, FCmdOptions.SkipAngle, 0.001);
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

  AssertTrue('Parse Success', ParseResult);
  AssertEquals('Input File Name Set', Input, FCmdOptions.InputFileName);
  AssertEquals('Output File Name Derived', ExpectedOutput, FCmdOptions.OutputFileName);
  AssertTrue('IsValid with Input', FCmdOptions.IsValid);
  AssertEquals('Error Message Empty', '', FCmdOptions.ErrorMessage);
end;

procedure TTestCmdLineOptions.TestInputOutput;
var
  ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-o', 'output.tif', 'input.jpg']);

  AssertTrue('Parse Success', ParseResult);
  AssertEquals('Input File Name Set', 'input.jpg', FCmdOptions.InputFileName);
  AssertEquals('Output File Name Set', 'output.tif', FCmdOptions.OutputFileName);
  AssertTrue('IsValid with Input/Output', FCmdOptions.IsValid);
  AssertEquals('Error Message Empty', '', FCmdOptions.ErrorMessage);

  ParseResult := FCmdOptions.Parse(['-o', '../dir/output.tif', '../other-dir/input.jpg']);
  AssertTrue(ParseResult);
  AssertEquals('../other-dir/input.jpg', FCmdOptions.InputFileName);
  AssertEquals('../dir/output.tif', FCmdOptions.OutputFileName);

  ParseResult := FCmdOptions.Parse(['-o', '"output with space.png"', 'in.png']);
  AssertTrue(ParseResult);
  AssertEquals('"output with space.png"', FCmdOptions.OutputFileName);
end;

procedure TTestCmdLineOptions.TestMaxAngle;
var ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-a', '5.7', 'in.png']);
  AssertTrue('Parse Success', ParseResult);
  AssertEquals('MaxAngle Set', 5.7, FCmdOptions.MaxAngle, 0.001);
  AssertTrue('IsValid with MaxAngle', FCmdOptions.IsValid);
  AssertEquals('Error Message Empty', '', FCmdOptions.ErrorMessage);
end;

procedure TTestCmdLineOptions.TestThresholdExplicit;
var ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-t', '99', 'in.png']);
  AssertTrue('Parse Success', ParseResult);
  AssertTrue('Threshold Method Explicit', tmExplicit = FCmdOptions.ThresholdingMethod);
  AssertEquals('Threshold Level Set', 99, FCmdOptions.ThresholdLevel);
  AssertTrue('IsValid with Explicit Threshold', FCmdOptions.IsValid);
  AssertEquals('Error Message Empty', '', FCmdOptions.ErrorMessage);
end;

procedure TTestCmdLineOptions.TestThresholdAuto;
var ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-t', 'a', 'in.png']);
  AssertTrue('Parse Success', ParseResult);
  AssertTrue('Threshold Method Auto', tmOtsu = FCmdOptions.ThresholdingMethod);
  AssertEquals('Threshold Level Default with Otsu', DefaultThreshold, FCmdOptions.ThresholdLevel);
  AssertTrue('IsValid with Auto Threshold', FCmdOptions.IsValid);
  AssertEquals('Error Message Empty', '', FCmdOptions.ErrorMessage);
end;

procedure TTestCmdLineOptions.TestSkipAngle;
var ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-l', '0.5', 'in.png']);
  AssertTrue('Parse Success', ParseResult);
  AssertEquals('SkipAngle Set', 0.5, FCmdOptions.SkipAngle, 0.001);
  AssertTrue('IsValid with SkipAngle', FCmdOptions.IsValid);
  AssertEquals('Error Message Empty', '', FCmdOptions.ErrorMessage);
end;

procedure TTestCmdLineOptions.TestDpiOverride;
var ParseResult: Boolean;
begin
  ParseResult := FCmdOptions.Parse(['-p', '300', 'in.png']);
  AssertTrue('Parse Success', ParseResult);
  AssertEquals('DPI Override Set', 300, FCmdOptions.DpiOverride);
  AssertTrue('IsValid with DPI Override', FCmdOptions.IsValid);
  AssertEquals('Error Message Empty', '', FCmdOptions.ErrorMessage);
end;

initialization

  RegisterTest(TTestCmdLineOptions);
end.

