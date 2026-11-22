{
  Tests of the GUI settings (Gui/options.pas): command line parameters they are
  translated to, margins validation and the INI file.
}
unit TestGuiOptions;

interface

uses
  Classes, SysUtils, IniFiles, fpcunit, testregistry,
  Options;

type
  TTestGuiOptions = class(TTestCase)
  private
    FOptions: TOptions;
    // Parameters without "-o <output>" at the start and the input file at the end
    function ParamsOf(Opts: TOptions): string;
    // Value of -o
    function OutputOf(Opts: TOptions): string;
    function NewOptions: TOptions;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaults;
    procedure TestAngles;
    procedure TestOperationalFlags;
    procedure TestOutputPath;
    procedure TestContentArea;
    procedure TestValidMargins;
    procedure TestOtherOptions;
    procedure TestExtraParameters;
    procedure TestIniRoundTrip;
    procedure TestIniBadValues;
    procedure TestIniOldSection;
  end;

implementation

function TTestGuiOptions.ParamsOf(Opts: TOptions): string;
var
  Params: TStringList;
  I: Integer;
begin
  Result := '';
  Params := TStringList.Create;
  try
    Opts.ToCmdLineParameters(Params, 0);
    AssertEquals('-o first', '-o', Params[0]);
    AssertEquals('input last', Opts.Files[0], Params[Params.Count - 1]);
    for I := 2 to Params.Count - 2 do
    begin
      if Result <> '' then
        Result := Result + ' ';
      Result := Result + Params[I];
    end;
  finally
    Params.Free;
  end;
end;

function TTestGuiOptions.OutputOf(Opts: TOptions): string;
var
  Params: TStringList;
begin
  Params := TStringList.Create;
  try
    Opts.ToCmdLineParameters(Params, 0);
    Result := Params[1];
  finally
    Params.Free;
  end;
end;

function TTestGuiOptions.NewOptions: TOptions;
begin
  Result := TOptions.Create;
  Result.Files.Add('in.png');
end;

procedure TTestGuiOptions.SetUp;
begin
  FOptions := NewOptions;
end;

procedure TTestGuiOptions.TearDown;
begin
  FOptions.Free;
end;

procedure TTestGuiOptions.TestDefaults;
begin
  // Only the white background is passed (default of the CLI is black),
  // the other GUI defaults must be the defaults of the CLI
  AssertEquals('defaults', '-b FFFFFFFF', ParamsOf(FOptions));
  FOptions.BackgroundColor := $FF000000;
  AssertEquals('black background', '', ParamsOf(FOptions));

  FOptions.PrintParams := True;
  FOptions.Reset;
  AssertFalse('Print parameters off after reset', FOptions.PrintParams);
  AssertEquals('defaults after reset', '-b FFFFFFFF', ParamsOf(FOptions));
end;

procedure TTestGuiOptions.TestAngles;
begin
  FOptions.BackgroundColor := $FF000000;
  // Values next to the defaults must be passed too
  FOptions.SkipAngle := 0;
  AssertEquals('skip angle 0', '-l 0.00', ParamsOf(FOptions));
  FOptions.SkipAngle := 0.02;
  AssertEquals('skip angle 0.02', '-l 0.02', ParamsOf(FOptions));
  FOptions.SkipAngle := 0.01;
  AssertEquals('skip angle default', '', ParamsOf(FOptions));
  FOptions.MaxAngle := 9.9;
  AssertEquals('max angle 9.9', '-a 9.90', ParamsOf(FOptions));
  FOptions.MaxAngle := 10.1;
  AssertEquals('max angle 10.1', '-a 10.10', ParamsOf(FOptions));
  FOptions.MaxAngle := 10;
  FOptions.AngleStep := 0.05;
  AssertEquals('angle step', '-d 0.05', ParamsOf(FOptions));
end;

procedure TTestGuiOptions.TestOperationalFlags;
begin
  FOptions.BackgroundColor := $FF000000;
  FOptions.CropToInput := True;
  FOptions.DetectOnly := True;
  FOptions.ForceRotation := True;
  AssertEquals('all flags', '-g cdf', ParamsOf(FOptions));
  FOptions.CropToInput := False;
  FOptions.DetectOnly := False;
  AssertEquals('force only', '-g f', ParamsOf(FOptions));
end;

procedure TTestGuiOptions.TestOutputPath;
var
  Scans, Reports: string;
begin
  // Output: file name, or just the folder in detect only mode
  Scans := 'C:' + PathDelim + 'scans' + PathDelim;
  Reports := 'D:' + PathDelim + 'reports';
  FOptions.Files[0] := Scans + 'in.png';
  AssertEquals('output file', Scans + 'deskewed-in.png', OutputOf(FOptions));

  FOptions.DefaultOutputFileOptions := False;
  FOptions.OutputFolder := Reports;
  AssertEquals('output folder', Reports + PathDelim + 'in.png', OutputOf(FOptions));
  FOptions.OutputFileFormat := ffTiff;
  AssertEquals('output folder and format', Reports + PathDelim + 'in.tif', OutputOf(FOptions));
  FOptions.OutputFolder := '';
  AssertEquals('format, input folder', Scans + 'deskewed-in.tif', OutputOf(FOptions));
  FOptions.OutputFileFormat := ffSameAsInput;

  FOptions.DetectOnly := True;
  AssertEquals('detect only: empty output folder', Scans, OutputOf(FOptions));
  FOptions.OutputFolder := Reports;
  AssertEquals('detect only: output folder', Reports + PathDelim, OutputOf(FOptions));
  FOptions.DefaultOutputFileOptions := True;
  AssertEquals('detect only: input folder', Scans, OutputOf(FOptions));
  FOptions.Files[0] := 'in.png';
  AssertEquals('detect only: no folder in input', '.' + PathDelim, OutputOf(FOptions));
end;

procedure TTestGuiOptions.TestContentArea;
begin
  FOptions.BackgroundColor := $FF000000;
  FOptions.AutoContentRect := True;
  AssertEquals('auto content', '-r auto', ParamsOf(FOptions));
  FOptions.ContentMargins := '5,%';
  AssertEquals('auto content wins over margins', '-r auto', ParamsOf(FOptions));
  FOptions.AutoContentRect := False;
  AssertEquals('margins with unit', '-m 5,%', ParamsOf(FOptions));
  FOptions.ContentMargins := ' 10, 20 , mm ';
  AssertEquals('margins with spaces', '-m 10,20,mm', ParamsOf(FOptions));
  FOptions.ContentMargins := '';
  AssertEquals('no margins', '', ParamsOf(FOptions));
end;

procedure TTestGuiOptions.TestValidMargins;

  procedure Check(const Margins: string; Expected: Boolean);
  begin
    AssertEquals('"' + Margins + '"', Expected, TOptions.IsValidMargins(Margins));
  end;

begin
  Check('', True);
  Check('5', True);
  Check('5,%', True);
  Check('1.5, 2.5', True);
  Check('10,20,MM', True);
  Check('1,2,3,4', True);
  Check('1,2,3,4,in', True);
  Check('0', True);
  Check('1,2,3', False);
  Check('1,2,3,4,5', False);
  Check('mm', False);
  Check('5,km', False);
  Check('a,b', False);
  Check('-5', False);
  Check('1;2', False);
  Check('1,,2', False);
  Check('1,5 mm', False);
end;

procedure TTestGuiOptions.TestOtherOptions;
begin
  FOptions.BackgroundColor := $FF000000;
  FOptions.DpiOverrideEnabled := True;
  FOptions.DpiOverride := 600;
  FOptions.ThresholdingAuto := False;
  FOptions.ThresholdLevel := 99;
  FOptions.ResamplingFilter := rfLanczos;
  FOptions.ForcedOutputFormat := fofGray8;
  FOptions.OutputFileParamsEnabled := [ffJpeg, ffTiff];
  FOptions.JpegCompressionQuality := 75;
  FOptions.TiffCompressionScheme := TiffCompressionOptionAsInputLossless;
  FOptions.PrintParams := True;
  AssertEquals('many', '-p 600 -f g8 -q lanczos -c j75,tinput-lossless -t 99 -s p', ParamsOf(FOptions));

  // Work image goes with the other info flags in one -s
  FOptions.SaveWorkImage := True;
  AssertEquals('work image + params', '-p 600 -f g8 -q lanczos -c j75,tinput-lossless -t 99 -s pw', ParamsOf(FOptions));
  FOptions.PrintParams := False;
  AssertEquals('work image', '-p 600 -f g8 -q lanczos -c j75,tinput-lossless -t 99 -s w', ParamsOf(FOptions));

  FOptions.OutputFileParamsEnabled := [ffTiff];
  FOptions.TiffCompressionScheme := TiffCompressionOptionGroup4;
  AssertEquals('TIFF only', '-p 600 -f g8 -q lanczos -c tg4 -t 99 -s w', ParamsOf(FOptions));
  FOptions.OutputFileParamsEnabled := [ffJpeg];
  AssertEquals('JPEG only', '-p 600 -f g8 -q lanczos -c j75 -t 99 -s w', ParamsOf(FOptions));
end;

procedure TTestGuiOptions.TestExtraParameters;
var
  Params: TStringList;
begin
  FOptions.BackgroundColor := $FF000000;
  FOptions.ExtraCmdLineArgs := '-r 1,2,3,4  -s t';
  AssertEquals('repeated spaces', '-r 1,2,3,4 -s t', ParamsOf(FOptions));

  // Quoted value with spaces is one parameter
  FOptions.ExtraCmdLineArgs := '-x "a b"  c';
  Params := TStringList.Create;
  try
    FOptions.ToCmdLineParameters(Params, 0);
    AssertEquals('count', 6, Params.Count);
    AssertEquals('-x', '-x', Params[2]);
    AssertEquals('quoted', 'a b', Params[3]);
    AssertEquals('after quoted', 'c', Params[4]);
    AssertEquals('input last', 'in.png', Params[5]);
  finally
    Params.Free;
  end;
end;

procedure TTestGuiOptions.TestIniRoundTrip;
var
  Loaded: TOptions;
  Ini: TIniFile;
  IniPath: string;
begin
  IniPath := GetTempDir + 'deskew-guitests-roundtrip.ini';
  SysUtils.DeleteFile(IniPath);
  FOptions.AutoContentRect := True;
  FOptions.ContentMargins := '5,10,cm';
  FOptions.ForceRotation := True;
  FOptions.SaveWorkImage := True;
  FOptions.MaxAngle := 7.5;
  FOptions.BackgroundColor := $80FF0000;
  FOptions.ResamplingFilter := rfCubic;
  FOptions.ForcedOutputFormat := fofRgba32;
  FOptions.OutputFileFormat := ffTiff;
  FOptions.OutputFileParamsEnabled := [ffTiff];
  FOptions.TiffCompressionScheme := TiffCompressionOptionAsInput;
  FOptions.ExtraCmdLineArgs := '-s t';

  Ini := TIniFile.Create(IniPath, [ifoFormatSettingsActive]);
  Loaded := NewOptions;
  try
    FOptions.SaveToIni(Ini);
    Ini.UpdateFile;
    Loaded.LoadFromIni(Ini);
    AssertTrue('auto content', Loaded.AutoContentRect);
    AssertEquals('margins', '5,10,cm', Loaded.ContentMargins);
    AssertTrue('force rotation', Loaded.ForceRotation);
    AssertTrue('save work image', Loaded.SaveWorkImage);
    AssertEquals('max angle', 7.5, Loaded.MaxAngle, 0.0001);
    AssertEquals('background', IntToHex(FOptions.BackgroundColor, 8), IntToHex(Loaded.BackgroundColor, 8));
    AssertTrue('filter', Loaded.ResamplingFilter = rfCubic);
    AssertTrue('forced format', Loaded.ForcedOutputFormat = fofRgba32);
    AssertTrue('file format', Loaded.OutputFileFormat = ffTiff);
    AssertTrue('file params', Loaded.OutputFileParamsEnabled = [ffTiff]);
    AssertEquals('TIFF scheme', TiffCompressionOptionAsInput, Loaded.TiffCompressionScheme);
    AssertEquals('extra parameters', '-s t', Loaded.ExtraCmdLineArgs);
    // Same parameters for the CLI
    AssertEquals('parameters', ParamsOf(FOptions), ParamsOf(Loaded));
  finally
    Loaded.Free;
    Ini.Free;
    SysUtils.DeleteFile(IniPath);
  end;
end;

procedure TTestGuiOptions.TestIniBadValues;
var
  Ini: TIniFile;
  IniPath: string;
begin
  IniPath := GetTempDir + 'deskew-guitests-bad.ini';
  SysUtils.DeleteFile(IniPath);
  Ini := TIniFile.Create(IniPath, [ifoFormatSettingsActive]);
  try
    Ini.WriteString('Options', 'ContentMargins', 'nonsense');
    Ini.WriteInteger('Options', 'TiffCompressionScheme', 42);
    Ini.WriteString('Options', 'ResamplingFilter', 'Bogus');
    Ini.WriteString('Options', 'BackgroundColor', '#red');
    FOptions.LoadFromIni(Ini);
    AssertEquals('bad margins dropped', '', FOptions.ContentMargins);
    AssertEquals('bad TIFF scheme replaced', TiffCompressionOptionLzw, FOptions.TiffCompressionScheme);
    AssertTrue('bad filter replaced', FOptions.ResamplingFilter = rfDefaultLinear);
    AssertEquals('bad background replaced', IntToHex($FFFFFFFF, 8), IntToHex(FOptions.BackgroundColor, 8));
  finally
    Ini.Free;
    SysUtils.DeleteFile(IniPath);
  end;
end;

procedure TTestGuiOptions.TestIniOldSection;
var
  Ini: TIniFile;
  IniPath: string;
begin
  // Settings of older versions in [Advanced] are moved to [Options],
  // values already in [Options] win
  IniPath := GetTempDir + 'deskew-guitests-old.ini';
  SysUtils.DeleteFile(IniPath);
  Ini := TIniFile.Create(IniPath, [ifoFormatSettingsActive]);
  try
    Ini.WriteFloat('Options', 'MaxAngle', 7.5);
    Ini.WriteFloat('Advanced', 'MaxAngle', 20);
    Ini.WriteFloat('Advanced', 'SkipAngle', 0.5);
    Ini.WriteString('Advanced', 'ContentMargins', '1,2,cm');
    FOptions.LoadFromIni(Ini);
    AssertEquals('[Options] wins over old section', 7.5, FOptions.MaxAngle, 0.0001);
    AssertEquals('skip angle from old section', 0.5, FOptions.SkipAngle, 0.0001);
    AssertEquals('margins from old section', '1,2,cm', FOptions.ContentMargins);
    AssertFalse('old section removed', Ini.SectionExists('Advanced'));
    FOptions.SaveToIni(Ini);
    AssertFalse('still no old section after save', Ini.SectionExists('Advanced'));
  finally
    Ini.Free;
    SysUtils.DeleteFile(IniPath);
  end;
end;

initialization
  RegisterTest(TTestGuiOptions);
end.
