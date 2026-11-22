{
  GUI settings against the real command line tool: the CLI is run with the
  parameters the GUI generates and the parameters it reports back in its JSON
  report (-s j) are compared with the GUI settings. Fails when the CLI renames
  or changes an option the GUI uses, or when a default of the CLI is no longer
  the default the GUI has (the GUI does not pass default values).

  Needs the command line tool built next to the test program (Bin/) and
  TestImages/ in the repository.
}
unit TestGuiCli;

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser, process,
  Options;

type
  TTestGuiCli = class(TTestCase)
  private
    FTempDir: string;
    FInputFile: string;
    function NewOptions: TOptions;
    // Runs the CLI with the parameters of the GUI and checks the reported parameters
    procedure RunAndCheck(const Name: string; Opts: TOptions);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaults;
    procedure TestDetectionOptions;
    procedure TestContentAuto;
    procedure TestMarginsAndDpi;
    procedure TestOutputFormats;
    procedure TestFileFormats;
    procedure TestTiffCompressions;
    procedure TestExtraParameters;
  end;

implementation

uses
  FileUtil, TestGuiUtils;

const
  // Names of the CLI (TCmdLineOptions.WriteToJson) for the GUI settings
  OutputFormatNames: array[TForcedOutputFormat] of string = ('', 'Binary', 'Gray8', 'R8G8B8', 'A8R8G8B8');
  FilterNames: array[TResamplingFilter] of string = ('linear', 'nearest', 'cubic', 'lanczos');
  TiffNames: array[TiffCompressionOptionNone..TiffCompressionOptionAsInputLossless] of string = (
    'none', 'lzw', 'rle', 'deflate', 'jpeg', 'g4', 'input', 'input-lossless');

procedure TTestGuiCli.SetUp;
begin
  AssertTrue('Command line tool not built: ' + DeskewExeNextToTests, FileExists(DeskewExeNextToTests));
  FInputFile := ExpandFileName(ExtractFilePath(DeskewExeNextToTests) + '..' + PathDelim +
    'TestImages' + PathDelim + '2.png');
  AssertTrue('Test image not found: ' + FInputFile, FileExists(FInputFile));

  FTempDir := IncludeTrailingPathDelimiter(GetTempDir) + 'deskew-guitests-' + IntToStr(Random(1000000));
  ForceDirectories(FTempDir);
end;

procedure TTestGuiCli.TearDown;
begin
  if FTempDir <> '' then
    DeleteDirectory(FTempDir, False);
end;

function TTestGuiCli.NewOptions: TOptions;
begin
  Result := TOptions.Create;
  Result.Files.Add(FInputFile);
  // Output into the temp folder, not next to the test image
  Result.DefaultOutputFileOptions := False;
  Result.OutputFolder := FTempDir;
end;

procedure TTestGuiCli.RunAndCheck(const Name: string; Opts: TOptions);
var
  Params: TStringList;
  Args: array of TProcessString;
  Output: string;
  ExitStatus, I: Integer;
  Report: TJSONData;
  Root, Pars, OutputObj: TJSONObject;
  ThresholdLevel, ErrorData: TJSONData;
  ExpectedArea: string;
begin
  Params := TStringList.Create;
  try
    Opts.ToCmdLineParameters(Params, 0);
    // JSON report to standard output, before the input file
    Params.Insert(Params.Count - 1, '-s');
    Params.Insert(Params.Count - 1, 'j');
    SetLength(Args, Params.Count);
    for I := 0 to Params.Count - 1 do
      Args[I] := Params[I];

    RunCommandIndir(FTempDir, DeskewExeNextToTests, Args, Output, ExitStatus, [poNoConsole]);

    Report := nil;
    try
      try
        Report := GetJSON(Output);
      except
        on E: Exception do
          Fail(Name + ': report is not JSON (' + E.Message + '): ' + Copy(Output, 1, 300) +
            ' parameters: ' + Params.DelimitedText);
      end;
      AssertTrue(Name + ': report is not an object', Report is TJSONObject);
      Root := TJSONObject(Report);

      ErrorData := Root.Find('error');
      if (ErrorData <> nil) and not (ErrorData is TJSONNull) then
        Fail(Name + ': error: ' + ErrorData.AsString + ', parameters: ' + Params.DelimitedText);
      AssertEquals(Name + ': exit code', 0, ExitStatus);

      Pars := Root.Objects['parameters'];
      AssertEquals(Name + ': maxAngle', Opts.MaxAngle, Pars.Floats['maxAngle'], 0.001);
      AssertEquals(Name + ': angleStep', Opts.AngleStep, Pars.Floats['angleStep'], 0.001);
      AssertEquals(Name + ': skipAngle', Opts.SkipAngle, Pars.Floats['skipAngle'], 0.001);

      ThresholdLevel := Pars.Find('thresholdLevel');
      if Opts.ThresholdingAuto then
      begin
        AssertEquals(Name + ': thresholding', 'auto', Pars.Strings['thresholding']);
        AssertTrue(Name + ': thresholdLevel', ThresholdLevel is TJSONNull);
      end
      else
      begin
        AssertEquals(Name + ': thresholding', 'explicit', Pars.Strings['thresholding']);
        AssertEquals(Name + ': thresholdLevel', Opts.ThresholdLevel, ThresholdLevel.AsInteger);
      end;

      if Opts.AutoContentRect then
        ExpectedArea := 'auto'
      else if Trim(Opts.ContentMargins) <> '' then
        ExpectedArea := 'margins'
      else
        ExpectedArea := 'page';
      AssertEquals(Name + ': contentArea', ExpectedArea, Pars.Strings['contentArea']);

      if Opts.DpiOverrideEnabled then
        AssertEquals(Name + ': dpiOverride', Opts.DpiOverride, Pars.Integers['dpiOverride'])
      else
        AssertTrue(Name + ': dpiOverride', Pars.Find('dpiOverride') is TJSONNull);

      AssertEquals(Name + ': resamplingFilter', FilterNames[Opts.ResamplingFilter], Pars.Strings['resamplingFilter']);
      AssertEquals(Name + ': backgroundColor', IntToHex(Opts.BackgroundColor, 8), Pars.Strings['backgroundColor']);

      if Opts.ForcedOutputFormat = fofNone then
        AssertTrue(Name + ': outputFormat', Pars.Find('outputFormat') is TJSONNull)
      else
        AssertEquals(Name + ': outputFormat', OutputFormatNames[Opts.ForcedOutputFormat], Pars.Strings['outputFormat']);

      if ffJpeg in Opts.OutputFileParamsEnabled then
        AssertEquals(Name + ': jpegQuality', Opts.JpegCompressionQuality, Pars.Integers['jpegQuality'])
      else
        AssertTrue(Name + ': jpegQuality', Pars.Find('jpegQuality') is TJSONNull);

      if ffTiff in Opts.OutputFileParamsEnabled then
        AssertEquals(Name + ': tiffCompression', TiffNames[Opts.TiffCompressionScheme], Pars.Strings['tiffCompression'])
      else
        AssertTrue(Name + ': tiffCompression', Pars.Find('tiffCompression') is TJSONNull);

      AssertEquals(Name + ': detectOnly', Opts.DetectOnly, Pars.Booleans['detectOnly']);
      AssertEquals(Name + ': forceRotation', Opts.ForceRotation, Pars.Booleans['forceRotation']);
      AssertEquals(Name + ': cropToInput', Opts.CropToInput, Pars.Booleans['cropToInput']);

      // Output written where the GUI expects it
      OutputObj := Root.Objects['output'];
      if Opts.DetectOnly then
        AssertEquals(Name + ': output written', 'none', OutputObj.Strings['written'])
      else
      begin
        AssertTrue(Name + ': output written', OutputObj.Strings['written'] <> 'none');
        AssertTrue(Name + ': output file ' + Params[1], FileExists(Params[1]));
      end;
    finally
      Report.Free;
    end;
  finally
    Params.Free;
  end;
end;

procedure TTestGuiCli.TestDefaults;
var
  Opts: TOptions;
begin
  // Nothing but the background is passed: defaults of the GUI must be those of the CLI
  Opts := NewOptions;
  try
    RunAndCheck('defaults', Opts);
    Opts.BackgroundColor := $FF000000;
    RunAndCheck('black background (not passed)', Opts);
  finally
    Opts.Free;
  end;
end;

procedure TTestGuiCli.TestDetectionOptions;
var
  Opts: TOptions;
begin
  Opts := NewOptions;
  try
    Opts.MaxAngle := 5.5;
    Opts.AngleStep := 0.05;
    Opts.SkipAngle := 0;
    Opts.ThresholdingAuto := False;
    Opts.ThresholdLevel := 99;
    Opts.CropToInput := True;
    RunAndCheck('angles, threshold, crop', Opts);

    Opts.SkipAngle := 2;
    Opts.ForceRotation := True;
    Opts.ResamplingFilter := rfLanczos;
    RunAndCheck('skip angle, forced rotation, lanczos', Opts);
  finally
    Opts.Free;
  end;
end;

procedure TTestGuiCli.TestContentAuto;
var
  Opts: TOptions;
begin
  Opts := NewOptions;
  try
    Opts.AutoContentRect := True;
    Opts.DetectOnly := True;
    Opts.SaveWorkImage := True;
    Opts.PrintParams := True;
    RunAndCheck('auto content, detect only, work image', Opts);
    AssertTrue('work image', FileExists(IncludeTrailingPathDelimiter(FTempDir) + '2.work.png'));
  finally
    Opts.Free;
  end;
end;

procedure TTestGuiCli.TestMarginsAndDpi;
var
  Opts: TOptions;
begin
  Opts := NewOptions;
  try
    Opts.ContentMargins := ' 5, 10 ,% ';
    Opts.ResamplingFilter := rfCubic;
    RunAndCheck('margins in %', Opts);

    // Test image has no DPI: physical units work with DPI given in the GUI
    Opts.ContentMargins := '10,mm';
    Opts.DpiOverrideEnabled := True;
    Opts.DpiOverride := 300;
    RunAndCheck('margins in mm with DPI', Opts);
  finally
    Opts.Free;
  end;
end;

procedure TTestGuiCli.TestOutputFormats;
var
  Opts: TOptions;
  Format: TForcedOutputFormat;
begin
  Opts := NewOptions;
  try
    Opts.BackgroundColor := $80FF0000;
    Opts.ResamplingFilter := rfNearest;
    for Format := Succ(fofNone) to High(TForcedOutputFormat) do
    begin
      Opts.ForcedOutputFormat := Format;
      RunAndCheck('output format ' + OutputFormatNames[Format], Opts);
    end;
  finally
    Opts.Free;
  end;
end;

procedure TTestGuiCli.TestFileFormats;
var
  Opts: TOptions;
  FileFormat: TFileFormat;
begin
  // Every file format offered in the GUI can be written by the CLI
  Opts := NewOptions;
  try
    for FileFormat := Low(TFileFormat) to High(TFileFormat) do
    begin
      Opts.OutputFileFormat := FileFormat;
      Opts.OutputFileParamsEnabled := [];
      if FileFormat = ffJpeg then
      begin
        Opts.OutputFileParamsEnabled := [ffJpeg];
        Opts.JpegCompressionQuality := 75;
      end;
      RunAndCheck('file format ' + IntToStr(Ord(FileFormat)), Opts);
    end;
  finally
    Opts.Free;
  end;
end;

procedure TTestGuiCli.TestTiffCompressions;
var
  Opts: TOptions;
  Scheme: Integer;
begin
  Opts := NewOptions;
  try
    Opts.OutputFileFormat := ffTiff;
    Opts.OutputFileParamsEnabled := [ffTiff, ffJpeg];
    for Scheme := Low(TiffNames) to High(TiffNames) do
    begin
      Opts.TiffCompressionScheme := Scheme;
      RunAndCheck('TIFF compression ' + TiffNames[Scheme], Opts);
    end;
  finally
    Opts.Free;
  end;
end;

procedure TTestGuiCli.TestExtraParameters;
var
  Opts: TOptions;
begin
  Opts := NewOptions;
  try
    Opts.ExtraCmdLineArgs := '-s  t';
    RunAndCheck('extra parameters', Opts);
  finally
    Opts.Free;
  end;
end;

initialization
  Randomize;
  RegisterTest(TTestGuiCli);
end.
