unit Options;

interface

uses
  Classes, SysUtils, ImagingTypes, IniFiles;

type
  TForcedOutputFormat = (
    fofNone,
    fofBinary1,
    fofGray8,
    fofRgb24,
    fofRgba32
  );

  TFileFormat = (
    ffSameAsInput,
    ffPng,
    ffJpeg,
    ffTiff,
    ffBmp,
    ffPsd,
    ffTga,
    ffJng,
    ffPpm
  );

  { TOptions }

  TOptions = class
  private
    FFiles: TStrings;
    function GetEffectiveExecutablePath: string;
    function GetOutputFilePath(const InputFilePath: string): string;
  public
    // Basic options
    DefaultOutputFileOptions: Boolean;
    OutputFolder: string;
    OutputFileFormat: TFileFormat;
    BackgroundColor: TColor32;

    // Advanced options
    MaxAngle: Double;
    ThresholdLevel: Integer;
    ForcedOutputFormat: TForcedOutputFormat;
    SkipAngle: Double;
    JpegCompressionQuality: Integer;
    TiffCompressionScheme: Integer;
    DefaultExecutable: Boolean;
    CustomExecutablePath: string;

    constructor Create;
    destructor Destroy; override;

    procedure ToCmdLineParameters(AParams: TStrings; AFileIndex: Integer);

    procedure SaveToIni(Ini: TIniFile);
    procedure LoadFromIni(Ini: TIniFile);
    procedure Reset;

    property Files: TStrings read FFiles;
    property EffectiveExecutablePath: string read GetEffectiveExecutablePath;
  end;

implementation

uses
  ImagingUtility, Utils;

const
  DefaultBackgroundColor = $FFFFFFFF; // white
  DefaultMaxAngle = 10.0;
  DefaultSkipAngle = 0.01;
  DefaultThresholdLevel = -1; // auto
  DefaultJpegCompressionQuality = -1; // use imaginglib default
  DefaultTiffCompressionScheme = -1; // use imaginglib default
  DefaultOutputFileNamePrefix = 'deskewed-';

  FileExts: array[TFileFormat] of string = (
    '',    // ffSameAsInput
    'png', // ffPng
    'jpg', // ffJpeg
    'tif', // ffTiff
    'bmp', // ffBmp
    'psd', // ffPsd
    'tga', // ffTga
    'jng', // ffJng
    'ppm'  // ffPpm
  );

  FormatIds: array[TForcedOutputFormat] of string = (
    '',      // fofNone,
    'b1',    // fofBinary1
    'g8',    // fofGray8
    'rgb24', // fofRgb24
    'rgba32' // fofRgba32
  );

  IniSectionOptions = 'Options';
  IniSectionAdvanced = 'Advanced';

{ TOptions }

constructor TOptions.Create;
begin
  FFiles := TStringList.Create;
end;

destructor TOptions.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

function TOptions.GetEffectiveExecutablePath: string;
begin
  if DefaultExecutable then
    Result := Utils.FindDeskewExePath
  else
    Result := CustomExecutablePath;
end;

function TOptions.GetOutputFilePath(const InputFilePath: string): string;
var
  FileName: string;
begin
  FileName := ExtractFileName(InputFilePath);

  if DefaultOutputFileOptions then
  begin
    Result := ExtractFilePath(InputFilePath) + DefaultOutputFileNamePrefix + FileName;
  end
  else
  begin
    if OutputFileFormat <> ffSameAsInput then
      FileName := ChangeFileExt(FileName, '.' + FileExts[OutputFileFormat]);

    Result := IncludeTrailingPathDelimiter(OutputFolder) + FileName;

    // Try to avoid overwriting existing file (in case in-folder = out-folder)
    if FileExists(Result) then
      Result := IncludeTrailingPathDelimiter(OutputFolder) + DefaultOutputFileNamePrefix + FileName;
  end;
end;

procedure TOptions.ToCmdLineParameters(AParams: TStrings; AFileIndex: Integer);

  function FloatToStrFmt(const F: Double): string;
  begin
    Result := Format('%.2f', [F], ImagingUtility.GetFormatSettingsForFloats);
  end;

begin
  Assert(AFileIndex < FFiles.Count);

  AParams.Clear;

  AParams.AddStrings(['-o', GetOutputFilePath(FFiles[AFileIndex])]);

  if BackgroundColor <> $FF000000 then
    AParams.AddStrings(['-b', IntToHex(BackgroundColor, 8)]);

  // Advanced options
  if not SameFloat(MaxAngle, DefaultMaxAngle, 0.1) then
    AParams.AddStrings(['-a', FloatToStrFmt(MaxAngle)]);
  if not SameFloat(SkipAngle, DefaultSkipAngle, 0.01) then
    AParams.AddStrings(['-l', FloatToStrFmt(SkipAngle)]);
  if ForcedOutputFormat <> fofNone then
    AParams.AddStrings(['-f', FormatIds[ForcedOutputFormat]]);

{$IFDEF DEBUG}
  AParams.AddStrings(['-s', 'p']);
{$ENDIF}
  AParams.Add(FFiles[AFileIndex]);
end;

procedure TOptions.SaveToIni(Ini: TIniFile);
begin
  Ini.WriteString(IniSectionOptions, 'DefaultOutputFileOptions', BoolToStr(DefaultOutputFileOptions, True));
  Ini.WriteString(IniSectionOptions, 'OutputFolder', OutputFolder);
  Ini.WriteString(IniSectionOptions, 'OutputFileFormat', TEnumUtils<TFileFormat>.EnumToStr(OutputFileFormat));
  Ini.WriteString(IniSectionOptions, 'BackgroundColor', ColorToString(BackgroundColor));

  Ini.WriteFloat(IniSectionAdvanced, 'MaxAngle', MaxAngle);
  Ini.WriteInteger(IniSectionAdvanced, 'ThresholdLevel', ThresholdLevel);
  Ini.WriteString(IniSectionAdvanced, 'ForcedOutputFormat', TEnumUtils<TForcedOutputFormat>.EnumToStr(ForcedOutputFormat));
  Ini.WriteFloat(IniSectionAdvanced, 'SkipAngle', SkipAngle);
  Ini.WriteInteger(IniSectionAdvanced, 'JpegCompressionQuality', JpegCompressionQuality);
  Ini.WriteInteger(IniSectionAdvanced, 'TiffCompressionScheme', TiffCompressionScheme);
  Ini.WriteString(IniSectionAdvanced, 'DefaultExecutable', BoolToStr(DefaultExecutable, True));
  Ini.WriteString(IniSectionAdvanced, 'CustomExecutablePath', CustomExecutablePath);
end;

procedure TOptions.LoadFromIni(Ini: TIniFile);
begin
  DefaultOutputFileOptions := Ini.ReadBool(IniSectionOptions, 'DefaultOutputFileOptions', True);
  OutputFolder := Ini.ReadString(IniSectionOptions, 'OutputFolder', '');
  OutputFileFormat := TEnumUtils<TFileFormat>.StrToEnum(Ini.ReadString(IniSectionOptions, 'OutputFileFormat', ''));
  BackgroundColor := StringToColorDef(Ini.ReadString(IniSectionOptions, 'BackgroundColor', ''), DefaultBackgroundColor);

  MaxAngle := Ini.ReadFloat(IniSectionAdvanced, 'MaxAngle', DefaultMaxAngle);
  ThresholdLevel := Ini.ReadInteger(IniSectionAdvanced, 'ThresholdLevel', DefaultThresholdLevel);
  ForcedOutputFormat := TEnumUtils<TForcedOutputFormat>.StrToEnum(Ini.ReadString(IniSectionAdvanced, 'ForcedOutputFormat', ''));
  SkipAngle := Ini.ReadFloat(IniSectionAdvanced, 'SkipAngle', DefaultSkipAngle);
  JpegCompressionQuality := Ini.ReadInteger(IniSectionAdvanced, 'JpegCompressionQuality', DefaultJpegCompressionQuality);
  TiffCompressionScheme := Ini.ReadInteger(IniSectionAdvanced, 'TiffCompressionScheme', DefaultTiffCompressionScheme);
  DefaultExecutable := Ini.ReadBool(IniSectionAdvanced, 'DefaultExecutable', True);
  CustomExecutablePath := Ini.ReadString(IniSectionAdvanced, 'CustomExecutablePath', '');
end;

procedure TOptions.Reset;
begin

end;

end.

