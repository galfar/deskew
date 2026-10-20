{
  Deskew
  by Marek Mauder
  https://galfar.vevb.net/deskew
  https://github.com/galfar/deskew
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
}
unit Options;

// Needed for int<>set casting for TypInfo.SetToString to compile
{$PACKENUM 1}
{$PACKSET 4}

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
    ffTga,
    ffPpm,
    ffPgm
  );
  TFileFormatSet = set of TFileFormat;

  TResamplingFilter = (
    rfDefaultLinear,
    rfNearest,
    rfCubic,
    rfLanczos
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
    CropToInput: Boolean;

    // Advanced options
    AngleStep: Double;
    DpiOverrideEnabled: Boolean;
    DpiOverride: Integer;
    DetectOnly: Boolean;
    ForceRotation: Boolean;
    AutoContentRect: Boolean;
    ContentMargins: string;
    ResamplingFilter: TResamplingFilter;
    ForcedOutputFormat: TForcedOutputFormat;
    JpegCompressionQuality: Integer;
    TiffCompressionScheme: Integer;
    OutputFileParamsEnabled: TFileFormatSet;
    MaxAngle: Double;
    SkipAngle: Double;
    ThresholdingAuto: Boolean;
    ThresholdLevel: Integer;
    PrintParams: Boolean;
    ExtraCmdLineArgs: string;
    DefaultExecutable: Boolean;
    CustomExecutablePath: string;

    constructor Create;
    destructor Destroy; override;

    procedure ToCmdLineParameters(AParams: TStrings; AFileIndex: Integer);

    procedure SaveToIni(Ini: TIniFile);
    procedure LoadFromIni(Ini: TIniFile);
    procedure Reset;

    // Checks margins given in the form used by the command line tool:
    // 1, 2, or 4 numbers and optional unit separated by commas.
    // Empty string (no margins) is valid too.
    class function IsValidMargins(const Margins: string): Boolean;

    property Files: TStrings read FFiles;
    property EffectiveExecutablePath: string read GetEffectiveExecutablePath;
  end;

implementation

uses
  ImagingUtility, Utils, TypInfo, LazFileUtils;

// From ImagingTiff.pas
const
  TiffCompressionOptionNone        = 0;
  TiffCompressionOptionLzw         = 1;
  TiffCompressionOptionPackbitsRle = 2;
  TiffCompressionOptionDeflate     = 3;
  TiffCompressionOptionJpeg        = 4;
  TiffCompressionOptionGroup4      = 5;
  // From CmdLineOptions.pas
  TiffCompressionOptionAsInput         = 6;
  TiffCompressionOptionAsInputLossless = 7;

const
  DefaultBackgroundColor = $FFFFFFFF; // white
  DefaultMaxAngle = 10.0;
  DefaultAngleStep = 0.1;
  DefaultSkipAngle = 0.01;
  DefaultDpiOverride = 300;
  DefaultThresholdLevel = 128;
  DefaultJpegCompressionQuality = 90; // imaginglib default
  DefaultTiffCompressionScheme = TiffCompressionOptionLzw; // imaginglib default
  DefaultOutputFileNamePrefix = 'deskewed-';
  DefaultPrintParams = {$IFNDEF DEBUG}False{$ELSE}True{$ENDIF};

  // From CmdLineOptions.pas
  MarginUnits: array[0..4] of string = ('px', '%', 'mm', 'cm', 'in');

  FileExts: array[TFileFormat] of string = (
    '',    // ffSameAsInput
    'png', // ffPng
    'jpg', // ffJpeg
    'tif', // ffTiff
    'bmp', // ffBmp
    'tga', // ffTga
    'ppm', // ffPpm
    'pgm'  // ffPgm
  );

  FormatIds: array[TForcedOutputFormat] of string = (
    '',      // fofNone,
    'b1',    // fofBinary1
    'g8',    // fofGray8
    'rgb24', // fofRgb24
    'rgba32' // fofRgba32
  );

  ResamplingIds: array[TResamplingFilter] of string = (
    'linear',  // rfDefaultLinear
    'nearest', // rfNearest
    'cubic',   // rfCubic
    'lanczos'  // rfLanczos
  );

  TiffCompressionSchemeNames: array[TiffCompressionOptionNone..TiffCompressionOptionAsInputLossless] of string = (
    'none', 'lzw', 'rle', 'deflate', 'jpeg', 'g4', 'input', 'input-lossless'
  );

  IniSectionOptions = 'Options';
  IniSectionAdvancedOld = 'Advanced';

{ TOptions }

constructor TOptions.Create;
begin
  FFiles := TStringList.Create;
  Reset;
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
  FileName, InputFolder, UsedOutputFolder: string;
begin
  FileName := ExtractFileName(InputFilePath);
  InputFolder := ExtractFilePath(InputFilePath);

  if DefaultOutputFileOptions then
  begin
    Result := InputFolder + DefaultOutputFileNamePrefix + FileName;
  end
  else
  begin
    if OutputFileFormat <> ffSameAsInput then
      FileName := ChangeFileExt(FileName, '.' + FileExts[OutputFileFormat]);

    UsedOutputFolder := OutputFolder;

    if UsedOutputFolder = '' then
    begin
      // No output folder is speficied, use the same as input
      UsedOutputFolder := InputFolder;
    end;

    UsedOutputFolder := IncludeTrailingPathDelimiter(UsedOutputFolder);

    if UsedOutputFolder = InputFolder then
    begin
       // Try to avoid overwriting existing file (in case in-folder = out-folder)
       Result := UsedOutputFolder + DefaultOutputFileNamePrefix + FileName;
    end
    else
      Result := UsedOutputFolder + FileName;
  end;
end;

procedure TOptions.ToCmdLineParameters(AParams: TStrings; AFileIndex: Integer);

  function FloatToStrFmt(const F: Double): string;
  begin
    Result := Format('%.2f', [F], ImagingUtility.GetFormatSettingsForFloats);
  end;

  function FileParamsToString: string;
  var
    FileParams: array of string;
    Count,Idx: Integer;
  begin
    Count := PopCnt(Cardinal(OutputFileParamsEnabled));
    Idx := 0;
    SetLength(FileParams, Count);
    if ffJpeg in OutputFileParamsEnabled then
    begin
      FileParams[Idx] := 'j' + IntToStr(JpegCompressionQuality);
      Inc(Idx);
    end;
    if ffTiff in OutputFileParamsEnabled then
      FileParams[Idx] := 't' + TiffCompressionSchemeNames[TiffCompressionScheme];

    Result := StrArrayJoin(FileParams, ',');
  end;

  function OperationalFlagsToString: string;
  begin
    Result := '';
    if CropToInput then
      Result := Result + 'c';
    if DetectOnly then
      Result := Result + 'd';
    if ForceRotation then
      Result := Result + 'f';
  end;

  function MarginsToString: string;
  begin
    // No spaces so it is always a single parameter
    Result := StringReplace(Trim(ContentMargins), ' ', '', [rfReplaceAll]);
  end;

  procedure AddExtraCmdLineArgs;
  var
    ExtraArgs: TStringList;
  begin
    ExtraArgs := TStringList.Create;
    try
      // Handles quoted args and repeated spaces
      SplitCmdLineParams(ExtraCmdLineArgs, ExtraArgs);
      AParams.AddStrings(ExtraArgs);
    finally
      ExtraArgs.Free;
    end;
  end;

begin
  Assert(AFileIndex < FFiles.Count);

  AParams.Clear;
  AParams.AddStrings(['-o', GetOutputFilePath(FFiles[AFileIndex])]);

  if BackgroundColor <> $FF000000 then
    AParams.AddStrings(['-b', IntToHex(BackgroundColor, 8)]);
  if OperationalFlagsToString <> '' then
    AParams.AddStrings(['-g', OperationalFlagsToString]);

  // Advanced options
  // Tolerances are well below the precision of the values in UI
  if not SameFloat(MaxAngle, DefaultMaxAngle, 0.001) then
    AParams.AddStrings(['-a', FloatToStrFmt(MaxAngle)]);
  if not SameFloat(AngleStep, DefaultAngleStep, 0.001) then
    AParams.AddStrings(['-d', FloatToStrFmt(AngleStep)]);
  if not SameFloat(SkipAngle, DefaultSkipAngle, 0.001) then
    AParams.AddStrings(['-l', FloatToStrFmt(SkipAngle)]);
  if AutoContentRect then
    AParams.AddStrings(['-r', 'auto'])
  else if MarginsToString <> '' then
    AParams.AddStrings(['-m', MarginsToString]);
  if DpiOverrideEnabled then
    AParams.AddStrings(['-p', IntToStr(DpiOverride)]);
  if ForcedOutputFormat <> fofNone then
    AParams.AddStrings(['-f', FormatIds[ForcedOutputFormat]]);
  if ResamplingFilter <> rfDefaultLinear then
    AParams.AddStrings(['-q', ResamplingIds[ResamplingFilter]]);
  if OutputFileParamsEnabled <> [ ] then
    AParams.AddStrings(['-c', FileParamsToString]);
  if not ThresholdingAuto then
    AParams.AddStrings(['-t', IntToStr(ThresholdLevel)]);
  if PrintParams then
    AParams.AddStrings(['-s', 'p']);

  if ExtraCmdLineArgs <> '' then
    AddExtraCmdLineArgs;

  AParams.Add(FFiles[AFileIndex]);
end;

class function TOptions.IsValidMargins(const Margins: string): Boolean;
var
  Parts: TStringList;
  I, NumCount: Integer;
  Value: Double;
  S: string;
begin
  if Trim(Margins) = '' then
    Exit(True);

  Parts := TStringList.Create;
  try
    Parts.StrictDelimiter := True;
    Parts.Delimiter := ',';
    Parts.DelimitedText := Margins;
    NumCount := Parts.Count;

    // Unit can be the last item
    S := LowerCase(Trim(Parts[Parts.Count - 1]));
    for I := Low(MarginUnits) to High(MarginUnits) do
      if S = MarginUnits[I] then
      begin
        Dec(NumCount);
        Break;
      end;

    Result := NumCount in [1, 2, 4];
    if Result then
      for I := 0 to NumCount - 1 do
        if not TryStrToFloat(Trim(Parts[I]), Value, ImagingUtility.GetFormatSettingsForFloats) or (Value < 0) then
          Exit(False);
  finally
    Parts.Free;
  end;
end;

procedure TOptions.Reset;
begin
  DefaultOutputFileOptions := True;
  OutputFolder := '';
  OutputFileFormat := ffSameAsInput;
  BackgroundColor := DefaultBackgroundColor;
  CropToInput := False;

  ResamplingFilter := rfDefaultLinear;
  MaxAngle := DefaultMaxAngle;
  AngleStep := DefaultAngleStep;
  DpiOverrideEnabled := False;
  DpiOverride := DefaultDpiOverride;
  DetectOnly := False;
  ForceRotation := False;
  AutoContentRect := False;
  ContentMargins := '';
  ThresholdLevel := DefaultThresholdLevel;
  ThresholdingAuto := True;
  ForcedOutputFormat := fofNone;
  SkipAngle := DefaultSkipAngle;
  OutputFileParamsEnabled := [ ];
  JpegCompressionQuality := DefaultJpegCompressionQuality;
  TiffCompressionScheme := DefaultTiffCompressionScheme;
  PrintParams := DefaultPrintParams;
  ExtraCmdLineArgs := '';
  DefaultExecutable := True;
  CustomExecutablePath := '';
end;

procedure TOptions.SaveToIni(Ini: TIniFile);
begin
  Ini.WriteNiceBool(IniSectionOptions, 'DefaultOutputFileOptions', DefaultOutputFileOptions);
  Ini.WriteString(IniSectionOptions, 'OutputFolder', OutputFolder);
  Ini.WriteString(IniSectionOptions, 'OutputFileFormat', TEnumUtils<TFileFormat>.EnumToStr(OutputFileFormat));
  Ini.WriteString(IniSectionOptions, 'BackgroundColor', ColorToString(BackgroundColor));
  Ini.WriteNiceBool(IniSectionOptions, 'CropToInput', CropToInput);
  Ini.WriteFloat(IniSectionOptions, 'MaxAngle', MaxAngle);
  Ini.WriteString(IniSectionOptions, 'ResamplingFilter', TEnumUtils<TResamplingFilter>.EnumToStr(ResamplingFilter));
  Ini.WriteFloat(IniSectionOptions, 'AngleStep', AngleStep);
  Ini.WriteNiceBool(IniSectionOptions, 'DpiOverrideEnabled', DpiOverrideEnabled);
  Ini.WriteInteger(IniSectionOptions, 'DpiOverride', DpiOverride);
  Ini.WriteNiceBool(IniSectionOptions, 'DetectOnly', DetectOnly);
  Ini.WriteNiceBool(IniSectionOptions, 'ForceRotation', ForceRotation);
  Ini.WriteNiceBool(IniSectionOptions, 'AutoContentRect', AutoContentRect);
  Ini.WriteString(IniSectionOptions, 'ContentMargins', ContentMargins);
  Ini.WriteInteger(IniSectionOptions, 'ThresholdLevel', ThresholdLevel);
  Ini.WriteNiceBool(IniSectionOptions, 'ThresholdingAuto', ThresholdingAuto);
  Ini.WriteString(IniSectionOptions, 'ForcedOutputFormat', TEnumUtils<TForcedOutputFormat>.EnumToStr(ForcedOutputFormat));
  Ini.WriteFloat(IniSectionOptions, 'SkipAngle', SkipAngle);
  Ini.WriteString(IniSectionOptions, 'OutputFileParamsEnabled', SetToString(PTypeInfo(TypeInfo(TFileFormatSet)), Integer(OutputFileParamsEnabled), True));
  Ini.WriteInteger(IniSectionOptions, 'JpegCompressionQuality', JpegCompressionQuality);
  Ini.WriteInteger(IniSectionOptions, 'TiffCompressionScheme', TiffCompressionScheme);
  Ini.WriteNiceBool(IniSectionOptions, 'PrintParams', PrintParams);
  Ini.WriteString(IniSectionOptions, 'ExtraCmdLineArgs', ExtraCmdLineArgs);
  Ini.WriteNiceBool(IniSectionOptions, 'DefaultExecutable', DefaultExecutable);
  Ini.WriteString(IniSectionOptions, 'CustomExecutablePath', CustomExecutablePath);
end;

procedure TOptions.LoadFromIni(Ini: TIniFile);
var
  Values: TStringList;
  I: Integer;
begin
  // Older versions kept part of the settings in a separate [Advanced] section,
  // move them over (whatever is already in [Options] wins).
  // TODO: remove this in another relase, all users whould be "upgraded" by then
  if Ini.SectionExists(IniSectionAdvancedOld) then
  begin
    Values := TStringList.Create;
    try
      Ini.ReadSectionValues(IniSectionAdvancedOld, Values);
      for I := 0 to Values.Count - 1 do
        if not Ini.ValueExists(IniSectionOptions, Values.Names[I]) then
          Ini.WriteString(IniSectionOptions, Values.Names[I], Values.ValueFromIndex[I]);
    finally
      Values.Free;
    end;
    Ini.EraseSection(IniSectionAdvancedOld);
  end;

  DefaultOutputFileOptions := Ini.ReadNiceBool(IniSectionOptions, 'DefaultOutputFileOptions', True);
  OutputFolder := Ini.ReadString(IniSectionOptions, 'OutputFolder', '');
  OutputFileFormat := TEnumUtils<TFileFormat>.StrToEnum(Ini.ReadString(IniSectionOptions, 'OutputFileFormat', ''));
  BackgroundColor := StringToColorDef(Ini.ReadString(IniSectionOptions, 'BackgroundColor', ''), DefaultBackgroundColor);
  CropToInput := Ini.ReadNiceBool(IniSectionOptions, 'CropToInput', False);
  MaxAngle := Ini.ReadFloat(IniSectionOptions, 'MaxAngle', DefaultMaxAngle);
  ResamplingFilter := TEnumUtils<TResamplingFilter>.StrToEnum(Ini.ReadString(IniSectionOptions, 'ResamplingFilter', ''));
  AngleStep := Ini.ReadFloat(IniSectionOptions, 'AngleStep', DefaultAngleStep);
  DpiOverrideEnabled := Ini.ReadNiceBool(IniSectionOptions, 'DpiOverrideEnabled', False);
  DpiOverride := Ini.ReadInteger(IniSectionOptions, 'DpiOverride', DefaultDpiOverride);
  DetectOnly := Ini.ReadNiceBool(IniSectionOptions, 'DetectOnly', False);
  ForceRotation := Ini.ReadNiceBool(IniSectionOptions, 'ForceRotation', False);
  AutoContentRect := Ini.ReadNiceBool(IniSectionOptions, 'AutoContentRect', False);
  ContentMargins := Ini.ReadString(IniSectionOptions, 'ContentMargins', '');
  if not IsValidMargins(ContentMargins) then
    ContentMargins := '';
  ThresholdLevel := Ini.ReadInteger(IniSectionOptions, 'ThresholdLevel', DefaultThresholdLevel);
  ThresholdingAuto := Ini.ReadNiceBool(IniSectionOptions, 'ThresholdingAuto', True);

  ForcedOutputFormat := TEnumUtils<TForcedOutputFormat>.StrToEnum(Ini.ReadString(IniSectionOptions, 'ForcedOutputFormat', ''));
  SkipAngle := Ini.ReadFloat(IniSectionOptions, 'SkipAngle', DefaultSkipAngle);
  OutputFileParamsEnabled := TFileFormatSet(StringToSet(PTypeInfo(TypeInfo(TFileFormatSet)), Ini.ReadString(IniSectionOptions, 'OutputFileParamsEnabled', '[]')));
  JpegCompressionQuality := Ini.ReadInteger(IniSectionOptions, 'JpegCompressionQuality', DefaultJpegCompressionQuality);
  TiffCompressionScheme := Ini.ReadInteger(IniSectionOptions, 'TiffCompressionScheme', DefaultTiffCompressionScheme);
  if (TiffCompressionScheme < Low(TiffCompressionSchemeNames)) or (TiffCompressionScheme > High(TiffCompressionSchemeNames)) then
    TiffCompressionScheme := DefaultTiffCompressionScheme;
  PrintParams := Ini.ReadNiceBool(IniSectionOptions, 'PrintParams', DefaultPrintParams);
  ExtraCmdLineArgs := Ini.ReadString(IniSectionOptions, 'ExtraCmdLineArgs', '');
  DefaultExecutable := Ini.ReadNiceBool(IniSectionOptions, 'DefaultExecutable', True);
  CustomExecutablePath := Ini.ReadString(IniSectionOptions, 'CustomExecutablePath', '');
end;

end.

