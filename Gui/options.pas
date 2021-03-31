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
    ffPsd,
    ffTga,
    ffJng,
    ffPpm
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
    AutoCrop: Boolean;

    // Advanced options
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

    property Files: TStrings read FFiles;
    property EffectiveExecutablePath: string read GetEffectiveExecutablePath;
  end;

implementation

uses
  ImagingUtility, ImagingTiff, Utils, TypInfo;

const
  DefaultBackgroundColor = $FFFFFFFF; // white
  DefaultMaxAngle = 10.0;
  DefaultSkipAngle = 0.01;
  DefaultThresholdLevel = 128;
  DefaultJpegCompressionQuality = 90; // imaginglib default
  DefaultTiffCompressionScheme = TiffCompressionOptionLzw; // imaginglib default
  DefaultOutputFileNamePrefix = 'deskewed-';
  DefaultPrintParams = {$IFNDEF DEBUG}False{$ELSE}True{$ENDIF};

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

  ResamplingIds: array[TResamplingFilter] of string = (
    'linear',  // rfDefaultLinear
    'nearest', // rfNearest
    'cubic',   // rfCubic
    'lanczos'  // rfLanczos
  );

  TiffCompressionSchemeNames: array[TiffCompressionOptionNone..TiffCompressionOptionGroup4] of string = (
    'none', 'lzw', 'rle', 'deflate', 'jpeg', 'g4'
  );

  IniSectionOptions = 'Options';
  IniSectionAdvanced = 'Advanced';

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

begin
  Assert(AFileIndex < FFiles.Count);

  AParams.Clear;
  AParams.AddStrings(['-o', GetOutputFilePath(FFiles[AFileIndex])]);

  if BackgroundColor <> $FF000000 then
    AParams.AddStrings(['-b', IntToHex(BackgroundColor, 8)]);
  if AutoCrop then
    AParams.AddStrings(['-g', 'c']);

  // Advanced options
  if not SameFloat(MaxAngle, DefaultMaxAngle, 0.1) then
    AParams.AddStrings(['-a', FloatToStrFmt(MaxAngle)]);
  if not SameFloat(SkipAngle, DefaultSkipAngle, 0.01) then
    AParams.AddStrings(['-l', FloatToStrFmt(SkipAngle)]);
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
    AParams.AddStrings(ExtraCmdLineArgs.Split(' '));

  AParams.Add(FFiles[AFileIndex]);
end;

procedure TOptions.Reset;
begin
  DefaultOutputFileOptions := True;
  OutputFolder := '';
  OutputFileFormat := ffSameAsInput;
  BackgroundColor := DefaultBackgroundColor;
  AutoCrop := False;

  ResamplingFilter := rfDefaultLinear;
  MaxAngle := DefaultMaxAngle;
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
  Ini.WriteNiceBool(IniSectionOptions, 'AutoCrop', AutoCrop);

  Ini.WriteString(IniSectionAdvanced, 'ResamplingFilter', TEnumUtils<TResamplingFilter>.EnumToStr(ResamplingFilter));
  Ini.WriteFloat(IniSectionAdvanced, 'MaxAngle', MaxAngle);
  Ini.WriteInteger(IniSectionAdvanced, 'ThresholdLevel', ThresholdLevel);
  Ini.WriteNiceBool(IniSectionAdvanced, 'ThresholdingAuto', ThresholdingAuto);
  Ini.WriteString(IniSectionAdvanced, 'ForcedOutputFormat', TEnumUtils<TForcedOutputFormat>.EnumToStr(ForcedOutputFormat));
  Ini.WriteFloat(IniSectionAdvanced, 'SkipAngle', SkipAngle);
  Ini.WriteString(IniSectionAdvanced, 'OutputFileParamsEnabled', SetToString(PTypeInfo(TypeInfo(TFileFormatSet)), Integer(OutputFileParamsEnabled), True));
  Ini.WriteInteger(IniSectionAdvanced, 'JpegCompressionQuality', JpegCompressionQuality);
  Ini.WriteInteger(IniSectionAdvanced, 'TiffCompressionScheme', TiffCompressionScheme);
  Ini.WriteNiceBool(IniSectionAdvanced, 'PrintParams', PrintParams);
  Ini.WriteString(IniSectionAdvanced, 'ExtraCmdLineArgs', ExtraCmdLineArgs);
  Ini.WriteNiceBool(IniSectionAdvanced, 'DefaultExecutable', DefaultExecutable);
  Ini.WriteString(IniSectionAdvanced, 'CustomExecutablePath', CustomExecutablePath);
end;

procedure TOptions.LoadFromIni(Ini: TIniFile);
begin
  DefaultOutputFileOptions := Ini.ReadNiceBool(IniSectionOptions, 'DefaultOutputFileOptions', True);
  OutputFolder := Ini.ReadString(IniSectionOptions, 'OutputFolder', '');
  OutputFileFormat := TEnumUtils<TFileFormat>.StrToEnum(Ini.ReadString(IniSectionOptions, 'OutputFileFormat', ''));
  BackgroundColor := StringToColorDef(Ini.ReadString(IniSectionOptions, 'BackgroundColor', ''), DefaultBackgroundColor);
  AutoCrop := Ini.ReadNiceBool(IniSectionOptions, 'AutoCrop', False);

  ResamplingFilter := TEnumUtils<TResamplingFilter>.StrToEnum(Ini.ReadString(IniSectionAdvanced, 'ResamplingFilter', ''));
  MaxAngle := Ini.ReadFloat(IniSectionAdvanced, 'MaxAngle', DefaultMaxAngle);
  ThresholdLevel := Ini.ReadInteger(IniSectionAdvanced, 'ThresholdLevel', DefaultThresholdLevel);
  ThresholdingAuto := Ini.ReadNiceBool(IniSectionAdvanced, 'ThresholdingAuto', True);

  ForcedOutputFormat := TEnumUtils<TForcedOutputFormat>.StrToEnum(Ini.ReadString(IniSectionAdvanced, 'ForcedOutputFormat', ''));
  SkipAngle := Ini.ReadFloat(IniSectionAdvanced, 'SkipAngle', DefaultSkipAngle);
  OutputFileParamsEnabled := TFileFormatSet(StringToSet(PTypeInfo(TypeInfo(TFileFormatSet)), Ini.ReadString(IniSectionAdvanced, 'OutputFileParamsEnabled', '[]')));
  JpegCompressionQuality := Ini.ReadInteger(IniSectionAdvanced, 'JpegCompressionQuality', DefaultJpegCompressionQuality);
  TiffCompressionScheme := Ini.ReadInteger(IniSectionAdvanced, 'TiffCompressionScheme', DefaultTiffCompressionScheme);
  PrintParams := Ini.ReadNiceBool(IniSectionAdvanced, 'PrintParams', DefaultPrintParams);
  ExtraCmdLineArgs := Ini.ReadString(IniSectionAdvanced, 'ExtraCmdLineArgs', '');
  DefaultExecutable := Ini.ReadNiceBool(IniSectionAdvanced, 'DefaultExecutable', True);
  CustomExecutablePath := Ini.ReadString(IniSectionAdvanced, 'CustomExecutablePath', '');
end;

end.

