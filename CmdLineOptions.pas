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
unit CmdLineOptions;

interface

uses
  Types,
  SysUtils,
  Classes,
  StrUtils,
  ImagingTypes,
  Imaging,
  ImagingUtility,
  ImagingTiff,
  ImageUtils;

const
  DefaultThreshold = 128;
  DefaultMaxAngle = 10;
  DefaultMinTestedPixels = 25;
  DefaultSkipAngle = 0.01;

type
  TThresholdingMethod = (
    // Use explicit threshold [0..255]
    tmExplicit,
    // Use adaptive thresholding: Otsu's method
    tmOtsu
  );

  TOperationalFlag = (
    ofCropToInput,
    ofDetectOnly
  );
  TOperationalFlags = set of TOperationalFlag;

  TSizeUnit = (
    suPixels,
    suPercent,
    suMm,
    suCm,
    suInch
  );

  TCmdLineOptions = class
  private
    FInputFileName: string;
    FOutputFileName: string;
    FMaxAngle: Double;
    FMinTestedPixels: Integer;
    FSkipAngle: Double;
    FResamplingFilter: TResamplingFilter;
    FThresholdingMethod: TThresholdingMethod;
    FThresholdLevel: Integer;
    FContentRect: TFloatRect;
    FContentSizeUnit: TSizeUnit;
    FBackgroundColor: TColor32;
    FForcedOutputFormat: TImageFormat;
    FDpiOverride: Integer;
    FOperationalFlags: TOperationalFlags;
    FShowStats: Boolean;
    FShowParams: Boolean;
    FShowTimings: Boolean;
    FJpegCompressionQuality: Integer;
    FTiffCompressionScheme: Integer;
    FFormatSettings: TFormatSettings;
    FErrorMessage: string;
    function GetIsValid: Boolean;
  public
    constructor Create;
    // Parses command line arguments to get options set by user
    function ParseCommandLine: Boolean;
    function OptionsToString: string;

    // Calculates final content rectangle in pixels for given image based
    // on user input (content vs margin, units).
    function CalcContentRectForImage(const ImageBounds: TRect; Metadata: TMetadata; out FinalRect: TRect): Boolean;

    function TrySetTiffCompressionFromMetadata(Metadata: TMetadata): Boolean;

    property InputFileName: string read FInputFileName;
    property OutputFileName: string read FOutputFileName;
    // Max expected rotation angle - algo then works in range [-MaxAngle, MaxAngle]
    property MaxAngle: Double read FMaxAngle;
    // Minimum tested pixels for any rotation
    property MinTestedPixels: Integer read FMinTestedPixels;
    // Skew threshold angle - skip deskewing if detected skew angle is in range (-MinAngle, MinAngle)
    property SkipAngle: Double read FSkipAngle;
    // Resampling filter used for rotations
    property ResamplingFilter: TResamplingFilter read FResamplingFilter;
    // Thresholding method used when converting images to binary black/white format
    property ThresholdingMethod: TThresholdingMethod read FThresholdingMethod;
    // Threshold for black/white pixel classification for explicit thresholding method
    property ThresholdLevel: Integer read FThresholdLevel;
    // Rect where to do the skew detection on the page image
    property ContentRect: TFloatRect read FContentRect;
    // Unit of size of content or margin regions
    property ContentSizeUnit: TSizeUnit read FContentSizeUnit;
    // Background color for the rotated image
    property BackgroundColor: TColor32 read FBackgroundColor;
    // Forced output format (applied just before saving the output)
    property ForcedOutputFormat: TImageFormat read FForcedOutputFormat;
    // DPI to use when input image has not print resolution info or
    // override it when it's present
    property DpiOverride: Integer read FDpiOverride;
    // On/Off flags that control parts of the whole operation
    property OperationalFlags: TOperationalFlags read FOperationalFlags;
    // Show skew detection stats
    property ShowStats: Boolean read FShowStats;
    // Show current params to user (for testing etc.)
    property ShowParams: Boolean read FShowParams;
    // Show timing of processing steps to user
    property ShowTimings: Boolean read FShowTimings;
    // Compression quality of JPEG outputs (also embedded) in range [1, 100(best)]
    property JpegCompressionQuality: Integer read FJpegCompressionQuality;
    // Compression scheme of TIFF outputs. Values and default in imaginglib.
    property TiffCompressionScheme: Integer read FTiffCompressionScheme;

    property IsValid: Boolean read GetIsValid;
    property ErrorMessage: string read FErrorMessage;
  end;

const
  TiffCompressionOptionAsInput = TiffCompressionOptionGroup4 + 1;

implementation

uses
  TypInfo;

const
  TiffCompressionNames: array[TiffCompressionOptionNone..TiffCompressionOptionAsInput] of string = (
    'none', 'lzw', 'rle', 'deflate', 'jpeg', 'g4', 'input'
  );
  SizeUnitTokens: array[TSizeUnit] of string = ('px', '%', 'mm', 'cm', 'in');

  SDefaultOutputFilePrefix = 'deskewed-';
  SDefaultOutputFileExt = 'png';

var
  FloatFmtSettings: TFormatSettings;

function EnsureTrailingPathDelimiter(const DirPath: string): string;
begin
  // IncludeTrailing... hapilly adds delimiter also to empty dir path
  // (e.g. file in current working dir).
  if DirPath <> '' then
    Result := IncludeTrailingPathDelimiter(DirPath)
  else
    Result := DirPath;
end;

{ TCmdLineOptions }

constructor TCmdLineOptions.Create;
begin
  FThresholdLevel := DefaultThreshold;
  FMaxAngle := DefaultMaxAngle;
  FMinTestedPixels := DefaultMinTestedPixels;
  FSkipAngle := DefaultSkipAngle;
  FResamplingFilter := rfLinear;
  FThresholdingMethod := tmOtsu;
  FContentRect := FloatRect(0, 0, 0, 0); // whole page
  FContentSizeUnit := suPixels;
  FBackgroundColor := $FF000000;
  FOperationalFlags := [];
  FShowStats := False;
  FShowParams := False;
  FShowTimings:= False;
  FForcedOutputFormat := ifUnknown;
  FJpegCompressionQuality := -1; // use imaginglib default
  FTiffCompressionScheme := -1; // use imaginglib default
  FFormatSettings := ImagingUtility.GetFormatSettingsForFloats;
end;

function TCmdLineOptions.GetIsValid: Boolean;
begin
  Result := (InputFileName <> '') and (MaxAngle > 0) and (SkipAngle >= 0) and
    (MinTestedPixels > 0) and
    ((ThresholdingMethod in [tmOtsu]) or (ThresholdingMethod = tmExplicit) and (ThresholdLevel > 0));
end;

function TCmdLineOptions.ParseCommandLine: Boolean;
var
  I: LongInt;
  Param, Arg: string;

  // From Delphi RTL StrUtils.pas - for compiling in Delphi 7
  function SplitString(const S, Delimiters: string): TDynStringArray;
  var
    StartIdx: Integer;
    FoundIdx: Integer;
    SplitPoints: Integer;
    CurrentSplit: Integer;
    I: Integer;

    function FindDelimiter(const Delimiters, S: string; StartIdx: Integer = 1): Integer;
    var
      Stop: Boolean;
      Len: Integer;
    begin
      Result := 0;
      Len := Length(S);
      Stop := False;
      while (not Stop) and (StartIdx <= Len) do
        if IsDelimiter(Delimiters, S, StartIdx) then
        begin
          Result := StartIdx;
          Stop := True;
        end
        else
          Inc(StartIdx);
    end;

  begin
    Result := nil;
    if S <> '' then
    begin
      SplitPoints := 0;
      for I := 1 to Length(S) do
      begin
        if IsDelimiter(Delimiters, S, I) then
          Inc(SplitPoints);
      end;

      SetLength(Result, SplitPoints + 1);
      StartIdx := 1;
      CurrentSplit := 0;
      repeat
        FoundIdx := FindDelimiter(Delimiters, S, StartIdx);
        if FoundIdx <> 0 then
        begin
          Result[CurrentSplit] := Copy(S, StartIdx, FoundIdx - StartIdx);
          Inc(CurrentSplit);
          StartIdx := FoundIdx + 1;
        end;
      until CurrentSplit = SplitPoints;
      Result[SplitPoints] := Copy(S, StartIdx, Length(S) - StartIdx + 1);
    end;
  end;

  function TryParseSizeRect(const StrArray: array of string; out SizeRect: TFloatRect): Boolean;
  var
    I, Len: Integer;
    SingleArray: array of Single;
  begin
    Result := True;
    Len := Length(StrArray);
    SetLength(SingleArray, Len);

    for I := 0 to Len - 1 do
      if not TryStrToFloat(StrArray[I], SingleArray[I], FloatFmtSettings) then
        Exit(False);

    case Len of
      1: SizeRect := FloatRect(SingleArray[0], SingleArray[0], SingleArray[0], SingleArray[0]);
      2: SizeRect := FloatRect(SingleArray[0], SingleArray[1], SingleArray[0], SingleArray[1]);
      4: SizeRect := FloatRect(SingleArray[0], SingleArray[1], SingleArray[2], SingleArray[3]);
    else
      Exit(False);
    end;
  end;

  function TryParseSizeUnit(const Str: string; out SizeUnit: TSizeUnit): Boolean;
  var
    S: TSizeUnit;
  begin
    Result := False;
    for S := Low(SizeUnitTokens) to High(SizeUnitTokens) do
      if SameText(Str, SizeUnitTokens[S]) then
      begin
        SizeUnit := S;
        Exit(True);
      end;
  end;

  function CheckParam(const Param, Value: string): Boolean;
  var
    StrArray: TDynStringArray;
    ValLower, S: string;
    TempColor: Cardinal;
    Val64: Int64;
    I, J, ValLength: Integer;
    ArgsOk: Boolean;
  begin
    Result := True;
    ValLower := LowerCase(Value);

    if Param = '-o' then
    begin
      FOutputFileName := Value
    end
    else if Param = '-a' then
    begin
      if not TryStrToFloat(Value, FMaxAngle, FFormatSettings) then
        FErrorMessage := 'Invalid value for max angle parameter: ' + Value;
    end
    else if Param = '-m' then
    begin
      if not TryStrToInt(Value, FMinTestedPixels) then
        FErrorMessage := 'Invalid value for minimum tested pixels parameter: ' + Value;
    end
    else if Param = '-l' then
    begin
      if not TryStrToFloat(Value, FSkipAngle, FFormatSettings) then
        FErrorMessage := 'Invalid value for skip angle parameter: ' + Value;
    end
    else if Param = '-t' then
    begin
      if ValLower = 'a' then
        FThresholdingMethod := tmOtsu
      else
      begin
        FThresholdingMethod := tmExplicit;
        if not TryStrToInt(Value, FThresholdLevel) then
          FErrorMessage := 'Invalid value for treshold parameter: ' + Value;
      end;
    end
    else if Param = '-b' then
    begin
      ValLength := Length(ValLower);
      if (ValLength <= 8) and TryStrToInt64('$' + ValLower, Val64) then
      begin
        TempColor := Cardinal(Val64 and $FFFFFFFF);
        if (TempColor <= $FF) and (ValLength <= 2) then
        begin
          // Just one channel given, replicate for all channels + make opaque
          FBackgroundColor := Color32($FF, Byte(TempColor), Byte(TempColor), Byte(TempColor)).Color;
        end
        else if (TempColor <= $FFFFFF) and (ValLength <= 6) then
        begin
          // RGB given, set alpha to 255 for background
          FBackgroundColor := $FF000000 or TempColor;
        end
        else
        begin
          // Full ARGB given
          FBackgroundColor := TempColor;
        end;
      end
      else
        FErrorMessage := 'Invalid value for background color parameter: ' + Value;
    end
    else if Param = '-f' then
    begin
      if ValLower = 'b1' then
        FForcedOutputFormat := ifBinary
      else if ValLower = 'g8' then
        FForcedOutputFormat := ifGray8
      else if ValLower = 'rgb24' then
        FForcedOutputFormat := ifR8G8B8
      else if ValLower = 'rgba32' then
        FForcedOutputFormat := ifA8R8G8B8
      else
        FErrorMessage := 'Invalid value for format parameter: ' + Value;
    end
    else if Param = '-q' then
    begin
      if ValLower = 'nearest' then
        FResamplingFilter := rfNearest
      else if ValLower = 'linear' then
        FResamplingFilter := rfLinear
      else if ValLower = 'cubic' then
        FResamplingFilter := rfCubic
      else if ValLower = 'lanczos' then
        FResamplingFilter := rfLanczos
      else
        FErrorMessage := 'Invalid value for resampling filter parameter: ' + Value;
    end
    else if Param = '-g' then
    begin
      if Pos('c', ValLower) > 0 then
        Include(FOperationalFlags, ofCropToInput);
      if Pos('d', ValLower) > 0 then
        Include(FOperationalFlags, ofDetectOnly);
    end
    else if Param = '-s' then
    begin
      if Pos('s', ValLower) > 0 then
        FShowStats := True;
      if Pos('p', ValLower) > 0 then
        FShowParams := True;
      if Pos('t', ValLower) > 0 then
        FShowTimings := True;
    end
    else if Param = '-r' then
    begin
      StrArray := SplitString(ValLower, ',');
      ValLength := Length(StrArray);
      // Allow also unit-less entry for backward compatibility
      ArgsOk := (ValLength in [4, 5]) and TryParseSizeRect(Copy(StrArray, 0, 4), FContentRect);

      if ArgsOk and (ValLength = 5) then
        ArgsOk := TryParseSizeUnit(StrArray[4], FContentSizeUnit);

      if not ArgsOk then
        FErrorMessage := 'Invalid definition of content rectangle: ' + Value;
    end
    else if Param = '-p' then
    begin
      if not TryStrToInt(Value, FDpiOverride) or (FDpiOverride < 1) then
        FErrorMessage := 'Invalid value for DPI override parameter: ' + Value;
    end
    else if Param = '-c' then
    begin
      StrArray := SplitString(ValLower, ',');
      for I := 0 to High(StrArray) do
      begin
        S := StrArray[I];
        if Pos('t', S) = 1 then
        begin
          S := Copy(S, 2);
          FTiffCompressionScheme := -1;

          for J := Low(TiffCompressionNames) to High(TiffCompressionNames) do
          begin
            if S = TiffCompressionNames[J] then
            begin
              FTiffCompressionScheme := J;
              Break;
            end;
          end;

          if FTiffCompressionScheme = -1 then
          begin
            FErrorMessage := 'Invalid TIFF output compression spec: ' + S;
            Exit(False);
          end;
        end
        else if Pos('j', S) = 1 then
        begin
          S := Copy(S, 2);
          if not TryStrToInt(S, FJpegCompressionQuality) then
          begin
            FErrorMessage := 'Invalid JPEG output compression spec: ' + S;
            Exit(False);
          end;
        end
        else
        begin
          FErrorMessage := 'Invalid output compression parameter: ' + S;
          Exit(False);
        end;
      end;
    end
    else
    begin
      FErrorMessage := 'Unknown parameter: ' + Param;
    end;

    if FErrorMessage <> '' then
      Result := False;
  end;

begin
  Result := True;
  I := 1;

  while I <= ParamCount do
  begin
    Param := ParamStr(I);
    if Pos('-', Param) = 1 then
    begin
      Arg := ParamStr(I + 1);
      Inc(I);
      if not CheckParam(Param, Arg) then
      begin
        Result := False;
        Exit;
      end;
    end
    else
      FInputFileName := Param;

    Inc(I);
  end;

  if FInputFileName = '' then
    FErrorMessage := 'No input file given';

  if FOutputFileName = '' then
  begin
    // No user output file name given => use prefixed input file name as default
    // (with PNG as file fomat to not introduce any new compression artifacts when
    // not explicitly asked for ).
    FOutputFileName := EnsureTrailingPathDelimiter(GetFileDir(FInputFileName)) +
      SDefaultOutputFilePrefix + ChangeFileExt(GetFileName(FInputFileName), '.' + SDefaultOutputFileExt);
  end;
end;

function TCmdLineOptions.CalcContentRectForImage(const ImageBounds: TRect; Metadata: TMetadata;
  out FinalRect: TRect): Boolean;
var
  PixXSize, PixYSize: Double;

  function MakeRect(const R: TFloatRect; WidthFactor, HeightFactor: Double): TRect;
  begin
    Result := Rect(Round(R.Left * WidthFactor),
                   Round(R.Top * HeightFactor),
                   Round(R.Right * WidthFactor),
                   Round(R.Bottom * HeightFactor));
  end;

begin
  if IsFloatRectEmpty(ContentRect) then
  begin
    FinalRect := ImageBounds;
    Exit(True);
  end;

  case ContentSizeUnit of
    suPixels: FinalRect := MakeRect(ContentRect, 1, 1);
    suPercent:
      begin
        FinalRect := MakeRect(ContentRect,
                              RectWidth(ImageBounds) / 100,
                              RectHeight(ImageBounds) / 100);
      end;
    suMm:
      begin
        if not Metadata.GetPhysicalPixelSize(TResolutionUnit.ruDpcm, PixXSize, PixYSize) then
          Exit(False);
        FinalRect := MakeRect(ContentRect, PixXSize / 10, PixYSize / 10);
      end;
    suCm:
      begin
        if not Metadata.GetPhysicalPixelSize(TResolutionUnit.ruDpcm, PixXSize, PixYSize) then
          Exit(False);
        FinalRect := MakeRect(ContentRect, PixXSize, PixYSize);
      end;
    suInch:
      begin
        if not Metadata.GetPhysicalPixelSize(TResolutionUnit.ruDpi, PixXSize, PixYSize) then
          Exit(False);
        FinalRect := MakeRect(ContentRect, PixXSize, PixYSize);
      end;
    else
      Assert(False);
  end;

  if not IntersectRect(FinalRect, FinalRect, ImageBounds) then
    FinalRect := ImageBounds;

  Result := True;
end;

function TCmdLineOptions.TrySetTiffCompressionFromMetadata(Metadata: TMetadata): Boolean;
var
  CompName: string;
begin
  Result := True;
  Assert(FTiffCompressionScheme = TiffCompressionOptionAsInput);
  if not Metadata.HasMetaItem(SMetaTiffCompressionName) then
  begin
    FTiffCompressionScheme := -1;
    Exit(False);
  end;

  CompName := Metadata.MetaItems[SMetaTiffCompressionName];

  // Names from "TTiffLibFileFormat.LoadData"
  if CompName = 'None' then
    FTiffCompressionScheme := TiffCompressionOptionNone
  else if CompName = 'LZW' then
    FTiffCompressionScheme := TiffCompressionOptionLzw
  else if CompName = 'JPEG' then
    FTiffCompressionScheme := TiffCompressionOptionJpeg
  else if CompName = 'Deflate' then
    FTiffCompressionScheme := TiffCompressionOptionDeflate
  else if StrUtils.MatchStr(CompName, ['CCITT Group 4 Fax', 'CCITT']) then
    FTiffCompressionScheme := TiffCompressionOptionGroup4;

  if FTiffCompressionScheme = TiffCompressionOptionAsInput then
  begin
    FTiffCompressionScheme := -1;
    Exit(False);
  end;
end;

function TCmdLineOptions.OptionsToString: string;
var
  I: Integer;
  CompJpegStr, CompTiffStr, FilterStr, CmdParams: string;
begin
  CmdParams := '';
  for I := 1 to ParamCount do
    CmdParams := CmdParams + ParamStr(I) + ' ';

  FilterStr := LowerCase(Copy(TypInfo.GetEnumName(TypeInfo(TResamplingFilter), Integer(FResamplingFilter)), 3));
  CompJpegStr := Iff(JpegCompressionQuality = -1, 'default', IntToStr(JpegCompressionQuality));
  CompTiffStr := 'default';
  if TiffCompressionScheme >= 0 then
    CompTiffStr := TiffCompressionNames[TiffCompressionScheme];

  Result :=
    'Parameters: ' + CmdParams + sLineBreak +
    '  input file          = ' + InputFileName + sLineBreak +
    '  output file         = ' + OutputFileName + sLineBreak +
    '  max angle           = ' + FloatToStr(MaxAngle) + sLineBreak +
    '  min tested pixels   = ' + FloatToStr(MinTestedPixels) + sLineBreak +
    '  background color    = ' + IntToHex(BackgroundColor, 8) + sLineBreak +
    '  resampling filter   = ' + FilterStr + sLineBreak +
    '  thresholding method = ' + Iff(ThresholdingMethod = tmExplicit, 'explicit', 'auto otsu') + sLineBreak +
    '  threshold level     = ' + IntToStr(ThresholdLevel) + sLineBreak +
    '  content rect        = ' + Format('%n,%n,%n,%n %s', [ContentRect.Left, ContentRect.Top, ContentRect.Right, ContentRect.Bottom,
                                        SizeUnitTokens[ContentSizeUnit]], FloatFmtSettings) + sLineBreak +
    '  output format       = ' + Iff(ForcedOutputFormat = ifUnknown, 'default', Imaging.GetFormatName(ForcedOutputFormat)) + sLineBreak +
    '  dpi override        = ' + IntToStr(DpiOverride) + sLineBreak +
    '  skip angle          = ' + FloatToStr(SkipAngle) + sLineBreak +
    '  oper flags          = ' + Iff(ofCropToInput in FOperationalFlags, 'crop-to-input ', '')
                               + Iff(ofDetectOnly in FOperationalFlags, 'detect-only ', '') + sLineBreak +
    '  show info           = ' + Iff(ShowParams, 'params ', '') + Iff(ShowStats, 'stats ', '') + Iff(ShowTimings, 'timings ', '') + sLineBreak +
    '  output compression  = jpeg:' + CompJpegStr + ' tiff:' + CompTiffStr + sLineBreak;
end;

initialization
  FloatFmtSettings := SysUtils.FormatSettings;
  FloatFmtSettings.ThousandSeparator := #0;
  FloatFmtSettings.DecimalSeparator  := '.';

end.
