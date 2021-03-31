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
{$IFNDEF FPC}
  Types,
  StrUtils,
{$ENDIF}
  SysUtils,
  Classes,
  ImagingTypes,
  ImagingUtility,
  ImageUtils;

const
  DefaultThreshold = 128;
  DefaultMaxAngle = 10;
  DefaultSkipAngle = 0.01;
  SDefaultOutputFile = 'out.png';

type
  TThresholdingMethod = (
    // Use explicit threshold [0..255]
    tmExplicit,
    // Use adaptive thresholding: Otsu's method
    tmOtsu
  );

  TOperationalFlag = (
    ofAutoCrop,
    ofDetectOnly
  );
  TOperationalFlags = set of TOperationalFlag;

  TCmdLineOptions = class
  private
    FInputFile: string;
    FOutputFile: string;
    FMaxAngle: Double;
    FSkipAngle: Double;
    FResamplingFilter: TResamplingFilter;
    FThresholdingMethod: TThresholdingMethod;
    FThresholdLevel: Integer;
    FContentRect: TRect;
    FBackgroundColor: TColor32;
    FForcedOutputFormat: TImageFormat;
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
    // Parses command line arguments to get optiosn set by user
    function ParseCommnadLine: Boolean;
    function OptionsToString: string;

    property InputFile: string read FInputFile;
    property OutputFile: string read FOutputFile;
    // Max expected rotation angle - algo then works in range [-MaxAngle, MaxAngle]
    property MaxAngle: Double read FMaxAngle;
    // Skew threshold angle - skip deskewing if detected skew angle is in range (-MinAngle, MinAngle)
    property SkipAngle: Double read FSkipAngle;
    // Resampling filter used for rotations
    property ResamplingFilter: TResamplingFilter read FResamplingFilter;
    // Thresholding method used when converting images to binary black/white format
    property ThresholdingMethod: TThresholdingMethod read FThresholdingMethod;
    // Threshold for black/white pixel classification for explicit thresholding method
    property ThresholdLevel: Integer read FThresholdLevel;
    // Rect where to do the skew detection on the page image
    property ContentRect: TRect read FContentRect;
    // Background color for the rotated image
    property BackgroundColor: TColor32 read FBackgroundColor;
    // Forced output format (applied just before saving the output)
    property ForcedOutputFormat: TImageFormat read FForcedOutputFormat;
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

implementation

uses
  TypInfo, Imaging, ImagingTiff;

const
  TiffCompressionNames: array[TiffCompressionOptionNone..TiffCompressionOptionGroup4] of string = (
    'none', 'lzw', 'rle', 'deflate', 'jpeg', 'g4'
  );

{ TCmdLineOptions }

constructor TCmdLineOptions.Create;
begin
  FThresholdLevel := DefaultThreshold;
  FMaxAngle := DefaultMaxAngle;
  FSkipAngle := DefaultSkipAngle;
  FResamplingFilter := rfLinear;
  FThresholdingMethod := tmOtsu;
  FContentRect := Rect(0, 0, 0, 0); // whole page
  FBackgroundColor := $FF000000;
  FOutputFile := SDefaultOutputFile;
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
  Result := (InputFile <> '') and (MaxAngle > 0) and (SkipAngle >= 0) and
    ((ThresholdingMethod in [tmOtsu]) or (ThresholdingMethod = tmExplicit) and (ThresholdLevel > 0));
end;

function TCmdLineOptions.ParseCommnadLine: Boolean;
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

  function CheckParam(const Param, Value: string): Boolean;
  var
    StrArray: TDynStringArray;
    ValLower, S: string;
    TempColor: Cardinal;
    Val64: Int64;
    I, J: Integer;
  begin
    Result := True;
    ValLower := LowerCase(Value);

    if Param = '-o' then
      FOutputFile := Value
    else if Param = '-a' then
    begin
      if not TryStrToFloat(Value, FMaxAngle, FFormatSettings) then
        FErrorMessage := 'Invalid value for max angle parameter: ' + Value;
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
      if TryStrToInt64('$' + ValLower, Val64) then
      begin
        TempColor := Cardinal(Val64 and $FFFFFFFF);
        if TempColor <= $FF then
        begin
          // Just one channel given, replicate for all channels + opaque
          FBackgroundColor := Color32($FF, Byte(TempColor), Byte(TempColor), Byte(TempColor)).Color;
        end
        else if (TempColor <= $FFFFFF) and (Length(ValLower) <= 6) then
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
        Include(FOperationalFlags, ofAutoCrop);
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
      if Length(StrArray) = 4 then
      begin
        FContentRect.Left := StrToInt(StrArray[0]);
        FContentRect.Top := StrToInt(StrArray[1]);
        FContentRect.Right := StrToInt(StrArray[2]);
        FContentRect.Bottom := StrToInt(StrArray[3]);
      end;
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
      FInputFile := Param;

    Inc(I);
  end;

  if FInputFile = '' then
    FErrorMessage := 'No input file given';
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
    '  input file          = ' + InputFile + sLineBreak +
    '  output file         = ' + OutputFile + sLineBreak +
    '  max angle           = ' + FloatToStr(MaxAngle) + sLineBreak +
    '  background color    = ' + IntToHex(BackgroundColor, 8) + sLineBreak +
    '  resampling filter   = ' + FilterStr + sLineBreak +
    '  thresholding method = ' + Iff(ThresholdingMethod = tmExplicit, 'explicit', 'auto otsu') + sLineBreak +
    '  threshold level     = ' + IntToStr(ThresholdLevel) + sLineBreak +
    '  content rect        = ' + Format('%d,%d,%d,%d', [ContentRect.Left, ContentRect.Top, ContentRect.Right, ContentRect.Bottom]) + sLineBreak +
    '  output format       = ' + Iff(ForcedOutputFormat = ifUnknown, 'default', Imaging.GetFormatName(ForcedOutputFormat)) + sLineBreak +
    '  skip angle          = ' + FloatToStr(SkipAngle) + sLineBreak +
    '  oper flags          = ' + Iff(ofAutoCrop in FOperationalFlags, 'auto-crop ', '') + Iff(ofDetectOnly in FOperationalFlags, 'detect-only ', '') + sLineBreak +
    '  show info           = ' + Iff(ShowParams, 'params ', '') + Iff(ShowStats, 'stats ', '') + Iff(ShowTimings, 'timings ', '') + sLineBreak +
    '  output compression  = jpeg:' + CompJpegStr + ' tiff:' + CompTiffStr + sLineBreak;
end;

end.
