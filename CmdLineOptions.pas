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
  Utils,
  ImagingTypes,
  Imaging,
  ImagingUtility,
  ImagingTiff,
  ImageUtils;

const
  DefaultThreshold = 128;
  DefaultMaxAngle = 10;
  DefaultAngleStep = 0.1;
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

  TCmdLineOptions = class
  private
    // Option values
    FInputFileName: string;
    FOutputFileName: string;
    FMaxAngle: Double;
    FAngleStep: Double;
    FSkipAngle: Double;
    FResamplingFilter: TResamplingFilter;
    FBackgroundColor: TColor32;
    FThresholdingMethod: TThresholdingMethod;
    FThresholdLevel: Integer;
    FContentRect: TFloatRect;
    FContentMargins: TFloatRect;
    FContentSizeUnit: TSizeUnit;
    FForcedOutputFormat: TImageFormat;
    FDpiOverride: Integer;
    FJpegCompressionQuality: Integer;
    FTiffCompressionScheme: Integer;
    FOperationalFlags: TOperationalFlags;
    FShowDetectionStats: Boolean;
    FShowParams: Boolean;
    FShowTimings: Boolean;
    FSaveWorkImage: Boolean;

    // Others
    FFormatSettings: TFormatSettings;
    FErrorMessage: string;

    function GetIsValid: Boolean;
    // Reverts to default values
    procedure Reset;
    // Check a single parameter
    function CheckParam(const Param, Value: string): Boolean;
  public
    constructor Create;

    // Parses command line arguments provided as an array of strings
    function Parse(Args: TStringDynArray): Boolean; overload;
    // Calles Parse() to get options set by user using global ParamStr/ParamCount
    function ParseCommandLine: Boolean;

    function OptionsToString: string;

    // Calculates final content rectangle in pixels for given image based
    // on user input (content vs margin, units).
    function CalcContentRectForImage(const ImageBounds: TRect; Metadata: TMetadata; out FinalRect: TRect): Boolean;

    function TrySetTiffCompressionFromMetadata(Metadata: TMetadata): Boolean;

    property InputFileName: string read FInputFileName;
    property OutputFileName: string read FOutputFileName;

    // Max expected rotation angle - detection algorithm then works in range [-MaxAngle, MaxAngle]
    property MaxAngle: Double read FMaxAngle;
    // Size of a single angle step during detection (in range [-MaxAngle, MaxAngle])
    property AngleStep: Double read FAngleStep;
    // Skew threshold angle - skip deskewing if detected skew angle is in range (-MinAngle, MinAngle)
    property SkipAngle: Double read FSkipAngle;

    // Resampling filter used for rotations
    property ResamplingFilter: TResamplingFilter read FResamplingFilter;
    // Background color for the rotated image
    property BackgroundColor: TColor32 read FBackgroundColor;
    // Thresholding method used when converting images to binary black/white format
    property ThresholdingMethod: TThresholdingMethod read FThresholdingMethod;
    // Threshold for black/white pixel classification for explicit thresholding method
    property ThresholdLevel: Integer read FThresholdLevel;

    // Rect where to do the skew detection on the page image.
    // Alternatively, you can use ContentMargins to define the content rectangle.
    property ContentRect: TFloatRect read FContentRect;
    // Margins of the page image to ignore when doing skew detection.
    // This is alternative way to define content rectangle.
    property ContentMargins: TFloatRect read FContentMargins;
    // Unit of size of content or margin regions (ContentRect and ContentMargins)
    property ContentSizeUnit: TSizeUnit read FContentSizeUnit;

    // Forced output format (applied just before saving the output)
    property ForcedOutputFormat: TImageFormat read FForcedOutputFormat;
    // DPI to use when input image has no print resolution info or
    // override it when it's present
    property DpiOverride: Integer read FDpiOverride;

    // Compression quality of JPEG outputs (also embedded) in range [1, 100(best)]
    property JpegCompressionQuality: Integer read FJpegCompressionQuality;
    // Compression scheme of TIFF outputs. Values and default in imaginglib.
    property TiffCompressionScheme: Integer read FTiffCompressionScheme;

    // On/Off flags that control parts of the whole operation
    property OperationalFlags: TOperationalFlags read FOperationalFlags;

    // Show skew detection stats
    property ShowDetectionStats: Boolean read FShowDetectionStats;
    // Show current params to user (for testing etc.)
    property ShowParams: Boolean read FShowParams;
    // Show timing of processing steps to user
    property ShowTimings: Boolean read FShowTimings;
    // Save "working" thresholded image (for testing and tweaking thresholding), effectively an input
    // for skew detection.
    property SaveWorkImage: Boolean read FSaveWorkImage;

    property IsValid: Boolean read GetIsValid;
    property ErrorMessage: string read FErrorMessage;
  end;

const
  TiffCompressionOptionAsInput = TiffCompressionOptionGroup4 + 1;

function EnsureTrailingPathDelimiter(const DirPath: string): string;

implementation

uses
  TypInfo, Math;

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
  FFormatSettings := ImagingUtility.GetFormatSettingsForFloats;
  Reset;
end;

function TCmdLineOptions.GetIsValid: Boolean;
begin
  Result := (InputFileName <> '') and (MaxAngle > 0) and (SkipAngle >= 0) and
    ((ThresholdingMethod in [tmOtsu]) or (ThresholdingMethod = tmExplicit) and (ThresholdLevel > 0))
    and (FErrorMessage = '');
end;

procedure TCmdLineOptions.Reset;
begin
  FInputFileName := '';
  FOutputFileName := '';
  FMaxAngle := DefaultMaxAngle;
  FAngleStep := DefaultAngleStep;
  FSkipAngle := DefaultSkipAngle;
  FResamplingFilter := rfLinear;
  FBackgroundColor := $FF000000;
  FThresholdingMethod := tmOtsu;
  FThresholdLevel := DefaultThreshold;
  FContentRect := NullFloatRect;    // whole page
  FContentMargins := NullFloatRect; // whole page
  FContentSizeUnit := suPixels;
  FForcedOutputFormat := ifUnknown;
  FDpiOverride := 0;
  FJpegCompressionQuality := -1; // use imaginglib default
  FTiffCompressionScheme := -1;  // use imaginglib default
  FOperationalFlags := [];
  FShowDetectionStats := False;
  FShowParams := False;
  FShowTimings:= False;
  FSaveWorkImage := False;

  FErrorMessage := '';
  Assert(not IsValid);
end;

function TCmdLineOptions.CheckParam(const Param, Value: string): Boolean;
var
  StrArray: TStringDynArray;
  ValLower, S: string;
  TempColor: Cardinal;
  Val64: Int64;
  I, J, ValLength: Integer;
  ArgsOk: Boolean;

  function TryParseSizeRect(const StrArray: array of string; out SizeRect: TFloatRect): Boolean;
  var
    I, Len: Integer;
    SingleArray: array of Single;
  begin
    Result := True;
    Len := Length(StrArray);
    Assert(Len <= 4);
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
  else if Param = '-d' then
  begin
    if not TryStrToFloat(Value, FAngleStep, FFormatSettings) or
       not InRange(FAngleStep, 0.01, 5) then
      FErrorMessage := 'Invalid value for angle step parameter: ' + Value;
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
    if ValLower = 'b1' then          FForcedOutputFormat := ifBinary
    else if ValLower = 'g8' then     FForcedOutputFormat := ifGray8
    else if ValLower = 'rgb24' then  FForcedOutputFormat := ifR8G8B8
    else if ValLower = 'rgba32' then FForcedOutputFormat := ifA8R8G8B8
    else
      FErrorMessage := 'Invalid value for format parameter: ' + Value;
  end
  else if Param = '-q' then
  begin
    if ValLower = 'nearest' then      FResamplingFilter := rfNearest
    else if ValLower = 'linear' then  FResamplingFilter := rfLinear
    else if ValLower = 'cubic' then   FResamplingFilter := rfCubic
    else if ValLower = 'lanczos' then FResamplingFilter := rfLanczos
    else
      FErrorMessage := 'Invalid value for resampling filter parameter: ' + Value;
  end
  else if Param = '-g' then
  begin
    if ContainsText(Value, 'c') then
       Include(FOperationalFlags, ofCropToInput);
    if ContainsText(Value, 'd') then
       Include(FOperationalFlags, ofDetectOnly);
  end
  else if Param = '-s' then
  begin
    if ContainsText(Value, 's') then FShowDetectionStats := True;
    if ContainsText(Value, 'p') then FShowParams := True;
    if ContainsText(Value, 't') then FShowTimings := True;
    if ContainsText(Value, 'w') then FSaveWorkImage := True;
  end
  else if Param = '-r' then
  begin
    if not IsFloatRectNull(FContentMargins) then
    begin
      FErrorMessage := 'Cannot accept content rectangle when content margins are already defined';
      Exit(False);
    end;

    StrArray := SplitString(ValLower, ',');
    ValLength := Length(StrArray);

    // Allow also unit-less entry for backward compatibility
    ArgsOk := (ValLength in [4, 5]) and TryParseSizeRect(Copy(StrArray, 0, 4), FContentRect);

    if ArgsOk and (ValLength = 5) then
      ArgsOk := TryParseSizeUnit(StrArray[4], FContentSizeUnit);

    if not ArgsOk then
      FErrorMessage := 'Invalid definition of content rectangle: ' + Value;
  end
  else if Param = '-m' then
  begin
    if not IsFloatRectNull(FContentRect) then
    begin
      FErrorMessage := 'Cannot accept content margins when content rectangle is already defined';
      Exit(False);
    end;

    StrArray := SplitString(ValLower, ',');
    ValLength := Length(StrArray);

    // Allow 1, 2, 4 values + optional unit
    ArgsOk := (ValLength in [1..5]);

    if not ArgsOk then
    begin
      FErrorMessage := 'Invalid definition of content margins: ' + Value;
      Exit(False);
    end;

    case ValLength of
      1, 4:
        begin
          // Just margin values without unit
          ArgsOk := TryParseSizeRect(StrArray, FContentMargins);
        end;
      3, 5:
        begin
          // Margin values and unit
          ArgsOk := TryParseSizeUnit(StrArray[ValLength - 1], FContentSizeUnit) and
                    TryParseSizeRect(Copy(StrArray, 0, ValLength - 1), FContentMargins);
        end;
      2:
        begin
          // More complicated case: either one value + unit, or two value without unit
          if TryParseSizeUnit(StrArray[1], FContentSizeUnit) then
            ArgsOk := TryParseSizeRect(Copy(StrArray, 0, 1), FContentMargins)
          else
            ArgsOk := TryParseSizeRect(StrArray, FContentMargins);
        end;
      else
        Assert(False);
    end;

    if not ArgsOk then
      FErrorMessage := 'Invalid definition of content margins: ' + Value;
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

        if FTiffCompressionScheme <> -1 then
        begin
          FErrorMessage := 'TIFF output compression already set but received: ' + S;
          Exit(False);
        end;

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

        if FJpegCompressionQuality <> -1 then
        begin
          FErrorMessage := 'JPEG output compression already set but received: ' + S;
          Exit(False);
        end;

        if not TryStrToInt(S, FJpegCompressionQuality) or not (FJpegCompressionQuality in [1..100]) then
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

function TCmdLineOptions.Parse(Args: TStringDynArray): Boolean;
var
  I: Integer;
  Param, Value: string;
begin
  Reset;
  Result := True;
  I := 0;

  while I <= High(Args) do
  begin
    Param := Args[I];
    if Pos('-', Param) = 1 then
    begin
      if I + 1 <= High(Args) then // Check if value exists
      begin
        Value := Args[I + 1];
        Inc(I); // Consume the value argument
        if not CheckParam(Param, Value) then
          Exit(False);
      end
      else
      begin
        FErrorMessage := 'Missing value for parameter: ' + Param;
        Exit(False);
      end;
    end
    else // Not starting with '-', assume it's the input file
    begin
      if FInputFileName <> '' then
      begin
        FErrorMessage := Format('Multiple input files specified (%s, %s)', [Param, FInputFileName]);
        Exit(False);
      end;
      FInputFileName := Param;
    end;

    Inc(I);
  end;

  if FInputFileName = '' then
  begin
    FErrorMessage := 'No input file given';
    Exit(False);
  end;

  if FOutputFileName = '' then
  begin
    // No user output file name given => use prefixed input file name as default
    // (with PNG as file fomat to not introduce any new compression artifacts when
    // not explicitly asked for ).
    FOutputFileName := EnsureTrailingPathDelimiter(GetFileDir(FInputFileName)) +
      SDefaultOutputFilePrefix + ChangeFileExt(GetFileName(FInputFileName), '.' + SDefaultOutputFileExt);
  end;
end;

function TCmdLineOptions.ParseCommandLine: Boolean;
var
  Args: TStringDynArray;
  I: Integer;
begin
  SetLength(Args, ParamCount);
  for I := 0 to ParamCount - 1 do
    Args[I] := ParamStr(I + 1);
  Result := Parse(Args);
end;

function TCmdLineOptions.CalcContentRectForImage(const ImageBounds: TRect; Metadata: TMetadata;
  out FinalRect: TRect): Boolean;
var
  MarginsInPx: TRect;
begin
  Assert(not IsRectEmpty(ImageBounds));

  // We have 3 cases here:
  // 1. no content area reduction is defined
  // 3. page margins (content inside) are defined
  // 2. content rect on page is defined

  if IsFloatRectNull(ContentRect) and IsFloatRectNull(ContentMargins) then
  begin
    FinalRect := ImageBounds;
    Exit(True);
  end;

  if not IsFloatRectNull(ContentMargins) then
  begin
    // Margings to pixels
    MarginsInPx := CalcRectInPixels(ContentMargins, ContentSizeUnit,
                                    ImageBounds, Metadata);

    if IsRectNull(MarginsInPx) then
      Exit(False);

    // Remove margins from page
    FinalRect := Rect(MarginsInPx.Left,
                      MarginsInPx.Top,
                      ImageBounds.Right - MarginsInPx.Right,
                      ImageBounds.Bottom - MarginsInPx.Bottom);
  end
  else if not IsFloatRectNull(ContentRect) then
  begin
    FinalRect := CalcRectInPixels(ContentRect, ContentSizeUnit,
                                  ImageBounds, Metadata);
  end;

  if IsRectEmpty(FinalRect) or not IntersectRect(FinalRect, FinalRect, ImageBounds) then
    Exit(False);

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

  function RectToStr(const R: TFloatRect): string;
  begin
    Result := Format('%n,%n,%n,%n %s', [R.Left, R.Top, R.Right, R.Bottom,
      SizeUnitTokens[ContentSizeUnit]], FloatFmtSettings);
  end;

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
    '  background color    = ' + IntToHex(BackgroundColor, 8) + sLineBreak +
    '  resampling filter   = ' + FilterStr + sLineBreak +
    '  max angle           = ' + FloatToStr(MaxAngle) + sLineBreak +
    '  angle step          = ' + FloatToStr(AngleStep) + sLineBreak +
    '  thresholding method = ' + Iff(ThresholdingMethod = tmExplicit, 'explicit', 'auto otsu') + sLineBreak +
    '  threshold level     = ' + IntToStr(ThresholdLevel) + sLineBreak +
    '  content rect        = ' + RectToStr(ContentRect) + sLineBreak +
    '  content margins     = ' + RectToStr(ContentMargins) + sLineBreak +
    '  output format       = ' + Iff(ForcedOutputFormat = ifUnknown, 'default', Imaging.GetFormatName(ForcedOutputFormat)) + sLineBreak +
    '  skip angle          = ' + FloatToStr(SkipAngle) + sLineBreak +
    '  dpi override        = ' + IntToStr(DpiOverride) + sLineBreak +
    '  oper flags          = ' + Iff(ofCropToInput in FOperationalFlags, 'crop-to-input ', '')
                               + Iff(ofDetectOnly in FOperationalFlags, 'detect-only ', '') + sLineBreak +
    '  info flags          = ' + Iff(ShowParams, 'params ', '') + Iff(ShowDetectionStats, 'detection-stats ', '') +
      Iff(ShowTimings, 'timings ', '') + Iff(SaveWorkImage, 'save-work-image ', '') + sLineBreak +
    '  output compression  = jpeg:' + CompJpegStr + ' tiff:' + CompTiffStr + sLineBreak;
end;

initialization
  FloatFmtSettings := SysUtils.FormatSettings;
  FloatFmtSettings.ThousandSeparator := #0;
  FloatFmtSettings.DecimalSeparator  := '.';

end.
