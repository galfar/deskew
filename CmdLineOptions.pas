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
  MaxAllowedAngle = 90;

type
  TThresholdingMethod = (
    // Use explicit threshold [0..255]
    tmExplicit,
    // Use adaptive thresholding: Otsu's method
    tmOtsu
  );

  TOperationalFlag = (
    // Output image keeps the size of the input, corners of the rotated image are cropped
    ofCropToInput,
    // Only detect the skew angle, no rotation and no output file
    ofDetectOnly,
    // Rotate by the detected angle always: also when it is not reliable
    // or smaller than the skip angle
    ofForceRotation
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
    FAutoContentRect: Boolean;
    FForcedOutputFormat: TImageFormat;
    FDpiOverride: Integer;
    FJpegCompressionQuality: Integer;
    FTiffCompressionScheme: Integer;
    FOperationalFlags: TOperationalFlags;
    FShowDetectionStats: Boolean;
    FShowParams: Boolean;
    FShowTimings: Boolean;
    FSaveWorkImage: Boolean;
    FJsonToStdOut: Boolean;
    FJsonReportFile: Boolean;
    FHelpRequested: Boolean;

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
    function Parse(const Args: array of string): Boolean;
    // Calles Parse() to get options set by user using global ParamStr/ParamCount
    function ParseCommandLine: Boolean;

    function OptionsToString: string;
    // Writes the options as members of the currently open JSON object
    procedure WriteToJson(Json: TJsonWriter);
    // Name of the file for JSON report (when JsonReportFile is set): output file
    // name with added extension. It's the same in detect only mode where
    // no output file is written.
    function JsonReportFileName: string;

    // Calculates final content rectangle in pixels for given image based
    // on user input (content vs margin, units).
    function CalcContentRectForImage(const ImageBounds: TRect; Metadata: TMetadata; out FinalRect: TRect): Boolean;

    // Resolves the "as input" TIFF compression options (input, input-lossless) to a real
    // scheme using the compression of the loaded input TIFF (stored in Metadata).
    // Returns False and resets the scheme to default when the input scheme is unknown.
    // With input-lossless, lossy input scheme (JPEG) is replaced by LZW (ReplacedLossy = True).
    function TrySetTiffCompressionFromMetadata(Metadata: TMetadata; out ReplacedLossy: Boolean): Boolean;

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
    // Content rectangle is not given by the user but it is found automatically
    // in each page image.
    property AutoContentRect: Boolean read FAutoContentRect;

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
    // Write JSON report to standard output. No other text is written there then.
    property JsonToStdOut: Boolean read FJsonToStdOut;
    // Write JSON report to a file next to the output file
    property JsonReportFile: Boolean read FJsonReportFile;
    // User only wants to see the usage/help, other options are not checked then
    property HelpRequested: Boolean read FHelpRequested;

    property IsValid: Boolean read GetIsValid;
    property ErrorMessage: string read FErrorMessage;
  end;

const
  // Same compression as the input TIFF (including lossy JPEG)
  TiffCompressionOptionAsInput = TiffCompressionOptionGroup4 + 1;
  // Same compression as the input TIFF but never lossy (JPEG input => LZW)
  TiffCompressionOptionAsInputLossless = TiffCompressionOptionAsInput + 1;

function EnsureTrailingPathDelimiter(const DirPath: string): string;

implementation

uses
  TypInfo, Math;

const
  TiffCompressionNames: array[TiffCompressionOptionNone..TiffCompressionOptionAsInputLossless] of string = (
    'none', 'lzw', 'rle', 'deflate', 'jpeg', 'g4', 'input', 'input-lossless'
  );
  SizeUnitTokens: array[TSizeUnit] of string = ('px', '%', 'mm', 'cm', 'in');
  SAutoContentRect = 'auto';
  // All the parameters that take a value
  KnownParams: array[0..13] of string = (
    '-o', '-a', '-d', '-l', '-t', '-b', '-f', '-q', '-g', '-s', '-r', '-m', '-p', '-c');

  SDefaultOutputFilePrefix = 'deskewed-';
  SDefaultOutputFileExt = 'png';
  SJsonReportFileExt = '.json';

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
  FAutoContentRect := False;
  FForcedOutputFormat := ifUnknown;
  FDpiOverride := 0;
  FJpegCompressionQuality := -1; // use imaginglib default
  FTiffCompressionScheme := -1;  // use imaginglib default
  FOperationalFlags := [];
  FShowDetectionStats := False;
  FShowParams := False;
  FShowTimings:= False;
  FSaveWorkImage := False;
  FJsonToStdOut := False;
  FJsonReportFile := False;
  FHelpRequested := False;

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

  // Checks that the value consists only of the known flag letters
  function CheckFlags(const AllowedFlags, FlagKind: string): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 1 to Length(ValLower) do
      if Pos(ValLower[I], AllowedFlags) = 0 then
      begin
        FErrorMessage := Format('Unknown %s flag "%s" in: %s', [FlagKind, ValLower[I], Value]);
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
    if not TryStrToFloat(Value, FMaxAngle, FFormatSettings) or
       (FMaxAngle <= 0) or (FMaxAngle > MaxAllowedAngle) then
      FErrorMessage := Format('Invalid value for max angle parameter (number of degrees ' +
        'greater than 0, up to %d): %s', [MaxAllowedAngle, Value]);
  end
  else if Param = '-d' then
  begin
    if not TryStrToFloat(Value, FAngleStep, FFormatSettings) or
       not InRange(FAngleStep, 0.01, 5) then
      FErrorMessage := 'Invalid value for angle step parameter (number of degrees ' +
        'in range 0.01 to 5): ' + Value;
  end
  else if Param = '-l' then
  begin
    if not TryStrToFloat(Value, FSkipAngle, FFormatSettings) or
       (FSkipAngle < 0) or (FSkipAngle > MaxAllowedAngle) then
      FErrorMessage := Format('Invalid value for skip angle parameter (number of degrees ' +
        'in range 0 to %d): %s', [MaxAllowedAngle, Value]);
  end
  else if Param = '-t' then
  begin
    if ValLower = 'a' then
      FThresholdingMethod := tmOtsu
    else
    begin
      FThresholdingMethod := tmExplicit;
      if not TryStrToInt(Value, FThresholdLevel) or not InRange(FThresholdLevel, 1, 255) then
        FErrorMessage := 'Invalid value for threshold parameter ("a" for auto or ' +
          'level in range 1 to 255): ' + Value;
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
    if not CheckFlags('cdf', 'operational') then
      Exit(False);
    if ContainsText(Value, 'c') then
       Include(FOperationalFlags, ofCropToInput);
    if ContainsText(Value, 'd') then
       Include(FOperationalFlags, ofDetectOnly);
    if ContainsText(Value, 'f') then
       Include(FOperationalFlags, ofForceRotation);
  end
  else if Param = '-s' then
  begin
    if not CheckFlags('sptwjr', 'info') then
      Exit(False);
    if ContainsText(Value, 's') then FShowDetectionStats := True;
    if ContainsText(Value, 'p') then FShowParams := True;
    if ContainsText(Value, 't') then FShowTimings := True;
    if ContainsText(Value, 'w') then FSaveWorkImage := True;
    if ContainsText(Value, 'j') then FJsonToStdOut := True;
    if ContainsText(Value, 'r') then FJsonReportFile := True;
  end
  else if Param = '-r' then
  begin
    if not IsFloatRectNull(FContentMargins) then
    begin
      FErrorMessage := 'Cannot accept content rectangle when content margins are already defined';
      Exit(False);
    end;

    if ValLower = SAutoContentRect then
    begin
      // Content rectangle will be detected for each image
      FAutoContentRect := True;
      FContentRect := NullFloatRect;
      Exit(True);
    end;

    FAutoContentRect := False;
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
    if FAutoContentRect or not IsFloatRectNull(FContentRect) then
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
        FErrorMessage := 'Invalid output file format spec: ' + S;
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

function TCmdLineOptions.Parse(const Args: array of string): Boolean;
var
  I: Integer;
  Param, Value: string;

  function IsKnownParam(const Param: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(KnownParams) to High(KnownParams) do
      if Param = KnownParams[I] then
        Exit(True);
  end;

begin
  Reset;
  Result := True;

  // Where to report is found out first: errors in the other
  // parameters must be reported the way the user asked for too.
  for I := 0 to High(Args) - 1 do
    if Args[I] = '-s' then
    begin
      if ContainsText(Args[I + 1], 'j') then FJsonToStdOut := True;
      if ContainsText(Args[I + 1], 'r') then FJsonReportFile := True;
    end;

  I := 0;
  while I <= High(Args) do
  begin
    Param := Args[I];
    if (Param = '-h') or (Param = '--help') or (Param = '-?') then
    begin
      // Nothing else matters now
      FHelpRequested := True;
      Exit(True);
    end
    else if Pos('-', Param) = 1 then
    begin
      // Unknown parameter is reported as such also when it is the last
      // argument and its value is missing
      if not IsKnownParam(Param) then
      begin
        FErrorMessage := 'Unknown parameter: ' + Param;
        Exit(False);
      end;

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

function TCmdLineOptions.TrySetTiffCompressionFromMetadata(Metadata: TMetadata; out ReplacedLossy: Boolean): Boolean;
var
  CompName: string;
  LosslessOnly: Boolean;
begin
  Result := True;
  ReplacedLossy := False;
  Assert(FTiffCompressionScheme in [TiffCompressionOptionAsInput, TiffCompressionOptionAsInputLossless]);
  LosslessOnly := FTiffCompressionScheme = TiffCompressionOptionAsInputLossless;
  if not Metadata.HasMetaItem(SMetaTiffCompressionName) then
  begin
    FTiffCompressionScheme := -1;
    Exit(False);
  end;

  CompName := Metadata.MetaItems[SMetaTiffCompressionName];

  // Names from CompressionName in ImagingTiff
  if CompName = 'None' then
    FTiffCompressionScheme := TiffCompressionOptionNone
  else if CompName = 'LZW' then
    FTiffCompressionScheme := TiffCompressionOptionLzw
  else if CompName = 'JPEG' then
    FTiffCompressionScheme := TiffCompressionOptionJpeg
  else if CompName = 'Deflate' then
    FTiffCompressionScheme := TiffCompressionOptionDeflate
  else if CompName = 'Packbits RLE' then
    FTiffCompressionScheme := TiffCompressionOptionPackbitsRle
  else if StrUtils.MatchStr(CompName, ['CCITT Group 4 Fax', 'CCITT']) then
    FTiffCompressionScheme := TiffCompressionOptionGroup4;

  if FTiffCompressionScheme in [TiffCompressionOptionAsInput, TiffCompressionOptionAsInputLossless] then
  begin
    // Input compression scheme is unknown or not supported for output (e.g. old-style JPEG)
    FTiffCompressionScheme := -1;
    Exit(False);
  end;

  if LosslessOnly and (FTiffCompressionScheme = TiffCompressionOptionJpeg) then
  begin
    // Recompressing already lossy-compressed rotated image would degrade it even more
    FTiffCompressionScheme := TiffCompressionOptionLzw;
    ReplacedLossy := True;
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
    '  content rect        = ' + Iff(AutoContentRect, SAutoContentRect, RectToStr(ContentRect)) + sLineBreak +
    '  content margins     = ' + RectToStr(ContentMargins) + sLineBreak +
    '  output format       = ' + Iff(ForcedOutputFormat = ifUnknown, 'default', Imaging.GetFormatName(ForcedOutputFormat)) + sLineBreak +
    '  skip angle          = ' + FloatToStr(SkipAngle) + sLineBreak +
    '  dpi override        = ' + IntToStr(DpiOverride) + sLineBreak +
    '  oper flags          = ' + Iff(ofCropToInput in FOperationalFlags, 'crop-to-input ', '')
                               + Iff(ofDetectOnly in FOperationalFlags, 'detect-only ', '')
                               + Iff(ofForceRotation in FOperationalFlags, 'force-rotation ', '') + sLineBreak +
    '  info flags          = ' + Iff(ShowParams, 'params ', '') + Iff(ShowDetectionStats, 'detection-stats ', '') +
      Iff(ShowTimings, 'timings ', '') + Iff(SaveWorkImage, 'save-work-image ', '') +
      Iff(JsonToStdOut, 'json-stdout ', '') + Iff(JsonReportFile, 'json-file ', '') + sLineBreak +
    '  output file specs   = jpeg quality:' + CompJpegStr + ' tiff compression:' + CompTiffStr + sLineBreak;
end;

procedure TCmdLineOptions.WriteToJson(Json: TJsonWriter);
var
  FilterStr: string;
begin
  FilterStr := LowerCase(Copy(TypInfo.GetEnumName(TypeInfo(TResamplingFilter), Integer(FResamplingFilter)), 3));

  Json.AddFloat('maxAngle', MaxAngle);
  Json.AddFloat('angleStep', AngleStep);
  Json.AddFloat('skipAngle', SkipAngle);

  if ThresholdingMethod = tmExplicit then
  begin
    Json.AddString('thresholding', 'explicit');
    Json.AddInt('thresholdLevel', ThresholdLevel);
  end
  else
  begin
    Json.AddString('thresholding', 'auto');
    Json.AddNull('thresholdLevel');
  end;

  // Content area as given by the user, in the units given by the user
  if AutoContentRect then
    Json.AddString('contentArea', 'auto')
  else if not IsFloatRectNull(ContentMargins) then
    Json.AddString('contentArea', 'margins')
  else if not IsFloatRectNull(ContentRect) then
    Json.AddString('contentArea', 'rect')
  else
    Json.AddString('contentArea', 'page');

  if not IsFloatRectNull(ContentMargins) then
    Json.AddFloatRect('contentAreaValues', ContentMargins.Left, ContentMargins.Top,
      ContentMargins.Right, ContentMargins.Bottom)
  else if not AutoContentRect and not IsFloatRectNull(ContentRect) then
    Json.AddFloatRect('contentAreaValues', ContentRect.Left, ContentRect.Top,
      ContentRect.Right, ContentRect.Bottom)
  else
    Json.AddNull('contentAreaValues');
  Json.AddString('contentAreaUnit', SizeUnitTokens[ContentSizeUnit]);

  if DpiOverride > 0 then
    Json.AddInt('dpiOverride', DpiOverride)
  else
    Json.AddNull('dpiOverride');

  Json.AddString('resamplingFilter', FilterStr);
  Json.AddString('backgroundColor', IntToHex(BackgroundColor, 8));

  if ForcedOutputFormat <> ifUnknown then
    Json.AddString('outputFormat', Imaging.GetFormatName(ForcedOutputFormat))
  else
    Json.AddNull('outputFormat');
  if JpegCompressionQuality <> -1 then
    Json.AddInt('jpegQuality', JpegCompressionQuality)
  else
    Json.AddNull('jpegQuality');
  if TiffCompressionScheme >= 0 then
    Json.AddString('tiffCompression', TiffCompressionNames[TiffCompressionScheme])
  else
    Json.AddNull('tiffCompression');

  Json.AddBool('detectOnly', ofDetectOnly in OperationalFlags);
  Json.AddBool('forceRotation', ofForceRotation in OperationalFlags);
  Json.AddBool('cropToInput', ofCropToInput in OperationalFlags);
end;

function TCmdLineOptions.JsonReportFileName: string;
begin
  Result := '';
  if OutputFileName <> '' then
    Result := OutputFileName + SJsonReportFileExt;
end;

initialization
  FloatFmtSettings := SysUtils.FormatSettings;
  FloatFmtSettings.ThousandSeparator := #0;
  FloatFmtSettings.DecimalSeparator  := '.';

end.
