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
unit MainUnit;

{$I ImagingOptions.inc}

interface

procedure RunDeskew;

implementation

uses
  Types,
  SysUtils,
  Classes,
  ImagingTypes,
  Imaging,
  ImagingClasses,
  ImagingFormats,
  ImagingUtility,
  ImagingExtFileFormats,
  ImagingNetworkGraphics,
  ImagingTiff,
  // Project units
  CmdLineOptions,
  Utils,
  ImageUtils,
  RotationDetector;

const
  SAppVersion = '1.33';
  SAppTitle = 'Deskew ' + SAppVersion + ' (2025-06-02, ' + SBuildTarget + ', ' + SBuildCompiler
    {$IFDEF DEBUG} + ', DEBUG'{$ENDIF}
    + ') by Marek Mauder';
  SAppHome = 'https://github.com/galfar/deskew' + sLineBreak +
             'https://galfar.vevb.net/deskew';

  SDetectionOutcomes: array[TSkewDetectionOutcome] of string = (
    'detected', 'no content', 'low confidence', 'at range limit');
  SDetectionOutcomeReasons: array[TSkewDetectionOutcome] of string = (
    '',
    'no content to detect the skew from (blank page?)',
    'no dominant direction of text lines found within the max angle',
    'skew seems to be larger than the max angle (it can be raised with "-a")');

  // Imaging cannot save PNG (and MNG, JNG) images with more pixel data than this yet.
  // It ends with out of memory error, or even with a crash when there is over 4 GiB of data.
  // Other file formats (TIFF, QOI, JPEG, BMP) were tested with more than 4 GiB.
  MaxPngPixelDataSize = Int64(2000) * 1000 * 1000;

type
  // Steps of the processing that are timed
  TTimedStep = (tsLoad, tsThresholding, tsContentDetection, tsSkewDetection, tsRotation, tsSave);
  // What happened with the output file
  TOutputWritten = (
    // No output file: detect only mode or an error
    owNone,
    // Image was changed and saved in the output file format
    owSaved,
    // Image was not changed, input file was copied as it is
    owCopied
  );

  // What is known about the current run, collected along the way for JSON report
  TDeskewReport = record
    StartTime: Int64;
    ParamsValid: Boolean;
    InputLoaded: Boolean;
    InputWidth, InputHeight: Integer;
    InputFormat: string;
    // Print resolution of the input (after the override), zero when not known
    DpiX, DpiY: Double;
    DetectionDone: Boolean;
    Threshold: Integer;
    HasContent: Boolean;
    // Content area found in the image and the area where the skew was detected
    ContentRect, DetectionRect: TRect;
    SkewAngle: Double;
    Stats: TCalcSkewAngleStats;
    Rotated: Boolean;
    OutputWritten: TOutputWritten;
    OutputWidth, OutputHeight: Integer;
    OutputFormat: string;
    // In microseconds, negative for steps that did not run
    Timings: array[TTimedStep] of Int64;
    ErrorMessage: string;
  end;

  PTextFile = ^Text;

const
  STimedSteps: array[TTimedStep] of string = (
    'Load input file', 'Auto thresholding', 'Content detection', 'Skew detection',
    'Rotate image', 'Save output file');

  // Names used in JSON report
  SJsonTimedSteps: array[TTimedStep] of string = (
    'load', 'thresholding', 'contentDetection', 'skewDetection', 'rotation', 'save');
  SJsonDetectionOutcomes: array[TSkewDetectionOutcome] of string = (
    'detected', 'noContent', 'lowConfidence', 'atRangeLimit');
  SJsonOutputWritten: array[TOutputWritten] of string = ('none', 'saved', 'copied');

var
  // Program options
  Options: TCmdLineOptions;
  // Input and output image
  InputImage, OutputImage: TSingleImage;
  // Where all the text for the user is written to. It is standard output unless
  // it is taken by JSON report: nothing but the report can be written there then.
  // Never use WriteLn without it.
  Log: PTextFile;
  Report: TDeskewReport;

procedure WriteUsage;
var
  InFilter, OutFilter, Ext: string;
  I, Count: Integer;
  Fmt: TImageFileFormat;
begin
  InFilter := '';
  OutFilter := '';

  WriteLn(Log^, 'Usage: deskew [-o output] [-a angle] [-b color] [..] input');
  WriteLn(Log^, '    input:         Input image file');
  WriteLn(Log^, '    -h:            Show this help');

  WriteLn(Log^);
  WriteLn(Log^, '  Options:');
  WriteLn(Log^, '    -o output:     Output image file name (default: deskewed-<input name>.png');
  WriteLn(Log^, '                   next to the input file)');
  WriteLn(Log^, '    -b color:      Background color in hex format RRGGBB|LL|AARRGGBB (default: black)');
  WriteLn(Log^, '    -q filter:     Resampling filter used for rotations (default: linear,');
  WriteLn(Log^, '                   values: nearest|linear|cubic|lanczos)');
  WriteLn(Log^, '    -a angle:      Maximal expected skew angle (both directions) in degrees (default: 10)');

  WriteLn(Log^);
  WriteLn(Log^, '  Advanced options:');
  WriteLn(Log^, '    -d angle:      Angle step during detection in degrees (default: 0.1)');
  WriteLn(Log^, '    -t a|level:    Auto threshold or level in 1..255, darker pixels are');
  WriteLn(Log^, '                   considered to be text (default: auto)');
  WriteLn(Log^, '    -m margins:    Skew detection only outside page margins:');
  WriteLn(Log^, '                   L,T,R,B[,unit] or H,V[,unit] or A[,unit] (default: whole page)');
  WriteLn(Log^, '                   unit: px|%|mm|cm|in (default: px)');
  WriteLn(Log^, '    -r rect:       Skew detection only in content rectangle:');
  WriteLn(Log^, '                   L,T,R,B[,unit] or auto (default: whole page)');
  WriteLn(Log^, '                   unit: px|%|mm|cm|in (default: px)');
  WriteLn(Log^, '                   auto: content area is found automatically for each image');
  WriteLn(Log^, '    -f format:     Force output pixel format (values: b1|g8|rgb24|rgba32)');
  WriteLn(Log^, '    -p dpi:        Print resolution of the image in DPI, overrides the one stored');
  WriteLn(Log^, '                   in the input file (default: use the stored one)');
  WriteLn(Log^, '    -l angle:      Skip deskewing step if skew angle is smaller, in degrees (default: 0.01)');
  WriteLn(Log^, '    -g flags:      Operational flags (any combination of):');
  WriteLn(Log^, '                   c - crop to input size, d - detect only (no output to file),');
  WriteLn(Log^, '                   f - force rotation: also when the skew angle is not reliable');
  WriteLn(Log^, '                   or smaller than the skip angle (-l)');
  WriteLn(Log^, '    -s info:       Info dump (any combination of):');
  WriteLn(Log^, '                   s - skew detection stats, p - program parameters, t - timings,');
  WriteLn(Log^, '                   w - save work/thresholded image,');
  WriteLn(Log^, '                   j - JSON report to standard output (and nothing else there),');
  WriteLn(Log^, '                   r - JSON report to file <output file name>.json');
  WriteLn(Log^, '    -c specs:      Output file format specs (compression etc.). Several specs');
  WriteLn(Log^, '                   can be defined - delimited by commas. Supported specs:');
  WriteLn(Log^, '                   jXX - JPEG compression quality, XX in range [1,100(best)]');
  WriteLn(Log^, '                   tSCHEME - TIFF compression scheme: none|lzw|rle|deflate|jpeg|g4|input|input-lossless');
  WriteLn(Log^);
  WriteLn(Log^, '  Skew angle is positive when the content is turned clockwise (text lines go down');
  WriteLn(Log^, '  to the right), image is then rotated counterclockwise by this angle to fix it.');

  Count := GetFileFormatCount;
  for I := 0 to Count - 1 do
  begin
    Fmt := GetFileFormatAtIndex(I);
    Ext := Fmt.Extensions[0];

    // PAM and PFM still work but are not listed (not scan formats)
    // and Imaging cannot leave it out without the other PNM formats
    if SameText(Ext, 'pam') or SameText(Ext, 'pfm') then
      Continue;

    // More common monikers
    if SameText(Ext, 'jpg') then
      Ext := 'JPEG';
    if SameText(Ext, 'tif') then
      Ext := 'TIFF';

    // Not every format can be both loaded and saved (WebP is input only)
    if Fmt.CanLoad then
      InFilter := InFilter + Iff(InFilter <> '', ', ', '') + Ext;
    if Fmt.CanSave then
      OutFilter := OutFilter + Iff(OutFilter <> '', ', ', '') + Ext;
  end;

  WriteLn(Log^);
  WriteLn(Log^, '  Supported file formats');
  WriteLn(Log^, '    Input:  ', UpperCase(InFilter));
  WriteLn(Log^, '    Output: ', UpperCase(OutFilter));
end;

procedure ReportBadInput(const Msg: string; ShowUsage: Boolean = True);
begin
  if ShowUsage then
  begin
    WriteUsage;
    WriteLn(Log^);
  end;

  // Error goes last so it does not get lost above the usage text
  WriteLn(Log^, 'ERROR: ' + Msg);
  if Options.ErrorMessage <> '' then
    WriteLn(Log^, Options.ErrorMessage);

  Report.ErrorMessage := Msg;
  if Options.ErrorMessage <> '' then
    Report.ErrorMessage := Msg + ' ' + Options.ErrorMessage;
  ExitCode := 1;
end;

function FormatNiceNumber(const X: Int64; Width : Integer = 16): string;
var
  FmtStr: string;
begin
  if Width = 0 then
    FmtStr := '%.0n'
  else
    FmtStr := '%' + IntToStr(Width) + '.0n';
  Result := Format(FmtStr, [X * 1.0], GetFormatSettingsForFloats);
end;

var
  Time: Int64;

procedure WriteTiming(Step: TTimedStep);
var
  Elapsed: Int64;
begin
  Elapsed := GetTimeMicroseconds - Time;
  Report.Timings[Step] := Elapsed;
  if Options.ShowTimings then
    WriteLn(Log^, STimedSteps[Step] + ' - time taken: ' + FormatNiceNumber(Elapsed, 0) + ' us');
end;

procedure InitReport;
var
  Step: TTimedStep;
begin
  Report.StartTime := GetTimeMicroseconds;
  Report.ParamsValid := False;
  Report.InputLoaded := False;
  Report.InputFormat := '';
  Report.DpiX := 0;
  Report.DpiY := 0;
  Report.DetectionDone := False;
  Report.Rotated := False;
  Report.OutputWritten := owNone;
  Report.OutputFormat := '';
  Report.ErrorMessage := '';
  for Step := Low(TTimedStep) to High(TTimedStep) do
    Report.Timings[Step] := -1;
end;

function BuildJsonReport: string;
var
  Json: TJsonWriter;
  Step: TTimedStep;
begin
  Json := TJsonWriter.Create;
  try
    Json.BeginObject;

    Json.BeginObject('deskew');
    Json.AddString('version', SAppVersion);
    Json.AddString('target', SBuildTarget + ', ' + SBuildCompiler);
    Json.EndObject;

    // There is nothing more to tell when Deskew did not even get proper parameters
    if Report.ParamsValid then
    begin
      Json.BeginObject('input');
      Json.AddString('file', ExpandFileName(Options.InputFileName));
      if Report.InputLoaded then
      begin
        Json.AddInt('width', Report.InputWidth);
        Json.AddInt('height', Report.InputHeight);
        Json.AddString('format', Report.InputFormat);
      end
      else
      begin
        Json.AddNull('width');
        Json.AddNull('height');
        Json.AddNull('format');
      end;
      if (Report.DpiX > 0) and (Report.DpiY > 0) then
      begin
        Json.AddFloat('dpiX', Report.DpiX, 3);
        Json.AddFloat('dpiY', Report.DpiY, 3);
      end
      else
      begin
        Json.AddNull('dpiX');
        Json.AddNull('dpiY');
      end;
      Json.EndObject;

      Json.BeginObject('parameters');
      Options.WriteToJson(Json);
      Json.EndObject;

      // Detection part is there only when Deskew got that far
      if Report.DetectionDone then
      begin
        Json.BeginObject('detection');
        Json.AddFloat('skewAngle', Report.SkewAngle);
        Json.AddFloat('confidence', Report.Stats.Confidence, 3);
        Json.AddString('outcome', SJsonDetectionOutcomes[Report.Stats.Outcome]);
        Json.AddBool('reliable', Report.Stats.Outcome = sdoDetected);
        Json.AddInt('threshold', Report.Threshold);
        if Report.HasContent then
          Json.AddRect('contentRect', Report.ContentRect)
        else
          Json.AddNull('contentRect');
        Json.AddRect('detectionRect', Report.DetectionRect);
        Json.AddFloat('peakAngle', Report.Stats.PeakAngle);
        Json.AddFloat('sharpness', Report.Stats.Sharpness, 3);
        Json.AddInt('linesAtPeak', Report.Stats.LinesAtPeak);
        Json.AddInt('bestCount', Report.Stats.BestCount);
        Json.AddInt('pixelCount', Report.Stats.PixelCount);
        Json.AddInt('testedPixels', Report.Stats.TestedPixels);
        Json.AddInt('accumulatorSize', Report.Stats.AccumulatorSize);
        Json.AddInt('accumulatedCounts', Report.Stats.AccumulatedCounts);
        Json.EndObject;
      end;

      Json.AddBool('rotated', Report.Rotated);

      Json.BeginObject('output');
      if ofDetectOnly in Options.OperationalFlags then
        Json.AddNull('file')
      else
        Json.AddString('file', ExpandFileName(Options.OutputFileName));
      Json.AddString('written', SJsonOutputWritten[Report.OutputWritten]);
      if Report.OutputWritten <> owNone then
      begin
        Json.AddInt('width', Report.OutputWidth);
        Json.AddInt('height', Report.OutputHeight);
        Json.AddString('format', Report.OutputFormat);
      end
      else
      begin
        Json.AddNull('width');
        Json.AddNull('height');
        Json.AddNull('format');
      end;
      Json.EndObject;

      Json.BeginObject('timingsMs');
      for Step := Low(TTimedStep) to High(TTimedStep) do
        if Report.Timings[Step] >= 0 then
          Json.AddFloat(SJsonTimedSteps[Step], Report.Timings[Step] / 1000, 3)
        else
          Json.AddNull(SJsonTimedSteps[Step]);
      Json.AddFloat('total', (GetTimeMicroseconds - Report.StartTime) / 1000, 3);
      Json.EndObject;
    end;

    if Report.ErrorMessage <> '' then
      Json.AddString('error', Report.ErrorMessage)
    else
      Json.AddNull('error');

    Json.EndObject;
    Result := Json.GetText;
  finally
    Json.Free;
  end;
end;

procedure EnsureOutputLocation(const FileName: string);
var
  Dir, Path: string;
begin
  Path := ExpandFileName(FileName);
  Dir := GetFileDir(Path);
  if Dir <> '' then
    ForceDirectories(Dir);
end;

// Writes JSON report where the user wants it: standard output, file, or both.
// Both get the same text.
procedure WriteReports;
var
  FileName, ReportText: string;
  FileData: AnsiString;
  Stream: TFileStream;
begin
  if Options.HelpRequested or not (Options.JsonToStdOut or Options.JsonReportFile) then
    Exit;

  ReportText := BuildJsonReport;

  // File name is not known when there is something wrong with the parameters
  FileName := Options.JsonReportFileName;
  if Options.JsonReportFile and (FileName <> '') then
  try
    FileData := AnsiString(ReportText + sLineBreak);
    EnsureOutputLocation(FileName);
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      Stream.WriteBuffer(FileData[1], Length(FileData));
    finally
      Stream.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(Log^);
      WriteLn(Log^, 'Could not write report file ', FileName, ': ', E.Message);
      ExitCode := 1;
      // The other report should know
      if Report.ErrorMessage = '' then
      begin
        Report.ErrorMessage := 'Could not write report file: ' + E.Message;
        ReportText := BuildJsonReport;
      end;
    end;
  end;

  if Options.JsonToStdOut then
  begin
    WriteLn(Output, ReportText);
    Flush(Output);
  end;
end;

function DoDeskew: Boolean;
var
  SkewAngle: Double;
  Threshold: Integer;
  ContentRect, FoundContentRect: TRect;
  Stats: TCalcSkewAngleStats;
  HasContent, ForceRotation: Boolean;

  procedure WriteDetectionStats;
  begin
    WriteLn(Log^, 'Skew detection stats:');
    WriteLn(Log^, '  content area:       ', Iff(HasContent, RectToStr(FoundContentRect), 'none'):16);
    WriteLn(Log^, '  pixel count:        ', FormatNiceNumber(Stats.PixelCount));
    WriteLn(Log^, '  tested pixels:      ', FormatNiceNumber(Stats.TestedPixels));
    WriteLn(Log^, '  accumulator size:   ', FormatNiceNumber(Stats.AccumulatorSize));
    WriteLn(Log^, '  accumulated counts: ', FormatNiceNumber(Stats.AccumulatedCounts));
    WriteLn(Log^, '  best count:         ', FormatNiceNumber(Stats.BestCount));
    WriteLn(Log^, '  peak angle:         ', Stats.PeakAngle:16:3);
    WriteLn(Log^, '  lines at peak:      ', FormatNiceNumber(Stats.LinesAtPeak));
    WriteLn(Log^, '  sharpness:          ', Stats.Sharpness:16:3);
    WriteLn(Log^, '  confidence:         ', Stats.Confidence:16:3);
    WriteLn(Log^, '  outcome:            ', SDetectionOutcomes[Stats.Outcome]:16);
  end;

  procedure EnsurePixelFormatForRotation;
  begin
    // Rotation is optimized for Gray8, RGB24, and ARGB32 formats at this time
    if not (OutputImage.Format in ImageUtils.SupportedRotationFormats) then
    begin
      if OutputImage.Format = ifIndex8 then
      begin
        if PaletteHasAlpha(OutputImage.Palette, OutputImage.PaletteEntries) then
          OutputImage.Format := ifA8R8G8B8
        else if PaletteIsGrayScale(OutputImage.Palette, OutputImage.PaletteEntries) then
          OutputImage.Format := ifGray8
        else
          OutputImage.Format := ifR8G8B8;
      end
      else if OutputImage.FormatInfo.HasAlphaChannel then
        OutputImage.Format := ifA8R8G8B8
      else if (OutputImage.Format = ifBinary) or OutputImage.FormatInfo.HasGrayChannel then
        OutputImage.Format := ifGray8
      else
        OutputImage.Format := ifR8G8B8;
    end;

    if (Options.BackgroundColor and $FF000000) <> $FF000000 then
    begin
      // User explicitly requested some alpha in background color
      OutputImage.Format := ifA8R8G8B8;
    end
    else if (OutputImage.Format = ifGray8) and not (
      (GetRedValue(Options.BackgroundColor) = GetGreenValue(Options.BackgroundColor)) and
      (GetBlueValue(Options.BackgroundColor) = GetGreenValue(Options.BackgroundColor))) then
    begin
      // Some non-grayscale background for gray image was requested
      OutputImage.Format := ifR8G8B8;
    end;
  end;

  function ChangeOutputFormatIfNeeded: Boolean;
  var
    ReplacedLossy: Boolean;
  begin
    Result := False;

    if (Options.ForcedOutputFormat <> ifUnknown) and (OutputImage.Format <> Options.ForcedOutputFormat) then
    begin
      // Force output format. For example Deskew won't automatically
      // save image as binary if the input was binary since it
      // might degrade the output a lot (rotation adds a lot of colors to image).
      OutputImage.Format := Options.ForcedOutputFormat;
      Result := True;
    end;

    // Special handling for TIFF compression
    if Imaging.FindImageFileFormatByName(Options.OutputFileName) is TTiffFileFormat then
    begin
      if Options.TiffCompressionScheme in [TiffCompressionOptionAsInput, TiffCompressionOptionAsInputLossless] then
      begin
        if not Options.TrySetTiffCompressionFromMetadata(Imaging.GlobalMetadata, ReplacedLossy) then
          WriteLn(Log^, 'Could not set TIFF output compression from input, using default.')
        else if ReplacedLossy then
          WriteLn(Log^, 'Input TIFF is JPEG-compressed, using lossless LZW compression for output instead.');
      end;

      if Options.TiffCompressionScheme = TiffCompressionOptionGroup4 then
      begin
        // User explicitly requested
        // CCITT Group 4 (T.6) compression which is for 1bit images only.
        OutputImage.Format := ifBinary;
        Result := True;
      end;
    end;
  end;

begin
  Result := False;

  Threshold := 0;
  ForceRotation := ofForceRotation in Options.OperationalFlags;
  WriteLn(Log^, 'Preparing input image (', ExtractFileName(Options.InputFileName), ' [',
    InputImage.Width, 'x', InputImage.Height, '/', string(InputImage.FormatInfo.Name), ']) ...');

  // Clone input image and convert it to 8bit grayscale. This will be our
  // working image.
  OutputImage.Assign(InputImage);
  InputImage.Format := ifGray8;

  if Options.DpiOverride > 0 then
  begin
    // If user DPI override is set we use it for both input and output.
    // Note that content margins/rectangle calculation of input will use this value.
    GlobalMetadata.SetPhysicalPixelSize(ruDpi, Options.DpiOverride, Options.DpiOverride);
  end;
  if not GlobalMetadata.GetPhysicalPixelSize(ruDpi, Report.DpiX, Report.DpiY) then
  begin
    Report.DpiX := 0;
    Report.DpiY := 0;
  end;

  // Determine the content rect - where exactly to detect rotated text
  if not Options.CalcContentRectForImage(InputImage.BoundsRect, GlobalMetadata, ContentRect) then
  begin
    // User most probably defined content/margins with physical resolution units (cm, inch, ...)
    // but the image has no physical resolution info inside or Deskew is not able to read it.
    // We could use some default (e.g. 96 DPI) but let's rather fail here
    // so the user clearly sees that the expected outcome is not possible.
    raise Exception.Create('Could not determine content rectangle in pixels.' + sLineBreak +
      'Image does not contain DPI information or Deskew failed to read it.');
  end;

  // Determine threshold level for black/white pixel classification during skew detection
  case Options.ThresholdingMethod of
    tmExplicit:
      begin
        // Use explicit threshold
        Threshold := Options.ThresholdLevel;
      end;
    tmOtsu:
      begin
        // Determine the threshold automatically
        Time := GetTimeMicroseconds;
        Threshold := OtsuThresholding(InputImage.ImageDataPointer^, @ContentRect);
        WriteTiming(tsThresholding);
      end;
  end;

  // Look where the actual content of the page is. Blank pages are recognized this way
  // and the skew detection can be limited to the content area if the user asks for it.
  Time := GetTimeMicroseconds;
  HasContent := DetectContentRect(InputImage.ImageDataPointer^, Threshold, FoundContentRect, @ContentRect);
  WriteTiming(tsContentDetection);

  if HasContent and Options.AutoContentRect then
  begin
    ContentRect := FoundContentRect;
    // Threshold for the content only, without the surroundings of the page
    if Options.ThresholdingMethod = tmOtsu then
      Threshold := OtsuThresholding(InputImage.ImageDataPointer^, @ContentRect);
  end;

  if HasContent or ForceRotation then
  begin
    // Main step - calculate image rotation SkewAngle
    WriteLn(Log^, 'Calculating skew angle',
      Iff(EqualRect(InputImage.BoundsRect, ContentRect), '', ' (in ' + RectToStr(ContentRect) +')'),
      ' using threshold ', Threshold, '...');
    Time := GetTimeMicroseconds;
    SkewAngle := CalcRotationAngle(Options.MaxAngle, Options.AngleStep, Threshold,
      InputImage.Width, InputImage.Height, InputImage.Bits,
      @ContentRect, @Stats);
    WriteTiming(tsSkewDetection);
  end
  else
  begin
    // Nothing to detect the skew from
    WriteLn(Log^, 'No content found in the image using threshold ', Threshold, ', skew detection skipped.');
    FillChar(Stats, SizeOf(Stats), 0);
    Stats.Outcome := sdoNoContent;
    SkewAngle := 0;
  end;

  Report.DetectionDone := True;
  Report.Threshold := Threshold;
  Report.HasContent := HasContent;
  Report.ContentRect := FoundContentRect;
  Report.DetectionRect := ContentRect;
  Report.SkewAngle := SkewAngle;
  Report.Stats := Stats;

  WriteLn(Log^, 'Skew angle found [deg]: ', SkewAngle:4:3);
  WriteLn(Log^, 'Detection confidence: ', Round(Stats.Confidence * 100), '%');

  if Options.ShowDetectionStats then
    WriteDetectionStats;

  if Stats.Outcome <> sdoDetected then
    WriteLn(Log^, 'Skew angle is not reliable: ', SDetectionOutcomeReasons[Stats.Outcome], '.');

  if Options.SaveWorkImage then
  begin
    // We can reuse InputImage here, no longer needed
    BinarizeImage(InputImage.ImageDataPointer^, Threshold, @ContentRect);
    InputImage.SaveToFile(ExtractFilePath(Options.OutputFileName) + 'work-image.png');
  end;

  if ofDetectOnly in Options.OperationalFlags then
    Exit;

  if (Stats.Outcome <> sdoDetected) and not ForceRotation then
  begin
    // Rotating by unreliable angle would do more harm than good
    WriteLn(Log^, 'Skipping deskewing step, use "-g f" to rotate the image anyway.');
  end
  // Check if detected skew angle is higher than "skip" threshold - may not
  // want to do rotation needlessly. Forced rotation overrides this too
  // (there is just nothing to rotate when the angle is zero).
  else if (ForceRotation and (SkewAngle <> 0)) or (Abs(SkewAngle) >= Options.SkipAngle) then
  begin
    Result := True;

    // Finally, rotate the image. We rotate the original input image, not the working
    // one so the color space is preserved if possible.
    WriteLn(Log^, 'Rotating image...');

    EnsurePixelFormatForRotation;

    Time := GetTimeMicroseconds;
    ImageUtils.RotateImage(OutputImage.ImageDataPointer^, SkewAngle, Options.BackgroundColor,
      Options.ResamplingFilter, not (ofCropToInput in Options.OperationalFlags));
    WriteTiming(tsRotation);
    Report.Rotated := True;
  end
  else
    WriteLn(Log^, 'Skipping deskewing step, skew angle lower than threshold of ', Options.SkipAngle:4:2);

  Result := ChangeOutputFormatIfNeeded or Result;
  // Result is True if the image was actually changed during the whole procedure
end;

procedure RunDeskew;

  procedure CopyFile(const SrcPath, DestPath: string);
  var
    SrcStream, DestStream: TFileStream;
  begin
    if SameText(SrcPath, DestPath) then
      Exit; // No need to copy anything
    SrcStream := TFileStream.Create(SrcPath, fmOpenRead);
    try
      DestStream := TFileStream.Create(DestPath, fmCreate);
      try
        DestStream.CopyFrom(SrcStream, SrcStream.Size);
      finally
        DestStream.Free;
      end;
    finally
      SrcStream.Free;
    end;
  end;

  // Large images cannot be saved in all the file formats. It's better to tell
  // sooner and clearly than to fail somewhere deep in the file format writer.
  procedure CheckOutputSizeLimit(Image: TSingleImage);
  begin
    if (Image.Size > MaxPngPixelDataSize) and
      (Imaging.FindImageFileFormatByName(Options.OutputFileName) is TNetworkGraphicsFileFormat) then
    begin
      raise Exception.CreateFmt('Image is too large to be saved as %s (%s bytes of pixel data, ' +
        'the limit is %s).' + sLineBreak + 'Use another output file format, e.g. TIFF or QOI: -o output.tif',
        [UpperCase(GetFileExt(Options.OutputFileName)), Trim(FormatNiceNumber(Image.Size, 0)),
         Trim(FormatNiceNumber(MaxPngPixelDataSize, 0))]);
    end;
  end;

  procedure SetImagingOutputOptions;
  begin
    if Options.JpegCompressionQuality <> -1 then
    begin
      Imaging.SetOption(ImagingJpegQuality, Options.JpegCompressionQuality);
      Imaging.SetOption(ImagingTiffJpegQuality, Options.JpegCompressionQuality);
      Imaging.SetOption(ImagingJNGQuality, Options.JpegCompressionQuality);
    end;
    // "As input" schemes were already resolved to a real one in ChangeOutputFormatIfNeeded
    if Options.TiffCompressionScheme in [TiffCompressionOptionNone..TiffCompressionOptionGroup4] then
      Imaging.SetOption(ImagingTiffCompression, Options.TiffCompressionScheme);
  end;

var
  Changed, Parsed: Boolean;
begin
{$IF Defined(FPC) and not Defined(MSWINDOWS)}
  // Flush after WriteLn also when output is redirected to file/pipe
  if Textrec(Output).FlushFunc = nil then
    Textrec(Output).FlushFunc := Textrec(Output).InOutFunc;
  if Textrec(ErrOutput).FlushFunc = nil then
    Textrec(ErrOutput).FlushFunc := Textrec(ErrOutput).InOutFunc;
{$IFEND}

  Log := @Output;
  InitReport;
  Options := TCmdLineOptions.Create;
  InputImage := TSingleImage.Create;
  OutputImage := TSingleImage.Create;

  try
    try
      // Parameters go first, they tell where to write the text to
      Parsed := Options.ParseCommandLine;
      if Options.JsonToStdOut and not Options.HelpRequested then
        Log := @ErrOutput;

      WriteLn(Log^, SAppTitle);
      WriteLn(Log^, SAppHome);
      WriteLn(Log^);

      if Options.HelpRequested then
      begin
        WriteUsage;
      end
      else if Parsed and Options.IsValid then
      begin
        Report.ParamsValid := True;
        if Options.ShowParams then
          WriteLn(Log^, Options.OptionsToString);

        if not IsFileFormatSupported(Options.InputFileName) then
        begin
          ReportBadInput('Input file format not supported: ' + Options.InputFileName);
          Exit;
        end;
        if not IsFileFormatSupported(Options.OutputFileName) then
        begin
          ReportBadInput('Output file format not supported: ' + Options.OutputFileName);
          Exit;
        end;

        // Load input image
        Time := GetTimeMicroseconds;
        InputImage.LoadFromFile(Options.InputFileName);
        WriteTiming(tsLoad);

        if not InputImage.Valid then
        begin
          ReportBadInput('Loaded input image is not valid: ' + Options.InputFileName, False);
          Exit;
        end;

        Report.InputLoaded := True;
        Report.InputWidth := InputImage.Width;
        Report.InputHeight := InputImage.Height;
        Report.InputFormat := string(InputImage.FormatInfo.Name);

        // No need to do all the work if the result cannot be saved anyway
        // (unless the output is going to be in another, smaller pixel format)
        if not (ofDetectOnly in Options.OperationalFlags) and (Options.ForcedOutputFormat = ifUnknown) then
          CheckOutputSizeLimit(InputImage);

        // Do the magic
        Changed := DoDeskew();

        if not (ofDetectOnly in Options.OperationalFlags) then
        begin
          WriteLn(Log^, 'Saving output (', ExpandFileName(Options.OutputFileName), ' [',
            OutputImage.Width, 'x', OutputImage.Height, '/', string(OutputImage.FormatInfo.Name), ']) ...');

          // Make sure output folders are ready
          EnsureOutputLocation(Options.OutputFileName);
          // In case no change to image was done by deskewing we still need to resave if requested file format differs from input
          Changed := Changed or not SameText(GetFileExt(Options.InputFileName), GetFileExt(Options.OutputFileName));

          Time := GetTimeMicroseconds;
          if Changed then
          begin
            CheckOutputSizeLimit(OutputImage);
            SetImagingOutputOptions;
            // Make sure recognized metadata stays (like scanning DPI info)
            GlobalMetadata.CopyLoadedMetaItemsForSaving;
            // Save the output
            try
              OutputImage.SaveToFile(Options.OutputFileName);
            except
              // Do not leave empty or broken file behind
              SysUtils.DeleteFile(Options.OutputFileName);
              raise;
            end;
            Report.OutputWritten := owSaved;
          end
          else
          begin
            // No change to image made, just copy it to the desired destination
            CopyFile(Options.InputFileName, Options.OutputFileName);
            Report.OutputWritten := owCopied;
          end;
          Report.OutputWidth := OutputImage.Width;
          Report.OutputHeight := OutputImage.Height;
          Report.OutputFormat := string(OutputImage.FormatInfo.Name);

          WriteTiming(tsSave);
        end;

        WriteLn(Log^, 'Done!');
      end
      else
      begin
        // Bad input, whole usage is shown only when there are no parameters at all
        ReportBadInput('Invalid parameters!', ParamCount = 0);
        if ParamCount > 0 then
          WriteLn(Log^, 'Use "-h" to see all the options.');
      end;

    except
      on E: Exception do
      begin
        WriteLn(Log^);
        WriteLn(Log^, E.ClassName, ': ', E.Message);
        Report.ErrorMessage := E.Message;
        ExitCode := 1;
      end;
    end;
  finally
    WriteReports;
    Options.Free;
    InputImage.Free;
    OutputImage.Free;

  {$IF Defined(DEBUG) and Defined(DELPHI) and Defined(MSWINDOWS)}
    {$WARN SYMBOL_PLATFORM OFF}
    if DebugHook <> 0 then
      ReadLn;
  {$IFEND}
  end;
end;

end.
