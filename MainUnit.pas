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
  ImagingTiff,
  // Project units
  CmdLineOptions,
  Utils,
  ImageUtils,
  RotationDetector;

const
  SAppTitle = 'Deskew 1.33 (2025-06-02, ' + SBuildTarget + ', ' + SBuildCompiler
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

var
  // Program options
  Options: TCmdLineOptions;
  // Input and output image
  InputImage, OutputImage: TSingleImage;

procedure WriteUsage;
var
  InFilter, OutFilter: string;
  I, Count: Integer;
  Fmt: TImageFileFormat;
begin
  InFilter := '';
  OutFilter := '';

  WriteLn('Usage: deskew [-o output] [-a angle] [-b color] [..] input');
  WriteLn('    input:         Input image file');
  WriteLn('    -h:            Show this help');

  WriteLn;
  WriteLn('  Options:');
  WriteLn('    -o output:     Output image file name (default: deskewed-<input name>.png');
  WriteLn('                   next to the input file)');
  WriteLn('    -b color:      Background color in hex format RRGGBB|LL|AARRGGBB (default: black)');
  WriteLn('    -q filter:     Resampling filter used for rotations (default: linear,');
  WriteLn('                   values: nearest|linear|cubic|lanczos)');
  WriteLn('    -a angle:      Maximal expected skew angle (both directions) in degrees (default: 10)');

  WriteLn;
  WriteLn('  Advanced options:');
  WriteLn('    -d angle:      Angle step during detection in degrees (default: 0.1)');
  WriteLn('    -t a|level:    Auto threshold or level in 1..255, darker pixels are');
  WriteLn('                   considered to be text (default: auto)');
  WriteLn('    -m margins:    Skew detection only outside page margins:');
  WriteLn('                   L,T,R,B[,unit] or H,V[,unit] or A[,unit] (default: whole page)');
  WriteLn('                   unit: px|%|mm|cm|in (default: px)');
  WriteLn('    -r rect:       Skew detection only in content rectangle:');
  WriteLn('                   L,T,R,B[,unit] or auto (default: whole page)');
  WriteLn('                   unit: px|%|mm|cm|in (default: px)');
  WriteLn('                   auto: content area is found automatically for each image');
  WriteLn('    -f format:     Force output pixel format (values: b1|g8|rgb24|rgba32)');
  WriteLn('    -p dpi:        Print resolution of the image in DPI, overrides the one stored');
  WriteLn('                   in the input file (default: use the stored one)');
  WriteLn('    -l angle:      Skip deskewing step if skew angle is smaller, in degrees (default: 0.01)');
  WriteLn('    -g flags:      Operational flags (any combination of):');
  WriteLn('                   c - crop to input size, d - detect only (no output to file),');
  WriteLn('                   f - force rotation: also when the skew angle is not reliable');
  WriteLn('                   or smaller than the skip angle (-l)');
  WriteLn('    -s info:       Info dump (any combination of):');
  WriteLn('                   s - skew detection stats, p - program parameters, t - timings,');
  WriteLn('                   w - save work/thresholded image');
  WriteLn('    -c specs:      Output file format specs (compression etc.). Several specs');
  WriteLn('                   can be defined - delimited by commas. Supported specs:');
  WriteLn('                   jXX - JPEG compression quality, XX in range [1,100(best)]');
  WriteLn('                   tSCHEME - TIFF compression scheme: none|lzw|rle|deflate|jpeg|g4|input|input-lossless');
  WriteLn;
  WriteLn('  Skew angle is positive when the content is turned clockwise (text lines go down');
  WriteLn('  to the right), image is then rotated counterclockwise by this angle to fix it.');

  Count := GetFileFormatCount;
  for I := 0 to Count - 1 do
  begin
    Fmt := GetFileFormatAtIndex(I);
    if Fmt.CanLoad then
      InFilter := InFilter + Fmt.Extensions[0] + Iff(I < Count - 1, ', ', '');
    if Fmt.CanSave then
      OutFilter := OutFilter + Fmt.Extensions[0] + Iff(I < Count - 1, ', ', '');
  end;

  WriteLn;
  WriteLn('  Supported file formats');
  WriteLn('    Input:  ', UpperCase(InFilter));
  WriteLn('    Output: ', UpperCase(OutFilter));
end;

procedure ReportBadInput(const Msg: string; ShowUsage: Boolean = True);
begin
  if ShowUsage then
  begin
    WriteUsage;
    WriteLn;
  end;

  // Error goes last so it does not get lost above the usage text
  WriteLn('ERROR: ' + Msg);
  if Options.ErrorMessage <> '' then
    WriteLn(Options.ErrorMessage);

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

procedure WriteTiming(const StepName: string);
begin
  if Options.ShowTimings then
    WriteLn(StepName + ' - time taken: ' + FormatNiceNumber(GetTimeMicroseconds - Time, 0) + ' us');
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
    WriteLn('Skew detection stats:');
    WriteLn('  content area:       ', Iff(HasContent, RectToStr(FoundContentRect), 'none'):16);
    WriteLn('  pixel count:        ', FormatNiceNumber(Stats.PixelCount));
    WriteLn('  tested pixels:      ', FormatNiceNumber(Stats.TestedPixels));
    WriteLn('  accumulator size:   ', FormatNiceNumber(Stats.AccumulatorSize));
    WriteLn('  accumulated counts: ', FormatNiceNumber(Stats.AccumulatedCounts));
    WriteLn('  best count:         ', FormatNiceNumber(Stats.BestCount));
    WriteLn('  peak angle:         ', Stats.PeakAngle:16:3);
    WriteLn('  lines at peak:      ', FormatNiceNumber(Stats.LinesAtPeak));
    WriteLn('  sharpness:          ', Stats.Sharpness:16:3);
    WriteLn('  confidence:         ', Stats.Confidence:16:3);
    WriteLn('  outcome:            ', SDetectionOutcomes[Stats.Outcome]:16);
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
    if Imaging.FindImageFileFormatByName(Options.OutputFileName) is TBaseTiffFileFormat then
    begin
      if Options.TiffCompressionScheme in [TiffCompressionOptionAsInput, TiffCompressionOptionAsInputLossless] then
      begin
        if not Options.TrySetTiffCompressionFromMetadata(Imaging.GlobalMetadata, ReplacedLossy) then
          WriteLn('Could not set TIFF output compression from input, using default.')
        else if ReplacedLossy then
          WriteLn('Input TIFF is JPEG-compressed, using lossless LZW compression for output instead.');
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
  WriteLn('Preparing input image (', ExtractFileName(Options.InputFileName), ' [',
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
        WriteTiming('Auto thresholding');
      end;
  end;

  // Look where the actual content of the page is. Blank pages are recognized this way
  // and the skew detection can be limited to the content area if the user asks for it.
  Time := GetTimeMicroseconds;
  HasContent := DetectContentRect(InputImage.ImageDataPointer^, Threshold, FoundContentRect, @ContentRect);
  WriteTiming('Content detection');

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
    WriteLn('Calculating skew angle',
      Iff(EqualRect(InputImage.BoundsRect, ContentRect), '', ' (in ' + RectToStr(ContentRect) +')'),
      ' using threshold ', Threshold, '...');
    Time := GetTimeMicroseconds;
    SkewAngle := CalcRotationAngle(Options.MaxAngle, Options.AngleStep, Threshold,
      InputImage.Width, InputImage.Height, InputImage.Bits,
      @ContentRect, @Stats);
    WriteTiming('Skew detection');
  end
  else
  begin
    // Nothing to detect the skew from
    WriteLn('No content found in the image using threshold ', Threshold, ', skew detection skipped.');
    FillChar(Stats, SizeOf(Stats), 0);
    Stats.Outcome := sdoNoContent;
    SkewAngle := 0;
  end;

  WriteLn('Skew angle found [deg]: ', SkewAngle:4:3);
  WriteLn('Detection confidence: ', Round(Stats.Confidence * 100), '%');

  if Options.ShowDetectionStats then
    WriteDetectionStats;

  if Stats.Outcome <> sdoDetected then
    WriteLn('Skew angle is not reliable: ', SDetectionOutcomeReasons[Stats.Outcome], '.');

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
    WriteLn('Skipping deskewing step, use "-g f" to rotate the image anyway.');
  end
  // Check if detected skew angle is higher than "skip" threshold - may not
  // want to do rotation needlessly. Forced rotation overrides this too
  // (there is just nothing to rotate when the angle is zero).
  else if (ForceRotation and (SkewAngle <> 0)) or (Abs(SkewAngle) >= Options.SkipAngle) then
  begin
    Result := True;

    // Finally, rotate the image. We rotate the original input image, not the working
    // one so the color space is preserved if possible.
    WriteLn('Rotating image...');

    EnsurePixelFormatForRotation;

    Time := GetTimeMicroseconds;
    ImageUtils.RotateImage(OutputImage.ImageDataPointer^, SkewAngle, Options.BackgroundColor,
      Options.ResamplingFilter, not (ofCropToInput in Options.OperationalFlags));
    WriteTiming('Rotate image');
  end
  else
    WriteLn('Skipping deskewing step, skew angle lower than threshold of ', Options.SkipAngle:4:2);

  Result := ChangeOutputFormatIfNeeded or Result;
  // Result is True if the image was actually changed during the whole procedure
end;

procedure RunDeskew;

  procedure EnsureOutputLocation(const FileName: string);
  var
    Dir, Path: string;
  begin
    Path := ExpandFileName(FileName);
    Dir := GetFileDir(Path);
    if Dir <> '' then
      ForceDirectories(Dir);
  end;

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
{$IFEND}

  WriteLn(SAppTitle);
  WriteLn(SAppHome);
  WriteLn;

  Options := TCmdLineOptions.Create;
  InputImage := TSingleImage.Create;
  OutputImage := TSingleImage.Create;

  try
    try
      Parsed := Options.ParseCommandLine;

      if Options.HelpRequested then
      begin
        WriteUsage;
      end
      else if Parsed and Options.IsValid then
      begin
        if Options.ShowParams then
          WriteLn(Options.OptionsToString);

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
        WriteTiming('Load input file');

        if not InputImage.Valid then
        begin
          ReportBadInput('Loaded input image is not valid: ' + Options.InputFileName, False);
          Exit;
        end;

        // Do the magic
        Changed := DoDeskew();

        if not (ofDetectOnly in Options.OperationalFlags) then
        begin
          WriteLn('Saving output (', ExpandFileName(Options.OutputFileName), ' [',
            OutputImage.Width, 'x', OutputImage.Height, '/', string(OutputImage.FormatInfo.Name), ']) ...');

          // Make sure output folders are ready
          EnsureOutputLocation(Options.OutputFileName);
          // In case no change to image was done by deskewing we still need to resave if requested file format differs from input
          Changed := Changed or not SameText(GetFileExt(Options.InputFileName), GetFileExt(Options.OutputFileName));

          Time := GetTimeMicroseconds;
          if Changed then
          begin
            SetImagingOutputOptions;
            // Make sure recognized metadata stays (like scanning DPI info)
            GlobalMetadata.CopyLoadedMetaItemsForSaving;
            // Save the output
            OutputImage.SaveToFile(Options.OutputFileName);
          end
          else
          begin
            // No change to image made, just copy it to the desired destination
            CopyFile(Options.InputFileName, Options.OutputFileName);
          end;

          WriteTiming('Save output file');
        end;

        WriteLn('Done!');
      end
      else
      begin
        // Bad input, whole usage is shown only when there are no parameters at all
        ReportBadInput('Invalid parameters!', ParamCount = 0);
        if ParamCount > 0 then
          WriteLn('Use "-h" to see all the options.');
      end;

    except
      on E: Exception do
      begin
        WriteLn;
        WriteLn(E.ClassName, ': ', E.Message);
        ExitCode := 1;
      end;
    end;
  finally
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
