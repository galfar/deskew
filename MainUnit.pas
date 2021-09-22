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
  ImagingExtras,
  // Project units
  CmdLineOptions,
  ImageUtils,
  RotationDetector;

const
  SAppTitle = 'Deskew 1.31 (2021-09-22)'
    {$IF Defined(CPUX64)} + ' x64'
    {$ELSEIF Defined(CPUX86)} + ' x86'
    {$ELSEIF Defined(CPUARM)} + ' ARM'
    {$IFEND}
    {$IFDEF DEBUG} + ' (DEBUG)'{$ENDIF}
    + ' by Marek Mauder';
  SAppHome = 'https://galfar.vevb.net/deskew/' + sLineBreak +
             'https://github.com/galfar/deskew';

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

  WriteLn('Usage:');
  WriteLn('deskew [-o output] [-a angle] [-b color] [..] input');
  WriteLn('    input:         Input image file');
  WriteLn('  Options:');
  WriteLn('    -o output:     Output image file name (default: prefixed input as png)');
  WriteLn('    -a angle:      Maximal expected skew angle (both directions) in degrees (default: 10)');
  WriteLn('    -b color:      Background color in hex format RRGGBB|LL|AARRGGBB (default: black)');
  WriteLn('  Ext. options:');
  WriteLn('    -q filter:     Resampling filter used for rotations (default: linear');
  WriteLn('                   values: nearest|linear|cubic|lanczos)');
  WriteLn('    -t a|treshold: Auto threshold or value in 0..255 (default: auto)');
  WriteLn('    -r rect:       Skew detection only in content rectangle (pixels):');
  WriteLn('                   left,top,right,bottom (default: whole page)');
  WriteLn('    -f format:     Force output pixel format (values: b1|g8|rgb24|rgba32)');
  WriteLn('    -l angle:      Skip deskewing step if skew angle is smaller (default: 0.01)');
  WriteLn('    -g flags:      Operational flags (any combination of):');
  WriteLn('                   c - crop to input size, d - detect only (no output to file)');
  WriteLn('    -s info:       Info dump (any combination of):');
  WriteLn('                   s - skew detection stats, p - program parameters, t - timings');
  WriteLn('    -c specs:      Output compression specs for some file formats. Several specs');
  WriteLn('                   can be defined - delimited by commas. Supported specs:');
  WriteLn('                   jXX - JPEG compression quality, XX is in range [1,100(best)]');
  WriteLn('                   tSCHEME - TIFF compression scheme: none|lzw|rle|deflate|jpeg|g4');


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
  WriteLn('ERROR: ' + Msg);
  if Options.ErrorMessage <> '' then
    WriteLn(Options.ErrorMessage);
  WriteLn;

  if ShowUsage then
    WriteUsage;

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
  ContentRect: TRect;
  Stats: TCalcSkewAngleStats;

  procedure WriteStats;
  begin
    WriteLn('Skew detection stats:');
    WriteLn('  pixel count:        ', FormatNiceNumber(Stats.PixelCount));
    WriteLn('  tested pixels:      ', FormatNiceNumber(Stats.TestedPixels));
    WriteLn('  accumulator size:   ', FormatNiceNumber(Stats.AccumulatorSize));
    WriteLn('  accumulated counts: ', FormatNiceNumber(Stats.AccumulatedCounts));
    WriteLn('  best count:         ', FormatNiceNumber(Stats.BestCount));
  end;

begin
  Result := False;
  Threshold := 0;
  WriteLn('Preparing input image (', ExtractFileName(Options.InputFileName), ' [',
    InputImage.Width, 'x', InputImage.Height, '/', string(InputImage.FormatInfo.Name), ']) ...');

  // Clone input image and convert it to 8bit grayscale. This will be our
  // working image.
  OutputImage.Assign(InputImage);
  InputImage.Format := ifGray8;

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
        Threshold := OtsuThresholding(InputImage.ImageDataPointer^);
        WriteTiming('Auto thresholding');
      end;
  end;

  // Determine the content rect - where exactly to detect rotated text
  ContentRect := InputImage.BoundsRect;
  if not IsRectEmpty(Options.ContentRect) then
  begin
    if not IntersectRect(ContentRect, Options.ContentRect, InputImage.BoundsRect) then
      ContentRect := InputImage.BoundsRect;
  end;

  // Main step - calculate image rotation SkewAngle
  WriteLn('Calculating skew angle...');
  Time := GetTimeMicroseconds;
  SkewAngle := CalcRotationAngle(Options.MaxAngle, Threshold,
    InputImage.Width, InputImage.Height, InputImage.Bits,
    @ContentRect, @Stats);
  WriteTiming('Skew detection');
  WriteLn('Skew angle found [deg]: ', SkewAngle:4:3);

  if Options.ShowStats then
    WriteStats;

  if ofDetectOnly in Options.OperationalFlags then
    Exit;

  // Check if detected skew angle is higher than "skip" threshold - may not
  // want to do rotation needlessly.
  if Abs(SkewAngle) >= Options.SkipAngle then
  begin
    Result := True;

    // Finally, rotate the image. We rotate the original input image, not the working
    // one so the color space is preserved if possible.
    WriteLn('Rotating image...');

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

    Time := GetTimeMicroseconds;
    ImageUtils.RotateImage(OutputImage.ImageDataPointer^, SkewAngle, Options.BackgroundColor,
      Options.ResamplingFilter, not (ofCropToInput in Options.OperationalFlags));
    WriteTiming('Rotate image');
  end
  else
    WriteLn('Skipping deskewing step, skew angle lower than threshold of ', Options.SkipAngle:4:2);

  if (Options.ForcedOutputFormat <> ifUnknown) and (OutputImage.Format <> Options.ForcedOutputFormat) then
  begin
    // Force output format. For example Deskew won't automatically
    // save image as binary if the input was binary since it
    // might degrade the output a lot (rotation adds a lot of colors to image).
    OutputImage.Format := Options.ForcedOutputFormat;
    Result := True;
  end;
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
    DestStream := TFileStream.Create(DestPath, fmCreate);
    DestStream.CopyFrom(SrcStream, SrcStream.Size);
    DestStream.Free;
    SrcStream.Free;
  end;

  procedure SetImagingOptions;
  begin
    if Options.JpegCompressionQuality <> -1 then
    begin
      Imaging.SetOption(ImagingJpegQuality, Options.JpegCompressionQuality);
      Imaging.SetOption(ImagingTiffJpegQuality, Options.JpegCompressionQuality);
      Imaging.SetOption(ImagingJNGQuality, Options.JpegCompressionQuality);
    end;
    if Options.TiffCompressionScheme <> -1 then
      Imaging.SetOption(ImagingTiffCompression, Options.TiffCompressionScheme);
  end;

var
  Changed: Boolean;
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
      if Options.ParseCommnadLine and Options.IsValid then
      begin
        SetImagingOptions;
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
        // Bad input
        ReportBadInput('Invalid parameters!');
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

{$IFDEF DEBUG}
  ReadLn;
{$ENDIF}
  end;
end;

end.
