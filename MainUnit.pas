{
  Deskew
  by Marek Mauder
  http://galfar.vevb.net/deskew

  The contents of this file are used with permission, subject to the Mozilla
  Public License Version 1.1 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
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
  // Project units
  CmdLineOptions,
  ImageUtils,
  RotationDetector;

const
  SAppTitle = 'Deskew 1.25 (2018-05-19)'
    {$IF Defined(CPUX64)} + ' x64'
    {$ELSEIF Defined(CPUX86)} + ' x86'
    {$ELSEIF Defined(CPUARM)} + ' ARM'
    {$IFEND}
    {$IFDEF DEBUG} + ' (DEBUG)'{$ENDIF}
    + ' by Marek Mauder';
  SAppHome = 'http://galfar.vevb.net/deskew/';

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
  WriteLn('    -o output:     Output image file (default: out.png)');
  WriteLn('    -a angle:      Maximal skew angle in degrees (default: 10)');
  WriteLn('    -b color:      Background color in hex format RRGGBB|LL|AARRGGBB (default: black)');
  WriteLn('  Ext. options:');
  WriteLn('    -t a|treshold: Auto threshold or value in 0..255 (default: a)');
  WriteLn('    -r rect:       Skew detection only in content rectangle (pixels):');
  WriteLn('                   left,top,right,bottom (default: whole page)');
  WriteLn('    -f format:     Force output pixel format (values: b1|g8|rgb24|rgba32)');
  WriteLn('    -l angle:      Skip deskewing step if skew angle is smaller (default: 0.01)');
  WriteLn('    -s info:       Info dump (any combination of):');
  WriteLn('                   s - skew detection stats, p - program parameters, t - timings');

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
  WriteLn;
  WriteLn('Error: ' + Msg);
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
  WriteLn('Preparing input image (', ExtractFileName(Options.InputFile), ') ...');

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
    ImageUtils.RotateImageWithBackground(OutputImage.ImageDataPointer^, SkewAngle, Options.BackgroundColor);
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

  if Options.ShowStats then
    WriteStats;
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
    SrcStream := TFileStream.Create(SrcPath, fmOpenRead);
    DestStream := TFileStream.Create(DestPath, fmCreate);
    DestStream.CopyFrom(SrcStream, SrcStream.Size);
    DestStream.Free;
    SrcStream.Free;
  end;

var
  Changed: Boolean;
begin
  WriteLn(SAppTitle);
  WriteLn(SAppHome);

  Options := TCmdLineOptions.Create;
  InputImage := TSingleImage.Create;
  OutputImage := TSingleImage.Create;

  try
    try
      if Options.ParseCommnadLine and Options.IsValid then
      begin
        if Options.ShowParams then
          WriteLn(Options.OptionsToString);

        if not IsFileFormatSupported(Options.InputFile) then
        begin
          ReportBadInput('File format not supported: ' + Options.InputFile);
          Exit;
        end;

        // Load input image
        Time := GetTimeMicroseconds;
        InputImage.LoadFromFile(Options.InputFile);
        WriteTiming('Load input file');

        if not InputImage.Valid then
        begin
          ReportBadInput('Loaded input image is not valid: ' + Options.InputFile, False);
          Exit;
        end;

        // Do the magic
        Changed := DoDeskew();
        // Make sure output folders are ready
        EnsureOutputLocation(Options.OutputFile);
        // In case no change to image was done by deskewing we still need to resave if requested file format differs from input
        Changed := Changed or not SameText(GetFileExt(Options.InputFile), GetFileExt(Options.OutputFile));

        Time := GetTimeMicroseconds;
        if Changed then
        begin
          // Make sure recognized metadata stays (like scanning DPI info)
          GlobalMetadata.CopyLoadedMetaItemsForSaving;
          // Save the output
          OutputImage.SaveToFile(Options.OutputFile);
        end
        else
        begin
          // No change to image made, just copy it to the desired destination
          CopyFile(Options.InputFile, Options.OutputFile);
        end;
        WriteTiming('Save output file');

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
