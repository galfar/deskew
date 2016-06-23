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

interface

uses
  Types,
  SysUtils,
  Classes,
  ImagingTypes,
  Imaging,
  ImagingClasses,
  ImagingUtility,
  // Project units
  CmdLineOptions,
  ImageUtils,
  RotationDetector;

procedure RunDeskew;

implementation

const
  SAppTitle = 'Deskew 1.14 (2016-06-24) by Marek Mauder';
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
  WriteLn('    -b color:      Background color in hex format RRGGBB (default: trns. black)');
  WriteLn('  Ext. options:');
  WriteLn('    -t a|treshold: Auto threshold or value in 0..255 (default: a)');
  WriteLn('    -r rect:       Skew detection only in content rectangle (pixels):');
  WriteLn('                   left,top,right,bottom (default: whole page)');
  WriteLn('    -f format:     Force output pixel format (values: b1|g8|rgba32)');
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
  HasBackColor: Boolean;
  OrigFormat: TImageFormat;

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
  WriteLn('Skew angle found: ', SkewAngle:4:2);

  // Check if detected skew angle is higher than "skip" threshold - may not
  // want to do rotation (+possible background fill) needlessly.
  if Abs(SkewAngle) >= Options.SkipAngle then
  begin
    Result := True;

    // Finally, rotate the image. We rotate the original input image, not the working
    // one so the color space is preserved.
    WriteLn('Rotating image...');
    if OutputImage.FormatInfo.IsIndexed or OutputImage.FormatInfo.IsSpecial then
      OutputImage.Format := ifA8R8G8B8; // Rotation doesn't like indexed and compressed/1bit images

    // For back color to work we need to make sure empty "void" around the image has alpha=0,
    // so 32bit ARGB format is needed

    HasBackColor := Options.BackgroundColor <> 0;

    if HasBackColor then
    begin
      OrigFormat := OutputImage.Format;
      OutputImage.Format := ifA8R8G8B8;

      Time := GetTimeMicroseconds;
      ImageUtils.RotateImageWithBackground(OutputImage.ImageDataPointer^, SkewAngle, Options.BackgroundColor);
      WriteTiming('Rotate image with background');

      // Convert image back to its original format (but only if there is no forced format applied later)
      if (OutputImage.Format <> OrigFormat) and (Options.ForcedOutputFormat = ifUnknown) then
        OutputImage.Format := OrigFormat;
    end
    else
    begin
      Time := GetTimeMicroseconds;
      OutputImage.Rotate(SkewAngle);
      WriteTiming('Rotate image');
    end;
  end
  else
    WriteLn('Skipping deskewing step, skew angle lower than threshold of ', Options.SkipAngle:4:2);

  if Options.ForcedOutputFormat <> ifUnknown then
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

        // Load input image
        Time := GetTimeMicroseconds;
        InputImage.LoadFromFile(Options.InputFile);
        WriteTiming('Load input file');

        // Do the magic
        Changed := DoDeskew();
        // Make sure output folders are ready
        EnsureOutputLocation(Options.OutputFile);

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
        WriteLn('Invalid parameters!');
        if Options.ErrorMessage <> '' then
          WriteLn(Options.ErrorMessage);
        WriteLn;

        WriteUsage;
        ExitCode := 1;
      end;

    except
      on E: Exception do
      begin
        WriteLn(E.ClassName, ': ', E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    Options.Free;
    InputImage.Free;
    OutputImage.Free;
  end;

{$IFDEF DEBUG}
  ReadLn;
{$ENDIF}
end;

end.
