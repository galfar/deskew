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
  SAppTitle = 'Deskew 1.00 (2012-06-05) by Marek Mauder';
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
  WriteLn('deskew [-a angle] [-t a|treshold] [-b color] [-r rect] [-o output] [-s info] input');
  WriteLn('    -a angle:      Maximal skew angle in degrees (default: 10)');
  WriteLn('    -t a|treshold: Auto threshold or value in 0..255 (default: a)');
  WriteLn('    -b color:      Background color in hex format RRGGBB (default: trns. black)');
  WriteLn('    -r rect:       Skew detection only in content rectangle (pixels):');
  WriteLn('                   left,top,right,bottom (default: whole page)');
  WriteLn('    -o output:     Output image file (default: out.png)');
  WriteLn('    -s info:       Info dump (any combination of):');
  WriteLn('                   s - skew detection stats, p - program parameters');
  WriteLn('    input:         Input image file');

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

procedure DoDeskew;
var
  Angle: Double;
  Threshold: Integer;
  ContentRect: TRect;
  Stats: TCalcSkewAngleStats;
  HasBackColor: Boolean;
  OrigFormat: TImageFormat;

  procedure WriteStats;
  begin
    WriteLn('Skew detection stats:');
    WriteLn('  pixel count:        ', Stats.PixelCount);
    WriteLn('  tested pixels:      ', Stats.TestedPixels);
    WriteLn('  accumulator size:   ', Stats.AccumulatorSize);
    WriteLn('  accumulated counts: ', Stats.AccumulatedCounts);
    WriteLn('  best count:         ', Stats.BestCount);
  end;

begin
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
        Threshold := OtsuThresholding(InputImage.ImageDataPointer^);
      end;
  end;

  // Determine the content rect - where exactly to detect rotated text
  ContentRect := InputImage.BoundsRect;
  if not IsRectEmpty(Options.ContentRect) then
  begin
    if not IntersectRect(ContentRect, Options.ContentRect, InputImage.BoundsRect) then
      ContentRect := InputImage.BoundsRect;
  end;

  // Main step - calculate image rotation angle
  WriteLn('Calculating skew angle...');
  Angle := CalcRotationAngle(Options.MaxAngle, Threshold,
    InputImage.Width, InputImage.Height, InputImage.Bits,
    @ContentRect, @Stats);
  WriteLn('Skew angle found: ', Angle:4:2);

  // Finally, rotate the image. We rotate the original input image, not the working
  // one so the color space is preserved.
  WriteLn('Rotating image...');
  if OutputImage.FormatInfo.IsIndexed or OutputImage.FormatInfo.IsSpecial then
    OutputImage.Format := ifA8R8G8B8; // Rotation doesn't like indexed and compressed images

  // For back color to work we need to make sure empty "void" around the image has alpha=0,
  // so 32bit ARGB format is needed
  OrigFormat := OutputImage.Format;
  HasBackColor := Options.BackgroundColor <> 0;
  if HasBackColor then
    OutputImage.Format := ifA8R8G8B8;

  OutputImage.Rotate(Angle);

  if HasBackColor then
  begin
    // Finally, merge rotated image with background
    MergeWithBackground(OutputImage, Options.BackgroundColor);
    if OutputImage.Format <> OrigFormat then
      OutputImage.Format := OrigFormat;
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

begin
  WriteLn(SAppTitle);
  WriteLn(SAppHome);

  Options := TCmdLineOptions.Create;
  InputImage := TSingleImage.Create;
  OutputImage := TSingleImage.Create;

  try
    try
      // Parse command line
      Options.ParseCommnadLine;

      if Options.IsValid then
      begin
        if Options.ShowParams then
          WriteLn(Options.OptionsToString);

        // Load input image
        InputImage.LoadFromFile(Options.InputFile);
        // Do the magic
        DoDeskew;
        // Save the output
        EnsureOutputLocation(Options.OutputFile);
        OutputImage.SaveToFile(Options.OutputFile);
        WriteLn('Done!');
      end
      else
      begin
        // Bad input
        WriteLn('Invalid parameters!');
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
end;

end.
