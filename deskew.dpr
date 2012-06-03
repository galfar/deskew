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

{
  Change list:
    0.95 2010-12-28: Added auto thresholding. Imaging library updated.
    0.90 2010-02-12: Initial version
}

program deskew;

{$IFDEF MSWINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  Types,
  SysUtils,
  ImagingTypes,
  Imaging,
  ImagingUtility,
  RotationDetector in 'RotationDetector.pas',
  Options in 'Options.pas',
  ImageUtils in 'ImageUtils.pas';

const
  // Default threshold value
  DefaultThreshold = 128;
  // Default max rotation angle
  DefaultMaxAngle = 10;

var
  // Input and output file names
  InputFile, OutputFile: string;
  // Max expected rotation angle (algo then works in range <-MaxAngle, MaxAngle>)
  MaxAngle: Integer = DefaultMaxAngle;
  // Threshold for black/white pixel classification
  Threshold: Integer = DefaultThreshold;
  // Determine threshold automatically?
  AutoThreshold: Boolean = True;
  // Input and output image
  InputImage, OutputImage: TImageData;

{
  Parses command line arguments.
}
procedure CheckCommandLine;
var
  I: LongInt;
  S, Arg, Dir: string;
begin
  I := 1;
  while I <= ParamCount do
  begin
    S := ParamStr(I);
    if Pos('-', S) = 1 then
    begin
      Arg := ParamStr(I + 1);
      Inc(I);
      if S = '-o' then
        OutputFile := Arg
      else if S = '-a' then
        MaxAngle := StrToIntDef(Arg, -1)
      else if S = '-t' then
      begin
        if Arg = 'a' then
          AutoThreshold := True
        else
        begin
          AutoThreshold := False;
          Threshold := StrToIntDef(Arg, -1);
        end;
      end;
    end
    else
      InputFile := S;

    Inc(I);
  end;

  if OutputFile = '' then
  begin
    Dir := ExtractFileDir(InputFile);
    if Dir <> '' then
      Dir := Dir + PathDelim;
    OutputFile := Dir + 'out.png';
  end;
end;

{
  Prints usage info if command line parsing fails.
}
procedure WriteUsage;
var
  InFilter, OutFilter: string;
  I, Count: Integer;
  Fmt: TImageFileFormat;
begin
  WriteLn('Usage:');
  WriteLn('  deskew [-a angle] [-t a|treshold] [-o output] input');
  WriteLn('    -a angle:      Maximal skew angle in degrees (default: 10)');
  WriteLn('    -t a|treshold: Auto threshold or value in 0..255 (default: a)');
  WriteLn('    -o output:     Output image file (default: out.png)');
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

{
  Thresholding using Otsu's method (which chooses the threshold
  to minimize the intraclass variance of the black and white pixels!).
}
function OtsuThresholding(var Image: TImageData): Integer;
var
  Histogram: array[Byte] of Single;
  Level, Max, Min, I, J, NumPixels: Integer;
  Pix: PByte;
  Mean, Variance: Single;
  Mu, Omega, LevelMean, LargestMu: Single;
begin
  FillChar(Histogram, SizeOf(Histogram), 0);
  Min := 255;
  Max := 0;
  Level := 0;
  NumPixels := Image.Width * Image.Height;
  Pix := Image.Bits;

  // Compute histogram and determine min and max pixel values
  for I := 0 to NumPixels - 1 do
  begin
    Histogram[Pix^] := Histogram[Pix^] + 1.0;
    if Pix^ < Min then
      Min := Pix^;
    if Pix^ > Max then
      Max := Pix^;
    Inc(Pix);
  end;

  // Normalize histogram
  for I := 0 to 255 do
    Histogram[I] := Histogram[I] / NumPixels;

  // Compute image mean and variance
  Mean := 0.0;
  Variance := 0.0;
  for I := 0 to 255 do
    Mean := Mean + (I + 1) * Histogram[I];
  for I := 0 to 255 do
    Variance := Variance + Sqr(I + 1 - Mean) * Histogram[I];

  // Now finally compute threshold level
  LargestMu := 0;

  for I := 0 to 255 do
  begin
    Omega := 0.0;
    LevelMean := 0.0;

    for J := 0 to I - 1 do
    begin
      Omega := Omega + Histogram[J];
      LevelMean := LevelMean + (J + 1) * Histogram[J];
    end;

    Mu := Sqr(Mean * Omega - LevelMean);
    Omega := Omega * (1.0 - Omega);

    if Omega > 0.0 then
      Mu := Mu / Omega
    else
      Mu := 0;

    if Mu > LargestMu then
    begin
      LargestMu := Mu;
      Level := I;
    end;
  end;

  // Do thresholding using computed level
  Pix := Image.Bits;
  for I := 0 to Image.Width * Image.Height - 1 do
  begin
    if Pix^ >= Level then
      Pix^ := 255
    else
      Pix^ := 0;
    Inc(Pix);
  end;

  Result := Level;
end;

{
  Calculates rotation angle for given 8bit grayscale image.
}
function CalcRotationAngle(MaxAngle: Integer; Treshold: Integer; Width, Height: Integer; Pixels: PByteArray): Double;
const
  // Number of "best" lines we take into account when determining
  // resulting rotation angle (lines with most votes).
  BestLinesCount = 20;
  // Angle step used in alpha parameter quantization
  AlphaStep = 0.1;
type
  TLine = record
    Count: Integer;
    Index: Integer;
    Alpha: Double;
    D: Double;
  end;
  TLineArray = array of TLine;
var
  AlphaStart, MinD, Sum: Double;
  AlphaSteps, DCount, AccumulatorSize, I: Integer;
  BestLines: TLineArray;
  HoughAccumulator: array of Integer;

  // Classifies pixel at [X, Y] as black or white using threshold.
  function IsPixelBlack(X, Y: Integer): Boolean;
  begin
    Result := Pixels[Y * Width + X] < Treshold;
  end;

  // Calculates alpha parameter for given angle step.
  function GetAlpha(Index: Integer): Double;
  begin
    Result := AlphaStart + Index * AlphaStep;
  end;

  function CalcDIndex(D: Double): Integer;
  begin
    Result := Trunc(D - MinD);
  end;

  // Calculates angle and distance parameters for all lines
  // going through point [X, Y].
  procedure CalcLines(X, Y: Integer);
  var
    D, Rads: Double;
    I, DIndex, Index: Integer;
  begin
    for I := 0 to AlphaSteps - 1 do
    begin
      Rads := GetAlpha(I) * Pi / 180; // Angle for current step in radians
      D := Y * Cos(Rads) - X * Sin(Rads); // Parameter D of the line y=tg(alpha)x + d
      DIndex := CalcDIndex(D);
      Index := DIndex * AlphaSteps + I;
      HoughAccumulator[Index] := HoughAccumulator[Index] + 1; // Add one vote for current line
    end;
  end;

  // Uses Hough transform to calculate all lines that intersect
  // interesting points (those classified as beign on base line of the text).
  procedure CalcHoughTransform;
  var
    Y, X: Integer;
  begin
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
      begin
        if IsPixelBlack(X, Y) and not IsPixelBlack(X, Y + 1) then
          CalcLines(X, Y);
      end;
  end;

  // Chooses "best" lines (with the most votes) from the accumulator
  function GetBestLines(Count: Integer): TLineArray;
  var
    I, J, DIndex, AlphaIndex: Integer;
    Temp: TLine;
  begin
    SetLength(Result, Count);

    for I := 0 to AccumulatorSize - 1 do
    begin
      if HoughAccumulator[I] > Result[Count - 1].Count then
      begin
        // Current line has more votes than the last selected one,
        // let's put it the pot
        Result[Count - 1].Count := HoughAccumulator[I];
        Result[Count - 1].Index := I;
        J := Count - 1;

        // Sort the lines based on number of votes
        while (J > 0) and (Result[J].Count > Result[J - 1].Count) do
        begin
          Temp := Result[J];
          Result[J] := Result[J - 1];
          Result[J - 1] := Temp;
          J := J - 1;
        end;
      end;
    end;

    for I := 0 to Count - 1 do
    begin
      // Caculate line angle and distance according to index in the accumulator
      DIndex := Result[I].Index div AlphaSteps;
      AlphaIndex := Result[I].Index - DIndex * AlphaSteps;
      Result[I].Alpha := GetAlpha(AlphaIndex);
      Result[I].D := DIndex + MinD;
    end;
  end;

begin
  AlphaStart := -MaxAngle;
  AlphaSteps := Round(2 * MaxAngle / AlphaStep); // Number of angle steps = samples from interval <-MaxAngle, MaxAngle>
  MinD := -Width;
  DCount := 2 * (Width + Height);

  // Determine the size of line accumulator
  AccumulatorSize := DCount * AlphaSteps;
  SetLength(HoughAccumulator, AccumulatorSize);

  // Calculate Hough transform
  CalcHoughTransform;

  // Get the best lines with most votes
  BestLines := GetBestLines(BestLinesCount);

  // Average angles of the selected lines to get the rotation angle of the image
  Sum := 0;
  for I := 0 to BestLinesCount - 1 do
    Sum := Sum + BestLines[I].Alpha;
  Result := Sum / BestLinesCount;
end;

procedure DoDeskew;
var
  Angle: Double;
begin
  WriteLn('Preparing input image...');

  // Clone input image and convert it to 8bit grayscale. This will be our
  // working image.
  CloneImage(InputImage, OutputImage);
  ConvertImage(InputImage, ifGray8);

  if AutoThreshold then
  begin
    // Determine the threshold automatically if needed.
    Threshold := OtsuThresholding(InputImage)
  end;

  // Main step - calculate image rotation angle
  WriteLn('Calculating skew angle...');
  Angle := CalcRotationAngle(MaxAngle, Threshold, InputImage.Width, InputImage.Height, InputImage.Bits);
  WriteLn('Skew angle found: ', Angle:4:2);

  // Finally, rotate the image. We rotate the original input image, not the working
  // one so the color space is preserved.
  WriteLn('Rotating image...');
  if OutputImage.Format = ifIndex8 then
    ConvertImage(OutputImage, ifA8R8G8B8); // Rotation doesn't like indexed images
  RotateImage(OutputImage, Angle);
end;

begin
  try
    // Parse command line
    CheckCommandLine;
    WriteLn('Deskew 0.95 (2010-12-28) by Marek Mauder');

    // Bad input
    if (InputFile = '') or (Threshold < 0) or (MaxAngle < 0) then
    begin
      WriteUsage;
      Halt(1);
    end;

    // Load input image
    if not LoadImageFromFile(InputFile, InputImage) then
    begin
      WriteLn('Error reading input image: ', InputFile);
      Halt(1);
    end;

    DoDeskew;

    // Save the output
    if not SaveImageToFile(OutputFile, OutputImage) then
    begin
      WriteLn('Error writing output image: ', InputFile);
      Halt(1);
    end;

    FreeImage(InputImage);
    FreeImage(OutputImage);

    WriteLn('Done!');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
