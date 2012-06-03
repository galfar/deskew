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
unit RotationDetector;

interface

uses
  Types,
  ImagingUtility;

type
  TCalcSkewAngleStats = record
    PixelCount: Integer;
    TestedPixels: Integer;
    AccumulatorSize: Integer;
    AccumulatedCounts: Integer;
    BestCount: Double;
  end;
  PCalcSkewAngleStats = ^TCalcSkewAngleStats;

{ Calculates rotation angle for given 8bit grayscale image.
  Useful for finding skew of scanned documents etc.
  Uses Hough transform internally.
  MaxAngle is maximal (abs. value) expected skew angle in degrees (to speed things up)
  and Threshold (0..255) is used to classify pixel as black (text) or white (background).}
function CalcRotationAngle(MaxAngle: Integer; Treshold: Integer;
  Width, Height: Integer; Pixels: PByteArray; Margins: PRect = nil;
  Stats: PCalcSkewAngleStats = nil): Double;

implementation

function CalcRotationAngle(MaxAngle: Integer; Treshold: Integer;
  Width, Height: Integer; Pixels: PByteArray; Margins: PRect; Stats: PCalcSkewAngleStats): Double;
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
  AlphaStart, MinD, SumAngles: Double;
  AlphaSteps, DCount, AccumulatorSize, I, AccumulatedCounts: Integer;
  BestLines: TLineArray;
  HoughAccumulator: array of Integer;
  PageWidth, PageHeight: Integer;
  PageMargins: TRect;

  // Classifies pixel at [X, Y] as black or white using threshold.
  function IsPixelBlack(X, Y: Integer): Boolean;
  begin
    Result := Pixels[(PageMargins.Top + Y) * Width + PageMargins.Left + X] < Treshold;
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
    for Y := 0 to PageHeight - 1 do
      for X := 0 to PageWidth - 1 do
      begin
        if IsPixelBlack(X, Y) and not IsPixelBlack(X, Y + 1) then
        begin
          CalcLines(X, Y);
        end;
      end;
  end;

  // Chooses "best" lines (with the most votes) from the accumulator
  function GetBestLines(Count: Integer): TLineArray;
  var
    I, J, DIndex, AlphaIndex: Integer;
    Temp: TLine;
  begin
    AccumulatedCounts := 0;
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

      AccumulatedCounts := AccumulatedCounts + HoughAccumulator[I];
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
  // Use supplied page margins or just the whole image
  if Margins = nil then
    PageMargins := Rect(0, 0, Width, Height)
  else
    PageMargins := Margins^;

  PageWidth := PageMargins.Right - PageMargins.Left;
  PageHeight := PageMargins.Bottom - PageMargins.Top;

  AlphaStart := -MaxAngle;
  AlphaSteps := Round(2 * MaxAngle / AlphaStep); // Number of angle steps = samples from interval <-MaxAngle, MaxAngle>
  MinD := -Width;
  DCount := 2 * (PageWidth + PageHeight);

  // Determine the size of line accumulator
  AccumulatorSize := DCount * AlphaSteps;
  SetLength(HoughAccumulator, AccumulatorSize);

  // Calculate Hough transform
  CalcHoughTransform;

  // Get the best lines with most votes
  BestLines := GetBestLines(BestLinesCount);

  // Average angles of the selected lines to get the rotation angle of the image
  SumAngles := 0;
  for I := 0 to BestLinesCount - 1 do
    SumAngles := SumAngles + BestLines[I].Alpha;

  Result := SumAngles / BestLinesCount;

  if Stats <> nil then
  begin
    Stats.BestCount := BestLines[0].Count;
    Stats.PixelCount := PageWidth * PageHeight;
    Stats.AccumulatorSize := AccumulatorSize;
    Stats.AccumulatedCounts := AccumulatedCounts;
    Stats.TestedPixels := AccumulatedCounts div AlphaSteps;
  end;
end;


end.
