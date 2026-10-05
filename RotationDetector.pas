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
unit RotationDetector;

interface

uses
  Types,
  SysUtils,
  Math,
  ImagingTypes,
  ImagingUtility;

type
  { How the skew detection ended. Angle is always calculated but it can
    be trusted only when the outcome is sdoDetected. }
  TSkewDetectionOutcome = (
    // Dominant direction of the lines was found
    sdoDetected,
    // Not enough content to detect the skew from (blank page etc.)
    sdoNoContent,
    // There is some content but without a dominant direction in the tested
    // angle range (noise, photos, skew much larger than max angle, ...)
    sdoLowConfidence,
    // Lines get stronger towards the limit of the tested angle range,
    // actual skew is most probably just outside of it
    sdoAtRangeLimit
  );

  TCalcSkewAngleStats = record
    PixelCount: Int64;
    TestedPixels: Int64;
    AccumulatorSize: Int64;
    AccumulatedCounts: Int64;
    BestCount: Integer;
    // How the detection ended
    Outcome: TSkewDetectionOutcome;
    // Estimated reliability of the detected angle in range [0, 1],
    // see MinDetectionConfidence.
    Confidence: Double;
    // Tested angle with the highest score - where the lines are the most pronounced (degrees)
    PeakAngle: Double;
    // How much the peak angle stands out from all the tested angles [0, 1]
    Sharpness: Double;
    // Number of the best lines that agree with the peak angle
    LinesAtPeak: Integer;
  end;
  PCalcSkewAngleStats = ^TCalcSkewAngleStats;

const
  // Detection with lower confidence ends as sdoLowConfidence
  MinDetectionConfidence = 0.3;

{ Calculates rotation angle for given 8bit grayscale image.
  Useful for finding skew of scanned documents etc.
  Uses Hough transform internally.

  Parameters:
  - MaxAngle: the largest skew angle that is looked for, in degrees. Angles in the range
    [-MaxAngle, MaxAngle] are tested, skew outside of it cannot be found.
  - AngleStep: distance between two neighboring tested angles, in degrees.
    There are 2 * MaxAngle / AngleStep of them.
  - Threshold (0..255) is used to classify pixel as black (text) or white (background).
  - Width, Height, Pixels: the image in 8bit grayscale, rows stored one after another.

  Running time and memory needed grow with the number of tested angles. AngleStep also
  limits the accuracy, although the result is finer than the step since several lines
  are averaged.

  Area of interest rectangle can be defined to restrict the detection to
  work only in defined part of image (useful when the document has text only in
  smaller area of page and non-text features outside the area confuse the rotation detector).

  How the angle is determined:
  1. Each "base line" pixel (black with white one below) votes for all the lines
     going through it (pairs of angle and distance in Hough accumulator).
  2. Each tested angle gets a score: sum of squared votes of all its lines.
     Votes concentrate in a few strong lines when the angle matches the direction
     of text lines, otherwise they are spread thin. Angle with the best score
     is the peak angle. All the lines contribute so a few long lines going in
     a different direction (page edges, borders) cannot outvote the text.
  3. Resulting angle is the average of angles of the strongest lines that agree
     with the peak angle. It's finer than the angle step. When the strongest lines
     do not agree with the peak (they belong to page edges etc.) the peak angle
     itself is used (refined by interpolation of the neighbor scores).

  Various calculations stats can be retrieved by passing Stats parameter. This includes
  the outcome of the detection and its confidence - the returned angle should not
  be used for anything serious unless Stats.Outcome is sdoDetected.
  Confidence tells how much the peak angle stands out from the others. }
function CalcRotationAngle(const MaxAngle, AngleStep: Double; Threshold: Integer;
  Width, Height: Integer; Pixels: PBuffer; DetectionArea: PRect = nil;
  Stats: PCalcSkewAngleStats = nil): Double;

implementation

function CalcRotationAngle(const MaxAngle, AngleStep: Double; Threshold: Integer;
  Width, Height: Integer; Pixels: PBuffer; DetectionArea: PRect; Stats: PCalcSkewAngleStats): Double;
const
  // Number of "best" lines (with most votes) we take into account
  // when determining resulting rotation angle.
  BestLinesCount = 20;
  // Best lines must be this close to the peak angle to be used for the resulting
  // angle (degrees, at least two angle steps though) ...
  PeakAngleTolerance = 0.5;
  // ... and there must be at least this many of them.
  MinLinesAtPeak = 3;
  // Upper limit for the number of lines (angle/distance pairs) in the accumulator.
  // Each line takes 4 bytes, so this is 2 GB of memory. It also keeps the line
  // indexes safely within 32-bit integers.
  // Number of lines is 2 * (width + height) * (2 * max angle / angle step):
  // 4.8 million (19 MB) for A4 page at 600 DPI with default settings (200 angle steps).
  // The limit is hit only with extreme settings: with max angle 90 and angle step 0.01
  // (18000 angle steps) when width + height of the page is over 13888 pixels.
  MaxAccumulatorSize = 500 * 1000 * 1000;
  // Number of tested pixels that is always considered too low to detect anything
  MinTestedPixels = 16;
  // Sharpness of the peak angle that gives full confidence. Clean text
  // pages are usually well above this.
  FullConfidenceSharpness = 0.5;
  // Average strength of lines (in votes) at the peak angle that gives full confidence.
  // It's only around one for sparse random dots (dust).
  FullConfidenceLineStrength = 10;
type
  TLine = record
    Count: Integer;
    Index: Integer;
    Alpha: Double;
    Distance: Double;
  end;
  TLineArray = array of TLine;
var
  AlphaStart, MinDist, SumAngles, PeakAngle, Tolerance: Double;
  AlphaSteps, DistCount, AccumulatorSize, I, LineCount, LinesAtPeak, PeakIndex: Integer;
  TestedPixels: Int64;
  BestLines: TLineArray;
  HoughAccumulator: array of Integer;
  AngleScores: array of Double;
  PageWidth, PageHeight: Integer;
  ContentRect: TRect;
  Outcome: TSkewDetectionOutcome;
  Sharpness, Confidence: Double;

  // Classifies pixel at [X, Y] as black or white using threshold.
  // Pixels are indexed through PBuffer: array pointers like PByteArray
  // cannot reach over 2 GB in Delphi.
  function IsPixelBlack(X, Y: Integer): Boolean;
  begin
    Result := Pixels[NativeInt(Y) * Width + X] < Threshold;
  end;

  // Calculates final angle for given angle step.
  function GetFinalAngle(StepIndex: Integer): Double;
  begin
    Result := AlphaStart + StepIndex * AngleStep;
  end;

  // Calculates angle and distance parameters for all lines
  // going through point [X, Y].
  procedure CalcLines(X, Y: Integer);
  var
    D, Rads: Double;
    I, DIndex, Index: Integer;
    Sin, Cos: Extended;
  begin
    for I := 0 to AlphaSteps - 1 do
    begin
      // Angle for current step in radians
      Rads := GetFinalAngle(I) * PI / 180;
      SinCos(Rads, Sin, Cos);
      // Parameter D(distance from origin) of the line y=tg(alpha)x + d
      D := Y * Cos - X * Sin;
      // Calc index into accumulator for current line
      DIndex := Trunc(D - MinDist);
      Index := DIndex * AlphaSteps + I;
      // Add one vote for current line
      HoughAccumulator[Index] := HoughAccumulator[Index] + 1;
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
        if IsPixelBlack(ContentRect.Left + X, ContentRect.Top + Y) and
          not IsPixelBlack(ContentRect.Left + X, ContentRect.Top + Y + 1) then
        begin
          CalcLines(X, Y);
          Inc(TestedPixels);
        end;
      end;
  end;

  // Chooses "best" lines (with the most votes) from the accumulator
  // and calculates scores of all the tested angles on the way.
  function GetBestLines(Count: Integer): TLineArray;
  var
    I, J, DistIndex, AlphaIndex, Votes: Integer;
    Temp: TLine;
  begin
    SetLength(Result, Count);
    I := 0;

    for DistIndex := 0 to DistCount - 1 do
      for AlphaIndex := 0 to AlphaSteps - 1 do
      begin
        Votes := HoughAccumulator[I];

        if Votes > 0 then
        begin
          AngleScores[AlphaIndex] := AngleScores[AlphaIndex] + Votes * 1.0 * Votes;

          if Votes > Result[Count - 1].Count then
          begin
            // Current line has more votes than the last selected one,
            // let's put it the pot
            Result[Count - 1].Count := Votes;
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

        Inc(I);
      end;

    for I := 0 to Count - 1 do
    begin
      // Caculate line angle and distance according to index in the accumulator
      DistIndex := Result[I].Index div AlphaSteps;
      AlphaIndex := Result[I].Index - DistIndex * AlphaSteps;
      Result[I].Alpha := GetFinalAngle(AlphaIndex);
      Result[I].Distance := DistIndex + MinDist;
    end;
  end;

  // Finds the tested angle with the best score and estimates
  // how much it can be trusted.
  procedure EvaluateAngleScores;
  var
    I: Integer;
    MaxScore, SumScores, LineStrength: Double;
  begin
    PeakIndex := 0;
    Sharpness := 0;
    Confidence := 0;

    MaxScore := 0;
    SumScores := 0;
    for I := 0 to AlphaSteps - 1 do
    begin
      SumScores := SumScores + AngleScores[I];
      if AngleScores[I] > MaxScore then
      begin
        MaxScore := AngleScores[I];
        PeakIndex := I;
      end;
    end;

    if (TestedPixels < MinTestedPixels) or (MaxScore <= 0) then
    begin
      Outcome := sdoNoContent;
      Exit;
    end;

    // Sharpness: how much the peak stands out from all the tested angles.
    // It's close to zero when the angles are all more or less the same (noise, photos)
    // or when the score just slowly rises towards real peak far outside of the tested range.
    Sharpness := 1.0 - (SumScores / AlphaSteps) / MaxScore;

    // Score of an angle divided by number of the tested pixels is the average strength
    // of the line a pixel belongs to. Real lines have many pixels,
    // random dots stay mostly alone.
    LineStrength := MaxScore / TestedPixels;

    Confidence := Math.Min(1.0, Sharpness / FullConfidenceSharpness) *
      EnsureRange((LineStrength - 1.0) / (FullConfidenceLineStrength - 1.0), 0.0, 1.0);

    if Confidence < MinDetectionConfidence then
      Outcome := sdoLowConfidence
    else if (AlphaSteps > 2) and ((PeakIndex = 0) or (PeakIndex = AlphaSteps - 1)) then
      Outcome := sdoAtRangeLimit
    else
      Outcome := sdoDetected;
  end;

  // Position of the score peak refined by fitting a parabola to its neighbors.
  function GetInterpolatedPeakAngle: Double;
  var
    Curvature: Double;
  begin
    Result := PeakAngle;
    if (PeakIndex > 0) and (PeakIndex < AlphaSteps - 1) then
    begin
      Curvature := AngleScores[PeakIndex - 1] - 2 * AngleScores[PeakIndex] + AngleScores[PeakIndex + 1];
      if Curvature < 0 then
        Result := Result + AngleStep * 0.5 * (AngleScores[PeakIndex - 1] - AngleScores[PeakIndex + 1]) / Curvature;
    end;
  end;

begin
  TestedPixels := 0;
  Outcome := sdoDetected;

  // Use supplied page content rect or just the whole image
  ContentRect := Rect(0, 0, Width, Height);
  if DetectionArea <> nil then
  begin
    Assert((RectWidth(DetectionArea^) <= Width) and (RectHeight(DetectionArea^) <= Height));
    ContentRect := DetectionArea^;
  end;

  PageWidth := ContentRect.Right - ContentRect.Left;
  PageHeight := ContentRect.Bottom - ContentRect.Top;
  if (ContentRect.Bottom = Height) then
    Dec(PageHeight); // Don't check for black pixels outsize of image in CalcHoughTransform()

  AlphaStart := -MaxAngle;
  AlphaSteps := Ceil(2 * MaxAngle / AngleStep); // Number of angle steps = samples from interval <-MaxAngle, MaxAngle>
  MinDist := -Max(PageWidth, PageHeight);
  DistCount := 2 * (PageWidth + PageHeight);

  // Determine the size of line accumulator
  if Int64(DistCount) * AlphaSteps > MaxAccumulatorSize then
    raise Exception.Create('Image is too large for skew detection with the given ' +
      'max angle and angle step. Use smaller max angle or larger angle step.');

  AccumulatorSize := DistCount * AlphaSteps;
  SetLength(HoughAccumulator, AccumulatorSize);
  SetLength(AngleScores, AlphaSteps);

  // Calculate Hough transform
  CalcHoughTransform;

  // Get the best lines with most votes
  BestLines := GetBestLines(BestLinesCount);

  // Find the angle where the lines are the most pronounced
  EvaluateAngleScores;
  PeakAngle := GetFinalAngle(PeakIndex);

  // Average angles of the best lines that agree with the peak angle
  // to get the rotation angle of the image. Usually it's all of them.
  // There can be less lines than we asked for in (nearly) blank images.
  Tolerance := Math.Max(PeakAngleTolerance, 2 * AngleStep) + 1E-9;
  SumAngles := 0;
  LineCount := 0;
  LinesAtPeak := 0;

  for I := 0 to BestLinesCount - 1 do
    if BestLines[I].Count > 0 then
    begin
      Inc(LineCount);
      if Abs(BestLines[I].Alpha - PeakAngle) <= Tolerance then
      begin
        SumAngles := SumAngles + BestLines[I].Alpha;
        Inc(LinesAtPeak);
      end;
    end;

  if LineCount = 0 then
    Result := 0 // No black pixel at all
  else if LinesAtPeak >= MinLinesAtPeak then
    Result := SumAngles / LinesAtPeak
  else
    Result := GetInterpolatedPeakAngle; // Best lines are something else than text (page edges, ...)

  if Stats <> nil then
  begin
    Stats.BestCount := BestLines[0].Count;
    Stats.PixelCount := Int64(PageWidth) * PageHeight;
    Stats.AccumulatorSize := AccumulatorSize;
    Stats.AccumulatedCounts := TestedPixels * AlphaSteps;
    Stats.TestedPixels := TestedPixels;
    Stats.Outcome := Outcome;
    Stats.Confidence := Confidence;
    Stats.PeakAngle := PeakAngle;
    Stats.Sharpness := Sharpness;
    Stats.LinesAtPeak := LinesAtPeak;
  end;
end;


end.
