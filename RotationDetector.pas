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
    There are 2 * MaxAngle / AngleStep + 1 of them, from -MaxAngle to MaxAngle
    (when the range is not a whole number of steps the last one is below MaxAngle).
  - Threshold (0..255) is used to classify pixel as black (text) or white (background).
  - Width, Height, Pixels: the image in 8bit grayscale, rows stored one after another.

  Running time and memory needed grow with the number of tested angles. AngleStep also
  limits the accuracy, although the result is finer than the step since several lines
  are averaged.

  Area of interest rectangle can be defined to restrict the detection to
  work only in defined part of image (useful when the document has text only in
  smaller area of page and non-text features outside the area confuse the rotation detector).
  Only the part of the area inside the image is used.

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
  be used for anything serious unless Stats.Outcome is sdoDetected. It is 0 when
  the outcome is sdoNoContent.
  Confidence tells how much the peak angle stands out from the others. }
function CalcRotationAngle(const MaxAngle, AngleStep: Double; Threshold: Integer;
  Width, Height: Integer; Pixels: PBuffer; DetectionArea: PRect = nil;
  Stats: PCalcSkewAngleStats = nil): Double;

implementation

// Every baseline pixel [Xs[P], Ys[P]] votes for all the lines going through it,
// one line for each tested angle (sine and cosine in the tables). Votes of one
// angle go into its own block of DistCount distances of the accumulator.
// It is not nested in CalcRotationAngle: the inner loop is much faster when
// it does not reach the variables through the stack frame of the outer function.
procedure VoteForLines(const Xs, Ys: array of Integer; PointCount: Integer;
  const SinTable, CosTable: array of Extended; MinDist: Double; DistCount: Integer;
  var Accumulator: array of Integer);
var
  I, P, Base: Integer;
  SinAlpha, CosAlpha: Extended;  // Extended like the tables
  D: Double;
begin
  // The tables have one item for each tested angle (AlphaSteps of them)
  for I := 0 to High(SinTable) do
  begin
    SinAlpha := SinTable[I];
    CosAlpha := CosTable[I];
    Base := I * DistCount;
    for P := 0 to PointCount - 1 do
    begin
      // Parameter D (distance from origin) of the line y=tg(alpha)x + d
      D := Ys[P] * CosAlpha - Xs[P] * SinAlpha;
      Inc(Accumulator[Base + Trunc(D - MinDist)]);
    end;
  end;
end;

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
  // 4.8 million (19 MB) for A4 page at 600 DPI with default settings (201 angle steps).
  // The limit is hit only with extreme settings: with max angle 90 and angle step 0.01
  // (18001 angle steps) when width + height of the page is over 13888 pixels.
  MaxAccumulatorSize = 500 * 1000 * 1000;
  // Number of tested pixels that is always considered too low to detect anything
  MinTestedPixels = 16;
  // Baseline pixels are collected and vote in passes of about this many pixels,
  // so the memory for their coordinates stays small even for huge noisy images.
  // For usual scans baseline pixels for whole page fit into one pass.
  MaxPointsPerPass = 1024 * 1024;
  // Sharpness of the peak angle that gives full confidence. Clean text
  // pages are usually well above this.
  FullConfidenceSharpness = 0.5;
  // Average strength of lines (in votes) at the peak angle that gives full confidence.
  // It's only around one for sparse random dots (dust).
  FullConfidenceLineStrength = 10;
  // With max angle over this the distance of a line is measured along the dominant
  // axis (y-intercept up to 45 degrees) instead of perpendicular to the line.
  // Distances are counted in whole pixels: perpendicular to the line, the pixel
  // grid lands evenly in them only for some angles. At exactly +-45 degrees one
  // distance gets one or two pixel diagonals (0.707 apart), and that unevenness alone
  // adds about 10% to the score of the angle. Text skewed a lot gets competition from
  // the sides of its glyphs (text angle + 90), and the extra 10% made such pages end
  // at exactly +-45 degrees as detected (DISE-2021 +-45 test set: 173 of 2800 images).
  // Along the dominant axis every angle gets the grid evenly. Spikes of the other
  // simple slopes are small below 20 degrees (under 2%), so smaller max angles
  // keep the original distance and give the same results as before.
  AxisDistanceMinMaxAngle = 20;

type
  TLine = record
    Count: Integer;
    AlphaIndex, DistIndex: Integer;
    Alpha: Double;
    Distance: Double;
  end;
  TLineArray = array of TLine;

var
  // Area where the skew is detected and its size. PageHeight leaves out
  // the last row of the image, it has no row below for the baseline check.
  ContentRect: TRect;
  PageWidth, PageHeight: Integer;
  // Tested angles: AlphaSteps of them from AlphaStart (-MaxAngle) by AngleStep (degrees)
  AlphaStart: Double;
  AlphaSteps: Integer;
  // Sine and cosine of all the tested angles (array size = AlphaSteps). They are the same for
  // every pixel so it would be a waste to calculate them over and over.
  SinTable, CosTable: array of Extended;
  // Distances D of the lines from the origin.
  // With D = Y * Cos - X * Sin, X in [0, PageWidth) and Y in [0, PageHeight),
  // D lies strictly in [-PageWidth, PageWidth + PageHeight],
  // for angles up to 90 degrees (Cos not negative).

  // Since we use the line distance as index into the accumulator we need to shift it
  // by MinDist so it's not negative (index then as Trunc(D - MinDist), D of index 0 starts at MinDist).
  // DistCount distances are enough for a line through any pixel of the content
  // rect at any tested angle.
  MinDist: Double;
  DistCount: Integer;
  // Votes for lines: block of DistCount distances for each tested angle (angle-major).
  // For each angle block we track here all the lines in the same direction with any distance from
  // the origin (where distance fits into the page).
  // AccumulatorSize = AlphaSteps * DistCount
  HoughAccumulator: array of Integer;
  AccumulatorSize: Integer;
  // Coordinates of baseline pixels of the current pass, relative to the content rect
  PointsX, PointsY: array of Integer;
  PointCount: Integer;
  // Number of baseline pixels found in all the passes (sum of their PointCount),
  // each voted once for every tested angle
  TestedPixels: Int64;
  // Score of each tested angle: sum of squared votes of its lines
  AngleScores: array of Double;
  // Tested angle with the best score and its index, how much it stands out
  // from the other angles and how much the result can be trusted
  PeakAngle: Double;
  PeakIndex: Integer;
  Sharpness, Confidence: Double;
  Outcome: TSkewDetectionOutcome;
  // Lines with the most votes, best first
  BestLines: TLineArray;
  // Best lines at most Tolerance degrees from the peak angle (LinesAtPeak
  // of them) are averaged into the result
  Tolerance: Double;
  LinesAtPeak: Integer;
  // Helpers for the result
  I, LineCount: Integer;
  SumAngles: Double;

  // Calculates final angle for given angle step.
  function GetFinalAngle(StepIndex: Integer): Double;
  begin
    Result := AlphaStart + StepIndex * AngleStep;
  end;

  procedure CalcSinCosTables;
  var
    I: Integer;
    Rads, Scale: Double;
  begin
    SetLength(SinTable, AlphaSteps);
    SetLength(CosTable, AlphaSteps);
    for I := 0 to AlphaSteps - 1 do
    begin
      // Angle for current step in radians
      Rads := GetFinalAngle(I) * PI / 180;
      SinCos(Rads, SinTable[I], CosTable[I]);

      if MaxAngle > AxisDistanceMinMaxAngle then
      begin
        // Distance along the dominant axis (see AxisDistanceMinMaxAngle)
        if Abs(SinTable[I]) > Abs(CosTable[I]) then
          Scale := 1.0 / Abs(SinTable[I])
        else
          Scale := 1.0 / Abs(CosTable[I]);
        SinTable[I] := SinTable[I] * Scale;
        CosTable[I] := CosTable[I] * Scale;
      end;
    end;
  end;

  // Collects baseline pixels (black pixel, darker than Threshold, with a white
  // one directly below) from row Y on, until the rows end or there are enough
  // for one pass. Y is the next row to look at then.
  procedure FindBaselinePixels(var Y: Integer);
  var
    X: Integer;
    Row, RowBelow: PBuffer;
  begin
    PointCount := 0;
    // A row adds at most PageWidth pixels, so the arrays never overflow ("+ PageWidth" capacity)
    while (Y < PageHeight) and (PointCount <= MaxPointsPerPass) do
    begin
      // Pixels are indexed through Imaging's PBuffer (array pointers like PByteArray
      // cannot reach over 2 GB in Delphi).
      Row := @Pixels[NativeInt(ContentRect.Top + Y) * Width + ContentRect.Left];
      RowBelow := @Pixels[NativeInt(ContentRect.Top + Y + 1) * Width + ContentRect.Left];

      for X := 0 to PageWidth - 1 do
        if (Row[X] < Threshold) and (RowBelow[X] >= Threshold) then
        begin
          PointsX[PointCount] := X;
          PointsY[PointCount] := Y;
          Inc(PointCount);
        end;
      Inc(Y);
    end;
  end;

  // Uses Hough transform to calculate all lines that intersect
  // interesting points (those classified as being on base line of the text).
  procedure CalcHoughTransform;
  var
    Y, Capacity: Integer;
  begin
    // A pass ends after the row that reached MaxPointsPerPass.
    // For usual scans we fit into a single pass.
    Capacity := MaxPointsPerPass + PageWidth;
    if Int64(PageWidth) * PageHeight < Capacity then
      Capacity := Math.Max(0, PageWidth * PageHeight);

    SetLength(PointsX, Capacity);
    SetLength(PointsY, Capacity);
    Y := 0;

    while Y < PageHeight do
    begin
      FindBaselinePixels(Y);
      VoteForLines(PointsX, PointsY, PointCount, SinTable, CosTable, MinDist, DistCount,
        HoughAccumulator);
      Inc(TestedPixels, PointCount);
    end;
  end;

  // Tells whether line A goes before line B among the best lines: it has more
  // votes or, with the same votes, the smaller distance and then the smaller
  // angle index. The order of lines with the same votes matters for the result,
  // this is the one the detection was calibrated with.
  function IsBetterLine(const A, B: TLine): Boolean;
  begin
    if A.Count <> B.Count then
      Result := A.Count > B.Count
    else if A.DistIndex <> B.DistIndex then
      Result := A.DistIndex < B.DistIndex
    else
      Result := A.AlphaIndex < B.AlphaIndex;
  end;

  // Chooses "best" lines (with the most votes) from the accumulator
  // and calculates scores of all the tested angles on the way.
  function GetBestLines(Count: Integer): TLineArray;
  var
    I, J, AlphaIndex, DistIndex, Index: Integer;
    Line, Temp: TLine;
  begin
    SetLength(Result, Count);
    Index := 0;

    // Accumulator is visited in the order it is stored, angle by angle
    for AlphaIndex := 0 to AlphaSteps - 1 do
    begin
      // Now all the lines on the page with this angle (but differing distance from origin)
      for DistIndex := 0 to DistCount - 1 do
      begin
        Line.Count := HoughAccumulator[Index];
        Inc(Index);

        if Line.Count > 0 then
        begin
          AngleScores[AlphaIndex] := AngleScores[AlphaIndex] + Line.Count * 1.0 * Line.Count;
          Line.AlphaIndex := AlphaIndex;
          Line.DistIndex := DistIndex;

          // Empty places in the result have zero votes, every line is better
          if IsBetterLine(Line, Result[Count - 1]) then
          begin
            // Current line is better than the last selected one,
            // let's put it the pot and sort it in
            Result[Count - 1] := Line;
            J := Count - 1;
            while (J > 0) and IsBetterLine(Result[J], Result[J - 1]) do
            begin
              Temp := Result[J];
              Result[J] := Result[J - 1];
              Result[J - 1] := Temp;
              J := J - 1;
            end;
          end;
        end;
      end;
    end;

    for I := 0 to Count - 1 do
    begin
      Result[I].Alpha := GetFinalAngle(Result[I].AlphaIndex);
      Result[I].Distance := Result[I].DistIndex + MinDist;
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
    // Only the part of the area inside the image is used (pixels outside would be
    // read past the end of the buffer), an area completely outside ends as no content
    if not IntersectRect(ContentRect, DetectionArea^, Rect(0, 0, Width, Height)) then
      ContentRect := Rect(0, 0, 0, 0);
  end;

  PageWidth := ContentRect.Right - ContentRect.Left;
  PageHeight := ContentRect.Bottom - ContentRect.Top;
  if (ContentRect.Bottom = Height) then
    Dec(PageHeight); // Baseline pixel check looks at the row below too, it must not go outside of the image

  AlphaStart := -MaxAngle;
  // Number of tested angles: both ends of <-MaxAngle, MaxAngle> are included when the range
  // is a whole number of steps (epsilon for the rounding of the division, e.g. 20 / 0.1)
  AlphaSteps := Floor(2 * MaxAngle / AngleStep + 1E-6) + 1;

  if MaxAngle <= AxisDistanceMinMaxAngle then
  begin
    // Distance to the origin measured perpendicular to the line
    MinDist := -Max(PageWidth, PageHeight);
    DistCount := 2 * (PageWidth + PageHeight);
  end
  else
  begin
    // Distance along the dominant axis is in [-(PageWidth + PageHeight), PageWidth + PageHeight]
    MinDist := -(PageWidth + PageHeight);
    DistCount := 2 * (PageWidth + PageHeight) + 1;
  end;

  // Determine the size of line accumulator
  if Int64(DistCount) * AlphaSteps > MaxAccumulatorSize then
    raise Exception.Create('Image is too large for skew detection with the given ' +
      'max angle and angle step. Use smaller max angle or larger angle step.');

  AccumulatorSize := DistCount * AlphaSteps;
  SetLength(HoughAccumulator, AccumulatorSize);
  SetLength(AngleScores, AlphaSteps);

  // Calculate Hough transform
  CalcSinCosTables;
  CalcHoughTransform;

  // Get the best lines with most votes
  BestLines := GetBestLines(BestLinesCount);

  // Find the angle where the lines are the most pronounced
  EvaluateAngleScores;
  PeakAngle := GetFinalAngle(PeakIndex);

  // Average angles of the best lines that agree with the peak angle
  // to get the rotation angle of the image. Usually it's all of them.
  // There can be less lines than we asked for in (nearly) blank images.
  // Small addition keeps lines exactly at the tolerance in spite of rounding errors
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

  if (LineCount = 0) or (Outcome = sdoNoContent) then
    Result := 0 // Nothing to detect from: angles of a few single pixels mean nothing
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
