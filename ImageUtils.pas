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
  Unit with various image processing functions. Some are taken from
  Imaging extensions.
}
unit ImageUtils;

{$I ImagingOptions.inc}

interface

uses
  SysUtils,
  Classes,
  ImagingTypes,
  Imaging,
  ImagingClasses,
{$IF not Defined(MACOS) and (Defined(FPC) or
  (Defined(DCC) and Defined(MSWINDOWS) and Defined(CPUX86)))}
  ImagingTiffLib,
{$ELSEIF Defined(MACOS)}
  // Not yet ready
  //ImagingTiffMac,
{$IFEND}
  ImagingPsd, // PSD support
  ImagingCanvases,
  ImagingFormats,
  ImagingUtility;

{ Thresholding using Otsu's method (which chooses the threshold
  to minimize the intraclass variance of the black and white pixels!).
  Functions returns calculated threshold level value [0..255].
  If BinarizeImage is True then the Image is automatically converted to binary using
  computed threshold level.}
function OtsuThresholding(var Image: TImageData; BinarizeImage: Boolean = False): Integer;
{ Merges 32bit image with a background of specified color. }
procedure MergeWithBackground(Image: TSingleImage; BackgroundColor: TColor32);

implementation

function OtsuThresholding(var Image: TImageData; BinarizeImage: Boolean): Integer;
var
  Histogram: array[Byte] of Single;
  Level, Max, Min, I, J, NumPixels: Integer;
  Pix: PByte;
  Mean, Variance: Single;
  Mu, Omega, LevelMean, LargestMu: Single;
begin
  Assert(Image.Format = ifGray8);

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

  if BinarizeImage then
  begin
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
  end;

  Result := Level;
end;

procedure MergeWithBackground(Image: TSingleImage; BackgroundColor: TColor32);
var
  Back: TSingleImage;
  Canvas, BackCanvas: TImagingCanvas;
begin
  Assert(Image.Format = ifA8R8G8B8);

  Back := TSingleImage.CreateFromParams(Image.Width, Image.Height, ifA8R8G8B8);
  Canvas := TFastARGB32Canvas.CreateForImage(Image);
  BackCanvas := TFastARGB32Canvas.CreateForImage(Back);
  try
    BackCanvas.FillColor32 := BackgroundColor;
    BackCanvas.FillRect(Back.BoundsRect);
    Canvas.DrawAlpha(Image.BoundsRect, BackCanvas, 0, 0);
    Back.CopyTo(0, 0, Image.Width, Image.Height, Image, 0, 0);
  finally
    Back.Free;
    Canvas.Free;
    BackCanvas.Free;
  end;
end;

end.