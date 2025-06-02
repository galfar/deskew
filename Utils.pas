unit Utils;

interface

uses
  Types,
  SysUtils,
  Imaging,
  ImagingUtility;

type
  TSizeUnit = (
    suPixels,
    suPercent,
    suMm,
    suCm,
    suInch
  );

const
  NullRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
  NullFloatRect: TFloatRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

// Checks if all rect values are zero
function IsRectNull(const R: TRect): Boolean;
// Checks if all rect values are zero. Exact bitwise zero - do not use
// for rects where values were calculated.
function IsFloatRectNull(const R: TFloatRect): Boolean;

function RectToStr(const R: TRect): string;

function CalcRectInPixels(const RectInUnits: TFloatRect; SizeUnit: TSizeUnit;
  const ImageBoundsPx: TRect;  Metadata: TMetadata): TRect;

implementation

function MakeScaledRect(const R: TFloatRect; WidthFactor, HeightFactor: Double): TRect;
begin
  Result := Rect(Round(R.Left * WidthFactor),
                 Round(R.Top * HeightFactor),
                 Round(R.Right * WidthFactor),
                 Round(R.Bottom * HeightFactor));
end;

function IsRectNull(const R: TRect): Boolean;
begin
  Result := (R.Left = 0) and (R.Top = 0) and (R.Right = 0) and (R.Bottom = 0);
end;

function IsFloatRectNull(const R: TFloatRect): Boolean;
begin
  Result := (R.Left = 0) and (R.Top = 0) and (R.Right = 0) and (R.Bottom = 0);
end;

function RectToStr(const R: TRect): string;
begin
  Result := Format('[%d,%d,%d,%d]', [R.Left, R.Top, R.Right, R.Bottom]);
end;

function CalcRectInPixels(const RectInUnits: TFloatRect; SizeUnit: TSizeUnit;
  const ImageBoundsPx: TRect;  Metadata: TMetadata): TRect;
var
  PixXSize, PixYSize: Double;
begin
  Assert(not IsRectEmpty(ImageBoundsPx) and not IsFloatRectNull(RectInUnits));
  Result := NullRect;

  case SizeUnit of
    suPixels:
      Result := MakeScaledRect(RectInUnits, 1, 1);
    suPercent:
      Result := MakeScaledRect(RectInUnits,
                               RectWidth(ImageBoundsPx) / 100,
                               RectHeight(ImageBoundsPx) / 100);
    suMm:
      begin
        if not Metadata.GetPhysicalPixelSize(TResolutionUnit.ruDpcm, PixXSize, PixYSize) then
          Exit;
        Result := MakeScaledRect(RectInUnits, PixXSize / 10, PixYSize / 10);
      end;
    suCm:
      begin
        if not Metadata.GetPhysicalPixelSize(TResolutionUnit.ruDpcm, PixXSize, PixYSize) then
          Exit;
        Result := MakeScaledRect(RectInUnits, PixXSize, PixYSize);
      end;
    suInch:
      begin
        if not Metadata.GetPhysicalPixelSize(TResolutionUnit.ruDpi, PixXSize, PixYSize) then
          Exit;
        Result := MakeScaledRect(RectInUnits, PixXSize, PixYSize);
      end;
    else
      Assert(False);
  end;
end;


end.
