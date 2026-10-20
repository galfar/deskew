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

{$I BuildTarget.inc}

// Checks if all rect values are zero
function IsRectNull(const R: TRect): Boolean;
// Checks if all rect values are zero. Exact bitwise zero - do not use
// for rects where values were calculated.
function IsFloatRectNull(const R: TFloatRect): Boolean;

function RectToStr(const R: TRect): string;

function CalcRectInPixels(const RectInUnits: TFloatRect; SizeUnit: TSizeUnit;
  const ImageBoundsPx: TRect;  Metadata: TMetadata): TRect;

{ True when the path ends with a path delimiter. Slash counts on Windows too
  (Delphi's IncludeTrailingPathDelimiter does not know it there).}
function EndsWithPathDelimiter(const Path: string): Boolean;
{ True when the path names a directory: it ends with a path delimiter
  (does not have to exist then) or it is an existing directory.}
function IsDirectoryName(const Path: string): Boolean;

type
  { Simple writer of JSON documents for the machine readable reports.
    Output is plain ASCII (other characters are escaped) so it is valid UTF-8
    and does not depend on the code page of the console or the system.
    Numbers do not depend on the system locale. }
  TJsonWriter = class
  private
    FText: string;
    // One item for each open object: True when it has some member already
    FHasMembers: array of Boolean;
    procedure WriteName(const Name: string);
    procedure AddRaw(const Name, RawValue: string);
  public
    constructor Create;

    // Root object is opened with an empty name
    procedure BeginObject(const Name: string = '');
    procedure EndObject;

    procedure AddString(const Name, Value: string);
    procedure AddInt(const Name: string; const Value: Int64);
    // Value is rounded to the given number of decimal places.
    // Not a number and infinity are written as null.
    procedure AddFloat(const Name: string; const Value: Double; Decimals: Integer = 6);
    procedure AddBool(const Name: string; Value: Boolean);
    procedure AddNull(const Name: string);
    // Written as array [left, top, right, bottom]
    procedure AddRect(const Name: string; const R: TRect);
    procedure AddFloatRect(const Name: string; const Left, Top, Right, Bottom: Double);

    // Text of the document, all the objects must be closed already
    function GetText: string;
  end;

// Returns string as JSON string literal including the quotes.
function JsonQuote(const S: UnicodeString): string;
// Returns number as JSON number literal rounded to the given number of decimal places.
function JsonFloat(const Value: Double; Decimals: Integer = 6): string;

implementation

uses
  Math;

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

function EndsWithPathDelimiter(const Path: string): Boolean;
begin
  Result := (Path <> '') and CharInSet(Path[Length(Path)], ['/', PathDelim]);
end;

function IsDirectoryName(const Path: string): Boolean;
begin
  Result := EndsWithPathDelimiter(Path) or ((Path <> '') and DirectoryExists(Path));
end;

const
  SIndent = '  ';
  SNull = 'null';

function JsonQuote(const S: UnicodeString): string;
var
  I: Integer;
  C: WideChar;
begin
  Result := '"';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case Ord(C) of
      Ord('"'):  Result := Result + '\"';
      Ord('\'):  Result := Result + '\\';
      8:         Result := Result + '\b';
      9:         Result := Result + '\t';
      10:        Result := Result + '\n';
      12:        Result := Result + '\f';
      13:        Result := Result + '\r';
      32..33, 35..91, 93..126:
        Result := Result + Char(Ord(C));
    else
      // Control and non-ASCII characters (surrogate pairs as two escapes)
      Result := Result + '\u' + IntToHex(Ord(C), 4);
    end;
  end;
  Result := Result + '"';
end;

function JsonFloat(const Value: Double; Decimals: Integer): string;
var
  Scale, Rounded: Double;
begin
  if IsNan(Value) or IsInfinite(Value) then
    Exit(SNull);

  Scale := IntPower(10, Decimals);
  Rounded := Value;
  // Leave huge numbers as they are, they have no decimal places to round anyway
  if Abs(Value) < 1E15 / Scale then
    Rounded := Round(Value * Scale) / Scale;

  Result := FloatToStrF(Rounded, ffGeneral, 15, 0, GetFormatSettingsForFloats);
end;

// Strings in FPC are in the system code page (UTF-8 outside of Windows),
// in Delphi they are in Unicode already.
function ToUnicode(const S: string): UnicodeString;
begin
{$IF Defined(FPC) and not Defined(MSWINDOWS)}
  Result := UTF8Decode(S);
{$ELSE}
  Result := UnicodeString(S);
{$IFEND}
end;

{ TJsonWriter }

constructor TJsonWriter.Create;
begin
  FText := '';
  SetLength(FHasMembers, 0);
end;

procedure TJsonWriter.WriteName(const Name: string);
var
  I, Level: Integer;
begin
  Level := Length(FHasMembers);
  if Level = 0 then
  begin
    // Root value has no name
    Assert((Name = '') and (FText = ''));
    Exit;
  end;

  Assert(Name <> '');
  if FHasMembers[Level - 1] then
    FText := FText + ',';
  FHasMembers[Level - 1] := True;

  FText := FText + sLineBreak;
  for I := 1 to Level do
    FText := FText + SIndent;
  FText := FText + JsonQuote(ToUnicode(Name)) + ': ';
end;

procedure TJsonWriter.AddRaw(const Name, RawValue: string);
begin
  WriteName(Name);
  FText := FText + RawValue;
end;

procedure TJsonWriter.BeginObject(const Name: string);
begin
  WriteName(Name);
  FText := FText + '{';
  SetLength(FHasMembers, Length(FHasMembers) + 1);
  FHasMembers[High(FHasMembers)] := False;
end;

procedure TJsonWriter.EndObject;
var
  I, Level: Integer;
begin
  Level := Length(FHasMembers);
  Assert(Level > 0);

  if FHasMembers[Level - 1] then
  begin
    FText := FText + sLineBreak;
    for I := 1 to Level - 1 do
      FText := FText + SIndent;
  end;
  FText := FText + '}';
  SetLength(FHasMembers, Level - 1);
end;

procedure TJsonWriter.AddString(const Name, Value: string);
begin
  AddRaw(Name, JsonQuote(ToUnicode(Value)));
end;

procedure TJsonWriter.AddInt(const Name: string; const Value: Int64);
begin
  AddRaw(Name, IntToStr(Value));
end;

procedure TJsonWriter.AddFloat(const Name: string; const Value: Double; Decimals: Integer);
begin
  AddRaw(Name, JsonFloat(Value, Decimals));
end;

procedure TJsonWriter.AddBool(const Name: string; Value: Boolean);
begin
  if Value then
    AddRaw(Name, 'true')
  else
    AddRaw(Name, 'false');
end;

procedure TJsonWriter.AddNull(const Name: string);
begin
  AddRaw(Name, SNull);
end;

procedure TJsonWriter.AddRect(const Name: string; const R: TRect);
begin
  AddRaw(Name, Format('[%d, %d, %d, %d]', [R.Left, R.Top, R.Right, R.Bottom]));
end;

procedure TJsonWriter.AddFloatRect(const Name: string; const Left, Top, Right, Bottom: Double);
begin
  AddRaw(Name, '[' + JsonFloat(Left) + ', ' + JsonFloat(Top) + ', ' +
    JsonFloat(Right) + ', ' + JsonFloat(Bottom) + ']');
end;

function TJsonWriter.GetText: string;
begin
  Assert(Length(FHasMembers) = 0);
  Result := FText;
end;

end.
