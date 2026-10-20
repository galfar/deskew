unit DeskewTestUtils;

interface

uses
  Types, Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFramework,
{$ENDIF}
  ImagingTypes,
  Imaging,
  ImagingClasses,
  ImagingUtility,
  Utils;

type
  { Shortcuts for preparing and checking the test images }
  TImageHelper = class helper for TBaseImage
  public
    procedure Fill(Color: Pointer); overload;
    procedure Fill(AValue: Byte); overload;
    // Rectangle is clipped to the image
    procedure FillRect(const ARect: TRect; Color: Pointer); overload;
    procedure FillRect(const ARect: TRect;  AValue: Byte); overload;
    function GetPixelByte(X, Y: Integer): Byte;
    function GetPixelColor32(X, Y: Integer): TColor32Rec;
  end;

  TDeskewTestCase = class(TTestCase)
  protected
{$IFNDEF FPC}
    procedure AssertTrue(const Msg: string; const ACondition: Boolean;
      AddrOfError: Pointer = nil); overload;
    procedure AssertTrue(const ACondition: Boolean); overload;
    procedure AssertFalse(const Msg: string; const ACondition: Boolean;
      AddrOfError: Pointer = nil); overload;

    procedure AssertEquals(const AMessage: string; Expected, Actual: string); overload;
    procedure AssertEquals(Expected, Actual: string); overload;
{$ENDIF}

    procedure AssertEquals(const AMessage: string; const Expected, Actual: Double); overload;
    procedure AssertEquals(const AMessage: string; const Expected, Actual: TFloatRect); overload;
    procedure AssertEquals(const AMessage: string; const Expected, Actual: TRect); overload;
    procedure AssertEquals(const AMessage: string; const Expected, Actual: TColor24Rec); overload;
    procedure AssertEquals(const AMessage: string; const Expected, Actual: TColor32Rec); overload;
  end;

  TTestClass = class of TDeskewTestCase;

procedure RegisterTest(TestClass: TTestClass); overload;

implementation

uses
  Math;

const
  Epsilon = 0.001;

const
  SCompare3Ints = '"%s" expected: <%d,%d,%d> but was: <%d,%d,%d>';
  SCompare4Ints = '"%s" expected: <%d,%d,%d,%d> but was: <%d,%d,%d,%d>';

procedure RegisterTest(TestClass: TTestClass);
begin
  {$IFDEF FPC}testregistry.{$ENDIF}RegisterTest(TestClass{$IFNDEF FPC}.Suite{$ENDIF});
end;

{ TImageHelper }

procedure TImageHelper.Fill(Color: Pointer);
begin
  FillRect(BoundsRect, Color);
end;

procedure TImageHelper.FillRect(const ARect: TRect; Color: Pointer);
begin
  Assert(Valid);
  Imaging.FillRect(ImageDataPointer^, ARect.Left, ARect.Top, ARect.Right - ARect.Left,
    ARect.Bottom - ARect.Top, Color);
end;

procedure TImageHelper.Fill(AValue: Byte);
begin
  Self.Fill(@AValue);
end;

procedure TImageHelper.FillRect(const ARect: TRect;  AValue: Byte);
begin
  Self.FillRect(ARect, @AValue);
end;

function TImageHelper.GetPixelByte(X, Y: Integer): Byte;
begin
  Assert(Valid and (Format = ifGray8));
  Assert((X >= 0) and (X < Width) and (Y >= 0) and (Y < Height));
  Result := PByteArray(Scanline[Y])[X];
end;

function TImageHelper.GetPixelColor32(X, Y: Integer): TColor32Rec;
begin
  Assert(Valid);
  Assert((X >= 0) and (X < Width) and (Y >= 0) and (Y < Height));
  Result := Imaging.GetPixel32(ImageDataPointer^, X, Y);
end;

{$IFNDEF FPC}
procedure TDeskewTestCase.AssertTrue(const Msg: string; const ACondition: Boolean;
  AddrOfError: Pointer);
begin
  CheckTrue(ACondition, Msg);
end;

procedure TDeskewTestCase.AssertTrue(const ACondition: Boolean);
begin
  CheckTrue(ACondition);
end;

procedure TDeskewTestCase.AssertFalse(const Msg: string; const ACondition: Boolean;
  AddrOfError: Pointer);
begin
  CheckFalse(ACondition, Msg);
end;

procedure TDeskewTestCase.AssertEquals(const AMessage: string; Expected, Actual: string);
begin
  CheckEquals(Expected, Actual, AMessage);
end;

procedure TDeskewTestCase.AssertEquals(Expected, Actual: string);
begin
  CheckEquals(Expected, Actual);
end;
{$ENDIF}

procedure TDeskewTestCase.AssertEquals(const AMessage: string; const Expected, Actual: Double);
begin
{$IFDEF FPC}
  AssertTrue(ComparisonMsg(AMessage, FloatToStr(Expected), FloatToStr(Actual)),
    SameValue(Expected, Actual, Epsilon), CallerAddr);
{$ELSE}
  CheckEquals(Expected, Actual, Epsilon, AMessage);
{$ENDIF}
end;

procedure TDeskewTestCase.AssertEquals(const AMessage: string; const Expected, Actual: TFloatRect);
const
  SCompare = '"%s" expected: <%g,%g,%g,%g> but was: <%g,%g,%g,%g>';

  function AreFloatRectsEqual(const R1, R2: TFloatRect): Boolean;
  begin
    Result := SameValue(R1.Left, R2.Left, Epsilon) and
              SameValue(R1.Top, R2.Top, Epsilon) and
              SameValue(R1.Right, R2.Right, Epsilon) and
              SameValue(R1.Bottom, R2.Bottom, Epsilon);
  end;

begin
  AssertTrue(
    Format(SCompare, [AMessage,
      Expected.Left, Expected.Top, Expected.Right, Expected.Bottom,
      Actual.Left, Actual.Top, Actual.Right, Actual.Bottom]),
    AreFloatRectsEqual(Expected, Actual) {$IFDEF FPC}, CallerAddr{$ENDIF});
end;

procedure TDeskewTestCase.AssertEquals(const AMessage: string; const Expected, Actual: TRect);
begin
  AssertTrue(
    Format(SCompare4Ints, [AMessage,
      Expected.Left, Expected.Top, Expected.Right, Expected.Bottom,
      Actual.Left, Actual.Top, Actual.Right, Actual.Bottom]),
    EqualRect(Expected, Actual) {$IFDEF FPC}, CallerAddr{$ENDIF});
end;

procedure TDeskewTestCase.AssertEquals(const AMessage: string; const Expected, Actual: TColor24Rec);
begin
  AssertTrue(
    Format(SCompare3Ints, [AMessage,
      Expected.R, Expected.G, Expected.B,
      Actual.R, Actual.G, Actual.B]),
    CompareMem(@Expected, @Actual, SizeOf(TColor24Rec)) {$IFDEF FPC}, CallerAddr{$ENDIF});
end;

procedure TDeskewTestCase.AssertEquals(const AMessage: string; const Expected, Actual: TColor32Rec);
begin
  AssertTrue(
    Format(SCompare4Ints, [AMessage,
      Expected.A, Expected.R, Expected.G, Expected.B,
      Actual.A, Actual.R, Actual.G, Actual.B]),
    Expected.Color = Actual.Color {$IFDEF FPC}, CallerAddr{$ENDIF});
end;

end.
