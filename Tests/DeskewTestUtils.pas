unit DeskewTestUtils;

interface

uses
  Types, Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFramework,
{$ENDIF}
  ImagingUtility,
  Utils;

type
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

    procedure AssertEquals(const AMessage: string; Expected, Actual: Double); overload;
    procedure AssertEquals(const AMessage: string; const Expected, Actual: TFloatRect); overload;
    procedure AssertEquals(const AMessage: string; const Expected, Actual: TRect); overload;
  end;

  TTestClass = class of TDeskewTestCase;

procedure RegisterTest(TestClass: TTestClass); overload;

implementation

uses
  Math;

const
  Epsilon = 0.001;

procedure RegisterTest(TestClass: TTestClass);
begin
  {$IFDEF FPC}testregistry.{$ENDIF}RegisterTest(TestClass{$IFNDEF FPC}.Suite{$ENDIF});
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

procedure TDeskewTestCase.AssertEquals(const AMessage: string; Expected, Actual: Double);
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
const
  SCompare = '"%s" expected: <%d,%d,%d,%d> but was: <%d,%d,%d,%d>';
begin
  AssertTrue(
    Format(SCompare, [AMessage,
      Expected.Left, Expected.Top, Expected.Right, Expected.Bottom,
      Actual.Left, Actual.Top, Actual.Right, Actual.Bottom]),
    EqualRect(Expected, Actual) {$IFDEF FPC}, CallerAddr{$ENDIF});
end;

end.
