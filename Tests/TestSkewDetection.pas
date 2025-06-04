unit TestSkewDetection;

interface

uses
  Types, Classes, SysUtils,
  DeskewTestUtils,
  ImageUtils;

type
  TTestSkewDetection = class(TDeskewTestCase)
  published
    procedure Test;
  end;


implementation

procedure TTestSkewDetection.Test;
begin
end;

initialization
  RegisterTest(TTestSkewDetection);
end.
