program tests;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner,
  DeskewTestUtils,
  TestCmdLineArgs,
  TestImageUtils,
  TestSkewDetection,
  TestUtilsUnit;

type
  TDeskewTestRunner = class(TTestRunner)
  end;

var
  Application: TDeskewTestRunner;

begin
  Application := TDeskewTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
