program tests;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner,
  TestCmdLineArgs, CmdLineOptions, ImageUtils, RotationDetector;

type

  TDeskewTestRunner = class(TTestRunner)
  protected
    // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TDeskewTestRunner;

begin
  Application := TDeskewTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
