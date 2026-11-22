{
  Unit tests of the GUI parts that work without forms: settings and their
  translation to command line parameters, helpers, and the parameters
  checked against the real command line tool. Console program built with
  the LCL "nogui" widgetset, it needs no display.

  Build: lazbuild Tests/gui/guitests.lpi (output Bin/guitests, the command
  line tool must be built into Bin/ too). Repository root is not in the unit
  search path: Gui/utils.pas is the Utils unit here, not the one of the CLI.
  That is also why DeskewTestUtils (uses the CLI's Utils) is not used.
  Run from Bin/: ./guitests -a -p --format=plain
}
program guitests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, Classes, consoletestrunner,
  TestGuiOptions, TestGuiUtils, TestGuiCli;

var
  TestRunner: TTestRunner;

begin
  Application.Initialize;
  TestRunner := TTestRunner.Create(nil);
  TestRunner.Initialize;
  TestRunner.Run;
  TestRunner.Free;
end.
