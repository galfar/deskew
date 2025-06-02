program tests;

uses
  SysUtils,
  Classes,
  TextTestRunner,
  GUITestRunner,
  TestCmdLineArgs,
  ImagingUtility in '..\Imaging\ImagingUtility.pas',
  Utils in '..\Utils.pas',
  ImageUtils in '..\ImageUtils.pas',
  CmdLineOptions in '..\CmdLineOptions.pas',
  RotationDetector in '..\RotationDetector.pas',
  MainUnit in '..\MainUnit.pas';

begin
  try
    ReportMemoryLeaksOnShutdown := True;

    //RunRegisteredTestsModelessUnattended;

    if SysUtils.FindCmdLineSwitch('text', True) then
      TextTestRunner.RunRegisteredTests
    else
      GUITestRunner.RunRegisteredTests;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
