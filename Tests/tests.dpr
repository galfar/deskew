program tests;

uses
  SysUtils,
  Classes,
  TextTestRunner,
  GUITestRunner,
  TestCmdLineArgs,
  Utils in '..\Utils.pas',
  ImagingUtility in '..\Imaging\ImagingUtility.pas',
  CmdLineOptions in '..\CmdLineOptions.pas',
  RotationDetector in '..\RotationDetector.pas';

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
