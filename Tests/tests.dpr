program tests;

uses
  SysUtils,
  Classes,

  ImagingUtility in '..\Imaging\ImagingUtility.pas',
  ImagingNetworkGraphics in '..\Imaging\ImagingNetworkGraphics.pas',
  Utils in '..\Utils.pas',
  ImageUtils in '..\ImageUtils.pas',
  CmdLineOptions in '..\CmdLineOptions.pas',
  RotationDetector in '..\RotationDetector.pas',
  MainUnit in '..\MainUnit.pas',

  TextTestRunner,
  GUITestRunner,
  DeskewTestUtils in 'DeskewTestUtils.pas',
  TestCmdLineArgs in 'TestCmdLineArgs.pas',
  TestImageUtils in 'TestImageUtils.pas',
  TestSkewDetection in 'TestSkewDetection.pas';

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
