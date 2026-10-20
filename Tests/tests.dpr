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
  TestSkewDetection in 'TestSkewDetection.pas',
  TestUtilsUnit in 'TestUtilsUnit.pas';

begin
  try
    // Only under the IDE debugger: the report is a modal dialog that would
    // block scripted runs (and DUnit leaves some of its own objects behind)
{$IFDEF MSWINDOWS}
  {$WARN SYMBOL_PLATFORM OFF}
    ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  {$WARN SYMBOL_PLATFORM ON}
{$ENDIF}

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
