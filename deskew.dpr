{
  Deskew
  by Marek Mauder
  https://galfar.vevb.net/deskew
  https://github.com/galfar/deskew
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
}
program deskew;

{$APPTYPE CONSOLE}

{$IFDEF FPC}
  {$ERROR 'Use deskew.lpr as FPC/Lazarus project file'}
{$ENDIF}

uses
  RotationDetector in 'RotationDetector.pas',
  CmdLineOptions in 'CmdLineOptions.pas',
  ImageUtils in 'ImageUtils.pas',
  Utils in 'Utils.pas',
  MainUnit in 'MainUnit.pas',
  Imaging in 'Imaging\Imaging.pas',
  ImagingClasses in 'Imaging\ImagingClasses.pas',
  ImagingFormats in 'Imaging\ImagingFormats.pas',
  ImagingTypes in 'Imaging\ImagingTypes.pas',
  ImagingUtility in 'Imaging\ImagingUtility.pas';

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  RunDeskew;
end.
