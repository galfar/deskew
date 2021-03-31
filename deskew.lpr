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

{$IFDEF MSWINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  RotationDetector in 'RotationDetector.pas',
  CmdLineOptions in 'CmdLineOptions.pas',
  ImageUtils in 'ImageUtils.pas',
  MainUnit in 'MainUnit.pas';

begin
{$IFDEF DEBUG}
{$IFNDEF FPC}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
{$ENDIF}
  RunDeskew;
end.
