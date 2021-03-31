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
program deskewgui;

uses
  {$IFDEF UNIX}
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  clocale,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  { you can add units after this }
  DataModule, MainForm, AdvOptionsForm, AboutForm, Runner, Utils, Options,
  Config;

{$R *.res}

begin
  Application.Title := 'Deskew GUI';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TModule, Module);
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormAdvOptions, FormAdvOptions);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.Run;
end.

