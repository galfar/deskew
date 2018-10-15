program deskewgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  clocale,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols,
  { you can add units after this }
  MainForm, Runner, Utils, Options;

{$R *.res}

begin
  Application.Title:='Deskew GUI';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

