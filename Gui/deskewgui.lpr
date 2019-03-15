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
  Application.Title:='Deskew GUIT';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TModule, Module);
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormAdvOptions, FormAdvOptions);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.Run;
end.

