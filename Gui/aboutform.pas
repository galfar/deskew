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
unit AboutForm;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    BtnClose: TButton;
    ImageIcon: TImage;
    Label1: TLabel;
    LabWeb: TLabel;
    LabTitle: TLabel;
    LabVersion: TLabel;
    LabGitHub: TLabel;
    procedure BtnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabGitHubClick(Sender: TObject);
    procedure LabWebClick(Sender: TObject);
  end;

var
  FormAbout: TFormAbout;

implementation

uses
  LCLIntf, DataModule, Config;

{$R *.lfm}

{ TFormAbout }

procedure TFormAbout.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}Color := clWhite;{$ENDIF}

  LabTitle.Caption := Application.Title;
  LabVersion.Caption := 'v' + Module.VersionString;
  LabWeb.Caption := Config.WebLink;
end;

procedure TFormAbout.LabGitHubClick(Sender: TObject);
begin
  OpenURL(LabGitHub.Caption);
end;

procedure TFormAbout.LabWebClick(Sender: TObject);
begin
  OpenURL(LabWeb.Caption);
end;

end.

