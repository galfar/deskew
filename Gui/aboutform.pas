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
    procedure BtnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
var
  Icon: TIcon;
begin
  LabTitle.Caption := Application.Title;
  LabVersion.Caption := 'v' + Module.VersionString;
  LabWeb.Caption := Config.WebLink;

  if Config.LogoImageResName = '' then
  begin
    Icon := TIcon.Create;
    try
      Icon.LoadFromResourceName(HInstance, 'MAINICON');
      ImageIcon.Picture.Assign(Icon);
  {$IF Defined(DARWIN)}
      ImageIcon.Stretch := False; // Currently broken in Cocoa WS
  {$ENDIF}
    finally
      Icon.Free;
    end;
  end
  else
  begin
    ImageIcon.Stretch := False;
    ImageIcon.Picture.LoadFromResourceName(HInstance, Config.LogoImageResName);
  end;
end;

procedure TFormAbout.LabWebClick(Sender: TObject);
begin
  OpenURL(LabWeb.Caption);
end;

end.

