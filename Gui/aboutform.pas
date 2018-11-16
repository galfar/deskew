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
  LCLIntf, DataModule;

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
  LabVersion.Caption := 'v' + Module.VersionString;

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
end;

procedure TFormAbout.LabWebClick(Sender: TObject);
begin
  OpenURL(LabWeb.Caption);
end;

end.

