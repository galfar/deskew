unit AboutForm;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    BtnClose: TButton;
    Image1: TImage;
    Label1: TLabel;
    LabWeb: TLabel;
    LabTitle: TLabel;
    LabVersion: TLabel;
    procedure BtnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabWebClick(Sender: TObject);
  private

  public

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
begin
  LabVersion.Caption := 'v' + Module.VersionString;
end;

procedure TFormAbout.LabWebClick(Sender: TObject);
begin
  OpenURL(LabWeb.Caption);
end;

end.

