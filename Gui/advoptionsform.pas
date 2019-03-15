unit AdvOptionsForm;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, ActnList, Options, Config;

type

  { TFormAdvOptions }

  TFormAdvOptions = class(TForm)
    ActResetOptions: TAction;
    AtcBrowseDeskewExe: TAction;
    ActionList: TActionList;
    BtnBrowseDeskewExePath: TButton;
    BtnResetOptions: TButton;
    CheckDefaultExecutable: TCheckBox;
    ComboOutputFormat: TComboBox;
    EdDeskewExePath: TEdit;
    LabDeskewExe: TLabel;
    LabTitle: TLabel;
    LabForcedFormat: TLabel;
    LabMaxAngle: TLabel;
    LabSkipAngle: TLabel;
    Panel1: TPanel;
    SpinEditMaxAngle: TFloatSpinEdit;
    SpinEditSkipAngle: TFloatSpinEdit;
    procedure ActResetOptionsExecute(Sender: TObject);
    procedure AtcBrowseDeskewExeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    procedure ApplyOptions(AOptions: TOptions);
    procedure GatherOptions(AOptions: TOptions);
    procedure DoIdle;
  end;

var
  FormAdvOptions: TFormAdvOptions;

implementation

uses
  DataModule, MainForm;

{$R *.lfm}

{ TFormAdvOptions }

procedure TFormAdvOptions.FormCreate(Sender: TObject);
begin
  ComboOutputFormat.Items.Clear;
  ComboOutputFormat.Items.AddObject('Default (usually same as input)', TObject(fofNone));
  ComboOutputFormat.Items.AddObject('1bit black and white', TObject(fofBinary1));
  ComboOutputFormat.Items.AddObject('8bit grayscale', TObject(fofGray8));
  ComboOutputFormat.Items.AddObject('24bit RGB', TObject(fofRgb24));
  ComboOutputFormat.Items.AddObject('32bit RGB + opacity', TObject(fofRgba32));
  ComboOutputFormat.ItemIndex := 0;

  if not Config.ShowDeskewExeOption then
  begin
    CheckDefaultExecutable.Visible := False;
    LabDeskewExe.Visible := False;
    EdDeskewExePath.Visible := False;
    BtnBrowseDeskewExePath.Visible := False;
    Height := EdDeskewExePath.BoundsRect.Bottom;
  end;

  ApplyOptions(Module.Options);
end;

procedure TFormAdvOptions.FormDestroy(Sender: TObject);
begin
  GatherOptions(Module.Options);
end;

procedure TFormAdvOptions.ApplyOptions(AOptions: TOptions);
begin
  CheckDefaultExecutable.Checked := AOptions.DefaultExecutable;
  EdDeskewExePath.Text := AOptions.CustomExecutablePath;
  EdDeskewExePath.SelStart := Length(EdDeskewExePath.Text);
  SpinEditMaxAngle.Value := AOptions.MaxAngle;
  SpinEditSkipAngle.Value := AOptions.SkipAngle;
  ComboOutputFormat.ItemIndex := Integer(AOptions.ForcedOutputFormat);
end;

procedure TFormAdvOptions.GatherOptions(AOptions: TOptions);
begin
  AOptions.MaxAngle := SpinEditMaxAngle.Value;
  AOptions.SkipAngle := SpinEditSkipAngle.Value;
  AOptions.ForcedOutputFormat := TForcedOutputFormat(PtrUInt(ComboOutputFormat.Items.Objects[ComboOutputFormat.ItemIndex]));
  AOptions.DefaultExecutable := CheckDefaultExecutable.Checked;
  AOptions.CustomExecutablePath := EdDeskewExePath.Text;
end;

procedure TFormAdvOptions.DoIdle;
begin
  AtcBrowseDeskewExe.Enabled := not CheckDefaultExecutable.Checked;
  EdDeskewExePath.Enabled := AtcBrowseDeskewExe.Enabled;
end;

procedure TFormAdvOptions.AtcBrowseDeskewExeExecute(Sender: TObject);
begin
  Module.OpenDialogSingle.Title := 'Select Deskew Binary Executable';
  if Module.OpenDialogSingle.Execute then
  begin
    EdDeskewExePath.Text := Module.OpenDialogSingle.FileName;
    EdDeskewExePath.SelStart := Length(EdDeskewExePath.Text);
  end;
end;

procedure TFormAdvOptions.ActResetOptionsExecute(Sender: TObject);
begin
  if Dialogs.QuestionDlg('Reset Options', 'Do you really want to reset the options to default?',
    mtConfirmation, [mrOk, 'Reset', mrCancel], 0) = mrOk then
  begin
    Module.Options.Reset;
    ApplyOptions(Module.Options);
    FormMain.ApplyOptions(Module.Options);
  end;
end;

end.

