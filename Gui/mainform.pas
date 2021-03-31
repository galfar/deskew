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
unit MainForm;

interface

uses
  Classes, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, ActnList,
  // App units
  Runner, Options;

type

  { TFormMain }

  TFormMain = class(TForm)
    ActDeskew: TAction;
    ActFinish: TAction;
    ActAddFiles: TAction;
    ActClearFiles: TAction;
    ActBrowseOutputDir: TAction;
    ActShowAbout: TAction;
    ActShowAdvOptions: TAction;
    ActionList: TActionList;
    ApplicationProperties: TApplicationProperties;
    BtnAddFiles: TButton;
    BtnDeskew: TButton;
    BtnClear: TButton;
    BtnFinish: TButton;
    BtnBrowseOutputDir: TButton;
    BtnAdvOptions: TButton;
    BtnAbout: TButton;
    CheckAutoCrop: TCheckBox;
    CheckDefaultOutputFileOptions: TCheckBox;
    ColorBtnBackground: TColorButton;
    ComboFileFormat: TComboBox;
    EdDirOutput: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabOptOutputFolder: TLabel;
    LabBackColor: TLabel;
    LabDeskewProgressTitle: TLabel;
    LabOptFileFormat: TLabel;
    LabProgressTitle: TLabel;
    LabCurrentFile: TLabel;
    MemoOutput: TMemo;
    MemoFiles: TMemo;
    Notebook: TNotebook;
    PageIn: TPage;
    PageOut: TPage;
    Panel1: TPanel;
    PanelProgress: TPanel;
    PanelOut: TPanel;
    PanelFiles: TPanel;
    PanelOptions: TPanel;
    ProgressBar: TProgressBar;
    procedure ActAddFilesExecute(Sender: TObject);
    procedure ActBrowseOutputDirExecute(Sender: TObject);
    procedure ActClearFilesExecute(Sender: TObject);
    procedure ActDeskewExecute(Sender: TObject);
    procedure ActDeskewUpdate(Sender: TObject);
    procedure ActFinishExecute(Sender: TObject);
    procedure ActShowAboutExecute(Sender: TObject);
    procedure ActShowAdvOptionsExecute(Sender: TObject);
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private
    FRunner: TRunner;

    procedure RunnerFinished(Sender: TObject; Reason: TFinishReason);
    procedure RunnerProgress(Sender: TObject; Index: Integer);

    procedure GatherOptions(AOptions: TOptions);
  public
    procedure ApplyOptions(AOptions: TOptions);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  ImagingUtility, Imaging, DataModule, AdvOptionsForm, AboutForm, Config;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FRunner := TRunner.Create(MemoOutput);
  FRunner.OnFinished := RunnerFinished;
  FRunner.OnProgress := RunnerProgress;

  Caption := Application.Title + ' v' + Module.VersionString;

  ComboFileFormat.Items.Clear;
  ComboFileFormat.Items.AddObject('Same as input', TObject(ffSameAsInput));
  ComboFileFormat.Items.AddObject('PNG', TObject(ffPng));
  ComboFileFormat.Items.AddObject('JPEG', TObject(ffJpeg));
  ComboFileFormat.Items.AddObject('TIFF (support depends on platform)', TObject(ffTiff));
  ComboFileFormat.Items.AddObject('BMP', TObject(ffBmp));
  ComboFileFormat.Items.AddObject('PSD', TObject(ffPsd));
  ComboFileFormat.Items.AddObject('TGA', TObject(ffTga));
  ComboFileFormat.Items.AddObject('JNG', TObject(ffJng));
  ComboFileFormat.Items.AddObject('PPM', TObject(ffPpm));
  ComboFileFormat.ItemIndex := 0;

  ApplyOptions(Module.Options);

  Config.AfterMainFormCreation(Self);

  if Screen.WorkAreaHeight < Height then
    Height := Round(Screen.WorkAreaHeight * 0.9);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  GatherOptions(Module.Options);
  FRunner.Free;
end;

procedure TFormMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  I: Integer;
begin
  for I := 0 to High(FileNames) do
    MemoFiles.Append(FileNames[I]);
end;

procedure TFormMain.ApplyOptions(AOptions: TOptions);
begin
  CheckDefaultOutputFileOptions.Checked := AOptions.DefaultOutputFileOptions;
  EdDirOutput.Text := AOptions.OutputFolder;
  EdDirOutput.SelStart := Length(EdDirOutput.Text);
  ComboFileFormat.ItemIndex := Integer(AOptions.OutputFileFormat);
  ColorBtnBackground.ButtonColor := RGBToColor(GetRedValue(AOptions.BackgroundColor), GetGreenValue(AOptions.BackgroundColor), GetBlueValue(AOptions.BackgroundColor));
  CheckAutoCrop.Checked := AOptions.AutoCrop;
end;

procedure TFormMain.GatherOptions(AOptions: TOptions);
var
  LazColor: TColor;
  I: Integer;
  S: string;
begin
  AOptions.Files.Clear;
  for I := 0 to MemoFiles.Lines.Count - 1 do
  begin
    S := Trim(MemoFiles.Lines[I]);
    if S <> '' then
      AOptions.Files.Add(S);
  end;

  AOptions.DefaultOutputFileOptions := CheckDefaultOutputFileOptions.Checked;
  AOptions.OutputFolder := EdDirOutput.Text;
  AOptions.OutputFileFormat := TFileFormat(PtrUInt(ComboFileFormat.Items.Objects[ComboFileFormat.ItemIndex]));
  LazColor := ColorBtnBackground.ButtonColor;
  AOptions.BackgroundColor := Color32(255, Red(LazColor), Green(LazColor), Blue(LazColor)).Color;
  AOptions.AutoCrop := CheckAutoCrop.Checked;
end;

procedure TFormMain.RunnerFinished(Sender: TObject; Reason: TFinishReason);
begin
  LabCurrentFile.Hide;
  ActFinish.Enabled := True;
  ActFinish.Caption := 'Back to Input';

  case Reason of
    frFinished: LabDeskewProgressTitle.Caption := 'Deskewing Finished';
    frFailure: LabDeskewProgressTitle.Caption := 'Deskewing Finished with Failures';
    frStopped: LabDeskewProgressTitle.Caption := 'Deskewing Stopped';
  else
    Assert(False);
  end;

  LabProgressTitle.Caption := Format('%d files processed', [FRunner.InputPos]);
  if FRunner.Failures > 0 then
    LabProgressTitle.Caption := LabProgressTitle.Caption + Format(', %d failed', [FRunner.Failures]);
end;

procedure TFormMain.RunnerProgress(Sender: TObject; Index: Integer);
begin
  ProgressBar.Position := Index + 1;
  LabCurrentFile.Caption := Format('%s [%d/%d]', [
    ExtractFileName(Module.Options.Files[Index]), Index + 1, Module.Options.Files.Count]);
  LabCurrentFile.Visible := True;
  LabProgressTitle.Visible := True;
end;

procedure TFormMain.ActDeskewUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (MemoFiles.Lines.Count > 0) and (Trim(MemoFiles.Lines[0]) <> '');
end;

procedure TFormMain.ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
var
  NoDefault: Boolean;
begin
  NoDefault := not CheckDefaultOutputFileOptions.Checked;
  ActBrowseOutputDir.Enabled := NoDefault;
  EdDirOutput.Enabled := ActBrowseOutputDir.Enabled;
  ComboFileFormat.Enabled := NoDefault;
  LabOptOutputFolder.Enabled := NoDefault;
  LabOptFileFormat.Enabled := NoDefault;

  FormAdvOptions.DoIdle;
end;

procedure TFormMain.ActDeskewExecute(Sender: TObject);
begin
  GatherOptions(Module.Options);
  FormAdvOptions.GatherOptions(Module.Options);

  ActFinish.Caption := 'Stop';
  MemoOutput.Clear;
  ProgressBar.Position := 0;
  ProgressBar.Max := Module.Options.Files.Count;
  LabCurrentFile.Hide;
  LabProgressTitle.Hide;
  LabProgressTitle.Caption := 'Current file:';

  Notebook.PageIndex := 1;
  Application.ProcessMessages;

  FRunner.Run(Module.Options);
end;

procedure TFormMain.ActAddFilesExecute(Sender: TObject);
var
  I: Integer;
begin
  Module.OpenDialogMulti.Title := 'Select Picture Files';
  if Module.OpenDialogMulti.Execute then
  begin
    for I := 0 to Module.OpenDialogMulti.Files.Count - 1 do
      MemoFiles.Append(Module.OpenDialogMulti.Files[I]);
  end;
end;

procedure TFormMain.ActBrowseOutputDirExecute(Sender: TObject);
begin
  if Module.SelectDirectoryDialog.Execute then
  begin
    EdDirOutput.Text := Module.SelectDirectoryDialog.FileName;
    EdDirOutput.SelStart := Length(EdDirOutput.Text);
  end;
end;

procedure TFormMain.ActClearFilesExecute(Sender: TObject);
begin
  MemoFiles.Clear;
end;

procedure TFormMain.ActFinishExecute(Sender: TObject);
begin
  if FRunner.IsRunning then
  begin
    ActFinish.Enabled := False;
    ActFinish.Caption := 'Stopping';
    FRunner.Stop;
  end
  else
  begin
    Notebook.PageIndex := 0;
  end;
end;

procedure TFormMain.ActShowAboutExecute(Sender: TObject);
begin
  if not FormAbout.Visible then
    FormAbout.ShowModal;
end;

procedure TFormMain.ActShowAdvOptionsExecute(Sender: TObject);
begin
  FormAdvOptions.ShowModal;
end;


end.

