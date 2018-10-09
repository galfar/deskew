unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin, EditBtn,
  ComCtrls, ActnList, IniFiles,
  // Units needed for file info reading
  fileinfo, winpeimagereader, elfreader, machoreader,
  // App units
  Runner, Options, Utils;

type

  { TFormMain }

  TFormMain = class(TForm)
    ActDeskew: TAction;
    ActFinish: TAction;
    ActAddFiles: TAction;
    ActClearFiles: TAction;
    ActionList: TActionList;
    ApplicationProperties: TApplicationProperties;
    BtnAddFiles: TButton;
    BtnDeskew: TButton;
    BtnClear: TButton;
    BtnFinish: TButton;
    CheckDefaultOutputFileOptions: TCheckBox;
    CheckDefaultExecutable: TCheckBox;
    ColorBtnBackground: TColorButton;
    ComboFileFormat: TComboBox;
    ComboOutputFormat: TComboBox;
    DirEditOutput: TDirectoryEdit;
    FileEditExecutable: TFileNameEdit;
    LabOptFileFormat2: TLabel;
    SpinEditMaxAngle: TFloatSpinEdit;
    SpinEditSkipAngle: TFloatSpinEdit;
    FlowPanel1: TFlowPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabAdvOptions: TLabel;
    Label7: TLabel;
    LabOptFileFormat1: TLabel;
    LabOptOutputFolder: TLabel;
    Label5: TLabel;
    LabDeskewProgressTitle: TLabel;
    Label6: TLabel;
    LabOptFileFormat: TLabel;
    LabProgressTitle: TLabel;
    LabCurrentFile: TLabel;
    MemoOutput: TMemo;
    MemoFiles: TMemo;
    Notebook: TNotebook;
    OpenDialog: TOpenDialog;
    PageIn: TPage;
    PageOut: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelAdvOptions: TPanel;
    PanelFiles: TPanel;
    PanelOptions: TPanel;
    ProgressBar: TProgressBar;
    procedure ActAddFilesExecute(Sender: TObject);
    procedure ActClearFilesExecute(Sender: TObject);
    procedure ActDeskewExecute(Sender: TObject);
    procedure ActDeskewUpdate(Sender: TObject);
    procedure ActFinishExecute(Sender: TObject);
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure LabAdvOptionsClick(Sender: TObject);
  private
    FRunner: TRunner;
    FOptions: TOptions;

    procedure RunnerFinished(Sender: TObject; Reason: TFinishReason);
    procedure RunnerProgress(Sender: TObject; Index: Integer);
    procedure ReadAndUseVersionInfo;
    procedure GatherOptions;
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  ImagingUtility, Imaging;

const
  SExpandSymbol   = '▽';
  SCollapseSymbol = '△';
  STitleAdvOptions = 'Advanced Options';

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FRunner := TRunner.Create(MemoOutput);
  FRunner.OnFinished := @RunnerFinished;
  FRunner.OnProgress := @RunnerProgress;
  FOptions := TOptions.Create;

  ReadAndUseVersionInfo;

  PanelOptions.AutoSize := True; // for collapsible adv. options panel

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

  ComboOutputFormat.Items.Clear;
  ComboOutputFormat.Items.AddObject('Default (usually same as input)', TObject(fofNone));
  ComboOutputFormat.Items.AddObject('1bit black and white', TObject(fofBinary1));
  ComboOutputFormat.Items.AddObject('8bit grayscale', TObject(fofGray8));
  ComboOutputFormat.Items.AddObject('24bit RGB', TObject(fofRgb24));
  ComboOutputFormat.Items.AddObject('32bit RGB + opacity', TObject(fofRgba32));
  ComboOutputFormat.ItemIndex := 0;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FOptions.Free;
  FRunner.Free;
end;

procedure TFormMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  I: Integer;
begin
  for I := 0 to High(FileNames) do
    MemoFiles.Append(FileNames[I]);
end;

procedure TFormMain.LabAdvOptionsClick(Sender: TObject);
var
  Symbol: string;
begin
  PanelAdvOptions.Visible := not PanelAdvOptions.Visible;
  Symbol := Iff(PanelAdvOptions.Visible, SCollapseSymbol, SExpandSymbol);
  LabAdvOptions.Caption := Symbol + Symbol + ' ' + STitleAdvOptions;
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
    ExtractFileName(FOptions.Files[Index]), Index + 1, FOptions.Files.Count]);
  LabCurrentFile.Visible := True;
  LabProgressTitle.Visible := True;
end;

procedure TFormMain.ReadAndUseVersionInfo;
var
  FileVerInfo: TFileVersionInfo;
  VersionStr: string;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    VersionStr := FileVerInfo.VersionStrings.Values['FileVersion'];
    VersionStr := Copy(VersionStr, 1, PosEx('.', VersionStr, 3) - 1);
    Caption := Application.Title + ' v' + VersionStr;
  finally
    FileVerInfo.Free;
  end;
end;

procedure TFormMain.GatherOptions;
var
  LazColor: TColor;
  I: Integer;
  S: string;
begin
  FOptions.Files.Clear;
  for I := 0 to MemoFiles.Lines.Count - 1 do
  begin
    S := Trim(MemoFiles.Lines[I]);
    if S <> '' then
      FOptions.Files.Add(S);
  end;

  FOptions.DefaultOutputFileOptions := CheckDefaultOutputFileOptions.Checked;
  if not FOptions.DefaultOutputFileOptions then
  begin
    FOptions.OutputFolder := DirEditOutput.Directory;
    FOptions.OutputFileFormat := TFileFormat(PtrUInt(ComboFileFormat.Items.Objects[ComboFileFormat.ItemIndex]));
  end;

  LazColor := ColorBtnBackground.ButtonColor;
  FOptions.BackgroundColor := Color32(255, Red(LazColor), Green(LazColor), Blue(LazColor)).Color;

  // Advanced options
  FOptions.MaxAngle := SpinEditMaxAngle.Value;
  FOptions.SkipAngle := SpinEditSkipAngle.Value;
  FOptions.ForcedOutputFormat := TForcedOutputFormat(PtrUInt(ComboOutputFormat.Items.Objects[ComboOutputFormat.ItemIndex]));
  FOptions.DefaultExecutable := CheckDefaultExecutable.Checked;
  if not FOptions.DefaultExecutable then
    FOptions.ExecutablePath := FileEditExecutable.FileName;
end;

procedure TFormMain.ActDeskewUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := MemoFiles.Lines.Count > 0;
end;

procedure TFormMain.ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
var
  NoDefault: Boolean;
begin
  NoDefault := not CheckDefaultOutputFileOptions.Checked;
  DirEditOutput.Enabled := NoDefault;
  ComboFileFormat.Enabled := NoDefault;
  LabOptOutputFolder.Enabled := NoDefault;
  LabOptFileFormat.Enabled := NoDefault;

  FileEditExecutable.Enabled := not CheckDefaultExecutable.Checked;
end;

procedure TFormMain.ActDeskewExecute(Sender: TObject);
begin
  GatherOptions;

  ActFinish.Caption := 'Stop';
  MemoOutput.Clear;
  ProgressBar.Position := 0;
  ProgressBar.Max := FOptions.Files.Count;
  LabCurrentFile.Hide;
  LabProgressTitle.Hide;
  LabProgressTitle.Caption := 'Current file:';

  Notebook.PageIndex := 1;

  FRunner.Run(FOptions);
end;

procedure TFormMain.ActAddFilesExecute(Sender: TObject);
var
  I: Integer;
begin
  if OpenDialog.Execute then
  begin
    for I := 0 to OpenDialog.Files.Count - 1 do
      MemoFiles.Append(OpenDialog.Files[I]);
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




end.

