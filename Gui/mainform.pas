unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, AsyncProcess, ExtDlgs, Spin, EditBtn, ComCtrls, ActnList, IniFiles,
  // Units needed for file info reading
  fileinfo, winpeimagereader, elfreader, machoreader,
  // App units
  Runner, Options, Utils;

type

  { TFormMain }

  TFormMain = class(TForm)
    ActDeskew: TAction;
    ActUseDefaultOutput: TAction;
    ActionList: TActionList;
    ApplicationProperties: TApplicationProperties;
    AsyncProcess: TAsyncProcess;
    BtnAddFiles: TButton;
    BtnDeskew: TButton;
    BtnClear: TButton;
    BtnFinish: TButton;
    CheckBox1: TCheckBox;
    ColorBtnBackground: TColorButton;
    ComboFileFormat: TComboBox;
    DirEditOutput: TDirectoryEdit;
    FloatSpinEdit1: TFloatSpinEdit;
    FlowPanel1: TFlowPanel;
    IdleTimer: TIdleTimer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabAdvOptions: TLabel;
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
    OpenPictureDialog: TOpenPictureDialog;
    PageIn: TPage;
    PageOut: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelAdvOptions: TPanel;
    PanelFiles: TPanel;
    PanelOptions: TPanel;
    ProgressBar: TProgressBar;
    procedure ActDeskewExecute(Sender: TObject);
    procedure ActDeskewUpdate(Sender: TObject);
    procedure ActUseDefaultOutputExecute(Sender: TObject);
    procedure BtnAddFilesClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure BtnFinishClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure LabAdvOptionsClick(Sender: TObject);
  private
    FRunner: TRunner;
    FOptions: TOptions;
    FFileFormats: TStringList;

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
  FRunner := TRunner.Create(AsyncProcess, MemoOutput);
  FRunner.OnFinished := @RunnerFinished;
  FRunner.OnProgress := @RunnerProgress;
  FOptions := TOptions.Create;

  ReadAndUseVersionInfo;

  PanelOptions.AutoSize := True; // for collapsible adv. options panel

  ActUseDefaultOutputExecute(ActUseDefaultOutput);

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
  BtnFinish.Enabled := True;
  BtnFinish.Caption := 'Back to Input';

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
begin
  FOptions.Files.Assign(MemoFiles.Lines);
  FOptions.DefaultOutputFileOptions := ActUseDefaultOutput.Checked;
  FOptions.OutputFolder := DirEditOutput.Directory;
  FOptions.OutputFileFormat := TFileFormat(PtrUInt(ComboFileFormat.Items.Objects[ComboFileFormat.ItemIndex]));

  LazColor := ColorBtnBackground.ButtonColor;
  FOptions.BackgroundColor := Color32(255, Red(LazColor), Green(LazColor), Blue(LazColor)).Color;
end;

procedure TFormMain.BtnAddFilesClick(Sender: TObject);
var
  I: Integer;
begin
  if OpenPictureDialog.Execute then
  begin
    for I := 0 to OpenPictureDialog.Files.Count - 1 do
      MemoFiles.Append(OpenPictureDialog.Files[I]);
  end;
end;

procedure TFormMain.ActDeskewUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := MemoFiles.Lines.Count > 0;
end;

procedure TFormMain.ActUseDefaultOutputExecute(Sender: TObject);
var
  NoDefault: Boolean;
begin
  NoDefault := not TAction(Sender).Checked;
  DirEditOutput.Enabled := NoDefault;
  ComboFileFormat.Enabled := NoDefault;
  LabOptOutputFolder.Enabled := NoDefault;
  LabOptFileFormat.Enabled := NoDefault;
end;

procedure TFormMain.ActDeskewExecute(Sender: TObject);
begin
  GatherOptions;

  BtnFinish.Caption := 'Stop';
  MemoOutput.Clear;
  ProgressBar.Position := 0;
  ProgressBar.Max := FOptions.Files.Count;
  LabCurrentFile.Hide;
  LabProgressTitle.Hide;
  LabProgressTitle.Caption := 'Current file:';

  Notebook.PageIndex := 1;

  FRunner.Run(FOptions);
end;

procedure TFormMain.BtnClearClick(Sender: TObject);
begin
  MemoFiles.Clear;
end;

procedure TFormMain.BtnFinishClick(Sender: TObject);
begin
  if FRunner.IsRunning then
  begin
    BtnFinish.Enabled := False;
    BtnFinish.Caption := 'Stopping';
    FRunner.Stop;
  end
  else
  begin
    Notebook.PageIndex := 0;
  end;
end;

end.

