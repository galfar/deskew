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
unit AdvOptionsForm;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, ActnList, Options, Config;

type

  { TFormAdvOptions }

  TFormAdvOptions = class(TForm)
    ActResetOptions: TAction;
    ActBrowseDeskewExe: TAction;
    ActionList: TActionList;
    Bevel1: TBevel;
    Bevel2: TBevel;
    BtnBrowseDeskewExePath: TButton;
    BtnResetOptions: TButton;
    CheckPrintParams: TCheckBox;
    CheckThresholdAuto: TCheckBox;
    CheckJpegQuality: TCheckBox;
    CheckDefaultExecutable: TCheckBox;
    CheckTiffCompression: TCheckBox;
    ComboTiffCompression: TComboBox;
    ComboOutputFormat: TComboBox;
    ComboResampling: TComboBox;
    EdDeskewExePath: TEdit;
    EdExtraCmdArgs: TEdit;
    LabDeskewExe: TLabel;
    LabExtraCmdArgs: TLabel;
    LabOutputFileParams: TLabel;
    LabThresholding: TLabel;
    LabResamling: TLabel;
    LabTitle: TLabel;
    LabForcedFormat: TLabel;
    LabMaxAngle: TLabel;
    LabSkipAngle: TLabel;
    Panel1: TPanel;
    SpinThresholdValue: TSpinEdit;
    SpinEditJpegQuality: TSpinEdit;
    SpinEditMaxAngle: TFloatSpinEdit;
    SpinEditSkipAngle: TFloatSpinEdit;
    procedure ActResetOptionsExecute(Sender: TObject);
    procedure ActBrowseDeskewExeExecute(Sender: TObject);
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
  ComboOutputFormat.Clear;
  ComboOutputFormat.AddItem('Default (usually same as input)', TObject(fofNone));
  ComboOutputFormat.AddItem('1bit black and white', TObject(fofBinary1));
  ComboOutputFormat.AddItem('8bit grayscale', TObject(fofGray8));
  ComboOutputFormat.AddItem('24bit RGB', TObject(fofRgb24));
  ComboOutputFormat.AddItem('32bit RGB + opacity', TObject(fofRgba32));
  ComboOutputFormat.ItemIndex := 0;

  ComboResampling.Clear;
  ComboResampling.AddItem('Default (Bilinear)', TObject(rfDefaultLinear));
  ComboResampling.AddItem('Nearest', TObject(rfNearest));
  ComboResampling.AddItem('Bicubic', TObject(rfCubic));
  ComboResampling.AddItem('Lanczos', TObject(rfLanczos));
  ComboResampling.ItemIndex := 0;

  ComboTiffCompression.Clear;
  ComboTiffCompression.Items.Add('Uncompressed');
  ComboTiffCompression.Items.Add('LZW');
  ComboTiffCompression.Items.Add('RLE');
  ComboTiffCompression.Items.Add('Deflate/ZLib');
  ComboTiffCompression.Items.Add('JPEG');
  ComboTiffCompression.Items.Add('Group 4 Fax');
  ComboTiffCompression.ItemIndex := 0;

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
  SpinEditMaxAngle.Value := AOptions.MaxAngle;
  SpinEditSkipAngle.Value := AOptions.SkipAngle;
  ComboResampling.ItemIndex := Integer(AOptions.ResamplingFilter);
  ComboOutputFormat.ItemIndex := Integer(AOptions.ForcedOutputFormat);
  CheckJpegQuality.Checked := ffJpeg in AOptions.OutputFileParamsEnabled;
  SpinEditJpegQuality.Value := AOptions.JpegCompressionQuality;
  CheckTiffCompression.Checked := ffTiff in AOptions.OutputFileParamsEnabled;
  ComboTiffCompression.ItemIndex := AOptions.TiffCompressionScheme;
  CheckThresholdAuto.Checked := AOptions.ThresholdingAuto;
  SpinThresholdValue.Value := AOptions.ThresholdLevel;
  CheckPrintParams.Checked := AOptions.PrintParams;
  EdExtraCmdArgs.Text := AOptions.ExtraCmdLineArgs;

  CheckDefaultExecutable.Checked := AOptions.DefaultExecutable;
  EdDeskewExePath.Text := AOptions.CustomExecutablePath;
  EdDeskewExePath.SelStart := Length(EdDeskewExePath.Text);
end;

procedure TFormAdvOptions.GatherOptions(AOptions: TOptions);
begin
  AOptions.MaxAngle := SpinEditMaxAngle.Value;
  AOptions.SkipAngle := SpinEditSkipAngle.Value;
  AOptions.ResamplingFilter := TResamplingFilter(PtrUInt(ComboResampling.Items.Objects[ComboResampling.ItemIndex]));
  AOptions.ForcedOutputFormat := TForcedOutputFormat(PtrUInt(ComboOutputFormat.Items.Objects[ComboOutputFormat.ItemIndex]));
  AOptions.JpegCompressionQuality := SpinEditJpegQuality.Value;
  AOptions.TiffCompressionScheme := ComboTiffCompression.ItemIndex;
  AOptions.ThresholdingAuto := CheckThresholdAuto.Checked;
  AOptions.ThresholdLevel := SpinThresholdValue.Value;
  AOptions.PrintParams := CheckPrintParams.Checked;
  AOptions.ExtraCmdLineArgs := EdExtraCmdArgs.Text;

  AOptions.OutputFileParamsEnabled := [ ];
  if CheckJpegQuality.Checked then Include(AOptions.OutputFileParamsEnabled, ffJpeg);
  if CheckTiffCompression.Checked then Include(AOptions.OutputFileParamsEnabled, ffTiff);

  AOptions.DefaultExecutable := CheckDefaultExecutable.Checked;
  AOptions.CustomExecutablePath := EdDeskewExePath.Text;
end;

procedure TFormAdvOptions.DoIdle;
begin
  ActBrowseDeskewExe.Enabled := not CheckDefaultExecutable.Checked;
  EdDeskewExePath.Enabled := ActBrowseDeskewExe.Enabled;
  SpinEditJpegQuality.Enabled := CheckJpegQuality.Checked;
  ComboTiffCompression.Enabled := CheckTiffCompression.Checked;
  SpinThresholdValue.Enabled := not CheckThresholdAuto.Checked;
end;

procedure TFormAdvOptions.ActBrowseDeskewExeExecute(Sender: TObject);
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

