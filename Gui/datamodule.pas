unit DataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Dialogs, ActnList,
  // Units needed for file info reading
  fileinfo, winpeimagereader, elfreader, machoreader,
  // App units
  Options;

type

  { TModule }

  TModule = class(TDataModule)
    ActShowAdvOptions: TAction;
    OpenDialogMulti: TOpenDialog;
    OpenDialogSingle: TOpenDialog;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    procedure ActShowAdvOptionsExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    procedure SaveOptions;
    procedure LoadOptions;
    procedure ReadVersionInfo;
  public
    Options: TOptions;
    VersionString: string;
  end;

var
  Module: TModule;

implementation

{$R *.lfm}

uses
  ImagingUtility, IniFiles, AdvOptionsForm;

const
  SOptionsFileName = 'deskewgui.ini';

{ TModule }

procedure TModule.DataModuleCreate(Sender: TObject);
begin
  ReadVersionInfo;

  Options := TOptions.Create;
  LoadOptions;
end;

procedure TModule.DataModuleDestroy(Sender: TObject);
begin
  SaveOptions;
  Options.Free;
end;

procedure TModule.LoadOptions;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(SOptionsFileName, [ifoFormatSettingsActive]);
  try
    Options.LoadFromIni(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TModule.ReadVersionInfo;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    VersionString := FileVerInfo.VersionStrings.Values['FileVersion'];
    VersionString := Copy(VersionString, 1, PosEx('.', VersionString, 3) - 1);
  finally
    FileVerInfo.Free;
  end;
end;

procedure TModule.SaveOptions;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(SOptionsFileName, [ifoFormatSettingsActive]);
  try
    Options.SaveToIni(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TModule.ActShowAdvOptionsExecute(Sender: TObject);
begin
  FormAdvOptions.ShowModal;
end;



end.

