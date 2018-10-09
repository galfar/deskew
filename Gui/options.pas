unit Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ImagingTypes;

type
  TForcedOutputFormat = (
    fofNone,
    fofBinary1,
    fofGray8,
    fofRgb24,
    fofRgba32
  );

  TFileFormat = (
    ffSameAsInput,
    ffPng,
    ffJpeg,
    ffTiff,
    ffBmp,
    ffPsd,
    ffTga,
    ffJng,
    ffPpm
  );

  { TOptions }

  TOptions = class
  private
    FFiles: TStrings;
  public
    DefaultOutputFileOptions: Boolean;
    OutputFolder: string;
    OutputFileFormat: TFileFormat;
    BackgroundColor: TColor32;
    MaxAngle: Double;
    ThresholdLevel: Integer; // -1 = auto
    ForcedOutputFormat: TForcedOutputFormat;
    SkipAngle: Double;
    JpegCompressionQuality: Integer;
    TiffCompressionScheme: Integer;
    DefaultExecutable: Boolean;
    ExecutablePath: string;

    constructor Create;
    destructor Destroy; override;

    procedure ToCmdLineParameters(AParams: TStrings; AFileIndex: Integer);

    property Files: TStrings read FFiles;
  end;

implementation

uses
  ImagingUtility;

{ TOptions }

constructor TOptions.Create;
begin
  FFiles := TStringList.Create;
end;

destructor TOptions.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

procedure TOptions.ToCmdLineParameters(AParams: TStrings; AFileIndex: Integer);

  function FloatToStrFmt(const F: Double): string;
  begin
    Result := Format('%.2f', [F], ImagingUtility.GetFormatSettingsForFloats);
  end;

begin
  Assert(AFileIndex < FFiles.Count);

  AParams.Clear;

  if BackgroundColor <> $FF000000 then
    AParams.AddStrings(['-b', IntToHex(BackgroundColor, 8)]);

  // Advamced options
  if not SameFloat(MaxAngle, 10, 0.1) then
    AParams.AddStrings(['-a', FloatToStrFmt(MaxAngle)]);
  if not SameFloat(SkipAngle, 0.01, 0.01) then
    AParams.AddStrings(['-l', FloatToStrFmt(SkipAngle)]);

{$IFDEF DEBUG}
  AParams.AddStrings(['-s', 'p']);
{$ENDIF}
  AParams.Add(FFiles[AFileIndex]);
end;

end.

