unit Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ImagingTypes;

type
  TForcedOutputFormat = (
    fofBinary1,
    fofGray8,
    fofRgb24,
    forRgba32
  );

  { TOptions }

  TOptions = class
  private
    FFiles: TStrings;
  public
    DefaultOutput: Boolean;
    OutputFolder: string;
    OutputName: string;
    OutputFileFormat: string;
    BackgroundColor: TColor32;
    MaxAngle: Double;
    ThresholdLevel: Integer; // -1 = auto
    ForcedOutputFormat: TForcedOutputFormat;
    SkipAngle: Double;
    JpegCompressionQuality: Integer;
    TiffCompressionScheme: Integer;

    ExecutablePath: string;

    constructor Create;
    destructor Destroy; override;

    procedure ToCmdLineParameters(AParams: TStrings; AFileIndex: Integer);

    property Files: TStrings read FFiles;
  end;

implementation

{ TOptions }

constructor TOptions.Create;
begin
  FFiles := TStringList.Create;
  ExecutablePath := './deskew';
end;

destructor TOptions.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

procedure TOptions.ToCmdLineParameters(AParams: TStrings; AFileIndex: Integer);
begin
  Assert(AFileIndex < FFiles.Count);

  AParams.Clear;

 // AParams.Add('-khkhj');

  AParams.Add(FFiles[AFileIndex]);
end;

end.

