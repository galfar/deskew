unit ImagingTiffMac;

{$I ImagingOptions.inc}

{$IFNDEF MACOSX}
  {$FATAL 'Mac OSX only'}
{$ENDIF}

interface

uses
  Types, SysUtils, Classes, Imaging, ImagingTypes, ImagingTiff, ImagingUtility;

type

  TTiffMacFileFormat = class(TTiffFileFormat)
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: Integer): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  end;

implementation

uses
  ImagingQuartz, ImagingIO;

{ TTiffMacFileFormat }

procedure TTiffMacFileFormat.Define;
begin
  inherited;
end;

function TTiffMacFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Stream: TCustomMemoryStream;
  Handler: TQuartzImageHandler;
begin
  Stream := TReadMemoryStream.CreateFromIOHandle(GetIO, Handle);
  Handler := TQuartzImageHandler.Create;
  try
    Handler.LoadImage(Stream, Images, OnlyFirstLevel);
  finally
     Handler.Free;
     Stream.Free;
  end;
end;

function TTiffMacFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: Integer): Boolean;
begin

end;

procedure TTiffMacFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
begin
  inherited;

end;

initialization
  RegisterImageFileFormat(TTiffMacFileFormat);



end.

