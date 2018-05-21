unit ImagingTiff;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Imaging, ImagingTypes, ImagingUtility, ImagingIO, ImagingExtras;

type
  { TIFF (Tag Image File Format) loader/saver base class.}
  TBaseTiffFileFormat = class(TImageFileFormat)
  protected
    FCompression: Integer;
    FJpegQuality: Integer;
    procedure Define; override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
    { Specifies compression scheme used when saving TIFF images. Supported values
      are 0 (Uncompressed), 1 (LZW), 2 (PackBits RLE), 3 (Deflate - ZLib), 4 (JPEG),
      5 (CCITT Group 4 fax encoding - for binary images only).
      Default is 1 (LZW). Note that not all images can be stored with
      JPEG compression - these images will be saved with default compression if
      JPEG is set.}
    property Compression: Integer read FCompression write FCompression;
    { Controls compression quality when selected TIFF compression is Jpeg.
      It is number in range 1..100. 1 means small/ugly file,
      100 means large/nice file. Accessible trough ImagingTiffJpegQuality option.}
    property JpegQuality: Integer read FJpegQuality write FJpegQuality;
  end;

const
  { Read only metadata info - name of compression scheme (LZW, none, JPEG, G4, ...)
    used in last loaded TIFF. }
  SMetaTiffCompressionName = 'TiffCompressionName';
  { Original resolution unit of loaded TIFF. Type is UInt.
    RESUNIT_NONE                        = 1;       // no meaningful units
    RESUNIT_INCH                        = 2;       // english
    RESUNIT_CENTIMETER                  = 3;       // metric }
  SMetaTiffResolutionUnit = 'TiffResolutionUnit';

implementation

// So far we have only one TIFF support implementation - libtiff
{$IF (Defined(DELPHI) and not Defined(CPUX64)) or (Defined(FPC) and not Defined(CPUARM)))}
uses
  ImagingTiffLib;
{$IFEND}

const
  STiffFormatName = 'Tagged Image File Format';
  STiffMasks      = '*.tif,*.tiff';
  TiffDefaultCompression = 1;
  TiffDefaultJpegQuality = 90;

const
  TiffBEMagic: TChar4 = 'MM'#0#42;
  TiffLEMagic: TChar4 = 'II'#42#0;

{
  TBaseTiffFileFormat implementation
}

procedure TBaseTiffFileFormat.Define;
begin
  inherited;
  FName := STiffFormatName;
  FFeatures := [ffLoad, ffSave, ffMultiImage];
  FCompression := TiffDefaultCompression;
  FJpegQuality := TiffDefaultJpegQuality;

  AddMasks(STiffMasks);
  RegisterOption(ImagingTiffCompression, @FCompression);
  RegisterOption(ImagingTiffJpegQuality, @FJpegQuality);
end;

function TBaseTiffFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Magic: TChar4;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Magic, SizeOf(Magic));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount >= SizeOf(Magic)) and
      ((Magic = TiffBEMagic) or (Magic = TiffLEMagic));
  end;
end;

end.
