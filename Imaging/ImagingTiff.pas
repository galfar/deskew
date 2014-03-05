unit ImagingTiff;

{$I ImagingOptions.inc}

{$IF Defined(LINUX) or Defined(BSD)}
  // Use LibTiff dynamic library in Linux/BSD instead of precompiled objects.
  // It's installed on most systems so let's use it and keep the binary smaller.
  {$DEFINE USE_DYN_LIB}
{$IFEND}

{$IF Defined(POSIX) and Defined(CPUX64)}
  // Workaround for problem on 64bit Linux where thandle_t in libtiff is
  // still 32bit so it cannot be used to pass pointers (for IO functions).
  {$DEFINE HANDLE_NOT_POINTER_SIZED}
{$IFEND}

interface

uses
  SysUtils, Imaging, ImagingTypes, ImagingUtility, ImagingIO, ImagingExtras;

type
  { TIFF (Tag Image File Format) loader/saver base class.}
  TTiffFileFormat = class(TImageFileFormat)
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

implementation

const
  STiffFormatName = 'Tagged Image File Format';
  STiffMasks      = '*.tif,*.tiff';
  TiffDefaultCompression = 1;
  TiffDefaultJpegQuality = 90;
  TiffDefaultAppendMode = False;

const
  TiffBEMagic: TChar4 = 'MM'#0#42;
  TiffLEMagic: TChar4 = 'II'#42#0;

{
  TTiffFileFormat implementation
}

procedure TTiffFileFormat.Define;
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

function TTiffFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
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