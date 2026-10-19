{
  Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  https://imaginglib.sourceforge.io
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
}

{ All JPEG coding in Imaging goes through this unit. It selects the JPEG
  codec (backend) and wraps it in a small API working with Imaging's image
  data and IO functions. The rest of Imaging (ImagingJpeg, JNG support in
  ImagingNetworkGraphics) doesn't use any JPEG library types or functions
  directly.

  Backends:

  JPEG_USE_BUNDLED      - ImPasJpeg unit, Pascal translation of IJG's libjpeg 6b
                          bundled with Imaging. Compiles everywhere (Delphi 7+, FPC)
                          and needs no external files.
  JPEG_USE_FPC          - pasjpeg package of Free Pascal (the same translation as
                          ImPasJpeg). Every Lazarus LCL application already has it
                          linked in, so using it avoids having two almost identical
                          libraries in the binary.
  JPEG_USE_TURBO_DYNLIB - TurboJPEG API of libjpeg-turbo shared library bound by
                          LibTurboJpegDynLib unit: turbojpeg.dll, libturbojpeg.so.0,
                          or libturbojpeg.0.dylib. Several times faster than the
                          Pascal codecs. Needs libjpeg-turbo 2.0 or newer.
                          Ignored on targets without dynamic libraries.

  Selection:

  One of the Pascal codecs is always compiled in, chosen automatically:
    - LCL is defined (Lazarus applications): JPEG_USE_FPC
    - otherwise: JPEG_USE_BUNDLED
  Define JPEG_USE_BUNDLED to use the bundled codec also with LCL.

  JPEG_USE_TURBO_DYNLIB is an addition to the Pascal codec, not a replacement.
  When it is defined the library is looked for when the first JPEG is loaded
  or saved (or by TryLoadTurboJpegLibrary). If a usable library is found it
  does all loading and saving from then on, otherwise the Pascal codec is
  used, so the program works without the library too. JpegBackend tells
  which one is active.

  The defines are listed in ImagingOptions.inc, set them there, in project
  options, or in ImagingUserOptions.inc.

  Notes on TurboJPEG backend:
    - The library works on whole files in memory: the rest of the input
      (from the current position) is read to a buffer when loading.
    - Images decode to the same pixels as with the Pascal codecs and
      saved files are the same too.
    - It is more tolerant to damaged files, e.g. missing Huffman tables.
    - Arithmetic coded files can not be loaded by the Pascal codec
      but libjpeg-turbo loads them (unless the library was built without
      arithmetic coding support).
}
unit ImagingJpegLib;

{$I ImagingOptions.inc}

{$IFNDEF HAS_DYNLIBS}
  {$UNDEF JPEG_USE_TURBO_DYNLIB}
{$ENDIF}

{$IF Defined(JPEG_USE_FPC) and not Defined(FPC)}
  {$UNDEF JPEG_USE_FPC}
{$IFEND}

{$IF not Defined(JPEG_USE_BUNDLED) and not Defined(JPEG_USE_FPC)}
  {$DEFINE NO_USER_JPEGLIB}
{$IFEND}

{$IF Defined(LCL) and Defined(NO_USER_JPEGLIB)}
  { Automatically use FPC's PasJpeg when compiling LCL applications.
    For just FPC we use JPEG_USE_BUNDLED, it's really the same decades old translation from C. }
  {$DEFINE JPEG_USE_FPC}
{$IFEND}

{$IF Defined(JPEG_USE_BUNDLED)}
  {$UNDEF JPEG_USE_FPC}
{$ELSEIF not Defined(JPEG_USE_FPC)}
  {$DEFINE JPEG_USE_BUNDLED} // Fallback
{$IFEND}

{$IF Defined(JPEG_USE_FPC)}
  { When using FPC's PasJpeg the channel order is BGR instead of RGB.
    See RGB_RED_IS_0 in jconfig.inc. }
  {$DEFINE RGBSWAPPED}
{$IFEND}

{ We usually want to skip the rest of the corrupted file when loading JPEG files
  instead of getting exception. JpegLib's error handler can only be
  exited using setjmp/longjmp ("non-local goto") functions to get error
  recovery when loading corrupted JPEG files. This is implemented in assembler
  and currently available only for 32bit Delphi targets and FPC.}
{$DEFINE ErrorJmpRecovery}
{$IF Defined(DCC) and not Defined(CPUX86)}
  {$UNDEF ErrorJmpRecovery}
{$IFEND}

interface

uses
  ImagingTypes, Imaging;

type
  { Color space of image data returned by JpegLoadImage. }
  TJpegColorSpace = (
    jcsGray,  // ifGray8 image
    jcsRGB,   // ifR8G8B8 image in Imaging's native channel order
    jcsCMYK   // ifA8R8G8B8 image with CMYK samples as stored in the file
              // (inverted for Adobe JPEGs) in byte order, so C=B, M=G, Y=R, K=A
  );

  { Information about JPEG image loaded by JpegLoadImage. }
  TJpegImageInfo = record
    ColorSpace: TJpegColorSpace;
    { False if decoding of corrupted file stopped early and the image
      contains only the part decoded so far. Fields below are valid
      only when the whole image was decoded. }
    Complete: Boolean;
    { True if the file has JFIF marker (with density info). }
    HasJFIF: Boolean;
    { JFIF density unit: 0 - unknown (only aspect ratio), 1 - dots per inch,
      2 - dots per cm. }
    DensityUnit: Integer;
    XDensity: Integer;
    YDensity: Integer;
  end;

  { Chroma subsampling used when saving color JPEG images
    (resolution of Cb and Cr components relative to Y). }
  TJpegChromaSubsampling = (
    jss420,  // half horizontal and half vertical resolution (default)
    jss422,  // half horizontal, full vertical resolution
    jss444   // full resolution, no subsampling
  );

  { Parameters for saving JPEG images with JpegSaveImage. }
  TJpegSaveParams = record
    { Compression quality, 1..100. }
    Quality: Integer;
    { Save as progressive JPEG .}
    Progressive: Boolean;
    { Chroma subsampling of color images, ignored for grayscale ones. }
    ChromaSubsampling: TJpegChromaSubsampling;
    { JFIF density info written to file, used only if DensityUnit > 0.
      Unit is 1 for dots per inch and 2 for dots per cm. }
    DensityUnit: Integer;
    XDensity: Integer;
    YDensity: Integer;
  end;

  { Color space of the data stored in a JPEG stream, for callers that know it
    better than the JPEG library can guess from the stream's markers. That is
    the case for JPEG data embedded in other formats: in TIFF it is the
    PhotometricInterpretation tag that says whether three components are
    YCbCr or untransformed RGB, and the stream itself usually has no JFIF or
    Adobe marker to tell. }
  TJpegSourceColorSpace = (
    jscAuto,   // let the JPEG library decide (as plain JPEG files need)
    jscGray,   // one component, loaded as ifGray8
    jscYCbCr,  // three components YCbCr, converted to RGB and loaded as ifR8G8B8
    jscRGB,    // three components stored as RGB, loaded as ifR8G8B8
    jscCMYK    // four components loaded as they are, see jcsCMYK
  );

  { JPEG codec implementation in use. }
  TJpegBackend = (
    jbPascal,     // Pascal codec compiled in (ImPasJpeg or FPC's PasJpeg)
    jbTurboJpeg   // TurboJPEG shared library
  );

{ Returns JPEG implementation in use. In builds with JPEG_USE_TURBO_DYNLIB it looks
  for the library (in default locations) when called for the first time. }
function JpegBackend: TJpegBackend;

{ Loads TurboJPEG library from the given file and uses it from now on (instead
  of looking for it in the default locations). Returns False and falls back
  to the Pascal codec when the library can't be loaded or is older than
  libjpeg-turbo 2.0. Always False in builds without JPEG_USE_TURBO_DYNLIB. }
function TryLoadTurboJpegLibrary(const FileName: string): Boolean;

{ Loads JPEG image from input represented by Handle and read using IO functions.
  New image is created in Image, its format depends on the color space
  of the file (see TJpegColorSpace). Returns False for unsupported
  color spaces, raises EImagingError on errors.
  Errors in image data are recovered from when possible: True is returned and
  Info.Complete is False in this case. After loading, the input position is
  just after the end of JPEG data. }
function JpegLoadImage(const IO: TIOFunctions; Handle: TImagingHandle;
  var Image: TImageData; out Info: TJpegImageInfo): Boolean; overload;
{ The same with color space of the stored data given by the caller.
  Returns False if SourceColorSpace does not fit the number of components
  of the JPEG stream. With the TurboJPEG backend, streams whose color space
  TurboJPEG would see differently (untransformed RGB) are loaded by the Pascal
  codec, TurboJPEG cannot be told about it. }
function JpegLoadImage(const IO: TIOFunctions; Handle: TImagingHandle;
  var Image: TImageData; out Info: TJpegImageInfo;
  SourceColorSpace: TJpegSourceColorSpace): Boolean; overload;

{ Saves Image as JPEG to output represented by Handle and written using IO functions.
  Image must be in ifGray8 or ifR8G8B8 format. Raises EImagingError on errors. }
function JpegSaveImage(const IO: TIOFunctions; Handle: TImagingHandle;
  const Image: TImageData; const Params: TJpegSaveParams): Boolean;

implementation

uses
  SysUtils,
{$IF Defined(JPEG_USE_BUNDLED)}
  ImPasJpeg,
{$ELSEIF Defined(JPEG_USE_FPC)}
  jpeglib, jmorecfg, jcomapi, jdapimin, jdeferr, jerror,
  jdapistd, jcapimin, jcapistd, jdmarker, jcparam,
{$IFEND}
{$IFDEF JPEG_USE_TURBO_DYNLIB}
  LibTurboJpegDynLib, ImagingIO,
{$ENDIF}
  ImagingUtility;

const
  BufferSize = 16384;

resourcestring
  SJpegError = 'JPEG Error';

type
  TJpegContext = record
    case Byte of
      0: (common: jpeg_common_struct);
      1: (d: jpeg_decompress_struct);
      2: (c: jpeg_compress_struct);
  end;

  TSourceMgr = record
    Pub: jpeg_source_mgr;
    IO: TIOFunctions;
    Input: TImagingHandle;
    Buffer: JOCTETPTR;
    StartOfFile: Boolean;
  end;
  PSourceMgr = ^TSourceMgr;

  TDestMgr = record
    Pub: jpeg_destination_mgr;
    IO: TIOFunctions;
    Output: TImagingHandle;
    Buffer: JOCTETPTR;
  end;
  PDestMgr = ^TDestMgr;

{$IFDEF ErrorJmpRecovery}
  {$IFDEF DCC}
  type
    jmp_buf = record
      EBX,
      ESI,
      EDI,
      ESP,
      EBP,
      EIP: UInt32;
    end;
    pjmp_buf = ^jmp_buf;

  { JmpLib SetJmp/LongJmp Library
    (C)Copyright 2003, 2004 Will DeWitt Jr. <edge@boink.net> }
  function  SetJmp(out jmpb: jmp_buf): Integer;
  asm
  {     ->  EAX     jmpb   }
  {     <-  EAX     Result }
            MOV     EDX, [ESP]  // Fetch return address (EIP)
            // Save task state
            MOV     [EAX+jmp_buf.&EBX], EBX
            MOV     [EAX+jmp_buf.&ESI], ESI
            MOV     [EAX+jmp_buf.&EDI], EDI
            MOV     [EAX+jmp_buf.&ESP], ESP
            MOV     [EAX+jmp_buf.&EBP], EBP
            MOV     [EAX+jmp_buf.&EIP], EDX

            SUB     EAX, EAX
  @@1:
  end;

  procedure LongJmp(const jmpb: jmp_buf; retval: Integer);
  asm
  {     ->  EAX     jmpb   }
  {         EDX     retval }
  {     <-  EAX     Result }
            XCHG    EDX, EAX

            MOV     ECX, [EDX+jmp_buf.&EIP]
            // Restore task state
            MOV     EBX, [EDX+jmp_buf.&EBX]
            MOV     ESI, [EDX+jmp_buf.&ESI]
            MOV     EDI, [EDX+jmp_buf.&EDI]
            MOV     ESP, [EDX+jmp_buf.&ESP]
            MOV     EBP, [EDX+jmp_buf.&EBP]
            MOV     [ESP], ECX  // Restore return address (EIP)

            TEST    EAX, EAX    // Ensure retval is <> 0
            JNZ     @@1
            MOV     EAX, 1
  @@1:
  end;
  {$ENDIF}

type
  TJmpBuf = jmp_buf;
  TErrorClientData = record
    JmpBuf: TJmpBuf;
    ScanlineReadReached: Boolean;
  end;
  PErrorClientData = ^TErrorClientData;
{$ENDIF}

procedure JpegError(CInfo: j_common_ptr);

  procedure RaiseError;
  var
    // Current FPC trunk 3.3.x switched PasJpeg for some reason to ShortStrings
    Buffer: {$IF Defined(JPEG_USE_FPC) and (FPC_FULLVERSION >= 30300)}shortstring
            {$ELSE}AnsiString
            {$IFEND};
  begin
    // Create the message and raise exception
    CInfo.err.format_message(CInfo, Buffer);
    raise EImagingError.CreateFmt(SJPEGError + ' %d: ' + string(Buffer), [CInfo.err.msg_code]);
  end;

begin
{$IFDEF ErrorJmpRecovery}
  // Only recovers on loads and when header is successfully loaded
  // (error occurs when reading scanlines)
  if (CInfo.client_data <> nil) and
    PErrorClientData(CInfo.client_data).ScanlineReadReached then
  begin
    // Non-local jump to error handler in JpegLoadImage
    longjmp(PErrorClientData(CInfo.client_data).JmpBuf, 1)
  end
  else
    RaiseError;
{$ELSE}
  RaiseError;
{$ENDIF}
end;

procedure OutputMessage(CurInfo: j_common_ptr);
begin
end;

procedure ReleaseContext(var jc: TJpegContext);
begin
  if jc.common.err = nil then
    Exit;
  jpeg_destroy(@jc.common);
  jpeg_destroy_decompress(@jc.d);
  jpeg_destroy_compress(@jc.c);
  jc.common.err := nil;
end;

procedure InitSource(cinfo: j_decompress_ptr);
begin
  PSourceMgr(cinfo.src).StartOfFile := True;
end;

function FillInputBuffer(cinfo: j_decompress_ptr): Boolean;
var
  NBytes: LongInt;
  Src: PSourceMgr;
begin
  Src := PSourceMgr(cinfo.src);
  NBytes := Src.IO.Read(Src.Input, Src.Buffer, BufferSize);

  if NBytes <= 0 then
  begin
    PByteArray(Src.Buffer)[0] := $FF;
    PByteArray(Src.Buffer)[1] := JPEG_EOI;
    NBytes := 2;
  end;
  Src.Pub.next_input_byte := Src.Buffer;
  Src.Pub.bytes_in_buffer := NBytes;
  Src.StartOfFile := False;
  Result := True;
end;

procedure SkipInputData(cinfo: j_decompress_ptr; num_bytes: LongInt);
var
  Src: PSourceMgr;
begin
  Src := PSourceMgr(cinfo.src);
  if num_bytes > 0 then
  begin
    while num_bytes > Src.Pub.bytes_in_buffer do
    begin
      Dec(num_bytes, Src.Pub.bytes_in_buffer);
      FillInputBuffer(cinfo);
    end;
    Src.Pub.next_input_byte := @PByteArray(Src.Pub.next_input_byte)[num_bytes];
    Dec(Src.Pub.bytes_in_buffer, num_bytes);
  end;
end;

procedure TermSource(cinfo: j_decompress_ptr);
var
  Src: PSourceMgr;
begin
  Src := PSourceMgr(cinfo.src);
  // Move stream position back just after EOI marker so that more that one
  // JPEG images can be loaded from one stream
  Src.IO.Seek(Src.Input, -Src.Pub.bytes_in_buffer, smFromCurrent);
end;

procedure JpegStdioSrc(var cinfo: jpeg_decompress_struct; const IO: TIOFunctions;
  Handle: TImagingHandle);
var
  Src: PSourceMgr;
begin
  if cinfo.src = nil then
  begin
    cinfo.src := cinfo.mem.alloc_small(j_common_ptr(@cinfo), JPOOL_PERMANENT,
      SizeOf(TSourceMgr));
    Src := PSourceMgr(cinfo.src);
    Src.Buffer := cinfo.mem.alloc_small(j_common_ptr(@cinfo), JPOOL_PERMANENT,
      BufferSize * SizeOf(JOCTET));
  end;
  Src := PSourceMgr(cinfo.src);
  Src.Pub.init_source := InitSource;
  Src.Pub.fill_input_buffer := FillInputBuffer;
  Src.Pub.skip_input_data := SkipInputData;
  Src.Pub.resync_to_restart := jpeg_resync_to_restart;
  Src.Pub.term_source := TermSource;
  Src.IO := IO;
  Src.Input := Handle;
  Src.Pub.bytes_in_buffer := 0;
  Src.Pub.next_input_byte := nil;
end;

procedure InitDest(cinfo: j_compress_ptr);
var
  Dest: PDestMgr;
begin
  Dest := PDestMgr(cinfo.dest);
  Dest.Pub.next_output_byte := Dest.Buffer;
  Dest.Pub.free_in_buffer := BufferSize;
end;

function EmptyOutput(cinfo: j_compress_ptr): Boolean;
var
  Dest: PDestMgr;
begin
  Dest := PDestMgr(cinfo.dest);
  Dest.IO.Write(Dest.Output, Dest.Buffer, BufferSize);
  Dest.Pub.next_output_byte := Dest.Buffer;
  Dest.Pub.free_in_buffer := BufferSize;
  Result := True;
end;

procedure TermDest(cinfo: j_compress_ptr);
var
  Dest: PDestMgr;
  DataCount: LongInt;
begin
  Dest := PDestMgr(cinfo.dest);
  DataCount := BufferSize - Dest.Pub.free_in_buffer;
  if DataCount > 0 then
    Dest.IO.Write(Dest.Output, Dest.Buffer, DataCount);
end;

procedure JpegStdioDest(var cinfo: jpeg_compress_struct; const IO: TIOFunctions;
  Handle: TImagingHandle);
var
  Dest: PDestMgr;
begin
  if cinfo.dest = nil then
    cinfo.dest := cinfo.mem.alloc_small(j_common_ptr(@cinfo), JPOOL_PERMANENT, SizeOf(TDestMgr));
  Dest := PDestMgr(cinfo.dest);
  Dest.Buffer := cinfo.mem.alloc_small(j_common_ptr(@cinfo), JPOOL_IMAGE, BufferSize * SIZEOF(JOCTET));
  Dest.Pub.init_destination := InitDest;
  Dest.Pub.empty_output_buffer := EmptyOutput;
  Dest.Pub.term_destination := TermDest;
  Dest.IO := IO;
  Dest.Output := Handle;
end;

procedure SetupErrorMgr(var jc: TJpegContext; var ErrorMgr: jpeg_error_mgr);
begin
  // Set standard error handlers and then override some
  jc.common.err := jpeg_std_error(ErrorMgr);
  jc.common.err.error_exit := JpegError;
  jc.common.err.output_message := OutputMessage;
end;

procedure InitDecompressor(const IO: TIOFunctions; Handle: TImagingHandle;
  var jc: TJpegContext);
begin
  jpeg_CreateDecompress(@jc.d, JPEG_LIB_VERSION, sizeof(jc.d));
  JpegStdioSrc(jc.d, IO, Handle);
  jpeg_read_header(@jc.d, True);
  jc.d.scale_num := 1;
  jc.d.scale_denom := 1;
  jc.d.do_block_smoothing := True;
end;

procedure InitCompressor(const IO: TIOFunctions; Handle: TImagingHandle;
  var jc: TJpegContext; GrayScale: Boolean; const Params: TJpegSaveParams);
const
  // JpegLib sets chroma resolution through sampling factors relative to the
  // largest factor of all components. Cb and Cr keep factor 1x1 (set by
  // jpeg_set_defaults) and the Y (luma) factor determines how much chroma
  // is subsampled: 2x2 = 4:2:0, 2x1 = 4:2:2, 1x1 = 4:4:4.
  LumaSamplingFactors: array[TJpegChromaSubsampling, 0..1] of Integer =
    ((2, 2), (2, 1), (1, 1));
begin
  jpeg_CreateCompress(@jc.c, JPEG_LIB_VERSION, sizeof(jc.c));
  JpegStdioDest(jc.c, IO, Handle);
  if GrayScale then
    jc.c.in_color_space := JCS_GRAYSCALE
  else
    jc.c.in_color_space := JCS_RGB;
  jpeg_set_defaults(@jc.c);

  if not GrayScale then
  begin
    jc.c.comp_info^[0].h_samp_factor := LumaSamplingFactors[Params.ChromaSubsampling, 0];
    jc.c.comp_info^[0].v_samp_factor := LumaSamplingFactors[Params.ChromaSubsampling, 1];
  end;

  jpeg_set_quality(@jc.c, Params.Quality, True);
  if Params.Progressive then
    jpeg_simple_progression(@jc.c);
end;

function PasLoadImage(const IO: TIOFunctions; Handle: TImagingHandle;
  var Image: TImageData; out Info: TJpegImageInfo;
  SourceColorSpace: TJpegSourceColorSpace): Boolean;
const
  SourceComponents: array[TJpegSourceColorSpace] of Integer = (0, 1, 3, 3, 4);
var
  PtrInc, LinesPerCall, LinesRead, I: Integer;
  Dest: PByte;
  jc: TJpegContext;
  ErrorMgr: jpeg_error_mgr;
  FmtInfo: TImageFormatInfo;
  NeedsRedBlueSwap: Boolean;
  Pix: PColor24Rec;
{$IFDEF ErrorJmpRecovery}
  ErrorClient: TErrorClientData;
{$ENDIF}
begin
  Result := False;
  FillChar(Info, SizeOf(Info), 0);

  with Image do
  try
    FillChar(jc, SizeOf(jc), 0);
    SetupErrorMgr(jc, ErrorMgr);
  {$IFDEF ErrorJmpRecovery}
    FillChar(ErrorClient, SizeOf(ErrorClient), 0);
    jc.common.client_data := @ErrorClient;
    if setjmp(ErrorClient.JmpBuf) <> 0 then
    begin
      Result := True;
      Exit;
    end;
  {$ENDIF}
    InitDecompressor(IO, Handle, jc);

    if SourceColorSpace <> jscAuto then
    begin
      // Caller knows what the components are: replace the guess JpegLib
      // made in jpeg_read_header and select the matching output.
      if jc.d.num_components <> SourceComponents[SourceColorSpace] then
        Exit;
      case SourceColorSpace of
        jscGray:
          begin
            jc.d.jpeg_color_space := JCS_GRAYSCALE;
            jc.d.out_color_space := JCS_GRAYSCALE;
          end;
        jscYCbCr:
          begin
            jc.d.jpeg_color_space := JCS_YCbCr;
            jc.d.out_color_space := JCS_RGB;
          end;
        jscRGB:
          begin
            jc.d.jpeg_color_space := JCS_RGB;
            jc.d.out_color_space := JCS_RGB;
          end;
        jscCMYK:
          begin
            jc.d.jpeg_color_space := JCS_CMYK;
            jc.d.out_color_space := JCS_CMYK;
          end;
      end;
    end;

    case jc.d.out_color_space of
      JCS_GRAYSCALE:
        begin
          Info.ColorSpace := jcsGray;
          Format := ifGray8;
        end;
      JCS_RGB:
        begin
          Info.ColorSpace := jcsRGB;
          Format := ifR8G8B8;
        end;
      JCS_CMYK:
        begin
          Info.ColorSpace := jcsCMYK;
          Format := ifA8R8G8B8;
        end;
    else
      Exit;
    end;

    NewImage(jc.d.image_width, jc.d.image_height, Format, Image);
    jpeg_start_decompress(@jc.d);
    GetImageFormatInfo(Format, FmtInfo);
    PtrInc := Width * FmtInfo.BytesPerPixel;
    LinesPerCall := 1;
    Dest := Bits;

    // If Jpeg's colorspace is RGB and not YCbCr we need to swap
    // R and B to get Imaging's native order
    NeedsRedBlueSwap := (jc.d.jpeg_color_space = JCS_RGB);
  {$IFDEF RGBSWAPPED}
    // Force R-B swap for FPC's PasJpeg
    NeedsRedBlueSwap := True;
  {$ENDIF}

  {$IFDEF ErrorJmpRecovery}
    ErrorClient.ScanlineReadReached := True;
  {$ENDIF}

    while jc.d.output_scanline < jc.d.output_height do
    begin
      LinesRead := jpeg_read_scanlines(@jc.d, @Dest, LinesPerCall);
      if NeedsRedBlueSwap and (Format = ifR8G8B8) then
      begin
        Pix := PColor24Rec(Dest);
        for I := 0 to Width - 1 do
        begin
          SwapValues(Pix.R, Pix.B);
          Inc(Pix);
        end;
      end;
      Inc(Dest, PtrInc * LinesRead);
    end;

    Info.Complete := True;
    Info.HasJFIF := jc.d.saw_JFIF_marker;
    Info.DensityUnit := jc.d.density_unit;
    Info.XDensity := jc.d.X_density;
    Info.YDensity := jc.d.Y_density;

    jpeg_finish_output(@jc.d);
    jpeg_finish_decompress(@jc.d);
    Result := True;
  finally
    ReleaseContext(jc);
  end;
end;

function PasSaveImage(const IO: TIOFunctions; Handle: TImagingHandle;
  const Image: TImageData; const Params: TJpegSaveParams): Boolean;
var
  PtrInc, LinesWritten: LongInt;
  Src, Line: PByte;
  jc: TJpegContext;
  ErrorMgr: jpeg_error_mgr;
  FmtInfo: TImageFormatInfo;
  GrayScale: Boolean;
{$IFDEF RGBSWAPPED}
  I: LongInt;
  Pix: PColor24Rec;
  SwapBuffer: TDynByteArray;
{$ENDIF}
begin
  with Image do
  try
    FillChar(jc, SizeOf(jc), 0);
    SetupErrorMgr(jc, ErrorMgr);

    GetImageFormatInfo(Format, FmtInfo);
    GrayScale := Format = ifGray8;
    InitCompressor(IO, Handle, jc, GrayScale, Params);
    jc.c.image_width := Width;
    jc.c.image_height := Height;
    if GrayScale then
    begin
      jc.c.input_components := 1;
      jc.c.in_color_space := JCS_GRAYSCALE;
    end
    else
    begin
      jc.c.input_components := 3;
      jc.c.in_color_space := JCS_RGB;
    end;

    PtrInc := Width * FmtInfo.BytesPerPixel;
    Src := Bits;

  {$IFDEF RGBSWAPPED}
    SetLength(SwapBuffer, PtrInc);
  {$ENDIF}

    if Params.DensityUnit > 0 then
    begin
      jc.c.density_unit := Params.DensityUnit;
      jc.c.X_density := Params.XDensity;
      jc.c.Y_density := Params.YDensity;
    end;

    jpeg_start_compress(@jc.c, True);
    while (jc.c.next_scanline < jc.c.image_height) do
    begin
      Line := Src;
    {$IFDEF RGBSWAPPED}
      if Format = ifR8G8B8 then
      begin
        Move(Src^, SwapBuffer[0], PtrInc);
        Pix := PColor24Rec(SwapBuffer);
        for I := 0 to Width - 1 do
        begin
          SwapValues(Pix.R, Pix.B);
          Inc(Pix, 1);
        end;
        Line := Pointer(SwapBuffer);
      end;
    {$ENDIF}

      LinesWritten := jpeg_write_scanlines(@jc.c, @Line, 1);
      Inc(Src, PtrInc * LinesWritten);
    end;

    jpeg_finish_compress(@jc.c);
    Result := True;
  finally
    ReleaseContext(jc);
  end;
end;

{$IFDEF JPEG_USE_TURBO_DYNLIB}

{ TurboJPEG backend }

const
  JfifHeaderSize = 18; // SOI + APP0 marker up to the density fields

type
  { What the scan of markers in JPEG data found. }
  TJpegScanInfo = record
    DataSize: Int64;     // Size of JPEG data up to and including EOI marker
    HasJFIF: Boolean;
    DensityUnit: Integer;
    XDensity: Integer;
    YDensity: Integer;
  end;

procedure RaiseTurboError(TJ: TTJHandle);
begin
  raise EImagingError.Create(SJpegError + ': ' + string(AnsiString(tjGetErrorStr2(TJ))));
end;

{ TurboJPEG doesn't tell where JPEG data ends in the buffer and has no access
  to JFIF density, so the markers are walked here. If the end (EOI marker)
  is not found the whole buffer is taken as JPEG data. }
procedure ScanJpegData(Data: PBuffer; Size: Int64; out Scan: TJpegScanInfo);
var
  Pos, DataEnd, SegSize: PtrInt;
  Marker: Byte;
begin
  FillChar(Scan, SizeOf(Scan), 0);
  Scan.DataSize := Size;
  DataEnd := PtrInt(Size);

  if (DataEnd < 4) or (Data[0] <> $FF) or (Data[1] <> $D8) then
    Exit;

  Pos := 2;

  while Pos + 1 < DataEnd do
  begin
    if Data[Pos] <> $FF then
    begin
      // Entropy coded data
      Inc(Pos);
      Continue;
    end;

    Marker := Data[Pos + 1];

    if Marker = $FF then
    begin
      // Fill byte before a marker
      Inc(Pos);
    end
    else if (Marker <= $01) or ((Marker >= $D0) and (Marker <= $D8)) then
    begin
      // Stuffed zero in entropy coded data and markers without parameters
      Inc(Pos, 2);
    end
    else if Marker = $D9 then
    begin
      Scan.DataSize := Pos + 2;
      Exit;
    end
    else
    begin
      // Marker segment with size, entropy coded data can follow it
      if Pos + 3 >= DataEnd then
        Exit;

      SegSize := (Data[Pos + 2] shl 8) or Data[Pos + 3];
      if SegSize < 2 then
        Exit;

      if (Marker = $E0) and not Scan.HasJFIF and (SegSize >= 14) and
         (Pos + 2 + SegSize <= DataEnd) and (Data[Pos + 4] = Ord('J')) and
         (Data[Pos + 5] = Ord('F')) and (Data[Pos + 6] = Ord('I')) and
         (Data[Pos + 7] = Ord('F')) and (Data[Pos + 8] = 0) then
      begin
        Scan.HasJFIF := True;
        Scan.DensityUnit := Data[Pos + 11];
        Scan.XDensity := (Data[Pos + 12] shl 8) or Data[Pos + 13];
        Scan.YDensity := (Data[Pos + 14] shl 8) or Data[Pos + 15];
      end;

      Inc(Pos, 2 + SegSize);
    end;
  end;
end;

function TurboLoadImage(const IO: TIOFunctions; Handle: TImagingHandle;
  var Image: TImageData; out Info: TJpegImageInfo;
  SourceColorSpace: TJpegSourceColorSpace): Boolean;
var
  Buffer: TDynByteArray;
  StartPos: Int64;
  Scan: TJpegScanInfo;
  TJHandle: TTJHandle;
  Width, Height, Subsamp, ColorSpace, PixelFormat, Res: Int32;
  Format: TImageFormat;
begin
  Result := False;
  FillChar(Info, SizeOf(Info), 0);
  Width := 0;
  Height := 0;
  Subsamp := 0;
  ColorSpace := 0;

  StartPos := IO.Tell(Handle);
  ReadRestOfInput(IO, Handle, Buffer);
  ScanJpegData(Pointer(Buffer), Length(Buffer), Scan);

  TJHandle := tjInitDecompress;
  if TJHandle = nil then
    raise EImagingError.Create(SJpegError);

  try
    // Warnings are reported as failures too, only fatal errors stop the loading
    Res := tjDecompressHeader3(TJHandle, Pointer(Buffer), Scan.DataSize, Width, Height,
                               Subsamp, ColorSpace);
    if (Res <> 0) and ((tjGetErrorCode(TJHandle) <> TJERR_WARNING) or
       (Width <= 0) or (Height <= 0)) then
    begin
      RaiseTurboError(TJHandle);
    end;

    // TurboJPEG decides about color space by itself. When the caller says
    // something else (e.g. untransformed RGB, which TurboJPEG takes for
    // YCbCr) the stream goes to the Pascal codec instead.
    if ((SourceColorSpace = jscGray) and (ColorSpace <> TJCS_GRAY)) or
       ((SourceColorSpace = jscYCbCr) and (ColorSpace <> TJCS_YCbCr)) or
       ((SourceColorSpace = jscCMYK) and (ColorSpace <> TJCS_CMYK)) or
       (SourceColorSpace = jscRGB) then
    begin
      tjDestroy(TJHandle);
      TJHandle := nil;
      IO.Seek(Handle, StartPos, smFromBeginning);
      Result := PasLoadImage(IO, Handle, Image, Info, SourceColorSpace);
      Exit;
    end;

    case ColorSpace of
      TJCS_GRAY:
        begin
          Info.ColorSpace := jcsGray;
          Format := ifGray8;
          PixelFormat := TJPF_GRAY;
        end;
      TJCS_CMYK, TJCS_YCCK:
        begin
          Info.ColorSpace := jcsCMYK;
          Format := ifA8R8G8B8;
          PixelFormat := TJPF_CMYK;
        end;
    else
      Info.ColorSpace := jcsRGB;
      Format := ifR8G8B8;
      // BGR is Imaging's native channel order
      PixelFormat := TJPF_BGR;
    end;

    if not NewImage(Width, Height, Format, Image) then
      Exit;

    Res := tjDecompress2(TJHandle, Pointer(Buffer), Scan.DataSize, Image.Bits, Width, 0,
                         Height, PixelFormat, TJFLAG_ACCURATEDCT);

    // After fatal error the image has only the part decoded so far
    Info.Complete := (Res = 0) or (tjGetErrorCode(TJHandle) = TJERR_WARNING);

    if Info.Complete then
    begin
      Info.HasJFIF := Scan.HasJFIF;
      Info.DensityUnit := Scan.DensityUnit;
      Info.XDensity := Scan.XDensity;
      Info.YDensity := Scan.YDensity;
    end;

    Result := True;
  finally
    if TJHandle <> nil then
      tjDestroy(TJHandle);
  end;

  // Move input position just after the JPEG data so that more than one
  // JPEG image can be loaded from one input
  IO.Seek(Handle, StartPos + Scan.DataSize, smFromBeginning);
end;

function TurboSaveImage(const IO: TIOFunctions; Handle: TImagingHandle;
  const Image: TImageData; const Params: TJpegSaveParams): Boolean;
const
  Subsamplings: array[TJpegChromaSubsampling] of Int32 =
    (TJSAMP_420, TJSAMP_422, TJSAMP_444);
var
  TJHandle: TTJHandle;
  JpegData: PByte;
  JpegSize: TTJULong;
  PixelFormat, Subsamp, Flags, Res: Int32;
  Header: PBuffer;
begin
  Result := False;

  if Image.Format = ifGray8 then
  begin
    PixelFormat := TJPF_GRAY;
    Subsamp := TJSAMP_GRAY;
  end
  else
  begin
    PixelFormat := TJPF_BGR;
    Subsamp := Subsamplings[Params.ChromaSubsampling];
  end;

  Flags := TJFLAG_ACCURATEDCT;
  if Params.Progressive then
    Flags := Flags or TJFLAG_PROGRESSIVE;

  TJHandle := tjInitCompress;
  if TJHandle = nil then
    raise EImagingError.Create(SJpegError);

  JpegData := nil;
  JpegSize := 0;

  try
    Res := tjCompress2(TJHandle, Image.Bits, Image.Width, 0, Image.Height, PixelFormat,
                       @JpegData, JpegSize, Subsamp, ClampInt(Params.Quality, 1, 100),
                       Flags);
    if (Res <> 0) and ((tjGetErrorCode(TJHandle) <> TJERR_WARNING) or (JpegData = nil)) then
      RaiseTurboError(TJHandle);

    // TurboJPEG can't set the density so it's written to JFIF header
    // (the first thing in the file) directly
    Header := PBuffer(JpegData);

    if (Params.DensityUnit > 0) and (JpegSize >= JfifHeaderSize) and
       (Header[2] = $FF) and (Header[3] = $E0) and (Header[6] = Ord('J')) and
       (Header[7] = Ord('F')) and (Header[8] = Ord('I')) and (Header[9] = Ord('F')) then
    begin
      Header[13] := Params.DensityUnit;
      Header[14] := (Params.XDensity shr 8) and $FF;
      Header[15] := Params.XDensity and $FF;
      Header[16] := (Params.YDensity shr 8) and $FF;
      Header[17] := Params.YDensity and $FF;
    end;

    Result := IO.Write(Handle, JpegData, JpegSize) = TIOReadWriteCount(JpegSize);
  finally
    if JpegData <> nil then
      tjFree(JpegData);
    tjDestroy(TJHandle);
  end;
end;

{$ENDIF}

function JpegBackend: TJpegBackend;
begin
  Result := jbPascal;
{$IFDEF JPEG_USE_TURBO_DYNLIB}
  // The library is searched for only by the first call
  if LoadLibTurboJpeg then
    Result := jbTurboJpeg;
{$ENDIF}
end;

function TryLoadTurboJpegLibrary(const FileName: string): Boolean;
begin
{$IFDEF JPEG_USE_TURBO_DYNLIB}
  Result := LoadLibTurboJpegFromFile(FileName);
{$ELSE}
  Result := False;
{$ENDIF}
end;

function JpegLoadImage(const IO: TIOFunctions; Handle: TImagingHandle;
  var Image: TImageData; out Info: TJpegImageInfo): Boolean;
begin
  Result := JpegLoadImage(IO, Handle, Image, Info, jscAuto);
end;

function JpegLoadImage(const IO: TIOFunctions; Handle: TImagingHandle;
  var Image: TImageData; out Info: TJpegImageInfo;
  SourceColorSpace: TJpegSourceColorSpace): Boolean;
begin
{$IFDEF JPEG_USE_TURBO_DYNLIB}
  if JpegBackend = jbTurboJpeg then
  begin
    Result := TurboLoadImage(IO, Handle, Image, Info, SourceColorSpace);
    Exit;
  end;
{$ENDIF}
  Result := PasLoadImage(IO, Handle, Image, Info, SourceColorSpace);
end;

function JpegSaveImage(const IO: TIOFunctions; Handle: TImagingHandle;
  const Image: TImageData; const Params: TJpegSaveParams): Boolean;
begin
{$IFDEF JPEG_USE_TURBO_DYNLIB}
  if JpegBackend = jbTurboJpeg then
  begin
    Result := TurboSaveImage(IO, Handle, Image, Params);
    Exit;
  end;
{$ENDIF}
  Result := PasSaveImage(IO, Handle, Image, Params);
end;

end.
