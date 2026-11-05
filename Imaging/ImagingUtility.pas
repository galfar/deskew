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

{ This unit contains utility functions and types for Imaging library.}
unit ImagingUtility;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, Types;

const
  STrue = 'True';
  SFalse = 'False';

type
{$IF Defined(DELPHI)}
  {$IF not Defined(UInt32)}
    UInt32 = Cardinal;
  {$IFEND}
  {$IF not Defined(PUInt32)}
    PUInt32 = ^UInt32;
  {$IFEND}
  {$IF CompilerVersion <= 18.5}
    { In Delphi 7-2007 NativeInt is incorrectly defined to have size of 8 bytes,
      redeclare it correctly (as ImagingTypes does).}
    NativeInt = Integer;
  {$IFEND}
{$IFEND}

  { For indexing big memory blocks (like TImageData.Bits) rather use PBuffer
    and other utility types declared bellow.
    PByteArray and others are capped at ~2GB (even in Delphi 64, not limited in FPC.}
  TUInt32Array = array[0..MaxInt div 4 - 1] of UInt32;
  PUInt32Array = ^TUInt32Array;
  TInt64Array = array[0..MaxInt div 8 - 1] of Int64;
  PInt64Array = ^TInt64Array;
  TSingleArray = array[0..MaxInt div 4 - 1] of Single;
  PSingleArray = ^TSingleArray;
  TBooleanArray = array[0..MaxInt - 1] of Boolean;
  PBooleanArray = ^TBooleanArray;

  TDynByteArray = array of Byte;
  TDynIntegerArray = array of Integer;
  TDynBooleanArray = array of Boolean;
  TDynStringArray = array of string;

  TWordRec = packed record
    case Integer of
      0: (WordValue: Word);
      1: (Low, High: Byte);
  end;
  PWordRec = ^TWordRec;
  TWordRecArray = array[0..MaxInt div 2 - 1] of TWordRec;
  PWordRecArray = ^TWordRecArray;

  TUInt32Rec = packed record
    case Integer of
      0: (UInt32Value: UInt32);
      1: (Low, High: Word);
      { Array variants - Index 0 means lowest significant byte (word, ...).}
      2: (Words: array[0..1] of Word);
      3: (Bytes: array[0..3] of Byte);
  end;
  PUInt32Rec = ^TUInt32Rec;
  TUInt32RecArray = array[0..MaxInt div 4 - 1] of TUInt32Rec;
  PUInt32RecArray = ^TUInt32RecArray;

  TInt64Rec = packed record
    case Integer of
      0: (Int64Value: Int64);
      1: (Low, High: UInt32);
      { Array variants - Index 0 means lowest significant byte (word, ...).}
      2: (Words: array[0..3] of Word);
      3: (Bytes: array[0..7] of Byte);
  end;
  PInt64Rec = ^TInt64Rec;
  TInt64RecArray = array[0..MaxInt div 8 - 1] of TInt64Rec;
  PInt64RecArray = ^TInt64RecArray;

{$IFDEF HAS_POINTER_MATH}
  PUInt32Buffer = PUInt32;
  PInt64Buffer = PInt64;
  PSingleBuffer = PSingle;
{$ELSE}
  // Delphi 2007 and earlier - no POINTERMATH support
  PUInt32Buffer = PUInt32Array;
  PInt64Buffer = PInt64Array;
  PSingleBuffer = PSingleArray;
{$ENDIF}

  TFloatHelper = record
    Data: Int64;
    case Integer of
      0: (Data64: Int64);
      1: (Data32: UInt32);
  end;
  PFloatHelper = ^TFloatHelper;

  TFloatPoint = record
    X, Y: Single;
  end;

  TFloatRect = record
    Left, Top, Right, Bottom: Single;
  end;

  TChar2 = array[0..1] of AnsiChar;
  TChar3 = array[0..2] of AnsiChar;
  TChar4 = array[0..3] of AnsiChar;
  TChar8 = array[0..7] of AnsiChar;
  TChar16 = array[0..15] of AnsiChar;
  TAnsiCharSet = set of AnsiChar;

  ENotImplemented = class(Exception)
  public
    constructor Create;
  end;

  { Options for BuildFileList function:
    flFullNames - file names in result will have full path names
                (ExtractFileDir(Path) + FileName)
    flRelNames  - file names in result will have names relative to
                ExtractFileDir(Path) dir
    flRecursive - adds files in subdirectories found in Path.}
  TFileListOption = (flFullNames, flRelNames, flRecursive);
  TFileListOptions = set of TFileListOption;


{ Frees class instance and sets its reference to nil.}
procedure FreeAndNil(var Obj);
{ Frees pointer and sets it to nil.}
procedure FreeMemNil(var P); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Replacement of standard System.FreeMem procedure which checks if P is nil
  (this is only needed for Free Pascal, Delphi makes checks in its FreeMem).}
procedure FreeMem(P: Pointer); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns current exception object. Do not call outside exception handler.}
function GetExceptObject: Exception; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns time value with microsecond resolution.}
function GetTimeMicroseconds: Int64;
{ Returns time value with millisecond resolution.}
function GetTimeMilliseconds: Int64;

{ Returns file extension (without "." dot)}
function GetFileExt(const FileName: string): string;
{ Returns file name of application's executable.}
function GetAppExe: string;
{ Returns directory where application's executable is located without
  path delimiter at the end.}
function GetAppDir: string;
{ Works like SysUtils.ExtractFileName but supports '/' and '\' dir delimiters
  at the same time (whereas ExtractFileName supports on default delimiter on current platform).}
function GetFileName(const FileName: string): string;
{ Works like SysUtils.ExtractFileDir but supports '/' and '\' dir delimiters
  at the same time (whereas ExtractFileDir supports on default delimiter on current platform).}
function GetFileDir(const FileName: string): string;
{ Returns True if Subject matches given Mask with optional case sensitivity.
  Mask can contain ? and * special characters: ? matches
  one character, * matches zero or more characters.}
function StrMaskMatch(const Subject, Mask: string; CaseSensitive: Boolean = False): Boolean;
{ This function fills Files string list with names of files found
  with FindFirst/FindNext functions (See details on Path/Atrr here).
  - BuildFileList('c:\*.*', faAnyFile, List, [flRecursive]) returns
    list of all files (only name.ext - no path) on C drive
  - BuildFileList('d:\*.*', faDirectory, List, [flFullNames]) returns
    list of all directories (d:\dirxxx) in root of D drive.}
function BuildFileList(Path: string; Attr: LongInt; Files: TStrings;
  Options: TFileListOptions = []): Boolean;
{ Similar to RTL's Pos function but with optional Offset where search will start.
  In recent FPC and Delphi XE3+ regular SysUtils.Pos has the Offset parameter as well.
  This function is in the RTL StrUtils unit, it's here to depend on additional
  unit for just this one function. }
function PosEx(const SubStr, S: string; Offset: LongInt = 1): LongInt;
{ Same as PosEx but without case sensitivity.}
function PosNoCase(const SubStr, S: string; Offset: LongInt = 1): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns a sub-string from S which is followed by
  Sep separator and deletes the sub-string from S including the separator.}
function StrToken(var S: string; Sep: Char): string;
{ Same as StrToken but searches from the end of S string.}
function StrTokenEnd(var S: string; Sep: Char): string;
{ Fills instance of TStrings with tokens from string S where tokens are separated by
  one of Seps characters.}
procedure StrTokensToList(const S: string; Sep: Char; Tokens: TStrings);
{ Returns string representation of integer number (with digit grouping).
  Uses current locale.}
function IntToStrFmt(const I: Int64): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns string representation of float number (with digit grouping).
  Uses current locale.}
function FloatToStrFmt(const F: Double; Precision: Integer = 2): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns format settings for parsing floats (dot as decimal separator).
  Useful when formatting/parsing floats etc.}
function GetFormatSettingsForFloats: TFormatSettings;
{ Returns True if S contains at least one of the substrings in SubStrs array. Case sensitive.}
function ContainsAnySubStr(const S: string; const SubStrs: array of string): Boolean;
{ Extracts substring starting at IdxStart ending at IdxEnd.
  S[IdxEnd] is not included in the result.}
function SubString(const S: string; IdxStart, IdxEnd: Integer): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Similar to Trim() but removes only characters in a given set.
  Part of FPC RTL here for Delphi compatibility. }
function TrimSet(const S: string; const CharSet: TSysCharSet): string;
{ Similar to TrimLeft() but removes only characters in a given set.
  Part of FPC RTL here for Delphi compatibility. }
function TrimLeftSet(const S: string; const CharSet:TSysCharSet): string;

{ Clamps integer value to range <Min, Max>}
function ClampInt(Number: LongInt; Min, Max: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Clamps float value to range <Min, Max>}
function ClampFloat(Number: Single; Min, Max: Single): Single; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Clamps integer value to Byte boundaries.}
function ClampToByte(Value: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Clamps integer value to Word boundaries.}
function ClampToWord(Value: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns True if Num is power of 2.}
function IsPow2(Num: LongInt): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns next power of 2 greater than or equal to Num
  (if Num itself is power of 2 then it returns Num).}
function NextPow2(Num: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Raises 2 to the given integer power (in range [0, 30]).}
function Pow2Int(Exponent: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Raises Base to any power.}
function Power(const Base, Exponent: Single): Single;
{ Returns log base 2 of integer X (max 2^30) or -1 if X is not power of 2.}
function Log2Int(X: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns log base 2 of X.}
function Log2(X: Single): Single;
{ Returns log base 10 of X.}
function Log10(X: Single): Single;
{ Returns largest integer <= Val (for 5.9 returns 5).}
function Floor(Value: Single): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns smallest integer >= Val (for 5.1 returns 6).}
function Ceil(Value: Single): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns lesser of two integer numbers.}
function Min(A, B: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns lesser of two float numbers.}
function MinFloat(A, B: Single): Single; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns greater of two integer numbers.}
function Max(A, B: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns greater of two float numbers.}
function MaxFloat(A, B: Single): Single; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns greater of two float numbers.}
function MaxFloat(const A, B: Double): Double; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns true if give floats are the equal within given delta.}
function SameFloat(A, B: Single; Delta: Single = 0.001): Boolean; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns true if give floats are the equal within given delta.}
function SameFloat(const A, B: Double; const Delta: Double = 0.000001): Boolean; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Returns result from multiplying Number by Numerator and then dividing by Denominator.
  Denominator must be greater than 0.
  Mind the limits, this is not the MulDiv of Windows API:
  - all parameters and the result are 16-bit, larger values are cut without
    any error when passed in,
  - the result is truncated, not rounded to the nearest value,
  - the result must fit into 16 bits and Number * Numerator must be less
    than 2^31, otherwise the result is wrong or an exception is raised
    (depends on the compiler and its overflow checking).
  So it is fine for things like scaling of 8-bit values by 8-bit factor.
  For scaling of values between two ranges (channels of different bit depths,
  samples of unusual precision) use ScaleValue. Mind the different order
  of parameters:
    MulDiv(Value, DstMax, SrcMax)  ->  ScaleValue(Value, SrcMax, DstMax)
  for instance 12-bit sample to 16 bits:
    MulDiv(Sample, 65535, 4095)    ->  ScaleValue(Sample, 4095, 65535)}
function MulDiv(Number, Numerator, Denominator: Word): Word; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Scales Value from range 0..SrcMax to range 0..DstMax and rounds the result
  to the nearest value. Zero stays zero and SrcMax becomes DstMax, so the
  largest value is kept when bit depth of samples is changed: for instance
  ScaleValue(V, 255, 65535) for 8 to 16 bits, ScaleValue(V, 4095, 255)
  for 12 to 8 bits. Values larger than SrcMax give DstMax.
  SrcMax must be greater than 0. Whole 32-bit range can be used for all
  parameters; when Value * DstMax does not fit into 63 bits the result is
  computed in floating point and can be off by one for values in the middle
  of the range.}
function ScaleValue(Value, SrcMax, DstMax: UInt32): UInt32;

{ Switches Boolean value.}
procedure Switch(var Value: Boolean); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function Iff(Condition: Boolean; TruePart, FalsePart: Integer): Integer; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function IffUnsigned(Condition: Boolean; TruePart, FalsePart: Cardinal): Cardinal; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function Iff(Condition, TruePart, FalsePart: Boolean): Boolean; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function Iff(Condition: Boolean; const TruePart, FalsePart: string): string; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function Iff(Condition: Boolean; TruePart, FalsePart: Char): Char; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function Iff(Condition: Boolean; TruePart, FalsePart: Pointer): Pointer; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function Iff(Condition: Boolean; const TruePart, FalsePart: Int64): Int64; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function IffFloat(Condition: Boolean; TruePart, FalsePart: Single): Single; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Swaps two Boolean values}
procedure SwapValues(var A, B: Boolean); overload;
{ Swaps two Byte values}
procedure SwapValues(var A, B: Byte); overload;
{ Swaps two Word values}
procedure SwapValues(var A, B: Word); overload;
{ Swaps two Integer values}
procedure SwapValues(var A, B: Integer); overload;
{$IFDEF LONGINT_IS_NOT_INTEGER}
{ Swaps two LongInt values}
procedure SwapValues(var A, B: LongInt); overload;
{$ENDIF}
{ Swaps two Single values}
procedure SwapValues(var A, B: Single); overload;
{ Swaps two values if necessary to ensure that Min <= Max.}
procedure SwapMin(var Min, Max: LongInt); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ This function returns True if running on little endian machine.}
function IsLittleEndian: Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Swaps byte order of Word value.}
function SwapEndianWord(Value: Word): Word; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Swaps byte order of multiple Word values.}
procedure SwapEndianWord(P: PWord; Count: NativeInt); overload;
{ Swaps byte order of UInt32 value.}
function SwapEndianUInt32(Value: UInt32): UInt32; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Swaps byte order of multiple UInt32 values.}
procedure SwapEndianUInt32(P: PUInt32; Count: NativeInt); overload;

{ Calculates CRC32 for the given data.}
procedure CalcCrc32(var Crc: UInt32; Data: Pointer; Size: NativeInt);
{ Fills given memory with given Byte value. Size is size of buffer in bytes.}
procedure FillMemoryByte(Data: Pointer; Size: NativeInt; Value: Byte);
{ Fills given memory with given Word value. Size is size of buffer in bytes.}
procedure FillMemoryWord(Data: Pointer; Size: NativeInt; Value: Word);
{ Fills given memory with given UInt32 value. Size is size of buffer in bytes.}
procedure FillMemoryUInt32(Data: Pointer; Size: NativeInt; Value: UInt32);
{ Fills given memory zeroes.}
{$EXTERNALSYM ZeroMemory} // Conflicts with WinAPI ZeroMemory in C++ Builder
procedure ZeroMemory(Data: Pointer; Size: NativeInt); {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Returns how many mipmap levels can be created for image of given size.}
function GetNumMipMapLevels(Width, Height: LongInt): LongInt;
{ Returns total number of levels of volume texture with given depth and
  mipmap count (this is not depth * mipmaps!).}
function GetVolumeLevelCount(Depth, MipMaps: LongInt): LongInt;

{ Returns rectangle (X, Y, X + Width, Y + Height).}
function BoundsToRect(X, Y, Width, Height: LongInt): TRect; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns rectangle (R.Left, R.Top, R.Left + R.Right, R.Top + R.Bottom).}
function BoundsToRect(const R: TRect): TRect; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns rectangle (R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top).}
function RectToBounds(const R: TRect): TRect; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns width of given rect. Part of RTL in newer Delphi.}
function RectWidth(const Rect: TRect): Integer;
{ Returns height of given rect. Part of RTL in newer Delphi.}
function RectHeight(const Rect: TRect): Integer;
{ Returns True if R1 fits into R2.}
function RectInRect(const R1, R2: TRect): Boolean;
{ Returns True if R1 and R2 intersects.}
function RectIntersects(const R1, R2: TRect): Boolean;
{ Ensures that rect's right>left and bottom>top. }
procedure NormalizeRect(var R: TRect);

{ Clips given bounds to Clip rectangle.}
procedure ClipRectBounds(var X, Y, Width, Height: LongInt; const Clip: TRect);
{ Clips given source bounds and dest position. It is used by various CopyRect
  functions that copy rect from one image to another. It handles clipping the same way
  as Win32 BitBlt function. }
procedure ClipCopyBounds(var SrcX, SrcY, Width, Height, DstX, DstY: LongInt;
  SrcImageWidth, SrcImageHeight: LongInt; const DstClip: TRect);
{ Clips stretching of SrcRect of an image (SrcImageWidth x SrcImageHeight) to
  DstRect, DstClip is e.g. bounds of the destination image or clip
  rectangle of a canvas. It keeps the mapping of the original rectangles:
  drawing of the clipped part gives the same pixels as the
  same part of unclipped drawing. DrawnRect gets the destination pixels to draw:
  those inside DstRect and DstClip whose centers map into the image.
  SrcValidRect gets the part of SrcRect inside the image, pixels outside of it
  must not be read (samplers clamp to it). Source positions of destination
  pixels are still computed from SrcRect and DstRect. Returns False when there
  is nothing to draw (also for empty or inverted rectangles).}
function ClipStretchRects(const SrcRect, DstRect: TRect; SrcImageWidth, SrcImageHeight: Integer;
  const DstClip: TRect; out DrawnRect, SrcValidRect: TRect): Boolean;
{ Scales one rectangle to fit into another. Proportions are preserved so
  it could be used for 'Stretch To Fit Window' image drawing for instance.}
function ScaleRectToRect(const SourceRect, TargetRect: TRect): TRect;
{ Scales given size to fit into max size while keeping the original aspect ratio.
  Useful for calculating thumbnail dimensions etc.}
function ScaleSizeToFit(const CurrentSize, MaxSize: TSize): TSize;

{ Converts pixel size in micrometers to corresponding DPI.}
function PixelSizeToDpi(SizeInMicroMeters: Single): Single;
{ Converts DPI to corresponding pixel size in micrometers.}
function DpiToPixelSize(Dpi: Single): Single;

function FloatPoint(AX, AY: Single): TFloatPoint; {$IFDEF USE_INLINE}inline;{$ENDIF}
function FloatRect(ALeft, ATop, ARight, ABottom: Single): TFloatRect;
function FloatRectWidth(const R: TFloatRect): Single;
function FloatRectHeight(const R: TFloatRect): Single;
function FloatRectFromRect(const R: TRect): TFloatRect;

{ Formats given message for usage in Exception.Create(..). Use only
  in except block - returned message contains message of last raised exception.}
function FormatExceptMsg(const Msg: string; const Args: array of const): string;
{ Outputs debug message - shows message dialog in Windows and writes to console
  in Linux/Unix.}
procedure DebugMsg(const Msg: string; const Args: array of const);

implementation

{$IF Defined(MSWINDOWS)}
uses
   Windows;
{$ELSEIF Defined(FPC) and Defined(UNIX)}
uses
   Dos, BaseUnix, Unix;
{$ELSEIF Defined(DELPHI)}
uses
   Posix.SysTime;
{$IFEND}

var
  FloatFormatSettings: TFormatSettings;

constructor ENotImplemented.Create;
begin
  inherited Create('Not implemented');
end;

procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

procedure FreeMemNil(var P);
begin
  FreeMem(Pointer(P));
  Pointer(P) := nil;
end;

procedure FreeMem(P: Pointer);
begin
  if P <> nil then
    System.FreeMem(P);
end;

function GetExceptObject: Exception;
begin
  Result := Exception(ExceptObject);
end;

{$IF Defined(MSWINDOWS)}
var
  PerfFrequency: Int64;
  InvPerfFrequency: Extended;

function GetTimeMicroseconds: Int64;
var
  Time: Int64;
begin
  QueryPerformanceCounter(Time);
  Result := Round(1000000 * InvPerfFrequency * Time);
end;
{$ELSEIF Defined(DELPHI)}
function GetTimeMicroseconds: Int64;
var
  Time: TimeVal;
begin
  Posix.SysTime.GetTimeOfDay(Time, nil);
  Result := Int64(Time.tv_sec) * 1000000 + Time.tv_usec;
end;
{$ELSEIF Defined(FPC) and Defined(UNIX)}
function GetTimeMicroseconds: Int64;
var
  TimeVal: TTimeVal;
begin
  fpGetTimeOfDay(@TimeVal, nil);
  Result := Int64(TimeVal.tv_sec) * 1000000 + TimeVal.tv_usec;
end;
{$ELSE}
// Other FPC targets (e.g. WebAssembly), millisecond resolution only
function GetTimeMicroseconds: Int64;
begin
  Result := Int64(GetTickCount64) * 1000;
end;
{$IFEND}

function GetTimeMilliseconds: Int64;
begin
  Result := GetTimeMicroseconds div 1000;
end;

function GetFileExt(const FileName: string): string;
begin
  Result := ExtractFileExt(FileName);
  if Length(Result) > 1 then
    Delete(Result, 1, 1);
end;

function GetAppExe: string;
{$IF Defined(MSWINDOWS)}
var
  FileName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, FileName,
    Windows.GetModuleFileName(MainInstance, FileName, SizeOf(FileName)));
{$ELSEIF Defined(DELPHI)} // Delphi non Win targets
var
  FileName: array[0..1024] of Char;
begin
  SetString(Result, FileName,
    System.GetModuleFileName(MainInstance, FileName, SizeOf(FileName)));
{$ELSE}
begin
  Result := ExpandFileName(ParamStr(0));
{$IFEND}
end;

function GetAppDir: string;
begin
  Result := ExtractFileDir(GetAppExe);
end;

function GetFileName(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('\/' + DriveDelim, FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function GetFileDir(const FileName: string): string;
const
  Delims = '\/' + DriveDelim;
var
  I: Integer;
begin
  I := LastDelimiter(Delims, Filename);
  if (I > 1) and
    ((FileName[I] = Delims[1]) or (FileName[I] = Delims[2])) and
    (not IsDelimiter(Delims, FileName, I - 1)) then Dec(I);
  Result := Copy(FileName, 1, I);
end;

function StrMaskMatch(const Subject, Mask: string; CaseSensitive: Boolean): Boolean;
var
  MaskLen, KeyLen : LongInt;

  function CharMatch(A, B: Char): Boolean;
  begin
    if CaseSensitive then
      Result := A = B
    else
      Result := AnsiUpperCase (A) = AnsiUpperCase (B);
  end;

  function MatchAt(MaskPos, KeyPos: LongInt): Boolean;
  begin
    while (MaskPos <= MaskLen) and (KeyPos <= KeyLen) do
    begin
      case Mask[MaskPos] of
        '?' :
          begin
            Inc(MaskPos);
            Inc(KeyPos);
          end;
        '*' :
          begin
            while (MaskPos <= MaskLen) and (Mask[MaskPos] = '*') do
              Inc(MaskPos);
            if MaskPos > MaskLen then
            begin
              Result := True;
              Exit;
            end;
            repeat
              if MatchAt(MaskPos, KeyPos) then
              begin
                Result := True;
                Exit;
              end;
              Inc(KeyPos);
            until KeyPos > KeyLen;
            Result := False;
            Exit;
          end;
        else
          if not CharMatch(Mask[MaskPos], Subject[KeyPos]) then
          begin
            Result := False;
            Exit;
          end
          else
          begin
            Inc(MaskPos);
            Inc(KeyPos);
          end;
      end;
    end;

    while (MaskPos <= MaskLen) and (AnsiChar(Mask[MaskPos]) in ['?', '*']) do
      Inc(MaskPos);
    if (MaskPos <= MaskLen) or (KeyPos <= KeyLen) then
    begin
      Result := False;
      Exit;
    end;

    Result := True;
  end;

begin
  MaskLen := Length(Mask);
  KeyLen := Length(Subject);
  if MaskLen = 0 then
  begin
    Result := True;
    Exit;
  end;
  Result := MatchAt(1, 1);
end;

function BuildFileList(Path: string; Attr: LongInt;
  Files: TStrings; Options: TFileListOptions): Boolean;
var
  FileMask: string;
  RootDir: string;
  Folders: TStringList;
  CurrentItem: LongInt;
  Counter: LongInt;
  LocAttr: LongInt;

  procedure BuildFolderList;
  var
    FindInfo: TSearchRec;
    Rslt: LongInt;
  begin
    Counter := Folders.Count - 1;
    CurrentItem := 0;
    while CurrentItem <= Counter do
    begin
      // Searching for subfolders
      Rslt := SysUtils.FindFirst(Folders[CurrentItem] + '*', faDirectory, FindInfo);
      try
        while Rslt = 0 do
        begin
          if (FindInfo.Name <> '.') and (FindInfo.Name <> '..') and
            (FindInfo.Attr and faDirectory = faDirectory) then
            Folders.Add(Folders[CurrentItem] + FindInfo.Name + PathDelim);
          Rslt := SysUtils.FindNext(FindInfo);
        end;
      finally
        SysUtils.FindClose(FindInfo);
      end;
      Counter := Folders.Count - 1;
      Inc(CurrentItem);
    end;
  end;

  procedure FillFileList(CurrentCounter: LongInt);
  var
    FindInfo: TSearchRec;
    Res: LongInt;
    CurrentFolder: string;
  begin
    CurrentFolder := Folders[CurrentCounter];
    Res := SysUtils.FindFirst(CurrentFolder + FileMask, LocAttr, FindInfo);
    if flRelNames in Options then
      CurrentFolder := ExtractRelativePath(RootDir, CurrentFolder);
    try
      while Res = 0 do
      begin
        if (FindInfo.Name <> '.') and (FindInfo.Name <> '..') then
        begin
          if (flFullNames in Options) or (flRelNames in Options) then
            Files.Add(CurrentFolder + FindInfo.Name)
          else
            Files.Add(FindInfo.Name);
        end;
        Res := SysUtils.FindNext(FindInfo);
      end;
    finally
      SysUtils.FindClose(FindInfo);
    end;
  end;

begin
  FileMask := ExtractFileName(Path);
  RootDir := ExtractFilePath(Path);
  Folders := TStringList.Create;
  Folders.Add(RootDir);
  Files.Clear;
{$IFDEF DCC}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
  if Attr = faAnyFile then
    LocAttr := faSysFile or faHidden or faArchive or faReadOnly
  else
    LocAttr := Attr;
{$IFDEF DCC}
  {$WARN SYMBOL_PLATFORM ON}
{$ENDIF}
  // Here's the recursive search for nested folders
  if flRecursive in Options then
    BuildFolderList;
  if Attr <> faDirectory then
    for Counter := 0 to Folders.Count - 1 do
      FillFileList(Counter)
  else
    Files.AddStrings(Folders);
  Folders.Free;
  Result := True;
end;

function PosEx(const SubStr, S: string; Offset: LongInt = 1): LongInt;
var
  I, X: LongInt;
  Len, LenSubStr: LongInt;
begin
  I := Offset;
  LenSubStr := Length(SubStr);
  Len := Length(S) - LenSubStr + 1;
  while I <= Len do
  begin
    if S[I] = SubStr[1] then
    begin
      X := 1;
      while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
        Inc(X);
      if (X = LenSubStr) then
      begin
        Result := I;
        Exit;
      end;
    end;
    Inc(I);
  end;
  Result := 0;
end;

function PosNoCase(const SubStr, S: string; Offset: LongInt): LongInt;
begin
  Result := PosEx(AnsiLowerCase(SubStr), AnsiLowerCase(S), Offset);
end;

function StrToken(var S: string; Sep: Char): string;
var
  I: LongInt;
begin
  I := Pos(Sep, S);
  if I <> 0 then
  begin
    Result := Copy(S, 1, I - 1);
    Delete(S, 1, I);
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

function StrTokenEnd(var S: string; Sep: Char): string;
var
  I, J: LongInt;
begin
  J := 0;
  I := Pos(Sep, S);
  while I <> 0 do
  begin
    J := I;
    I := PosEx(Sep, S, J + 1);
  end;
  if J <> 0 then
  begin
    Result := Copy(S, J + 1, MaxInt);
    Delete(S, J, MaxInt);
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

procedure StrTokensToList(const S: string; Sep: Char; Tokens: TStrings);
var
  Token, Str: string;
begin
  Tokens.Clear;
  Str := S;
  while Str <> '' do
  begin
    Token := StrToken(Str, Sep);
    Tokens.Add(Token);
  end;
end;

function IntToStrFmt(const I: Int64): string;
begin
  Result := Format('%.0n', [I * 1.0]);
end;

function FloatToStrFmt(const F: Double; Precision: Integer): string;
begin
  Result := Format('%.' + IntToStr(Precision) + 'n', [F]);
end;

function GetFormatSettingsForFloats: TFormatSettings;
begin
  Result := FloatFormatSettings;
end;

function ContainsAnySubStr(const S: string; const SubStrs: array of string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(SubStrs) do
  begin
    Result := Pos(SubStrs[I], S) > 0;
    if Result then
      Exit;
  end;
end;

function SubString(const S: string; IdxStart, IdxEnd: Integer): string;
begin
  Result := Copy(S, IdxStart, IdxEnd - IdxStart);
end;

{$IF Defined(DCC) and (CompilerVersion < 20.0)}
{ CharInSet is in SysUtils only since Delphi 2009.}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$IFEND}

function TrimSet(const S: string; const CharSet: TSysCharSet): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and CharInSet(S[I], CharSet) do
    Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while CharInSet(S[L], CharSet) do
      Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function TrimLeftSet(const S: string; const CharSet: TSysCharSet): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and CharInSet(S[I], CharSet) do
    Inc(I);
  Result := Copy(S, I, MaxInt);
end;

function ClampInt(Number: LongInt; Min, Max: LongInt): LongInt;
begin
  Result := Number;
  if Result < Min then
    Result := Min
  else if Result > Max then
    Result := Max;
end;

function ClampFloat(Number: Single; Min, Max: Single): Single;
begin
  Result := Number;
  if Result < Min then
    Result := Min
  else if Result > Max then
    Result := Max;
end;

function ClampToByte(Value: LongInt): LongInt;
begin
  Result := Value;
  if Result > 255 then
    Result := 255
  else if Result < 0 then
    Result := 0;
end;

function ClampToWord(Value: LongInt): LongInt;
begin
  Result := Value;
  if Result > 65535 then
    Result := 65535
  else if Result < 0 then
    Result := 0;
end;

function IsPow2(Num: LongInt): Boolean;
begin
  Result := (Num and -Num) = Num;
end;

function NextPow2(Num: LongInt): LongInt;
begin
  Result := Num and -Num;
  while Result < Num do
    Result := Result shl 1;
end;

function Pow2Int(Exponent: LongInt): LongInt;
begin
  Result := 1 shl Exponent;
end;

function Power(const Base, Exponent: Single): Single;
begin
  if Exponent = 0.0 then
    Result := 1.0
  else if (Base = 0.0) and (Exponent > 0.0) then
    Result := 0.0
  else
    Result := Exp(Exponent * Ln(Base));
end;

function Log2Int(X: LongInt): LongInt;
begin
  case X of
    1: Result := 0;
    2: Result := 1;
    4: Result := 2;
    8: Result := 3;
    16: Result := 4;
    32: Result := 5;
    64: Result := 6;
    128: Result := 7;
    256: Result := 8;
    512: Result := 9;
    1024: Result := 10;
    2048: Result := 11;
    4096: Result := 12;
    8192: Result := 13;
    16384: Result := 14;
    32768: Result := 15;
    65536: Result := 16;
    131072: Result := 17;
    262144: Result := 18;
    524288: Result := 19;
    1048576: Result := 20;
    2097152: Result := 21;
    4194304: Result := 22;
    8388608: Result := 23;
    16777216: Result := 24;
    33554432: Result := 25;
    67108864: Result := 26;
    134217728: Result := 27;
    268435456: Result := 28;
    536870912: Result := 29;
    1073741824: Result := 30;
  else
    Result := -1;
  end;
end;

function Log2(X: Single): Single;
{$IFDEF USE_ASM}
asm
  FLD1
  FLD     X
  FYL2X
  FWAIT
end;
{$ELSE}
const
  Ln2: Single = 0.6931471;
begin
  Result := Ln(X) / Ln2;
end;
{$ENDIF}

function Log10(X: Single): Single;
{$IFDEF USE_ASM}
asm
  FLDLG2
  FLD     X
  FYL2X
  FWAIT
end;
{$ELSE}
const
  Ln10: Single = 2.30258509299405;
begin
  Result := Ln(X) / Ln10;
end;
{$ENDIF}

function Floor(Value: Single): LongInt;
begin
  Result := Trunc(Value);
  if Value < Result then
    Dec(Result);
end;

function Ceil(Value: Single): LongInt;
begin
  Result := Trunc(Value);
  if Value > Result then
    Inc(Result);
end;

procedure Switch(var Value: Boolean);
begin
  Value := not Value;
end;

function Iff(Condition: Boolean; TruePart, FalsePart: Integer): Integer;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function IffUnsigned(Condition: Boolean; TruePart, FalsePart: Cardinal): Cardinal;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(Condition, TruePart, FalsePart: Boolean): Boolean;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(Condition: Boolean; const TruePart, FalsePart: string): string;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(Condition: Boolean; TruePart, FalsePart: Char): Char;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(Condition: Boolean; TruePart, FalsePart: Pointer): Pointer;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(Condition: Boolean; const TruePart, FalsePart: Int64): Int64;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function IffFloat(Condition: Boolean; TruePart, FalsePart: Single): Single;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

procedure SwapValues(var A, B: Boolean);
var
  Tmp: Boolean;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure SwapValues(var A, B: Byte);
var
  Tmp: Byte;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure SwapValues(var A, B: Word);
var
  Tmp: Word;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure SwapValues(var A, B: Integer);
var
  Tmp: Integer;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

{$IFDEF LONGINT_IS_NOT_INTEGER}
procedure SwapValues(var A, B: LongInt);
var
  Tmp: LongInt;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;
{$ENDIF}

procedure SwapValues(var A, B: Single);
var
  Tmp: Single;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure SwapMin(var Min, Max: LongInt);
var
  Tmp: LongInt;
begin
  if Min > Max then
  begin
    Tmp := Min;
    Min := Max;
    Max := Tmp;
  end;
end;

function Min(A, B: LongInt): LongInt;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function MinFloat(A, B: Single): Single;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(A, B: LongInt): LongInt;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function MaxFloat(A, B: Single): Single;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function MaxFloat(const A, B: Double): Double;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function SameFloat(A, B: Single; Delta: Single): Boolean;
begin
  Result := Abs(A - B) <= Delta;
end;

function SameFloat(const A, B: Double; const Delta: Double): Boolean;
begin
  Result := Abs(A - B) <= Delta;
end;

function MulDiv(Number, Numerator, Denominator: Word): Word;
{$IF Defined(USE_ASM) and (not Defined(USE_INLINE))}
asm
         MUL DX
         DIV CX
end;
{$ELSE}
begin
  Result := Number * Numerator div Denominator;
end;
{$IFEND}

function ScaleValue(Value, SrcMax, DstMax: UInt32): UInt32;
const
  // Typed constant makes the expression below double precision. FPC gives
  // floating point literals the smallest type that holds them exactly and
  // computes expressions of integers and Single values in single precision
  // on targets without x87 FPU (like x86_64), which would lose bits here.
  // This is intended behavior of FPC and it is not going to change
  // (FPC issues 36415 and 37935). It can be switched off by -CF64 option or
  // $MINFPCONSTPREC 64 directive, but the library must not depend on
  // compiler options of its users: typed Double constant or a cast to Double
  // gives double precision with any settings.
  One: Double = 1.0;
begin
  Assert(SrcMax > 0);

  if Value >= SrcMax then
    Result := DstMax
  else if (Value <= $7FFFFFFF) and (DstMax <= $7FFFFFFF) then
    Result := (Int64(Value) * DstMax + SrcMax shr 1) div SrcMax
  else
    Result := Round(One * Value / SrcMax * DstMax);
end;

function IsLittleEndian: Boolean;
var
  W: Word;
begin
  W := $00FF;
  Result := PByte(@W)^ = $FF;
end;

function SwapEndianWord(Value: Word): Word;
{$IF Defined(USE_ASM) and (not Defined(USE_INLINE))}
asm
  XCHG   AH, AL
end;
{$ELSE}
begin
  TWordRec(Result).Low := TWordRec(Value).High;
  TWordRec(Result).High := TWordRec(Value).Low;
end;
{$IFEND}

// System.PWord: Windows unit used in implementation has its own PWord type
procedure SwapEndianWord(P: System.PWord; Count: NativeInt);
{$IFDEF USE_ASM}
asm
@Loop:
  MOV    CX, [EAX]
  XCHG   CH, CL
  MOV    [EAX], CX
  ADD    EAX, 2
  DEC    EDX
  JNZ    @Loop
end;
{$ELSE}
var
  I: NativeInt;
  Temp: Word;
begin
  for I := 0 to Count - 1 do
  begin
    Temp := P^;
    TWordRec(P^).Low := TWordRec(Temp).High;
    TWordRec(P^).High := TWordRec(Temp).Low;
    Inc(P);
  end;
end;
{$ENDIF}

function SwapEndianUInt32(Value: UInt32): UInt32;
{$IF Defined(USE_ASM) and (not Defined(USE_INLINE))}
asm
  BSWAP   EAX
end;
{$ELSE}
begin
  TUInt32Rec(Result).Bytes[0] := TUInt32Rec(Value).Bytes[3];
  TUInt32Rec(Result).Bytes[1] := TUInt32Rec(Value).Bytes[2];
  TUInt32Rec(Result).Bytes[2] := TUInt32Rec(Value).Bytes[1];
  TUInt32Rec(Result).Bytes[3] := TUInt32Rec(Value).Bytes[0];
end;
{$IFEND}

procedure SwapEndianUInt32(P: PUInt32; Count: NativeInt);
{$IFDEF USE_ASM}
asm
@Loop:
  MOV    ECX, [EAX]
  BSWAP  ECX
  MOV    [EAX], ECX
  ADD    EAX, 4
  DEC    EDX
  JNZ    @Loop
end;
{$ELSE}
var
  I: NativeInt;
  Temp: UInt32;
begin
  for I := 0 to Count - 1 do
  begin
    Temp := P^;
    TUInt32Rec(P^).Bytes[0] := TUInt32Rec(Temp).Bytes[3];
    TUInt32Rec(P^).Bytes[1] := TUInt32Rec(Temp).Bytes[2];
    TUInt32Rec(P^).Bytes[2] := TUInt32Rec(Temp).Bytes[1];
    TUInt32Rec(P^).Bytes[3] := TUInt32Rec(Temp).Bytes[0];
    Inc(P);
  end;
end;
{$ENDIF}

var
  { Tables for CRC computed 8 bytes at once (slicing-by-8): the first one
    is the usual byte table, table K gives CRC of a byte followed by K zero bytes.}
  CrcTables: array[0..7, Byte] of UInt32;

procedure InitCrcTable;
const
  Polynom = $EDB88320;
var
  I, J: LongInt;
  C: UInt32;
begin
  for I := 0 to 255 do
  begin
    C := I;
    for J := 0 to 7 do
    begin
      if (C and $01) <> 0 then
        C := Polynom xor (C shr 1)
      else
        C := C shr 1;
    end;
    CrcTables[0, I] := C;
  end;

  for I := 0 to 255 do
  begin
    for J := 1 to 7 do
    begin
      C := CrcTables[J - 1, I];
      CrcTables[J, I] := (C shr 8) xor CrcTables[0, C and $FF];
    end;
  end;
end;

procedure CalcCrc32(var Crc: UInt32; Data: Pointer; Size: NativeInt);
var
  C, Low, High: UInt32;
  B: PByte;
begin
  // CRC is kept in a local variable, the var parameter would be read and
  // written in memory in every step by some compilers
  C := Crc;
  B := Data;

  // Bytes up to the first 4-byte aligned address one by one
  while (Size > 0) and ({$IFDEF FPC}PtrUInt{$ELSE}NativeInt{$ENDIF}(B) and 3 <> 0) do
  begin
    C := (C shr 8) xor CrcTables[0, B^ xor Byte(C)];
    Inc(B);
    Dec(Size);
  end;

  // Then 8 bytes at once
  while Size >= 8 do
  begin
    Low := PUInt32(B)^ xor C;
    Inc(B, 4);
    High := PUInt32(B)^;
    Inc(B, 4);
    C := CrcTables[7, Low and $FF] xor CrcTables[6, (Low shr 8) and $FF] xor
         CrcTables[5, (Low shr 16) and $FF] xor CrcTables[4, Low shr 24] xor
         CrcTables[3, High and $FF] xor CrcTables[2, (High shr 8) and $FF] xor
         CrcTables[1, (High shr 16) and $FF] xor CrcTables[0, High shr 24];
    Dec(Size, 8);
  end;

  // The rest one by one
  while Size > 0 do
  begin
    C := (C shr 8) xor CrcTables[0, B^ xor Byte(C)];
    Inc(B);
    Dec(Size);
  end;

  Crc := C;
end;

procedure FillMemoryByte(Data: Pointer; Size: NativeInt; Value: Byte);
{$IFDEF USE_ASM}
asm
  PUSH   EDI
  MOV    EDI, EAX
  MOV    EAX, ECX
  MOV    AH, AL
  MOV    CX, AX
  SHL    EAX, 16
  MOV    AX, CX
  MOV    ECX, EDX
  SAR    ECX, 2
  JS     @Exit
  REP    STOSD
  MOV    ECX, EDX
  AND    ECX, 3
  REP    STOSB
  POP    EDI
@Exit:
end;
{$ELSE}
begin
  FillChar(Data^, Size, Value);
end;
{$ENDIF}

procedure FillMemoryWord(Data: Pointer; Size: NativeInt; Value: Word);
{$IFDEF USE_ASM}
asm
  PUSH   EDI
  PUSH   EBX
  MOV    EBX, EDX
  MOV    EDI, EAX
  MOV    EAX, ECX
  MOV    CX, AX
  SHL    EAX, 16
  MOV    AX, CX
  MOV    ECX, EDX
  SHR    ECX, 2
  JZ     @Word
  REP    STOSD
@Word:
  MOV    ECX, EBX
  AND    ECX, 2
  JZ     @Byte
  MOV    [EDI], AX
  ADD    EDI, 2
@Byte:
  MOV    ECX, EBX
  AND    ECX, 1
  JZ     @Exit
  MOV    [EDI], AL
@Exit:
  POP    EBX
  POP    EDI
end;
{$ELSE}
var
  I: NativeInt;
  V: UInt32;
  Item: PUInt32;
begin
  V := UInt32(Value) shl 16 or Value;
  Item := Data;

  for I := 0 to Size div 4 - 1 do
  begin
    Item^ := V;
    Inc(Item);
  end;

  // The rest that does not make whole four bytes, Item points to it
  case Size mod 4 of
    1: PByteArray(Item)[0] := Lo(Value);
    2: PWord(Item)^ := Value;
    3:
      begin
        PWord(Item)^ := Value;
        PByteArray(Item)[2] := Lo(Value);
      end;
  end;
end;
{$ENDIF}

procedure FillMemoryUInt32(Data: Pointer; Size: NativeInt; Value: UInt32);
{$IFDEF USE_ASM}
asm
  PUSH   EDI
  PUSH   EBX
  MOV    EBX, EDX
  MOV    EDI, EAX
  MOV    EAX, ECX
  MOV    ECX, EDX
  SHR    ECX, 2
  JZ     @Word
  REP    STOSD
@Word:
  MOV    ECX, EBX
  AND    ECX, 2
  JZ     @Byte
  MOV    [EDI], AX
  ADD    EDI, 2
@Byte:
  MOV    ECX, EBX
  AND    ECX, 1
  JZ     @Exit
  MOV    [EDI], AL
@Exit:
  POP    EBX
  POP    EDI
end;
{$ELSE}
var
  I: NativeInt;
  Item: PUInt32;
begin
  Item := Data;

  for I := 0 to Size div 4 - 1 do
  begin
    Item^ := Value;
    Inc(Item);
  end;

  // The rest that does not make whole four bytes, Item points to it
  case Size mod 4 of
    1: PByteArray(Item)[0] := TUInt32Rec(Value).Bytes[0];
    2: PWord(Item)^ := TUInt32Rec(Value).Words[0];
    3:
      begin
        PWord(Item)^ := TUInt32Rec(Value).Words[0];
        PByteArray(Item)[2] := TUInt32Rec(Value).Bytes[0];
      end;
  end;
end;
{$ENDIF}

procedure ZeroMemory(Data: Pointer; Size: NativeInt);
begin
  FillMemoryByte(Data, Size, 0);
end;

function GetNumMipMapLevels(Width, Height: LongInt): LongInt;
begin
  Result := 0;
  if (Width > 0) and (Height > 0) then
  begin
    Result := 1;
    while (Width <> 1) or (Height <> 1) do
    begin
      Width := Width div 2;
      Height := Height div 2;
      if Width < 1 then Width := 1;
      if Height < 1 then Height := 1;
      Inc(Result);
    end;
  end;
end;

function GetVolumeLevelCount(Depth, MipMaps: LongInt): LongInt;
var
  I: LongInt;
begin
  Result := Depth;
  for I := 1 to MipMaps - 1 do
    Inc(Result, ClampInt(Depth shr I, 1, Depth));
end;

function BoundsToRect(X, Y, Width, Height: LongInt): TRect;
begin
  Result.Left := X;
  Result.Top := Y;
  Result.Right := X + Width;
  Result.Bottom := Y + Height;
end;

function BoundsToRect(const R: TRect): TRect;
begin
  Result.Left := R.Left;
  Result.Top := R.Top;
  Result.Right := R.Left + R.Right;
  Result.Bottom := R.Top + R.Bottom;
end;

function RectToBounds(const R: TRect): TRect;
begin
  Result.Left := R.Left;
  Result.Top := R.Top;
  Result.Right := R.Right - R.Left;
  Result.Bottom := R.Bottom - R.Top;
end;

function RectWidth(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function RectHeight(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function RectInRect(const R1, R2: TRect): Boolean;
begin
  Result:=
    (R1.Left >= R2.Left) and
    (R1.Top >= R2.Top) and
    (R1.Right <= R2.Right) and
    (R1.Bottom <= R2.Bottom);
end;

function RectIntersects(const R1, R2: TRect): Boolean;
begin
  Result :=
    not (R1.Left > R2.Right) and
    not (R1.Top > R2.Bottom) and
    not (R1.Right < R2.Left) and
    not (R1.Bottom < R2.Top);
end;

procedure NormalizeRect(var R: TRect);
begin
  if R.Right < R.Left then
    SwapValues(R.Right, R.Left);
  if R.Bottom < R.Top then
    SwapValues(R.Bottom, R.Top);
end;

procedure ClipRectBounds(var X, Y, Width, Height: LongInt; const Clip: TRect);

  procedure ClipDim(var AStart, ALength: LongInt; ClipMin, ClipMax: LongInt);
  begin
    if AStart < ClipMin then
    begin
      ALength := ALength - (ClipMin - AStart);
      AStart := ClipMin;
    end;
    if AStart + ALength > ClipMax then ALength := Max(0, ClipMax - AStart);
  end;

begin
  ClipDim(X, Width, Clip.Left, Clip.Right);
  ClipDim(Y, Height, Clip.Top, Clip.Bottom);
end;

procedure ClipCopyBounds(var SrcX, SrcY, Width, Height, DstX, DstY: LongInt; SrcImageWidth, SrcImageHeight: LongInt; const DstClip: TRect);

  procedure ClipDim(var SrcPos, DstPos, Size: LongInt; SrcClipMax,
    DstClipMin, DstClipMax: LongInt);
  var
    OldDstPos: LongInt;
    Diff: LongInt;
  begin
    OldDstPos := Iff(DstPos < 0, DstPos, 0);
    if DstPos < DstClipMin then
    begin
      Diff := DstClipMin - DstPos;
      Size := Size - Diff;
      SrcPos := SrcPos + Diff;
      DstPos := DstClipMin;
    end;
    if SrcPos < 0 then
    begin
      Size := Size + SrcPos - OldDstPos;
      DstPos := DstPos - SrcPos + OldDstPos;
      SrcPos := 0;
    end;
    if SrcPos + Size > SrcClipMax then Size := SrcClipMax - SrcPos;
    if DstPos + Size > DstClipMax then Size := DstClipMax - DstPos;
  end;

begin
  ClipDim(SrcX, DstX, Width, SrcImageWidth, DstClip.Left, DstClip.Right);
  ClipDim(SrcY, DstY, Height, SrcImageHeight, DstClip.Top, DstClip.Bottom);
end;

function ClipStretchRects(const SrcRect, DstRect: TRect; SrcImageWidth, SrcImageHeight: Integer;
  const DstClip: TRect; out DrawnRect, SrcValidRect: TRect): Boolean;

  { Returns the first destination position (relative to the start of destination)
    whose pixel center maps to source position SrcPos (relative to the start of
    source) or after it: the smallest D with (D + 0.5) * SrcSize / DstSize >= SrcPos,
    that is D >= (2 * SrcPos * DstSize - SrcSize) / (2 * SrcSize). Computed with
    integers so it is exact.}
  function FirstDstPos(SrcPos, SrcSize, DstSize: Integer): Integer;
  var
    Num, Den: Int64;
  begin
    Num := 2 * Int64(SrcPos) * DstSize - SrcSize;
    Den := 2 * Int64(SrcSize);
    // Ceiling of the division for positive Den
    if Num >= 0 then
      Result := (Num + Den - 1) div Den
    else
      Result := -((-Num) div Den);
  end;

  function ClipDim(SrcStart, SrcEnd, DstStart, DstEnd, SrcImageSize, ClipMin, ClipMax: Integer;
    out DrawnStart, DrawnEnd, ValidStart, ValidEnd: Integer): Boolean;
  var
    SrcSize, DstSize: Integer;
  begin
    SrcSize := SrcEnd - SrcStart;
    DstSize := DstEnd - DstStart;
    ValidStart := Max(SrcStart, 0);
    ValidEnd := Min(SrcEnd, SrcImageSize);
    DrawnStart := DstStart;
    DrawnEnd := DstStart;
    Result := (SrcSize > 0) and (DstSize > 0) and (ValidStart < ValidEnd);
    if not Result then
      Exit;

    DrawnStart := Max(Max(DstStart, ClipMin),
                      DstStart + FirstDstPos(ValidStart - SrcStart, SrcSize, DstSize));
    DrawnEnd := Min(Min(DstEnd, ClipMax),
                    DstStart + FirstDstPos(ValidEnd - SrcStart, SrcSize, DstSize));
    Result := DrawnStart < DrawnEnd;
  end;

var
  ResultX, ResultY: Boolean;
begin
  ResultX := ClipDim(SrcRect.Left, SrcRect.Right, DstRect.Left, DstRect.Right, SrcImageWidth,
                     DstClip.Left, DstClip.Right, DrawnRect.Left, DrawnRect.Right,
                     SrcValidRect.Left, SrcValidRect.Right);
  ResultY := ClipDim(SrcRect.Top, SrcRect.Bottom, DstRect.Top, DstRect.Bottom, SrcImageHeight,
                     DstClip.Top, DstClip.Bottom, DrawnRect.Top, DrawnRect.Bottom,
                     SrcValidRect.Top, SrcValidRect.Bottom);
  Result := ResultX and ResultY;
end;

function ScaleRectToRect(const SourceRect, TargetRect: TRect): TRect;
var
  SourceWidth: LongInt;
  SourceHeight: LongInt;
  TargetWidth: LongInt;
  TargetHeight: LongInt;
  ScaledWidth: LongInt;
  ScaledHeight: LongInt;
begin
  SourceWidth := SourceRect.Right - SourceRect.Left;
  SourceHeight := SourceRect.Bottom - SourceRect.Top;
  TargetWidth := TargetRect.Right - TargetRect.Left;
  TargetHeight := TargetRect.Bottom - TargetRect.Top;

  // Products of two dimensions are computed in 64 bits
  if Int64(SourceWidth) * TargetHeight < Int64(SourceHeight) * TargetWidth then
  begin
    ScaledWidth := (Int64(SourceWidth) * TargetHeight) div SourceHeight;
    Result := BoundsToRect(TargetRect.Left + ((TargetWidth - ScaledWidth) div 2),
      TargetRect.Top, ScaledWidth, TargetHeight);
  end
  else
  begin
    ScaledHeight := (Int64(SourceHeight) * TargetWidth) div SourceWidth;
    Result := BoundsToRect(TargetRect.Left, TargetRect.Top + ((TargetHeight - ScaledHeight) div 2),
      TargetWidth, ScaledHeight);
  end;
end;

function ScaleSizeToFit(const CurrentSize, MaxSize: Types.TSize): Types.TSize;
var
  SR, TR, ScaledRect: TRect;
begin
  SR := Types.Rect(0, 0, CurrentSize.CX, CurrentSize.CY);
  TR := Types.Rect(0, 0, MaxSize.CX, MaxSize.CY);
  ScaledRect := ScaleRectToRect(SR, TR);
  Result.CX := ScaledRect.Right - ScaledRect.Left;
  Result.CY := ScaledRect.Bottom - ScaledRect.Top;
end;

function PixelSizeToDpi(SizeInMicroMeters: Single): Single;
begin
  Result := 25400 / SizeInMicroMeters;
end;

function DpiToPixelSize(Dpi: Single): Single;
begin
  Result := 1e03 / (Dpi / 25.4);
end;

function FloatPoint(AX, AY: Single): TFloatPoint;
begin
  Result.X := AX;
  Result.Y := AY;
end;

function FloatRect(ALeft, ATop, ARight, ABottom: Single): TFloatRect;
begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
  end;
end;

function FloatRectWidth(const R: TFloatRect): Single;
begin
  Result := R.Right - R.Left;
end;

function FloatRectHeight(const R: TFloatRect): Single;
begin
  Result := R.Bottom - R.Top;
end;

function FloatRectFromRect(const R: TRect): TFloatRect;
begin
  Result := FloatRect(R.Left, R.Top, R.Right, R.Bottom);
end;

function FormatExceptMsg(const Msg: string; const Args: array of const): string;
begin
  Result := Format(Msg + SLineBreak + 'Message: ' + GetExceptObject.Message, Args);
end;

procedure DebugMsg(const Msg: string; const Args: array of const);
var
  FmtMsg: string;
begin
  FmtMsg := Format(Msg, Args);
{$IFDEF MSWINDOWS}
  if IsConsole then
    WriteLn('DebugMsg: ' + FmtMsg)
  else
    MessageBox(GetActiveWindow, PChar(FmtMsg), 'DebugMsg', MB_OK);
{$ENDIF}
{$IFDEF UNIX}
  WriteLn('DebugMsg: ' + FmtMsg);
{$ENDIF}
{$IFDEF MSDOS}
  WriteLn('DebugMsg: ' + FmtMsg);
{$ENDIF}
end;

initialization
  InitCrcTable;
{$IFDEF MSWINDOWS}
  QueryPerformanceFrequency(PerfFrequency);
  InvPerfFrequency := 1.0 / PerfFrequency;
{$ENDIF}

{$IF Defined(DELPHI)}
  {$IF CompilerVersion >= 23}
  FloatFormatSettings := TFormatSettings.Create('en-US');
  {$ELSE}
  GetLocaleFormatSettings(1033, FloatFormatSettings);
  {$IFEND}
{$ELSE FPC}
  FloatFormatSettings := DefaultFormatSettings;
  FloatFormatSettings.DecimalSeparator := '.';
  FloatFormatSettings.ThousandSeparator := ',';
{$IFEND}

{
  File Notes:

  -- 0.77.1 ----------------------------------------------------
    - Added GetFileName, GetFileDir, RectWidth, RectHeight function.
    - Added ScaleSizeToFit function.
    - Added ZeroMemory and SwapValues for Booleans.
    - Added Substring function.
    - Renamed MatchFileNameMask to StrMaskMatch (it's for general use not
      just filenames).
    - Delphi XE2 new targets (Win64, OSX32) compatibility changes.
    - Added GetFormatSettingsForFloats function.

  -- 0.26.5 Changes/Bug Fixes -----------------------------------
    - Added Log10 function.
    - Added TFloatRect type and helper functions FloatRect, FloatRectWidth,
      FloatRectHeight.
    - Added string function ContainsAnySubStr.
    - Added functions PixelSizeToDpi, DpiToPixelSize.

  -- 0.26.1 Changes/Bug Fixes -----------------------------------
    - Some formatting changes.
    - Changed some string functions to work with localized strings.
    - ASM version of PosEx had bugs, removed it.
    - Added StrTokensToList function.

  -- 0.25.0 Changes/Bug Fixes -----------------------------------
    - Fixed error in ClipCopyBounds which was causing ... bad clipping!

  -- 0.24.3 Changes/Bug Fixes -----------------------------------
    - Added GetTimeMilliseconds function.
    - Added IntToStrFmt and FloatToStrFmt helper functions.

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Added RectInRect and RectIntersects functions
    - Added some string utils: StrToken, StrTokenEnd, PosEx, PosNoCase.
    - Moved BuildFileList here from DemoUtils.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Moved GetVolumeLevelCount from ImagingDds here.
    - Renamed FillMemory to FillMemoryByte to avoid name collision in C++ Builder.
    - Added Iff function for Char, Pointer, and Int64 types.
    - Added IsLittleEndian function.
    - Added array types for TWordRec, TLongWordRec, and TInt64Rec.
    - Added MatchFileNameMask function.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - added ScaleRectToRect (thanks to Paul Michell)
    - added BoundsToRect, ClipBounds, ClipCopyBounds, ClipStretchBounds functions
    - added MulDiv function
    - FreeAndNil is not inline anymore - caused AV in one program

  -- 0.17 Changes/Bug Fixes -----------------------------------

    - GetAppExe didn't return absolute path in FreeBSD, fixed
    - added debug message output
    - fixed Unix compatibility issues (thanks to Ales Katona).
      Imaging now compiles in FreeBSD and maybe in other Unixes as well.

  -- 0.15 Changes/Bug Fixes -----------------------------------
    - added some new utility functions

  -- 0.13 Changes/Bug Fixes -----------------------------------
    - added many new utility functions
    - minor change in SwapEndian to avoid range check error

}
end.


