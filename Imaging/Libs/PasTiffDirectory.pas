{
  PasTiff - pure Pascal TIFF reader/writer
  by Marek Mauder
  https://github.com/galfar/imaginglib
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
}

{ TIFF container level: file header, image file directories (IFDs) and their
  tags, for classic TIFF and BigTIFF, in both byte orders.

  This unit knows nothing about pixels or compression. It is all that is needed
  to read or write any TIFF-structured data, including EXIF blocks (which are
  small TIFF files without image data, see PasTiffExif.pas).

  Structure of a TIFF file (TIFF 6.0 section 2, BigTIFF in parentheses):

    Header   2 bytes  byte order, "II" little-endian or "MM" big-endian
             2 bytes  magic 42 (43)
            (2 bytes  size of offsets, always 8; 2 bytes zero)
             4 (8)    offset of the first IFD
    IFD      2 (8)    number of entries N
             N * 12 (20) bytes of entries, sorted by tag
             4 (8)    offset of the next IFD, 0 for the last one
    Entry    2 bytes  tag
             2 bytes  data type
             4 (8)    number of values
             4 (8)    the values themselves if they fit, left justified,
                      otherwise offset of the values

  All offsets are from the start of the TIFF header, which for an embedded
  EXIF block is not the start of the stream. TTiffContainer handles that.

  Reference: libtiff's tif_dirread.c, tif_dirwrite.c and tif_open.c. Unlike
  libtiff there is no table of known tags: a tag is just a number, a type and
  an array of values, and callers ask for it with the type they expect. }
unit PasTiffDirectory;

{$I PasTiff.inc}

interface

uses
  SysUtils, Classes, PasTiffTypes;

const
  { An IFD with more entries than this is rejected. Same limit as in libtiff
    ("Sanity check on directory count failed, this is probably not a valid IFD
    offset"): a random offset into pixel data would otherwise be taken for a
    directory with up to 65535 (or 2^64) entries. }
  TiffMaxDirEntries = 4096;
  { Largest value array of one tag PasTiff loads, the same 2 GiB - 1 limit as
    libtiff's MAX_SIZE_TAG_DATA. }
  TiffMaxTagDataSize = 2147483647;

type
  TTiffByteOrder = (tboLittleEndian, tboBigEndian);

  TTiffContainer = class;

  { One IFD entry. Data holds Count values of DataType in host byte order
    once the tag is loaded; use the Get* methods to read them. }
  PTiffTag = ^TTiffTag;
  TTiffTag = record
    Tag: Word;
    DataType: Word;     // tt* constant
    Count: Int64;       // number of values, not bytes
    Data: TBytes;
    { False for a value array stored outside the IFD that was not read yet.
      Tags obtained from TTiffDirectory.Find are always loaded. }
    Loaded: Boolean;
    { Where the not yet loaded values are in the container. }
    ValueOffset: Int64;
    { Size of all values in bytes. }
    function DataSize: Int64;
    { Value as integer. Accepts every numeric type: files often use another
      integer type than the specification asks for (LONG instead of SHORT and
      the like) and libtiff reads them all. Rationals and floats are truncated.
      Raises ETiffFormatError if Index is out of range or the value does not
      fit Int64. }
    function GetInt(Index: Int64 = 0): Int64;
    { Value as floating point number, for every numeric type.
      A rational with zero denominator gives 0, as in libtiff. So do FLOAT
      and DOUBLE values that are infinite, NaN or absurdly large, so that
      callers can compute with the result without floating point exceptions. }
    function GetFloat(Index: Int64 = 0): Double;
    { Text of ASCII (and UTF-8, BYTE, UNDEFINED) tag up to the first NUL.
      A missing terminating NUL is tolerated. }
    function GetText: AnsiString;
  end;

  { Image file directory: set of tags sorted by tag number.

    A directory read from a file loads large value arrays lazily, on first
    access, so it needs its container (and the stream) to stay alive. Pointers
    returned by Find and TagAt are valid until the directory is modified. }
  TTiffDirectory = class
  private
    FTags: array of TTiffTag;
    FCount: Integer;
    FContainer: TTiffContainer;
    FOffset: Int64;
    FNextOffset: Int64;
    function IndexOf(Tag: Word; out Index: Integer): Boolean;
    function AddTag(Tag, DataType: Word; Count: Int64): PTiffTag;
    procedure LoadTag(var T: TTiffTag);
  public
    constructor Create(AContainer: TTiffContainer = nil);

    { Returns loaded tag or nil if the directory does not have it. }
    function Find(Tag: Word): PTiffTag;
    function HasTag(Tag: Word): Boolean;
    { Access to all tags in ascending order; TagAt does not load the values. }
    property TagCount: Integer read FCount;
    function TagAt(Index: Integer): PTiffTag;
    { Loads values of all tags, after which the container is no longer needed.
      Tags whose values cannot be read (bad offset, too large) are removed.
      MaxTotalSize limits memory taken by a hostile file. }
    procedure LoadAll(MaxTotalSize: Int64 = 64 * 1024 * 1024);

    { Getters. "Try" variants return False if the tag is missing or has fewer
      values than Index + 1; the others return Default in that case. }
    function TryGetInt(Tag: Word; out Value: Int64; Index: Int64 = 0): Boolean;
    function GetInt(Tag: Word; Default: Int64): Int64;
    function GetIntArray(Tag: Word): TTiffInt64Array;
    function TryGetFloat(Tag: Word; out Value: Double; Index: Int64 = 0): Boolean;
    function GetFloat(Tag: Word; const Default: Double): Double;
    function GetFloatArray(Tag: Word): TTiffDoubleArray;
    function GetText(Tag: Word): AnsiString;
    { Raw values in host byte order, empty array if the tag is missing. }
    function GetBytes(Tag: Word): TBytes;

    { Setters replace an existing tag. Integer values must fit DataType. }
    procedure SetInt(Tag, DataType: Word; Value: Int64);
    procedure SetIntArray(Tag, DataType: Word; const Values: array of Int64);
    { Stores Value as unsigned RATIONAL. }
    procedure SetRational(Tag: Word; const Value: Double);
    { Stores text with terminating NUL. }
    procedure SetText(Tag: Word; const Value: AnsiString; DataType: Word = ttAscii);
    { Stores Count values of DataType given in host byte order. }
    procedure SetBytes(Tag, DataType: Word; Data: Pointer; Count: Int64);
    procedure Remove(Tag: Word);
    { Copies tag with its values from another directory, replacing an
      existing one. Returns False if Source does not have the tag. }
    function CopyTagFrom(Source: TTiffDirectory; Tag: Word): Boolean;

    { Offset this directory was read from or written to. }
    property Offset: Int64 read FOffset;
    { Offset of the next directory in the chain, 0 if this is the last one. }
    property NextOffset: Int64 read FNextOffset;
  end;

  { TIFF structure inside a stream: header, raw data blocks and directories.
    Created either for reading (ReadHeader) or for writing (WriteHeader).
    The stream position at the time of creation is offset 0 of the TIFF. }
  TTiffContainer = class
  private
    FStream: TStream;
    FBase: Int64;
    FSize: Int64;
    FByteOrder: TTiffByteOrder;
    FBigTiff: Boolean;
    FSwap: Boolean;
    FFirstDirOffset: Int64;
    procedure StreamRead(Buffer: Pointer; Count: NativeInt);
    procedure StreamWrite(Buffer: Pointer; Count: NativeInt);
    function GetU16(P: PByte): Word;
    function GetU32(P: PByte): Cardinal;
    function GetOffset(P: PByte; out Value: Int64): Boolean;
    procedure PutU16(P: PByte; Value: Word);
    procedure PutU32(P: PByte; Value: Cardinal);
    procedure PutU64(P: PByte; Value: Int64);
  public
    { ASize is the size of TIFF data in the stream from the current position;
      -1 means up to the end of the stream. }
    constructor Create(AStream: TStream; ASize: Int64 = -1);

    { Reading }

    { Reads and checks the header. Raises ETiffFormatError if it is not TIFF. }
    procedure ReadHeader;
    { Reads Count bytes at Offset. Raises ETiffFormatError when the range is
      not inside the TIFF data: every offset taken from a file ends up here,
      so a damaged or hostile file cannot make us read (or allocate for)
      anything that is not there. }
    procedure ReadAt(Offset: Int64; Buffer: Pointer; Count: NativeInt);
    { Reads directory at Offset. Works for any directory: pages from the main
      chain as well as sub-directories referenced by tags (Exif, GPS, SubIFD).
      Caller owns the result. }
    function ReadDirectory(Offset: Int64): TTiffDirectory;

    { Returns offset of the directory following the one at Offset (0 if it
      is the last) without reading its tags. Used to walk the chain of pages.
      Raises ETiffFormatError if there is no valid directory at Offset. }
    function ReadDirectoryLink(Offset: Int64): Int64;

    { Writing }

    { Starts a new little-endian TIFF: writes the header with empty link to
      the first directory. }
    procedure WriteHeader(ABigTiff: Boolean);
    { Writes data block at the end and returns its offset. Blocks start at
      even offsets as TIFF requires. Raises ETiffError when the offset cannot
      be represented in classic TIFF (4 GiB limit). }
    function Append(Data: Pointer; Count: NativeInt): Int64;
    { Writes directory with all its values at the end and returns its offset.
      NextLinkOffset is where the offset of the following directory can be
      stored later with WriteOffsetAt. }
    function WriteDirectory(Dir: TTiffDirectory; out NextLinkOffset: Int64): Int64;
    { Overwrites an offset field (4 or 8 bytes) at Position. }
    procedure WriteOffsetAt(Position, Value: Int64);

    property Stream: TStream read FStream;
    property ByteOrder: TTiffByteOrder read FByteOrder;
    property IsBigTiff: Boolean read FBigTiff;
    { True if multi-byte values in the file have other byte order than the CPU. }
    property NeedsSwap: Boolean read FSwap;
    { Size of TIFF data: fixed when reading, grows when writing. }
    property Size: Int64 read FSize;
    property FirstDirOffset: Int64 read FFirstDirOffset;
    { Position of the header field holding FirstDirOffset. }
    function FirstDirLinkOffset: Int64;
    { Size of offset fields: 4 for classic TIFF, 8 for BigTIFF. }
    function OffsetSize: Integer;
  end;

implementation

const
  { Stream reads and writes are split to blocks of this size because
    TStream.Read/Write take 32-bit counts. }
  IOBlockSize = 1 shl 30;

  SNotTiff = 'Not a TIFF file';
  SBeyondEnd = 'TIFF data at offset %d (%d bytes) lies beyond the end of the file';
  STagIndex = 'TIFF tag %d has no value with index %d';
  STagRange = 'Value %d does not fit data type %d of TIFF tag %d';

{ TTiffTag }

function TTiffTag.DataSize: Int64;
begin
  Result := Count * TiffTypeSize(DataType);
end;

function TTiffTag.GetInt(Index: Int64): Int64;
var
  P: PByte;
  F: Double;
begin
  if (Index < 0) or (Index >= Count) or not Loaded then
    raise ETiffFormatError.CreateFmt(STagIndex, [Tag, Index]);
  P := @Data[Index * TiffTypeSize(DataType)];
  case DataType of
    ttByte, ttUndefined, ttAscii, ttUtf8: Result := P^;
    ttSByte: Result := PShortInt(P)^;
    ttShort: Result := PWord(P)^;
    ttSShort: Result := PSmallInt(P)^;
    ttLong, ttIFD: Result := PCardinal(P)^;
    ttSLong: Result := PInteger(P)^;
    ttLong8, ttIFD8:
      begin
        Result := PInt64(P)^;
        // Unsigned 64-bit values above High(Int64) cannot be real sizes or offsets
        if Result < 0 then
          raise ETiffFormatError.CreateFmt('Value of TIFF tag %d is out of range', [Tag]);
      end;
    ttSLong8: Result := PInt64(P)^;
  else
    begin
      F := GetFloat(Index);
      if (F < -9.2e18) or (F > 9.2e18) then
        raise ETiffFormatError.CreateFmt('Value of TIFF tag %d is out of range', [Tag]);
      Result := Trunc(F);
    end;
  end;
end;

function TTiffTag.GetFloat(Index: Int64): Double;
var
  P: PByte;
begin
  if (Index < 0) or (Index >= Count) or not Loaded then
    raise ETiffFormatError.CreateFmt(STagIndex, [Tag, Index]);
  P := @Data[Index * TiffTypeSize(DataType)];
  case DataType of
    ttRational:
      // libtiff (TIFFReadDirEntryCheckedRational): zero denominator reads as 0
      if PCardinal(P)[1] = 0 then
        Result := 0
      else
        Result := PCardinal(P)[0] / PCardinal(P)[1];
    ttSRational:
      if PInteger(P)[1] = 0 then
        Result := 0
      else
        Result := PInteger(P)[0] / PInteger(P)[1];
    ttFloat:
      // Checked on the bits, via an exact conversion to double
      if (PCardinal(P)^ and $7F800000) = $7F800000 then
        Result := 0
      else
        Result := PSingle(P)^;
    ttDouble: Result := TiffSafeDouble(P);
  else
    Result := GetInt(Index);
  end;
end;

function TTiffTag.GetText: AnsiString;
var
  Len: Integer;
begin
  Result := '';
  if not Loaded or (TiffTypeSize(DataType) <> 1) then
    Exit;
  Len := 0;
  // The value should end with NUL but not all writers add it; libtiff forces
  // the terminator, we simply stop at the end of data.
  while (Len < Count) and (Data[Len] <> 0) do
    Inc(Len);
  SetString(Result, PAnsiChar(@Data[0]), Len);
end;

{ TTiffDirectory }

constructor TTiffDirectory.Create(AContainer: TTiffContainer);
begin
  inherited Create;
  FContainer := AContainer;
end;

{ Binary search. Index is the position of the tag or where it would be inserted. }
function TTiffDirectory.IndexOf(Tag: Word; out Index: Integer): Boolean;
var
  L, H, M: Integer;
begin
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    M := (L + H) shr 1;
    if FTags[M].Tag < Tag then
      L := M + 1
    else if FTags[M].Tag > Tag then
      H := M - 1
    else
    begin
      Index := M;
      Result := True;
      Exit;
    end;
  end;
  Index := L;
  Result := False;
end;

{ Returns tag with zeroed Data of the right size, replacing an existing one. }
function TTiffDirectory.AddTag(Tag, DataType: Word; Count: Int64): PTiffTag;
var
  Index, I, Size: Integer;
begin
  Size := TiffTypeSize(DataType);
  if Size = 0 then
    raise ETiffError.CreateFmt('Unknown data type %d for TIFF tag %d', [DataType, Tag]);
  if (Count < 0) or (Count > TiffMaxTagDataSize div Size) then
    raise ETiffError.CreateFmt('Too many values for TIFF tag %d', [Tag]);

  if not IndexOf(Tag, Index) then
  begin
    if FCount = Length(FTags) then
      SetLength(FTags, FCount + 16);
    for I := FCount - 1 downto Index do
      FTags[I + 1] := FTags[I];
    Inc(FCount);
  end;
  Result := @FTags[Index];
  Result.Tag := Tag;
  Result.DataType := DataType;
  Result.Count := Count;
  Result.Data := nil;
  SetLength(Result.Data, Count * Size);
  Result.Loaded := True;
  Result.ValueOffset := 0;
end;

procedure TTiffDirectory.LoadTag(var T: TTiffTag);
var
  Size: Int64;
begin
  if T.Loaded then
    Exit;
  Size := T.DataSize;
  if Size > TiffMaxTagDataSize then
    raise ETiffFormatError.CreateFmt('Values of TIFF tag %d are too large (%d bytes)', [T.Tag, Size]);
  if FContainer = nil then
    raise ETiffError.CreateFmt('Values of TIFF tag %d are not available', [T.Tag]);
  // The values must really be in the file - checked before memory is
  // allocated for them: a tag can claim up to 2 GiB of values in a file of
  // a few bytes. (Found by the robustness tests under 32-bit Delphi, where
  // such an allocation simply fails.)
  if (T.ValueOffset < 0) or (T.ValueOffset > FContainer.Size) or
     (Size > FContainer.Size - T.ValueOffset) then
    raise ETiffFormatError.CreateFmt(
      'TIFF data at offset %d (%d bytes) lies beyond the end of the file', [T.ValueOffset, Size]);
  SetLength(T.Data, Size);
  FContainer.ReadAt(T.ValueOffset, @T.Data[0], Size);
  if FContainer.NeedsSwap then
    TiffSwapBytes(@T.Data[0], TiffTypeSwapUnit(T.DataType), Size div TiffTypeSwapUnit(T.DataType));
  T.Loaded := True;
end;

function TTiffDirectory.Find(Tag: Word): PTiffTag;
var
  Index: Integer;
begin
  if IndexOf(Tag, Index) then
  begin
    Result := @FTags[Index];
    LoadTag(Result^);
  end
  else
    Result := nil;
end;

function TTiffDirectory.HasTag(Tag: Word): Boolean;
var
  Index: Integer;
begin
  Result := IndexOf(Tag, Index);
end;

function TTiffDirectory.TagAt(Index: Integer): PTiffTag;
begin
  if (Index < 0) or (Index >= FCount) then
    raise ETiffError.CreateFmt('TIFF tag index %d is out of range', [Index]);
  Result := @FTags[Index];
end;

procedure TTiffDirectory.LoadAll(MaxTotalSize: Int64);
var
  I: Integer;
  Total: Int64;
  Ok: Boolean;
begin
  Total := 0;
  I := 0;
  while I < FCount do
  begin
    Ok := True;
    if not FTags[I].Loaded then
    begin
      Inc(Total, FTags[I].DataSize);
      Ok := Total <= MaxTotalSize;
      if Ok then
      try
        LoadTag(FTags[I]);
      except
        on ETiffFormatError do
          Ok := False;
      end;
    end;
    if Ok then
      Inc(I)
    else
      Remove(FTags[I].Tag);
  end;
  FContainer := nil;
end;

function TTiffDirectory.TryGetInt(Tag: Word; out Value: Int64; Index: Int64): Boolean;
var
  T: PTiffTag;
begin
  T := Find(Tag);
  Result := (T <> nil) and (Index >= 0) and (Index < T.Count);
  if Result then
    Value := T.GetInt(Index)
  else
    Value := 0;
end;

function TTiffDirectory.GetInt(Tag: Word; Default: Int64): Int64;
begin
  if not TryGetInt(Tag, Result) then
    Result := Default;
end;

function TTiffDirectory.GetIntArray(Tag: Word): TTiffInt64Array;
var
  T: PTiffTag;
  I: Integer;
begin
  Result := nil;
  T := Find(Tag);
  if T = nil then
    Exit;
  SetLength(Result, T.Count);
  for I := 0 to T.Count - 1 do
    Result[I] := T.GetInt(I);
end;

function TTiffDirectory.TryGetFloat(Tag: Word; out Value: Double; Index: Int64): Boolean;
var
  T: PTiffTag;
begin
  T := Find(Tag);
  Result := (T <> nil) and (Index >= 0) and (Index < T.Count);
  if Result then
    Value := T.GetFloat(Index)
  else
    Value := 0;
end;

function TTiffDirectory.GetFloat(Tag: Word; const Default: Double): Double;
begin
  if not TryGetFloat(Tag, Result) then
    Result := Default;
end;

function TTiffDirectory.GetFloatArray(Tag: Word): TTiffDoubleArray;
var
  T: PTiffTag;
  I: Integer;
begin
  Result := nil;
  T := Find(Tag);
  if T = nil then
    Exit;
  SetLength(Result, T.Count);
  for I := 0 to T.Count - 1 do
    Result[I] := T.GetFloat(I);
end;

function TTiffDirectory.GetText(Tag: Word): AnsiString;
var
  T: PTiffTag;
begin
  T := Find(Tag);
  if T <> nil then
    Result := T.GetText
  else
    Result := '';
end;

function TTiffDirectory.GetBytes(Tag: Word): TBytes;
var
  T: PTiffTag;
begin
  T := Find(Tag);
  if T <> nil then
    Result := Copy(T.Data, 0, Length(T.Data))
  else
    Result := nil;
end;

procedure PutIntValue(P: PByte; Tag, DataType: Word; Value: Int64);
var
  Lo, Hi: Int64;
begin
  case DataType of
    ttByte, ttUndefined, ttAscii, ttUtf8: begin Lo := 0; Hi := $FF; end;
    ttSByte: begin Lo := -128; Hi := 127; end;
    ttShort: begin Lo := 0; Hi := $FFFF; end;
    ttSShort: begin Lo := -32768; Hi := 32767; end;
    ttLong, ttIFD: begin Lo := 0; Hi := $FFFFFFFF; end;
    ttSLong: begin Lo := Low(Integer); Hi := High(Integer); end;
    ttLong8, ttIFD8: begin Lo := 0; Hi := High(Int64); end;
    ttSLong8: begin Lo := Low(Int64); Hi := High(Int64); end;
  else
    raise ETiffError.CreateFmt('TIFF tag %d: data type %d is not an integer type', [Tag, DataType]);
  end;
  if (Value < Lo) or (Value > Hi) then
    raise ETiffError.CreateFmt(STagRange, [Value, DataType, Tag]);
  case TiffTypeSize(DataType) of
    1: P^ := Byte(Value and $FF);
    2: PWord(P)^ := Word(Value and $FFFF);
    4: PCardinal(P)^ := Cardinal(Value and $FFFFFFFF);
    8: PInt64(P)^ := Value;
  end;
end;

procedure TTiffDirectory.SetInt(Tag, DataType: Word; Value: Int64);
begin
  SetIntArray(Tag, DataType, [Value]);
end;

procedure TTiffDirectory.SetIntArray(Tag, DataType: Word; const Values: array of Int64);
var
  T: PTiffTag;
  I, Size: Integer;
begin
  T := AddTag(Tag, DataType, Length(Values));
  Size := TiffTypeSize(DataType);
  for I := 0 to High(Values) do
    PutIntValue(@T.Data[I * Size], Tag, DataType, Values[I]);
end;

procedure TTiffDirectory.SetRational(Tag: Word; const Value: Double);
const
  // Keeps 4 decimal places, which is plenty for resolutions like 299.9994
  Denominator = 10000;
var
  T: PTiffTag;
  Num, Den: Cardinal;
begin
  if (Value < 0) or (Value > High(Cardinal)) then
    raise ETiffError.CreateFmt('Value %g cannot be stored as RATIONAL in TIFF tag %d', [Value, Tag]);
  if (Frac(Value) = 0) or (Value > High(Cardinal) / Denominator) then
  begin
    Num := Round(Value);
    Den := 1;
  end
  else
  begin
    Num := Round(Value * Denominator);
    Den := Denominator;
  end;
  T := AddTag(Tag, ttRational, 1);
  PCardinal(@T.Data[0])[0] := Num;
  PCardinal(@T.Data[0])[1] := Den;
end;

procedure TTiffDirectory.SetText(Tag: Word; const Value: AnsiString; DataType: Word);
var
  T: PTiffTag;
begin
  // AddTag zeroes Data, so the terminating NUL is already there
  T := AddTag(Tag, DataType, Length(Value) + 1);
  if Value <> '' then
    Move(Value[1], T.Data[0], Length(Value));
end;

procedure TTiffDirectory.SetBytes(Tag, DataType: Word; Data: Pointer; Count: Int64);
var
  T: PTiffTag;
begin
  T := AddTag(Tag, DataType, Count);
  if Length(T.Data) > 0 then
    Move(Data^, T.Data[0], Length(T.Data));
end;

procedure TTiffDirectory.Remove(Tag: Word);
var
  Index, I: Integer;
begin
  if not IndexOf(Tag, Index) then
    Exit;
  for I := Index to FCount - 2 do
    FTags[I] := FTags[I + 1];
  Dec(FCount);
  FTags[FCount].Data := nil;
end;

function TTiffDirectory.CopyTagFrom(Source: TTiffDirectory; Tag: Word): Boolean;
var
  T: PTiffTag;
begin
  T := Source.Find(Tag);
  Result := T <> nil;
  if Result then
    if T.Count > 0 then
      SetBytes(Tag, T.DataType, @T.Data[0], T.Count)
    else
      SetBytes(Tag, T.DataType, nil, 0);
end;

{ TTiffContainer }

constructor TTiffContainer.Create(AStream: TStream; ASize: Int64);
begin
  inherited Create;
  FStream := AStream;
  FBase := AStream.Position;
  if ASize < 0 then
    FSize := AStream.Size - FBase
  else
    FSize := ASize;
end;

function TTiffContainer.OffsetSize: Integer;
begin
  if FBigTiff then
    Result := 8
  else
    Result := 4;
end;

function TTiffContainer.FirstDirLinkOffset: Int64;
begin
  if FBigTiff then
    Result := 8
  else
    Result := 4;
end;

procedure TTiffContainer.StreamRead(Buffer: Pointer; Count: NativeInt);
var
  P: PByte;
  Chunk, Got: Integer;
begin
  P := Buffer;
  while Count > 0 do
  begin
    if Count > IOBlockSize then
      Chunk := IOBlockSize
    else
      Chunk := Count;
    Got := FStream.Read(P^, Chunk);
    if Got <= 0 then
      raise ETiffIOError.Create('Error reading TIFF data from stream');
    Inc(P, Got);
    Dec(Count, Got);
  end;
end;

procedure TTiffContainer.StreamWrite(Buffer: Pointer; Count: NativeInt);
var
  P: PByte;
  Chunk, Done: Integer;
begin
  P := Buffer;
  while Count > 0 do
  begin
    if Count > IOBlockSize then
      Chunk := IOBlockSize
    else
      Chunk := Count;
    Done := FStream.Write(P^, Chunk);
    if Done <= 0 then
      raise ETiffIOError.Create('Error writing TIFF data to stream');
    Inc(P, Done);
    Dec(Count, Done);
  end;
end;

{ Unaligned access to file structures; they are copied to local variables
  because some CPUs do not allow reading multi-byte values at odd addresses. }

function TTiffContainer.GetU16(P: PByte): Word;
begin
  Move(P^, Result, 2);
  if FSwap then
    TiffSwapBytes(@Result, 2, 1);
end;

function TTiffContainer.GetU32(P: PByte): Cardinal;
begin
  Move(P^, Result, 4);
  if FSwap then
    TiffSwapBytes(@Result, 4, 1);
end;

{ Reads offset or count field of the current TIFF flavor. Returns False for
  64-bit values above High(Int64): no real file has them. }
function TTiffContainer.GetOffset(P: PByte; out Value: Int64): Boolean;
begin
  if FBigTiff then
  begin
    Move(P^, Value, 8);
    if FSwap then
      TiffSwapBytes(@Value, 8, 1);
    Result := Value >= 0;
  end
  else
  begin
    Value := GetU32(P);
    Result := True;
  end;
end;

procedure TTiffContainer.PutU16(P: PByte; Value: Word);
begin
  if FSwap then
    TiffSwapBytes(@Value, 2, 1);
  Move(Value, P^, 2);
end;

procedure TTiffContainer.PutU32(P: PByte; Value: Cardinal);
begin
  if FSwap then
    TiffSwapBytes(@Value, 4, 1);
  Move(Value, P^, 4);
end;

procedure TTiffContainer.PutU64(P: PByte; Value: Int64);
begin
  if FSwap then
    TiffSwapBytes(@Value, 8, 1);
  Move(Value, P^, 8);
end;

procedure TTiffContainer.ReadHeader;
var
  Header: array[0..15] of Byte;
  Magic: Word;
begin
  if FSize < 8 then
    raise ETiffFormatError.Create(SNotTiff);
  FStream.Position := FBase;
  StreamRead(@Header, 8);

  if (Header[0] = Ord('I')) and (Header[1] = Ord('I')) then
    FByteOrder := tboLittleEndian
  else if (Header[0] = Ord('M')) and (Header[1] = Ord('M')) then
    FByteOrder := tboBigEndian
  else
    raise ETiffFormatError.Create(SNotTiff);
  // The CPU is little-endian (see PasTiff.inc)
  FSwap := FByteOrder = tboBigEndian;

  Magic := GetU16(@Header[2]);
  case Magic of
    42:
      begin
        FBigTiff := False;
        FFirstDirOffset := GetU32(@Header[4]);
      end;
    43:
      begin
        // BigTIFF header continues with: size of offsets (always 8), zero
        // padding and the 8-byte offset of the first directory.
        FBigTiff := True;
        if FSize < 16 then
          raise ETiffFormatError.Create(SNotTiff);
        StreamRead(@Header[8], 8);
        if (GetU16(@Header[4]) <> 8) or (GetU16(@Header[6]) <> 0) then
          raise ETiffFormatError.Create('Invalid BigTIFF header');
        if not GetOffset(@Header[8], FFirstDirOffset) then
          raise ETiffFormatError.Create('Invalid BigTIFF header');
      end;
  else
    raise ETiffFormatError.Create(SNotTiff);
  end;
end;

procedure TTiffContainer.ReadAt(Offset: Int64; Buffer: Pointer; Count: NativeInt);
begin
  if Count = 0 then
    Exit;
  // Written so that Offset + Count cannot overflow
  if (Offset < 0) or (Count < 0) or (Offset > FSize) or (Count > FSize - Offset) then
    raise ETiffFormatError.CreateFmt(SBeyondEnd, [Offset, Int64(Count)]);
  FStream.Position := FBase + Offset;
  StreamRead(Buffer, Count);
end;

function TTiffContainer.ReadDirectory(Offset: Int64): TTiffDirectory;
var
  CountBuf: array[0..7] of Byte;
  Entries: TBytes;
  NextBuf: array[0..7] of Byte;
  EntrySize, CountSize, I, J, InsertAt: Integer;
  N, ValueCount, ValueSize: Int64;
  P: PByte;
  Tag, DataType: Word;
  TypeSize: Integer;
  T: PTiffTag;
  Duplicate: Boolean;
begin
  if FBigTiff then
  begin
    EntrySize := 20;
    CountSize := 8;
  end
  else
  begin
    EntrySize := 12;
    CountSize := 2;
  end;

  ReadAt(Offset, @CountBuf, CountSize);
  if FBigTiff then
  begin
    if not GetOffset(@CountBuf, N) then
      N := High(Int64);
  end
  else
    N := GetU16(@CountBuf);
  // Both checks are libtiff's (TIFFFetchDirectory). The upper limit catches
  // offsets that do not point to a directory at all.
  if N = 0 then
    raise ETiffFormatError.Create('TIFF directory has no tags');
  if N > TiffMaxDirEntries then
    raise ETiffFormatError.CreateFmt('TIFF directory at offset %d is not valid (too many tags)', [Offset]);

  SetLength(Entries, N * EntrySize);
  ReadAt(Offset + CountSize, @Entries[0], Length(Entries));

  Result := TTiffDirectory.Create(Self);
  try
    Result.FOffset := Offset;
    SetLength(Result.FTags, N);

    for I := 0 to N - 1 do
    begin
      P := @Entries[I * EntrySize];
      Tag := GetU16(P);
      DataType := GetU16(P + 2);

      // Entries with a data type we do not know cannot be read (size of the
      // values is unknown) and are skipped, as libtiff does with a warning.
      TypeSize := TiffTypeSize(DataType);
      if TypeSize = 0 then
        Continue;
      // Same for value counts that cannot be real
      if not GetOffset(P + 4, ValueCount) then
        Continue;
      if ValueCount > High(Int64) div TypeSize then
        Continue;
      ValueSize := ValueCount * TypeSize;

      // TIFF requires entries sorted by tag and each tag only once. Real files
      // break both rules. libtiff warns about wrong order and reads on, and it
      // ignores all but the first occurrence of a tag ("bugzilla 1994": a file
      // with duplicated StripOffsets made it use mismatched arrays). We do the
      // same: insertion sort keeps the first of the duplicates.
      InsertAt := Result.FCount;
      Duplicate := False;
      while InsertAt > 0 do
      begin
        if Result.FTags[InsertAt - 1].Tag = Tag then
          Duplicate := True;
        if Result.FTags[InsertAt - 1].Tag <= Tag then
          Break;
        Dec(InsertAt);
      end;
      if Duplicate then
        Continue;
      for J := Result.FCount - 1 downto InsertAt do
        Result.FTags[J + 1] := Result.FTags[J];
      Inc(Result.FCount);

      T := @Result.FTags[InsertAt];
      T.Tag := Tag;
      T.DataType := DataType;
      T.Count := ValueCount;
      T.Data := nil;
      if ValueSize <= OffsetSize then
      begin
        // Values that fit into the offset field are stored in it, starting at
        // its first byte whatever the byte order is, e.g. one SHORT occupies
        // the first two of the four bytes.
        SetLength(T.Data, ValueSize);
        if ValueSize > 0 then
        begin
          Move((P + 4 + OffsetSize)^, T.Data[0], ValueSize);
          if FSwap then
            TiffSwapBytes(@T.Data[0], TiffTypeSwapUnit(DataType), ValueSize div TiffTypeSwapUnit(DataType));
        end;
        T.Loaded := True;
        T.ValueOffset := 0;
      end
      else
      begin
        // Values elsewhere in the file are loaded when somebody asks for the
        // tag. A directory can thus be read even if tags nobody needs have bad
        // offsets, and a hostile directory cannot make us allocate memory for
        // thousands of huge value arrays.
        T.Loaded := False;
        if not GetOffset(P + 4 + OffsetSize, T.ValueOffset) then
          T.ValueOffset := -1; // fails the range check when loaded
      end;
    end;

    // A file cut right after the entries still has a usable last directory
    Result.FNextOffset := 0;
    if Offset + CountSize + Length(Entries) + OffsetSize <= FSize then
    begin
      ReadAt(Offset + CountSize + Length(Entries), @NextBuf, OffsetSize);
      if not GetOffset(@NextBuf, Result.FNextOffset) then
        Result.FNextOffset := 0;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TTiffContainer.ReadDirectoryLink(Offset: Int64): Int64;
var
  Buf: array[0..7] of Byte;
  N: Int64;
  EntrySize, CountSize: Integer;
begin
  if FBigTiff then
  begin
    EntrySize := 20;
    CountSize := 8;
  end
  else
  begin
    EntrySize := 12;
    CountSize := 2;
  end;
  ReadAt(Offset, @Buf, CountSize);
  if FBigTiff then
  begin
    if not GetOffset(@Buf, N) then
      N := High(Int64);
  end
  else
    N := GetU16(@Buf);
  // Same sanity checks as in ReadDirectory
  if (N = 0) or (N > TiffMaxDirEntries) then
    raise ETiffFormatError.CreateFmt('TIFF directory at offset %d is not valid', [Offset]);
  ReadAt(Offset + CountSize + N * EntrySize, @Buf, OffsetSize);
  if not GetOffset(@Buf, Result) then
    raise ETiffFormatError.CreateFmt('TIFF directory at offset %d has invalid link', [Offset]);
end;

procedure TTiffContainer.WriteHeader(ABigTiff: Boolean);
var
  Header: array[0..15] of Byte;
begin
  FBigTiff := ABigTiff;
  // Written in the byte order of the CPU, which is little-endian
  FByteOrder := tboLittleEndian;
  FSwap := False;
  FFirstDirOffset := 0;

  FillChar(Header, SizeOf(Header), 0);
  Header[0] := Ord('I');
  Header[1] := Ord('I');
  FStream.Position := FBase;
  if ABigTiff then
  begin
    PutU16(@Header[2], 43);
    PutU16(@Header[4], 8);
    StreamWrite(@Header, 16);
    FSize := 16;
  end
  else
  begin
    PutU16(@Header[2], 42);
    StreamWrite(@Header, 8);
    FSize := 8;
  end;
end;

function TTiffContainer.Append(Data: Pointer; Count: NativeInt): Int64;
const
  Pad: Byte = 0;
begin
  FStream.Position := FBase + FSize;
  // "The value is expected to begin on a word boundary" (TIFF 6.0, section 2)
  if Odd(FSize) then
  begin
    StreamWrite(@Pad, 1);
    Inc(FSize);
  end;
  Result := FSize;
  // Only the offset has to fit 32 bits in classic TIFF, the block itself may
  // reach beyond 4 GiB.
  if not FBigTiff and (Result > $FFFFFFFF) then
    raise ETiffError.Create('File is too large for classic TIFF (4 GiB), BigTIFF must be used');
  StreamWrite(Data, Count);
  Inc(FSize, Count);
end;

function TTiffContainer.WriteDirectory(Dir: TTiffDirectory; out NextLinkOffset: Int64): Int64;
var
  Buffer, Swapped: TBytes;
  EntrySize, CountSize, I: Integer;
  P: PByte;
  T: PTiffTag;
  ValueSize, ValueOffset: Int64;
  Values: PByte;
begin
  if Dir.FCount = 0 then
    raise ETiffError.Create('Cannot write TIFF directory without tags');
  if FBigTiff then
  begin
    EntrySize := 20;
    CountSize := 8;
  end
  else
  begin
    EntrySize := 12;
    CountSize := 2;
  end;

  SetLength(Buffer, CountSize + Dir.FCount * EntrySize + OffsetSize);
  if FBigTiff then
    PutU64(@Buffer[0], Dir.FCount)
  else
    PutU16(@Buffer[0], Dir.FCount);

  for I := 0 to Dir.FCount - 1 do
  begin
    T := @Dir.FTags[I];
    Dir.LoadTag(T^);
    if not FBigTiff then
    begin
      if T.DataType in [ttLong8, ttSLong8, ttIFD8] then
        raise ETiffError.CreateFmt('TIFF tag %d has a BigTIFF-only data type', [T.Tag]);
      if T.Count > $FFFFFFFF then
        raise ETiffError.CreateFmt('TIFF tag %d has too many values', [T.Tag]);
    end;

    P := @Buffer[CountSize + I * EntrySize];
    PutU16(P, T.Tag);
    PutU16(P + 2, T.DataType);
    if FBigTiff then
      PutU64(P + 4, T.Count)
    else
      PutU32(P + 4, T.Count);

    ValueSize := T.DataSize;
    if ValueSize = 0 then
      Continue;
    Values := @T.Data[0];
    if FSwap and (TiffTypeSwapUnit(T.DataType) > 1) then
    begin
      Swapped := Copy(T.Data, 0, ValueSize);
      TiffSwapBytes(@Swapped[0], TiffTypeSwapUnit(T.DataType), ValueSize div TiffTypeSwapUnit(T.DataType));
      Values := @Swapped[0];
    end;

    if ValueSize <= OffsetSize then
      // Small values live in the offset field itself, left justified
      Move(Values^, (P + 4 + OffsetSize)^, ValueSize)
    else
    begin
      ValueOffset := Append(Values, ValueSize);
      if FBigTiff then
        PutU64(P + 4 + OffsetSize, ValueOffset)
      else
        PutU32(P + 4 + OffsetSize, ValueOffset);
    end;
  end;

  // Link to the next directory stays zero until the caller patches it
  Result := Append(@Buffer[0], Length(Buffer));
  NextLinkOffset := Result + CountSize + Dir.FCount * EntrySize;
  Dir.FOffset := Result;
end;

procedure TTiffContainer.WriteOffsetAt(Position, Value: Int64);
var
  Buf: array[0..7] of Byte;
begin
  if (Position < 0) or (Position + OffsetSize > FSize) then
    raise ETiffError.Create('Invalid position of TIFF offset field');
  if FBigTiff then
    PutU64(@Buf, Value)
  else
  begin
    if Value > $FFFFFFFF then
      raise ETiffError.Create('File is too large for classic TIFF (4 GiB), BigTIFF must be used');
    PutU32(@Buf, Value);
  end;
  FStream.Position := FBase + Position;
  StreamWrite(@Buf, OffsetSize);
  if Position = FirstDirLinkOffset then
    FFirstDirOffset := Value;
end;

end.
