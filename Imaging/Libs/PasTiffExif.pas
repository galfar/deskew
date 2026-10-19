{
  PasTiff - pure Pascal TIFF reader/writer
  by Marek Mauder
  https://github.com/galfar/imaginglib
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
}

{ Bare-bones EXIF reader and writer.

  EXIF metadata is a TIFF structure. In a JPEG file it is the payload of an
  APP1 segment after the six bytes "Exif#0#0"; PNG has it in the eXIf chunk
  and WebP in the EXIF chunk. In all of them it is a complete little TIFF
  file without image data:

    header -> IFD0 (the "main" directory: Make, Model, Orientation,
              |      resolution, DateTime, Software, Artist, Copyright...)
              |-- tag 34665 -> Exif directory (exposure, lens, dates,
              |                  |             UserComment, MakerNote...)
              |                  '-- tag 40965 -> Interoperability directory
              |-- tag 34853 -> GPS directory
              '-> IFD1 (thumbnail)

  In a TIFF file the page's own directory plays the role of IFD0 and the
  Exif and GPS directories hang from the same pointer tags.

  This unit keeps those directories as TTiffDirectory objects, so any tag is
  available by number through the generic getters and setters, and adds what
  generic access lacks: loading and saving the whole structure, and reading
  and writing text, which EXIF stores in four different ways.

  It needs only PasTiffTypes and PasTiffDirectory and can be used without the
  rest of PasTiff. Finding the EXIF block inside a JPEG/PNG/WebP file and
  putting it back is up to the caller.

  Limits:
    - MakerNote (tag 37500 in the Exif directory) is kept as an opaque block
      of bytes. Camera makers store offsets in it that are relative to its
      original position in the file; after SaveToStream the block is at
      another position and vendor tools may no longer read it.
    - IFD1 and the thumbnail are not loaded and so are gone after saving.
    - Tags with data types this library does not know are dropped on load.
    - There is no table of tag names. }
unit PasTiffExif;

{$I PasTiff.inc}

interface

uses
  SysUtils, Classes, PasTiffTypes, PasTiffDirectory;

const
  { A few tags with special handling here. }
  ExifTagUserComment      = 37510;  // Exif directory; text with character code prefix
  ExifTagMakerNote        = 37500;  // Exif directory; opaque
  ExifTagXPTitle          = 40091;  // main directory; UTF-16 text written by Windows
  ExifTagXPComment        = 40092;
  ExifTagXPAuthor         = 40093;
  ExifTagXPKeywords       = 40094;
  ExifTagXPSubject        = 40095;

type
  TExifData = class
  private
    FMain, FExif, FGps, FInterop: TTiffDirectory;
    function LoadSub(Container: TTiffContainer; Parent: TTiffDirectory; PointerTag: Word): TTiffDirectory;
  public
    destructor Destroy; override;
    procedure Clear;
    { True if no directory holds any tag. }
    function IsEmpty: Boolean;

    { Loads EXIF from a TIFF-structured block starting at the current stream
      position (at the "II" or "MM" byte order mark, i.e. after "Exif#0#0" in
      a JPEG APP1 segment). Size is the size of the block, -1 for the rest of
      the stream. Everything is read at once, the stream is not needed
      afterwards. Raises ETiffFormatError if the block is not valid;
      sub-directories that cannot be read are skipped. }
    procedure LoadFromStream(Stream: TStream; Size: Int64 = -1);
    { Writes all directories as a TIFF-structured block (little-endian,
      classic TIFF) at the current stream position. }
    procedure SaveToStream(Stream: TStream);

    { Loads EXIF of a TIFF page: its Exif and GPS sub-directories, and a copy
      of the page's descriptive tags (see CopyMainTagsTo) as main directory. }
    procedure LoadFromPage(Container: TTiffContainer; PageDir: TTiffDirectory);
    { Copies descriptive tags of the main directory (description, make,
      model, orientation, resolution, software, date, artist, copyright, XP
      texts) to a directory, e.g. to that of a TIFF page being written.
      Exif and Gps are attached to a page with TTiffWriter.AddSubDirectory. }
    procedure CopyMainTagsTo(Dir: TTiffDirectory);

    { The directories; nil if absent. Any tag can be read and changed through
      them. The Need* functions create a directory that does not exist yet. }
    property Main: TTiffDirectory read FMain;
    property Exif: TTiffDirectory read FExif;
    property Gps: TTiffDirectory read FGps;
    property Interop: TTiffDirectory read FInterop;
    function NeedMain: TTiffDirectory;
    function NeedExif: TTiffDirectory;
    function NeedGps: TTiffDirectory;

    { Text of a tag, whatever way it is stored:
        ASCII       - plain tags. Meant to be 7-bit ASCII, in practice often
                      UTF-8 or a local code page: decoded as UTF-8 if valid,
                      else as Latin-1
        UTF-8       - data type 129 of EXIF 3.0
        UserComment - 8 bytes naming the character code, then the text:
                      "ASCII", "UNICODE" (UTF-16) or undefined (all zeros,
                      treated like an ASCII tag); "JIS" is not supported
        XP* tags    - UTF-16 little-endian in a BYTE array
      Returns False if Dir is nil, the tag is missing or holds no text. }
    function GetText(Dir: TTiffDirectory; Tag: Word; out Text: UnicodeString): Boolean;
    { Stores text in the form that fits the tag: UserComment with its prefix
      ("ASCII" if possible, else "UNICODE"), XP* tags as UTF-16, everything
      else as ASCII data type holding UTF-8. Empty text removes the tag. }
    procedure SetText(Dir: TTiffDirectory; Tag: Word; const Text: UnicodeString);
  end;

implementation

const
  { Limit for all values of one directory when loading (hostile files). }
  MaxDirectoryData = 16 * 1024 * 1024;

  { Tags of a main directory that describe the picture rather than how its
    pixels are stored. }
  MainTags: array[0..15] of Word = (TiffTagImageDescription, TiffTagMake, TiffTagModel,
    TiffTagOrientation, TiffTagXResolution, TiffTagYResolution, TiffTagResolutionUnit,
    TiffTagSoftware, TiffTagDateTime, TiffTagArtist, TiffTagCopyright,
    ExifTagXPTitle, ExifTagXPComment, ExifTagXPAuthor, ExifTagXPKeywords, ExifTagXPSubject);

  { Character code prefixes of UserComment }
  CodeAscii: array[0..7] of Byte = (Ord('A'), Ord('S'), Ord('C'), Ord('I'), Ord('I'), 0, 0, 0);
  CodeUnicode: array[0..7] of Byte = (Ord('U'), Ord('N'), Ord('I'), Ord('C'), Ord('O'), Ord('D'), Ord('E'), 0);

function IsXPTag(Tag: Word): Boolean;
begin
  Result := (Tag >= ExifTagXPTitle) and (Tag <= ExifTagXPSubject);
end;

{ True if the bytes are well-formed UTF-8. Checked by hand because library
  decoders differ in what they do with bad input (empty result, question
  marks...) and we need a clear yes or no. }
function IsValidUtf8(P: PByte; Len: Integer): Boolean;
var
  I, Follow: Integer;
begin
  Result := False;
  I := 0;
  while I < Len do
  begin
    Follow := 0;
    if P[I] < $80 then
      // plain ASCII byte
    else if (P[I] >= $C2) and (P[I] <= $DF) then
      Follow := 1
    else if (P[I] >= $E0) and (P[I] <= $EF) then
      Follow := 2
    else if (P[I] >= $F0) and (P[I] <= $F4) then
      Follow := 3
    else
      Exit;
    Inc(I);
    while Follow > 0 do
    begin
      if (I >= Len) or (P[I] and $C0 <> $80) then
        Exit;
      Inc(I);
      Dec(Follow);
    end;
  end;
  Result := True;
end;

{ UTF-8 if the bytes are valid UTF-8, otherwise each byte is one character
  (Latin-1): better than nothing for text in an unknown code page. }
function DecodeBytes(P: PByte; Len: Integer): UnicodeString;
var
  S: UTF8String;   // typed as UTF-8 so that no code page conversion happens
  I: Integer;
  PureAscii: Boolean;
begin
  Result := '';
  if Len <= 0 then
    Exit;
  PureAscii := True;
  for I := 0 to Len - 1 do
    if P[I] >= $80 then
      PureAscii := False;
  if not PureAscii and IsValidUtf8(P, Len) then
  begin
    SetString(S, PAnsiChar(P), Len);
    Result := {$IFDEF PASTIFF_FPC}UTF8Decode{$ELSE}UTF8ToUnicodeString{$ENDIF}(S);
    if Result <> '' then
      Exit;
  end;
  SetLength(Result, Len);
  for I := 0 to Len - 1 do
    Result[I + 1] := WideChar(P[I]);
end;

function DecodeUtf16(P: PByte; Len: Integer; BigEndian: Boolean): UnicodeString;
var
  I, N: Integer;
  W: Word;
begin
  N := Len div 2;
  SetLength(Result, N);
  for I := 0 to N - 1 do
  begin
    if BigEndian then
      W := (Word(P[2 * I]) shl 8) or P[2 * I + 1]
    else
      W := (Word(P[2 * I + 1]) shl 8) or P[2 * I];
    // Text ends at the first NUL
    if W = 0 then
    begin
      SetLength(Result, I);
      Exit;
    end;
    Result[I + 1] := WideChar(W);
  end;
end;

{ TExifData }

destructor TExifData.Destroy;
begin
  Clear;
  inherited;
end;

procedure TExifData.Clear;
begin
  FreeAndNil(FMain);
  FreeAndNil(FExif);
  FreeAndNil(FGps);
  FreeAndNil(FInterop);
end;

function TExifData.IsEmpty: Boolean;
begin
  Result := ((FMain = nil) or (FMain.TagCount = 0)) and ((FExif = nil) or (FExif.TagCount = 0)) and
    ((FGps = nil) or (FGps.TagCount = 0)) and ((FInterop = nil) or (FInterop.TagCount = 0));
end;

function TExifData.NeedMain: TTiffDirectory;
begin
  if FMain = nil then
    FMain := TTiffDirectory.Create;
  Result := FMain;
end;

function TExifData.NeedExif: TTiffDirectory;
begin
  if FExif = nil then
    FExif := TTiffDirectory.Create;
  Result := FExif;
end;

function TExifData.NeedGps: TTiffDirectory;
begin
  if FGps = nil then
    FGps := TTiffDirectory.Create;
  Result := FGps;
end;

{ Reads the directory a pointer tag of Parent refers to, nil if there is
  none or it is damaged. The pointer tag is removed from Parent: it would be
  wrong as soon as the data is saved anywhere else, and SaveToStream sets it. }
function TExifData.LoadSub(Container: TTiffContainer; Parent: TTiffDirectory; PointerTag: Word): TTiffDirectory;
var
  Offset: Int64;
begin
  Result := nil;
  if Parent = nil then
    Exit;
  try
    if Parent.TryGetInt(PointerTag, Offset) and (Offset > 0) then
    begin
      Result := Container.ReadDirectory(Offset);
      Result.LoadAll(MaxDirectoryData);
    end;
  except
    on ETiffFormatError do
      FreeAndNil(Result);
  end;
  Parent.Remove(PointerTag);
end;

procedure TExifData.LoadFromStream(Stream: TStream; Size: Int64);
var
  Container: TTiffContainer;
begin
  Clear;
  Container := TTiffContainer.Create(Stream, Size);
  try
    Container.ReadHeader;
    FMain := Container.ReadDirectory(Container.FirstDirOffset);
    FExif := LoadSub(Container, FMain, TiffTagExifIFD);
    FGps := LoadSub(Container, FMain, TiffTagGpsIFD);
    FInterop := LoadSub(Container, FExif, TiffTagInteropIFD);
    // Last, so that the pointer tags above were still readable
    FMain.LoadAll(MaxDirectoryData);
  finally
    Container.Free;
  end;
end;

procedure TExifData.LoadFromPage(Container: TTiffContainer; PageDir: TTiffDirectory);
var
  I: Integer;
  Pointers: TTiffDirectory;
begin
  Clear;
  // LoadSub removes the pointer tags it follows; the page's own directory
  // must stay as it is, so they are followed from a copy
  Pointers := TTiffDirectory.Create;
  try
    Pointers.CopyTagFrom(PageDir, TiffTagExifIFD);
    Pointers.CopyTagFrom(PageDir, TiffTagGpsIFD);
    FExif := LoadSub(Container, Pointers, TiffTagExifIFD);
    FGps := LoadSub(Container, Pointers, TiffTagGpsIFD);
    FInterop := LoadSub(Container, FExif, TiffTagInteropIFD);
  finally
    Pointers.Free;
  end;
  for I := 0 to High(MainTags) do
    if PageDir.HasTag(MainTags[I]) then
    try
      NeedMain.CopyTagFrom(PageDir, MainTags[I]);
    except
      on ETiffFormatError do ;   // tag whose values cannot be read
    end;
end;

procedure TExifData.CopyMainTagsTo(Dir: TTiffDirectory);
var
  I: Integer;
begin
  if FMain = nil then
    Exit;
  for I := 0 to High(MainTags) do
    Dir.CopyTagFrom(FMain, MainTags[I]);
end;

procedure TExifData.SaveToStream(Stream: TStream);
var
  Container: TTiffContainer;
  Offset, Link: Int64;

  { Writes a sub-directory and stores its offset in the parent. }
  procedure WriteSub(Dir, Parent: TTiffDirectory; PointerTag: Word);
  begin
    if (Dir = nil) or (Dir.TagCount = 0) then
      Exit;
    Offset := Container.WriteDirectory(Dir, Link);
    Parent.SetInt(PointerTag, ttLong, Offset);
  end;

begin
  if IsEmpty then
    raise ETiffError.Create('No EXIF data to save');
  Container := TTiffContainer.Create(Stream);
  try
    Container.WriteHeader(False);
    // Innermost first: a directory can only be written once the offsets of
    // the directories it points to are known
    if (FInterop <> nil) and (FInterop.TagCount > 0) then
      WriteSub(FInterop, NeedExif, TiffTagInteropIFD);
    if FExif <> nil then
      WriteSub(FExif, NeedMain, TiffTagExifIFD);
    if FGps <> nil then
      WriteSub(FGps, NeedMain, TiffTagGpsIFD);
    Offset := Container.WriteDirectory(NeedMain, Link);
    Container.WriteOffsetAt(Container.FirstDirLinkOffset, Offset);
  finally
    Container.Free;
    // Offsets are only valid inside the block just written
    if FMain <> nil then
    begin
      FMain.Remove(TiffTagExifIFD);
      FMain.Remove(TiffTagGpsIFD);
    end;
    if FExif <> nil then
      FExif.Remove(TiffTagInteropIFD);
  end;
end;

function TExifData.GetText(Dir: TTiffDirectory; Tag: Word; out Text: UnicodeString): Boolean;
var
  T: PTiffTag;
  P: PByte;
  Len, I, EvenZeros, OddZeros: Integer;
begin
  Text := '';
  Result := False;
  if Dir = nil then
    Exit;
  T := Dir.Find(Tag);
  if (T = nil) or (TiffTypeSize(T.DataType) <> 1) or (T.Count = 0) then
    Exit;
  P := @T.Data[0];
  Len := T.Count;

  if IsXPTag(Tag) then
    Text := DecodeUtf16(P, Len, False)
  else if (Tag = ExifTagUserComment) and (Len >= 8) then
  begin
    if CompareMem(P, @CodeUnicode, 8) then
    begin
      Inc(P, 8);
      Dec(Len, 8);
      // EXIF does not say which byte order "UNICODE" text has; writers use
      // the order of the file or of their CPU. A byte order mark settles it,
      // otherwise the position of zero bytes does: text in Latin script has
      // them in the high bytes.
      if (Len >= 2) and (P[0] = $FE) and (P[1] = $FF) then
        Text := DecodeUtf16(P + 2, Len - 2, True)
      else if (Len >= 2) and (P[0] = $FF) and (P[1] = $FE) then
        Text := DecodeUtf16(P + 2, Len - 2, False)
      else
      begin
        EvenZeros := 0;
        OddZeros := 0;
        for I := 0 to Len - 1 do
          if P[I] = 0 then
            if Odd(I) then Inc(OddZeros) else Inc(EvenZeros);
        Text := DecodeUtf16(P, Len, EvenZeros > OddZeros);
      end;
    end
    else if CompareMem(P, @CodeAscii, 5) or ((P[0] = 0) and (P[1] = 0)) then
    begin
      // "ASCII", or the undefined code (eight zero bytes)
      Inc(P, 8);
      Dec(Len, 8);
      I := 0;
      while (I < Len) and (P[I] <> 0) do
        Inc(I);
      Text := DecodeBytes(P, I);
    end
    else
      Exit;   // JIS or something unknown
  end
  else
  begin
    I := 0;
    while (I < Len) and (P[I] <> 0) do
      Inc(I);
    Text := DecodeBytes(P, I);
  end;
  Result := Text <> '';
end;

procedure TExifData.SetText(Dir: TTiffDirectory; Tag: Word; const Text: UnicodeString);
var
  Utf8: UTF8String;
  Bytes: TBytes;
  I: Integer;
  PureAscii: Boolean;

  procedure PutUtf16(Offset: Integer);
  var
    J: Integer;
  begin
    for J := 1 to Length(Text) do
    begin
      Bytes[Offset + 2 * (J - 1)] := Ord(Text[J]) and $FF;
      Bytes[Offset + 2 * (J - 1) + 1] := Ord(Text[J]) shr 8;
    end;
  end;

begin
  if Text = '' then
  begin
    Dir.Remove(Tag);
    Exit;
  end;
  PureAscii := True;
  for I := 1 to Length(Text) do
    if Ord(Text[I]) >= $80 then
      PureAscii := False;

  if IsXPTag(Tag) then
  begin
    // UTF-16 little-endian with terminating NUL character
    SetLength(Bytes, 2 * Length(Text) + 2);
    PutUtf16(0);
    Dir.SetBytes(Tag, ttByte, @Bytes[0], Length(Bytes));
  end
  else if Tag = ExifTagUserComment then
  begin
    if PureAscii then
    begin
      SetLength(Bytes, 8 + Length(Text));
      Move(CodeAscii, Bytes[0], 8);
      for I := 1 to Length(Text) do
        Bytes[7 + I] := Ord(Text[I]);
    end
    else
    begin
      SetLength(Bytes, 8 + 2 * Length(Text));
      Move(CodeUnicode, Bytes[0], 8);
      PutUtf16(8);
    end;
    Dir.SetBytes(Tag, ttUndefined, @Bytes[0], Length(Bytes));
  end
  else
  begin
    // Bytes of the UTF-8 string with its terminating NUL, copied as they
    // are (a cast to AnsiString would convert to the system code page)
    Utf8 := UTF8Encode(Text);
    Dir.SetBytes(Tag, ttAscii, PAnsiChar(Utf8), Length(Utf8) + 1);
  end;
end;

end.
