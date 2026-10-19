{
  PasTiff - pure Pascal TIFF reader/writer
  by Marek Mauder
  https://github.com/galfar/imaginglib
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
}

{ JPEG compression in TIFF (Compression=7) as defined by Adobe's TIFF
  Technical Note 2, "new-style JPEG". The obsolete scheme of TIFF 6.0
  (Compression=6, "old-style JPEG") is not supported.

  How JPEG data is stored: every strip or tile is a JPEG image of its own,
  with the dimensions of the strip/tile. To avoid repeating quantization and
  Huffman tables in every strip, they can be stored once in the JPEGTables
  tag as an "abbreviated table specification" (a JPEG stream with tables but
  no image); strips are then "abbreviated image" streams without tables.

  The JPEG codec is not part of PasTiff: this unit goes through the JPEG
  wrapper of Vampyre Imaging Library (ImagingJpegLib.pas), which can load a
  whole JPEG stream and nothing else. So instead of feeding the tables stream
  and the image stream to the decoder one after the other, as libtiff's
  tif_jpeg.c does, the two are spliced into one ordinary JPEG stream:

    JPEGTables:  SOI  DQT.. DHT..  EOI
    strip:       SOI  SOF  SOS  data  EOI
    spliced:     SOI  DQT.. DHT..  SOF  SOS  data  EOI

  Color: the JPEG stream in a TIFF normally has no JFIF or Adobe marker, the
  meaning of its components is given by PhotometricInterpretation:
    YCbCr      components are Y, Cb, Cr (possibly subsampled); decoded to RGB
    RGB        components are R, G, B, stored without color transform
    Separated  four components (CMYK), stored without color transform
    gray       one component
  With separate planes each strip holds one component and decodes as gray.
  Decoded data is therefore always 8-bit samples, SamplesPerPixel (or one)
  per pixel, with YCbCr already converted to RGB.

  This unit depends on Imaging; define PASTIFF_NO_JPEG to leave it out. }
unit PasTiffJpeg;

{$I PasTiff.inc}

interface

uses
  SysUtils, Classes, PasTiffTypes, PasTiffCodecs;

{ True if JPEG support is compiled in. }
function TiffJpegAvailable: Boolean;

{ Decodes one JPEG compressed strip or tile.
    Photometric - PhotometricInterpretation of the page
    Components  - samples per pixel in this chunk (1 for separate planes)
    Tables      - contents of the JPEGTables tag, nil if the page has none
    Width, Rows - size of the chunk in pixels
    MaxRows     - height the JPEG image may have at most (RowsPerStrip or
                  the tile height; at least Rows)
  Dst receives Rows rows of Width * Components bytes.

  Returns number of bytes decoded: Rows * Width * Components if all went
  well, less if the stream is damaged or does not fit the page (what could
  be decoded is in Dst anyway, the rest is zeroed). }
function TiffJpegDecode(Photometric, Components: Integer;
  Tables: PByte; TablesSize: NativeInt; Src: PByte; SrcSize: NativeInt;
  Dst: PByte; Width, Rows, MaxRows: Integer): NativeInt;

{ Compresses one strip: Rows rows of Width pixels, 8-bit samples, one (gray)
  or three (R, G, B) per pixel. RGB is stored as YCbCr with chroma subsampled
  by SubsampleH x SubsampleV (2x2, 2x1 or 1x1); the page then needs
  PhotometricInterpretation=YCbCr and a matching YCbCrSubSampling tag.

  Tables holds the contents for the JPEGTables tag. It must be empty for the
  first strip of a page, which fills it; the following strips are written
  without their tables as long as they are the same (they are, with constant
  quality). The compressed strip is appended to Output. }
procedure TiffJpegEncode(Components: Integer; Src: PByte; Width, Rows: Integer;
  Quality, SubsampleH, SubsampleV: Integer; var Tables: TBytes; var Output: TTiffOutBuffer);

implementation

{$IFNDEF PASTIFF_NO_JPEG}
uses
  ImagingTypes, Imaging, ImagingIO, ImagingJpegLib;
{$ENDIF}

function TiffJpegAvailable: Boolean;
begin
  Result := {$IFDEF PASTIFF_NO_JPEG}False{$ELSE}True{$ENDIF};
end;

{$IFDEF PASTIFF_NO_JPEG}

procedure TiffJpegEncode(Components: Integer; Src: PByte; Width, Rows: Integer;
  Quality, SubsampleH, SubsampleV: Integer; var Tables: TBytes; var Output: TTiffOutBuffer);
begin
  raise ETiffUnsupported.Create('PasTiff was compiled without JPEG support');
end;

function TiffJpegDecode(Photometric, Components: Integer;
  Tables: PByte; TablesSize: NativeInt; Src: PByte; SrcSize: NativeInt;
  Dst: PByte; Width, Rows, MaxRows: Integer): NativeInt;
begin
  Result := 0;
end;

{$ELSE}

const
  JpegMarkerSoi = $D8;
  JpegMarkerEoi = $D9;

{ Joins tables and image streams into one JPEG stream, see the unit header.
  Tolerates what real files do: tables without SOI and/or EOI, and a tables
  stream that is empty or only SOI + EOI. }
function SpliceStreams(Tables: PByte; TablesSize: NativeInt; Src: PByte; SrcSize: NativeInt): TBytes;
var
  N: NativeInt;
begin
  if (TablesSize >= 2) and (Tables[0] = $FF) and (Tables[1] = JpegMarkerSoi) then
  begin
    Inc(Tables, 2);
    Dec(TablesSize, 2);
  end;
  if (TablesSize >= 2) and (Tables[TablesSize - 2] = $FF) and (Tables[TablesSize - 1] = JpegMarkerEoi) then
    Dec(TablesSize, 2);
  if (SrcSize >= 2) and (Src[0] = $FF) and (Src[1] = JpegMarkerSoi) then
  begin
    Inc(Src, 2);
    Dec(SrcSize, 2);
  end;

  SetLength(Result, 2 + TablesSize + SrcSize);
  Result[0] := $FF;
  Result[1] := JpegMarkerSoi;
  N := 2;
  if TablesSize > 0 then
  begin
    Move(Tables^, Result[N], TablesSize);
    Inc(N, TablesSize);
  end;
  if SrcSize > 0 then
    Move(Src^, Result[N], SrcSize);
end;

{ Finds the frame header (SOFn marker) of a JPEG stream and returns what it
  says about the image. False if there is none before the image data. }
function ReadFrameHeader(Data: PByte; Size: NativeInt;
  out FrameWidth, FrameHeight, FrameComponents, Precision: Integer): Boolean;
var
  Pos, Len: NativeInt;
  Marker: Byte;
begin
  Result := False;
  if (Size < 4) or (Data[0] <> $FF) or (Data[1] <> JpegMarkerSoi) then
    Exit;
  Pos := 2;
  while Pos + 4 <= Size do
  begin
    if Data[Pos] <> $FF then
      Exit;
    Marker := Data[Pos + 1];
    // Fill bytes before a marker are allowed
    if Marker = $FF then
    begin
      Inc(Pos);
      Continue;
    end;
    if Marker = $DA then
      Exit;   // start of scan without a frame header
    Len := (NativeInt(Data[Pos + 2]) shl 8) or Data[Pos + 3];
    if (Len < 2) or (Pos + 2 + Len > Size) then
      Exit;
    // SOF0..SOF15 except DHT (C4), JPG (C8) and DAC (CC)
    if (Marker >= $C0) and (Marker <= $CF) and not (Marker in [$C4, $C8, $CC]) then
    begin
      if Len < 8 then
        Exit;
      Precision := Data[Pos + 4];
      FrameHeight := (Integer(Data[Pos + 5]) shl 8) or Data[Pos + 6];
      FrameWidth := (Integer(Data[Pos + 7]) shl 8) or Data[Pos + 8];
      FrameComponents := Data[Pos + 9];
      Result := True;
      Exit;
    end;
    Inc(Pos, Len + 2);
  end;
end;

function TiffJpegDecode(Photometric, Components: Integer;
  Tables: PByte; TablesSize: NativeInt; Src: PByte; SrcSize: NativeInt;
  Dst: PByte; Width, Rows, MaxRows: Integer): NativeInt;
var
  FrameWidth, FrameHeight, FrameComponents, Precision: Integer;
  Stream: TBytes;
  MemRec: TMemoryIORec;
  Image: TImageData;
  Info: TJpegImageInfo;
  Source: TJpegSourceColorSpace;
  RowBytes: NativeInt;
  CopyWidth, CopyRows, Y, X: Integer;
  S, D: PByte;
  Loaded: Boolean;
begin
  Result := 0;
  RowBytes := NativeInt(Width) * Components;
  FillChar(Dst^, RowBytes * Rows, 0);
  if SrcSize <= 0 then
    Exit;

  // What the components are. libtiff does the same in JPEGPreDecode: with
  // JPEGCOLORMODE_RGB it tells libjpeg the data is YCbCr only when the
  // photometric tag says so and otherwise turns color conversion off.
  case Components of
    1: Source := jscGray;
    3:
      if Photometric = TiffPhotometricYCbCr then
        Source := jscYCbCr
      else
        Source := jscRGB;
    4: Source := jscCMYK;   // any four components, taken as they are
  else
    Exit;
  end;

  if (Tables <> nil) and (TablesSize > 0) then
  begin
    Stream := SpliceStreams(Tables, TablesSize, Src, SrcSize);
    Src := @Stream[0];
    SrcSize := Length(Stream);
  end;

  // The JPEG stream says for itself how large its image is, and the decoder
  // allocates accordingly - up to 65535 x 65535 pixels for a strip that
  // should have a few rows. So the frame header is checked against the size
  // of the strip or tile before the decoder sees the data. libtiff does the
  // same in JPEGPreDecode ("JPEG strip/tile size exceeds expected
  // dimensions"), including the allowance for a last strip that was
  // compressed with the full strip height.
  if MaxRows < Rows then
    MaxRows := Rows;
  if not ReadFrameHeader(Src, SrcSize, FrameWidth, FrameHeight, FrameComponents, Precision) or
     (Precision <> 8) or (FrameComponents <> Components) or
     (FrameWidth <> Width) or (FrameHeight < 1) or (FrameHeight > MaxRows) then
    Exit;
  MemRec := PrepareMemIO(Src, SrcSize);

  InitImage(Image);
  try
    try
      Loaded := JpegLoadImage(MemoryIO, @MemRec, Image, Info, Source);
    except
      // Not a decodable JPEG stream (or wrong number of components)
      on Exception do
        Loaded := False;
    end;
    if not Loaded or (Image.Bits = nil) then
      Exit;

    // The JPEG image should have the size of the strip or tile. When it does
    // not, libtiff accepts one case (the last strip of an image coded with
    // full strip height) and refuses larger images because it decodes
    // straight into the caller's buffer. We decode to an image of our own
    // and can simply copy the part that fits, whatever the mismatch.
    CopyWidth := Image.Width;
    if CopyWidth > Width then
      CopyWidth := Width;
    CopyRows := Image.Height;
    if CopyRows > Rows then
      CopyRows := Rows;

    for Y := 0 to CopyRows - 1 do
    begin
      S := PByte(Image.Bits) + NativeInt(Y) * Image.Width * Components;
      D := Dst + Y * RowBytes;
      if Components = 3 then
      begin
        // Imaging keeps 24-bit pixels as B, G, R in memory, TIFF data is R, G, B
        for X := 0 to CopyWidth - 1 do
        begin
          D[0] := S[2];
          D[1] := S[1];
          D[2] := S[0];
          Inc(S, 3);
          Inc(D, 3);
        end;
      end
      else
        // Gray, and four components that Imaging returns in stream order
        Move(S^, D^, NativeInt(CopyWidth) * Components);
    end;

    // A smaller image or one that the decoder could not finish means damaged
    // data: report fewer bytes than asked for
    if Info.Complete and (CopyWidth = Width) and (CopyRows = Rows) then
      Result := RowBytes * Rows
    else if (CopyWidth = Width) and (CopyRows > 0) then
      Result := RowBytes * CopyRows - Ord(not Info.Complete)
    else
      Result := 0;
  finally
    FreeImage(Image);
  end;
end;

{ Splits a complete JPEG stream as written by the JPEG library into the
  parts TIFF wants. Up to the start of scan the stream is a sequence of
  segments: FF, marker byte, 2-byte big-endian length (counting itself), data.
    TableSegs  - DQT and DHT segments (quantization and Huffman tables)
    HeadSegs   - everything else before the scan (SOF, DRI...), except the
                 JFIF APP0 segment: Technical Note 2 wants the TIFF tags to
                 describe the image, not JFIF (libtiff suppresses it as well)
    ScanStart  - offset of the SOS marker; the rest of the stream follows
  Returns False if the stream does not look as expected. }
function SplitJpegStream(const Data: TBytes; Size: NativeInt;
  var TableSegs, HeadSegs: TTiffOutBuffer; out ScanStart: NativeInt): Boolean;
const
  MarkerSos = $DA;
  MarkerDqt = $DB;
  MarkerDht = $C4;
  MarkerApp0 = $E0;
var
  Pos, Len: NativeInt;
  Marker: Byte;
begin
  Result := False;
  ScanStart := 0;
  if (Size < 4) or (Data[0] <> $FF) or (Data[1] <> JpegMarkerSoi) then
    Exit;
  Pos := 2;
  while Pos + 4 <= Size do
  begin
    if Data[Pos] <> $FF then
      Exit;
    Marker := Data[Pos + 1];
    if Marker = MarkerSos then
    begin
      ScanStart := Pos;
      Result := True;
      Exit;
    end;
    Len := (NativeInt(Data[Pos + 2]) shl 8) or Data[Pos + 3];
    if (Len < 2) or (Pos + 2 + Len > Size) then
      Exit;
    case Marker of
      MarkerDqt, MarkerDht:
        TableSegs.Add(@Data[Pos], Len + 2);
      MarkerApp0: ;
    else
      HeadSegs.Add(@Data[Pos], Len + 2);
    end;
    Inc(Pos, Len + 2);
  end;
end;

procedure TiffJpegEncode(Components: Integer; Src: PByte; Width, Rows: Integer;
  Quality, SubsampleH, SubsampleV: Integer; var Tables: TBytes; var Output: TTiffOutBuffer);
const
  Marker: array[0..3] of Byte = ($FF, JpegMarkerSoi, $FF, JpegMarkerEoi);
var
  Image: TImageData;
  Params: TJpegSaveParams;
  Swapped: TBytes;
  Stream: TMemoryStream;
  Data: TBytes;
  TableSegs, HeadSegs: TTiffOutBuffer;
  ScanStart, I, Count: NativeInt;
  S, D: PByte;
  SameTables: Boolean;
begin
  if not (Components in [1, 3]) then
    raise ETiffUnsupported.Create('JPEG compression needs gray or RGB data');

  InitImage(Image);
  Image.Width := Width;
  Image.Height := Rows;
  Image.Size := NativeInt(Width) * Rows * Components;
  if Components = 1 then
  begin
    Image.Format := ifGray8;
    Image.Bits := Src;
  end
  else
  begin
    // Imaging wants 24-bit pixels as B, G, R
    Image.Format := ifR8G8B8;
    Count := NativeInt(Width) * Rows;
    SetLength(Swapped, Count * 3);
    S := Src;
    D := @Swapped[0];
    for I := 0 to Count - 1 do
    begin
      D[0] := S[2];
      D[1] := S[1];
      D[2] := S[0];
      Inc(S, 3);
      Inc(D, 3);
    end;
    Image.Bits := @Swapped[0];
  end;

  FillChar(Params, SizeOf(Params), 0);
  Params.Quality := Quality;
  if (SubsampleH = 2) and (SubsampleV = 2) then
    Params.ChromaSubsampling := jss420
  else if (SubsampleH = 2) and (SubsampleV = 1) then
    Params.ChromaSubsampling := jss422
  else
    Params.ChromaSubsampling := jss444;

  Stream := TMemoryStream.Create;
  try
    try
      if not JpegSaveImage(StreamIO, Stream, Image, Params) then
        raise ETiffError.Create('JPEG compression of TIFF strip failed');
    except
      on E: ETiffError do
        raise;
      on E: Exception do
        raise ETiffError.Create('JPEG compression of TIFF strip failed: ' + E.Message);
    end;
    SetLength(Data, Stream.Size);
    if Stream.Size > 0 then
      Move(Stream.Memory^, Data[0], Stream.Size);
  finally
    Stream.Free;
  end;

  TableSegs.Data := nil;
  TableSegs.Clear;
  HeadSegs.Data := nil;
  HeadSegs.Clear;
  if not SplitJpegStream(Data, Length(Data), TableSegs, HeadSegs, ScanStart) then
  begin
    // Unexpected structure: a complete JPEG stream with its own tables is a
    // valid strip too, just a little larger
    Output.Add(@Data[0], Length(Data));
    Exit;
  end;

  if Length(Tables) = 0 then
  begin
    // JPEGTables: SOI, tables, EOI
    SetLength(Tables, TableSegs.Size + 4);
    Move(Marker[0], Tables[0], 2);
    if TableSegs.Size > 0 then
      Move(TableSegs.Data[0], Tables[2], TableSegs.Size);
    Move(Marker[2], Tables[TableSegs.Size + 2], 2);
    SameTables := True;
  end
  else
    SameTables := (Length(Tables) = TableSegs.Size + 4) and
      ((TableSegs.Size = 0) or CompareMem(@Tables[2], @TableSegs.Data[0], TableSegs.Size));

  // Strip: SOI, frame header, (tables if they differ from JPEGTables), scan
  Output.Add(@Marker[0], 2);
  if HeadSegs.Size > 0 then
    Output.Add(@HeadSegs.Data[0], HeadSegs.Size);
  if not SameTables and (TableSegs.Size > 0) then
    Output.Add(@TableSegs.Data[0], TableSegs.Size);
  Output.Add(@Data[ScanStart], Length(Data) - ScanStart);
end;

{$ENDIF}

end.
