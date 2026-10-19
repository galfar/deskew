{
  PasTiff - pure Pascal TIFF reader/writer
  by Marek Mauder
  https://github.com/galfar/imaginglib
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.

  LZW and predictor code follows libtiff's tif_lzw.c and tif_predict.c:

    Copyright (c) 1988-1997 Sam Leffler
    Copyright (c) 1991-1997 Silicon Graphics, Inc.

    Permission to use, copy, modify, distribute, and sell this software and
    its documentation for any purpose is hereby granted without fee, provided
    that (i) the above copyright notices and this permission notice appear in
    all copies of the software and related documentation, and (ii) the names of
    Sam Leffler and Silicon Graphics may not be used in any advertising or
    publicity relating to the software without the specific, prior written
    permission of Sam Leffler and Silicon Graphics.

    THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
    EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
    WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.

    IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
    ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
    OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
    WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF
    LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
    OF THIS SOFTWARE.
}

{ Byte oriented compression schemes of TIFF and the predictors that go with
  them: PackBits (TIFF 6.0 section 9), LZW (section 13), Deflate (Technical
  Note 3 / Adobe Photoshop TIFF Technical Notes), horizontal differencing
  predictor (section 14) and the floating point predictor (Technical Note 3).

  Decoders are classes derived from TTiffChunkDecoder: they are given the
  whole compressed strip or tile once and then deliver the decoded data in
  pieces of any size, each call continuing where the previous one stopped.
  That lets the reader decode a huge strip (images of gigabytes written as a
  single strip are common) row group by row group without a buffer for all of
  it. The one-shot functions (TiffLzwDecode...) decode a whole chunk at once
  through the same classes.

  Decoders never write past the buffer they are given and never raise for
  damaged data: less output than asked for is the error signal and the
  caller (TTiffReader) decides what to do about it.

  CCITT and JPEG compression have their own units (PasTiffFax, PasTiffJpeg). }
unit PasTiffCodecs;

{$I PasTiff.inc}

interface

uses
  SysUtils, PasTiffTypes
{$IFNDEF PASTIFF_NO_DEFLATE}
  , ImagingZLib
{$ENDIF}
  ;

type
  { Growing byte buffer the encoders write to. }
  TTiffOutBuffer = record
    Data: TBytes;
    Size: NativeInt;
    procedure Clear;
    procedure AddByte(Value: Byte); inline;
    procedure Add(Source: Pointer; Count: NativeInt);
  private
    procedure Grow(Needed: NativeInt);
  end;

  { Decoder of one compressed strip or tile delivering its data in pieces.
    Start may be called again for the next chunk, the object is reused. }
  TTiffChunkDecoder = class
  protected
    FSrc: PByte;
    FSrcSize: NativeInt;
    FSrcPos: NativeInt;
  public
    { Starts a new chunk. Src must stay valid (and unchanged) until the last
      Decode call for this chunk. }
    procedure Start(Src: PByte; SrcSize: NativeInt); virtual;
    { Decodes the next Count bytes of the chunk to Dst. Returns the number of
      bytes produced, which is less than Count only when the data ends or is
      damaged (later calls then return 0). }
    function Decode(Dst: PByte; Count: NativeInt): NativeInt; virtual; abstract;
  end;

  TTiffPackBitsDecoder = class(TTiffChunkDecoder)
  private
    FRunLeft: Integer;     // bytes of a run still to be written
    FRunByte: Byte;
    FLiteralLeft: Integer; // literal bytes still to be copied
  public
    procedure Start(Src: PByte; SrcSize: NativeInt); override;
    function Decode(Dst: PByte; Count: NativeInt): NativeInt; override;
  end;

  TTiffLzwDecoder = class(TTiffChunkDecoder)
  private
    // String of code C is the string of Prefix[C] followed by Suffix[C].
    // StrLen and First (first byte of the string) are kept so that a string
    // can be written from its end without a temporary stack.
    FPrefix, FStrLen: array[0..4095] of Word;
    FSuffix, FFirst: array[0..4095] of Byte;
    FOldStyle: Boolean;
    FBitBuf: Cardinal;
    FBitCount, FNBits: Integer;
    FFreeCode, FOldCode: Integer;
    FPendingCode: Integer;    // string not written completely by the last call, -1 if none
    FPendingPos: Integer;     // index of its next byte
    FEnded: Boolean;
    function WriteString(Code, From: Integer; Dst: PByte; Room: NativeInt): NativeInt;
  public
    procedure Start(Src: PByte; SrcSize: NativeInt); override;
    function Decode(Dst: PByte; Count: NativeInt): NativeInt; override;
  end;

  { Not available when compiled with PASTIFF_NO_DEFLATE: Decode then
    returns 0. }
  TTiffDeflateDecoder = class(TTiffChunkDecoder)
  private
  {$IFNDEF PASTIFF_NO_DEFLATE}
    FInflater: TZInflater;
  {$ENDIF}
    FEnded: Boolean;
    procedure Finish;
  public
    destructor Destroy; override;
    procedure Start(Src: PByte; SrcSize: NativeInt); override;
    function Decode(Dst: PByte; Count: NativeInt): NativeInt; override;
  end;

{ Decodes a whole chunk with Decoder, which is freed afterwards. }
function DecodeAll(Decoder: TTiffChunkDecoder; Src: PByte; SrcSize: NativeInt;
  Dst: PByte; DstSize: NativeInt): NativeInt;

{ PackBits }

function TiffPackBitsDecode(Src: PByte; SrcSize: NativeInt; Dst: PByte; DstSize: NativeInt): NativeInt;
{ Compresses RowCount rows of RowBytes each. Runs never cross row boundaries. }
procedure TiffPackBitsEncode(Src: PByte; RowBytes: NativeInt; RowCount: Integer; var Output: TTiffOutBuffer);

{ LZW }

function TiffLzwDecode(Src: PByte; SrcSize: NativeInt; Dst: PByte; DstSize: NativeInt): NativeInt;
procedure TiffLzwEncode(Src: PByte; SrcSize: NativeInt; var Output: TTiffOutBuffer);

{ Deflate (zlib). Not available when compiled with PASTIFF_NO_DEFLATE:
  decoder then returns 0 and encoder raises ETiffUnsupported. }

function TiffDeflateDecode(Src: PByte; SrcSize: NativeInt; Dst: PByte; DstSize: NativeInt): NativeInt;
procedure TiffDeflateEncode(Src: PByte; SrcSize: NativeInt; var Output: TTiffOutBuffer);
function TiffDeflateAvailable: Boolean;

{ Predictors }

{ True if the predictor can be used with this sample layout. Horizontal
  predictor works on 8, 16, 32 and 64-bit samples, the floating point one on
  16, 24, 32 and 64-bit floats. }
function TiffPredictorSupported(Predictor, BitsPerSample, SampleFormat: Integer): Boolean;

{ Undoes the predictor on RowCount decoded rows in place and leaves samples in
  host byte order.
    Stride      - number of samples between a sample and the one it was
                  predicted from: SamplesPerPixel for contiguous data, 1 for
                  separate planes.
    SwapBytes   - multi-byte samples in Data have other byte order than the
                  CPU (the file's byte order differs). The horizontal predictor
                  swaps them before accumulating; the floating point predictor
                  ignores this as its data has no file byte order. }
procedure TiffPredictorDecode(Predictor: Integer; Data: PByte; RowBytes: NativeInt;
  RowCount: Integer; BitsPerSample, Stride: Integer; SwapBytes: Boolean);
{ Applies horizontal predictor to rows of samples in host byte order, in place. }
procedure TiffPredictorEncode(Data: PByte; RowBytes: NativeInt; RowCount: Integer;
  BitsPerSample, Stride: Integer);

implementation

{ TTiffChunkDecoder }

procedure TTiffChunkDecoder.Start(Src: PByte; SrcSize: NativeInt);
begin
  FSrc := Src;
  FSrcSize := SrcSize;
  if FSrcSize < 0 then
    FSrcSize := 0;
  FSrcPos := 0;
end;

function DecodeAll(Decoder: TTiffChunkDecoder; Src: PByte; SrcSize: NativeInt;
  Dst: PByte; DstSize: NativeInt): NativeInt;
begin
  try
    Decoder.Start(Src, SrcSize);
    Result := Decoder.Decode(Dst, DstSize);
  finally
    Decoder.Free;
  end;
end;

{ TTiffOutBuffer }

procedure TTiffOutBuffer.Clear;
begin
  Size := 0;
end;

procedure TTiffOutBuffer.Grow(Needed: NativeInt);
var
  NewCap: NativeInt;
begin
  NewCap := Length(Data) * 2;
  if NewCap < 4096 then
    NewCap := 4096;
  if NewCap < Needed then
    NewCap := Needed;
  SetLength(Data, NewCap);
end;

procedure TTiffOutBuffer.AddByte(Value: Byte);
begin
  if Size >= Length(Data) then
    Grow(Size + 1);
  Data[Size] := Value;
  Inc(Size);
end;

procedure TTiffOutBuffer.Add(Source: Pointer; Count: NativeInt);
begin
  if Count <= 0 then
    Exit;
  if Size + Count > Length(Data) then
    Grow(Size + Count);
  Move(Source^, Data[Size], Count);
  Inc(Size, Count);
end;

{ PackBits

  A stream of packets: header byte N, then
    N in 0..127     N + 1 literal bytes follow
    N in -127..-1   the next byte is repeated 1 - N times
    N = -128        no operation (not produced by any known writer but allowed) }

procedure TTiffPackBitsDecoder.Start(Src: PByte; SrcSize: NativeInt);
begin
  inherited;
  FRunLeft := 0;
  FLiteralLeft := 0;
end;

function TTiffPackBitsDecoder.Decode(Dst: PByte; Count: NativeInt): NativeInt;
var
  N: NativeInt;
  Header: Integer;
begin
  Result := 0;
  while Result < Count do
  begin
    // Rest of a packet the previous call (or the previous loop) did not finish.
    // A run or literal reaching past the expected size of the chunk is never
    // asked for and so clipped. libtiff does the same ("Discarding %d bytes to
    // avoid buffer overrun"): some writers pack the whole strip in one go and
    // let the last packet run a few bytes over.
    if FRunLeft > 0 then
    begin
      N := Count - Result;
      if N > FRunLeft then
        N := FRunLeft;
      FillChar(Dst[Result], N, FRunByte);
      Inc(Result, N);
      Dec(FRunLeft, N);
      Continue;
    end;
    if FLiteralLeft > 0 then
    begin
      N := Count - Result;
      if N > FLiteralLeft then
        N := FLiteralLeft;
      // Literal bytes missing at the end of data: take what is there
      if N > FSrcSize - FSrcPos then
        N := FSrcSize - FSrcPos;
      if N <= 0 then
      begin
        FLiteralLeft := 0;
        Break;
      end;
      Move(FSrc[FSrcPos], Dst[Result], N);
      Inc(FSrcPos, N);
      Inc(Result, N);
      Dec(FLiteralLeft, N);
      Continue;
    end;

    // Next packet
    if FSrcPos >= FSrcSize then
      Break;
    Header := ShortInt(FSrc[FSrcPos]);
    Inc(FSrcPos);
    if Header = -128 then
      Continue
    else if Header < 0 then
    begin
      if FSrcPos >= FSrcSize then
        Break;
      FRunByte := FSrc[FSrcPos];
      Inc(FSrcPos);
      FRunLeft := 1 - Header;
    end
    else
      FLiteralLeft := Header + 1;
  end;
end;

function TiffPackBitsDecode(Src: PByte; SrcSize: NativeInt; Dst: PByte; DstSize: NativeInt): NativeInt;
begin
  Result := DecodeAll(TTiffPackBitsDecoder.Create, Src, SrcSize, Dst, DstSize);
end;

procedure PackBitsEncodeRow(P: PByte; N: NativeInt; var Output: TTiffOutBuffer);
var
  I, Run, LitStart, LitLen: NativeInt;
begin
  I := 0;
  while I < N do
  begin
    // Length of the run starting here
    Run := 1;
    while (I + Run < N) and (Run < 128) and (P[I + Run] = P[I]) do
      Inc(Run);
    if Run >= 2 then
    begin
      Output.AddByte(Byte(1 - Run));
      Output.AddByte(P[I]);
      Inc(I, Run);
    end
    else
    begin
      // Literal: goes on until a run of 3 starts (a run of 2 inside a literal
      // costs the same as breaking the literal, so it is left in), or 128 bytes
      LitStart := I;
      Inc(I);
      while (I < N) and (I - LitStart < 128) do
      begin
        if (I + 2 < N) and (P[I] = P[I + 1]) and (P[I] = P[I + 2]) then
          Break;
        Inc(I);
      end;
      LitLen := I - LitStart;
      Output.AddByte(Byte(LitLen - 1));
      Output.Add(P + LitStart, LitLen);
    end;
  end;
end;

procedure TiffPackBitsEncode(Src: PByte; RowBytes: NativeInt; RowCount: Integer; var Output: TTiffOutBuffer);
var
  Row: Integer;
begin
  // "Each row must be packed separately. Do not compress across row
  // boundaries." (TIFF 6.0). libtiff's encoder explains why it matters: a
  // reader that decodes row by row cannot deal with a run that continues
  // into the next row.
  for Row := 0 to RowCount - 1 do
  begin
    PackBitsEncodeRow(Src, RowBytes, Output);
    Inc(Src, RowBytes);
  end;
end;

{ LZW

  TIFF's LZW is the GIF algorithm with these specifics:
    - codes are 9 to 12 bits wide and are packed most significant bit first,
    - code 256 is Clear (reset the table), 257 is EndOfInformation, the first
      free table entry is 258,
    - "early change": code width grows one code earlier than strictly needed,
      when entry 510 (1022, 2046) has been added rather than 511.

  Old-style streams. Before TIFF 5.0 (1988) fixed the details, some writers
  (libtiff calls it "old-style LZW", produced by early Aldus software and
  others for years after) packed codes least significant bit first and did
  not use the early change. Such streams are recognized by their first two
  bytes: a strip starts with Clear (256), which is 00 followed by an odd byte
  when written LSB first, and 80 00 when written MSB first. Same test as in
  libtiff's LZWPreDecode. }

const
  LzwCodeClear = 256;
  LzwCodeEoi   = 257;
  LzwCodeFirst = 258;
  LzwMaxBits   = 12;
  LzwTableSize = 1 shl LzwMaxBits;

procedure TTiffLzwDecoder.Start(Src: PByte; SrcSize: NativeInt);
var
  I: Integer;
begin
  inherited;
  FOldStyle := (FSrcSize >= 2) and (FSrc[0] = 0) and ((FSrc[1] and 1) <> 0);
  for I := 0 to 255 do
  begin
    FSuffix[I] := I;
    FFirst[I] := I;
    FStrLen[I] := 1;
    FPrefix[I] := 0;
  end;
  FBitBuf := 0;
  FBitCount := 0;
  FNBits := 9;
  FFreeCode := LzwCodeFirst;
  FOldCode := -1;
  FPendingCode := -1;
  FEnded := FSrcSize <= 0;
end;

{ Writes bytes From.. of the string of Code, as many as fit into Room.
  What does not fit is remembered for the next call. }
function TTiffLzwDecoder.WriteString(Code, From: Integer; Dst: PByte; Room: NativeInt): NativeInt;
var
  Len, Skip, I, C: Integer;
  P: PByte;
begin
  Len := FStrLen[Code];
  Result := Len - From;
  if Result > Room then
    Result := Room;
  // The chain of a code leads from the last byte of its string to the first,
  // so the bytes after the part written now are skipped first
  C := Code;
  Skip := Len - From - Result;
  while Skip > 0 do
  begin
    C := FPrefix[C];
    Dec(Skip);
  end;
  P := Dst + Result;
  for I := 1 to Result do
  begin
    Dec(P);
    P^ := FSuffix[C];
    C := FPrefix[C];
  end;
  if From + Result < Len then
  begin
    FPendingCode := Code;
    FPendingPos := From + Result;
  end
  else
    FPendingCode := -1;
end;

function TTiffLzwDecoder.Decode(Dst: PByte; Count: NativeInt): NativeInt;
var
  Code: Integer;
  FirstByte: Byte;
begin
  Result := 0;
  // String cut by the end of the previous call
  if FPendingCode >= 0 then
    Inc(Result, WriteString(FPendingCode, FPendingPos, Dst, Count));

  while (Result < Count) and not FEnded do
  begin
    // Next code. Running out of input ends decoding: the strip is short.
    while FBitCount < FNBits do
    begin
      if FSrcPos >= FSrcSize then
      begin
        FEnded := True;
        Exit;
      end;
      if FOldStyle then
        FBitBuf := FBitBuf or (Cardinal(FSrc[FSrcPos]) shl FBitCount)
      else
        FBitBuf := (FBitBuf shl 8) or FSrc[FSrcPos];
      Inc(FSrcPos);
      Inc(FBitCount, 8);
    end;
    if FOldStyle then
    begin
      Code := FBitBuf and ((1 shl FNBits) - 1);
      FBitBuf := FBitBuf shr FNBits;
    end
    else
      Code := (FBitBuf shr (FBitCount - FNBits)) and ((1 shl FNBits) - 1);
    Dec(FBitCount, FNBits);

    if Code = LzwCodeEoi then
    begin
      FEnded := True;
      Break;
    end;

    if Code = LzwCodeClear then
    begin
      FFreeCode := LzwCodeFirst;
      FNBits := 9;
      FOldCode := -1;
      Continue;
    end;

    if FOldCode < 0 then
    begin
      // First code after Clear must be a plain byte.
      //
      // This branch also takes the first code of a strip that does not start
      // with Clear. The specification requires the Clear and current libtiff
      // rejects such strips as corrupted (older versions read them, with a
      // table numbering no encoder could have meant). We read them as a
      // standard LZW decoder would: the first code is output and defines no
      // table entry. Deviation from libtiff, harmless for valid files.
      if Code > 255 then
      begin
        FEnded := True;
        Break;
      end;
      Dst[Result] := Code;
      Inc(Result);
      FOldCode := Code;
      Continue;
    end;

    if Code < FFreeCode then
      FirstByte := FFirst[Code]
    else if (Code = FFreeCode) and (FFreeCode < LzwTableSize) then
      // The one code that can be used before it is defined: its string is the
      // previous string plus the previous string's first byte (the "KwKwK" case).
      FirstByte := FFirst[FOldCode]
    else
    begin
      // Code that is not in the table: data is corrupted
      FEnded := True;
      Break;
    end;

    // New table entry: previous string + first byte of the current one.
    //
    // A full table normally cannot happen: writers emit Clear before that
    // and libtiff treats any other code at this point as corruption. We keep
    // decoding with the table as it is, which is what GIF decoders do for
    // writers that postpone the Clear. Deviation from libtiff.
    if FFreeCode < LzwTableSize then
    begin
      FPrefix[FFreeCode] := FOldCode;
      FSuffix[FFreeCode] := FirstByte;
      FFirst[FFreeCode] := FFirst[FOldCode];
      FStrLen[FFreeCode] := FStrLen[FOldCode] + 1;
      Inc(FFreeCode);
      // Width of the next code. New style switches one entry early
      // (FreeCode = 511, 1023, 2047), old style when the table for the
      // current width is really full (512, 1024, 2048).
      if FNBits < LzwMaxBits then
        if (FOldStyle and (FFreeCode >= (1 shl FNBits))) or
           (not FOldStyle and (FFreeCode >= (1 shl FNBits) - 1)) then
          Inc(FNBits);
    end;

    // The string of Code; what does not fit is written by the next call
    Inc(Result, WriteString(Code, 0, Dst + Result, Count - Result));
    FOldCode := Code;
  end;
end;

function TiffLzwDecode(Src: PByte; SrcSize: NativeInt; Dst: PByte; DstSize: NativeInt): NativeInt;
begin
  Result := DecodeAll(TTiffLzwDecoder.Create, Src, SrcSize, Dst, DstSize);
end;

procedure TiffLzwEncode(Src: PByte; SrcSize: NativeInt; var Output: TTiffOutBuffer);
const
  // Open addressing hash table of strings (prefix code + byte) -> code.
  // Prime size about twice the number of codes, as in libtiff.
  HashSize = 9001;
var
  HashKey: array[0..HashSize - 1] of Integer;   // (Prefix shl 8) or byte, -1 if empty
  HashCode: array[0..HashSize - 1] of Word;
  BitBuf: Cardinal;
  BitCount, NBits: Integer;
  FreeCode, Ent, Key, H, Step: Integer;
  I: NativeInt;
  B: Byte;
  Found: Boolean;

  procedure PutCode(Code: Integer);
  begin
    BitBuf := (BitBuf shl NBits) or Cardinal(Code);
    Inc(BitCount, NBits);
    while BitCount >= 8 do
    begin
      Output.AddByte((BitBuf shr (BitCount - 8)) and $FF);
      Dec(BitCount, 8);
    end;
  end;

  procedure ResetTable;
  var
    J: Integer;
  begin
    for J := 0 to HashSize - 1 do
      HashKey[J] := -1;
    FreeCode := LzwCodeFirst;
  end;

begin
  BitBuf := 0;
  BitCount := 0;
  NBits := 9;
  ResetTable;
  // Every strip starts with Clear so that it can be decoded on its own
  PutCode(LzwCodeClear);

  if SrcSize > 0 then
  begin
    Ent := Src[0];
    for I := 1 to SrcSize - 1 do
    begin
      B := Src[I];
      Key := (Ent shl 8) or B;
      H := (Integer(B) shl 4) xor Ent;       // below 8192, so within the table
      Found := False;
      if HashKey[H] <> -1 then
      begin
        if H = 0 then
          Step := 1
        else
          Step := HashSize - H;
        repeat
          if HashKey[H] = Key then
          begin
            Found := True;
            Break;
          end;
          Dec(H, Step);
          if H < 0 then
            Inc(H, HashSize);
        until HashKey[H] = -1;
      end;
      if Found then
      begin
        Ent := HashCode[H];
        Continue;
      end;

      // String Ent + B is new: emit Ent, remember the string, start over with B
      PutCode(Ent);
      Ent := B;
      HashKey[H] := Key;
      HashCode[H] := FreeCode;
      Inc(FreeCode);
      if FreeCode = LzwTableSize - 2 then
      begin
        // Table is as full as libtiff's encoder lets it get (codes up to 4093
        // are defined): Clear and start a new table
        PutCode(LzwCodeClear);
        ResetTable;
        NBits := 9;
      end
      else if FreeCode > (1 shl NBits) - 1 then
        // Early change, see the comment at the top
        Inc(NBits);
    end;

    // Last string. The decoder defines one more entry when it reads this
    // code, so the width of EndOfInformation may have to grow too.
    PutCode(Ent);
    Inc(FreeCode);
    if FreeCode = LzwTableSize - 2 then
    begin
      PutCode(LzwCodeClear);
      NBits := 9;
    end
    else if FreeCode > (1 shl NBits) - 1 then
      Inc(NBits);
  end;

  PutCode(LzwCodeEoi);
  if BitCount > 0 then
    Output.AddByte((BitBuf shl (8 - BitCount)) and $FF);
end;

{ Deflate

  Plain zlib stream per strip or tile. Compression value 8 is the official
  one; 32946 was used by early versions of the Deflate support before a code
  was assigned and still appears in files. Both mean exactly the same. }

function TiffDeflateAvailable: Boolean;
begin
  Result := {$IFDEF PASTIFF_NO_DEFLATE}False{$ELSE}True{$ENDIF};
end;

destructor TTiffDeflateDecoder.Destroy;
begin
  Finish;
  inherited;
end;

procedure TTiffDeflateDecoder.Finish;
begin
{$IFNDEF PASTIFF_NO_DEFLATE}
  ZInflateEnd(FInflater);
{$ENDIF}
  FEnded := True;
end;

procedure TTiffDeflateDecoder.Start(Src: PByte; SrcSize: NativeInt);
begin
  inherited;
  Finish;
{$IFDEF PASTIFF_NO_DEFLATE}
  FEnded := True;
{$ELSE}
  ZInflateBegin(FInflater);
  FEnded := False;
{$ENDIF}
end;

function TTiffDeflateDecoder.Decode(Dst: PByte; Count: NativeInt): NativeInt;
{$IFNDEF PASTIFF_NO_DEFLATE}
const
  // Output is produced in pieces of this size: when zlib reports damaged
  // data, the bytes of the failing call are lost, those before are kept
  Piece = 64 * 1024;
var
  N, Consumed, Produced: NativeInt;
  More: Boolean;
{$ENDIF}
begin
  Result := 0;
{$IFNDEF PASTIFF_NO_DEFLATE}
  // Output is limited to what is asked for, so a "decompression bomb" strip
  // costs nothing, and bytes after the end of the zlib stream are never read
  // (libtiff's test image deflate-last-strip-extra-data.tiff).
  while (Result < Count) and not FEnded do
  begin
    N := Count - Result;
    if N > Piece then
      N := Piece;
    try
      More := ZInflateData(FInflater, FSrc + FSrcPos, FSrcSize - FSrcPos, Dst + Result, N,
        Consumed, Produced);
    except
      on EZlibError do
      begin
        Finish;
        Break;
      end;
    end;
    Inc(FSrcPos, Consumed);
    Inc(Result, Produced);
    // End of the zlib stream, or no progress (input used up)
    if not More or ((Consumed = 0) and (Produced = 0)) then
      Finish;
  end;
{$ENDIF}
end;

function TiffDeflateDecode(Src: PByte; SrcSize: NativeInt; Dst: PByte; DstSize: NativeInt): NativeInt;
begin
  Result := DecodeAll(TTiffDeflateDecoder.Create, Src, SrcSize, Dst, DstSize);
end;

procedure TiffDeflateEncode(Src: PByte; SrcSize: NativeInt; var Output: TTiffOutBuffer);
{$IFNDEF PASTIFF_NO_DEFLATE}
var
  OutBuf: Pointer;
  OutBytes: NativeInt;
{$ENDIF}
begin
{$IFDEF PASTIFF_NO_DEFLATE}
  raise ETiffUnsupported.Create('PasTiff was compiled without Deflate support');
{$ELSE}
  OutBuf := nil;
  CompressBuf(Src, SrcSize, OutBuf, OutBytes);
  try
    Output.Add(OutBuf, OutBytes);
  finally
    FreeMem(OutBuf);
  end;
{$ENDIF}
end;

{ Predictors }

function TiffPredictorSupported(Predictor, BitsPerSample, SampleFormat: Integer): Boolean;
begin
  case Predictor of
    TiffPredictorNone:
      Result := True;
    TiffPredictorHorizontal:
      Result := (BitsPerSample = 8) or (BitsPerSample = 16) or
        (BitsPerSample = 32) or (BitsPerSample = 64);
    TiffPredictorFloat:
      Result := (SampleFormat = TiffSampleFormatFloat) and
        ((BitsPerSample = 16) or (BitsPerSample = 24) or
         (BitsPerSample = 32) or (BitsPerSample = 64));
  else
    Result := False;
  end;
end;

// Sample arithmetic below wraps around on purpose
{$IFOPT Q+}{$DEFINE PASTIFF_RESTORE_Q}{$Q-}{$ENDIF}
{$IFOPT R+}{$DEFINE PASTIFF_RESTORE_R}{$R-}{$ENDIF}

{ Horizontal differencing: every sample is stored as the difference to the
  sample of the same channel in the previous pixel of the row; the first pixel
  of a row is stored as is. Arithmetic wraps around in the sample size.

  The loops are bounded by the row size in whole samples. libtiff computes
  them from byte counts and had to add checks that the row size is a multiple
  of the pixel size (heap overflows otherwise, CVE-2016-9535 and others);
  counting whole samples makes that class of error impossible here. }
procedure HorizontalAccumulate(Row: PByte; RowBytes: NativeInt; BytesPerSample, Stride: Integer);
var
  I, Count: NativeInt;
  P16: PWord;
  P32: PCardinal;
  P64: PUInt64;
begin
  Count := RowBytes div BytesPerSample;
  case BytesPerSample of
    1:
      for I := Stride to Count - 1 do
        Row[I] := Byte(Row[I] + Row[I - Stride]);
    2:
      begin
        P16 := PWord(Row);
        for I := Stride to Count - 1 do
          P16[I] := Word(P16[I] + P16[I - Stride]);
      end;
    4:
      begin
        P32 := PCardinal(Row);
        for I := Stride to Count - 1 do
          P32[I] := P32[I] + P32[I - Stride];
      end;
    8:
      begin
        P64 := PUInt64(Row);
        for I := Stride to Count - 1 do
          P64[I] := P64[I] + P64[I - Stride];
      end;
  end;
end;

procedure HorizontalDifference(Row: PByte; RowBytes: NativeInt; BytesPerSample, Stride: Integer);
var
  I, Count: NativeInt;
  P16: PWord;
  P32: PCardinal;
  P64: PUInt64;
begin
  Count := RowBytes div BytesPerSample;
  // From the end of the row so that every difference uses original values
  case BytesPerSample of
    1:
      for I := Count - 1 downto Stride do
        Row[I] := Byte(Row[I] - Row[I - Stride]);
    2:
      begin
        P16 := PWord(Row);
        for I := Count - 1 downto Stride do
          P16[I] := Word(P16[I] - P16[I - Stride]);
      end;
    4:
      begin
        P32 := PCardinal(Row);
        for I := Count - 1 downto Stride do
          P32[I] := P32[I] - P32[I - Stride];
      end;
    8:
      begin
        P64 := PUInt64(Row);
        for I := Count - 1 downto Stride do
          P64[I] := P64[I] - P64[I - Stride];
      end;
  end;
end;

{ Floating point predictor (Adobe Photoshop TIFF Technical Note 3).

  The encoder splits the floats of a row into bytes and stores all most
  significant bytes first, then all second bytes and so on, and finally
  applies byte-wise horizontal differencing to the reordered row. Exponents
  and high mantissa bytes of neighbouring pixels are similar, so this
  compresses much better than raw floats.

  Decoding reverses it: accumulate bytes, then put the bytes of each float
  back together. The stored order is most significant byte first whatever the
  file's byte order is, so the result is assembled directly in the CPU's
  (little-endian) order and needs no swapping afterwards (libtiff's fpAcc). }
procedure FloatAccumulate(Row, Temp: PByte; RowBytes: NativeInt; BytesPerSample, Stride: Integer);
var
  I, Count: NativeInt;
  B: Integer;
begin
  Count := RowBytes div BytesPerSample;
  // Note the stride: it is in bytes here, the reordered row has no samples
  for I := Stride to Count * BytesPerSample - 1 do
    Row[I] := Byte(Row[I] + Row[I - Stride]);
  Move(Row^, Temp^, Count * BytesPerSample);
  for I := 0 to Count - 1 do
    for B := 0 to BytesPerSample - 1 do
      Row[BytesPerSample * I + B] := Temp[(BytesPerSample - B - 1) * Count + I];
end;

{$IFDEF PASTIFF_RESTORE_Q}{$Q+}{$ENDIF}
{$IFDEF PASTIFF_RESTORE_R}{$R+}{$ENDIF}

procedure TiffPredictorDecode(Predictor: Integer; Data: PByte; RowBytes: NativeInt;
  RowCount: Integer; BitsPerSample, Stride: Integer; SwapBytes: Boolean);
var
  Row: Integer;
  BytesPerSample: Integer;
  Temp: TBytes;
begin
  BytesPerSample := BitsPerSample div 8;
  if (BytesPerSample = 0) or (Stride < 1) then
    Exit;
  case Predictor of
    TiffPredictorHorizontal:
      for Row := 0 to RowCount - 1 do
      begin
        // Differences were computed on values, so bytes have to be in CPU
        // order before they are added up (libtiff's swabHorAcc16 and friends)
        if SwapBytes and (BytesPerSample > 1) then
          TiffSwapBytes(Data, BytesPerSample, RowBytes div BytesPerSample);
        HorizontalAccumulate(Data, RowBytes, BytesPerSample, Stride);
        Inc(Data, RowBytes);
      end;
    TiffPredictorFloat:
      begin
        SetLength(Temp, RowBytes);
        for Row := 0 to RowCount - 1 do
        begin
          FloatAccumulate(Data, @Temp[0], RowBytes, BytesPerSample, Stride);
          Inc(Data, RowBytes);
        end;
      end;
  end;
end;

procedure TiffPredictorEncode(Data: PByte; RowBytes: NativeInt; RowCount: Integer;
  BitsPerSample, Stride: Integer);
var
  Row: Integer;
begin
  if (BitsPerSample < 8) or (Stride < 1) then
    Exit;
  for Row := 0 to RowCount - 1 do
  begin
    HorizontalDifference(Data, RowBytes, BitsPerSample div 8, Stride);
    Inc(Data, RowBytes);
  end;
end;

end.
