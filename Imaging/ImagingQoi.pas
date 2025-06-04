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

{ Adds support for the Quite OK Image Format (QOI).
  Based on the QOI specification by Dominic Szablewski https://qoiformat.org. }
unit ImagingQoi;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingFormats, ImagingIO, ImagingUtility;

type
  { Class for loading and saving Quite OK Image (QOI) files.
    Supports 3-channel (RGB) and 4-channel (RGBA) images.
    Uses a simple, fast, lossless compression scheme. }
  TQoiFileFormat = class(TImageFileFormat)
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  end;

implementation

uses
  ImagingColors;

const
  SQOIFormatName = 'Quite OK Image';
  SQOIMasks      = '*.qoi';
  QOISupportedFormats: TImageFormats = [ifR8G8B8, ifA8R8G8B8];

const
  // QOI constants from the specification
  QoiMagic = UInt32(Byte('q') or (Byte('o') shl 8) or (Byte('i') shl 16) or (Byte('f') shl 24)); // 'qoif' magic bytes
  QoiHeaderSize  = 14;
  QoiPaddingSize = 8; // 7 bytes 0x00, 1 byte 0x01
  QoiMask2Tag    = $C0;             // 11000000 - Mask for 2-bit tags
  QoiMask2Data   = not QoiMask2Tag; // $3F = 00111111 - Mask for "data" of 2-bit tags

  // QOI Opcodes
  // 2-bit tags
  QOI_OP_INDEX = $00; // 00xxxxxx
  QOI_OP_DIFF  = $40; // 01xxxxxx
  QOI_OP_LUMA  = $80; // 10xxxxxx
  QOI_OP_RUN   = $C0; // 11xxxxxx
  // 8-bit tags
  QOI_OP_RGB   = $FE; // 11111110
  QOI_OP_RGBA  = $FF; // 11111111

var
  // Padding written at the end of the QOI file
  QoiPadding: array[0..QoiPaddingSize - 1] of Byte = (0, 0, 0, 0, 0, 0, 0, 1);

type
  TQoiHeader = packed record
    Magic: UInt32;      // Magic identifier 'qoif'
    Width: UInt32;      // Image width in pixels (Big Endian)
    Height: UInt32;     // Image height in pixels (Big Endian)
    Channels: Byte;     // 3 = RGB, 4 = RGBA
    Colorspace: Byte;   // 0 = sRGB with linear alpha, 1 = all channels linear
  end;

// Calculates the QOI hash index for a color
function QoiColorHash(const C: TColor32Rec): Byte; {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := (C.R * 3 + C.G * 5 + C.B * 7 + C.A * 11) mod 64;
end;

// Swaps header fields between Big Endian (QOI file) and Little Endian (System)
procedure SwapQoiHeader(var Header: TQoiHeader);
begin
  // Magic is already correct if read as UInt32 on Little Endian
  Header.Width := SwapEndianUInt32(Header.Width);
  Header.Height := SwapEndianUInt32(Header.Height);
  // Channels and Colorspace are single bytes, no swap needed
end;

{ TQoiFileFormat implementation }

procedure TQoiFileFormat.Define;
begin
  inherited Define;
  FName := SQOIFormatName;
  FFeatures := [ffLoad, ffSave];
  FSupportedFormats := QOISupportedFormats;
  AddMasks(SQOIMasks);
end;

function TQoiFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Magic: UInt32;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Magic, SizeOf(Magic));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount = SizeOf(Magic)) and (Magic = QoiMagic);
  end;
end;

function TQoiFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Header: TQoiHeader;
  Stream: TImagingIOStream;
  NumPixels, PixelIndex: NativeInt;
  RunLength: Integer;
  Index: array[0..63] of TColor32Rec; // Running index of recently seen colors
  Pixel, PrevPixel: TColor32Rec;
  DestPtr: PByte;
  B1, B2: Byte;
  DR, DG, DB: Integer; // Differences
begin
  Result := False;
  SetLength(Images, 1);
  Stream := TImagingIOStream.Create(GetIO, Handle);

  with Images[0] do
  try
    // Read and validate header
    Stream.ReadBuffer(Header, SizeOf(Header));
    if Header.Magic <> QoiMagic then
      Exit;

    SwapQoiHeader(Header); // Convert from Big Endian

    if (Header.Width = 0) or (Header.Height = 0) or
       (Header.Channels < 3) or (Header.Channels > 4) or
       (Header.Colorspace > 1) then
    begin
      Exit; // Invalid header data
    end;

    if Header.Channels = 3 then
      Format := ifR8G8B8
    else
      Format := ifA8R8G8B8;

    if not NewImage(Header.Width, Header.Height, Format, Images[0]) then
      Exit;

    // Initialize decoder state
    FillChar(Index, SizeOf(Index), 0);
    PrevPixel.Color := pcBlack;  // Start with opaque black
    Pixel := PrevPixel;

    DestPtr := Bits;
    NumPixels := NativeInt(Header.Width) * Header.Height;
    PixelIndex := 0;
    RunLength := 0;

    while PixelIndex < NumPixels do
    begin
      if RunLength > 0 then // Handle pending run
      begin
        Dec(RunLength);
      end
      else // Read next tag/opcode
      begin
        B1 := Stream.ReadByte;

        if B1 = QOI_OP_RGB then
        begin
          Pixel.R := Stream.ReadByte;
          Pixel.G := Stream.ReadByte;
          Pixel.B := Stream.ReadByte;
          // Alpha remains the same as previous pixel
        end
        else if B1 = QOI_OP_RGBA then
        begin
          Pixel.R := Stream.ReadByte;
          Pixel.G := Stream.ReadByte;
          Pixel.B := Stream.ReadByte;
          Pixel.A := Stream.ReadByte;
        end
        else if (B1 and QoiMask2Tag) = QOI_OP_INDEX then
        begin
          Pixel := Index[B1 and QoiMask2Data];
        end
        else if (B1 and QoiMask2Tag) = QOI_OP_DIFF then
        begin
          DR := ((B1 shr 4) and $03) - 2; // dr = ((B1 >> 4) & 0x03) - 2;
          DG := ((B1 shr 2) and $03) - 2; // dg = ((B1 >> 2) & 0x03) - 2;
          DB := ( B1        and $03) - 2; // db = ( B1       & 0x03) - 2;

          // QOI_OP_DIFF can be the first OP in the file and then
          // DR/DG/DB will be -1 => valid case producing white pixel.
          // So we cannot clamp to byte range here when assigning channels of Pixel
          // or just do nothing like C implementation does (Pascal's range check error).
          // We take lowest 8 bits of the -1 value, producing a 255 byte,
          // alternative to e.g. B:=Byte(Cardinal(-1)).

          Pixel.R := (PrevPixel.R + DR) and $FF;
          Pixel.G := (PrevPixel.G + DG) and $FF;
          Pixel.B := (PrevPixel.B + DB) and $FF;
          // Alpha remains the same
        end
        else if (B1 and QoiMask2Tag) = QOI_OP_LUMA then
        begin
          B2 := Stream.ReadByte;

          DG := (B1 and QoiMask2Data) - 32;    // dg = (B1 & 0x3f) - 32;
          DR := DG + ((B2 shr 4) and $0F) - 8; // dr_dg = (B2 >> 4) & 0x0f; dr = dr_dg - 8 + dg;
          DB := DG + ( B2        and $0F) - 8; // db_dg = (B2     ) & 0x0f; db = db_dg - 8 + dg;
          Pixel.R := (PrevPixel.R + DR) and $FF;
          Pixel.G := (PrevPixel.G + DG) and $FF;
          Pixel.B := (PrevPixel.B + DB) and $FF;
          // Alpha remains the same
        end
        else if (B1 and QoiMask2Tag) = QOI_OP_RUN then
        begin
          RunLength := (B1 and QoiMask2Data); // run = (B1 & 0x3f)
          // Pixel value remains the same as previous
        end;
      end; // end read next tag

      Index[QoiColorHash(Pixel)] := Pixel; // Update running index

      if Format = ifA8R8G8B8 then
      begin
        PColor32Rec(DestPtr)^ := Pixel;
        Inc(DestPtr, 4);
      end
      else
      begin
        PColor24Rec(DestPtr)^ := Pixel.Color24Rec;
        Inc(DestPtr, 3);
      end;

      PrevPixel := Pixel;
      Inc(PixelIndex);
    end; // while PixelIndex < PixelsTotal

    Stream.Seek(QoiPaddingSize, soFromCurrent);

    Result := (PixelIndex = NumPixels); // Check if all pixels were decoded
  finally
    Stream.Free;
  end;
end;

function TQoiFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  ImageToSave: TImageData;
  Stream: TImagingIOStream;
  MustBeFreed: Boolean;
  Header: TQoiHeader;
  RunLength: Integer;
  NumPixels, PixelIndex: NativeInt;
  ColorsIndex: array[0..63] of TColor32Rec; // Running index of colors
  Pixel, PrevPixel: TColor32Rec;
  SrcPtr: PByte;
  HashIndex: Byte;
  DR, DG, DB: Integer;
  DR_DG, DB_DG : Integer;
  B1, B2: Byte;
begin
  Result := False;
  MustBeFreed := False;

  // Make image compatible (ifR8G8B8 or ifA8R8G8B8)
  if not MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
    Exit;

  Stream := TImagingIOStream.Create(GetIO, Handle);

  try
    // Prepare Header
    FillChar(Header, SizeOf(Header), 0);
    Header.Magic := QoiMagic;
    Header.Width := ImageToSave.Width;
    Header.Height := ImageToSave.Height;

    if ImageToSave.Format = ifA8R8G8B8 then
      Header.Channels := 4
    else // ifR8G8B8
      Header.Channels := 3;
    Header.Colorspace := 0; // sRGB (linear alpha assumed by spec)

    SwapQoiHeader(Header); // Convert to Big Endian for file
    Stream.WriteBuffer(Header, SizeOf(Header));

    // Initialize encoder state
    FillChar(ColorsIndex, SizeOf(ColorsIndex), 0);
    PrevPixel.Color := pcBlack;
    Pixel := PrevPixel;
    RunLength := 0;

    SrcPtr := ImageToSave.Bits;
    NumPixels := NativeInt(ImageToSave.Width) * ImageToSave.Height;

    for PixelIndex := 0 to NumPixels - 1 do
    begin
      // Read pixel from source TImageData
      if Header.Channels = 4 then
      begin
        Pixel := PColor32Rec(SrcPtr)^;
        Inc(SrcPtr, 4);
      end
      else // Channels = 3
      begin
        Pixel.Color24Rec := PColor24Rec(SrcPtr)^;
        Pixel.A := PrevPixel.A; // Keep previous alpha for RGB format
        Inc(SrcPtr, 3);
      end;

      // Check for run
      if (Pixel.R = PrevPixel.R) and (Pixel.G = PrevPixel.G) and
         (Pixel.B = PrevPixel.B) and (Pixel.A = PrevPixel.A) then
      begin
        Inc(RunLength);

        if (RunLength = 62) or (PixelIndex = (NumPixels - 1)) then
        begin
          // Max run length or EOF, write QOI_OP_RUN
          B1 := QOI_OP_RUN or (RunLength - 1);
          Stream.WriteByte(B1);
          RunLength := 0;
        end;
      end
      else // Not a run, or run ended
      begin
        // Write any pending run first
        if RunLength > 0 then
        begin
          B1 := QOI_OP_RUN or (RunLength - 1);
          Stream.WriteByte(B1);
          RunLength := 0;
        end;

        // Try other encodings
        HashIndex := QoiColorHash(Pixel);

        if (ColorsIndex[HashIndex].R = Pixel.R) and (ColorsIndex[HashIndex].G = Pixel.G) and
           (ColorsIndex[HashIndex].B = Pixel.B) and (ColorsIndex[HashIndex].A = Pixel.A) then
        begin
          // QOI_OP_INDEX
          B1 := QOI_OP_INDEX or HashIndex;
          Stream.WriteByte(B1);
        end
        else // Not in index, try diff/luma/rgb(a)
        begin
          ColorsIndex[HashIndex] := Pixel; // Update index for next time

          if Pixel.A = PrevPixel.A then // Alpha hasn't changed, try diff/luma
          begin
            DR := Pixel.R - PrevPixel.R;
            DG := Pixel.G - PrevPixel.G;
            DB := Pixel.B - PrevPixel.B;

            DR_DG := DR - DG;
            DB_DG := DB - DG;

            if (DR >= -2) and (DR <= 1) and (DG >= -2) and (DG <= 1) and (DB >= -2) and (DB <= 1) then
            begin
              // QOI_OP_DIFF
              B1 := QOI_OP_DIFF or ((DR + 2) shl 4) or ((DG + 2) shl 2) or (DB + 2);
              Stream.WriteByte(B1);
            end
            else if (DR_DG >= -8) and (DR_DG <= 7) and (DG >= -32) and (DG <= 31) and (DB_DG >= -8) and (DB_DG <= 7) then
            begin
              // QOI_OP_LUMA
              B1 := QOI_OP_LUMA or (DG + 32);
              B2 := ((DR_DG + 8) shl 4) or (DB_DG + 8);
              Stream.WriteByte(B1);
              Stream.WriteByte(B2);
            end
            else // Can't use diff or luma, use RGB
            begin
              B1 := QOI_OP_RGB;
              Stream.WriteByte(B1);
              Stream.WriteByte(Pixel.R);
              Stream.WriteByte(Pixel.G);
              Stream.WriteByte(Pixel.B);
            end;
          end
          else // Alpha changed, use RGBA
          begin
            B1 := QOI_OP_RGBA;
            Stream.WriteByte(B1);
            Stream.WriteByte(Pixel.R);
            Stream.WriteByte(Pixel.G);
            Stream.WriteByte(Pixel.B);
            Stream.WriteByte(Pixel.A);
          end;
        end; // end not in index
      end; // end not a run

      PrevPixel := Pixel;
    end; // for PixelIndex

    // Write the final padding
    Stream.WriteBuffer(QoiPadding, SizeOf(QoiPadding));

    Result := True;
  finally
    Stream.Free;
    if MustBeFreed then
      FreeImage(ImageToSave);
  end;
end;

procedure TQoiFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  ConvFormat: TImageFormat;
begin
  // QOI supports RGB and RGBA. Convert other formats appropriately.
  if Info.HasAlphaChannel or Info.IsIndexed then // Indexed might have transparency
    ConvFormat := ifA8R8G8B8
  else
    ConvFormat := ifR8G8B8;

  ConvertImage(Image, ConvFormat);
end;

initialization
  RegisterImageFileFormat(TQoiFileFormat);
end.
