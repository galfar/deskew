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

{ This unit contains image format loader/saver for Targa images.}
unit ImagingTarga;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging, ImagingFormats, ImagingUtility;

type
  { Class for loading and saving Truevision Targa images.
    It can load/save 8bit indexed or grayscale, 16 bit RGB or grayscale,
    24 bit RGB and 32 bit ARGB images with or without RLE compression.}
  TTargaFileFormat = class(TImageFileFormat)
  protected
    FUseRLE: LongBool;
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  published
    { Controls that RLE compression is used during saving. Accessible trough
      ImagingTargaRLE option.}
    property UseRLE: LongBool read FUseRLE write FUseRLE;
  end;

implementation

const
  STargaFormatName = 'Truevision Targa Image';
  STargaMasks      = '*.tga';
  TargaSupportedFormats: TImageFormats = [ifIndex8, ifGray8, ifA1R5G5B5,
    ifR8G8B8, ifA8R8G8B8];
  TargaDefaultRLE = False;

const
  STargaSignature = 'TRUEVISION-XFILE';
  // RLE data are read and written in blocks of this size
  RleChunkSize = 1024 * 1024;
  // Largest width and height a header can hold
  TargaMaxDimension = 65535;

type
  { Targa file header }
  TTargaHeader = packed record
    IDLength: Byte;
    ColorMapType: Byte;
    ImageType: Byte;
    ColorMapOff: Word;
    ColorMapLength: Word;
    ColorEntrySize: Byte;
    XOrg: SmallInt;
    YOrg: SmallInt;
    Width: UInt16;
    Height: UInt16;
    PixelSize: Byte;
    Desc: Byte;
  end;

  { Footer at the end of TGA file }
  TTargaFooter = packed record
    ExtOff: UInt32;               // Extension Area Offset
    DevDirOff: UInt32;            // Developer Directory Offset
    Signature: TChar16;           // TRUEVISION-XFILE
    Reserved: Byte;               // ASCII period '.'
    NullChar: Byte;               // 0
  end;


{ TTargaFileFormat class implementation }

procedure TTargaFileFormat.Define;
begin
  inherited;
  FName := STargaFormatName;
  FFeatures := [ffLoad, ffSave, ffHugeImages];
  FSupportedFormats := TargaSupportedFormats;

  FUseRLE := TargaDefaultRLE;

  AddMasks(STargaMasks);
  RegisterOption(ImagingTargaRLE, @FUseRLE);
end;

function TTargaFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Hdr: TTargaHeader;
  Foo: TTargaFooter;
  FooterFound, ExtFound: Boolean;
  I, PSize, PalSize: Integer;
  Pal: TDynByteArray;
  FmtInfo: TImageFormatInfo;
  WordValue: Word;

  procedure LoadRLE;
  var
    Buffer: TDynByteArray;
    BufLen, BufPos, PixelsLeft: NativeInt;
    EndOfInput: Boolean;
    Bpp, MaxPacketSize, PacketSize, Count, I: Integer;
    PacketHeader: Byte;
    Src, Dest: PByte;

    procedure FillBuffer;
    var
      Rest, ReadCount: NativeInt;
    begin
      // Unread bytes are moved to the start, the rest is read from input
      Rest := BufLen - BufPos;
      if Rest > 0 then
        Move(Buffer[BufPos], Buffer[0], Rest);

      ReadCount := GetIO.Read(Handle, @Buffer[Rest], RleChunkSize - Rest);
      if ReadCount < 0 then
        ReadCount := 0;

      EndOfInput := ReadCount < RleChunkSize - Rest;
      BufLen := Rest + ReadCount;
      BufPos := 0;
    end;

  begin
    // Compressed data are read in blocks, only whole packets are decoded.
    // Pixels after the end of truncated data are left zero.
    Bpp := FmtInfo.BytesPerPixel;
    MaxPacketSize := 1 + 128 * Bpp;
    SetLength(Buffer, RleChunkSize);
    BufLen := 0;
    BufPos := 0;
    EndOfInput := False;

    Dest := Images[0].Bits;
    PixelsLeft := GetPixelCount(Images[0]);

    while PixelsLeft > 0 do
    begin
      if (BufLen - BufPos < MaxPacketSize) and not EndOfInput then
        FillBuffer;

      if BufPos >= BufLen then
        Break;

      PacketHeader := Buffer[BufPos];
      Count := PacketHeader and $7F + 1;

      if PacketHeader and $80 <> 0 then
        PacketSize := 1 + Bpp
      else
        PacketSize := 1 + Count * Bpp;

      // Packet is cut by the end of input
      if BufPos + PacketSize > BufLen then
        Break;

      // Packet must not write past the end of the image
      if Count > PixelsLeft then
        Count := PixelsLeft;

      Src := @Buffer[BufPos + 1];

      if PacketHeader and $80 <> 0 then
      begin
        // Run of one repeated pixel
        for I := 0 to Count - 1 do
        begin
          case Bpp of
            1: Dest^ := Src^;
            2: PWord(Dest)^ := PWord(Src)^;
            3: PColor24Rec(Dest)^ := PColor24Rec(Src)^;
            4: PUInt32(Dest)^ := PUInt32(Src)^;
          end;
          Inc(Dest, Bpp);
        end;
      end
      else
      begin
        // Raw pixels
        Move(Src^, Dest^, Count * Bpp);
        Inc(Dest, Count * Bpp);
      end;

      Inc(BufPos, PacketSize);
      Dec(PixelsLeft, Count);
    end;

    // Set position in input to the end of compressed data
    if BufLen > BufPos then
      GetIO.Seek(Handle, -(BufLen - BufPos), smFromCurrent);
  end;

begin
  SetLength(Images, 1);
  with GetIO, Images[0] do
  begin
    // Read targa header
    Read(Handle, @Hdr, SizeOf(Hdr));
    // Skip image ID info
    Seek(Handle, Hdr.IDLength, smFromCurrent);
    // Determine image format
    Format := ifUnknown;
    case Hdr.ImageType of
      1, 9: Format := ifIndex8;
      2, 10: case Hdr.PixelSize of
          15: Format := ifX1R5G5B5;
          16: Format := ifA1R5G5B5;
          24: Format := ifR8G8B8;
          32: Format := ifA8R8G8B8;
        end;
      3, 11: Format := ifGray8;
    end;
    // Format was not assigned by previous testing (it should be in
    // well formed Targas), so format which reflect the bit depth is selected
    if Format = ifUnknown then
      case Hdr.PixelSize of
        8: Format := ifGray8;
        15: Format := ifX1R5G5B5;
        16: Format := ifA1R5G5B5;
        24: Format := ifR8G8B8;
        32: Format := ifA8R8G8B8;
      end;
    NewImage(Hdr.Width, Hdr.Height, Format, Images[0]);
    FmtInfo := GetFormatInfo(Format);

    if (Hdr.ColorMapType = 1) and (Hdr.ImageType in [1, 9]) then
    begin
      // Read palette
      PSize := Hdr.ColorMapLength * (Hdr.ColorEntrySize shr 3);
      SetLength(Pal, PSize);
      Read(Handle, Pointer(Pal), PSize);
      // Process palette
      PalSize := Iff(Hdr.ColorMapLength > FmtInfo.PaletteEntries,
        FmtInfo.PaletteEntries, Hdr.ColorMapLength);

      for I := 0 to PalSize - 1 do
        case Hdr.ColorEntrySize of
          24:
            with Palette[I] do
            begin
              A := $FF;
              R := PPalette24(Pal)[I].R;
              G := PPalette24(Pal)[I].G;
              B := PPalette24(Pal)[I].B;
            end;
          // I've never seen TGA with these palettes so they are untested
          16:
            with Palette[I] do
            begin
              A := (PWordArray(Pal)[I] and $8000) shr 12;
              R := (PWordArray(Pal)[I] and $FC00) shr 7;
              G := (PWordArray(Pal)[I] and $03E0) shr 2;
              B := (PWordArray(Pal)[I] and $001F) shl 3;
            end;
          32:
            with Palette[I] do
            begin
              A := PPalette32(Pal)[I].A;
              R := PPalette32(Pal)[I].R;
              G := PPalette32(Pal)[I].G;
              B := PPalette32(Pal)[I].B;
            end;
        end;
    end;

    case Hdr.ImageType of
      0, 1, 2, 3:
        // Load uncompressed mode images
        Read(Handle, Bits, Size);
      9, 10, 11:
        // Load RLE compressed mode images
        LoadRLE;
    end;

    // Check if there is alpha channel present in A1R5GB5 images, if it is not
    // change format to X1R5G5B5
    if Format = ifA1R5G5B5 then
    begin
      if not Has16BitImageAlpha(GetPixelCount(Images[0]), Bits) then
        Format := ifX1R5G5B5;
    end;

    // We must find true end of file and set input's position to it.
    // Some paint programs appends extra info at the end of Targas,
    // some of them multiple times (PSP Pro 8)
    repeat
      ExtFound := False;
      FooterFound := False;

      if Read(Handle, @WordValue, 2) = 2 then
      begin
        // 495 = size of Extension Area
        if WordValue = 495 then
        begin
          Seek(Handle, 493, smFromCurrent);
          ExtFound := True;
        end
        else
          Seek(Handle, -2, smFromCurrent);
      end;

      if Read(Handle, @Foo, SizeOf(Foo)) = SizeOf(Foo) then
      begin
        if Foo.Signature = STargaSignature then
          FooterFound := True
        else
          Seek(Handle, -SizeOf(Foo), smFromCurrent);
      end;
    until (not ExtFound) and (not FooterFound);

    // Some editors save Targas flipped
    if Hdr.Desc < 31 then
      FlipImage(Images[0]);

    Result := True;
  end;
end;

function TTargaFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  I: LongInt;
  Hdr: TTargaHeader;
  FmtInfo: TImageFormatInfo;
  Pal: PPalette24;
  ImageToSave: TImageData;
  MustBeFreed: Boolean;

  procedure SaveRLE;
  var
    Buffer: TDynByteArray;
    BufLen: NativeInt;
    RowMaxSize, Written, Y: Integer;

    function CountDiff(Data: PByte; Bpp, PixelCount: Longint): LongInt;
    var
      Pixel: UInt32;
      NextPixel: UInt32;
      N: LongInt;
    begin
      N := 0;
      Pixel := 0;
      NextPixel := 0;
      if PixelCount = 1 then
      begin
        Result := PixelCount;
        Exit;
      end;
      case Bpp of
        1: Pixel := Data^;
        2: Pixel := PWord(Data)^;
        3: PColor24Rec(@Pixel)^ := PColor24Rec(Data)^;
        4: Pixel := PUInt32(Data)^;
      end;
      while PixelCount > 1 do
      begin
        Inc(Data, Bpp);
        case Bpp of
          1: NextPixel := Data^;
          2: NextPixel := PWord(Data)^;
          3: PColor24Rec(@NextPixel)^ := PColor24Rec(Data)^;
          4: NextPixel := PUInt32(Data)^;
        end;
        if NextPixel = Pixel then
          Break;
        Pixel := NextPixel;
        N := N + 1;
        PixelCount := PixelCount - 1;
      end;
      if NextPixel = Pixel then
        Result := N
      else
        Result := N + 1;
    end;

    function CountSame(Data: PByte; Bpp, PixelCount: LongInt): LongInt;
    var
      Pixel: UInt32;
      NextPixel: UInt32;
      N: LongInt;
    begin
      N := 1;
      Pixel := 0;
      NextPixel := 0;
      case Bpp of
        1: Pixel := Data^;
        2: Pixel := PWord(Data)^;
        3: PColor24Rec(@Pixel)^ := PColor24Rec(Data)^;
        4: Pixel := PUInt32(Data)^;
      end;
      PixelCount := PixelCount - 1;
      while PixelCount > 0 do
      begin
        Inc(Data, Bpp);
        case Bpp of
          1: NextPixel := Data^;
          2: NextPixel := PWord(Data)^;
          3: PColor24Rec(@NextPixel)^ := PColor24Rec(Data)^;
          4: NextPixel := PUInt32(Data)^;
        end;
        if NextPixel <> Pixel then
          Break;
        N := N + 1;
        PixelCount := PixelCount - 1;
      end;
      Result := N;
    end;

    procedure RleCompressLine(Data: PByte; PixelCount, Bpp: LongInt; Dest:
      PByte; out Written: LongInt);
    const
      MaxRun = 128;
    var
      DiffCount: LongInt;
      SameCount: LongInt;
      RleBufSize: LongInt;
    begin
      RleBufSize := 0;
      while PixelCount > 0 do
      begin
        // Only pixels that can get into one packet are looked at (one more
        // for CountDiff, it ends a run of different pixels at a repeated one)
        DiffCount := CountDiff(Data, Bpp, Min(PixelCount, MaxRun + 1));
        SameCount := CountSame(Data, Bpp, Min(PixelCount, MaxRun));

        if (DiffCount > MaxRun) then
          DiffCount := MaxRun;
        if (SameCount > MaxRun) then
          SameCount := MaxRun;

        if (DiffCount > 0) then
        begin
          Dest^ := Byte(DiffCount - 1);
          Inc(Dest);
          PixelCount := PixelCount - DiffCount;
          RleBufSize := RleBufSize + (DiffCount * Bpp) + 1;
          Move(Data^, Dest^, DiffCount * Bpp);
          Inc(Data, DiffCount * Bpp);
          Inc(Dest, DiffCount * Bpp);
        end;
        if SameCount > 1 then
        begin
          Dest^ := Byte((SameCount - 1) or $80);
          Inc(Dest);
          PixelCount := PixelCount - SameCount;
          RleBufSize := RleBufSize + Bpp + 1;
          Inc(Data, (SameCount - 1) * Bpp);
          case Bpp of
            1: Dest^ := Data^;
            2: PWord(Dest)^ := PWord(Data)^;
            3: PColor24Rec(Dest)^ := PColor24Rec(Data)^;
            4: PUInt32(Dest)^ := PUInt32(Data)^;
          end;
          Inc(Data, Bpp);
          Inc(Dest, Bpp);
        end;
      end;
      Written := RleBufSize;
    end;

  begin
    with ImageToSave do
    begin
      // Rows are compressed into a buffer which is written out when the next
      // row might not fit. Compressed row is at most one packet header per
      // pixel larger than the raw one.
      RowMaxSize := Width * (FmtInfo.BytesPerPixel + 1);
      SetLength(Buffer, Max(RleChunkSize, RowMaxSize));
      BufLen := 0;

      for Y := 0 to Height - 1 do
      begin
        if BufLen + RowMaxSize > Length(Buffer) then
        begin
          GetIO.Write(Handle, Pointer(Buffer), BufLen);
          BufLen := 0;
        end;

        RleCompressLine(GetScanlinePointer(ImageToSave, Y, FmtInfo.BytesPerPixel), Width,
                        FmtInfo.BytesPerPixel, @Buffer[BufLen], Written);
        Inc(BufLen, Written);
      end;

      GetIO.Write(Handle, Pointer(Buffer), BufLen);
    end;
  end;

begin
  Result := False;
  // Dimensions are stored in 16 bits
  if (Images[Index].Width > TargaMaxDimension) or (Images[Index].Height > TargaMaxDimension) then
    Exit;

  if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  with GetIO, ImageToSave do
  try
    FmtInfo := GetFormatInfo(Format);
    // Fill targa header
    FillChar(Hdr, SizeOf(Hdr), 0);
    Hdr.IDLength := 0;
    Hdr.ColorMapType := Iff(FmtInfo.PaletteEntries > 0, 1, 0);
    Hdr.Width := Width;
    Hdr.Height := Height;
    Hdr.PixelSize := FmtInfo.BytesPerPixel * 8;
    Hdr.ColorMapLength := FmtInfo.PaletteEntries;
    Hdr.ColorEntrySize := Iff(FmtInfo.PaletteEntries > 0, 24, 0);
    Hdr.ColorMapOff := 0;
    // This indicates that targa is stored in top-left format
    // as our images -> no flipping is needed.
    Hdr.Desc := 32;
    // Set alpha channel size in descriptor (mostly ignored by other software though)
    if Format = ifA8R8G8B8 then
      Hdr.Desc := Hdr.Desc or 8
    else if Format = ifA1R5G5B5 then
      Hdr.Desc := Hdr.Desc or 1;

    // Choose image type
    if FmtInfo.IsIndexed then
      Hdr.ImageType := Iff(FUseRLE, 9, 1)
    else if FmtInfo.HasGrayChannel then
      Hdr.ImageType := Iff(FUseRLE, 11, 3)
    else
      Hdr.ImageType := Iff(FUseRLE, 10, 2);

    Write(Handle, @Hdr, SizeOf(Hdr));

    // Write palette
    if FmtInfo.PaletteEntries > 0 then
    begin
      GetMem(Pal, FmtInfo.PaletteEntries * SizeOf(TColor24Rec));
      try
        for I := 0 to FmtInfo.PaletteEntries - 1 do
          with Pal[I] do
          begin
            R := Palette[I].R;
            G := Palette[I].G;
            B := Palette[I].B;
          end;
        Write(Handle, Pal, FmtInfo.PaletteEntries * SizeOf(TColor24Rec));
      finally
        FreeMemNil(Pal);
      end;
    end;

    if FUseRLE then
      // Save RLE compressed mode images
      SaveRLE
    else
      // Save uncompressed mode images
      Write(Handle, Bits, Size);

    Result := True;
  finally
    if MustBeFreed then
      FreeImage(ImageToSave);
  end;
end;

procedure TTargaFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  ConvFormat: TImageFormat;
begin
  if Info.HasGrayChannel then
    // Convert all grayscale images to Gray8 (preserve alpha of AxGrayx formats)
    ConvFormat := IffFormat(not Info.HasAlphaChannel, ifGray8, ifA8R8G8B8)
  else if Info.IsIndexed then
    // Convert all indexed images to Index8
    ConvFormat := ifIndex8
  else if Info.HasAlphaChannel then
    // Convert images with alpha channel to A8R8G8B8
    ConvFormat := ifA8R8G8B8
  else if Info.UsePixelFormat then
    // Convert 16bit images (without alpha channel) to A1R5G5B5
    ConvFormat := ifA1R5G5B5
  else
    // Convert all other formats to R8G8B8
    ConvFormat := ifR8G8B8;

  ConvertImage(Image, ConvFormat);
end;

function TTargaFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Hdr: TTargaHeader;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Hdr, SizeOf(Hdr));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount >= SizeOf(Hdr)) and
      (Hdr.ImageType in [0, 1, 2, 3, 9, 10, 11]) and
      (Hdr.PixelSize in [1, 8, 15, 16, 24, 32]) and
      (Hdr.ColorEntrySize in [0, 16, 24, 32]);
  end;
end;

initialization
  RegisterImageFileFormat(TTargaFileFormat);

{
  File Notes:

  * More recent changes are in VCS history *

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - MakeCompatible method moved to base class, put ConvertToSupported here.
      GetSupportedFormats removed, it is now set in constructor.
    - Made public properties for options registered to SetOption/GetOption
      functions.
    - Changed extensions to filename masks.
    - Changed SaveData, LoadData, and MakeCompatible methods according
      to changes in base class in Imaging unit.

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - 16 bit images are usually without alpha but some has alpha
      channel and there is no indication of it - so I have added
      a check: if all pixels of image are with alpha = 0 image is treated
      as X1R5G5B5 otherwise as A1R5G5B5
    - fixed problems with some nonstandard 15 bit images
}

end.

