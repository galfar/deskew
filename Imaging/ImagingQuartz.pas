unit ImagingQuartz;

{$I ImagingOptions.inc}

{$IFNDEF MACOSX}
  {$FATAL 'Mac OSX only'}
{$ENDIF}

interface

uses
  Types, SysUtils, Classes, ImagingTypes, Imaging, ImagingUtility,
{$IFDEF DCC}
  Macapi.CocoaTypes, Macapi.CoreFoundation, Macapi.CoreGraphics, Macapi.QuartzCore, Macapi.ImageIO
{$ELSE}
  CGBase, CGShading, CGColor, CGColorSpace, CGContext, CGBitmapContext, CGImageSource,
  CGImageDestination, CGDataProvider, CGDataConsumer, CFDictionary, CGAffineTransforms, CGPath
{$ENDIF};

type
  TQuartzImageHandler = class
  private
    FDefaultColorSpace: CGColorSpaceRef;
    function FindImageFormat(ImgRef: CGImageRef): TImageFormat;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadImage(Stream: TCustomMemoryStream; var Images: TDynImageDataArray; OnlyFirstImage: Boolean): Boolean;
  end;

implementation

function CGRectFromRect(const R: TRect): CGRect;
begin
  Result.origin.x := R.Left;
  Result.origin.Y := R.Top;
  Result.size.Width := R.Right - R.Left;
  Result.size.Height := R.Bottom - R.Top;
end;

{ TMacImageHandler }

constructor TQuartzImageHandler.Create;
begin
  FDefaultColorSpace := CGColorSpaceCreateDeviceRGB;
end;

destructor TQuartzImageHandler.Destroy;
begin
  CGColorSpaceRelease(FDefaultColorSpace);
  inherited;
end;

function TQuartzImageHandler.FindImageFormat(ImgRef: CGImageRef): TImageFormat;
var
  ColorSpaceRef: CGColorSpaceRef;
  ColorModel: CGColorSpaceModel;
  AlphaInfo: CGImageAlphaInfo;
  BitmapInfo: CGBitmapInfo;
  BitsPerPixel, Components, BitsPerComponent: Integer;

  isMask: integer;
  intent:  CGColorRenderingIntent;
begin
  Result := ifUnknown;
  AlphaInfo := CGImageGetAlphaInfo(ImgRef);
  BitmapInfo := CGImageGetBitmapInfo(ImgRef);
  ColorSpaceRef := CGImageGetColorSpace(ImgRef);

  intent := CGImageGetRenderingIntent(ImgRef);
  isMask := CGImageIsMask(ImgRef);

{
  Also check   BitmapInfo
  kCGBitmapByteOrderDefault  = NO
  kCGBitmapByteOrder16Little = NO
  kCGBitmapByteOrder32Little = NO
  kCGBitmapByteOrder16Big    = NO
  kCGBitmapByteOrder32Big    = NO

  float formats
  }

  if ColorSpaceRef <> nil then
  begin
    try
      BitsPerPixel := CGImageGetBitsPerPixel(ImgRef);
      BitsPerComponent := CGImageGetBitsPerComponent(ImgRef);
      ColorModel := CGColorSpaceGetModel(ColorSpaceRef);
      Components := CGColorSpaceGetNumberOfComponents(ColorSpaceRef);

      if (ColorModel = kCGColorSpaceModelMonochrome) and (Components = 1) then
      begin
        // Grayscale formats
        if AlphaInfo = kCGImageAlphaFirst then
        begin
          if (BitsPerComponent = 8) and (BitsPerPixel = 16) then
            Result := ifA8Gray8
          else if (BitsPerComponent = 16) and (BitsPerPixel = 32) then
            Result := ifA16Gray16;
        end
        else if AlphaInfo = kCGImageAlphaNone then
        begin
          if BitsPerPixel = 8 then
            Result := ifGray8
          else if BitsPerPixel = 16 then
            Result := ifGray16;
        end;
      end
      else if ColorModel = kCGColorSpaceModelRGB then
      begin
        // RGB
        if (BitsPerPixel = 16) and (AlphaInfo = kCGImageAlphaNoneSkipFirst) then
        begin
          Result := ifX1R5G5B5;
        end
        else if AlphaInfo = kCGImageAlphaFirst then
        begin
          if (BitsPerComponent = 8) and (BitsPerPixel = 32) then
            Result := ifA8R8G8B8
          else if (BitsPerComponent = 16) and (BitsPerPixel = 64) then
            Result := ifA16R16G16B16;
        end
        else if AlphaInfo = kCGImageAlphaNone then
        begin
          if (BitsPerComponent = 8) and (BitsPerPixel = 24) then
            Result := ifR8G8B8
          else if (BitsPerComponent = 16) and (BitsPerPixel = 48) then
            Result := ifR16G16B16;
        end;
      end;
    finally
      CGColorSpaceRelease(ColorSpaceRef);
    end;
  end;
end;

function TQuartzImageHandler.LoadImage(Stream: TCustomMemoryStream; var Images: TDynImageDataArray; OnlyFirstImage: Boolean): Boolean;
var
  Provider: CGDataProviderRef;
  PixelsData: CFDataRef;
  PixelsPtr, DestPtr: PByteArray;
  ImgSourceRef: CGImageSourceRef;
  ImgRef: CGImageRef;
  CtxRef: CGContextRef;
  I, Count, Y: Integer;
  Width, Height, BytesPerRow, WidthBytes: Integer;
  ImgFormat: TImageFormat;
begin
  Result := False;
  Provider := CGDataProviderCreateWithData(nil, Stream.Memory, Stream.Size, nil);
  if Provider <> nil then
  begin
    ImgSourceRef := CGImageSourceCreateWithDataProvider(Provider, nil);
    if ImgSourceRef <> nil then
    begin
      Count := CGImageSourceGetCount(ImgSourceRef);
      if (Count > 1) and OnlyFirstImage then
        Count := 1;
      SetLength(Images, Count);

      for I := 0 to Count - 1 do
      begin
        ImgRef := CGImageSourceCreateImageAtIndex(ImgSourceRef, I, nil);
        if ImgRef <> nil then
        begin
          Width := CGImageGetWidth(ImgRef);
          Height := CGImageGetHeight(ImgRef);
          BytesPerRow := CGImageGetBytesPerRow(ImgRef);
          ImgFormat := FindImageFormat(ImgRef);

          if ImgFormat = ifUnknown then
          begin
            NewImage(Width, Height, ifA8R8G8B8, Images[I]);
            CtxRef := CGBitmapContextCreate(Images[I].Bits, Width, Height, 8,
              Width * 4, FDefaultColorSpace, kCGImageAlphaPremultipliedFirst);
            CGContextDrawImage(CtxRef, CGRectFromRect(Rect(0, 0, Width, Height)), ImgRef);
            CGContextRelease(CtxRef);
          end
          else
          begin
            NewImage(Width, Height, ImgFormat, Images[I]);
            DestPtr := PByteArray(Images[I].Bits);
            WidthBytes := Images[I].Size div Height;

            PixelsData := CGDataProviderCopyData(CGImageGetDataProvider(ImgRef));
            PixelsPtr := PByteArray(CFDataGetBytePtr(PixelsData));

            for Y := 0 to Height - 1 do
            begin
              //
              Move(PixelsPtr[Y * BytesPerRow], DestPtr[Y * WidthBytes], WidthBytes);
            end;
            CFRelease(PixelsData);
          end;

          CGImageRelease(ImgRef);
        end;
      end;
      CFRelease(ImgSourceRef);
    end;
  end;
end;

end.

