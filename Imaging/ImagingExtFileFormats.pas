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

{ This is helper unit that registers all image file formats in Extensions package
  to Imaging core loading and saving functions. Just put this unit in your uses
  clause instead of adding every unit that provides new file format support.
  Also new constants for SetOption/GetOption functions for new file formats
  are located here.}
unit ImagingExtFileFormats;

{$I ImagingOptions.inc}

{ Which file formats are linked is set by DONT_LINK_* and LINK_ALL_FILE_FORMATS
  defines (the latter adds less common formats that are left out by default).
  They are listed in ImagingOptions.inc, set them there, in project options,
  or in ImagingUserOptions.inc.

  Look at "uses" section in this unit's implementation to how the defines
  are used. }

interface

const
  { Those are new options for GetOption/SetOption interface. }

  { Controls JPEG 2000 lossy compression quality. It is number in range 1..100.
    1 means small/ugly file, 100 means large/nice file. Default is 80.}
  ImagingJpeg2000Quality             = 55;
  { Controls whether JPEG 2000 image is saved with full file headers or just
    as code stream. Default value is False (0).}
  ImagingJpeg2000CodeStreamOnly      = 56;
  { Specifies JPEG 2000 image compression type. If True (1), saved JPEG 2000 files
    will be losslessly compressed. Otherwise lossy compression is used.
    Default value is False (0).}
  ImagingJpeg2000LosslessCompression = 57;
  { Specifies JPEG 2000 output scaling. Since JPEG 2000 supports arbitrary Bit Depths,
    the default behaviour is to scale the images up to the next 8^n bit depth.
    This can be disabled by setting this option to False.
    Default value is True. }
  ImagingJpeg2000ScaleOutput = 58;
  { Specifies compression scheme used when saving TIFF images. Supported values
    are 0 (Uncompressed), 1 (LZW), 2 (PackBits RLE), 3 (Deflate - ZLib), 4 (JPEG),
    5 (CCITT Group 4 fax encoding - for binary images only).
    Default is 1 (LZW). Note that not all images can be stored with
    JPEG compression - these images will be saved with default compression if
    JPEG is set.}
  ImagingTiffCompression             = 65;
  { Controls compression quality when selected TIFF compression is Jpeg.
    It is number in range 1..100. 1 means small/ugly file,
    100 means large/nice file. Accessible trough ImagingTiffJpegQuality option.}
  ImagingTiffJpegQuality             = 66;
  { Controls writing of BigTIFF files. Default value is IfNeeded, not much SW can open BigTIFF
    so try to use it only when needed. Values:
    - IfNeeded(0) writes BigTIFF only when the file size for sure exceeds
      the classic TIFF 4 GiB limit (can only assure for uncompressed TIFFs).
    - IfSafer(1) also uses BigTIFF when compression makes the final file size uncertain.
    - Always(2) always writes BigTIFF files. }
  ImagingTiffBigTiffWriteMode        = 67;
  { Controls the horizontal predictor (differences of neighboring samples
    are compressed instead of the samples) of LZW and Deflate compressed
    TIFFs. It makes photos, color scans and elevation data much smaller but
    document scans and images with few colors larger. Values:
    - Auto(0) decides for each image: a sample of its rows (about 256 KB from
      the top, middle and bottom) is compressed with and without it.
    - On(1) always uses it, Off(2) never.
    It is never used for palette images, floating point and 1-bit data.
    Default is Auto. }
  ImagingTiffPredictor               = 68;

  { If enabled image data is saved as layer of PSD file. This is required
    to get proper transparency when opened in Photoshop for images with
    alpha data (will be opened with one layer, RGB color channels, and transparency).
    If you don't need this Photoshop compatibility turn this option off as you'll get
    smaller file (will be opened in PS as background raster with RGBA channels).
    Default value is True (1). }
  ImagingPSDSaveAsLayer              = 70;

  { Quality of lossy compression used when saving WebP images. It is number
    in range 0..100, higher value means larger file with better quality.
    Default value is 80. }
  ImagingWebPQuality                 = 75;
  { Specifies whether WebP images are saved using lossless compression,
    can be 0 or 1. Default value is 0. }
  ImagingWebPLossless                = 76;

implementation

uses
{$IFNDEF DONT_LINK_FILE_FORMATS}
  {$IFNDEF DONT_LINK_TIFF}
    ImagingTiff,
  {$ENDIF}
  {$IFNDEF DONT_LINK_WEBP}
    ImagingWebP,
  {$ENDIF}
  {$IFNDEF DONT_LINK_JPEG2000}
    ImagingJpeg2000,
  {$ENDIF}
  {$IFNDEF DONT_LINK_PSD}
    ImagingPsd,
  {$ENDIF}
  {$IFNDEF DONT_LINK_PCX}
    ImagingPcx,
  {$ENDIF}
  {$IFNDEF DONT_LINK_XPM}
    ImagingXpm,
  {$ENDIF}
  {$IFDEF LINK_ALL_FILE_FORMATS}
    ElderImagery,
  {$ENDIF}
{$ENDIF}
  ImagingTypes;

end.
