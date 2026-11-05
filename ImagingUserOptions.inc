{
  Deskew options for Vampyre Imaging Library.
  Included from Imaging/ImagingOptions.inc when IMAGING_USER_OPTIONS is defined
  (project root must be on the include path).
  Imaging/ itself is synced from imaginglib by Scripts/sync_imaging.sh, keep
  Deskew specific settings here.
}

{ Image file formats Deskew does not need }

// Core
{$DEFINE DONT_LINK_DDS}
{$DEFINE DONT_LINK_JNG}
{$DEFINE DONT_LINK_MNG}
{$DEFINE DONT_LINK_RADHDR}

// Ext
{$DEFINE DONT_LINK_JPEG2000}
{$DEFINE DONT_LINK_PCX}
{$DEFINE DONT_LINK_XPM}
{$DEFINE DONT_LINK_PSD}

{ Third party libraries

  Shared libraries are used when they are found at run time, the Pascal codecs
  compiled in are the fallback, so Deskew works without any of them. On targets
  without dynamic libraries (WebAssembly) Imaging ignores these defines.
  DESKEW_PASCAL_CODECS_ONLY (given on the command line) makes a build that never
  looks for them, CI tests the fallbacks with it. }

{$IFNDEF DESKEW_PASCAL_CODECS_ONLY}
  {$DEFINE ZLIB_USE_DYNLIB}         // zlib (zlib1.dll, libz.so.1, libz.1.dylib), also zlib-ng compat
  {$DEFINE JPEG_USE_TURBO_DYNLIB}   // libjpeg-turbo 2.0+ (TurboJPEG API)
  {$DEFINE TIFF_USE_LIBTIFF_DYNLIB} // libtiff 4
{$ELSE}
  {$DEFINE WEBP_NO_DYNLIB}
{$ENDIF}

// libwebp is looked for without a define (WEBP_NO_DYNLIB turns that off), the
// Pascal decoder is the fallback: WebP input works everywhere, WebP output only
// with libwebp. Do not define TIFF_NO_BUNDLED or WEBP_NO_BUNDLED here, they leave
// out the Pascal fallbacks.
