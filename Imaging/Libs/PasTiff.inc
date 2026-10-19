{
  PasTiff - pure Pascal TIFF reader/writer
  Compiler settings shared by all PasTiff units.

  This include is deliberately independent of Imaging's ImagingOptions.inc
  so the PasTiff folder can be used (and later moved) on its own.

  Supported compilers: Free Pascal 3.2.2+ and Delphi 2009+.
  Delphi 7 is not supported (advanced records and POINTERMATH are used).
  Little-endian CPUs only (as Vampyre Imaging Library). TIFF files of both
  byte orders are read; files are written little-endian.

  User defines:
    PASTIFF_NO_DEFLATE - compile without Deflate support (no zlib dependency)
    PASTIFF_NO_JPEG    - compile without JPEG-in-TIFF support (no JPEG library
                         dependency)
}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$MODESWITCH ADVANCEDRECORDS}
  // Version check with plain IFDEFs: Delphi evaluates $IF expressions even
  // inside this block it skips, and does not know FPC_FULLVERSION
  {$IFDEF VER2}
    {$FATAL PasTiff needs Free Pascal 3.2.2 or newer}
  {$ENDIF}
  {$IFDEF VER3_0}
    {$FATAL PasTiff needs Free Pascal 3.2.2 or newer}
  {$ENDIF}
  {$DEFINE PASTIFF_FPC}
  // Byte order of samples and tag values in memory is assumed to be
  // little-endian everywhere; all Delphi targets are
  {$IFDEF ENDIAN_BIG}
    {$FATAL PasTiff supports little-endian CPUs only}
  {$ENDIF}
{$ELSE}
  {$IF CompilerVersion < 20.0}
    {$MESSAGE FATAL 'PasTiff needs Delphi 2009 or newer'}
  {$IFEND}
  {$IF CompilerVersion >= 25.0}
    // Allow closing $IF with $IFEND in XE4+ as in older versions
    {$LEGACYIFEND ON}
  {$IFEND}
  {$DEFINE PASTIFF_DCC}
{$ENDIF}

{$ALIGN ON}
{$BOOLEVAL OFF}
{$EXTENDEDSYNTAX ON}
{$LONGSTRINGS ON}
{$MINENUMSIZE 1}
{$TYPEDADDRESS OFF}
{$WRITEABLECONST OFF}
// Typed pointers can be indexed like arrays: P[I]. Codecs rely on it.
{$POINTERMATH ON}
