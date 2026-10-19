{
  User Options
  =======================

  Following defines and options can be changed by user.
}

(*
  File format support linking options
  ------------------------------------

  Define formats which you don't want to be registered automatically (by adding
  Imaging.pas unit to your uses clause).
  Default: most formats are registered = no symbols defined.
  Example: If you want to disable JPEG support just uncomment //{$DEFINE DONT_LINK_JPEG} line
*)

{.$DEFINE DONT_LINK_JPEG}        // support for JPEG images
{.$DEFINE DONT_LINK_PNG}         // support for PNG images
{.$DEFINE DONT_LINK_TARGA}       // support for TGA images
{.$DEFINE DONT_LINK_BITMAP}      // support for Windows Bitmap images
{.$DEFINE DONT_LINK_DDS}         // support for DDS images
{.$DEFINE DONT_LINK_GIF}         // support for GIF images
{.$DEFINE DONT_LINK_MNG}         // support for MNG images
{.$DEFINE DONT_LINK_JNG}         // support for JNG images
{.$DEFINE DONT_LINK_PNM}         // support for PortableMap images (PBM, PGM, PPM, PAM, PFM)
{.$DEFINE DONT_LINK_RADHDR}      // support for Radiance HDR/RGBE file format
{.$DEFINE DONT_LINK_QOI}         // support for Quite OK Image Format

{.$DEFINE DONT_LINK_EXT_FILE_FORMATS} // support for file formats defined in
                                 // Extensions folder (ImagingExtFileFormats unit).
                                 // Exactly which formats will be registered
                                 // depends on the defines below. The folder
                                 // doesn't have to be in search path when this
                                 // is defined. Was DONT_LINK_EXTRAS before 2026-10.

{ File formats from Extensions folder }

{.$DEFINE DONT_LINK_TIFF}        // support for TIFF images
{.$DEFINE DONT_LINK_WEBP}        // support for WebP images
{.$DEFINE DONT_LINK_JPEG2000}    // support for JPEG2000 images
{.$DEFINE DONT_LINK_PSD}         // support for PSD images
{.$DEFINE DONT_LINK_PCX}         // support for PCX images
{.$DEFINE DONT_LINK_XPM}         // support for XPM images

{.$DEFINE LINK_ALL_FILE_FORMATS} // opt-in support for less common formats that
                                 // are left out by default.
                                 // DONT_LINK_* defines still apply.
                                 // Was FULL_FEATURE_SET before 2026-10.

{.$DEFINE DONT_LINK_FILE_FORMATS} // no auto link support of any file format

{ Third party libraries
  ------------------------------

  Each library is chosen automatically by its wrapper
  unit when nothing is defined here, see the units for details. 

  Names used before 2026-10 (not recognized anymore):
    IMPASZLIB -> ZLIB_USE_BUNDLED, ZLIB_DYNLIB -> ZLIB_USE_DYNLIB,
    IMPASJPEG -> JPEG_USE_BUNDLED, FPCPASJPEG -> JPEG_USE_FPC,
    WEBP_NO_PASCAL_DECODER -> WEBP_NO_BUNDLED }

// ZLib (Source/Libs/ImagingZLib.pas)
{.$DEFINE ZLIB_USE_BUNDLED}       // use bundled Pascal ZLib (ImPasZLib) instead of
                                  // the one shipped with FPC or Delphi
{.$DEFINE ZLIB_USE_DYNLIB}        // use ZLib shared library (must be present at run time)

// JPEG (Source/Libs/ImagingJpegLib.pas)
{.$DEFINE JPEG_USE_BUNDLED}       // use bundled Pascal JpegLib (ImPasJpeg) (also in
                                  // LCL applications)
{.$DEFINE JPEG_USE_FPC}           // use PasJpeg shipped with FPC (also outside of
                                  // LCL applications)
{.$DEFINE JPEG_USE_TURBO_DYNLIB}  // use TurboJPEG shared library (libjpeg-turbo 2.0+)
                                  // when it is found at run time, Pascal codec otherwise

// TIFF (Extensions/Libs/ImagingTiffBackend.pas)
{.$DEFINE TIFF_USE_LIBTIFF_DYNLIB} // use libtiff 4 shared library when it is found
                                  // at run time, Pascal codec (PasTiff) otherwise.
                                  // Always on for Delphi older than 2009 (no PasTiff)
{.$DEFINE TIFF_NO_BUNDLED}        // leave out PasTiff (libtiff shared library only)

// WebP (Extensions/Libs/ImagingLibWebP.pas)
{.$DEFINE WEBP_NO_DYNLIB}         // never look for libwebp shared library
{.$DEFINE WEBP_NO_BUNDLED}        // leave out Pascal WebP decoder (WebPDec)

{ Source options
  ------------------------------ }

{$DEFINE USE_INLINE}          // Use function inlining for some functions
                              // works in Free Pascal and Delphi 9+.
{$DEFINE USE_ASM}             // If defined, assembler versions of some
                              // functions will be used (only for x86).

{.$DEFINE FORCE_DEBUG}      // Normally, Imaging takes DEBUG/RELEASE compiler options
                            // as defined for project/program.
                            // If needed, debug options can be forced with this define.
                            // If defined, debug info, range/IO/overflow
                            // checking, stack frames, assertions, and
                            // other debugging options will be turned on.

{ Application-specific options
  ------------------------------

  define IMAGING_USER_OPTIONS and put your own
  ImagingUserOptions.inc on the include path (outside the Imaging folders).
  It can (un)define anything from the User Options section above, including
  the third party library selection (see ImagingUserOptions.inc.sample).
  Useful when you cannot modify Imaging code directly.
}
{$IFDEF IMAGING_USER_OPTIONS}
  {$I ImagingUserOptions.inc}
{$ENDIF}

{
  Auto Options
  =======================

  Following options and defines are set automatically and some
  are required for Imaging to compile successfully. Do not change
  anything here if you don't know what you are doing.
}

{ Compiler options }

{$ALIGN ON}               // Field alignment: 8 Bytes (in D6+)
{$BOOLEVAL OFF}           // Boolean eval: off
{$EXTENDEDSYNTAX ON}      // Extended syntax: on
{$LONGSTRINGS ON}         // string = AnsiString: on
{$MINENUMSIZE 1}          // Min enum size: 1 B
{$TYPEDADDRESS OFF}       // Typed pointers: off
{$WRITEABLECONST OFF}     // Writeable constants: off

{$IFNDEF FPC}
  {$DEFINE DCC}           // if not using FPC then DCC compiler is used (Delphi/BCB)
                          // others are not supported
{$ENDIF}

{$IFDEF DCC}
  {$DEFINE DELPHI}
  {$IF (Defined(DCC) and (CompilerVersion >= 25.0))}
    {$LEGACYIFEND ON}
  {$IFEND}
{$ENDIF}

{$IF (Defined(DCC) and (CompilerVersion >= 18.5))}
  {$IFDEF RELEASE}
    {$UNDEF DEBUG} // If we are using Delphi 2007+ where you can set
                   // DEBUG/RELEASE mode in project options and RELEASE
                   // is currently set we undef DEBUG mode
  {$ENDIF}
{$IFEND}

{$IFDEF FORCE_DEBUG}
  {$DEFINE DEBUG}
  {$ASSERTIONS ON}
  {$DEBUGINFO ON}
  {$RANGECHECKS ON}
  {$IOCHECKS ON}
  {$OVERFLOWCHECKS ON}
  {$IFDEF DCC}
    {$OPTIMIZATION OFF}
    {$STACKFRAMES ON}
    {$LOCALSYMBOLS ON}
    {$DEFINE MEMCHECK}
  {$ENDIF}
  {$IFDEF FPC}
    {$S+}
  {$ENDIF}
{$ENDIF}

{$IF Defined(CPU86) and not Defined(CPUX86)}
  {$DEFINE CPUX86} // Compatibility with Delphi
{$IFEND}

{$IF Defined(CPUX86_64) and not Defined(CPUX64)}
  {$DEFINE CPUX64} // Compatibility with Delphi
{$IFEND}

{$IF Defined(DARWIN) and not Defined(MACOS)}
  {$DEFINE MACOS} // Compatibility with Delphi
{$IFEND}
{$IF Defined(MACOS)}
  {$DEFINE MACOSX}
{$IFEND}

{$IF Defined(DCC) and (CompilerVersion < 23)}   // < XE2
  {$DEFINE CPUX86} // Compatibility with older Delphi
{$IFEND}

{$IF Defined(WIN32) or Defined(WIN64)}
  {$DEFINE MSWINDOWS} // Compatibility with Delphi
{$IFEND}

{$IF Defined(UNIX) and not Defined(POSIX)}
  {$DEFINE POSIX} // Compatibility with Delphi
{$IFEND}

{ Compiler capabilities }

// Define if compiler supports inlining of functions and procedures
{$IF (Defined(DCC) and (CompilerVersion >= 17)) or Defined(FPC)}
  {$DEFINE HAS_INLINE}
{$IFEND}

// Define if compiler supports pointer math: Delphi 2009+ and FPC
{$IF (Defined(DCC) and (CompilerVersion >= 20.0)) or Defined(FPC)}
  {$DEFINE HAS_POINTER_MATH}
{$IFEND}

// Define if compiler supports advanced records with methods
{$IF (Defined(DCC) and (CompilerVersion >= 18)) or
  (Defined(FPC) and (FPC_FULLVERSION >= 20600))}
  {$DEFINE HAS_ADVANCED_RECORDS}
{$IFEND}

// Define if compiler supports operator overloading
// (unfortunately Delphi and FPC operator overloading is not compatible).
// FPC supports Delphi compatible operator overloads since 2.6.0
{$IF (Defined(DCC) and (CompilerVersion >= 18)) or
  (Defined(FPC) and (FPC_FULLVERSION >= 20600))}
  {$DEFINE HAS_OPERATOR_OVERLOADING}
{$IFEND}

// Anonymous methods
{$IF Defined(DCC) and (CompilerVersion >= 20) }
  {$DEFINE HAS_ANON_METHODS}
{$IFEND}

// Generic types (Delphi and FPC implementations incompatible).
// Update: FPC supports Delphi compatible generics since 2.6.0
{$IF (Defined(DCC) and (CompilerVersion >= 20)) or
  (Defined(FPC) and (FPC_FULLVERSION >= 20600))}
  {$DEFINE HAS_GENERICS}
{$IFEND}

// Dynamic libraries (DLL/SO/dylib) can be loaded at run time.
{$DEFINE HAS_DYNLIBS}
{$IFDEF WASI}
  {$UNDEF HAS_DYNLIBS} // No dynamic libraries on WebAssembly (WASM/WASI)
{$ENDIF}

{ Compiler pecularities }

// Delphi 64bit POSIX targets
{$IF Defined(DCC) and (SizeOf(Integer) <> SizeOf(LongInt))}
  {$DEFINE LONGINT_IS_NOT_INTEGER}
{$IFEND}

// They used to force IFEND, now they warn about it
{$IF Defined(DCC) and (CompilerVersion >= 33)}
  {$LEGACYIFEND ON}
{$IFEND}

{ Imaging options check}

{$IFNDEF HAS_INLINE}
  {$UNDEF USE_INLINE}
{$ENDIF}

{$IF not Defined(CPUX86)}
  {$UNDEF USE_ASM}
{$IFEND}

{$IFDEF FPC}
  {$DEFINE COMPONENT_SET_LCL}
  {$UNDEF COMPONENT_SET_VCL}
{$ENDIF}

{$IFDEF DELPHI}
  {$UNDEF COMPONENT_SET_LCL}
  {$DEFINE COMPONENT_SET_VCL}
{$ENDIF}

{ More compiler options }

{$IFDEF FPC}               // Free Pascal options - some options set above (like min enum size)
                           // are reset to defaults by setting {$MODE} so they are
                           // redeclared here
  {$MODE DELPHI}           // compatible with delphi
  {$GOTO ON}               // alow goto
  {$PACKRECORDS 8}         // same as ALING 8 for Delphi
  {$PACKENUM 4}            // Min enum size: 4 B
  {$IFDEF CPU86}
    {$ASMMODE INTEL}       // intel assembler mode
  {$ENDIF}
{$ENDIF}

{$IFDEF HAS_INLINE}
  {$INLINE ON}             // turns inlining on for compilers that support it
{$ENDIF}

{$IFDEF HAS_POINTER_MATH}
  {$POINTERMATH ON}        // turns pointer math on for compilers that support it
{$ENDIF}

