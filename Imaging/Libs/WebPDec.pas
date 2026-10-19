unit WebPDec;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Description:	WEBP port                                                     //
// Version:	0.5                                                           //
// Date:	30-MAY-2026                                                   //
// License:     MIT                                                           //
// Target:	Win64, Free Pascal, Delphi                                    //
// Copyright:	(c) 2026 Xelitan.com.                                         //
//		All rights reserved.                                          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}{$inline on}
{$ENDIF}
{$R-}{$Q-}

interface

// All output buffers allocated with GetMem; caller frees with FreeMem.
function WebPGetInfo(Data: PByte; DataSize: NativeUInt;
  out Width, Height: Integer): Boolean;
function WebPDecodeRGBA(Data: PByte; DataSize: NativeUInt;
  out Width, Height: Integer): PByte;
function WebPDecodeARGB(Data: PByte; DataSize: NativeUInt;
  out Width, Height: Integer): PByte;
function WebPDecodeBGRA(Data: PByte; DataSize: NativeUInt;
  out Width, Height: Integer): PByte;
function WebPDecodeRGB(Data: PByte; DataSize: NativeUInt;
  out Width, Height: Integer): PByte;
function WebPDecodeBGR(Data: PByte; DataSize: NativeUInt;
  out Width, Height: Integer): PByte;
procedure WebPFree(ptr: Pointer);

implementation
{$POINTERMATH ON}

// ============================================================
// TYPES
// ============================================================
type
  {$IF NOT DECLARED(PInt16)}
  PInt16  = ^Int16;
  {$IFEND}
  {$IF NOT DECLARED(PUInt32)}
  PUInt32 = ^UInt32;
  {$IFEND}
  TCSMode = (csmRGBA, csmARGB, csmBGRA, csmRGB, csmBGR);

// ============================================================
// CONSTANTS
// ============================================================
const
  BPS      = 32;                    // YUV reconstruction buffer stride
  YUV_SIZE = BPS * 17 + BPS * 9;   // = 832
  Y_OFF    = BPS * 1 + 8;           // = 40
  U_OFF    = Y_OFF + BPS * 16 + BPS * 1;  // = 584
  V_OFF    = U_OFF + 16;            // = 600

  NUM_MB_SEGMENTS       = 4;
  NUM_TYPES             = 4;
  NUM_BANDS             = 8;
  NUM_CTX               = 3;
  NUM_PROBAS            = 11;
  MB_FEATURE_TREE_PROBS = 3;
  NUM_REF_LF_DELTAS     = 4;
  NUM_MODE_LF_DELTAS    = 4;
  MAX_NUM_PARTITIONS    = 8;

  // 4x4 intra block modes
  B_DC_PRED = 0;  B_TM_PRED = 1;  B_VE_PRED = 2;  B_HE_PRED = 3;
  B_RD_PRED = 4;  B_VR_PRED = 5;  B_LD_PRED = 6;  B_VL_PRED = 7;
  B_HD_PRED = 8;  B_HU_PRED = 9;
  NUM_BMODES = 10;

  // 16x16 / UV intra modes — MUST match B_*_PRED values so that
  // I16x16 mode values stored as I4x4 top/left context use correct kBModesProba rows.
  // C: DC_PRED=B_DC_PRED=0, TM_PRED=B_TM_PRED=1, V_PRED=B_VE_PRED=2, H_PRED=B_HE_PRED=3
  DC_PRED = 0;  TM_PRED = 1;  V_PRED = 2;  H_PRED = 3;  B_PRED = 4;

  FIXED_TABLE_SIZE = 630 * 3 + 410;  // = 2300  (VP8L Huffman)

// ============================================================
// VP8 PROBABILITY / QUANTIZATION TABLES
// ============================================================
const
  CoeffsProba0: array[0..3,0..7,0..2,0..10] of Byte = (
  ((( 128,128,128,128,128,128,128,128,128,128,128),
    ( 128,128,128,128,128,128,128,128,128,128,128),
    ( 128,128,128,128,128,128,128,128,128,128,128)),
   (( 253,136,254,255,228,219,128,128,128,128,128),
    ( 189,129,242,255,227,213,255,219,128,128,128),
    ( 106,126,227,252,214,209,255,255,128,128,128)),
   ((   1, 98,248,255,236,226,255,255,128,128,128),
    ( 181,133,238,254,221,234,255,154,128,128,128),
    (  78,134,202,247,198,180,255,219,128,128,128)),
   ((   1,185,249,255,243,255,128,128,128,128,128),
    ( 184,150,247,255,236,224,128,128,128,128,128),
    (  77,110,216,255,236,230,128,128,128,128,128)),
   ((   1,101,251,255,241,255,128,128,128,128,128),
    ( 170,139,241,252,236,209,255,255,128,128,128),
    (  37,116,196,243,228,255,255,255,128,128,128)),
   ((   1,204,254,255,245,255,128,128,128,128,128),
    ( 207,160,250,255,238,128,128,128,128,128,128),
    ( 102,103,231,255,211,171,128,128,128,128,128)),
   ((   1,152,252,255,240,255,128,128,128,128,128),
    ( 177,135,243,255,234,225,128,128,128,128,128),
    (  80,129,211,255,194,224,128,128,128,128,128)),
   ((   1,  1,255,128,128,128,128,128,128,128,128),
    ( 246,  1,255,128,128,128,128,128,128,128,128),
    ( 255,128,128,128,128,128,128,128,128,128,128))),
  ((( 198, 35,237,223,193,187,162,160,145,155, 62),
    ( 131, 45,198,221,172,176,220,157,252,221,  1),
    (  68, 47,146,208,149,167,221,162,255,223,128)),
   ((   1,149,241,255,221,224,255,255,128,128,128),
    ( 184,141,234,253,222,220,255,199,128,128,128),
    (  81, 99,181,242,176,190,249,202,255,255,128)),
   ((   1,129,232,253,214,197,242,196,255,255,128),
    (  99,121,210,250,201,198,255,202,128,128,128),
    (  23, 91,163,242,170,187,247,210,255,255,128)),
   ((   1,200,246,255,234,255,128,128,128,128,128),
    ( 109,178,241,255,231,245,255,255,128,128,128),
    (  44,130,201,253,205,192,255,255,128,128,128)),
   ((   1,132,239,251,219,209,255,165,128,128,128),
    (  94,136,225,251,218,190,255,255,128,128,128),
    (  22,100,174,245,186,161,255,199,128,128,128)),
   ((   1,182,249,255,232,235,128,128,128,128,128),
    ( 124,143,241,255,227,234,128,128,128,128,128),
    (  35, 77,181,251,193,211,255,205,128,128,128)),
   ((   1,157,247,255,236,231,255,255,128,128,128),
    ( 121,141,235,255,225,227,255,255,128,128,128),
    (  45, 99,188,251,195,217,255,224,128,128,128)),
   ((   1,  1,251,255,213,255,128,128,128,128,128),
    ( 203,  1,248,255,255,128,128,128,128,128,128),
    ( 137,  1,177,255,224,255,128,128,128,128,128))),
  ((( 253,  9,248,251,207,208,255,192,128,128,128),
    ( 175, 13,224,243,193,185,249,198,255,255,128),
    (  73, 17,171,221,161,179,236,167,255,234,128)),
   ((   1, 95,247,253,212,183,255,255,128,128,128),
    ( 239, 90,244,250,211,209,255,255,128,128,128),
    ( 155, 77,195,248,188,195,255,255,128,128,128)),
   ((   1, 24,239,251,218,219,255,205,128,128,128),
    ( 201, 51,219,255,196,186,128,128,128,128,128),
    (  69, 46,190,239,201,218,255,228,128,128,128)),
   ((   1,191,251,255,255,128,128,128,128,128,128),
    ( 223,165,249,255,213,255,128,128,128,128,128),
    ( 141,124,248,255,255,128,128,128,128,128,128)),
   ((   1, 16,248,255,255,128,128,128,128,128,128),
    ( 190, 36,230,255,236,255,128,128,128,128,128),
    ( 149,  1,255,128,128,128,128,128,128,128,128)),
   ((   1,226,255,128,128,128,128,128,128,128,128),
    ( 247,192,255,128,128,128,128,128,128,128,128),
    ( 240,128,255,128,128,128,128,128,128,128,128)),
   ((   1,134,252,255,255,128,128,128,128,128,128),
    ( 213, 62,250,255,255,128,128,128,128,128,128),
    (  55, 93,255,128,128,128,128,128,128,128,128)),
   (( 128,128,128,128,128,128,128,128,128,128,128),
    ( 128,128,128,128,128,128,128,128,128,128,128),
    ( 128,128,128,128,128,128,128,128,128,128,128))),
  ((( 202, 24,213,235,186,191,220,160,240,175,255),
    ( 126, 38,182,232,169,184,228,174,255,187,128),
    (  61, 46,138,219,151,178,240,170,255,216,128)),
   ((   1,112,230,250,199,191,247,159,255,255,128),
    ( 166,109,228,252,211,215,255,174,128,128,128),
    (  39, 77,162,232,172,180,245,178,255,255,128)),
   ((   1, 52,220,246,198,199,249,220,255,255,128),
    ( 124, 74,191,243,183,193,250,221,255,255,128),
    (  24, 71,130,219,154,170,243,182,255,255,128)),
   ((   1,182,225,249,219,240,255,224,128,128,128),
    ( 149,150,226,252,216,205,255,171,128,128,128),
    (  28,108,170,242,183,194,254,223,255,255,128)),
   ((   1, 81,230,252,204,203,255,192,128,128,128),
    ( 123,102,209,247,188,196,255,233,128,128,128),
    (  20, 95,153,243,164,173,255,203,128,128,128)),
   ((   1,222,248,255,216,213,128,128,128,128,128),
    ( 168,175,246,252,235,205,255,255,128,128,128),
    (  47,116,215,255,211,212,255,255,128,128,128)),
   ((   1,121,236,253,212,214,255,255,128,128,128),
    ( 141, 84,213,252,201,202,255,219,128,128,128),
    (  42, 80,160,240,162,185,255,205,128,128,128)),
   ((   1,  1,255,128,128,128,128,128,128,128,128),
    ( 244,  1,255,128,128,128,128,128,128,128,128),
    ( 238,  1,255,128,128,128,128,128,128,128,128)))
  );

  CoeffsUpdateProba: array[0..3,0..7,0..2,0..10] of Byte = (
  (((255,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255)),
   ((176,246,255,255,255,255,255,255,255,255,255),
    (223,241,252,255,255,255,255,255,255,255,255),
    (249,253,253,255,255,255,255,255,255,255,255)),
   ((255,244,252,255,255,255,255,255,255,255,255),
    (234,254,254,255,255,255,255,255,255,255,255),
    (253,255,255,255,255,255,255,255,255,255,255)),
   ((255,246,254,255,255,255,255,255,255,255,255),
    (239,253,254,255,255,255,255,255,255,255,255),
    (254,255,254,255,255,255,255,255,255,255,255)),
   ((255,248,254,255,255,255,255,255,255,255,255),
    (251,255,254,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255)),
   ((255,253,254,255,255,255,255,255,255,255,255),
    (251,254,254,255,255,255,255,255,255,255,255),
    (254,255,254,255,255,255,255,255,255,255,255)),
   ((255,254,253,255,254,255,255,255,255,255,255),
    (250,255,254,255,254,255,255,255,255,255,255),
    (254,255,255,255,255,255,255,255,255,255,255)),
   ((255,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255))),
  (((217,255,255,255,255,255,255,255,255,255,255),
    (225,252,241,253,255,255,254,255,255,255,255),
    (234,250,241,250,253,255,253,254,255,255,255)),
   ((255,254,255,255,255,255,255,255,255,255,255),
    (223,254,254,255,255,255,255,255,255,255,255),
    (238,253,254,254,255,255,255,255,255,255,255)),
   ((255,248,254,255,255,255,255,255,255,255,255),
    (249,254,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255)),
   ((255,253,255,255,255,255,255,255,255,255,255),
    (247,254,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255)),
   ((255,253,254,255,255,255,255,255,255,255,255),
    (252,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255)),
   ((255,254,254,255,255,255,255,255,255,255,255),
    (253,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255)),
   ((255,254,253,255,255,255,255,255,255,255,255),
    (250,255,255,255,255,255,255,255,255,255,255),
    (254,255,255,255,255,255,255,255,255,255,255)),
   ((255,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255))),
  (((186,251,250,255,255,255,255,255,255,255,255),
    (234,251,244,254,255,255,255,255,255,255,255),
    (251,251,243,253,254,255,254,255,255,255,255)),
   ((255,253,254,255,255,255,255,255,255,255,255),
    (236,253,254,255,255,255,255,255,255,255,255),
    (251,253,253,254,254,255,255,255,255,255,255)),
   ((255,254,254,255,255,255,255,255,255,255,255),
    (254,254,254,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255)),
   ((255,254,255,255,255,255,255,255,255,255,255),
    (254,254,255,255,255,255,255,255,255,255,255),
    (254,255,255,255,255,255,255,255,255,255,255)),
   ((255,255,255,255,255,255,255,255,255,255,255),
    (254,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255)),
   ((255,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255)),
   ((255,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255)),
   ((255,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255))),
  (((248,255,255,255,255,255,255,255,255,255,255),
    (250,254,252,254,255,255,255,255,255,255,255),
    (248,254,249,253,255,255,255,255,255,255,255)),
   ((255,253,253,255,255,255,255,255,255,255,255),
    (246,253,253,255,255,255,255,255,255,255,255),
    (252,254,251,254,254,255,255,255,255,255,255)),
   ((255,254,252,255,255,255,255,255,255,255,255),
    (248,254,253,255,255,255,255,255,255,255,255),
    (253,255,254,254,255,255,255,255,255,255,255)),
   ((255,251,254,255,255,255,255,255,255,255,255),
    (245,251,254,255,255,255,255,255,255,255,255),
    (253,253,254,255,255,255,255,255,255,255,255)),
   ((255,251,253,255,255,255,255,255,255,255,255),
    (252,253,254,255,255,255,255,255,255,255,255),
    (255,254,255,255,255,255,255,255,255,255,255)),
   ((255,252,255,255,255,255,255,255,255,255,255),
    (249,255,254,255,255,255,255,255,255,255,255),
    (255,255,254,255,255,255,255,255,255,255,255)),
   ((255,255,253,255,255,255,255,255,255,255,255),
    (250,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255)),
   ((255,255,255,255,255,255,255,255,255,255,255),
    (254,255,255,255,255,255,255,255,255,255,255),
    (255,255,255,255,255,255,255,255,255,255,255)))
  );

  kBands: array[0..16] of Byte =
    (0,1,2,3,6,4,5,6,6,6,6,6,6,6,6,7,0);

  kBModesProba: array[0..NUM_BMODES-1,0..NUM_BMODES-1,0..NUM_BMODES-2] of Byte = (
  (( 231,120, 48, 89,115,113,120,152,112),
   ( 152,179, 64,126,170,118, 46, 70, 95),
   ( 175, 69,143, 80, 85, 82, 72,155,103),
   (  56, 58, 10,171,218,189, 17, 13,152),
   ( 114, 26, 17,163, 44,195, 21, 10,173),
   ( 121, 24, 80,195, 26, 62, 44, 64, 85),
   ( 144, 71, 10, 38,171,213,144, 34, 26),
   ( 170, 46, 55, 19,136,160, 33,206, 71),
   (  63, 20,  8,114,114,208, 12,  9,226),
   (  81, 40, 11, 96,182, 84, 29, 16, 36)),
  (( 134,183, 89,137, 98,101,106,165,148),
   (  72,187,100,130,157,111, 32, 75, 80),
   (  66,102,167, 99, 74, 62, 40,234,128),
   (  41, 53,  9,178,241,141, 26,  8,107),
   (  74, 43, 26,146, 73,166, 49, 23,157),
   (  65, 38,105,160, 51, 52, 31,115,128),
   ( 104, 79, 12, 27,217,255, 87, 17,  7),
   (  87, 68, 71, 44,114, 51, 15,186, 23),
   (  47, 41, 14,110,182,183, 21, 17,194),
   (  66, 45, 25,102,197,189, 23, 18, 22)),
  ((  88, 88,147,150, 42, 46, 45,196,205),
   (  43, 97,183,117, 85, 38, 35,179, 61),
   (  39, 53,200, 87, 26, 21, 43,232,171),
   (  56, 34, 51,104,114,102, 29, 93, 77),
   (  39, 28, 85,171, 58,165, 90, 98, 64),
   (  34, 22,116,206, 23, 34, 43,166, 73),
   ( 107, 54, 32, 26, 51,  1, 81, 43, 31),
   (  68, 25,106, 22, 64,171, 36,225,114),
   (  34, 19, 21,102,132,188, 16, 76,124),
   (  62, 18, 78, 95, 85, 57, 50, 48, 51)),
  (( 193,101, 35,159,215,111, 89, 46,111),
   (  60,148, 31,172,219,228, 21, 18,111),
   ( 112,113, 77, 85,179,255, 38,120,114),
   (  40, 42,  1,196,245,209, 10, 25,109),
   (  88, 43, 29,140,166,213, 37, 43,154),
   (  61, 63, 30,155, 67, 45, 68,  1,209),
   ( 100, 80,  8, 43,154,  1, 51, 26, 71),
   ( 142, 78, 78, 16,255,128, 34,197,171),
   (  41, 40,  5,102,211,183,  4,  1,221),
   (  51, 50, 17,168,209,192, 23, 25, 82)),
  (( 138, 31, 36,171, 27,166, 38, 44,229),
   (  67, 87, 58,169, 82,115, 26, 59,179),
   (  63, 59, 90,180, 59,166, 93, 73,154),
   (  40, 40, 21,116,143,209, 34, 39,175),
   (  47, 15, 16,183, 34,223, 49, 45,183),
   (  46, 17, 33,183,  6, 98, 15, 32,183),
   (  57, 46, 22, 24,128,  1, 54, 17, 37),
   (  65, 32, 73,115, 28,128, 23,128,205),
   (  40,  3,  9,115, 51,192, 18,  6,223),
   (  87, 37,  9,115, 59, 77, 64, 21, 47)),
  (( 104, 55, 44,218,  9, 54, 53,130,226),
   (  64, 90, 70,205, 40, 41, 23, 26, 57),
   (  54, 57,112,184,  5, 41, 38,166,213),
   (  30, 34, 26,133,152,116, 10, 32,134),
   (  39, 19, 53,221, 26,114, 32, 73,255),
   (  31,  9, 65,234,  2, 15,  1,118, 73),
   (  75, 32, 12, 51,192,255,160, 43, 51),
   (  88, 31, 35, 67,102, 85, 55,186, 85),
   (  56, 21, 23,111, 59,205, 45, 37,192),
   (  55, 38, 70,124, 73,102,  1, 34, 98)),
  (( 125, 98, 42, 88,104, 85,117,175, 82),
   (  95, 84, 53, 89,128,100,113,101, 45),
   (  75, 79,123, 47, 51,128, 81,171,  1),
   (  57, 17,  5, 71,102, 57, 53, 41, 49),
   (  38, 33, 13,121, 57, 73, 26,  1, 85),
   (  41, 10, 67,138, 77,110, 90, 47,114),
   ( 115, 21,  2, 10,102,255,166, 23,  6),
   ( 101, 29, 16, 10, 85,128,101,196, 26),
   (  57, 18, 10,102,102,213, 34, 20, 43),
   ( 117, 20, 15, 36,163,128, 68,  1, 26)),
  (( 102, 61, 71, 37, 34, 53, 31,243,192),
   (  69, 60, 71, 38, 73,119, 28,222, 37),
   (  68, 45,128, 34,  1, 47, 11,245,171),
   (  62, 17, 19, 70,146, 85, 55, 62, 70),
   (  37, 43, 37,154,100,163, 85,160,  1),
   (  63,  9, 92,136, 28, 64, 32,201, 85),
   (  75, 15,  9,  9, 64,255,184,119, 16),
   (  86,  6, 28,  5, 64,255, 25,248,  1),
   (  56,  8, 17,132,137,255, 55,116,128),
   (  58, 15, 20, 82,135, 57, 26,121, 40)),
  (( 164, 50, 31,137,154,133, 25, 35,218),
   (  51,103, 44,131,131,123, 31,  6,158),
   (  86, 40, 64,135,148,224, 45,183,128),
   (  22, 26, 17,131,240,154, 14,  1,209),
   (  45, 16, 21, 91, 64,222,  7,  1,197),
   (  56, 21, 39,155, 60,138, 23,102,213),
   (  83, 12, 13, 54,192,255, 68, 47, 28),
   (  85, 26, 85, 85,128,128, 32,146,171),
   (  18, 11,  7, 63,144,171,  4,  4,246),
   (  35, 27, 10,146,174,171, 12, 26,128)),
  (( 190, 80, 35, 99,180, 80,126, 54, 45),
   (  85,126, 47, 87,176, 51, 41, 20, 32),
   ( 101, 75,128,139,118,146,116,128, 85),
   (  56, 41, 15,176,236, 85, 37,  9, 62),
   (  71, 30, 17,119,118,255, 17, 18,138),
   ( 101, 38, 60,138, 55, 70, 43, 26,142),
   ( 146, 36, 19, 30,171,255, 97, 27, 20),
   ( 138, 45, 61, 62,219,  1, 81,188, 64),
   (  32, 41, 20,117,151,142, 20, 21,163),
   ( 112, 19, 12, 61,195,128, 48,  4, 24))
  );

  kDcTable: array[0..127] of Byte = (
    4,  5,  6,  7,  8,  9, 10, 10,
   11, 12, 13, 14, 15, 16, 17, 17,
   18, 19, 20, 20, 21, 21, 22, 22,
   23, 23, 24, 25, 25, 26, 27, 28,
   29, 30, 31, 32, 33, 34, 35, 36,
   37, 37, 38, 39, 40, 41, 42, 43,
   44, 45, 46, 46, 47, 48, 49, 50,
   51, 52, 53, 54, 55, 56, 57, 58,
   59, 60, 61, 62, 63, 64, 65, 66,
   67, 68, 69, 70, 71, 72, 73, 74,
   75, 76, 76, 77, 78, 79, 80, 81,
   82, 83, 84, 85, 86, 87, 88, 89,
   91, 93, 95, 96, 98,100,101,102,
  104,106,108,110,112,114,116,118,
  122,124,126,128,130,132,134,136,
  138,140,143,145,148,151,154,157);

  kAcTable: array[0..127] of Word = (
    4,  5,  6,  7,  8,  9, 10, 11,
   12, 13, 14, 15, 16, 17, 18, 19,
   20, 21, 22, 23, 24, 25, 26, 27,
   28, 29, 30, 31, 32, 33, 34, 35,
   36, 37, 38, 39, 40, 41, 42, 43,
   44, 45, 46, 47, 48, 49, 50, 51,
   52, 53, 54, 55, 56, 57, 58, 60,
   62, 64, 66, 68, 70, 72, 74, 76,
   78, 80, 82, 84, 86, 88, 90, 92,
   94, 96, 98,100,102,104,106,108,
  110,112,114,116,119,122,125,128,
  131,134,137,140,143,146,149,152,
  155,158,161,164,167,170,173,177,
  181,185,189,193,197,201,205,209,
  213,217,221,225,229,234,239,245,
  249,254,259,264,269,274,279,284);

  // Zigzag scan order for 4x4 block
  kZigzag: array[0..15] of Byte =
    (0,1,4,8, 5,2,3,6, 9,12,13,10, 7,11,14,15);

  // Byte offsets of each 4x4 sub-block in the YUV reconstruction buffer
  // BPS=32; sub-block row i starts at i*4*BPS = i*128
  kScan: array[0..15] of Integer = (
      0,  4,  8, 12,
    128,132,136,140,
    256,260,264,268,
    384,388,392,396);

  // Category probability tables for large residual values
  kCat3: array[0..3] of Byte = (173,148,140,  0);
  kCat4: array[0..4] of Byte = (176,155,140,135,  0);
  kCat5: array[0..5] of Byte = (180,157,141,134,130,  0);
  kCat6: array[0..11] of Byte = (254,254,243,230,196,177,153,140,133,130,129,0);

  // VP8L distance-to-plane offset table (kCodeToPlane[120])
  kCodeToPlane: array[0..119] of Integer = (
    $18,  $07,  $17,  $19,  $28,  $06,  $27,  $29,
    $16,  $1a,  $26,  $2a,  $38,  $05,  $37,  $39,
    $15,  $1b,  $36,  $3a,  $25,  $2b,  $48,  $04,
    $47,  $49,  $14,  $1c,  $35,  $3b,  $46,  $4a,
    $24,  $2c,  $58,  $45,  $4b,  $34,  $3c,  $03,
    $57,  $59,  $13,  $1d,  $56,  $5a,  $23,  $2d,
    $44,  $4c,  $55,  $5b,  $33,  $3d,  $68,  $02,
    $67,  $69,  $12,  $1e,  $66,  $6a,  $22,  $2e,
    $54,  $5c,  $43,  $4d,  $65,  $6b,  $32,  $3e,
    $78,  $01,  $77,  $79,  $53,  $5d,  $11,  $1f,
    $64,  $6c,  $42,  $4e,  $76,  $7a,  $21,  $2f,
    $75,  $7b,  $31,  $3f,  $63,  $6d,  $52,  $5e,
    $00,  $74,  $7c,  $41,  $4f,  $10,  $20,  $62,
    $6e,  $30,  $73,  $7d,  $51,  $5f,  $40,  $72,
    $7e,  $61,  $6f,  $50,  $71,  $7f,  $60,  $70);

  // Huffman code length reorder for VP8L canonical codes
  kCodeLengthCodeOrder: array[0..18] of Byte =
    (17,18,0,1,2,3,4,5,16,6,7,8,9,10,11,12,13,14,15);

  // Alphabet sizes for the 5 Huffman code groups in VP8L
  kAlphabetSize: array[0..4] of Integer = (280, 256, 256, 256, 40);

// ============================================================
// RECORD TYPES  (after consts so sizes are known)
// ============================================================
type
  TVP8BandProbas = record
    Probas: array[0..NUM_CTX-1, 0..NUM_PROBAS-1] of Byte;
  end;

  // One row of band-pointers (17 entries: bands 0..16, with kBands mapping)
  TBandPtrsRow = array[0..16] of ^TVP8BandProbas;

  TVP8Proba = record
    Bands:    array[0..NUM_TYPES-1, 0..NUM_BANDS-1] of TVP8BandProbas;
    BandsPtr: array[0..NUM_TYPES-1] of TBandPtrsRow;
  end;

  TVP8QuantMatrix = record
    Y1Mat:   array[0..1] of Integer;
    Y2Mat:   array[0..1] of Integer;
    UVMat:   array[0..1] of Integer;
    UVQuant: Integer;
  end;

  TVP8SegmentHeader = record
    UseSegment:    Boolean;
    UpdateMap:     Boolean;
    AbsoluteDelta: Boolean;
    Quantizer:     array[0..NUM_MB_SEGMENTS-1] of Integer;
    FilterStrength:array[0..NUM_MB_SEGMENTS-1] of Integer;
    SegProbs:      array[0..MB_FEATURE_TREE_PROBS-1] of Byte; // segment map probs
  end;

  TVP8MB = record
    NZ:   Byte;   // non-zero AC flags (Y0..Y3, U0,U1, V0,V1)
    NZDC: Byte;   // non-zero DC flags
  end;
  PVP8MB = ^TVP8MB;

  TVP8MBData = record
    Coeffs:   array[0..383] of Int16;  // 24 blocks * 16 = 384
    IsI4x4:   Boolean;
    IModes:   array[0..15] of Byte;    // per-4x4-block modes
    UVMode:   Byte;
    NonZeroY: Cardinal;
    NonZeroUV:Cardinal;
    Skip:     Boolean;
    Segment:  Byte;
  end;
  PVP8MBData = ^TVP8MBData;

  // Huffman code entry used by VP8L
  THuffmanCode = record
    Bits:  Byte;   // code length (0 = invalid)
    Value: Word;   // symbol value
  end;
  PHuffmanCode = ^THuffmanCode;

  THuffmanCode32 = record
    Bits:  Byte;
    Value: Cardinal;
  end;

// ============================================================
// HELPER FUNCTIONS
// ============================================================

// Arithmetic right shift by n bits (FPC's shr is logical/unsigned).
// Equivalent to C's (v >> n) for signed int32.
// Uses the identity: sar(v,n) = ~(~v >> n) for negative v, v>>n for positive v.
function SarI(v, n: Integer): Integer; inline;
begin
  if v >= 0 then Result := v shr n
  else Result := not (not v shr n);
end;

function Clip8b(v: Integer): Byte; inline;
begin
  if v < 0 then Result := 0
  else if v > 255 then Result := 255
  else Result := Byte(v);
end;

function ClipMax(v, M: Integer): Integer; inline;
begin
  if v < 0 then Result := 0
  else if v > M then Result := M
  else Result := v;
end;

// YUV → RGB (libwebp formulas from yuv.h)
function MultHi(v, c: Integer): Integer; inline;
begin
  Result := (v * c) shr 8;
end;

function VP8Clip8(v: Integer): Byte; inline;
// YUV_FIX2=6, YUV_MASK2=$3FFF
begin
  if (v and (not $3FFF)) = 0 then
    Result := Byte(v shr 6)
  else if v < 0 then
    Result := 0
  else
    Result := 255;
end;

function YuvToR(y, v: Integer): Byte; inline;
begin
  Result := VP8Clip8(MultHi(y, 19077) + MultHi(v, 26149) - 14234);
end;

function YuvToG(y, u, v: Integer): Byte; inline;
begin
  Result := VP8Clip8(MultHi(y, 19077) - MultHi(u, 6419) - MultHi(v, 13320) + 8708);
end;

function YuvToB(y, u: Integer): Byte; inline;
begin
  Result := VP8Clip8(MultHi(y, 19077) + MultHi(u, 33050) - 17685);
end;

// ============================================================
// VP8L BIT READER  (LSB-first, 64-bit accumulator)
// ============================================================
type
  TVP8LBitReader = record
    Val:       UInt64;
    Available: Integer;
    Buf:       PByte;
    BufEnd:    PByte;
    Eos:       Boolean;
  end;

procedure VP8LFillBitWindow(var BR: TVP8LBitReader); inline;
begin
  while (BR.Available <= 56) and (BR.Buf < BR.BufEnd) do
  begin
    BR.Val := BR.Val or (UInt64(BR.Buf^) shl BR.Available);
    Inc(BR.Buf);
    Inc(BR.Available, 8);
  end;
  if (BR.Buf >= BR.BufEnd) and (BR.Available < 0) then
    BR.Eos := True;
end;

procedure VP8LInitBitReader(var BR: TVP8LBitReader; Data: PByte; Size: NativeUInt);
begin
  BR.Val       := 0;
  BR.Available := 0;
  BR.Buf       := Data;
  BR.BufEnd    := Data + Size;
  BR.Eos       := (Size = 0);
  VP8LFillBitWindow(BR);
end;

function VP8LReadBits(var BR: TVP8LBitReader; N: Integer): Cardinal; inline;
begin
  if N = 0 then begin Result := 0; Exit; end;
  Result := Cardinal(BR.Val) and Cardinal((UInt64(1) shl N) - 1);
  BR.Val := BR.Val shr N;
  Dec(BR.Available, N);
  if BR.Available <= 32 then VP8LFillBitWindow(BR);
end;

function VP8LPeekBits(const BR: TVP8LBitReader; N: Integer): Cardinal; inline;
begin
  Result := Cardinal(BR.Val) and Cardinal((UInt64(1) shl N) - 1);
end;

// ============================================================
// VP8 BOOLEAN BIT READER  (MSB-first, range coder)
// Matches libwebp bit_reader_utils.c:
//   range starts at 254; split = (range * prob) >> 8
// ============================================================
type
  TVP8Rd = record
    Val:    UInt64;   // accumulated bit window
    Range:  UInt32;   // current range [127..254]
    Bits:   Integer;  // valid bits in Val (>= 0)
    Buf:    PByte;
    BufEnd: PByte;
    Eof:    Boolean;
  end;

procedure VP8RdLoadByte(var R: TVP8Rd); inline;
begin
  if R.Buf < R.BufEnd then
  begin
    R.Val := (R.Val shl 8) or R.Buf^;
    Inc(R.Buf);
  end else
  begin
    R.Eof := True;
    R.Val := R.Val shl 8;
  end;
  Inc(R.Bits, 8);
end;

procedure VP8RdInit(var R: TVP8Rd; Data: PByte; Size: NativeUInt);
begin
  R.Val    := 0;
  R.Range  := 254;
  R.Bits   := -8;
  R.Buf    := Data;
  R.BufEnd := Data + Size;
  R.Eof    := (Size = 0);
  VP8RdLoadByte(R);   // Bits = 0 after this
end;

function VP8RdGetBit(var R: TVP8Rd; Prob: Integer): Integer; inline;
var
  split: UInt32;
begin
  if R.Bits < 0 then VP8RdLoadByte(R);
  split := (R.Range * UInt32(Prob)) shr 8;
  if (R.Val shr R.Bits) > split then
  begin
    Dec(R.Range, split + 1);
    Dec(R.Val, UInt64(split + 1) shl R.Bits);
    Result := 1;
  end else
  begin
    R.Range := split;
    Result := 0;
  end;
  // Normalize: keep Range in [127..254]
  while R.Range < 127 do
  begin
    R.Range := R.Range * 2 + 1;
    Dec(R.Bits);
    if R.Bits < 0 then
      VP8RdLoadByte(R);
  end;
end;

function VP8RdGet(var R: TVP8Rd): Integer; inline;
begin
  Result := VP8RdGetBit(R, 128);
end;

// Specialized sign-bit read matching C's VP8GetSigned(br, v):
//   Returns v if sign=0, -v if sign=1.
//   Uses prob=128 but with the simplified update matching libwebp's VP8GetSigned.
//   Unlike VP8RdGetBit(128), this does NOT normalize Range afterward,
//   instead it unconditionally decrements Bits by 1 and sets Range |= 1.
//   The two functions diverge only for Range=254 with sign=0:
//     VP8RdGetBit gives Range=127, Bits unchanged;
//     VP8RdGetSigned gives Range=255, Bits-=1.
//   Using VP8RdGetSigned matches the C reference decoder exactly.
function VP8RdGetSigned(var R: TVP8Rd; v: Integer): Integer; inline;
var
  pos: Integer;
  split, value: UInt32;
  mask: Integer;
begin
  if R.Bits < 0 then VP8RdLoadByte(R);
  pos   := R.Bits;                              // save original Bits position
  split := R.Range shr 1;
  value := UInt32(R.Val shr pos);
  // SarI: arithmetic right shift gives 0 or -1 (FPC shr is logical → wrong)
  mask  := SarI(Integer(split) - Integer(value), 31);
  R.Bits := pos - 1;                            // decrement by 1 (may become -1)
  Inc(R.Range, UInt32(mask));                   // range-1 if bit=1 (mask=-1), range if bit=0
  R.Range := R.Range or 1;                      // always ensure lowest bit set
  Dec(R.Val, UInt64((split + 1) and UInt32(mask)) shl pos);  // use original pos, not decremented
  Result := (v xor mask) - mask;                // v if mask=0, -v if mask=-1
end;

function VP8RdGetValue(var R: TVP8Rd; N: Integer): Cardinal; inline;
var i: Integer;
begin
  Result := 0;
  for i := N - 1 downto 0 do
    if VP8RdGetBit(R, 128) <> 0 then
      Result := Result or (Cardinal(1) shl i);
end;

function VP8RdGetSignedValue(var R: TVP8Rd; N: Integer): Integer; inline;
var v: Cardinal;
begin
  v := VP8RdGetValue(R, N);
  if VP8RdGetBit(R, 128) <> 0 then
    Result := -Integer(v)
  else
    Result := Integer(v);
end;

// ============================================================
// VP8L HUFFMAN TABLE BUILDER
// Port of huffman_utils.c : BuildHuffmanTable
// ============================================================
const
  HUFF_LUT_BITS = 8;  // root table bits

type
  THuffTable = array[0..FIXED_TABLE_SIZE-1] of THuffmanCode;
  PHuffTable = ^THuffTable;

// Correct Huffman build used in VP8L decode
// Returns True on success. Builds reverse-lookup table.
function VP8LBuildHuffmanTable(
  const Lengths: array of Integer; NumSymbols: Integer;
  Table: PHuffmanCode; TableBits: Integer): Boolean;
var
  count:     array[0..16] of Integer;
  nextcode:  array[0..16] of Cardinal;
  code:      Cardinal;
  i, len:    Integer;
  entry:     THuffmanCode;
  mirror:    Cardinal;
  j, step:   Integer;
  tableSize: Integer;
begin
  Result := False;
  tableSize := 1 shl TableBits;
  FillChar(count, SizeOf(count), 0);
  for i := 0 to NumSymbols-1 do
  begin
    len := Lengths[i];
    if (len < 0) or (len > 15) then Exit;
    Inc(count[len]);
  end;
  // Assign canonical codes
  code := 0;
  nextcode[0] := 0;
  for len := 1 to 15 do
  begin
    code := (code + Cardinal(count[len-1])) shl 1;
    nextcode[len] := code;
  end;
  // Fill table
  for i := 0 to tableSize-1 do
  begin
    Table[i].Bits  := 0;
    Table[i].Value := $FFFF;
  end;
  for i := 0 to NumSymbols-1 do
  begin
    len := Lengths[i];
    if len = 0 then Continue;
    if len > TableBits then Continue;  // ignore overflow for now
    // Reverse the canonical code to get LSB-first lookup key
    code := nextcode[len];
    Inc(nextcode[len]);
    mirror := 0;
    for j := 0 to len-1 do
      if (code shr j) and 1 <> 0 then
        mirror := mirror or (Cardinal(1) shl (len-1-j));
    // Replicate for all suffixes
    step := 1 shl len;
    j := Integer(mirror);
    while j < tableSize do
    begin
      entry.Bits  := Byte(len);
      entry.Value := Word(i);
      Table[j]    := entry;
      Inc(j, step);
    end;
  end;
  Result := True;
end;

// Read a Huffman symbol using the lookup table
function HuffReadSymbol(var BR: TVP8LBitReader;
  const Table: PHuffmanCode; TableBits: Integer): Integer; inline;
var
  key: Cardinal;
  e:   THuffmanCode;
begin
  key := VP8LPeekBits(BR, TableBits);
  e   := Table[key];
  if e.Bits = 0 then begin Result := -1; Exit; end;
  // Consume the bits
  BR.Val := BR.Val shr e.Bits;
  Dec(BR.Available, e.Bits);
  if BR.Available <= 32 then VP8LFillBitWindow(BR);
  Result := Integer(e.Value);
end;

// ============================================================
// DECODER DATA TYPES
// ============================================================
type
  // Per-macroblock loop-filter info (mirrors libwebp VP8FInfo)
  TVP8FInfo = record
    FLimit:    Integer;   // filter limit (0 = no filtering)
    FILevel:   Integer;   // interior limit
    HevThresh: Integer;   // high edge variance threshold
    FInner:    Boolean;   // filter inner edges
  end;
  PVP8FInfo = ^TVP8FInfo;

  TVP8Decoder = record
    // Main bitreader (partition 0)
    BR:          TVP8Rd;
    // AC residual partition readers
    Parts:       array[0..MAX_NUM_PARTITIONS-1] of TVP8Rd;
    NumParts:    Integer;

    // Picture dimensions
    PicWidth:    Integer;
    PicHeight:   Integer;
    MbW:         Integer;   // macroblock columns
    MbH:         Integer;   // macroblock rows

    // Headers
    KeyFrame:    Boolean;
    Profile:     Integer;
    PartLen0:    Integer;   // partition 0 byte length

    // Segment
    SegHdr:      TVP8SegmentHeader;
    // Quantization matrices (one per segment)
    DQM:         array[0..NUM_MB_SEGMENTS-1] of TVP8QuantMatrix;
    // Probability tables
    Proba:       TVP8Proba;

    // Filter (skipped — just store for parsing)
    FilterSimple:   Boolean;
    FilterLevel:    Integer;
    FilterSharpness:Integer;
    UseLFDelta:     Boolean;
    RefLFDelta:     array[0..NUM_REF_LF_DELTAS-1] of Integer;
    ModeLFDelta:    array[0..NUM_MODE_LF_DELTAS-1] of Integer;

    // Decoded output row (YUV → RGB, written row by row)
    OutputMode:  TCSMode;
    OutStride:   Integer;   // bytes per output row
    OutBpp:      Integer;   // bytes per pixel

    // YUV reconstruction buffer for current MB row
    YuvBuf:      array[0..YUV_SIZE-1] of Byte;
    // Top context rows for inter-MB prediction
    YTopBuf:     PByte;   // MbW*16 bytes  (Y top row)
    UTopBuf:     PByte;   // MbW*8 bytes
    VTopBuf:     PByte;   // MbW*8 bytes
    // Per-MB NZ info (MbW+1, index 0 = left border)
    MBInfo:      PVP8MB;
    // Current MB working data
    MBData:      TVP8MBData;
    // Skip probability
    UseSkipProba: Boolean;
    SkipP:        Byte;
    // I4x4 intra-mode context
    IntraT:       PByte;          // MbW*4 bytes: top 4x4 mode per column
    IntraL:       array[0..3] of Byte;  // left 4x4 mode per row
    // Final output buffer
    OutBuf:      PByte;

    // --- In-loop filter ---
    FilterType:  Integer;   // 0 = none, 1 = simple, 2 = complex
    FStrength:   array[0..NUM_MB_SEGMENTS-1, 0..1] of TVP8FInfo;  // [segment][i4x4]
    FInfo:       PVP8FInfo; // per-MB (MbW*MbH) filter info
    // Full-frame reconstructed YUV planes (filled per MB, then filtered)
    FYPlane:     PByte;
    FUPlane:     PByte;
    FVPlane:     PByte;
    FYStride:    Integer;
    FUVStride:   Integer;
  end;

// ============================================================
// VP8 HEADER PARSING
// ============================================================

procedure VP8ParseSegmentHeader(var BR: TVP8Rd; var Hdr: TVP8SegmentHeader);
var i: Integer;
begin
  Hdr.UseSegment := VP8RdGet(BR) <> 0;
  if not Hdr.UseSegment then
  begin
    Hdr.UpdateMap := False;
    Exit;
  end;
  Hdr.UpdateMap := VP8RdGet(BR) <> 0;
  // update_data flag is separate from update_map
  if VP8RdGet(BR) <> 0 then   // update data?
  begin
    Hdr.AbsoluteDelta := VP8RdGet(BR) <> 0; // 1=absolute, 0=delta (matches C absolute_delta)
    for i := 0 to NUM_MB_SEGMENTS-1 do
      if VP8RdGet(BR) <> 0 then
        Hdr.Quantizer[i] := VP8RdGetSignedValue(BR, 7)
      else
        Hdr.Quantizer[i] := 0;
    for i := 0 to NUM_MB_SEGMENTS-1 do
      if VP8RdGet(BR) <> 0 then
        Hdr.FilterStrength[i] := VP8RdGetSignedValue(BR, 6)
      else
        Hdr.FilterStrength[i] := 0;
  end;
  if Hdr.UpdateMap then
  begin
    for i := 0 to MB_FEATURE_TREE_PROBS-1 do
      if VP8RdGet(BR) <> 0 then
        Hdr.SegProbs[i] := Byte(VP8RdGetValue(BR, 8))
      else
        Hdr.SegProbs[i] := 255;
  end;
end;

procedure VP8ParseFilterHeader(var BR: TVP8Rd; var D: TVP8Decoder);
var i: Integer;
begin
  D.FilterSimple    := VP8RdGet(BR) <> 0;
  D.FilterLevel     := Integer(VP8RdGetValue(BR, 6));
  D.FilterSharpness := Integer(VP8RdGetValue(BR, 3));
  D.UseLFDelta      := VP8RdGet(BR) <> 0;
  if D.UseLFDelta and (VP8RdGet(BR) <> 0) then
  begin
    for i := 0 to NUM_REF_LF_DELTAS-1 do
      if VP8RdGet(BR) <> 0 then
        D.RefLFDelta[i] := VP8RdGetSignedValue(BR, 6);
    for i := 0 to NUM_MODE_LF_DELTAS-1 do
      if VP8RdGet(BR) <> 0 then
        D.ModeLFDelta[i] := VP8RdGetSignedValue(BR, 6);
  end;
end;

// Clip helper used in VP8ParseQuant
function QClip(v, M: Integer): Integer; inline;
begin
  if v < 0 then Result := 0
  else if v > M then Result := M
  else Result := v;
end;

procedure VP8ParseQuant(var BR: TVP8Rd; var D: TVP8Decoder);
var
  base_q0: Integer;
  dqy1_dc, dqy2_dc, dqy2_ac, dquv_dc, dquv_ac: Integer;
  i, q: Integer;
  m: ^TVP8QuantMatrix;
begin
  q := 0;
  base_q0 := Integer(VP8RdGetValue(BR, 7));
  if VP8RdGet(BR) <> 0 then dqy1_dc  := VP8RdGetSignedValue(BR, 4) else dqy1_dc  := 0;
  if VP8RdGet(BR) <> 0 then dqy2_dc  := VP8RdGetSignedValue(BR, 4) else dqy2_dc  := 0;
  if VP8RdGet(BR) <> 0 then dqy2_ac  := VP8RdGetSignedValue(BR, 4) else dqy2_ac  := 0;
  if VP8RdGet(BR) <> 0 then dquv_dc  := VP8RdGetSignedValue(BR, 4) else dquv_dc  := 0;
  if VP8RdGet(BR) <> 0 then dquv_ac  := VP8RdGetSignedValue(BR, 4) else dquv_ac  := 0;
  for i := 0 to NUM_MB_SEGMENTS-1 do
  begin
    if D.SegHdr.UseSegment then
    begin
      q := D.SegHdr.Quantizer[i];
      if not D.SegHdr.AbsoluteDelta then Inc(q, base_q0);
    end else
    begin
      if i > 0 then begin D.DQM[i] := D.DQM[0]; Continue; end;
      q := base_q0;
    end;
    m := @D.DQM[i];
    m^.Y1Mat[0] := kDcTable[QClip(q + dqy1_dc, 127)];
    m^.Y1Mat[1] := kAcTable[QClip(q,           127)];
    m^.Y2Mat[0] := kDcTable[QClip(q + dqy2_dc, 127)] * 2;
    m^.Y2Mat[1] := (Integer(kAcTable[QClip(q + dqy2_ac, 127)]) * 101581) shr 16;
    if m^.Y2Mat[1] < 8 then m^.Y2Mat[1] := 8;
    m^.UVMat[0] := kDcTable[QClip(q + dquv_dc, 117)];   // max 117!
    m^.UVMat[1] := kAcTable[QClip(q + dquv_ac, 127)];
    m^.UVQuant  := q + dquv_ac;
  end;
end;

procedure VP8ParseProba(var BR: TVP8Rd; var D: TVP8Decoder);
var t, b, ctx, p: Integer;
begin
  // Copy defaults
  for t := 0 to NUM_TYPES-1 do
    for b := 0 to NUM_BANDS-1 do
      for ctx := 0 to NUM_CTX-1 do
        for p := 0 to NUM_PROBAS-1 do
          D.Proba.Bands[t,b].Probas[ctx,p] := CoeffsProba0[t,b,ctx,p];
  // Read updates
  for t := 0 to NUM_TYPES-1 do
    for b := 0 to NUM_BANDS-1 do
      for ctx := 0 to NUM_CTX-1 do
        for p := 0 to NUM_PROBAS-1 do
          if VP8RdGetBit(BR, CoeffsUpdateProba[t,b,ctx,p]) <> 0 then
            D.Proba.Bands[t,b].Probas[ctx,p] := Byte(VP8RdGetValue(BR, 8));
  // Build BandsPtr: BandsPtr[t][b] = @Bands[t][kBands[b]]
  for t := 0 to NUM_TYPES-1 do
    for b := 0 to 16 do
      D.Proba.BandsPtr[t][b] := @D.Proba.Bands[t, kBands[b]];
  // Skip probability (Paragraph 9.11)
  D.UseSkipProba := VP8RdGet(BR) <> 0;
  if D.UseSkipProba then
    D.SkipP := Byte(VP8RdGetValue(BR, 8));
end;

// ============================================================
// VP8 INTRA MODE PARSING
// ============================================================

function ParseIntra16Mode(var BR: TVP8Rd): Integer; inline;
begin
  // bit(156)? (bit(128)?TM:H) : (bit(163)?V:DC)
  if VP8RdGetBit(BR, 156) <> 0 then
  begin
    if VP8RdGetBit(BR, 128) <> 0 then Result := TM_PRED else Result := H_PRED;
  end else
  begin
    if VP8RdGetBit(BR, 163) <> 0 then Result := V_PRED else Result := DC_PRED;
  end;
end;

function ParseUVMode(var BR: TVP8Rd): Integer; inline;
begin
  if VP8RdGetBit(BR, 142) = 0 then Result := DC_PRED
  else if VP8RdGetBit(BR, 114) = 0 then Result := V_PRED
  else if VP8RdGetBit(BR, 183) <> 0 then Result := TM_PRED
  else Result := H_PRED;
end;

function ParseIntra4x4Mode(var BR: TVP8Rd;
  const Prob: array of Byte): Integer;
// Prob is kBModesProba[topMode][leftMode]
begin
  if VP8RdGetBit(BR, Prob[0]) = 0 then begin Result := B_DC_PRED; Exit; end;
  if VP8RdGetBit(BR, Prob[1]) = 0 then begin Result := B_TM_PRED; Exit; end;
  if VP8RdGetBit(BR, Prob[2]) = 0 then begin Result := B_VE_PRED; Exit; end;
  if VP8RdGetBit(BR, Prob[3]) = 0 then
  begin
    if VP8RdGetBit(BR, Prob[4]) = 0 then begin Result := B_HE_PRED; Exit; end;
    if VP8RdGetBit(BR, Prob[5]) = 0 then begin Result := B_RD_PRED; Exit; end;
    Result := B_VR_PRED;
  end else
  begin
    if VP8RdGetBit(BR, Prob[6]) = 0 then begin Result := B_LD_PRED; Exit; end;
    if VP8RdGetBit(BR, Prob[7]) = 0 then begin Result := B_VL_PRED; Exit; end;
    if VP8RdGetBit(BR, Prob[8]) = 0 then begin Result := B_HD_PRED; Exit; end;
    Result := B_HU_PRED;
  end;
end;

procedure VP8ParseIntraModes(var D: TVP8Decoder);
// Parses all macroblock intra prediction modes for the current frame.
// Fills D.MBData[] — called ONCE per frame before residual decoding.
// For each MB: IsI4x4, IModes[16], UVMode, Segment
var
  topY: PByte;  // [MbW * 16] — Y top-row modes for 4x4
  seg_proba: array[0..MB_FEATURE_TREE_PROBS-1] of Byte;
  leftMode: array[0..15] of Byte; // left column 4x4 modes
begin
  // For mode parsing we need a top-modes array (one 4x4 mode per top-pixel)
  // Allocate temporary: MbW * 4 bytes for top modes
  topY := AllocMem(D.MbW * 4 * SizeOf(Byte));
  FillChar(topY^, D.MbW * 4, B_DC_PRED);
  FillChar(leftMode, SizeOf(leftMode), B_DC_PRED);
  //leftUV := DC_PRED;

  // Default segment proba
  seg_proba[0] := 145; seg_proba[1] := 145; seg_proba[2] := 145;

  //pMB := PVP8MBData(D.OutBuf); // WRONG — need separate mode buffer
  // Actually store modes in D.MBData (only last row needed for residuals)
  // For simplicity, we parse modes and residuals together per-row in the main loop
  FreeMem(topY);
end;

// ============================================================
// VP8 RESIDUAL COEFFICIENT DECODING
// ============================================================

// Decode residual coefficients for one 4x4 block.
// Matches C GetCoeffsFast exactly:
//   p[0]=EOB check, p[1]=zero/nonzero, p[2..]=value decode
//   BandsPtr[n] already incorporates kBands mapping (DO NOT apply kBands again)
// Returns position of last non-zero coeff + 1 (i.e. 0 = all-zero)
function VP8GetCoeffsFast(var BR: TVP8Rd;
  const BandsPtr: TBandPtrsRow;
  StartCtx, First: Integer;
  Dq0, Dq1: Integer;
  Coeffs: PInt16): Integer;
var
  n, v: Integer;
  p: PByte;  // points to BandsPtr[n]^.Probas[ctx, 0]
  tab: PByte;
  bit1, bit0, cat: Integer;
begin
  // p points to the 11-byte probability row for (position n, context ctx)
  // p[0]=EOB  p[1]=zero  p[2]=v>1  p[3..]=value
  n := First;
  p := @(BandsPtr[n]^.Probas[StartCtx, 0]);

  while n < 16 do
  begin
    // p[0]: is there any non-zero coeff from position n onwards? (0 = EOB)
    if VP8RdGetBit(BR, p[0]) = 0 then
    begin
      Result := n;
      Exit;
    end;
    // p[1]: is coeff at n non-zero? (0 = this coeff is zero, advance)
    while VP8RdGetBit(BR, p[1]) = 0 do
    begin
      Inc(n);
      if n = 16 then begin Result := 16; Exit; end;
      p := @(BandsPtr[n]^.Probas[0, 0]);  // ctx=0 after zero run
    end;
    // Non-zero coeff at position n; decode absolute value using p[2..10]
    if VP8RdGetBit(BR, p[2]) = 0 then
    begin
      v := 1;
      p := @(BandsPtr[n + 1]^.Probas[1, 0]);  // ctx=1 for next
    end else
    begin
      // GetLargeValue: v > 1
      if VP8RdGetBit(BR, p[3]) = 0 then
      begin
        if VP8RdGetBit(BR, p[4]) = 0 then
          v := 2
        else
          v := 3 + VP8RdGetBit(BR, p[5]);
      end else if VP8RdGetBit(BR, p[6]) = 0 then
      begin
        if VP8RdGetBit(BR, p[7]) = 0 then
          v := 5 + VP8RdGetBit(BR, 159)
        else
        begin
          v := 7 + 2 * VP8RdGetBit(BR, 165);
          v := v + VP8RdGetBit(BR, 145);
        end;
      end else
      begin
        // Cat 3..6 using kCat3456 tables
        bit1 := VP8RdGetBit(BR, p[8]);
        bit0 := VP8RdGetBit(BR, p[9 + bit1]);
        cat  := 2 * bit1 + bit0;
        case cat of
          0: tab := @kCat3[0];
          1: tab := @kCat4[0];
          2: tab := @kCat5[0];
          3: tab := @kCat6[0];
          else tab := @kCat3[0]; // unreachable
        end;
        v := 0;
        while tab^ <> 0 do
        begin
          v := v * 2 + VP8RdGetBit(BR, tab^);
          Inc(tab);
        end;
        v := v + 3 + (8 shl cat);
      end;
      p := @(BandsPtr[n + 1]^.Probas[2, 0]);  // ctx=2 for next
    end;
    // Sign bit — use VP8RdGetSigned (matches C's VP8GetSigned) for exact range-coder state
    v := VP8RdGetSigned(BR, v);
    // Dequantize: DC (n=0) uses Dq0, AC uses Dq1
    if n = 0 then
      Coeffs[kZigzag[0]] := Int16(v * Dq0)
    else
      Coeffs[kZigzag[n]] := Int16(v * Dq1);
    Inc(n);
    if n = 16 then Break;
    // p already set to BandsPtr[n]^.Probas[ctx_new] for next iteration
  end;
  Result := n;
end;

// Forward declaration needed: VP8TransformWHT is defined in the IDCT section below
procedure VP8TransformWHT(DC: PInt16; Out16: PInt16); forward;

// Parse residuals for one macroblock. Matches C ParseResiduals exactly.
// WHT for I16x16 is applied HERE and DCs injected into Coeffs[n*16+0].
function VP8ParseResiduals(var D: TVP8Decoder; MbX: Integer;
  var Part: TVP8Rd; PartIdx: Integer): Boolean;
var
  mb:        ^TVP8MBData;
  leftMB:    PVP8MB;   // left border  = D.MBInfo (index -1)
  topMB:     PVP8MB;   // current col  = D.MBInfo + (MbX+1)
  dqm:       ^TVP8QuantMatrix;
  dcBuf:     array[0..15] of Int16;  // Y2/WHT input (separate from Coeffs)
  dst:       PInt16;
  first:     Integer;
  acBands:   ^TBandPtrsRow;
  tnz, lnz:  Byte;
  l, nz_val: Integer;
  x, y, ch:  Integer;
  ctx:       Integer;
  nzCoeffs:  Cardinal;
  nonZeroY:  Cardinal;
  nonZeroUV: Cardinal;
  outTNZ, outLNZ: Byte;
begin
  mb      := @D.MBData;
  leftMB  := D.MBInfo;                                          // dec->mb_info - 1
  topMB   := PVP8MB(NativeUInt(D.MBInfo) + NativeUInt((MbX+1)*SizeOf(TVP8MB)));  // dec->mb_info + mb_x
  dqm     := @D.DQM[mb^.Segment];

  FillChar(mb^.Coeffs[0], SizeOf(mb^.Coeffs), 0);
  nonZeroY  := 0;
  nonZeroUV := 0;

  // === Y2 / WHT DC block (type 1), only for I16x16 ===
  if not mb^.IsI4x4 then
  begin
    FillChar(dcBuf, SizeOf(dcBuf), 0);
    ctx := Integer(topMB^.NZDC) + Integer(leftMB^.NZDC);
    nz_val := VP8GetCoeffsFast(Part, D.Proba.BandsPtr[1], ctx, 0,
                               dqm^.Y2Mat[0], dqm^.Y2Mat[1], @dcBuf[0]);
    topMB^.NZDC  := Byte(nz_val > 0);
    leftMB^.NZDC := Byte(nz_val > 0);
    if nz_val > 1 then
    begin
      // Full WHT: inject 16 DCs into each Y block's position 0
      VP8TransformWHT(@dcBuf[0], @dcBuf[0]);  // in-place into same buffer (uses tmp)
      for y := 0 to 15 do mb^.Coeffs[y * 16] := dcBuf[y];
    end else if nz_val = 1 then
    begin
      // Simplified: all 16 DCs get the same value (dc0+3)>>3 (arithmetic shift)
      nz_val := SarI(Integer(dcBuf[0]) + 3, 3);
      for y := 0 to 15 do mb^.Coeffs[y * 16] := Int16(nz_val);
    end;
    // else all zero — Coeffs already zero
    first := 1;
    acBands := @D.Proba.BandsPtr[0];
  end else
  begin
    first := 0;
    acBands := @D.Proba.BandsPtr[3];
  end;

  // === Y luma AC blocks (type 0 for I16x16, type 3 for I4x4) ===
  // Track NZ context using C's circular-buffer approach
  tnz := topMB^.NZ  and $0F;
  lnz := leftMB^.NZ and $0F;
  dst := @mb^.Coeffs[0];
  for y := 0 to 3 do
  begin
    l := lnz and 1;
    nzCoeffs := 0;
    for x := 0 to 3 do
    begin
      ctx    := l + (tnz and 1);
      nz_val := VP8GetCoeffsFast(Part, acBands^, ctx, first,
                                 dqm^.Y1Mat[0], dqm^.Y1Mat[1], dst);
      l      := Byte(nz_val > first);
      tnz    := Byte((tnz shr 1) or (l shl 7));
      // NzCodeBits: nzCoeffs = (nzCoeffs << 2) | (nz>3 ? 3 : nz>1 ? 2 : dc_nz)
      nzCoeffs := nzCoeffs shl 2;
      if nz_val > 3 then nzCoeffs := nzCoeffs or 3
      else if nz_val > 1 then nzCoeffs := nzCoeffs or 2
      else if dst[0] <> 0 then nzCoeffs := nzCoeffs or 1;
      Inc(dst, 16);
    end;
    tnz := tnz shr 4;
    lnz := Byte((lnz shr 1) or (l shl 7));
    nonZeroY := (nonZeroY shl 8) or nzCoeffs;
  end;
  outTNZ := tnz;
  outLNZ := lnz shr 4;
  mb^.NonZeroY := nonZeroY;

  // === UV chroma blocks (type 2): 2 channels × 2×2 blocks ===
  for ch := 0 to 1 do
  begin
    nzCoeffs := 0;
    tnz := topMB^.NZ  shr (4 + ch * 2);
    lnz := leftMB^.NZ shr (4 + ch * 2);
    for y := 0 to 1 do
    begin
      l := lnz and 1;
      for x := 0 to 1 do
      begin
        ctx    := l + (tnz and 1);
        nz_val := VP8GetCoeffsFast(Part, D.Proba.BandsPtr[2], ctx, 0,
                                   dqm^.UVMat[0], dqm^.UVMat[1], dst);
        l    := Byte(nz_val > 0);
        tnz  := Byte((tnz shr 1) or (l shl 3));
        nzCoeffs := nzCoeffs shl 2;
        if nz_val > 3 then nzCoeffs := nzCoeffs or 3
        else if nz_val > 1 then nzCoeffs := nzCoeffs or 2
        else if dst[0] <> 0 then nzCoeffs := nzCoeffs or 1;
        Inc(dst, 16);
      end;
      tnz := tnz shr 2;
      lnz := Byte((lnz shr 1) or (l shl 5));
    end;
    nonZeroUV := nonZeroUV or (nzCoeffs shl (4 * ch * 2));
    outTNZ    := outTNZ or Byte((tnz shl 4) shl (ch * 2));
    outLNZ    := outLNZ or Byte((lnz and $F0) shl (ch * 2));
  end;
  mb^.NonZeroUV := nonZeroUV;

  topMB^.NZ  := outTNZ;
  leftMB^.NZ := outLNZ;

  Result := (nonZeroY or nonZeroUV) = 0;  // True if skip (all zero)
end;

// ============================================================
// VP8 DSP: IDCT
// ============================================================

// 4x4 IDCT: C[16] coefficients (PInt16), adds to Pred, stores to Dst.
// Dst and Pred may be the same pointer (in-place add).
procedure VP8TransformOne(C: PInt16; Pred: PByte; Dst: PByte; Bps: Integer);
var
  tmp: array[0..3,0..3] of Integer;
  i: Integer;
  a, b, c2, d2: Integer;
  a0, a1, a2, a3: Integer;
begin
  // Vertical pass: process each column of the 4x4 coefficient block
  for i := 0 to 3 do
  begin
    a  := C[0+i] + C[8+i];
    b  := C[0+i] - C[8+i];
    // MUL1(x) = ((x*20091)>>16)+x;  MUL2(x) = (x*35468)>>16
    // Use SarI (arithmetic) because FPC shr is logical — wrong for negative values
    c2 := SarI(C[4+i] * 35468, 16) - (SarI(C[12+i] * 20091, 16) + C[12+i]);
    d2 := (SarI(C[4+i] * 20091, 16) + C[4+i]) + SarI(C[12+i] * 35468, 16);
    tmp[i,0] := a + d2;
    tmp[i,1] := b + c2;
    tmp[i,2] := b - c2;
    tmp[i,3] := a - d2;
  end;
  // Horizontal pass (process rows of tmp → output rows)
  for i := 0 to 3 do
  begin
    a  := tmp[0,i] + tmp[2,i];
    b  := tmp[0,i] - tmp[2,i];
    c2 := SarI(tmp[1,i] * 35468, 16) - (SarI(tmp[3,i] * 20091, 16) + tmp[3,i]);
    d2 := (SarI(tmp[1,i] * 20091, 16) + tmp[1,i]) + SarI(tmp[3,i] * 35468, 16);
    // Use SarI (arithmetic right shift) because FPC's shr is logical (unsigned)
    a0 := SarI(a + d2 + 4, 3);
    a1 := SarI(b + c2 + 4, 3);
    a2 := SarI(b - c2 + 4, 3);
    a3 := SarI(a - d2 + 4, 3);
    // Add prediction and clip
    (Dst + i * Bps + 0)^ := Clip8b(Integer((Pred + i * Bps + 0)^) + a0);
    (Dst + i * Bps + 1)^ := Clip8b(Integer((Pred + i * Bps + 1)^) + a1);
    (Dst + i * Bps + 2)^ := Clip8b(Integer((Pred + i * Bps + 2)^) + a2);
    (Dst + i * Bps + 3)^ := Clip8b(Integer((Pred + i * Bps + 3)^) + a3);
  end;
end;

// WHT transform: 16 DC coefficients (PInt16 DC) → 16 DCs (PInt16 Out16)
procedure VP8TransformWHT(DC: PInt16; Out16: PInt16);
var
  tmp: array[0..15] of Integer;
  i, a0, a1, a2, a3: Integer;
begin
  for i := 0 to 3 do
  begin
    a0 := DC[0+i] + DC[12+i];
    a1 := DC[4+i] + DC[ 8+i];
    a2 := DC[4+i] - DC[ 8+i];
    a3 := DC[0+i] - DC[12+i];
    tmp[0+i]  := a0 + a1;
    tmp[8+i]  := a0 - a1;
    tmp[4+i]  := a3 + a2;
    tmp[12+i] := a3 - a2;
  end;
  for i := 0 to 3 do
  begin
    a0 := tmp[0 + i*4] + tmp[3 + i*4];
    a1 := tmp[1 + i*4] + tmp[2 + i*4];
    a2 := tmp[1 + i*4] - tmp[2 + i*4];
    a3 := tmp[0 + i*4] - tmp[3 + i*4];
    Out16[0 + i*4] := Int16(SarI(a0 + a1 + 3, 3));
    Out16[1 + i*4] := Int16(SarI(a3 + a2 + 3, 3));
    Out16[2 + i*4] := Int16(SarI(a0 - a1 + 3, 3));
    Out16[3 + i*4] := Int16(SarI(a3 - a2 + 3, 3));
  end;
end;

// ============================================================
// VP8 DSP: INTRA PREDICTION
// ============================================================

// Fill a 16x16 or 8x8 block with a constant value
procedure Fill(Dst: PByte; Val: Byte; W, H, Stride: Integer);
var r: Integer;
begin
  for r := 0 to H-1 do
    FillChar((Dst + r * Stride)^, W, Val);
end;

// ---- 16x16 luma prediction ----
procedure I16x16_DC(Dst: PByte; Top, Left: PByte; Stride: Integer);
var sum, i: Integer;
begin
  sum := 0;
  for i := 0 to 15 do begin Inc(sum, (Left + i)^); Inc(sum, (Top + i)^); end;
  Fill(Dst, Byte((sum + 16) shr 5), 16, 16, Stride);
end;

procedure I16x16_DC_Left(Dst: PByte; Left: PByte; Stride: Integer);
var sum, i: Integer;
begin
  sum := 0;
  for i := 0 to 15 do Inc(sum, (Left + i)^);
  Fill(Dst, Byte((sum + 8) shr 4), 16, 16, Stride);
end;

procedure I16x16_DC_Top(Dst: PByte; Top: PByte; Stride: Integer);
var sum, i: Integer;
begin
  sum := 0;
  for i := 0 to 15 do Inc(sum, (Top + i)^);
  Fill(Dst, Byte((sum + 8) shr 4), 16, 16, Stride);
end;

procedure I16x16_V(Dst: PByte; Top: PByte; Stride: Integer);
var r: Integer;
begin
  for r := 0 to 15 do
    Move(Top^, (Dst + r * Stride)^, 16);
end;

procedure I16x16_H(Dst: PByte; Left: PByte; Stride: Integer);
var r: Integer;
begin
  for r := 0 to 15 do
    FillChar((Dst + r * Stride)^, 16, (Left + r)^);
end;

procedure I16x16_TM(Dst: PByte; Top, Left: PByte; TopLeft: Byte; Stride: Integer);
var r, c, v: Integer;
begin
  for r := 0 to 15 do
    for c := 0 to 15 do
    begin
      v := Integer((Left + r)^) + Integer((Top + c)^) - Integer(TopLeft);
      (Dst + r * Stride + c)^ := Clip8b(v);
    end;
end;

// Predict 16x16 luma into YuvBuf (dst=@YuvBuf[Y_OFF + mbx*16])
// TopCtx: top row Y samples, LeftCtx: left column Y samples
// HasTop, HasLeft: border flags
procedure VP8PredLuma16(Mode: Integer; Dst: PByte; TopCtx, LeftCtx: PByte;
  HasTop, HasLeft: Boolean; Stride: Integer);
var topLeft: Byte;
    tmpLeft: array[0..15] of Byte;
    tmpTop:  array[0..15] of Byte;
begin
  if not HasTop  then FillChar(tmpTop,  16, 127) else Move(TopCtx^, tmpTop, 16);
  if not HasLeft then FillChar(tmpLeft, 16, 129) else Move(LeftCtx^, tmpLeft, 16);
  // Top-left corner lives at Dst[-Stride-1] in the YUV buffer.
  // For mby=0 it was initialised to 127; for mby>0,mbx=0 to 129; for mbx>0 the
  // column-rotation copies the last byte of the previous MB's top-row here.
  // Always read the buffer directly — do NOT override with 129 when HasTop=False,
  // because TM_PRED requires the actual value (127 for the first MB row, not 129).
  topLeft := (Dst - Stride - 1)^;
  case Mode of
    DC_PRED:
      if HasTop and HasLeft then I16x16_DC(Dst, @tmpTop[0], @tmpLeft[0], Stride)
      else if HasLeft        then I16x16_DC_Left(Dst, @tmpLeft[0], Stride)
      else if HasTop         then I16x16_DC_Top(Dst, @tmpTop[0], Stride)
      else                        Fill(Dst, 128, 16, 16, Stride);
    V_PRED:  I16x16_V(Dst, @tmpTop[0], Stride);
    H_PRED:  I16x16_H(Dst, @tmpLeft[0], Stride);
    TM_PRED: I16x16_TM(Dst, @tmpTop[0], @tmpLeft[0], topLeft, Stride);
  end;
end;

// ---- 8x8 chroma prediction ----
procedure I8x8_DC(Dst: PByte; Top, Left: PByte; HasTop, HasLeft: Boolean; Stride: Integer);
var sum, i: Integer;
begin
  sum := 0;
  if HasTop  then for i := 0 to 7 do Inc(sum, (Top  + i)^);
  if HasLeft then for i := 0 to 7 do Inc(sum, (Left + i)^);
  if HasTop and HasLeft then Fill(Dst, Byte((sum + 8) shr 4), 8, 8, Stride)
  else if HasTop  then Fill(Dst, Byte((sum + 4) shr 3), 8, 8, Stride)
  else if HasLeft then Fill(Dst, Byte((sum + 4) shr 3), 8, 8, Stride)
  else                 Fill(Dst, 128, 8, 8, Stride);
end;

procedure I8x8_V(Dst: PByte; Top: PByte; Stride: Integer);
var r: Integer;
begin
  for r := 0 to 7 do Move(Top^, (Dst + r * Stride)^, 8);
end;

procedure I8x8_H(Dst: PByte; Left: PByte; Stride: Integer);
var r: Integer;
begin
  for r := 0 to 7 do FillChar((Dst + r * Stride)^, 8, (Left + r)^);
end;

procedure I8x8_TM(Dst: PByte; Top, Left: PByte; TL: Byte; Stride: Integer);
var r, c, v: Integer;
begin
  for r := 0 to 7 do
    for c := 0 to 7 do
    begin
      v := Integer((Left + r)^) + Integer((Top + c)^) - Integer(TL);
      (Dst + r * Stride + c)^ := Clip8b(v);
    end;
end;

procedure VP8PredChroma8(Mode: Integer; Dst: PByte; TopCtx, LeftCtx: PByte;
  HasTop, HasLeft: Boolean; Stride: Integer);
var tmpLeft: array[0..7] of Byte;
    tmpTop:  array[0..7] of Byte;
    tl: Byte;
begin
  if not HasTop  then FillChar(tmpTop,  8, 127) else Move(TopCtx^, tmpTop, 8);
  if not HasLeft then FillChar(tmpLeft, 8, 129) else Move(LeftCtx^, tmpLeft, 8);
  tl := (Dst - Stride - 1)^;
  case Mode of
    DC_PRED: I8x8_DC(Dst, @tmpTop[0], @tmpLeft[0], HasTop, HasLeft, Stride);
    V_PRED:  I8x8_V(Dst, @tmpTop[0], Stride);
    H_PRED:  I8x8_H(Dst, @tmpLeft[0], Stride);
    TM_PRED: I8x8_TM(Dst, @tmpTop[0], @tmpLeft[0], tl, Stride);
  end;
end;

// ---- 4x4 luma intra prediction (for I4x4 macroblocks) ----
// Returns average of 4 bytes at p
function Avg4(a,b,c,d: Integer): Byte; inline;
begin Result := Byte((a+b+c+d+2) shr 2); end;

function Avg3(a,b,c: Integer): Byte; inline;
begin Result := Byte((a+2*b+c+2) shr 2); end;

function Avg2(a,b: Integer): Byte; inline;
begin Result := Byte((a+b+1) shr 1); end;

procedure I4x4_DC(Dst: PByte; Top: PByte; Left: PByte; Stride: Integer);
var s: Integer;
begin
  s := (Top+0)^ + (Top+1)^ + (Top+2)^ + (Top+3)^ +
       (Left+0)^ + (Left+1)^ + (Left+2)^ + (Left+3)^ + 4;
  Fill(Dst, Byte(s shr 3), 4, 4, Stride);
end;

procedure I4x4_TM(Dst: PByte; Top, Left: PByte; TL: Byte; Stride: Integer);
var r, c: Integer;
begin
  for r := 0 to 3 do
    for c := 0 to 3 do
      (Dst + r*Stride + c)^ := Clip8b((Left+r)^ + (Top+c)^ - TL);
end;

procedure I4x4_VE(Dst: PByte; Top: PByte; Stride: Integer);
// Vertical (extrapolate from top)
var r: Integer;
    vals: array[0..3] of Byte;
begin
  vals[0] := Avg3((Top-1)^, (Top+0)^, (Top+1)^);
  vals[1] := Avg3((Top+0)^, (Top+1)^, (Top+2)^);
  vals[2] := Avg3((Top+1)^, (Top+2)^, (Top+3)^);
  vals[3] := Avg3((Top+2)^, (Top+3)^, (Top+4)^);
  for r := 0 to 3 do
    Move(vals[0], (Dst + r*Stride)^, 4);
end;

procedure I4x4_HE(Dst: PByte; Left: PByte; TL: Byte; Stride: Integer);
var c: array[0..3] of Byte;
begin
  c[0] := Avg3(TL,         (Left+0)^, (Left+1)^);
  c[1] := Avg3((Left+0)^,  (Left+1)^, (Left+2)^);
  c[2] := Avg3((Left+1)^,  (Left+2)^, (Left+3)^);
  c[3] := Avg3((Left+2)^,  (Left+3)^, (Left+3)^); // last repeats
  FillChar((Dst + 0*Stride)^, 4, c[0]);
  FillChar((Dst + 1*Stride)^, 4, c[1]);
  FillChar((Dst + 2*Stride)^, 4, c[2]);
  FillChar((Dst + 3*Stride)^, 4, c[3]);
end;

procedure I4x4_RD(Dst: PByte; Top, Left: PByte; TL: Byte; Stride: Integer);
// DST(x,y) = Dst[y*Stride+x]
// X=TL, I=Left[0], J=Left[1], K=Left[2], L=Left[3], A..D=Top[0..3]
var X, I, J, K, L, A, B, C, D: Integer;
begin
  X := TL;          I := (Left+0)^; J := (Left+1)^;
  K := (Left+2)^;   L := (Left+3)^;
  A := (Top+0)^;    B := (Top+1)^;  C := (Top+2)^; D := (Top+3)^;
  (Dst + 3*Stride + 0)^ := Avg3(J, K, L);
  (Dst + 3*Stride + 1)^ := Avg3(I, J, K);
  (Dst + 2*Stride + 0)^ := Avg3(I, J, K);
  (Dst + 3*Stride + 2)^ := Avg3(X, I, J);
  (Dst + 2*Stride + 1)^ := Avg3(X, I, J);
  (Dst + 1*Stride + 0)^ := Avg3(X, I, J);
  (Dst + 3*Stride + 3)^ := Avg3(A, X, I);
  (Dst + 2*Stride + 2)^ := Avg3(A, X, I);
  (Dst + 1*Stride + 1)^ := Avg3(A, X, I);
  (Dst + 0*Stride + 0)^ := Avg3(A, X, I);
  (Dst + 2*Stride + 3)^ := Avg3(B, A, X);
  (Dst + 1*Stride + 2)^ := Avg3(B, A, X);
  (Dst + 0*Stride + 1)^ := Avg3(B, A, X);
  (Dst + 1*Stride + 3)^ := Avg3(C, B, A);
  (Dst + 0*Stride + 2)^ := Avg3(C, B, A);
  (Dst + 0*Stride + 3)^ := Avg3(D, C, B);
end;

procedure I4x4_LD(Dst: PByte; Top: PByte; Stride: Integer);
var t: array[0..7] of Integer;
begin
  t[0]:=(Top+0)^; t[1]:=(Top+1)^; t[2]:=(Top+2)^; t[3]:=(Top+3)^;
  t[4]:=(Top+4)^; t[5]:=(Top+5)^; t[6]:=(Top+6)^; t[7]:=(Top+7)^;
  (Dst+0*Stride+0)^:=Avg3(t[0],t[1],t[2]); (Dst+0*Stride+1)^:=Avg3(t[1],t[2],t[3]);
  (Dst+0*Stride+2)^:=Avg3(t[2],t[3],t[4]); (Dst+0*Stride+3)^:=Avg3(t[3],t[4],t[5]);
  (Dst+1*Stride+0)^:=Avg3(t[1],t[2],t[3]); (Dst+1*Stride+1)^:=Avg3(t[2],t[3],t[4]);
  (Dst+1*Stride+2)^:=Avg3(t[3],t[4],t[5]); (Dst+1*Stride+3)^:=Avg3(t[4],t[5],t[6]);
  (Dst+2*Stride+0)^:=Avg3(t[2],t[3],t[4]); (Dst+2*Stride+1)^:=Avg3(t[3],t[4],t[5]);
  (Dst+2*Stride+2)^:=Avg3(t[4],t[5],t[6]); (Dst+2*Stride+3)^:=Avg3(t[5],t[6],t[7]);
  (Dst+3*Stride+0)^:=Avg3(t[3],t[4],t[5]); (Dst+3*Stride+1)^:=Avg3(t[4],t[5],t[6]);
  (Dst+3*Stride+2)^:=Avg3(t[5],t[6],t[7]); (Dst+3*Stride+3)^:=Avg3(t[6],t[7],t[7]);
end;

procedure I4x4_VR(Dst: PByte; Top, Left: PByte; TL: Byte; Stride: Integer);
// Matches VR4_C: DST(x,y) = (Dst + y*Stride + x)^
// X=TL, I=Left[0], J=Left[1], K=Left[2]; A..D=Top[0..3]
var X, I, J, K, A, B, C, D: Integer;
begin
  X := TL;        I := (Left+0)^; J := (Left+1)^; K := (Left+2)^;
  A := (Top+0)^;  B := (Top+1)^;  C := (Top+2)^;  D := (Top+3)^;
  // DST(0,0)=DST(1,2)=Avg2(X,A)
  (Dst+0*Stride+0)^ := Avg2(X,A);  (Dst+2*Stride+1)^ := Avg2(X,A);
  // DST(1,0)=DST(2,2)=Avg2(A,B)
  (Dst+0*Stride+1)^ := Avg2(A,B);  (Dst+2*Stride+2)^ := Avg2(A,B);
  // DST(2,0)=DST(3,2)=Avg2(B,C)
  (Dst+0*Stride+2)^ := Avg2(B,C);  (Dst+2*Stride+3)^ := Avg2(B,C);
  // DST(3,0)=Avg2(C,D)
  (Dst+0*Stride+3)^ := Avg2(C,D);
  // DST(0,1)=DST(1,3)=Avg3(I,X,A)
  (Dst+1*Stride+0)^ := Avg3(I,X,A);  (Dst+3*Stride+1)^ := Avg3(I,X,A);
  // DST(1,1)=DST(2,3)=Avg3(X,A,B)
  (Dst+1*Stride+1)^ := Avg3(X,A,B);  (Dst+3*Stride+2)^ := Avg3(X,A,B);
  // DST(2,1)=DST(3,3)=Avg3(A,B,C)
  (Dst+1*Stride+2)^ := Avg3(A,B,C);  (Dst+3*Stride+3)^ := Avg3(A,B,C);
  // DST(3,1)=Avg3(B,C,D)
  (Dst+1*Stride+3)^ := Avg3(B,C,D);
  // DST(0,2)=Avg3(J,I,X)
  (Dst+2*Stride+0)^ := Avg3(J,I,X);
  // DST(0,3)=Avg3(K,J,I)
  (Dst+3*Stride+0)^ := Avg3(K,J,I);
end;

procedure I4x4_VL(Dst: PByte; Top: PByte; Stride: Integer);
var t: array[0..7] of Integer;
begin
  t[0]:=(Top+0)^; t[1]:=(Top+1)^; t[2]:=(Top+2)^; t[3]:=(Top+3)^;
  t[4]:=(Top+4)^; t[5]:=(Top+5)^; t[6]:=(Top+6)^; t[7]:=(Top+7)^;
  (Dst+0*Stride+0)^:=Avg2(t[0],t[1]); (Dst+0*Stride+1)^:=Avg2(t[1],t[2]);
  (Dst+0*Stride+2)^:=Avg2(t[2],t[3]); (Dst+0*Stride+3)^:=Avg2(t[3],t[4]);
  (Dst+1*Stride+0)^:=Avg3(t[0],t[1],t[2]); (Dst+1*Stride+1)^:=Avg3(t[1],t[2],t[3]);
  (Dst+1*Stride+2)^:=Avg3(t[2],t[3],t[4]); (Dst+1*Stride+3)^:=Avg3(t[3],t[4],t[5]);
  (Dst+2*Stride+0)^:=Avg2(t[1],t[2]); (Dst+2*Stride+1)^:=Avg2(t[2],t[3]);
  (Dst+2*Stride+2)^:=Avg2(t[3],t[4]); (Dst+2*Stride+3)^:=Avg3(t[4],t[5],t[6]);
  (Dst+3*Stride+0)^:=Avg3(t[1],t[2],t[3]); (Dst+3*Stride+1)^:=Avg3(t[2],t[3],t[4]);
  (Dst+3*Stride+2)^:=Avg3(t[3],t[4],t[5]); (Dst+3*Stride+3)^:=Avg3(t[5],t[6],t[7]);
end;

procedure I4x4_HD(Dst: PByte; Top, Left: PByte; TL: Byte; Stride: Integer);
// Matches HD4_C: DST(x,y) = (Dst + y*Stride + x)^
// X=TL, I=Left[0], J=Left[1], K=Left[2], L=Left[3]; A..C=Top[0..2], D=Top[3]
var X, I, J, K, L, A, B, C: Integer;
begin
  X := TL;        I := (Left+0)^; J := (Left+1)^; K := (Left+2)^; L := (Left+3)^;
  A := (Top+0)^;  B := (Top+1)^;  C := (Top+2)^;  //D := (Top+3)^;
  // DST(0,0)=DST(2,1)=Avg2(I,X)
  (Dst+0*Stride+0)^ := Avg2(I,X);  (Dst+1*Stride+2)^ := Avg2(I,X);
  // DST(0,1)=DST(2,2)=Avg2(J,I)
  (Dst+1*Stride+0)^ := Avg2(J,I);  (Dst+2*Stride+2)^ := Avg2(J,I);
  // DST(0,2)=DST(2,3)=Avg2(K,J)
  (Dst+2*Stride+0)^ := Avg2(K,J);  (Dst+3*Stride+2)^ := Avg2(K,J);
  // DST(0,3)=Avg2(L,K)
  (Dst+3*Stride+0)^ := Avg2(L,K);
  // DST(3,0)=Avg3(A,B,C)
  (Dst+0*Stride+3)^ := Avg3(A,B,C);
  // DST(2,0)=Avg3(X,A,B)
  (Dst+0*Stride+2)^ := Avg3(X,A,B);
  // DST(1,0)=DST(3,1)=Avg3(I,X,A)
  (Dst+0*Stride+1)^ := Avg3(I,X,A);  (Dst+1*Stride+3)^ := Avg3(I,X,A);
  // DST(1,1)=DST(3,2)=Avg3(J,I,X)
  (Dst+1*Stride+1)^ := Avg3(J,I,X);  (Dst+2*Stride+3)^ := Avg3(J,I,X);
  // DST(1,2)=DST(3,3)=Avg3(K,J,I)
  (Dst+2*Stride+1)^ := Avg3(K,J,I);  (Dst+3*Stride+3)^ := Avg3(K,J,I);
  // DST(1,3)=Avg3(L,K,J)
  (Dst+3*Stride+1)^ := Avg3(L,K,J);
  // Note: D (Top[3]) is not used in HD4
  //D := D; // suppress hint
end;

procedure I4x4_HU(Dst: PByte; Left: PByte; Stride: Integer);
var l: array[0..3] of Integer;
begin
  l[0]:=(Left+0)^; l[1]:=(Left+1)^; l[2]:=(Left+2)^; l[3]:=(Left+3)^;
  (Dst+0*Stride+0)^:=Avg2(l[0],l[1]); (Dst+0*Stride+1)^:=Avg3(l[0],l[1],l[2]);
  (Dst+0*Stride+2)^:=Avg2(l[1],l[2]); (Dst+0*Stride+3)^:=Avg3(l[1],l[2],l[3]);
  (Dst+1*Stride+0)^:=Avg2(l[1],l[2]); (Dst+1*Stride+1)^:=Avg3(l[1],l[2],l[3]);
  (Dst+1*Stride+2)^:=Avg2(l[2],l[3]); (Dst+1*Stride+3)^:=Avg3(l[2],l[3],l[3]);
  (Dst+2*Stride+0)^:=Avg2(l[2],l[3]); (Dst+2*Stride+1)^:=Avg3(l[2],l[3],l[3]);
  (Dst+2*Stride+2)^:=l[3];             (Dst+2*Stride+3)^:=l[3];
  (Dst+3*Stride+0)^:=l[3]; (Dst+3*Stride+1)^:=l[3];
  (Dst+3*Stride+2)^:=l[3]; (Dst+3*Stride+3)^:=l[3];
end;

// Predict one 4x4 block in the luma plane
// TopSamples: 8 bytes (4 top + 4 top-right) at Top[0..7]
// LeftSamples: 4 bytes at Left[0..3]
// TopLeft: single byte (top-left corner)
procedure VP8PredLuma4(Mode: Integer; Dst: PByte; Top, Left: PByte;
  TL: Byte; Stride: Integer);
begin
  case Mode of
    B_DC_PRED: I4x4_DC(Dst, Top, Left, Stride);
    B_TM_PRED: I4x4_TM(Dst, Top, Left, TL, Stride);
    B_VE_PRED: I4x4_VE(Dst, Top, Stride);
    B_HE_PRED: I4x4_HE(Dst, Left, TL, Stride);
    B_RD_PRED: I4x4_RD(Dst, Top, Left, TL, Stride);
    B_VR_PRED: I4x4_VR(Dst, Top, Left, TL, Stride);
    B_LD_PRED: I4x4_LD(Dst, Top, Stride);
    B_VL_PRED: I4x4_VL(Dst, Top, Stride);
    B_HD_PRED: I4x4_HD(Dst, Top, Left, TL, Stride);
    B_HU_PRED: I4x4_HU(Dst, Left, Stride);
  end;
end;

// ============================================================
// VP8 MACROBLOCK RECONSTRUCTION
// ============================================================

// Copy 4 bytes: used for left-context updates
procedure Copy4(Dst, Src: PByte); inline;
begin
  PCardinal(Dst)^ := PCardinal(Src)^;
end;

// Reconstruct one macroblock into YuvBuf
// D.MBData must have been populated by VP8ParseResiduals
// YBuf = @YuvBuf[Y_OFF], UBuf = @YuvBuf[U_OFF], VBuf = @YuvBuf[V_OFF]
procedure VP8ReconstructMB(var D: TVP8Decoder; MbX: Integer;
  HasTop, HasLeft: Boolean);
var
  mb:   ^TVP8MBData;
  y, x, n: Integer;
  yBase, uBase, vBase: PByte;
  topY, topU, topV: PByte;
  leftY: array[0..15] of Byte;
  leftU, leftV: array[0..7] of Byte;
  yDst, uDst, vDst: PByte;
  leftCol: array[0..15] of Byte;
begin
  mb := @D.MBData;
  yBase := @D.YuvBuf[Y_OFF];
  uBase := @D.YuvBuf[U_OFF];
  vBase := @D.YuvBuf[V_OFF];

  // Top-row context pointers
  topY := D.YTopBuf + MbX * 16;
  topU := D.UTopBuf + MbX * 8;
  topV := D.VTopBuf + MbX * 8;

  // Left-column context: read from YuvBuf border pixels
  // Left Y: column -1 of Y = yBase - 1, rows 0..15
  // Left U: column -1 of U = uBase - 1, rows 0..7
  if HasLeft then
  begin
    for y := 0 to 15 do leftY[y] := (yBase + y * BPS - 1)^;
    for y := 0 to  7 do leftU[y] := (uBase + y * BPS - 1)^;
    for y := 0 to  7 do leftV[y] := (vBase + y * BPS - 1)^;
  end else
  begin
    FillChar(leftY, 16, 129);
    FillChar(leftU,  8, 129);
    FillChar(leftV,  8, 129);
  end;

  // Luma prediction
  if not mb^.IsI4x4 then
  begin
    // I16x16: predict then apply residuals per 4x4 block
    // WHT DCs were already injected into mb^.Coeffs[n*16+0] by VP8ParseResiduals
    VP8PredLuma16(mb^.IModes[0], yBase, topY, @leftY[0], HasTop, HasLeft, BPS);
    for y := 0 to 3 do
      for x := 0 to 3 do
      begin
        n := y * 4 + x;
        yDst := yBase + kScan[n];
        VP8TransformOne(@mb^.Coeffs[n*16], yDst, yDst, BPS);
      end;
  end else
  begin
    // I4x4: predict each 4x4 sub-block independently, then apply residuals
    for n := 0 to 15 do
    begin
      //x := n and 3;
      //y := n shr 2;
      yDst := yBase + kScan[n];
      // Collect left column (4 pixels, strided BPS apart) into contiguous temp
      leftCol[0] := (yDst - 1 + 0*BPS)^;
      leftCol[1] := (yDst - 1 + 1*BPS)^;
      leftCol[2] := (yDst - 1 + 2*BPS)^;
      leftCol[3] := (yDst - 1 + 3*BPS)^;
      VP8PredLuma4(mb^.IModes[n], yDst,
                   yDst - BPS,       // top row (4+4 bytes available)
                   @leftCol[0],      // left column (contiguous 4 bytes)
                   (yDst - BPS - 1)^,
                   BPS);
      // Apply IDCT residuals
      VP8TransformOne(@mb^.Coeffs[n*16], yDst, yDst, BPS);
    end;
  end;

  // Chroma prediction (8x8 U and V)
  VP8PredChroma8(mb^.UVMode, uBase, topU, @leftU[0], HasTop, HasLeft, BPS);
  VP8PredChroma8(mb^.UVMode, vBase, topV, @leftV[0], HasTop, HasLeft, BPS);
  // Apply chroma IDCT (4 blocks each for U and V)
  for n := 0 to 3 do
  begin
    x := n and 1; y := n shr 1;
    uDst := uBase + (x*4) + (y*4*BPS);
    vDst := vBase + (x*4) + (y*4*BPS);
    VP8TransformOne(@mb^.Coeffs[(16+n)*16], uDst, uDst, BPS);
    VP8TransformOne(@mb^.Coeffs[(20+n)*16], vDst, vDst, BPS);
  end;

  // Update top-row context
  Move((yBase + 15*BPS)^, topY^, 16);
  Move((uBase +  7*BPS)^, topU^,  8);
  Move((vBase +  7*BPS)^, topV^,  8);
end;

// ============================================================
// YUV -> RGB OUTPUT CONVERSION
// ============================================================

// Output one row of pixels from the YUV buffer into the output buffer.
// OutputRow: destination (pre-positioned)
// Y, U, V: source row pointers (Y has 'width' pixels, U/V have width/2)
// Width: number of Y pixels
procedure EmitRGBRow(Y, U, V: PByte; Width: Integer;
  Dst: PByte; Mode: TCSMode; Bpp: Integer);
var x: Integer;
    yv, uv, vv: Integer;
    r, g, b: Byte;
    p: PByte;
begin
  for x := 0 to Width-1 do
  begin
    yv := Y[x];
    uv := U[x shr 1];
    vv := V[x shr 1];
    r := YuvToR(yv, vv);
    g := YuvToG(yv, uv, vv);
    b := YuvToB(yv, uv);
    p := Dst + x * Bpp;
    case Mode of
      csmRGBA: begin p[0]:=r; p[1]:=g; p[2]:=b; p[3]:=255; end;
      csmARGB: begin p[0]:=255; p[1]:=r; p[2]:=g; p[3]:=b; end;
      csmBGRA: begin p[0]:=b; p[1]:=g; p[2]:=r; p[3]:=255; end;
      csmRGB:  begin p[0]:=r; p[1]:=g; p[2]:=b; end;
      csmBGR:  begin p[0]:=b; p[1]:=g; p[2]:=r; end;
    end;
  end;
end;

// ============================================================
// VP8 IN-LOOP FILTER  (port of dsp/dec.c + frame_dec.c)
// ============================================================

function FSclip1(v: Integer): Integer; inline;
begin if v < -128 then Result := -128 else if v > 127 then Result := 127 else Result := v; end;
function FSclip2(v: Integer): Integer; inline;
begin if v < -16 then Result := -16 else if v > 15 then Result := 15 else Result := v; end;
function FClip1(v: Integer): Integer; inline;
begin if v < 0 then Result := 0 else if v > 255 then Result := 255 else Result := v; end;

// 4 pixels in, 2 pixels out
procedure FDoFilter2(p: PByte; step: Integer); inline;
var p1, p0, q0, q1, a, a1, a2: Integer;
begin
  p1 := (p + (-2*step))^; p0 := (p + (-step))^; q0 := p^; q1 := (p + step)^;
  a  := 3*(q0 - p0) + FSclip1(p1 - q1);
  a1 := FSclip2(SarI(a + 4, 3));
  a2 := FSclip2(SarI(a + 3, 3));
  (p + (-step))^ := Byte(FClip1(p0 + a2));
  p^             := Byte(FClip1(q0 - a1));
end;

// 4 pixels in, 4 pixels out
procedure FDoFilter4(p: PByte; step: Integer); inline;
var p1, p0, q0, q1, a, a1, a2, a3: Integer;
begin
  p1 := (p + (-2*step))^; p0 := (p + (-step))^; q0 := p^; q1 := (p + step)^;
  a  := 3*(q0 - p0);
  a1 := FSclip2(SarI(a + 4, 3));
  a2 := FSclip2(SarI(a + 3, 3));
  a3 := SarI(a1 + 1, 1);
  (p + (-2*step))^ := Byte(FClip1(p1 + a3));
  (p + (-step))^   := Byte(FClip1(p0 + a2));
  p^               := Byte(FClip1(q0 - a1));
  (p + step)^      := Byte(FClip1(q1 - a3));
end;

// 6 pixels in, 6 pixels out
procedure FDoFilter6(p: PByte; step: Integer); inline;
var p2, p1, p0, q0, q1, q2, a, a1, a2, a3: Integer;
begin
  p2 := (p + (-3*step))^; p1 := (p + (-2*step))^; p0 := (p + (-step))^;
  q0 := p^; q1 := (p + step)^; q2 := (p + 2*step)^;
  a  := FSclip1(3*(q0 - p0) + FSclip1(p1 - q1));
  a1 := SarI(27*a + 63, 7);
  a2 := SarI(18*a + 63, 7);
  a3 := SarI(9*a  + 63, 7);
  (p + (-3*step))^ := Byte(FClip1(p2 + a3));
  (p + (-2*step))^ := Byte(FClip1(p1 + a2));
  (p + (-step))^   := Byte(FClip1(p0 + a1));
  p^               := Byte(FClip1(q0 - a1));
  (p + step)^      := Byte(FClip1(q1 - a2));
  (p + 2*step)^    := Byte(FClip1(q2 - a3));
end;

function FHev(p: PByte; step, thresh: Integer): Boolean; inline;
var p1, p0, q0, q1: Integer;
begin
  p1 := (p + (-2*step))^; p0 := (p + (-step))^; q0 := p^; q1 := (p + step)^;
  Result := (Abs(p1 - p0) > thresh) or (Abs(q1 - q0) > thresh);
end;

function FNeedsFilter(p: PByte; step, t: Integer): Boolean; inline;
var p1, p0, q0, q1: Integer;
begin
  p1 := (p + (-2*step))^; p0 := (p + (-step))^; q0 := p^; q1 := (p + step)^;
  Result := (4*Abs(p0 - q0) + Abs(p1 - q1)) <= t;
end;

function FNeedsFilter2(p: PByte; step, t, it: Integer): Boolean; inline;
var p3, p2, p1, p0, q0, q1, q2, q3: Integer;
begin
  p3 := (p + (-4*step))^; p2 := (p + (-3*step))^; p1 := (p + (-2*step))^; p0 := (p + (-step))^;
  q0 := p^; q1 := (p + step)^; q2 := (p + 2*step)^; q3 := (p + 3*step)^;
  if (4*Abs(p0 - q0) + Abs(p1 - q1)) > t then begin Result := False; Exit; end;
  Result := (Abs(p3-p2) <= it) and (Abs(p2-p1) <= it) and (Abs(p1-p0) <= it)
        and (Abs(q3-q2) <= it) and (Abs(q2-q1) <= it) and (Abs(q1-q0) <= it);
end;

// --- Simple filter (luma only) ---
procedure FSimpleVFilter16(p: PByte; stride, thresh: Integer);
var i, t2: Integer;
begin
  t2 := 2*thresh + 1;
  for i := 0 to 15 do
    if FNeedsFilter(p + i, stride, t2) then FDoFilter2(p + i, stride);
end;
procedure FSimpleHFilter16(p: PByte; stride, thresh: Integer);
var i, t2: Integer;
begin
  t2 := 2*thresh + 1;
  for i := 0 to 15 do
    if FNeedsFilter(p + i*stride, 1, t2) then FDoFilter2(p + i*stride, 1);
end;
procedure FSimpleVFilter16i(p: PByte; stride, thresh: Integer);
var k: Integer;
begin
  for k := 3 downto 1 do begin p := p + 4*stride; FSimpleVFilter16(p, stride, thresh); end;
end;
procedure FSimpleHFilter16i(p: PByte; stride, thresh: Integer);
var k: Integer;
begin
  for k := 3 downto 1 do begin p := p + 4; FSimpleHFilter16(p, stride, thresh); end;
end;

// --- Complex filter ---
procedure FFilterLoop26(p: PByte; hstride, vstride, size, thresh, ithresh, hevt: Integer);
var t2: Integer;
begin
  t2 := 2*thresh + 1;
  while size > 0 do
  begin
    if FNeedsFilter2(p, hstride, t2, ithresh) then
    begin
      if FHev(p, hstride, hevt) then FDoFilter2(p, hstride)
      else FDoFilter6(p, hstride);
    end;
    p := p + vstride;
    Dec(size);
  end;
end;
procedure FFilterLoop24(p: PByte; hstride, vstride, size, thresh, ithresh, hevt: Integer);
var t2: Integer;
begin
  t2 := 2*thresh + 1;
  while size > 0 do
  begin
    if FNeedsFilter2(p, hstride, t2, ithresh) then
    begin
      if FHev(p, hstride, hevt) then FDoFilter2(p, hstride)
      else FDoFilter4(p, hstride);
    end;
    p := p + vstride;
    Dec(size);
  end;
end;

procedure FVFilter16(p: PByte; stride, thresh, ithresh, hevt: Integer);
begin FFilterLoop26(p, stride, 1, 16, thresh, ithresh, hevt); end;
procedure FHFilter16(p: PByte; stride, thresh, ithresh, hevt: Integer);
begin FFilterLoop26(p, 1, stride, 16, thresh, ithresh, hevt); end;
procedure FVFilter16i(p: PByte; stride, thresh, ithresh, hevt: Integer);
var k: Integer;
begin
  for k := 3 downto 1 do begin p := p + 4*stride; FFilterLoop24(p, stride, 1, 16, thresh, ithresh, hevt); end;
end;
procedure FHFilter16i(p: PByte; stride, thresh, ithresh, hevt: Integer);
var k: Integer;
begin
  for k := 3 downto 1 do begin p := p + 4; FFilterLoop24(p, 1, stride, 16, thresh, ithresh, hevt); end;
end;
procedure FVFilter8(u, v: PByte; stride, thresh, ithresh, hevt: Integer);
begin
  FFilterLoop26(u, stride, 1, 8, thresh, ithresh, hevt);
  FFilterLoop26(v, stride, 1, 8, thresh, ithresh, hevt);
end;
procedure FHFilter8(u, v: PByte; stride, thresh, ithresh, hevt: Integer);
begin
  FFilterLoop26(u, 1, stride, 8, thresh, ithresh, hevt);
  FFilterLoop26(v, 1, stride, 8, thresh, ithresh, hevt);
end;
procedure FVFilter8i(u, v: PByte; stride, thresh, ithresh, hevt: Integer);
begin
  FFilterLoop24(u + 4*stride, stride, 1, 8, thresh, ithresh, hevt);
  FFilterLoop24(v + 4*stride, stride, 1, 8, thresh, ithresh, hevt);
end;
procedure FHFilter8i(u, v: PByte; stride, thresh, ithresh, hevt: Integer);
begin
  FFilterLoop24(u + 4, 1, stride, 8, thresh, ithresh, hevt);
  FFilterLoop24(v + 4, 1, stride, 8, thresh, ithresh, hevt);
end;

// Precompute per-segment / per-mode filter strengths (frame_dec.c)
procedure VP8PrecomputeFilterStrengths(var D: TVP8Decoder);
var s, i4x4, baseLevel, level, ilevel: Integer;
begin
  if D.FilterType = 0 then Exit;
  for s := 0 to NUM_MB_SEGMENTS-1 do
  begin
    if D.SegHdr.UseSegment then
    begin
      baseLevel := D.SegHdr.FilterStrength[s];
      if not D.SegHdr.AbsoluteDelta then Inc(baseLevel, D.FilterLevel);
    end else
      baseLevel := D.FilterLevel;
    for i4x4 := 0 to 1 do
    begin
      level := baseLevel;
      if D.UseLFDelta then
      begin
        Inc(level, D.RefLFDelta[0]);
        if i4x4 <> 0 then Inc(level, D.ModeLFDelta[0]);
      end;
      if level < 0 then level := 0 else if level > 63 then level := 63;
      if level > 0 then
      begin
        ilevel := level;
        if D.FilterSharpness > 0 then
        begin
          if D.FilterSharpness > 4 then ilevel := ilevel shr 2
          else ilevel := ilevel shr 1;
          if ilevel > 9 - D.FilterSharpness then ilevel := 9 - D.FilterSharpness;
        end;
        if ilevel < 1 then ilevel := 1;
        D.FStrength[s][i4x4].FILevel := ilevel;
        D.FStrength[s][i4x4].FLimit  := 2*level + ilevel;
        if level >= 40 then D.FStrength[s][i4x4].HevThresh := 2
        else if level >= 15 then D.FStrength[s][i4x4].HevThresh := 1
        else D.FStrength[s][i4x4].HevThresh := 0;
      end else
        D.FStrength[s][i4x4].FLimit := 0;
      D.FStrength[s][i4x4].FInner := (i4x4 <> 0);
    end;
  end;
end;

// Filter a single macroblock on the full-frame planes
procedure VP8DoFilter(var D: TVP8Decoder; mbx, mby: Integer);
var
  finfo: PVP8FInfo;
  yBps, uvBps, ilevel, limit, hevt: Integer;
  yDst, uDst, vDst: PByte;
begin
  finfo := D.FInfo + (mby*D.MbW + mbx);
  limit := finfo^.FLimit;
  if limit = 0 then Exit;
  ilevel := finfo^.FILevel;
  yBps   := D.FYStride;
  uvBps  := D.FUVStride;
  yDst   := D.FYPlane + mby*16*yBps + mbx*16;
  if D.FilterType = 1 then
  begin
    if mbx > 0          then FSimpleHFilter16(yDst, yBps, limit + 4);
    if finfo^.FInner    then FSimpleHFilter16i(yDst, yBps, limit);
    if mby > 0          then FSimpleVFilter16(yDst, yBps, limit + 4);
    if finfo^.FInner    then FSimpleVFilter16i(yDst, yBps, limit);
  end else
  begin
    hevt := finfo^.HevThresh;
    uDst := D.FUPlane + mby*8*uvBps + mbx*8;
    vDst := D.FVPlane + mby*8*uvBps + mbx*8;
    if mbx > 0 then
    begin
      FHFilter16(yDst, yBps, limit + 4, ilevel, hevt);
      FHFilter8(uDst, vDst, uvBps, limit + 4, ilevel, hevt);
    end;
    if finfo^.FInner then
    begin
      FHFilter16i(yDst, yBps, limit, ilevel, hevt);
      FHFilter8i(uDst, vDst, uvBps, limit, ilevel, hevt);
    end;
    if mby > 0 then
    begin
      FVFilter16(yDst, yBps, limit + 4, ilevel, hevt);
      FVFilter8(uDst, vDst, uvBps, limit + 4, ilevel, hevt);
    end;
    if finfo^.FInner then
    begin
      FVFilter16i(yDst, yBps, limit, ilevel, hevt);
      FVFilter8i(uDst, vDst, uvBps, limit, ilevel, hevt);
    end;
  end;
end;

procedure VP8FilterFrame(var D: TVP8Decoder);
var mbx, mby: Integer;
begin
  if D.FilterType = 0 then Exit;
  for mby := 0 to D.MbH-1 do
    for mbx := 0 to D.MbW-1 do
      VP8DoFilter(D, mbx, mby);
end;

// Write one RGB(A) pixel from a (Y,U,V) sample.
procedure StorePixel(yv, uv, vv: Integer; p: PByte; Mode: TCSMode); inline;
var r, g, b: Byte;
begin
  r := YuvToR(yv, vv);
  g := YuvToG(yv, uv, vv);
  b := YuvToB(yv, uv);
  case Mode of
    csmRGBA: begin p[0]:=r; p[1]:=g; p[2]:=b; p[3]:=255; end;
    csmARGB: begin p[0]:=255; p[1]:=r; p[2]:=g; p[3]:=b; end;
    csmBGRA: begin p[0]:=b; p[1]:=g; p[2]:=r; p[3]:=255; end;
    csmRGB:  begin p[0]:=r; p[1]:=g; p[2]:=b; end;
    csmBGR:  begin p[0]:=b; p[1]:=g; p[2]:=r; end;
  end;
end;

// Fancy (bilinear) chroma upsampling for a pair of output rows.
// Faithful port of libwebp UPSAMPLE_FUNC (dsp/upsampling.c).
procedure UpsamplePair(topY, botY, topU, topV, curU, curV, topDst, botDst: PByte;
  len, bpp: Integer; Mode: TCSMode);
var
  x, lastPair: Integer;
  tlU, tlV, lU, lV, tU, tV, cU, cV: Integer;
  avgU, avgV, d12U, d12V, d03U, d03V: Integer;
begin
  lastPair := (len - 1) shr 1;
  tlU := topU[0]; tlV := topV[0];
  lU  := curU[0]; lV  := curV[0];
  // first pixel
  StorePixel(topY[0], (3*tlU + lU + 2) shr 2, (3*tlV + lV + 2) shr 2, topDst, Mode);
  if botY <> nil then
    StorePixel(botY[0], (3*lU + tlU + 2) shr 2, (3*lV + tlV + 2) shr 2, botDst, Mode);
  for x := 1 to lastPair do
  begin
    tU := topU[x]; tV := topV[x];
    cU := curU[x]; cV := curV[x];
    avgU := tlU + tU + lU + cU + 8;
    avgV := tlV + tV + lV + cV + 8;
    d12U := (avgU + 2*(tU + lU)) shr 3;  d12V := (avgV + 2*(tV + lV)) shr 3;
    d03U := (avgU + 2*(tlU + cU)) shr 3; d03V := (avgV + 2*(tlV + cV)) shr 3;
    StorePixel(topY[2*x-1], (d12U + tlU) shr 1, (d12V + tlV) shr 1, topDst + (2*x-1)*bpp, Mode);
    StorePixel(topY[2*x],   (d03U + tU)  shr 1, (d03V + tV)  shr 1, topDst + (2*x)*bpp,   Mode);
    if botY <> nil then
    begin
      StorePixel(botY[2*x-1], (d03U + lU) shr 1, (d03V + lV) shr 1, botDst + (2*x-1)*bpp, Mode);
      StorePixel(botY[2*x],   (d12U + cU) shr 1, (d12V + cV) shr 1, botDst + (2*x)*bpp,   Mode);
    end;
    tlU := tU; tlV := tV; lU := cU; lV := cV;
  end;
  if (len and 1) = 0 then
  begin
    StorePixel(topY[len-1], (3*tlU + lU + 2) shr 2, (3*tlV + lV + 2) shr 2, topDst + (len-1)*bpp, Mode);
    if botY <> nil then
      StorePixel(botY[len-1], (3*lU + tlU + 2) shr 2, (3*lV + tlV + 2) shr 2, botDst + (len-1)*bpp, Mode);
  end;
end;

// Convert the (filtered) full-frame YUV planes to RGB in OutBuf, using
// libwebp-compatible fancy upsampling for the chroma planes.
procedure VP8EmitFrame(var D: TVP8Decoder);
var
  y, cu, W, H, bpp, ys, uvs, os: Integer;
  fy, fu, fv, ob: PByte;
begin
  W := D.PicWidth; H := D.PicHeight;
  bpp := D.OutBpp;
  ys := D.FYStride; uvs := D.FUVStride; os := D.OutStride;
  fy := D.FYPlane; fu := D.FUPlane; fv := D.FVPlane; ob := D.OutBuf;
  if H <= 0 then Exit;

  // First output row: mirror chroma row 0.
  UpsamplePair(fy, nil, fu, fv, fu, fv, ob, nil, W, bpp, D.OutputMode);
  cu := 0;
  y := 0;
  while y + 2 < H do
  begin
    // top chroma row = cu, cur chroma row = cu+1; outputs rows y+1, y+2
    UpsamplePair(fy + (y+1)*ys, fy + (y+2)*ys,
                 fu + cu*uvs, fv + cu*uvs,
                 fu + (cu+1)*uvs, fv + (cu+1)*uvs,
                 ob + NativeUInt(y+1)*NativeUInt(os),
                 ob + NativeUInt(y+2)*NativeUInt(os),
                 W, bpp, D.OutputMode);
    Inc(cu);
    Inc(y, 2);
  end;
  // For even height, the last row is produced by mirroring the last chroma row.
  if (H and 1) = 0 then
    UpsamplePair(fy + (H-1)*ys, nil,
                 fu + cu*uvs, fv + cu*uvs, fu + cu*uvs, fv + cu*uvs,
                 ob + NativeUInt(H-1)*NativeUInt(os), nil, W, bpp, D.OutputMode);
end;

// ============================================================
// VP8 FRAME DECODE
// ============================================================

function VP8DecodeFrame(var D: TVP8Decoder): Boolean;
var
  mby, mbx: Integer;
  hasTop, hasLeft: Boolean;
  partIdx: Integer;
  mb: ^TVP8MBData;
  br: ^TVP8Rd;
  info: PVP8MB;
  y, x, ix, iy: Integer;
  topCtx: PByte;    // pointer into D.IntraT for current macroblock's 4 columns
  ymode: Integer;
  leftMode: Integer;  // running left context for I4x4 mode parsing
  yBase, uBase, vBase: PByte;
  j: Integer;
  finfo: PVP8FInfo;
begin
  Result := False;

  yBase := @D.YuvBuf[Y_OFF];
  uBase := @D.YuvBuf[U_OFF];
  vBase := @D.YuvBuf[V_OFF];

  for mby := 0 to D.MbH-1 do
  begin
    hasTop := (mby > 0);

    // --- Reset left-column context for this row (mirrors VP8InitScanline) ---
    // Left col-(-1) for Y rows 0..15 and U/V rows 0..7
    for j := 0 to 15 do (yBase + j * BPS - 1)^ := 129;
    for j := 0 to  7 do begin (uBase + j * BPS - 1)^ := 129; (vBase + j * BPS - 1)^ := 129; end;

    // Top-left corner and top row initialisation
    if mby = 0 then
    begin
      // First row: no top context → fill top row + top-left with 127
      FillChar((yBase - BPS - 1)^, 16 + 4 + 1, 127);
      FillChar((uBase - BPS - 1)^, 8 + 1, 127);
      FillChar((vBase - BPS - 1)^, 8 + 1, 127);
    end else
    begin
      // Not first row: top-left corner = 129 (border value)
      (yBase - BPS - 1)^ := 129;
      (uBase - BPS - 1)^ := 129;
      (vBase - BPS - 1)^ := 129;
    end;

    // VP8InitScanline: reset left NZ and left intra context
    D.MBInfo^.NZ   := 0;
    D.MBInfo^.NZDC := 0;
    FillChar(D.IntraL, SizeOf(D.IntraL), B_DC_PRED);

    // Token partition for this row (all MBs in a row use the same partition)
    partIdx := mby mod D.NumParts;

    for mbx := 0 to D.MbW-1 do
    begin
      hasLeft := (mbx > 0);

      // --- Rotate right column → left column (left context for this MB) ---
      // Mirrors C's Copy32b(y_dst[j*BPS-4], y_dst[j*BPS+12]) for j=-1..15
      if mbx > 0 then
      begin
        for j := -1 to 15 do (yBase + j * BPS - 1)^ := (yBase + j * BPS + 15)^;
        for j := -1 to  7 do
        begin
          (uBase + j * BPS - 1)^ := (uBase + j * BPS + 7)^;
          (vBase + j * BPS - 1)^ := (vBase + j * BPS + 7)^;
        end;
      end;

      // --- Copy top-row samples into the buffer (needed by I4x4 prediction) ---
      // Mirrors C's memcpy(y_dst-BPS, top_yuv[mb_x].y, 16)
      if hasTop then
      begin
        Move((D.YTopBuf + mbx * 16)^, (yBase - BPS)^, 16);
        Move((D.UTopBuf + mbx *  8)^, (uBase - BPS)^,  8);
        Move((D.VTopBuf + mbx *  8)^, (vBase - BPS)^,  8);
        // Top-right 4 pixels (extend top row beyond the 16-pixel MB width)
        if mbx < D.MbW - 1 then
          Move((D.YTopBuf + (mbx + 1) * 16)^, (yBase - BPS + 16)^, 4)
        else
          FillChar((yBase - BPS + 16)^, 4, (D.YTopBuf + mbx * 16 + 15)^);
      end;
      // Replicate top-right to rows 3/7/11 in the buffer — always, for I4x4
      // (C: top_right[k*BPS] = top_right[0] where top_right is uint32_t*,
      //  stride = BPS * sizeof(uint32_t) = 128 bytes each step)
      PCardinal(yBase + 3  * BPS + 16)^ := PCardinal(yBase - BPS + 16)^;
      PCardinal(yBase + 7  * BPS + 16)^ := PCardinal(yBase - BPS + 16)^;
      PCardinal(yBase + 11 * BPS + 16)^ := PCardinal(yBase - BPS + 16)^;

      mb  := @D.MBData;
      info := PVP8MB(NativeUInt(D.MBInfo) + NativeUInt((mbx+1)*SizeOf(TVP8MB)));
      topCtx := D.IntraT + mbx * 4;

      // --- Parse intra modes from partition 0 ---
      br := @D.BR;

      // Segment ID — balanced binary tree (VP8 spec §9.3):
      //   bit0=0 → {0,1} via prob[1];  bit0=1 → {2,3} via prob[2]
      // Always consumes exactly 2 bits when update_map is set.
      if D.SegHdr.UseSegment and D.SegHdr.UpdateMap then
      begin
        if VP8RdGetBit(br^, D.SegHdr.SegProbs[0]) = 0 then
          mb^.Segment := VP8RdGetBit(br^, D.SegHdr.SegProbs[1])
        else
          mb^.Segment := 2 + VP8RdGetBit(br^, D.SegHdr.SegProbs[2]);
      end else
        mb^.Segment := 0;

      // Skip flag (read from partition 0 when use_skip_proba is set)
      if D.UseSkipProba then
        mb^.Skip := VP8RdGetBit(br^, D.SkipP) <> 0
      else
        mb^.Skip := False;

      // Intra mode
      mb^.IsI4x4 := VP8RdGetBit(br^, 145) = 0;
      if not mb^.IsI4x4 then
      begin
        ymode := ParseIntra16Mode(br^);
        // Fill all 16 sub-modes and update top/left context
        for ix := 0 to 15 do mb^.IModes[ix] := ymode;
        // Update IntraT (4 columns) and IntraL (4 rows)
        for ix := 0 to 3 do topCtx[ix] := ymode;
        for iy := 0 to 3 do D.IntraL[iy] := ymode;
      end else
      begin
        // I4x4: read 16 modes with proper top/left context
        // leftMode is the running left-neighbour mode, updated per-pixel (like
        // C's `ymode` variable in ParseIntraMode).
        for iy := 0 to 3 do
        begin
          leftMode := D.IntraL[iy];
          for ix := 0 to 3 do
          begin
            y := ParseIntra4x4Mode(br^,
              kBModesProba[topCtx[ix], leftMode]);
            mb^.IModes[iy*4 + ix] := y;
            topCtx[ix] := y;
            leftMode := y;
          end;
          D.IntraL[iy] := leftMode;  // = last decoded mode in this row
        end;
      end;
      mb^.UVMode := ParseUVMode(br^);      // --- Residuals from AC partition ---
      if not mb^.Skip then
      begin
        VP8ParseResiduals(D, mbx, D.Parts[partIdx], partIdx);
      end else
      begin
        FillChar(mb^.Coeffs[0], SizeOf(mb^.Coeffs), 0);
        mb^.NonZeroY  := 0;
        mb^.NonZeroUV := 0;
        // Matches C: left->nz = mb->nz = 0; if (!is_i4x4) left->nz_dc = mb->nz_dc = 0;
        D.MBInfo^.NZ := 0;   // left->nz
        info^.NZ     := 0;   // mb->nz (current column top context)
        if not mb^.IsI4x4 then
        begin
          D.MBInfo^.NZDC := 0;   // left->nz_dc
          info^.NZDC     := 0;   // mb->nz_dc
        end;
      end;

      // --- Store per-MB loop-filter info ---
      if D.FilterType > 0 then
      begin
        finfo := D.FInfo + (mby*D.MbW + mbx);
        finfo^ := D.FStrength[mb^.Segment and 3][Ord(mb^.IsI4x4)];
        // f_inner |= (MB has any non-zero coefficient)
        if (mb^.NonZeroY <> 0) or (mb^.NonZeroUV <> 0) then
          finfo^.FInner := True;
      end;      // --- Reconstruct YUV ---
      VP8ReconstructMB(D, mbx, hasTop, hasLeft);

      // --- Store reconstructed (unfiltered) MB into full-frame planes ---
      for y := 0 to 15 do
        Move(D.YuvBuf[Y_OFF + y * BPS],
             (D.FYPlane + (mby*16 + y)*D.FYStride + mbx*16)^, 16);
      for y := 0 to 7 do
      begin
        Move(D.YuvBuf[U_OFF + y * BPS],
             (D.FUPlane + (mby*8 + y)*D.FUVStride + mbx*8)^, 8);
        Move(D.YuvBuf[V_OFF + y * BPS],
             (D.FVPlane + (mby*8 + y)*D.FUVStride + mbx*8)^, 8);
      end;
    end;
  end;
  Result := True;
end;

// ============================================================
// VP8 FRAME HEADER PARSING
// ============================================================

function VP8ParseHeaders(var D: TVP8Decoder; Data: PByte; Size: NativeUInt): Boolean;
var
  tmp: Cardinal;
  partLen: Cardinal;
  dataBR: TVP8Rd;
  w, h: Integer;
  partData: PByte;
  szPtr:    PByte;
  partSize: NativeUInt;
  i: Integer;
begin
  Result := False;
  if Size < 10 then Exit;

  // 3-byte frame header
  tmp := PByte(Data)[0] or (Cardinal(PByte(Data)[1]) shl 8) or
         (Cardinal(PByte(Data)[2]) shl 16);
  D.KeyFrame := (tmp and 1) = 0;
  D.Profile  := (tmp shr 1) and 7;
  // show_frame = (tmp shr 4) and 1;
  partLen    := (tmp shr 5) and $7FFFF;

  if not D.KeyFrame then Exit;  // we only support key frames

  // 3-byte start code
  if (Data[3] <> $9D) or (Data[4] <> $01) or (Data[5] <> $2A) then Exit;

  // Width/Height
  w := (Data[6] or (Cardinal(Data[7]) shl 8)) and $3FFF;
  h := (Data[8] or (Cardinal(Data[9]) shl 8)) and $3FFF;
  D.PicWidth  := w;
  D.PicHeight := h;
  D.MbW       := (w + 15) shr 4;
  D.MbH       := (h + 15) shr 4;

  D.PartLen0 := partLen;

  // Partition 0: starts at Data+10 (after 3-byte frame tag + 7-byte picture header)
  // Length = partLen (= first_part_size from the frame tag, excludes picture header)
  VP8RdInit(D.BR, Data + 10, partLen);

  // Parse header fields from partition 0
  if VP8RdGet(D.BR) <> 0 then Exit; // color_space must be 0
  VP8RdGet(D.BR); // clamp_type (ignored)

  VP8ParseSegmentHeader(D.BR, D.SegHdr);
  VP8ParseFilterHeader(D.BR, D);
  // Number of token partitions: 2^n (n = 2-bit value)
  D.NumParts := 1 shl Integer(VP8RdGetValue(D.BR, 2));
  VP8ParseQuant(D.BR, D);
  VP8RdGet(D.BR); // update_proba bit — read and ignore (not an error if 1)
  VP8ParseProba(D.BR, D);  // also reads use_skip_proba/skip_p at the end

  // Token partition data starts immediately after partition 0.
  // Layout: [(NumParts-1) × 3-byte sizes][part0 data][part1 data]...
  // partData → size table entries; szPtr → pointer advancing through sizes
  partData := Data + 10 + partLen;           // start of token area (= size table)
  szPtr    := partData;                       // walks through 3-byte size entries
  partData := partData + NativeUInt(D.NumParts - 1) * 3;  // start of actual data
  for i := 0 to D.NumParts - 2 do
  begin
    partSize := szPtr[0] or (Cardinal(szPtr[1]) shl 8) or
                (Cardinal(szPtr[2]) shl 16);
    Inc(szPtr, 3);
    VP8RdInit(D.Parts[i], partData, partSize);
    Inc(partData, partSize);
  end;
  // Last partition: rest of the VP8 chunk
  if NativeUInt(partData) < NativeUInt(Data + Size) then
    partSize := NativeUInt(Data + Size) - NativeUInt(partData)
  else
    partSize := 0;
  VP8RdInit(D.Parts[D.NumParts-1], partData, partSize);

  Result := True;
end;

// ============================================================
// VP8L (LOSSLESS) DECODER
// ============================================================

// Each group: 5 Huffman tables for (green+len, red, blue, alpha, dist)
// Tables are dynamically allocated (two-level LUT for codes > 8 bits)
const
  HV_MAX_CODELEN = 15;

type
  TVP8LHuffTable = record
    E:  PHuffmanCode;  // allocated flat array (root + secondary tables)
    Sz: Integer;       // total entry count
  end;

  TVP8LHuffGroup = record
    T: array[0..4] of TVP8LHuffTable;
  end;
  PVP8LHuffGroup = ^TVP8LHuffGroup;

// --- Next canonical code key (bit-reversal step) ---
function VP8LGetNextKey(Key: Cardinal; Len: Integer): Cardinal;
var step: Cardinal;
begin
  step := Cardinal(1) shl (Len - 1);
  while (Key and step) <> 0 do step := step shr 1;
  if step <> 0 then Result := (Key and (step - 1)) + step
  else Result := Key;
end;

// --- Minimum extra bits needed for next secondary table ---
function VP8LNextTableBits(Count: PInteger; Len, RootBits: Integer): Integer;
var left: Integer;
begin
  left := 1 shl (Len - RootBits);
  while Len < HV_MAX_CODELEN do
  begin
    Dec(left, Count[Len]);
    if left <= 0 then Break;
    Inc(Len);
    left := left shl 1;
  end;
  Result := Len - RootBits;
end;

// --- Build two-level Huffman LUT (matches libwebp BuildHuffmanTable) ---
// First call with Table=nil to get size; then allocate and call again to fill.
// Returns total entries needed, or 0 on error.
function VP8LHuffBuild(Table: PHuffmanCode; RootBits: Integer;
  Lengths: PInteger; NumSymbols: Integer): Integer;
var
  Count:    array[0..HV_MAX_CODELEN + 1] of Integer;
  Offset:   array[0..HV_MAX_CODELEN + 1] of Integer;
  Sorted:   array[0..2327] of Word;
  Sym, Len, j: Integer;
  Step, TotalSize, TableBits, TableSize, SecBase: Integer;
  NumNodes, NumOpen, Sym2, totalNZ: Integer;
  Low, Mask, Key: Cardinal;
  Code: THuffmanCode;
begin
  Result := 0;
  FillChar(Count, SizeOf(Count), 0);
  for Sym := 0 to NumSymbols - 1 do
  begin
    Len := Lengths[Sym];
    if (Len < 0) or (Len > HV_MAX_CODELEN) then begin Exit; end;
    Inc(Count[Len]);
  end;
  if Count[0] = NumSymbols then begin Exit; end;
  for Len := 1 to HV_MAX_CODELEN do
    if Count[Len] > (1 shl Len) then begin Exit; end;

  Offset[1] := 0;
  for Len := 1 to HV_MAX_CODELEN - 1 do
    Offset[Len + 1] := Offset[Len] + Count[Len];
  for Sym := 0 to NumSymbols - 1 do
  begin
    Len := Lengths[Sym];
    if Len > 0 then begin Sorted[Offset[Len]] := Word(Sym); Inc(Offset[Len]); end;
  end;
  totalNZ := Offset[HV_MAX_CODELEN];

  if totalNZ = 1 then
  begin
    if Table <> nil then
    begin
      Code.Bits := 0; Code.Value := Sorted[0];
      for j := 0 to (1 shl RootBits) - 1 do Table[j] := Code;
    end;
    Result := 1 shl RootBits;
    Exit;
  end;

  // Rebuild Count (Offset[] was incremented during sort)
  FillChar(Count, SizeOf(Count), 0);
  for Sym := 0 to NumSymbols - 1 do
  begin
    Len := Lengths[Sym];
    if (Len > 0) and (Len <= HV_MAX_CODELEN) then Inc(Count[Len]);
  end;

  TotalSize := 1 shl RootBits;
  Low       := $FFFFFFFF;
  Mask      := Cardinal(TotalSize - 1);
  Key       := 0;
  NumNodes  := 1;
  NumOpen   := 1;
  TableSize := 1 shl RootBits;  // initial root size (used for first SecBase advance)
  //TableBits := RootBits;
  SecBase   := 0;
  Sym2      := 0;

  // Fill root table (code lengths 1..RootBits)
  Step := 2;
  for Len := 1 to RootBits do
  begin
    NumOpen := NumOpen shl 1;
    Inc(NumNodes, NumOpen);
    Dec(NumOpen, Count[Len]);
    if NumOpen < 0 then begin Exit; end;
    while Count[Len] > 0 do
    begin
      if Table <> nil then
      begin
        Code.Bits  := Byte(Len);
        Code.Value := Sorted[Sym2];
        j := Integer(Key);
        while j < (1 shl RootBits) do begin Table[j] := Code; Inc(j, Step); end;
      end;
      Inc(Sym2);
      Key := VP8LGetNextKey(Key, Len);
      Dec(Count[Len]);
    end;
    Step := Step shl 1;
  end;

  // Fill secondary tables (code lengths > RootBits)
  Step := 2;
  for Len := RootBits + 1 to HV_MAX_CODELEN do
  begin
    NumOpen := NumOpen shl 1;
    Inc(NumNodes, NumOpen);
    Dec(NumOpen, Count[Len]);
    if NumOpen < 0 then begin Exit; end;
    while Count[Len] > 0 do
    begin
      if (Key and Mask) <> Low then
      begin
        SecBase   := SecBase + TableSize;
        TableBits := VP8LNextTableBits(@Count[0], Len, RootBits);
        TableSize := 1 shl TableBits;
        Inc(TotalSize, TableSize);
        Low := Key and Mask;
        if Table <> nil then
        begin
          Table[Low].Bits  := Byte(TableBits + RootBits);
          Table[Low].Value := Word(SecBase - Integer(Low));
        end;
      end;
      if Table <> nil then
      begin
        Code.Bits  := Byte(Len - RootBits);
        Code.Value := Sorted[Sym2];
        j := Integer(Key shr RootBits);
        while j < TableSize do begin Table[SecBase + j] := Code; Inc(j, Step); end;
      end;
      Inc(Sym2);
      Key := VP8LGetNextKey(Key, Len);
      Dec(Count[Len]);
    end;
    Step := Step shl 1;
  end;

  if NumNodes <> 2 * totalNZ - 1 then begin Exit; end;
  Result := TotalSize;
end;

// ---- Transform type constants ----
const
  VP8L_MAX_TRANSFORMS = 4;
  VP8L_TT_PREDICTOR  = 0;
  VP8L_TT_COLORXFORM = 1;
  VP8L_TT_SUBGREEN   = 2;
  VP8L_TT_COLORINDEX = 3;

// ---- Transform record ----
type
  TVP8LTransform = record
    TType:   Integer;   // VP8L_TT_*
    Bits:    Integer;   // block bits (PRED/COLOR) or packing bits (CI)
    XSize:   Integer;   // original image width before packing
    YSize:   Integer;
    Data:    PCardinal; // allocated sub-image (transform data)
  end;

// ---- VP8L decoder state ----
type
  TVP8LState = record
    BR:           TVP8LBitReader;
    CacheBits:    Integer;
    ColorCache:   PCardinal;
    HuffBits:     Integer;  // 0 = single group
    HuffW:        Integer;  // huffman image width in tiles
    HuffImage:    PCardinal;
    NumGroups:    Integer;
    Groups:       PVP8LHuffGroup; // allocated array
  end;

// ---- Read code lengths (code-length alphabet has 19 symbols, fits in 256-entry LUT) ----
// Faithful port of libwebp ReadHuffmanCodeLengths (incl. max_symbol prefix).
function VP8LReadCodeLengths(var BR: TVP8LBitReader;
  CLTable: PHuffmanCode; NumSymbols: Integer; Lengths: PInteger): Boolean;
var
  i, sym, reps: Integer;
  prev: Integer;
  maxSymbol, lengthNbits: Integer;
  usePrev, length: Integer;
  extraBits: array[0..2] of Integer;
  repOff:    array[0..2] of Integer;
begin
  Result := False;
  extraBits[0] := 2; extraBits[1] := 3; extraBits[2] := 7;
  repOff[0]    := 3; repOff[1]    := 3; repOff[2]    := 11;

  // Optional max_symbol prefix
  if VP8LReadBits(BR, 1) <> 0 then
  begin
    lengthNbits := 2 + 2 * Integer(VP8LReadBits(BR, 3));
    maxSymbol   := 2 + Integer(VP8LReadBits(BR, lengthNbits));
    if maxSymbol > NumSymbols then Exit;
  end else
    maxSymbol := NumSymbols;

  i := 0; prev := 8;
  while i < NumSymbols do
  begin
    if maxSymbol = 0 then Break;
    Dec(maxSymbol);
    if BR.Available <= 32 then VP8LFillBitWindow(BR);
    sym := HuffReadSymbol(BR, CLTable, HUFF_LUT_BITS);
    if sym < 0 then Exit;
    if sym < 16 then
    begin
      Lengths[i] := sym;
      if sym <> 0 then prev := sym;
      Inc(i);
    end else
    begin
      usePrev := Ord(sym = 16);
      reps    := Integer(VP8LReadBits(BR, extraBits[sym - 16])) + repOff[sym - 16];
      if i + reps > NumSymbols then Exit;
      if usePrev <> 0 then length := prev else length := 0;
      while reps > 0 do begin Lengths[i] := length; Inc(i); Dec(reps); end;
    end;
  end;
  Result := True;
end;

// ---- Read one Huffman table into TVP8LHuffTable (two-level LUT) ----
function VP8LReadHuffTable(var BR: TVP8LBitReader; AlphabetSize: Integer;
  out HT: TVP8LHuffTable): Boolean;
var
  isSimple, numSyms, lenBit, sym1, sym2: Integer;
  clLengths:   array[0..18] of Integer;
  clTable:     array[0..255] of THuffmanCode;
  codeLengths: array[0..2327] of Integer;
  numCodes, j, tsz: Integer;
begin
  Result   := False;
  HT.E     := nil;
  HT.Sz    := 0;
  isSimple := Integer(VP8LReadBits(BR, 1));
  FillChar(codeLengths[0], AlphabetSize * SizeOf(Integer), 0);
  if isSimple <> 0 then
  begin
    numSyms := Integer(VP8LReadBits(BR, 1)) + 1;
    lenBit  := Integer(VP8LReadBits(BR, 1));
    if lenBit = 0 then sym1 := Integer(VP8LReadBits(BR, 1))
    else               sym1 := Integer(VP8LReadBits(BR, 8));
    if (sym1 >= 0) and (sym1 < AlphabetSize) then codeLengths[sym1] := 1;
    sym2 := -1;
    if numSyms = 2 then
    begin
      sym2 := Integer(VP8LReadBits(BR, 8));
      if (sym2 >= 0) and (sym2 < AlphabetSize) then codeLengths[sym2] := 1;
    end;
  end else
  begin
    FillChar(clLengths, SizeOf(clLengths), 0);
    numCodes := Integer(VP8LReadBits(BR, 4)) + 4;
    for j := 0 to numCodes - 1 do
      clLengths[kCodeLengthCodeOrder[j]] := Integer(VP8LReadBits(BR, 3));
    if not VP8LBuildHuffmanTable(clLengths, 19, @clTable[0], HUFF_LUT_BITS) then
    begin
      Exit;
    end;
    if not VP8LReadCodeLengths(BR, @clTable[0], AlphabetSize, @codeLengths[0]) then
    begin
      Exit;
    end;
  end;
  tsz := VP8LHuffBuild(nil, HUFF_LUT_BITS, @codeLengths[0], AlphabetSize);
  if tsz = 0 then Exit;
  HT.E  := AllocMem(tsz * SizeOf(THuffmanCode));
  HT.Sz := tsz;
  if VP8LHuffBuild(HT.E, HUFF_LUT_BITS, @codeLengths[0], AlphabetSize) = 0 then
  begin
    FreeMem(HT.E); HT.E := nil; HT.Sz := 0;
    Exit;
  end;
  Result := True;
end;

// ---- Free dynamically allocated tables inside one group ----
procedure VP8LFreeGroupTables(grp: PVP8LHuffGroup);
var j: Integer;
begin
  for j := 0 to 4 do
    if grp^.T[j].E <> nil then begin FreeMem(grp^.T[j].E); grp^.T[j].E := nil; end;
end;

// ---- Read NumGroups sets of 5 Huffman tables ----
function VP8LReadGroups(var BR: TVP8LBitReader; NumGroups: Integer;
  GreenAlphaSize: Integer; Groups: PVP8LHuffGroup): Boolean;
var
  g, j: Integer;
  alphabets: array[0..4] of Integer;
begin
  Result       := False;
  alphabets[0] := GreenAlphaSize;
  alphabets[1] := 256; alphabets[2] := 256;
  alphabets[3] := 256; alphabets[4] := 40;
  for g := 0 to NumGroups - 1 do
    for j := 0 to 4 do
      if not VP8LReadHuffTable(BR, alphabets[j], Groups[g].T[j]) then
      begin
        Exit;
      end;
  Result := True;
end;

// ---- Symbol read with two-level LUT ----
function VP8LReadSym(var BR: TVP8LBitReader; const HT: TVP8LHuffTable): Integer;
var
  key:     Cardinal;
  e:       PHuffmanCode;
  nbits:   Integer;
  xkey:    Cardinal;
begin
  key   := VP8LPeekBits(BR, HUFF_LUT_BITS);
  e     := @HT.E[key];
  nbits := Integer(e^.Bits) - HUFF_LUT_BITS;
  if nbits > 0 then
  begin
    BR.Val := BR.Val shr HUFF_LUT_BITS;
    Dec(BR.Available, HUFF_LUT_BITS);
    if BR.Available <= 32 then VP8LFillBitWindow(BR);
    xkey := VP8LPeekBits(BR, nbits);
    Inc(e, Integer(e^.Value) + Integer(xkey));
  end;
  BR.Val := BR.Val shr e^.Bits;
  Dec(BR.Available, e^.Bits);
  if BR.Available <= 32 then VP8LFillBitWindow(BR);
  Result := Integer(e^.Value);
end;

// ---- Convert plane-code to pixel distance ----
function VP8LPlaneCodeToDist(XSize, PlaneCode: Integer): Integer; inline;
var
  dc, yoff, xoff, d: Integer;
begin
  if PlaneCode > 120 then
    Result := PlaneCode - 120
  else
  begin
    dc   := Integer(kCodeToPlane[PlaneCode - 1]);
    yoff := dc shr 4;
    xoff := 8 - (dc and $F);
    d    := yoff * XSize + xoff;
    if d < 1 then d := 1;
    Result := d;
  end;
end;

// ---- Expand copy code prefix into actual length/distance value ----
function VP8LCopyCode(sym: Integer; var BR: TVP8LBitReader): Integer; inline;
var
  extra, offset: Integer;
begin
  if sym < 4 then
    Result := sym + 1
  else
  begin
    extra  := (sym - 2) shr 1;
    offset := (2 + (sym and 1)) shl extra;
    Result := offset + Integer(VP8LReadBits(BR, extra)) + 1;
  end;
end;

// ---- Per-channel wrapping add (for predictor inverse) ----
function VP8LAddPx(a, b: Cardinal): Cardinal; inline;
begin
  Result := ((a + b) and $FF)
    or ((((a shr 8) + (b shr 8)) and $FF) shl 8)
    or ((((a shr 16) + (b shr 16)) and $FF) shl 16)
    or ((((a shr 24) + (b shr 24)) and $FF) shl 24);
end;

// ---- Predictor averages ----
function PAvg2(a, b: Cardinal): Cardinal; inline;
begin
  Result := (((a xor b) and $FEFEFEFE) shr 1) + (a and b);
end;

function PAvg3(a, b, c: Cardinal): Cardinal; inline;
begin
  Result := PAvg2(PAvg2(a, c), b);
end;

function PAvg4(a, b, c, d: Cardinal): Cardinal; inline;
begin
  Result := PAvg2(PAvg2(a, b), PAvg2(c, d));
end;

function Clip8(v: Integer): Integer; inline;
begin
  if v < 0 then Result := 0 else if v > 255 then Result := 255 else Result := v;
end;

function PSelect(a, b, c: Cardinal): Cardinal; inline;
var
  pa: Integer;
begin
  pa := (Abs(Integer((a shr 24) and $FF) - Integer((c shr 24) and $FF))
       - Abs(Integer((b shr 24) and $FF) - Integer((c shr 24) and $FF)))
      + (Abs(Integer((a shr 16) and $FF) - Integer((c shr 16) and $FF))
       - Abs(Integer((b shr 16) and $FF) - Integer((c shr 16) and $FF)))
      + (Abs(Integer((a shr 8) and $FF) - Integer((c shr 8) and $FF))
       - Abs(Integer((b shr 8) and $FF) - Integer((c shr 8) and $FF)))
      + (Abs(Integer(a and $FF) - Integer(c and $FF))
       - Abs(Integer(b and $FF) - Integer(c and $FF)));
  // libwebp: Sub3(a,b,c)=|b-c|-|a-c|; returns a when sum<=0. Our pa has the
  // opposite sign (|a-c|-|b-c|), so return a when pa >= 0.
  if pa >= 0 then Result := a else Result := b;
end;

function PClampFull(c0, c1, c2: Cardinal): Cardinal; inline;
begin
  Result :=
    (Cardinal(Clip8(Integer(c0 shr 24) + Integer(c1 shr 24) - Integer(c2 shr 24))) shl 24) or
    (Cardinal(Clip8(Integer((c0 shr 16) and $FF) + Integer((c1 shr 16) and $FF) - Integer((c2 shr 16) and $FF))) shl 16) or
    (Cardinal(Clip8(Integer((c0 shr 8) and $FF) + Integer((c1 shr 8) and $FF) - Integer((c2 shr 8) and $FF))) shl 8) or
    Cardinal(Clip8(Integer(c0 and $FF) + Integer(c1 and $FF) - Integer(c2 and $FF)));
end;

function PClampHalf(c0, c1, c2: Cardinal): Cardinal; inline;
var
  av: Cardinal;
  aa, ra, ga, ba, ac, rc, gc, bc: Integer;
begin
  av := PAvg2(c0, c1);
  aa := Integer(av shr 24) and $FF;  ac := Integer(c2 shr 24) and $FF;
  ra := Integer((av shr 16) and $FF); rc := Integer((c2 shr 16) and $FF);
  ga := Integer((av shr 8) and $FF);  gc := Integer((c2 shr 8) and $FF);
  ba := Integer(av and $FF);           bc := Integer(c2 and $FF);
  Result :=
    (Cardinal(Clip8(aa + (aa - ac) div 2)) shl 24) or
    (Cardinal(Clip8(ra + (ra - rc) div 2)) shl 16) or
    (Cardinal(Clip8(ga + (ga - gc) div 2)) shl 8) or
     Cardinal(Clip8(ba + (ba - bc) div 2));
end;

function VP8LPredict(mode: Integer; left, top, topLeft, topRight: Cardinal): Cardinal; inline;
begin
  case mode of
    0:  Result := $FF000000;
    1:  Result := left;
    2:  Result := top;
    3:  Result := topRight;
    4:  Result := topLeft;
    5:  Result := PAvg3(left, top, topRight);
    6:  Result := PAvg2(left, topLeft);
    7:  Result := PAvg2(left, top);
    8:  Result := PAvg2(topLeft, top);
    9:  Result := PAvg2(top, topRight);
    10: Result := PAvg4(left, topLeft, top, topRight);
    11: Result := PSelect(top, left, topLeft);
    12: Result := PClampFull(left, top, topLeft);
    13: Result := PClampHalf(left, top, topLeft);
    else Result := $FF000000;
  end;
end;

// ---- Inverse SubGreen: add green to red and blue ----
procedure VP8LInvSubGreen(Pixels: PCardinal; Count: Integer);
var i: Integer; px, g: Cardinal;
begin
  for i := 0 to Count - 1 do
  begin
    px := Pixels[i];
    g  := (px shr 8) and $FF;
    Pixels[i] := (px and $FF00FF00)
               or (((px shr 16) and $FF) + g) and $FF shl 16
               or ((px and $FF) + g) and $FF;
  end;
end;

// ---- Inverse Color Transform ----
function CTDelta(t, c: Integer): Integer; inline;
begin
  if t >= 128 then Dec(t, 256);
  if c >= 128 then Dec(c, 256);
  Result := SarI(t * c, 5);  // arithmetic shift (C does >>5 on signed int)
end;

procedure VP8LInvColorXform(TData: PCardinal; TBits, W: Integer;
  Pixels: PCardinal; Count: Integer);
var
  i, tw, tx, ty, tileIdx: Integer;
  px, td: Cardinal;
  g, r, b: Integer;
begin
  tw := 1 shl TBits;
  for i := 0 to Count - 1 do
  begin
    tx := (i mod W) shr TBits;
    ty := (i div W) shr TBits;
    tileIdx := ty * ((W + tw - 1) shr TBits) + tx;
    td := TData[tileIdx];
    px := Pixels[i];
    g  := Integer((px shr 8) and $FF);
    r  := Integer((px shr 16) and $FF);
    b  := Integer(px and $FF);
    r  := (r + CTDelta(td and $FF, g)) and $FF;
    b  := (b + CTDelta((td shr 8) and $FF, g) + CTDelta((td shr 16) and $FF, r)) and $FF;
    Pixels[i] := (px and $FF00FF00) or (Cardinal(r) shl 16) or Cardinal(b);
  end;
end;

// ---- Inverse Predictor Transform ----
procedure VP8LInvPredictor(TData: PCardinal; TBits, W, H: Integer;
  Pixels: PCardinal);
var
  x, y, tw, tileW, tileIdx: Integer;
  idx: Integer;
  pred, residual: Cardinal;
  mode: Integer;
  topRow, topRowPrev: PCardinal;
begin
  tw := 1 shl TBits;
  // First row: mode=1 (left prediction) for pixel 0, then mode=1 for rest
  // Actually: pixel 0 uses mode=0 (black), pixels 1+ use mode=1 (left)
  // This matches libwebp: PredictorAdd0_C for x=0 y=0, PredictorAdd1_C for rest of first row
  // Then for y>0: x=0 uses mode=2 (top), rest uses mode from TData

  // Process first row
  Pixels[0] := VP8LAddPx(Pixels[0], $FF000000);
  for x := 1 to W - 1 do
    Pixels[x] := VP8LAddPx(Pixels[x], Pixels[x - 1]);

  // Process remaining rows
  for y := 1 to H - 1 do
  begin
    idx := y * W;
    // First pixel of each row: mode=2 (top)
    Pixels[idx] := VP8LAddPx(Pixels[idx], Pixels[idx - W]);
    for x := 1 to W - 1 do
    begin
      // Get tile's predictor mode (from green channel >> 8, shifted into bits 8..11)
      tileIdx := (y shr TBits) * ((W + tw - 1) shr TBits) + (x shr TBits);
      mode := Integer((TData[tileIdx] shr 8) and $F);
      pred := VP8LPredict(mode,
                Pixels[idx + x - 1],       // left
                Pixels[idx + x - W],        // top
                Pixels[idx + x - W - 1],    // top-left
                Pixels[idx + x - W + 1]);   // top-right (clamp at W-1 is done by caller)
      Pixels[idx + x] := VP8LAddPx(Pixels[idx + x], pred);
    end;
  end;
end;

// ---- Inverse Color Indexing ----
// OrigW is the width before packing; W is the encoded (compressed) width
procedure VP8LInvColorIndex(Palette: PCardinal; PackBits: Integer;
  OrigW: Integer; Pixels: PCardinal; W, H: Integer);
var
  bpp, ppb, bmask, i: Integer;
  x, y, srcIdx, outIdx, idx: Integer;
  packedByte: Cardinal;
  outBuf: PCardinal;
begin
  if PackBits = 0 then
  begin
    for srcIdx := 0 to W * H - 1 do
    begin
      idx := Integer((Pixels[srcIdx] shr 8) and $FF);
      Pixels[srcIdx] := Palette[idx];
    end;
  end else
  begin
    bpp   := 8 shr PackBits;
    ppb   := 1 shl PackBits;
    bmask := (1 shl bpp) - 1;
    outBuf := AllocMem(OrigW * H * SizeOf(Cardinal));
    outIdx := 0;
    srcIdx := 0;
    for y := 0 to H - 1 do
    begin
      x := 0;
      while x < OrigW do
      begin
        packedByte := (Pixels[srcIdx] shr 8) and $FF;
        Inc(srcIdx);
        for i := 0 to ppb - 1 do
        begin
          if x >= OrigW then Break;
          outBuf[outIdx] := Palette[packedByte and bmask];
          packedByte := packedByte shr bpp;
          Inc(x);
          Inc(outIdx);
        end;
      end;
    end;
    Move(outBuf^, Pixels^, OrigW * H * SizeOf(Cardinal));
    FreeMem(outBuf);
  end;
end;

// ---- AddPixels delta-expand palette ----
procedure VP8LExpandPalette(Palette: PCardinal; N: Integer);
var i: Integer;
begin
  for i := 1 to N - 1 do
    Palette[i] := VP8LAddPx(Palette[i], Palette[i - 1]);
end;

// ---- Build the full color map (matches libwebp ExpandColorMap) ----
// Cumulative-add the numColors entries, then pad to (1 shl (8 shr bits))
// entries with zeros ("black tail"), so out-of-range indices read 0.
function VP8LBuildColorMap(SubPix: PCardinal; NumColors, Bits: Integer): PCardinal;
var
  finalN, i: Integer;
begin
  finalN := 1 shl (8 shr Bits);
  Result := AllocMem(finalN * SizeOf(Cardinal));  // zero-filled tail
  if NumColors > 0 then Result[0] := SubPix[0];
  for i := 1 to NumColors - 1 do
    Result[i] := VP8LAddPx(SubPix[i], Result[i - 1]);
end;

// ---- Forward declaration for recursive sub-image decode ----
function VP8LDecodeSubImage(var BR: TVP8LBitReader; SW, SH: Integer;
  out Pixels: PCardinal): Boolean; forward;

// ---- Decode pixels using state (no transforms applied here) ----
function VP8LDecodePixels(var S: TVP8LState; Pixels: PCardinal; W, H: Integer): Boolean;
var
  nPix, pidx, x, y, i: Integer;
  sym, length, dcode, pdist, sidx: Integer;
  green, red, blue, alpha: Cardinal;
  px: Cardinal;
  cidx: Integer;
  grpIdx: Integer;
  grp: PVP8LHuffGroup;
begin
  Result := False;
  nPix   := W * H;
  pidx   := 0; x := 0; y := 0;
  while pidx < nPix do
  begin
    if S.HuffBits > 0 then
    begin
      grpIdx := Integer(S.HuffImage[(y shr S.HuffBits) * S.HuffW + (x shr S.HuffBits)]);
      grp := @S.Groups[grpIdx];
    end else
      grp := @S.Groups[0];

    sym := VP8LReadSym(S.BR, grp^.T[0]);
    if sym < 0 then Exit;

    if sym < 256 then
    begin
      green := Cardinal(sym);
      red   := Cardinal(VP8LReadSym(S.BR, grp^.T[1]));
      blue  := Cardinal(VP8LReadSym(S.BR, grp^.T[2]));
      alpha := Cardinal(VP8LReadSym(S.BR, grp^.T[3]));
      px := (alpha shl 24) or (red shl 16) or (green shl 8) or blue;
      Pixels[pidx] := px;
      if S.CacheBits > 0 then
      begin
        cidx := Integer(Cardinal(px * Cardinal($1E35A7BD)) shr (32 - S.CacheBits));
        S.ColorCache[cidx] := px;
      end;
      Inc(pidx); Inc(x); if x >= W then begin x := 0; Inc(y); end;
    end else if sym < 256 + 24 then
    begin
      length := VP8LCopyCode(sym - 256, S.BR);
      dcode  := VP8LReadSym(S.BR, grp^.T[4]);
      if dcode < 0 then Exit;
      pdist := VP8LPlaneCodeToDist(W, VP8LCopyCode(dcode, S.BR));
      if (pdist < 1) or (pdist > pidx) then Exit;  // invalid back-reference
      // Standard LZ77 copy: read pdist behind, advancing both (handles overlap)
      for i := 0 to length - 1 do
      begin
        if pidx >= nPix then Break;
        px := Pixels[pidx - pdist];
        Pixels[pidx] := px;
        if S.CacheBits > 0 then
        begin
          cidx := Integer(Cardinal(px * Cardinal($1E35A7BD)) shr (32 - S.CacheBits));
          S.ColorCache[cidx] := px;
        end;
        Inc(pidx); Inc(x); if x >= W then begin x := 0; Inc(y); end;
      end;
    end else if (S.CacheBits > 0) and (sym < 256 + 24 + (1 shl S.CacheBits)) then
    begin
      cidx := sym - 256 - 24;
      px := S.ColorCache[cidx];
      Pixels[pidx] := px;
      Inc(pidx); Inc(x); if x >= W then begin x := 0; Inc(y); end;
    end else
    begin
      Inc(pidx); Inc(x); if x >= W then begin x := 0; Inc(y); end;
    end;
  end;
  Result := True;
end;

// ---- Setup state from BR: color-cache + meta-huffman + huffman tables ----
// After call, BR is advanced past all table data
// AllowMeta = True only for the top-level (level0) image stream; sub-images
// (entropy image, transform data) must NOT read a meta-huffman bit.
function VP8LSetupState(var BR: TVP8LBitReader; W, H: Integer;
  AllowMeta: Boolean; var S: TVP8LState): Boolean;
var
  cacheBit, metaBit: Integer;
  numPix, i, g, gi: Integer;
  greenAlpha: Integer;
  grpsBuf: PVP8LHuffGroup;
  cacheSz: Integer;
  huffH: Integer;
begin
  Result := False;
  FillChar(S, SizeOf(S), 0);
  S.BR := BR;

  // Color cache
  cacheBit := Integer(VP8LReadBits(S.BR, 1));
  if cacheBit <> 0 then
  begin
    S.CacheBits := Integer(VP8LReadBits(S.BR, 4));
    if (S.CacheBits < 1) or (S.CacheBits > 11) then Exit;
    S.ColorCache := AllocMem((1 shl S.CacheBits) * SizeOf(Cardinal));
  end;
  greenAlpha := 280;
  if S.CacheBits > 0 then Inc(greenAlpha, 1 shl S.CacheBits);

  // Meta-Huffman (only for the top-level image stream)
  if AllowMeta then metaBit := Integer(VP8LReadBits(S.BR, 1))
  else              metaBit := 0;
  if metaBit <> 0 then
  begin
    S.HuffBits := Integer(VP8LReadBits(S.BR, 3)) + 2;
    S.HuffW    := (W + (1 shl S.HuffBits) - 1) shr S.HuffBits;
    huffH      := (H + (1 shl S.HuffBits) - 1) shr S.HuffBits;
    if not VP8LDecodeSubImage(S.BR, S.HuffW, huffH, S.HuffImage) then
    begin
      if S.ColorCache <> nil then FreeMem(S.ColorCache);
      Exit;
    end;
    numPix := S.HuffW * huffH;
    S.NumGroups := 0;
    for i := 0 to numPix - 1 do
    begin
      g := Integer((S.HuffImage[i] shr 8) and $FFFF);
      S.HuffImage[i] := Cardinal(g);
      if g >= S.NumGroups then S.NumGroups := g + 1;
    end;
    if S.NumGroups = 0 then S.NumGroups := 1;
  end else
  begin
    S.HuffBits  := 0;
    S.NumGroups := 1;
  end;

  grpsBuf := AllocMem(S.NumGroups * SizeOf(TVP8LHuffGroup));
  S.Groups := grpsBuf;
  if not VP8LReadGroups(S.BR, S.NumGroups, greenAlpha, S.Groups) then
  begin
    for gi := 0 to S.NumGroups - 1 do VP8LFreeGroupTables(@S.Groups[gi]);
    FreeMem(grpsBuf); S.Groups := nil;
    if S.ColorCache <> nil then begin FreeMem(S.ColorCache); S.ColorCache := nil; end;
    if S.HuffImage  <> nil then begin FreeMem(S.HuffImage);  S.HuffImage  := nil; end;
    Exit;
  end;

  BR := S.BR;
  Result := True;
end;

procedure VP8LFreeState(var S: TVP8LState);
var g: Integer;
begin
  if S.Groups <> nil then
  begin
    for g := 0 to S.NumGroups - 1 do VP8LFreeGroupTables(@S.Groups[g]);
    FreeMem(S.Groups);
    S.Groups := nil;
  end;
  if S.ColorCache <> nil then begin FreeMem(S.ColorCache); S.ColorCache := nil; end;
  if S.HuffImage  <> nil then begin FreeMem(S.HuffImage);  S.HuffImage  := nil; end;
end;

// ---- Decode a sub-image (for transform metadata or meta-huffman index) ----
// No transforms inside sub-images; uses VP8LSetupState + VP8LDecodePixels.
function VP8LDecodeSubImage(var BR: TVP8LBitReader; SW, SH: Integer;
  out Pixels: PCardinal): Boolean;
var
  S:   TVP8LState;
  nPx: Integer;
begin
  Result := False;
  Pixels := nil;
  FillChar(S, SizeOf(S), 0);
  if not VP8LSetupState(BR, SW, SH, False, S) then  // sub-image: no meta-huffman
  begin
    VP8LFreeState(S);
    Exit;
  end;
  nPx    := SW * SH;
  Pixels := AllocMem(nPx * SizeOf(Cardinal));
  Result := VP8LDecodePixels(S, Pixels, SW, SH);
  if not Result then begin FreeMem(Pixels); Pixels := nil; end;
  BR := S.BR;
  VP8LFreeState(S);
end;

// ---- Full VP8L decode ----
// Core VP8L decode: from transforms through inverse transforms.
// BR must be positioned right after the (already-read) image dimensions.
// Returns internal ARGB pixels (0xAARRGGBB), NOT R/B swapped. Caller frees.
function VP8LDecodeBody(var BR: TVP8LBitReader; w, h: Integer;
  out pixels: PCardinal): Boolean;
var
  encW:        Integer;  // encoded (possibly packed) width
  i, t:        Integer;
  transforms:  array[0..VP8L_MAX_TRANSFORMS - 1] of TVP8LTransform;
  numT:        Integer;
  ttType:      Integer;
  ttBits:      Integer;
  numColors:   Integer;
  subPix:      PCardinal;
  S:           TVP8LState;
  nPix:        Integer;
  ok:          Boolean;
begin
  Result := False;
  pixels := nil;
  encW   := w;
  numT   := 0;
  FillChar(transforms, SizeOf(transforms), 0);

  // Read transforms
  while VP8LReadBits(BR, 1) <> 0 do
  begin
    if numT >= VP8L_MAX_TRANSFORMS then begin
      for i := 0 to numT-1 do
        if transforms[i].Data <> nil then FreeMem(transforms[i].Data);
      Exit;
    end;
    ttType := Integer(VP8LReadBits(BR, 2));
    transforms[numT].TType := ttType;
    transforms[numT].XSize := encW;
    transforms[numT].YSize := h;
    case ttType of
      VP8L_TT_PREDICTOR, VP8L_TT_COLORXFORM:
      begin
        ttBits := Integer(VP8LReadBits(BR, 3)) + 2;
        transforms[numT].Bits := ttBits;
        if not VP8LDecodeSubImage(BR,
               (encW + (1 shl ttBits) - 1) shr ttBits,
               (h    + (1 shl ttBits) - 1) shr ttBits,
               subPix) then
        begin
          for i := 0 to numT-1 do
            if transforms[i].Data <> nil then FreeMem(transforms[i].Data);
          Exit;
        end;
        transforms[numT].Data := subPix;
      end;
      VP8L_TT_SUBGREEN: ; // no data
      VP8L_TT_COLORINDEX:
      begin
        numColors := Integer(VP8LReadBits(BR, 8)) + 1;
        if numColors > 16      then ttBits := 0
        else if numColors > 4  then ttBits := 1
        else if numColors > 2  then ttBits := 2
        else                        ttBits := 3;
        transforms[numT].Bits := ttBits;
        if not VP8LDecodeSubImage(BR, numColors, 1, subPix) then
        begin
          for i := 0 to numT-1 do
            if transforms[i].Data <> nil then FreeMem(transforms[i].Data);
          Exit;
        end;
        transforms[numT].Data := VP8LBuildColorMap(subPix, numColors, ttBits);
        FreeMem(subPix);
        if ttBits > 0 then
          encW := (w + (1 shl ttBits) - 1) shr ttBits;
      end;
    end;
    Inc(numT);
  end;

  // Setup Huffman state (color cache + meta-huffman + tables)
  FillChar(S, SizeOf(S), 0);
  if not VP8LSetupState(BR, encW, h, True, S) then  // top-level: allow meta-huffman
  begin
    VP8LFreeState(S);
    for i := 0 to numT-1 do
      if transforms[i].Data <> nil then FreeMem(transforms[i].Data);
    Exit;
  end;

  // Decode pixels at encoded width (allocate w*h for COLOR_INDEX expansion)
  nPix   := w * h;
  pixels := AllocMem(nPix * SizeOf(Cardinal));
  ok     := VP8LDecodePixels(S, pixels, encW, h);
  VP8LFreeState(S);

  if not ok then
  begin
    FreeMem(pixels); pixels := nil;
    for i := 0 to numT-1 do
      if transforms[i].Data <> nil then FreeMem(transforms[i].Data);
    Exit;
  end;

  // Apply inverse transforms in reverse order (last-decoded first)
  for t := numT - 1 downto 0 do
  begin
    case transforms[t].TType of
      VP8L_TT_SUBGREEN:
        VP8LInvSubGreen(pixels, encW * h);
      VP8L_TT_COLORXFORM:
        VP8LInvColorXform(transforms[t].Data, transforms[t].Bits, encW, pixels, encW * h);
      VP8L_TT_PREDICTOR:
        VP8LInvPredictor(transforms[t].Data, transforms[t].Bits, encW, h, pixels);
      VP8L_TT_COLORINDEX:
      begin
        VP8LInvColorIndex(transforms[t].Data, transforms[t].Bits, w, pixels, encW, h);
        encW := w; // restore original width
      end;
    end;
    if transforms[t].Data <> nil then FreeMem(transforms[t].Data);
  end;

  Result := True;
end;

function VP8LDecode(Data: PByte; Size: NativeUInt;
  out PixBuf: PByte; out Width, Height: Integer): Boolean;
var
  BR:        TVP8LBitReader;
  w, h, i:   Integer;
  pixels:    PCardinal;
  alphaUsed: Integer;
begin
  Result := False;
  PixBuf := nil;
  Width  := 0;
  Height := 0;
  if Size < 5 then Exit;
  if Data[0] <> $2F then Exit;

  VP8LInitBitReader(BR, Data + 1, Size - 1);
  w := Integer(VP8LReadBits(BR, 14)) + 1;
  h := Integer(VP8LReadBits(BR, 14)) + 1;
  alphaUsed := Integer(VP8LReadBits(BR, 1)); // alpha_is_used
  if VP8LReadBits(BR, 3) <> 0 then Exit; // version
  Width  := w; Height := h;

  if not VP8LDecodeBody(BR, w, h, pixels) then Exit;

  // Convert internal ARGB (0xAARRGGBB, memory B,G,R,A) to RGBA byte order
  // by swapping the R and B bytes, so the returned buffer is true RGBA.
  // If alpha_is_used = 0, the image is opaque: force alpha to 0xFF.
  if alphaUsed = 0 then
    for i := 0 to w * h - 1 do
      pixels[i] := $FF000000
                or (pixels[i] and $0000FF00)
                or ((pixels[i] and $000000FF) shl 16)
                or ((pixels[i] shr 16) and $000000FF)
  else
    for i := 0 to w * h - 1 do
      pixels[i] := (pixels[i] and $FF00FF00)
                or ((pixels[i] and $000000FF) shl 16)
                or ((pixels[i] shr 16) and $000000FF);

  PixBuf := PByte(pixels);
  Result := True;
end;

// ---- Alpha plane decode (ALPH chunk) ----
// Spatial unfilter (in-place, per row; prev = previous row, nil for row 0).
procedure VP8UnfilterAlpha(Filter: Integer; Plane: PByte; W, H: Integer);
var
  y, i, g, pred: Integer;
  cur, prev: PByte;
begin
  if Filter = 0 then Exit;  // WEBP_FILTER_NONE
  prev := nil;
  for y := 0 to H - 1 do
  begin
    cur := Plane + y * W;
    case Filter of
      1: // HORIZONTAL
      begin
        if prev = nil then pred := 0 else pred := prev[0];
        cur[0] := Byte(pred + cur[0]);
        for i := 1 to W - 1 do cur[i] := Byte(cur[i-1] + cur[i]);
      end;
      2: // VERTICAL
      begin
        if prev = nil then
        begin
          for i := 1 to W - 1 do cur[i] := Byte(cur[i-1] + cur[i]);
        end else
          for i := 0 to W - 1 do cur[i] := Byte(prev[i] + cur[i]);
      end;
      3: // GRADIENT
      begin
        if prev = nil then
        begin
          for i := 1 to W - 1 do cur[i] := Byte(cur[i-1] + cur[i]);
        end else
        begin
          cur[0] := Byte(prev[0] + cur[0]);  // left=top=topleft=prev[0]
          for i := 1 to W - 1 do
          begin
            g := Integer(cur[i-1]) + Integer(prev[i]) - Integer(prev[i-1]);
            if g < 0 then pred := 0 else if g > 255 then pred := 255 else pred := g;
            cur[i] := Byte(cur[i] + pred);
          end;
        end;
      end;
    end;
    prev := cur;
  end;
end;

// Decode the ALPH chunk into AlphaOut (W*H bytes). Returns False on error.
function DecodeAlpha(AlphData: PByte; AlphSize: NativeUInt; W, H: Integer;
  AlphaOut: PByte): Boolean;
var
  method, filter, i, n: Integer;
  bitData: PByte;
  bitSize: NativeUInt;
  pixels: PCardinal;
  BR: TVP8LBitReader;
begin
  Result := False;
  if AlphSize < 1 then Exit;
  method := AlphData[0] and 3;
  filter := (AlphData[0] shr 2) and 3;
  bitData := AlphData + 1;
  bitSize := AlphSize - 1;
  n := W * H;
  if method = 0 then
  begin
    // Uncompressed: raw alpha bytes
    if bitSize < NativeUInt(n) then Exit;
    Move(bitData^, AlphaOut^, n);
  end else if method = 1 then
  begin
    // VP8L lossless: headerless stream, alpha = green channel
    VP8LInitBitReader(BR, bitData, bitSize);
    if not VP8LDecodeBody(BR, W, H, pixels) then Exit;
    for i := 0 to n - 1 do AlphaOut[i] := Byte((pixels[i] shr 8) and $FF);
    FreeMem(pixels);
  end else
    Exit;

  VP8UnfilterAlpha(filter, AlphaOut, W, H);
  Result := True;
end;

// ============================================================
// RIFF CONTAINER PARSER
// ============================================================

function ReadLE32(p: PByte): Cardinal; inline;
begin
  Result := p[0] or (Cardinal(p[1]) shl 8) or
            (Cardinal(p[2]) shl 16) or (Cardinal(p[3]) shl 24);
end;

type
  TWebPChunk = record
    FourCC: Cardinal;
    Size:   Cardinal;
    Data:   PByte;
  end;

function FindChunk(const RIFF: PByte; RiffSize: NativeUInt;
  const Tag: AnsiString; out Chunk: TWebPChunk): Boolean;
var
  p:    PByte;
  left: NativeUInt;
  cc, sz: Cardinal;
  tagCC: Cardinal;
begin
  Result := False;
  tagCC := Ord(Tag[1]) or (Cardinal(Ord(Tag[2])) shl 8) or
           (Cardinal(Ord(Tag[3])) shl 16) or (Cardinal(Ord(Tag[4])) shl 24);
  p    := RIFF;
  left := RiffSize;
  while left >= 8 do
  begin
    cc := ReadLE32(p);
    sz := ReadLE32(p + 4);
    if cc = tagCC then
    begin
      Chunk.FourCC := cc;
      Chunk.Size   := sz;
      Chunk.Data   := p + 8;
      Result := True;
      Exit;
    end;
    // align to 2 bytes
    sz := (sz + 1) and (not 1);
    Inc(p, 8 + sz);
    if 8 + sz > left then Break;
    Dec(left, 8 + sz);
  end;
end;

// Parse RIFF header and locate the VP8/VP8L/VP8X chunk.
// Returns: 1 = VP8 lossy, 2 = VP8L lossless, 0 = error
function ParseRIFF(Data: PByte; Size: NativeUInt;
  out ChunkData: PByte; out ChunkSize: NativeUInt;
  out IsLossless: Boolean;
  out HasAlpha: Boolean;
  out AlphaData: PByte; out AlphaSize: NativeUInt): Integer;
var
  riffTag, webpTag: Cardinal;
  riffSize: Cardinal;
  inner: PByte;
  innerSize: NativeUInt;
  chunk: TWebPChunk;
  vp8x_flags: Cardinal;
  alphaChunk: TWebPChunk;
begin
  Result    := 0;
  IsLossless := False;
  HasAlpha   := False;
  ChunkData  := nil;
  ChunkSize  := 0;
  AlphaData  := nil;
  AlphaSize  := 0;
  if Size < 12 then Exit;

  riffTag := ReadLE32(Data);
  riffSize := ReadLE32(Data + 4);
  webpTag := ReadLE32(Data + 8);

  // 'RIFF'
  if riffTag <> $46464952 then Exit;
  // 'WEBP'
  if webpTag <> $50424557 then Exit;

  inner     := Data + 12;
  innerSize := Size - 12;

  // Try VP8X (extended format)
  if FindChunk(inner, innerSize, 'VP8X', chunk) then
  begin
    if chunk.Size >= 10 then
    begin
      vp8x_flags := ReadLE32(chunk.Data);
      HasAlpha    := (vp8x_flags and 16) <> 0;
    end;
    // Locate the alpha chunk (for lossy + alpha)
    if FindChunk(inner, innerSize, 'ALPH', alphaChunk) then
    begin
      AlphaData := alphaChunk.Data;
      AlphaSize := alphaChunk.Size;
    end;
    // Now find actual image chunk
    if FindChunk(inner, innerSize, 'VP8L', chunk) then
    begin
      ChunkData  := chunk.Data;
      ChunkSize  := chunk.Size;
      IsLossless := True;
      Result     := 2;
      Exit;
    end;
    if FindChunk(inner, innerSize, 'VP8 ', chunk) then
    begin
      ChunkData  := chunk.Data;
      ChunkSize  := chunk.Size;
      IsLossless := False;
      Result     := 1;
      Exit;
    end;
    Exit;
  end;

  // Try VP8L directly
  if FindChunk(inner, innerSize, 'VP8L', chunk) then
  begin
    ChunkData  := chunk.Data;
    ChunkSize  := chunk.Size;
    IsLossless := True;
    Result     := 2;
    Exit;
  end;

  // Try VP8 (lossy) directly
  if FindChunk(inner, innerSize, 'VP8 ', chunk) then
  begin
    ChunkData  := chunk.Data;
    ChunkSize  := chunk.Size;
    IsLossless := False;
    Result     := 1;
    Exit;
  end;
end;

// ============================================================
// VP8 LOSSY DECODE (FULL DRIVER)
// ============================================================

function VP8Decode(Data: PByte; DataSize: NativeUInt;
  Mode: TCSMode; out Width, Height: Integer): PByte;
var
  D:          TVP8Decoder;
  outSize:    NativeUInt;
  topBufSize: NativeUInt;
  topBuf:     PByte;
  mbInfoSz:   NativeUInt;
  i:          Integer;
begin
  Result := nil;
  Width  := 0;
  Height := 0;

  FillChar(D, SizeOf(D), 0);
  D.OutputMode := Mode;
  case Mode of
    csmRGB, csmBGR:     D.OutBpp := 3;
    else                D.OutBpp := 4;
  end;
  D.SegHdr.UseSegment := False;
  D.SegHdr.AbsoluteDelta := True;

  if not VP8ParseHeaders(D, Data, DataSize) then Exit;
  Width  := D.PicWidth;
  Height := D.PicHeight;

  // Determine in-loop filter type (0=none, 1=simple, 2=complex)
  if D.FilterLevel = 0 then D.FilterType := 0
  else if D.FilterSimple then D.FilterType := 1
  else D.FilterType := 2;

  // Allocate output buffer
  D.OutStride := D.PicWidth * D.OutBpp;
  outSize  := NativeUInt(D.PicHeight) * NativeUInt(D.OutStride);
  D.OutBuf := AllocMem(outSize);

  // Allocate full-frame YUV planes (size = MB-aligned dimensions)
  D.FYStride  := D.MbW * 16;
  D.FUVStride := D.MbW * 8;
  D.FYPlane := AllocMem(D.FYStride * D.MbH * 16);
  D.FUPlane := AllocMem(D.FUVStride * D.MbH * 8);
  D.FVPlane := AllocMem(D.FUVStride * D.MbH * 8);
  D.FInfo   := PVP8FInfo(AllocMem(D.MbW * D.MbH * SizeOf(TVP8FInfo)));

  VP8PrecomputeFilterStrengths(D);

  // Allocate top-row context buffers
  topBufSize := D.MbW * 32;  // 16Y + 8U + 8V per MB column
  topBuf     := AllocMem(topBufSize);
  FillChar(topBuf^, topBufSize, 127);
  D.YTopBuf  := topBuf;
  D.UTopBuf  := topBuf + D.MbW * 16;
  D.VTopBuf  := topBuf + D.MbW * 24;

  // Allocate MB info array (MbW+1 entries)
  mbInfoSz := (D.MbW + 1) * SizeOf(TVP8MB);
  D.MBInfo  := PVP8MB(AllocMem(mbInfoSz));
  FillChar(D.MBInfo^, mbInfoSz, 0);

  // Allocate I4x4 top-context array and initialize to B_DC_PRED
  D.IntraT := AllocMem(D.MbW * 4 + 4);
  FillChar(D.IntraT^, D.MbW * 4 + 4, B_DC_PRED);

  // Initialize YUV buffer
  FillChar(D.YuvBuf, SizeOf(D.YuvBuf), 128);
  // Left border column (col -1 of Y/U/V, used as left context for first MB column)
  for i := 0 to 15 do D.YuvBuf[Y_OFF + i * BPS - 1] := 129;
  for i := 0 to  7 do D.YuvBuf[U_OFF + i * BPS - 1] := 129;
  for i := 0 to  7 do D.YuvBuf[V_OFF + i * BPS - 1] := 129;

  if VP8DecodeFrame(D) then
  begin
    VP8FilterFrame(D);   // in-loop deblocking filter
    VP8EmitFrame(D);     // YUV planes -> RGB output
    Result := D.OutBuf;
  end
  else
  begin
    FreeMem(D.OutBuf);
    D.OutBuf := nil;
  end;

  FreeMem(topBuf);
  FreeMem(D.MBInfo);
  FreeMem(D.IntraT);
  if D.FYPlane <> nil then FreeMem(D.FYPlane);
  if D.FUPlane <> nil then FreeMem(D.FUPlane);
  if D.FVPlane <> nil then FreeMem(D.FVPlane);
  if D.FInfo   <> nil then FreeMem(D.FInfo);
end;

// ============================================================
// PUBLIC API
// ============================================================

function WebPGetInfo(Data: PByte; DataSize: NativeUInt;
  out Width, Height: Integer): Boolean;
var
  chunkData: PByte;
  chunkSize: NativeUInt;
  isLossless, hasAlpha: Boolean;
  riffType: Integer;
  BR:  TVP8LBitReader;
  tmp: Cardinal;
  alphaData: PByte;
  alphaSize: NativeUInt;
begin
  Result := False;
  Width  := 0;
  Height := 0;
  if DataSize < 12 then Exit;

  riffType := ParseRIFF(Data, DataSize, chunkData, chunkSize, isLossless, hasAlpha, alphaData, alphaSize);
  if riffType = 0 then Exit;

  if isLossless then
  begin
    // VP8L: signature + 14+14 bits
    if chunkSize < 5 then Exit;
    if chunkData[0] <> $2F then Exit;
    VP8LInitBitReader(BR, chunkData + 1, chunkSize - 1);
    Width  := Integer(VP8LReadBits(BR, 14)) + 1;
    Height := Integer(VP8LReadBits(BR, 14)) + 1;
    Result := True;
  end else
  begin
    // VP8: 3-byte frame header + start code + w/h
    if chunkSize < 10 then Exit;
    tmp := chunkData[0] or (Cardinal(chunkData[1]) shl 8) or
           (Cardinal(chunkData[2]) shl 16);
    if (tmp and 1) <> 0 then Exit; // not a key frame
    if (chunkData[3] <> $9D) or (chunkData[4] <> $01) or (chunkData[5] <> $2A) then Exit;
    Width  := (chunkData[6] or (Cardinal(chunkData[7]) shl 8)) and $3FFF;
    Height := (chunkData[8] or (Cardinal(chunkData[9]) shl 8)) and $3FFF;
    Result := True;
  end;
end;

function InternalDecode(Data: PByte; DataSize: NativeUInt;
  Mode: TCSMode; out Width, Height: Integer): PByte;
var
  chunkData: PByte;
  chunkSize: NativeUInt;
  isLossless, hasAlpha: Boolean;
  riffType: Integer;
  lsBuf: PByte;
  lsW, lsH: Integer;
  outBuf: PByte;
  i: Integer;
  alphaData: PByte;
  alphaSize: NativeUInt;
  alphaPlane: PByte;
  aOff: Integer;
begin
  Result := nil;
  Width  := 0;
  Height := 0;

  riffType := ParseRIFF(Data, DataSize, chunkData, chunkSize, isLossless, hasAlpha, alphaData, alphaSize);
  if riffType = 0 then Exit;

  if isLossless then
  begin
    if not VP8LDecode(chunkData, chunkSize, lsBuf, lsW, lsH) then Exit;
    Width  := lsW;
    Height := lsH;
    // lsBuf is RGBA; convert to requested mode if needed
    if Mode = csmRGBA then
    begin
      Result := lsBuf;
      Exit;
    end;
    // Convert RGBA to target mode
    case Mode of
      csmARGB:
      begin
        outBuf := AllocMem(lsW * lsH * 4);
        for i := 0 to lsW * lsH - 1 do
        begin
          outBuf[i*4+0] := lsBuf[i*4+3]; // A
          outBuf[i*4+1] := lsBuf[i*4+0]; // R
          outBuf[i*4+2] := lsBuf[i*4+1]; // G
          outBuf[i*4+3] := lsBuf[i*4+2]; // B
        end;
        FreeMem(lsBuf);
        Result := outBuf;
      end;
      csmBGRA:
      begin
        outBuf := AllocMem(lsW * lsH * 4);
        for i := 0 to lsW * lsH - 1 do
        begin
          outBuf[i*4+0] := lsBuf[i*4+2]; // B
          outBuf[i*4+1] := lsBuf[i*4+1]; // G
          outBuf[i*4+2] := lsBuf[i*4+0]; // R
          outBuf[i*4+3] := lsBuf[i*4+3]; // A
        end;
        FreeMem(lsBuf);
        Result := outBuf;
      end;
      csmRGB:
      begin
        outBuf := AllocMem(lsW * lsH * 3);
        for i := 0 to lsW * lsH - 1 do
        begin
          outBuf[i*3+0] := lsBuf[i*4+0];
          outBuf[i*3+1] := lsBuf[i*4+1];
          outBuf[i*3+2] := lsBuf[i*4+2];
        end;
        FreeMem(lsBuf);
        Result := outBuf;
      end;
      csmBGR:
      begin
        outBuf := AllocMem(lsW * lsH * 3);
        for i := 0 to lsW * lsH - 1 do
        begin
          outBuf[i*3+0] := lsBuf[i*4+2];
          outBuf[i*3+1] := lsBuf[i*4+1];
          outBuf[i*3+2] := lsBuf[i*4+0];
        end;
        FreeMem(lsBuf);
        Result := outBuf;
      end;
    end;
  end else
  begin
    Result := VP8Decode(chunkData, chunkSize, Mode, Width, Height);
    // Decode and apply the alpha plane (VP8X + ALPH chunk), if present.
    if (Result <> nil) and hasAlpha and (alphaData <> nil) and
       (Mode in [csmRGBA, csmBGRA, csmARGB]) then
    begin
      alphaPlane := AllocMem(Width * Height);
      if DecodeAlpha(alphaData, alphaSize, Width, Height, alphaPlane) then
      begin
        if Mode = csmARGB then aOff := 0 else aOff := 3;  // alpha byte offset
        for i := 0 to Width * Height - 1 do
          Result[i*4 + aOff] := alphaPlane[i];
      end;
      FreeMem(alphaPlane);
    end;
  end;
end;

function WebPDecodeRGBA(Data: PByte; DataSize: NativeUInt;
  out Width, Height: Integer): PByte;
begin
  Result := InternalDecode(Data, DataSize, csmRGBA, Width, Height);
end;

function WebPDecodeARGB(Data: PByte; DataSize: NativeUInt;
  out Width, Height: Integer): PByte;
begin
  Result := InternalDecode(Data, DataSize, csmARGB, Width, Height);
end;

function WebPDecodeBGRA(Data: PByte; DataSize: NativeUInt;
  out Width, Height: Integer): PByte;
begin
  Result := InternalDecode(Data, DataSize, csmBGRA, Width, Height);
end;

function WebPDecodeRGB(Data: PByte; DataSize: NativeUInt;
  out Width, Height: Integer): PByte;
begin
  Result := InternalDecode(Data, DataSize, csmRGB, Width, Height);
end;

function WebPDecodeBGR(Data: PByte; DataSize: NativeUInt;
  out Width, Height: Integer): PByte;
begin
  Result := InternalDecode(Data, DataSize, csmBGR, Width, Height);
end;

procedure WebPFree(ptr: Pointer);
begin
  FreeMemory(ptr);
end;

end.
