{
  Deskew
  by Marek Mauder
  https://galfar.vevb.net/deskew
  https://github.com/galfar/deskew
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
}

{ Build target info constants, included in the interface const section of
  Utils units of both CLI (Utils.pas) and GUI (Gui/utils.pas).
  Only compiler-native defines are used here (FPC and Delphi). }

  // CPU architecture
  SBuildCpu =
  {$IF Defined(CPUX86_64) or Defined(CPUX64)}
    'x64'
  {$ELSEIF Defined(CPUI386) or Defined(CPUX86)}
    'x86'
  {$ELSEIF Defined(CPUAARCH64) or Defined(CPUARM64)}
    'ARM64'
  {$ELSEIF Defined(CPUARMV7A) or Defined(CPUARMV7)}
    'ARMv7'
  {$ELSEIF Defined(CPUARMV6) or Defined(CPUARMV6K) or Defined(CPUARMV6T2) or Defined(CPUARMV6Z)}
    'ARMv6'
  {$ELSEIF Defined(CPUARM) or Defined(CPUARM32)}
    'ARM'
  {$ELSEIF Defined(CPUPOWERPC64) and Defined(FPC_LITTLE_ENDIAN)}
    'PowerPC64LE'
  {$ELSEIF Defined(CPUPOWERPC64)}
    'PowerPC64'
  {$ELSEIF Defined(CPURISCV64)}
    'RISC-V64'
  {$ELSEIF Defined(FPC)}
    {$I %FPCTARGETCPU%}
  {$ELSE}
    'unknown CPU'
  {$IFEND};

  // Operating system
  SBuildOs =
  {$IF Defined(ANDROID)}
    'Android'
  {$ELSEIF Defined(MSWINDOWS)}
    'Windows'
  {$ELSEIF Defined(LINUX)}
    'Linux'
  {$ELSEIF Defined(IOS)}
    'iOS'
  {$ELSEIF Defined(DARWIN) or Defined(MACOS)}
    'macOS'
  {$ELSEIF Defined(FREEBSD)}
    'FreeBSD'
  {$ELSEIF Defined(OPENBSD)}
    'OpenBSD'
  {$ELSEIF Defined(NETBSD)}
    'NetBSD'
  {$ELSEIF Defined(FPC)}
    {$I %FPCTARGETOS%}
  {$ELSE}
    'unknown OS'
  {$IFEND};

  // Compiler
  SBuildCompiler = {$IFDEF FPC}'FPC'{$ELSE}'DCC'{$ENDIF};

  // e.g. 'ARMv7 Linux', 'x64 Windows'
  SBuildTarget = SBuildCpu + ' ' + SBuildOs;
