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
unit Utils;

interface

uses
  Classes, SysUtils, TypInfo, IniFiles, ImagingTypes;

type
  // Workaround for generic functions needing FPC 3.1.1+
  TEnumUtils<T> = class
  public
    class function EnumToStr(const EnumValue: T): string;
    class function StrToEnum(const Str: string): T;
    class function GetEnumPrefix: string;
  end;

  TIniFileHelper = class helper for TIniFile
  public
    function ReadNiceBool(const Section, Ident: string; Default: Boolean): Boolean;
    procedure WriteNiceBool(const Section, Ident: string; Value: Boolean);
  end;


// Returns the path of Deskew CLI next to the GUI or in PATH, empty string if not found
function FindDeskewExePath: string;
function DetermineConfigFolder: string;
function ColorToString(Color: TColor32): string;
function StringToColorDef(const Str: string; Default: TColor32): TColor32;
function StrArrayJoin(const StringArray: array of string; const Separator: string): string;
// Returns the first of FontNames that is installed, Default if there is none
function FindFirstFont(const FontNames: array of string; const Default: string): string;

function Color32(A, R, G, B: Byte): TColor32Rec; inline;
function GetRedValue(Color32: TColor32): Byte; inline;
function GetGreenValue(Color32: TColor32): Byte; inline;
function GetBlueValue(Color32: TColor32): Byte; inline;

const
{$I ../BuildTarget.inc}

implementation

uses
{$IF Defined(DARWIN)}
  StrUtils,
{$ENDIF}
  LazFileUtils, FileUtil, Forms;

function FindDeskewExePath: string;
var
  ExeDir, S: string;
begin
  ExeDir := Application.Location;
  if DirectoryExists(ExeDir) then
  begin
{$IF Defined(MSWINDOWS)}
    S := ExeDir + 'deskew.exe';
    if FileExists(S) then
      Exit(S);
{$ELSEIF Defined(DARWIN)}
    S := ExeDir + 'deskew';
    if FileExists(S) then
      Exit(S);

    if AnsiContainsText(ExeDir, '.app/Contents/MacOS') then
    begin
      // Get out af the bundle
      S := ExtractFileDir(ExtractFileDir(ExtractFileDir(ExcludeTrailingPathDelimiter(ExeDir)))) + '/deskew';
      if FileExists(S) then
        Exit(S);
    end;
{$ELSEIF Defined(LINUX)}
    S := ExeDir + 'deskew';
    if FileExists(S) then
      Exit(S);
{$ENDIF}
  end;

  // Installed elsewhere, e.g. by a package manager
  Result := FindDefaultExecutablePath('deskew');
{$IF Defined(DARWIN)}
  // Apps started from Finder do not get PATH of the shell, try where Homebrew installs
  if Result = '' then
  begin
    S := '/opt/homebrew/bin/deskew'; // Apple Silicon
    if FileExists(S) then
      Exit(S);
    S := '/usr/local/bin/deskew'; // Intel
    if FileExists(S) then
      Exit(S);
  end;
{$ENDIF}
end;

function DetermineConfigFolder: string;
var
  ExeDir: string;
begin
  Result := GetAppConfigDir(False);

  ExeDir := Application.Location;
  if DirectoryExists(ExeDir) and DirectoryIsWritable(ExeDir) then
    Result := ExeDir;
end;

function ColorToString(Color: TColor32): string;
begin
  Result := '#' + HexStr(Color, 8);
end;

function StringToColorDef(const Str: string; Default: TColor32): TColor32;
var
  S: string;
begin
  S := '$' + Copy(Str, 2);
  Result := StrToDWordDef(S, Default);
end;

function StrArrayJoin(const StringArray: array of string; const Separator: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(StringArray) to High(StringArray) do
    Result := Result + StringArray[I] + Separator;

  Delete(Result, Length(Result), 1);
end;

function FindFirstFont(const FontNames: array of string; const Default: string): string;
var
  I: Integer;
begin
  for I := Low(FontNames) to High(FontNames) do
  begin
    if Screen.Fonts.IndexOf(FontNames[I]) >= 0 then
      Exit(FontNames[I]);
  end;
  Result := Default;
end;

function Color32(A, R, G, B: Byte): TColor32Rec;
begin
  Result.A := A;
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

function GetRedValue(Color32: TColor32): Byte;
begin
  Result := (Color32 shr 16) and $FF;
end;

function GetGreenValue(Color32: TColor32): Byte;
begin
  Result := (Color32 shr 8) and $FF;
end;

function GetBlueValue(Color32: TColor32): Byte;
begin
  Result := Color32 and $FF;
end;

class function TEnumUtils<T>.EnumToStr(const EnumValue: T): string;
var
  S: string;
  L: Integer;
begin
  S := TypInfo.GetEnumName(TypeInfo(T), Integer(EnumValue));
  L := Length(GetEnumPrefix);
  Result := Copy(S, L + 1);
end;

class function TEnumUtils<T>.StrToEnum(const Str: string): T;
var
  S: string;
  I: Integer;
begin
  S := GetEnumPrefix + Str;
  I := TypInfo.GetEnumValue(TypeInfo(T), S);
  if I >= 0 then
    Result := T(I)
  else
    Result := Default(T);
end;

class function TEnumUtils<T>.GetEnumPrefix: string;
var
  S: string;
begin
  S := TypInfo.GetEnumName(TypeInfo(T), Integer(Default(T)));
  Result := Copy(S, 1, 2);
  if S[3] in ['a'..'z'] then
    Result := Result + S[3];
end;

function TIniFileHelper.ReadNiceBool(const Section, Ident: string; Default: Boolean): Boolean;
begin
  Result := StrToBoolDef(ReadString(Section, Ident, ''),  Default);
end;

procedure TIniFileHelper.WriteNiceBool(const Section, Ident: string; Value: Boolean);
begin
  WriteString(Section, Ident, BoolToStr(Value, True));
end;

end.

