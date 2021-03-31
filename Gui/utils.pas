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


function FindDeskewExePath: string;
function DetermineConfigFolder: string;
function ColorToString(Color: TColor32): string;
function StringToColorDef(const Str: string; Default: TColor32): TColor32;
function StrArrayJoin(const StringArray: array of string; const Separator: string): string;

implementation

uses
{$IF Defined(DARWIN)}
  StrUtils,
{$ENDIF}
  LazFileUtils, Forms;

function FindDeskewExePath: string;
var
  ExeDir, S: string;
begin
  Result := './deskew';

  ExeDir := Application.Location;
  if DirectoryExists(ExeDir) then
  begin
{$IF Defined(MSWINDOWS)}
  {$IF Defined(CPU386)}
    S := ExeDir + 'deskew32.exe';
    if FileExists(S) then
      Exit(S);
  {$ELSE}
    S := ExeDir + 'deskew64.exe';
    if FileExists(S) then
      Exit(S);
  {$ENDIF}
    S := ExeDir + 'deskew.exe';
    if FileExists(S) then
      Exit(S);
{$ELSEIF Defined(DARWIN)}
    S := ExeDir + 'deskew-mac';
    if FileExists(S) then
      Exit(S);

    S := ExeDir + 'deskew';
    if FileExists(S) then
      Exit(S);

    if AnsiContainsText(ExeDir, '.app/Contents/MacOS') then
    begin
      // Get out af the bundle
      S := ExtractFileDir(ExtractFileDir(ExtractFileDir(ExcludeTrailingPathDelimiter(ExeDir)))) + '/deskew-mac';
      if FileExists(S) then
        Exit(S);
    end;
{$ELSEIF Defined(LINUX)}
    S := ExeDir + 'deskew';
    if FileExists(S) then
      Exit(S);
{$ENDIF}
  end;
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

