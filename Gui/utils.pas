unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function FindDeskewExePath: string;

implementation

uses
  Forms, StrUtils;

function FindDeskewExePath: string;
var
  ExeDir, S: string;
begin
  Result := './deskew';

  ExeDir := Application.Location;
  if DirectoryExists(ExeDir) then
  begin
{$IF Defined(MSWINDOWS)}
    S := ExeDir + 'deskew64.exe';
    if FileExists(S) then
      Exit(S);

    S := ExeDir + 'deskew.exe';
    if FileExists(S) then
      Exit(S);

    S := ExeDir + 'deskew32.exe';
    if FileExists(S) then
      Exit(S);
{$ELSEIF Defined(DARWIN)}
    S := ExeDir + 'deskew-osx';
    if FileExists(S) then
      Exit(S);

    S := ExeDir + 'deskew';
    if FileExists(S) then
      Exit(S);

    if AnsiContainsText(ExeDir, '.app/Contents/MacOS') then
    begin
      // Get out af the bundle
      S := ExtractFileDir(ExtractFileDir(ExtractFileDir(ExcludeTrailingPathDelimiter(ExeDir)))) + '/deskew-osx';
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

end.

