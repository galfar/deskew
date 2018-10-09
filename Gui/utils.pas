unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function FindDeskewExePath: string;

implementation

uses
  Forms;

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

{$ELSEIF Defined(LINUX)}
    S := ExeDir + 'deskew';
    if FileExists(S) then
      Exit(S);
{$ENDIF}
  end;
end;

end.

