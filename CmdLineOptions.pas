{
  Deskew
  by Marek Mauder
  http://galfar.vevb.net/deskew

  The contents of this file are used with permission, subject to the Mozilla
  Public License Version 1.1 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
}

unit CmdLineOptions;

interface

uses
  Types,
  SysUtils,
  Classes,
  ImagingTypes;

const
  DefaultThreshold = 128;
  DefaultMaxAngle = 10;
  SDefaultOutputFile = 'out.png';

type
  TThresholdingMethod = (
    // Use explicit threshold [0..255]
    tmExplicit,
    // Use adaptive thresholding: Otsu's method
    tmOtsu
  );

  TCmdLineOptions = class
  private
    FInputFile: string;
    FOutputFile: string;
    FMaxAngle: Integer;
    FThresholdingMethod: TThresholdingMethod;
    FThresholdLevel: Integer;
    FMargins: TRect;
    FBackgroundColor: TColor32;
    FShowStats: Boolean;
    function GetIsValid: Boolean;
  public
    constructor Create;
    // Parses command line arguments to get optiosn set by user
    procedure ParseCommnadLine;

    property InputFile: string read FInputFile;
    property OutputFile: string read FOutputFile;
    // Max expected rotation angle (algo then works in range <-MaxAngle, MaxAngle>)
    property MaxAngle: Integer read FMaxAngle;
    // Thresholding method used when converting images to binary black/white format
    property ThresholdingMethod: TThresholdingMethod read FThresholdingMethod;
    // Threshold for black/white pixel classification for explicit thresholding method
    property ThresholdLevel: Integer read FThresholdLevel;

    property Margins: TRect read FMargins;
    property BackgroundColor: TColor32 read FBackgroundColor;
    property ShowStats: Boolean read FShowStats;

    property IsValid: Boolean read GetIsValid;
  end;

implementation

{ TCmdLineOptions }

constructor TCmdLineOptions.Create;
begin
  FThresholdLevel := DefaultThreshold;
  FMaxAngle := DefaultMaxAngle;
  FThresholdingMethod := tmOtsu;
  FMargins := Rect(0, 0, 0, 0);
  FBackgroundColor := 0;
  FShowStats := False;
  FOutputFile := SDefaultOutputFile;
end;

function TCmdLineOptions.GetIsValid: Boolean;
begin
  Result := (InputFile <> '') and (MaxAngle > 0) and
    ((ThresholdingMethod in [tmOtsu]) or (ThresholdingMethod = tmExplicit) and (ThresholdLevel > 0));
end;

procedure TCmdLineOptions.ParseCommnadLine;
var
  I: LongInt;
  Param, Arg, Dir: string;

  procedure CheckParam(const Param, Value: string);
  begin
    if Param = '-o' then
      FOutputFile := Value
    else if Param = '-a' then
      FMaxAngle := StrToIntDef(Value, -1)
    else if Param = '-t' then
    begin
      if Value = 'a' then
        FThresholdingMethod := tmOtsu
      else
      begin
        FThresholdingMethod := tmExplicit;
        FThresholdLevel := StrToIntDef(Value, -1);
      end;
    end;
  end;

begin
  I := 1;

  while I <= ParamCount do
  begin
    Param := ParamStr(I);
    if Pos('-', Param) = 1 then
    begin
      Arg := ParamStr(I + 1);
      Inc(I);
      CheckParam(Param, Arg);
    end
    else
      FInputFile := Param;

    Inc(I);
  end;
end;

end.
