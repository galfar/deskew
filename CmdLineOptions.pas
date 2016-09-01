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
  StrUtils,
  ImagingTypes,
  ImagingUtility;

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
    FMaxAngle: Double;
    FSkipAngle: Double;
    FThresholdingMethod: TThresholdingMethod;
    FThresholdLevel: Integer;
    FContentRect: TRect;
    FBackgroundColor: TColor32;
    FForcedOutputFormat: TImageFormat;
    FShowStats: Boolean;
    FShowParams: Boolean;
    FShowTimings: Boolean;
    FFormatSettings: TFormatSettings;
    FErrorMessage: string;
    function GetIsValid: Boolean;
  public
    constructor Create;
    // Parses command line arguments to get optiosn set by user
    function ParseCommnadLine: Boolean;
    function OptionsToString: string;

    property InputFile: string read FInputFile;
    property OutputFile: string read FOutputFile;
    // Max expected rotation angle - algo then works in range [-MaxAngle, MaxAngle]
    property MaxAngle: Double read FMaxAngle;
    // Skew threshold angle - skip deskewing if detected skew angle is in range (-MinAngle, MinAngle)
    property SkipAngle: Double read FSkipAngle;
    // Thresholding method used when converting images to binary black/white format
    property ThresholdingMethod: TThresholdingMethod read FThresholdingMethod;
    // Threshold for black/white pixel classification for explicit thresholding method
    property ThresholdLevel: Integer read FThresholdLevel;
    // Rect where to do the skew detection on the page image
    property ContentRect: TRect read FContentRect;
    // Background color for the rotated image
    property BackgroundColor: TColor32 read FBackgroundColor;
    // Forced output format (applied just before saving the output)
    property ForcedOutputFormat: TImageFormat read FForcedOutputFormat;
    // Show skew detection stats
    property ShowStats: Boolean read FShowStats;
    // Show current params to user (for testing etc.)
    property ShowParams: Boolean read FShowParams;
    // Show timing of processing steps to user
    property ShowTimings: Boolean read FShowTimings;

    property IsValid: Boolean read GetIsValid;
    property ErrorMessage: string read FErrorMessage;
  end;

implementation

uses
  Imaging;

{ TCmdLineOptions }

constructor TCmdLineOptions.Create;
begin
  FThresholdLevel := DefaultThreshold;
  FMaxAngle := DefaultMaxAngle;
  FSkipAngle := 0.01;
  FThresholdingMethod := tmOtsu;
  FContentRect := Rect(0, 0, 0, 0); // whole page
  FBackgroundColor := $FF000000;
  FOutputFile := SDefaultOutputFile;
  FShowStats := False;
  FShowParams := False;
  FShowTimings:= False;
  FForcedOutputFormat := ifUnknown;
  FFormatSettings := ImagingUtility.GetFormatSettingsForFloats;
end;

function TCmdLineOptions.GetIsValid: Boolean;
begin
  Result := (InputFile <> '') and (MaxAngle > 0) and (SkipAngle >= 0) and
    ((ThresholdingMethod in [tmOtsu]) or (ThresholdingMethod = tmExplicit) and (ThresholdLevel > 0));
end;

function TCmdLineOptions.ParseCommnadLine: Boolean;
var
  I: LongInt;
  Param, Arg: string;

  // From Delphi RTL StrUtils.pas - for compiling in Delphi 7
  function SplitString(const S, Delimiters: string): TDynStringArray;
  var
    StartIdx: Integer;
    FoundIdx: Integer;
    SplitPoints: Integer;
    CurrentSplit: Integer;
    I: Integer;

    function FindDelimiter(const Delimiters, S: string; StartIdx: Integer = 1): Integer;
    var
      Stop: Boolean;
      Len: Integer;
    begin
      Result := 0;
      Len := Length(S);
      Stop := False;
      while (not Stop) and (StartIdx <= Len) do
        if IsDelimiter(Delimiters, S, StartIdx) then
        begin
          Result := StartIdx;
          Stop := True;
        end
        else
          Inc(StartIdx);
    end;

  begin
    Result := nil;
    if S <> '' then
    begin
      SplitPoints := 0;
      for I := 1 to Length(S) do
      begin
        if IsDelimiter(Delimiters, S, I) then
          Inc(SplitPoints);
      end;

      SetLength(Result, SplitPoints + 1);
      StartIdx := 1;
      CurrentSplit := 0;
      repeat
        FoundIdx := FindDelimiter(Delimiters, S, StartIdx);
        if FoundIdx <> 0 then
        begin
          Result[CurrentSplit] := Copy(S, StartIdx, FoundIdx - StartIdx);
          Inc(CurrentSplit);
          StartIdx := FoundIdx + 1;
        end;
      until CurrentSplit = SplitPoints;
      Result[SplitPoints] := Copy(S, StartIdx, Length(S) - StartIdx + 1);
    end;
  end;

  function CheckParam(const Param, Value: string): Boolean;
  var
    StrArray: TDynStringArray;
    ValLower: string;
    TempColor: Cardinal;
  begin
    Result := True;
    ValLower := LowerCase(Value);

    if Param = '-o' then
      FOutputFile := Value
    else if Param = '-a' then
      FMaxAngle := StrToFloatDef(Value, -1, FFormatSettings)
    else if Param = '-l' then
      FSkipAngle := StrToFloatDef(Value, -1, FFormatSettings)
    else if Param = '-t' then
    begin
      if ValLower = 'a' then
        FThresholdingMethod := tmOtsu
      else
      begin
        FThresholdingMethod := tmExplicit;
        FThresholdLevel := StrToIntDef(Value, -1);
      end;
    end
    else if Param = '-b' then
    begin
      TempColor := Cardinal(StrToInt64('$' + ValLower) and $FFFFFFFF);
      if TempColor <= $FF then
      begin
        // Just one channel given, replicate for all channels + opaque
        FBackgroundColor := Color32($FF, Byte(TempColor), Byte(TempColor), Byte(TempColor)).Color;
      end
      else if TempColor <= $FFFFFF then
      begin
        // RGB given, set alpha to 255 for background
        FBackgroundColor := $FF000000 or TempColor;
      end
      else
      begin
        // Full ARGB given
        FBackgroundColor := TempColor;
      end;
    end
    else if Param = '-f' then
    begin
      if ValLower = 'b1' then
        FForcedOutputFormat := ifBinary
      else if ValLower = 'g8' then
        FForcedOutputFormat := ifGray8
      else if ValLower = 'rgb24' then
        FForcedOutputFormat := ifR8G8B8
      else if ValLower = 'rgba32' then
        FForcedOutputFormat := ifA8R8G8B8;
    end
    else if Param = '-s' then
    begin
      if Pos('s', ValLower) > 0 then
        FShowStats := True;
      if Pos('p', ValLower) > 0 then
        FShowParams := True;
      if Pos('t', ValLower) > 0 then
        FShowTimings := True;
    end
    else if Param = '-r' then
    begin
      StrArray := SplitString(ValLower, ',');
      if Length(StrArray) = 4 then
      begin
        FContentRect.Left := StrToInt(StrArray[0]);
        FContentRect.Top := StrToInt(StrArray[1]);
        FContentRect.Right := StrToInt(StrArray[2]);
        FContentRect.Bottom := StrToInt(StrArray[3]);
      end;
    end
    else
    begin
      FErrorMessage := 'Unknown parameter: ' + Param;
      Result := False;
    end;
  end;

begin
  Result := True;
  I := 1;

  while I <= ParamCount do
  begin
    Param := ParamStr(I);
    if Pos('-', Param) = 1 then
    begin
      Arg := ParamStr(I + 1);
      Inc(I);
      if not CheckParam(Param, Arg) then
      begin
        Result := False;
        Exit;
      end;
    end
    else
      FInputFile := Param;

    Inc(I);
  end;
end;

function TCmdLineOptions.OptionsToString: string;
begin
  Result :=
    '  input file          = ' + InputFile + sLineBreak +
    '  output file         = ' + OutputFile + sLineBreak +
    '  max angle           = ' + FloatToStr(MaxAngle) + sLineBreak +
    '  background color    = ' + IntToHex(BackgroundColor, 8) + sLineBreak +
    '  thresholding method = ' + Iff(ThresholdingMethod = tmExplicit, 'explicit', 'auto otsu') + sLineBreak +
    '  threshold level     = ' + IntToStr(ThresholdLevel) + sLineBreak +
    '  content rect        = ' + Format('%d,%d,%d,%d', [ContentRect.Left, ContentRect.Top, ContentRect.Right, ContentRect.Bottom]) + sLineBreak +
    '  output format       = ' + Iff(ForcedOutputFormat = ifUnknown, 'default', Imaging.GetFormatName(ForcedOutputFormat)) + sLineBreak +
    '  skip angle          = ' + FloatToStr(SkipAngle) + sLineBreak +
    '  show info           = ' + Iff(ShowParams, 'params ', '') + Iff(ShowStats, 'stats ', '') + Iff(ShowTimings, 'timings ', '') + sLineBreak;
end;

end.
