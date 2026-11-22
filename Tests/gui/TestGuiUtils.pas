{
  Tests of the helpers in Gui/utils.pas.
}
unit TestGuiUtils;

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Options, Utils;

type
  TTestGuiUtils = class(TTestCase)
  published
    procedure TestColorStrings;
    procedure TestEnumStrings;
    procedure TestStrArrayJoin;
    procedure TestColorComponents;
    procedure TestFindFirstFont;
    procedure TestFindDeskewExePath;
    procedure TestEffectiveExecutablePath;
  end;

// Path of the command line tool built next to the test program (in Bin/)
function DeskewExeNextToTests: string;

implementation

uses
  FileUtil, Forms;

function DeskewExeNextToTests: string;
begin
  Result := Application.Location + 'deskew' + GetExeExt;
end;

procedure TTestGuiUtils.TestColorStrings;
begin
  AssertEquals('white', '#FFFFFFFF', ColorToString($FFFFFFFF));
  AssertEquals('semi-transparent red', '#80FF0000', ColorToString($80FF0000));
  AssertEquals('read', IntToHex($80FF0000, 8), IntToHex(StringToColorDef('#80FF0000', 0), 8));
  AssertEquals('round trip', IntToHex($12345678, 8), IntToHex(StringToColorDef(ColorToString($12345678), 0), 8));
  AssertEquals('empty', IntToHex($123, 8), IntToHex(StringToColorDef('', $123), 8));
  AssertEquals('not hex', IntToHex(5, 8), IntToHex(StringToColorDef('#XYZ', 5), 8));
end;

procedure TTestGuiUtils.TestEnumStrings;
begin
  // Prefix of the enum type is left out in the INI file
  AssertEquals('file format', 'Tiff', TEnumUtils<TFileFormat>.EnumToStr(ffTiff));
  AssertEquals('filter', 'DefaultLinear', TEnumUtils<TResamplingFilter>.EnumToStr(rfDefaultLinear));
  AssertEquals('forced format (3 letter prefix)', 'Gray8', TEnumUtils<TForcedOutputFormat>.EnumToStr(fofGray8));
  AssertTrue('file format back', TEnumUtils<TFileFormat>.StrToEnum('Tiff') = ffTiff);
  AssertTrue('forced format back', TEnumUtils<TForcedOutputFormat>.StrToEnum('Rgba32') = fofRgba32);
  AssertTrue('unknown gives first', TEnumUtils<TFileFormat>.StrToEnum('Bogus') = ffSameAsInput);
  AssertTrue('empty gives first', TEnumUtils<TResamplingFilter>.StrToEnum('') = rfDefaultLinear);
end;

procedure TTestGuiUtils.TestStrArrayJoin;
begin
  AssertEquals('two', 'a,b', StrArrayJoin(['a', 'b'], ','));
  AssertEquals('one', 'j90', StrArrayJoin(['j90'], ','));
  AssertEquals('none', '', StrArrayJoin([], ','));
end;

procedure TTestGuiUtils.TestColorComponents;
begin
  AssertEquals('red', $33, GetRedValue($80336699));
  AssertEquals('green', $66, GetGreenValue($80336699));
  AssertEquals('blue', $99, GetBlueValue($80336699));
  AssertEquals('Color32', IntToHex($80336699, 8), IntToHex(Color32($80, $33, $66, $99).Color, 8));
end;

procedure TTestGuiUtils.TestFindFirstFont;
begin
  AssertEquals('none installed', 'Fallback', FindFirstFont(['No Such Font 1', 'No Such Font 2'], 'Fallback'));
  AssertEquals('empty list', 'Fallback', FindFirstFont([], 'Fallback'));
end;

procedure TTestGuiUtils.TestFindDeskewExePath;
begin
  // The CLI is built into Bin/ next to the tests (as next to the GUI in releases)
  AssertTrue('Command line tool not built: ' + DeskewExeNextToTests, FileExists(DeskewExeNextToTests));
  AssertEquals('next to the program', DeskewExeNextToTests, FindDeskewExePath);
end;

procedure TTestGuiUtils.TestEffectiveExecutablePath;
var
  Opts: TOptions;
begin
  Opts := TOptions.Create;
  try
    AssertEquals('default', FindDeskewExePath, Opts.EffectiveExecutablePath);
    Opts.DefaultExecutable := False;
    AssertEquals('custom, empty', '', Opts.EffectiveExecutablePath);
    Opts.CustomExecutablePath := '  ' + DeskewExeNextToTests + ' ';
    AssertEquals('custom, full path', DeskewExeNextToTests, Opts.EffectiveExecutablePath);
    Opts.CustomExecutablePath := 'surely-no-such-deskew-tool';
    AssertEquals('custom, not found', '', Opts.EffectiveExecutablePath);
  finally
    Opts.Free;
  end;
end;

initialization
  RegisterTest(TTestGuiUtils);
end.
