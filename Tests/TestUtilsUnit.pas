unit TestUtilsUnit;

interface

uses
  Types, Classes, SysUtils,
  DeskewTestUtils,
  ImagingTypes, Imaging, ImagingUtility,
  Utils;

type
  TTestUtils = class(TDeskewTestCase)
  published
    procedure TestIsRectNull;
    procedure TestIsFloatRectNull;
    procedure TestRectToStr;
    procedure TestCalcRectInPixels;
    procedure TestCalcRectInPixelsPhysicalUnits;
    procedure TestCalcRectInPixelsWithoutDpi;
    procedure TestPathDelimiterAndDirectory;
  end;

  TTestJsonWriter = class(TDeskewTestCase)
  published
    procedure TestQuote;
    procedure TestQuoteNonAscii;
    procedure TestFloat;
    procedure TestFloatIgnoresLocale;
    procedure TestEmptyObject;
    procedure TestDocument;
  end;

implementation

uses
  Math;

{ TTestUtils }

procedure TTestUtils.TestIsRectNull;
begin
  AssertTrue('NullRect', IsRectNull(NullRect));
  AssertTrue('all zero', IsRectNull(Rect(0, 0, 0, 0)));
  AssertFalse('left', IsRectNull(Rect(1, 0, 0, 0)));
  AssertFalse('top', IsRectNull(Rect(0, 1, 0, 0)));
  AssertFalse('right', IsRectNull(Rect(0, 0, 1, 0)));
  AssertFalse('bottom', IsRectNull(Rect(0, 0, 0, 1)));
  AssertFalse('negative', IsRectNull(Rect(-1, 0, 0, 0)));
  // Empty is not the same as null
  AssertFalse('empty but not null', IsRectNull(Rect(10, 10, 10, 10)));
end;

procedure TTestUtils.TestIsFloatRectNull;
begin
  AssertTrue('NullFloatRect', IsFloatRectNull(NullFloatRect));
  AssertTrue('all zero', IsFloatRectNull(FloatRect(0, 0, 0, 0)));
  AssertFalse('left', IsFloatRectNull(FloatRect(0.5, 0, 0, 0)));
  AssertFalse('top', IsFloatRectNull(FloatRect(0, 0.5, 0, 0)));
  AssertFalse('right', IsFloatRectNull(FloatRect(0, 0, 0.5, 0)));
  AssertFalse('bottom', IsFloatRectNull(FloatRect(0, 0, 0, 0.001)));
  AssertFalse('negative', IsFloatRectNull(FloatRect(0, 0, 0, -1)));
end;

procedure TTestUtils.TestRectToStr;
begin
  AssertEquals('positive', '[10,20,300,400]', RectToStr(Rect(10, 20, 300, 400)));
  AssertEquals('null', '[0,0,0,0]', RectToStr(NullRect));
  AssertEquals('negative', '[-5,-1,0,7]', RectToStr(Rect(-5, -1, 0, 7)));
  AssertEquals('large', '[0,0,48000,44800]', RectToStr(Rect(0, 0, 48000, 44800)));
end;

procedure TTestUtils.TestCalcRectInPixels;
var
  Bounds: TRect;
  Metadata: TMetadata;
begin
  Bounds := Rect(0, 0, 500, 1000);
  Metadata := TMetadata.Create;
  try
    // Pixels are taken as they are, rounded
    AssertEquals('px', Rect(10, 20, 490, 980),
      CalcRectInPixels(FloatRect(10, 20, 490, 980), suPixels, Bounds, Metadata));
    // Round() takes halves to the even number: 489.5 -> 490
    AssertEquals('px rounded', Rect(10, 21, 490, 980),
      CalcRectInPixels(FloatRect(10.4, 20.6, 489.5, 980.2), suPixels, Bounds, Metadata));

    // Percents of the width for horizontal values, of the height for vertical ones
    AssertEquals('%', Rect(50, 200, 450, 800),
      CalcRectInPixels(FloatRect(10, 20, 90, 80), suPercent, Bounds, Metadata));
    AssertEquals('% whole', Bounds,
      CalcRectInPixels(FloatRect(0, 0, 100, 100), suPercent, Bounds, Metadata));
    AssertEquals('% fractions', Rect(3, 6, 497, 994),
      CalcRectInPixels(FloatRect(0.6, 0.6, 99.4, 99.4), suPercent, Bounds, Metadata));
  finally
    Metadata.Free;
  end;
end;

procedure TTestUtils.TestCalcRectInPixelsPhysicalUnits;
var
  Bounds: TRect;
  Metadata: TMetadata;
begin
  Bounds := Rect(0, 0, 2480, 3508);
  Metadata := TMetadata.Create;
  try
    // 254 DPI: 1 in = 254 px, 1 cm = 100 px, 1 mm = 10 px
    Metadata.SetPhysicalPixelSize(ruDpi, 254, 254);
    AssertEquals('mm', Rect(100, 200, 2400, 3400),
      CalcRectInPixels(FloatRect(10, 20, 240, 340), suMm, Bounds, Metadata));
    AssertEquals('cm', Rect(100, 250, 2400, 3400),
      CalcRectInPixels(FloatRect(1, 2.5, 24, 34), suCm, Bounds, Metadata));
    AssertEquals('in', Rect(254, 127, 2286, 3302),
      CalcRectInPixels(FloatRect(1, 0.5, 9, 13), suInch, Bounds, Metadata));

    // Different horizontal and vertical resolution
    Metadata.SetPhysicalPixelSize(ruDpi, 254, 127);
    AssertEquals('cm, X and Y DPI differ', Rect(100, 50, 200, 100),
      CalcRectInPixels(FloatRect(1, 1, 2, 2), suCm, Bounds, Metadata));
    AssertEquals('in, X and Y DPI differ', Rect(254, 127, 508, 254),
      CalcRectInPixels(FloatRect(1, 1, 2, 2), suInch, Bounds, Metadata));

    // Pixels and percents do not care about DPI
    AssertEquals('px with DPI', Rect(1, 2, 3, 4),
      CalcRectInPixels(FloatRect(1, 2, 3, 4), suPixels, Bounds, Metadata));
  finally
    Metadata.Free;
  end;
end;

procedure TTestUtils.TestCalcRectInPixelsWithoutDpi;
var
  Bounds: TRect;
  Metadata: TMetadata;
begin
  Bounds := Rect(0, 0, 500, 1000);
  Metadata := TMetadata.Create;
  try
    // Physical units need the resolution, null rect tells it is not known
    AssertTrue('mm', IsRectNull(CalcRectInPixels(FloatRect(10, 10, 20, 20), suMm, Bounds, Metadata)));
    AssertTrue('cm', IsRectNull(CalcRectInPixels(FloatRect(1, 1, 2, 2), suCm, Bounds, Metadata)));
    AssertTrue('in', IsRectNull(CalcRectInPixels(FloatRect(1, 1, 2, 2), suInch, Bounds, Metadata)));
    // These work without it
    AssertEquals('px', Rect(10, 10, 20, 20),
      CalcRectInPixels(FloatRect(10, 10, 20, 20), suPixels, Bounds, Metadata));
    AssertEquals('%', Rect(50, 100, 100, 200),
      CalcRectInPixels(FloatRect(10, 10, 20, 20), suPercent, Bounds, Metadata));
  finally
    Metadata.Free;
  end;
end;

procedure TTestUtils.TestPathDelimiterAndDirectory;
var
  CurrentDir: string;
begin
  AssertFalse('empty', EndsWithPathDelimiter(''));
  AssertTrue('slash', EndsWithPathDelimiter('reports/'));
  AssertTrue('path delimiter', EndsWithPathDelimiter('reports' + PathDelim));
  AssertFalse('file name', EndsWithPathDelimiter('reports/out.png'));

  AssertFalse('dir: empty', IsDirectoryName(''));
  AssertTrue('dir: trailing slash, does not exist', IsDirectoryName('no-such-dir-here/'));
  AssertFalse('dir: does not exist', IsDirectoryName('no-such-dir-here'));
  CurrentDir := ExcludeTrailingPathDelimiter(GetCurrentDir);
  AssertTrue('dir: existing', IsDirectoryName(CurrentDir));
end;

{ TTestJsonWriter }

procedure TTestJsonWriter.TestQuote;
begin
  AssertEquals('empty', '""', JsonQuote(''));
  AssertEquals('plain', '"deskew 1.35"', JsonQuote('deskew 1.35'));
  AssertEquals('quotes', '"say \"hi\""', JsonQuote('say "hi"'));
  AssertEquals('Windows path', '"C:\\Scans\\page 1.tif"', JsonQuote('C:\Scans\page 1.tif'));
  AssertEquals('Unix path', '"/home/me/page.tif"', JsonQuote('/home/me/page.tif'));
  AssertEquals('whitespace', '"a\tb\r\nc"', JsonQuote('a'#9'b'#13#10'c'));
  AssertEquals('control chars', '"\u0001\u001F\u007F"', JsonQuote(#1#31#127));
  AssertEquals('backspace, form feed', '"\b\f"', JsonQuote(#8#12));
end;

procedure TTestJsonWriter.TestQuoteNonAscii;
var
  S: UnicodeString;
begin
  // Built from the codes so the test does not depend on encoding of this file
  S := 'sken';
  S := S + WideChar($010D) + WideChar($00E9) + '.png';
  AssertEquals('Latin', '"sken\u010D\u00E9.png"', JsonQuote(S));

  S := WideChar($65E5);
  S := S + WideChar($672C);
  AssertEquals('CJK', '"\u65E5\u672C"', JsonQuote(S));

  // Characters outside of BMP are two escapes (surrogate pair)
  S := WideChar($D83D);
  S := S + WideChar($DE00);
  AssertEquals('surrogates', '"\uD83D\uDE00"', JsonQuote(S));
end;

procedure TTestJsonWriter.TestFloat;
begin
  AssertEquals('zero', '0', JsonFloat(0));
  AssertEquals('whole number', '10', JsonFloat(10));
  AssertEquals('one decimal', '0.1', JsonFloat(0.1));
  AssertEquals('negative', '-6.25', JsonFloat(-6.25));
  AssertEquals('default decimals', '0.333333', JsonFloat(1 / 3));
  AssertEquals('3 decimals', '0.984', JsonFloat(0.98371, 3));
  AssertEquals('rounded up', '1.5', JsonFloat(1.4996, 3));
  AssertEquals('no decimals', '77', JsonFloat(77.4, 0));
  AssertEquals('small one rounded to zero', '0', JsonFloat(0.0000001));
  AssertEquals('large', '2150400000', JsonFloat(2150400000.0));
  AssertEquals('NaN', 'null', JsonFloat(NaN));
  AssertEquals('infinity', 'null', JsonFloat(Infinity));
  AssertEquals('negative infinity', 'null', JsonFloat(NegInfinity));
end;

procedure TTestJsonWriter.TestFloatIgnoresLocale;
var
  OldDecimal, OldThousand: Char;
begin
  OldDecimal := SysUtils.FormatSettings.DecimalSeparator;
  OldThousand := SysUtils.FormatSettings.ThousandSeparator;
  try
    SysUtils.FormatSettings.DecimalSeparator := ',';
    SysUtils.FormatSettings.ThousandSeparator := '.';
    AssertEquals('decimal comma locale', '1234.5', JsonFloat(1234.5));
  finally
    SysUtils.FormatSettings.DecimalSeparator := OldDecimal;
    SysUtils.FormatSettings.ThousandSeparator := OldThousand;
  end;
end;

procedure TTestJsonWriter.TestEmptyObject;
var
  Json: TJsonWriter;
begin
  Json := TJsonWriter.Create;
  try
    Json.BeginObject;
    Json.EndObject;
    AssertEquals('empty root', '{}', Json.GetText);
  finally
    Json.Free;
  end;

  Json := TJsonWriter.Create;
  try
    Json.BeginObject;
    Json.BeginObject('inner');
    Json.EndObject;
    Json.EndObject;
    AssertEquals('empty inner', '{' + sLineBreak + '  "inner": {}' + sLineBreak + '}', Json.GetText);
  finally
    Json.Free;
  end;
end;

procedure TTestJsonWriter.TestDocument;
var
  Json: TJsonWriter;
  Expected: string;
begin
  Expected :=
    '{' + sLineBreak +
    '  "name": "page \"1\"",' + sLineBreak +
    '  "count": 3,' + sLineBreak +
    '  "big": 6451200000,' + sLineBreak +
    '  "detection": {' + sLineBreak +
    '    "skewAngle": -1.25,' + sLineBreak +
    '    "confidence": 0.5,' + sLineBreak +
    '    "reliable": true,' + sLineBreak +
    '    "contentRect": [10, 20, 300, 400],' + sLineBreak +
    '    "margins": [1.5, 2, 1.5, 2],' + sLineBreak +
    '    "stats": {' + sLineBreak +
    '      "rotated": false' + sLineBreak +
    '    }' + sLineBreak +
    '  },' + sLineBreak +
    '  "error": null' + sLineBreak +
    '}';

  Json := TJsonWriter.Create;
  try
    Json.BeginObject;
    Json.AddString('name', 'page "1"');
    Json.AddInt('count', 3);
    Json.AddInt('big', 6451200000);
    Json.BeginObject('detection');
    Json.AddFloat('skewAngle', -1.25);
    Json.AddFloat('confidence', 0.5004, 3);
    Json.AddBool('reliable', True);
    Json.AddRect('contentRect', Rect(10, 20, 300, 400));
    Json.AddFloatRect('margins', 1.5, 2, 1.5, 2);
    Json.BeginObject('stats');
    Json.AddBool('rotated', False);
    Json.EndObject;
    Json.EndObject;
    Json.AddNull('error');
    Json.EndObject;

    AssertEquals('document', Expected, Json.GetText);
  finally
    Json.Free;
  end;
end;

initialization
  RegisterTest(TTestUtils);
  RegisterTest(TTestJsonWriter);
end.
