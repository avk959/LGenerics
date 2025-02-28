unit LGJsonTest;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, fpcunit, testregistry, FileUtil,
  lgUtils,
  lgJson,
  lgList;

type

  { TTestJVariant }

  TTestJVariant = class(TTestCase)
  published
    procedure TestNull;
    procedure TestBool;
    procedure TestString;
    procedure TestNumber;
    procedure TestArray;
    procedure TestPairs;
    procedure TestAsJson;
    procedure TestEqual;
  end;

  { TTestDouble2Str }

  TTestDouble2Str = class(TTestCase)
  published
    procedure Basic;
    procedure Bounds;
    procedure Subnormal;
    procedure MinMax;
    procedure Regression;
    procedure LooksLikePow5;
    procedure OutputLength;
    procedure MinMaxShift;
  end;

  { TTestTryStr2Double }

  TTestTryStr2Double = class(TTestCase)
    procedure Basic;
    procedure Misc;
    procedure IbmFpgen;
    procedure IbmFpgen2;
    procedure Roundtrip;
  end;


  { TTestJson }

  TTestJson = class(TTestCase)
  private
    function CreateArrayOfObj: TJsonNode;
    function GetNodeCount(aNode: TJsonNode): SizeInt;
  published
    procedure Parser;
    procedure Validator;
    procedure Parse;
    procedure GetEnumerator;
    procedure SubTree;
    procedure Enrties;
    procedure Names;
    procedure IdenticNames;
    procedure JsonPointer;
    procedure TestTryAdd;
    procedure TestAddAllVar;
    procedure TestAddAll;
    procedure Values;
    procedure SkipBom;
    procedure Equal;
    procedure Clone;
    procedure TestSave2Stream;
  end;

  { TTestJsonWriter }

  TTestJsonWriter = class(TTestCase)
  private
  type
    TWriter    = specialize TGUniqRef<TJsonWriter>;
    TStrStream = specialize TGAutoRef<TStringStream>;
    TNode      = specialize TGUniqRef<TJsonNode>;
  published
    procedure AddNull;
    procedure AddFalse;
    procedure AddTrue;
    procedure AddNumber;
    procedure AddString;
    procedure AddJson;
    procedure AddNode;
    procedure WriteArray;
    procedure WriteObject;
    procedure Write;
  end;

  { TTestJsonReader }

  TTestJsonReader = class(TTestCase)
  private
  type
    TPair     = specialize TGMapEntry<string, string>;
    THashList = specialize TGLiteHashList2<string, TPair, string>;

    TIterObj = object
      List: THashList;
      function OnStruct(aReader: TJsonReader): Boolean;
      function OnValue(aReader: TJsonReader): Boolean;
      function CountIt(aReader: TJsonReader): Boolean;
    end;

  published
    procedure Read;
    procedure Read1;
    procedure ReadStr;
    procedure ReadStr1;
    procedure ReadNum;
    procedure ReadNum1;
    procedure ReadNull;
    procedure ReadNull1;
    procedure ReadTrue;
    procedure ReadFalse;
    procedure ReadArray;
    procedure ReadObject;
    procedure Skip;
    procedure Iterate;
    procedure Iterate1;
    procedure Iterate2;
    procedure IterateNest;
    procedure IterateNest1;
    procedure IterateNest2;
    procedure CopyStruct;
    procedure MoveNext;
    procedure Find;
    procedure FindPath;
    procedure FindPath1;
    procedure Path;
    procedure TestReader;
    procedure SkipBom;
    procedure SkipBom1;
  end;

  { TTestJsonPatch }

  TTestJsonPatch = class(TTestCase)
  private
  const
    sSource1 = '"bar"';
    sTarget1 = '"baz"';
    sSource2 = '{"foo": [1, 3, [2, "wat"]], "boo": ["bar", "baz"]}';
    sTarget2 = '{"foo": [1, 2, 3, [2, 3, "wat"], 9], "boo": ["bar", "ozz", "baz"], "zoo": "boom"}';
    sSource3 = '{"boo": ["bar", "baz", null]}';
    sTarget3 = '{"boo": []}';
    sPatch1  = '[{"op": "replace", "path": "", "value": "baz"}]';
    sPatch2  =
               '[{"op": "add", "path": "/foo/2/1", "value": 3,"fizz": 42}, ' +
               ' {"op": "add", "path": "/foo/1", "value": 2, "oops":"pop"},' +
               ' {"op": "add", "path": "/foo/4", "value": 9},              ' +
               ' {"op": "add", "path": "/boo/1", "value": "ozz"},          ' +
               ' {"op": "add", "path": "/zoo", "value": "boom"}]';
    sPatch3  = '[{"op": "add", "path": "", "value": "baz"}]';

    sBadPatch1 = '{"op": "add", "path": "", "value": "baz"}';
    sBadPatch2 = '[{"op": "pop", "path": "", "value": "baz"}]';
    sBadPatch3 = '[{"op": "add", "path": "", "value": "baz","op": "test"}]';
    sBadPatch4 = '[{"op": "copy", "path": "/boo/1", "from": "/foo/2/-"}]';
    sBadPatch5 = '[{"op": "move", "path": "/boo/1", "from": "/foo/2/-"}]';
  published
    procedure PatchBasic;
    procedure PatchBasic1;
    procedure PatchTests;
    procedure DiffBasic;
    procedure DiffTests;
    procedure DiffEmitTestOnRemove;
    procedure DiffEmitTestOnReplace;
    procedure DiffEnableMove;
  end;

const
  TestJson =
    '[{"userid":42,"name":"John","age":30,"online":false,"groups":["talk","humor","cook"],' +
    '"spouse":null},' +
    '{"userid":1001,"name":"Thomas","age":42,"online":true,"groups":["talk","games","math","art"],' +
    '"spouse":"Mary"}]';
  TestJson0 =
    '{"userid":42,"name":"John","age":30,"online":false,"groups":["talk","humor","cook"],"spouse":null}';
  TestJson1 =
    '{"userid":1001,"name":"Thomas","age":42,"online":true,"groups":["talk","games","math","art"],"spouse":"Mary"}';

  PathJson = '{"foo": ["bar", "baz"], "": 0,"a/b": 1,"c%d": 2,"e^f": 3, "g|h": 4,' +
             ' "i\\j": 5, "k\"l": 6, " ": 7, "m~n": 8}';

implementation

var
  TestFileList: TStringList = nil;
  TestPatchList: TStringList = nil;
  TestDir: string = '';
const
  ParseDir = 'parsing';
  PatchDir = 'patch';

procedure LoadFileLists;
var
  Dir: string;
begin
  Dir := ExcludeTrailingPathDelimiter(ProgramDirectory);
  while (Dir <> '') and (ExtractFileName(Dir) <> 'test') do
    Dir := ExcludeTrailingPathDelimiter(ExtractFilePath(Dir));
  if Dir = '' then exit;
  TestDir := Dir + DirectorySeparator + 'json_testset' + DirectorySeparator;
  Dir := TestDir + ParseDir + DirectorySeparator;
  if DirectoryExists(Dir) then
    TestFileList := FindAllFiles(Dir);
  Dir := TestDir + PatchDir + DirectorySeparator;
  if DirectoryExists(Dir) then
    TestPatchList := FindAllFiles(Dir);
end;

{ TTestJVariant }

procedure TTestJVariant.TestNull;
var
  v: TJVariant;
  Rased: Boolean = False;
begin
  AssertTrue({%H-}v.Kind = vkNull);
  AssertTrue(v.ToString = 'null');
  try
    v.AsString;
  except
    Rased := True;
  end;
  AssertTrue(Rased);
end;

procedure TTestJVariant.TestBool;
var
  v: TJVariant;
  Rased: Boolean = False;
begin
  v := False;
  AssertTrue(v.Kind = vkBool);
  AssertFalse(v.AsBoolean);
  AssertTrue(v.ToString = 'false');
  v := True;
  AssertTrue(v.Kind = vkBool);
  AssertTrue(v.AsBoolean);
  AssertTrue(v.ToString = 'true');
  try
    v.AsString;
  except
    Rased := True;
  end;
  AssertTrue(Rased);
end;

procedure TTestJVariant.TestString;
var
  v: TJVariant;
  Rased: Boolean = False;
begin
  v := 'string';
  AssertTrue(v.Kind = vkString);
  AssertTrue(v.AsString = 'string');
  try
    v.AsBoolean;
  except
    Rased := True;
  end;
  AssertTrue(Rased);
end;

procedure TTestJVariant.TestNumber;
var
  v: TJVariant;
  Rased: Boolean = False;
begin
  v := Double(1.141);
  AssertTrue(v.Kind = vkNumber);
  AssertTrue(v.AsNumber = Double(1.141));
  try
    v.AsBoolean;
  except
    Rased := True;
  end;
  AssertTrue(Rased);
end;

procedure TTestJVariant.TestArray;
var
  v: TJVariant;
  a: TJVarArray;
  Rased: Boolean = False;
begin
  v := [42, False];
  AssertTrue(v.Kind = vkArray);
  a := v;
  AssertTrue(Length(a) = 2);
  AssertTrue(a[0].Kind = vkNumber);
  AssertTrue(a[0] = 42);
  AssertTrue(a[1].Kind = vkBool);
  AssertFalse(a[1].AsBoolean);
  try
    v.AsPairs;
  except
    Rased := True;
  end;
  AssertTrue(Rased);

  v.Clear;
  v := [];
  AssertTrue(v.Kind = vkArray);
  AssertTrue(Length(v.AsArray) = 0);
end;

procedure TTestJVariant.TestPairs;
var
  v: TJVariant;
  a: TJPairArray;
  Rased: Boolean = False;
begin
  v := [JPair('a', JNull), JPair('b', 42), JPair('c', [False, 'bla'])];
  AssertTrue(v.Kind = vkPairs);
  a := v;
  AssertTrue(Length(a) = 3);
  AssertTrue(a[0].Key = 'a');
  AssertTrue(a[0].Value.Kind = vkNull);
  AssertTrue(a[1].Key = 'b');
  AssertTrue(a[1].Value.Kind = vkNumber);
  AssertTrue(a[1].Value.AsNumber = 42);
  AssertTrue(a[2].Key = 'c');
  AssertTrue(a[2].Value.Kind = vkArray);
  AssertTrue(Length(a[2].Value.AsArray) = 2);
  AssertFalse(a[2].Value.AsArray[0]);
  AssertTrue(a[2].Value.AsArray[1] = 'bla');
  try
    v.AsArray;
  except
    Rased := True;
  end;
  AssertTrue(Rased);

  v := EMPTY_PAIRS;
  AssertTrue(v.Kind = vkPairs);
  AssertTrue(Length(v.AsPairs) = 0);
end;

procedure TTestJVariant.TestAsJson;
var
  v: TJVariant;
begin
  v := JNull;
  AssertTrue(v.AsJson = 'null');

  v := False;
  AssertTrue(v.AsJson = 'false');

  v := True;
  AssertTrue(v.AsJson = 'true');

  v := 42;
  AssertTrue(v.AsJson = '42');

  v := 'bla';
  AssertTrue(v.AsJson = '"bla"');

  v := [];
  AssertTrue(v.AsJson = '[]');

  v := EMPTY_PAIRS;
  AssertTrue(v.AsJson = '{}');

  v := [JPair('a', JNull), JPair('b', 42), JPair('c', [False, 'bla'])];
  AssertTrue(v.AsJson = '{"a":null,"b":42,"c":[false,"bla"]}');
end;

procedure TTestJVariant.TestEqual;
var
  v1, v2: TJVariant;
begin
  AssertTrue(v1 = v2);
  v1 := True;
  AssertFalse(v1 = v2);
  v2 := False;
  AssertFalse(v1 = v2);
  v2 := True;
  AssertTrue(v1 = v2);
  v1 := 'str';
  AssertFalse(v1 = v2);
  v2 := 'str';
  AssertTrue(v1 = v2);
  v1 := 42;
  AssertFalse(v1 = v2);
  v2 := 42;
  AssertTrue(v1 = v2);
  v1.Clear;
  AssertFalse(v1 = v2);
  v2.Clear;
  AssertTrue(v1 = v2);
  v1 := [JPair('a', JNull), JPair('b', 42), JPair('c', [False, 'bla'])];
  v2 := [JPair('a', JNull), JPair('b', 42), JPair('c', [False, 'bla'])];
  AssertTrue(v1 = v2);
end;

{ TTestDouble2Str }

procedure TTestDouble2Str.Basic;
var
  d: Double;
begin
  AssertTrue(Double2Str(0.0) = '0');
  PQWord(@d)^ := QWord($8000000000000000);
  AssertTrue(Double2Str(d) = '-0');
  AssertTrue(Double2Str(Double.PositiveInfinity) = 'Infinity');
  AssertTrue(Double2Str(Double.NegativeInfinity) = '-Infinity');
  AssertTrue(Double2Str(Double.NaN) = 'NaN');
end;

procedure TTestDouble2Str.Bounds;
begin
  AssertTrue(Double2Str(1e0) = '1');
  AssertTrue(Double2Str(-1e0) = '-1');
  AssertTrue(Double2Str(1.0e7) = '10000000');
  AssertTrue(Double2Str(-1.0e7) = '-10000000');
  AssertTrue(Double2Str(9007199254740991) = '9007199254740991');
  AssertTrue(Double2Str(-9007199254740980) = '-9007199254740980');
  AssertTrue(Double2Str(9007199254740992) = '9007199254740992.0');
  AssertTrue(Double2Str(999999999999999.8) = '999999999999999.8');
  AssertTrue(Double2Str(999999999999999.8e1) = '9999999999999998.0');
  AssertTrue(Double2Str(9.999999999999998e-3) = '0.009999999999999998');
  AssertTrue(Double2Str(999999999999999.8e2) = '9.999999999999998E16');
  AssertTrue(Double2Str(9.9999999999e15) = '9999999999900000.0');
end;

procedure TTestDouble2Str.Subnormal;
begin
  AssertTrue(Double2Str(2.2250738585072014E-308) = '2.2250738585072014E-308');
  AssertTrue(Double2Str(4.9406564584e-312) = '4.9406564584E-312');
end;

procedure TTestDouble2Str.MinMax;
begin
  AssertTrue(Double2Str(5e-324) = '5E-324');
  AssertTrue(Double2Str(1.7976931348623157e308) = '1.7976931348623157E308');
end;

procedure TTestDouble2Str.Regression;
begin
  AssertTrue(Double2Str(-2.1098088986959632e16) = '-2.109808898695963E16');
  AssertTrue(Double2Str(4.940656e-318) = '4.940656E-318');
  AssertTrue(Double2Str(1.18575755e-316) = '1.18575755E-316');
  AssertTrue(Double2Str(2.989102097996e-312) = '2.989102097996E-312');
  AssertTrue(Double2Str(9.0608011534336e15) = '9060801153433600.0');
  AssertTrue(Double2Str(4.708356024711512e18) = '4.708356024711512E18');
  AssertTrue(Double2Str(9.409340012568248e18) = '9.409340012568248E18');
  AssertTrue(Double2Str(1.2345678e0) = '1.2345678');
  AssertTrue(Double2Str(1.9430376160308388e16) = '1.9430376160308388E16');
  AssertTrue(Double2Str(-6.9741824662760956e19) = '-6.9741824662760956E19');
  AssertTrue(Double2Str(4.3816050601147837e18) = '4.3816050601147837E18');
  AssertTrue(Double2Str(1.8531501765868567e21) = '1.8531501765868567E21');
  AssertTrue(Double2Str(-3.347727380279489e33) = '-3.347727380279489E33');
end;

procedure TTestDouble2Str.LooksLikePow5;
var
  d: Double;
begin
  PQWord(@d)^ := QWord($4830f0cf064dd592);
  AssertTrue(Double2Str(d) = '5.764607523034235E39');
  PQWord(@d)^ := QWord($4840f0cf064dd592);
  AssertTrue(Double2Str(d) = '1.152921504606847E40');
  PQWord(@d)^ := QWord($4850f0cf064dd592);
  AssertTrue(Double2Str(d) = '2.305843009213694E40');
end;

procedure TTestDouble2Str.OutputLength;
begin
  AssertTrue(Double2Str(1.2) = '1.2');
  AssertTrue(Double2Str(1.23) = '1.23');
  AssertTrue(Double2Str(1.234) = '1.234');
  AssertTrue(Double2Str(1.2345) = '1.2345');
  AssertTrue(Double2Str(1.23456) = '1.23456');
  AssertTrue(Double2Str(1.234567) = '1.234567');
  AssertTrue(Double2Str(1.23456789) = '1.23456789');
  AssertTrue(Double2Str(1.234567895) = '1.234567895');
  AssertTrue(Double2Str(1.2345678950) = '1.234567895');
  AssertTrue(Double2Str(1.2345678901) = '1.2345678901');
  AssertTrue(Double2Str(1.23456789012) = '1.23456789012');
  AssertTrue(Double2Str(1.234567890123) = '1.234567890123');
  AssertTrue(Double2Str(1.2345678901234) = '1.2345678901234');
  AssertTrue(Double2Str(1.23456789012345) = '1.23456789012345');
  AssertTrue(Double2Str(1.234567890123456) = '1.234567890123456');
  AssertTrue(Double2Str(1.2345678901234567) = '1.2345678901234567');

  AssertTrue(Double2Str(4.294967294) = '4.294967294');
  AssertTrue(Double2Str(4.294967295) = '4.294967295');
  AssertTrue(Double2Str(4.294967296) = '4.294967296');
  AssertTrue(Double2Str(4.294967297) = '4.294967297');
  AssertTrue(Double2Str(4.294967298) = '4.294967298');
end;

procedure TTestDouble2Str.MinMaxShift;
  function IEEEParts2Double(const aIsNeg: Boolean; const aExp, aMantis: QWord): Double;
  begin
    PQWord(@Result)^ := QWord(aIsNeg) shl 63 or aExp shl 52 or aMantis;
  end;
const
  MaxMantissa = Pred(QWord(1) shl 53);
begin
  // 32-bit opt-size=0:  49 <= dist <= 50
  // 32-bit opt-size=1:  30 <= dist <= 50
  // 64-bit opt-size=0:  50 <= dist <= 50
  // 64-bit opt-size=1:  30 <= dist <= 50
  AssertTrue(Double2Str(IEEEParts2Double(False, 4, 0)) = '1.7800590868057611E-307');
  // 32-bit opt-size=0:  49 <= dist <= 49
  // 32-bit opt-size=1:  28 <= dist <= 49
  // 64-bit opt-size=0:  50 <= dist <= 50
  // 64-bit opt-size=1:  28 <= dist <= 50
  AssertTrue(Double2Str(IEEEParts2Double(False, 6, MaxMantissa)) = '2.8480945388892175E-306');
  // 32-bit opt-size=0:  52 <= dist <= 53
  // 32-bit opt-size=1:   2 <= dist <= 53
  // 64-bit opt-size=0:  53 <= dist <= 53
  // 64-bit opt-size=1:   2 <= dist <= 53
  AssertTrue(Double2Str(IEEEParts2Double(False, 41, 0)) = '2.446494580089078E-296');
  // 32-bit opt-size=0:  52 <= dist <= 52
  // 32-bit opt-size=1:   2 <= dist <= 52
  // 64-bit opt-size=0:  53 <= dist <= 53
  // 64-bit opt-size=1:   2 <= dist <= 53
  AssertTrue(Double2Str(IEEEParts2Double(False, 40, MaxMantissa)) = '4.8929891601781557E-296');
  // 32-bit opt-size=0:  57 <= dist <= 58
  // 32-bit opt-size=1:  57 <= dist <= 58
  // 64-bit opt-size=0:  58 <= dist <= 58
  // 64-bit opt-size=1:  58 <= dist <= 58
  AssertTrue(Double2Str(IEEEParts2Double(False, 1077, 0)) = '1.8014398509481984E16');
  // 32-bit opt-size=0:  57 <= dist <= 57
  // 32-bit opt-size=1:  57 <= dist <= 57
  // 64-bit opt-size=0:  58 <= dist <= 58
  // 64-bit opt-size=1:  58 <= dist <= 58
  AssertTrue(Double2Str(IEEEParts2Double(False, 1076, MaxMantissa)) = '3.6028797018963964E16');
  // 32-bit opt-size=0:  51 <= dist <= 52
  // 32-bit opt-size=1:  51 <= dist <= 59
  // 64-bit opt-size=0:  52 <= dist <= 52
  // 64-bit opt-size=1:  52 <= dist <= 59
  AssertTrue(Double2Str(IEEEParts2Double(False, 307, 0)) = '2.900835519859558E-216');
  // 32-bit opt-size=0:  51 <= dist <= 51
  // 32-bit opt-size=1:  51 <= dist <= 59
  // 64-bit opt-size=0:  52 <= dist <= 52
  // 64-bit opt-size=1:  52 <= dist <= 59
  AssertTrue(Double2Str(IEEEParts2Double(False, 306, MaxMantissa)) = '5.801671039719115E-216');
  // 32-bit opt-size=0:  49 <= dist <= 49
  // 32-bit opt-size=1:  44 <= dist <= 49
  // 64-bit opt-size=0:  50 <= dist <= 50
  // 64-bit opt-size=1:  44 <= dist <= 50
  AssertTrue(Double2Str(IEEEParts2Double(False, 934 ,QWord($000FA7161A4D6E0C))) = '3.196104012172126E-27');
end;

{ TTestTryStr2Double }

procedure TTestTryStr2Double.Basic;
var
  d: Double;
  q: QWord absolute d;
begin
  AssertFalse(TryStr2Double('Nan', d));
  AssertFalse(TryStr2Double('Infinity', d));
  AssertFalse(TryStr2Double('-Infinity', d));
  AssertFalse(TryStr2Double('-', d));
  AssertFalse(TryStr2Double('+', d));
  AssertFalse(TryStr2Double('.', d));
  AssertFalse(TryStr2Double('0.', d));
  AssertFalse(TryStr2Double('.0', d));
  AssertFalse(TryStr2Double('-02.0', d));
  AssertFalse(TryStr2Double('+2.0', d));
  AssertFalse(TryStr2Double(' -2.0', d));
  AssertFalse(TryStr2Double('-2.0 ', d));
  AssertFalse(TryStr2Double('2.0e', d));
  AssertFalse(TryStr2Double('.e', d));
  AssertFalse(TryStr2Double('2.0e-', d));

  AssertTrue(TryStr2Double('0', d));
  AssertTrue(d = 0);

  AssertTrue(TryStr2Double('-0', d));
  AssertTrue(q.ToHexString(16), q and (QWord(1) shl 63) <> 0);
  AssertTrue(q and Pred(QWord(1) shl 63) = 0);

  AssertTrue(TryStr2Double('-0.0', d));
  AssertTrue(q.ToHexString(16), q and (QWord(1) shl 63) <> 0);
  AssertTrue(q and Pred(QWord(1) shl 63) = 0);

  AssertTrue(TryStr2Double('0e+42949672970', d));
  AssertTrue(d = 0);

  AssertTrue(TryStr2Double('5e0016', d));
  AssertTrue(Double2Str(d) = '5E16');
end;

procedure TTestTryStr2Double.Misc;
var
  d: Double;
begin
  AssertTrue(TryStr2Double('1.421085474167199e-14', d));
  AssertTrue(Double2Str(d) = '1.421085474167199E-14');

  AssertTrue(TryStr2Double('1421085474167e7', d));
  AssertTrue(Double2Str(d), Double2Str(d) = '1.421085474167E19');

end;

procedure TTestTryStr2Double.IbmFpgen;
var
  Lines: specialize TGAutoRef<TStringList>;

  function Test(const s: string): Boolean;
  var
    d: Double;
    q: QWord absolute d;
    v: QWord;
    I: Integer;
    HexPart, FloatPart: string;
  begin
    Result := False;
    try
      HexPart := '$' + Copy(s, 15, 16);
      FloatPart := Copy(s, 32, Length(s) - 31);
      Val(HexPart, v, I);
      if I <> 0 then exit;
      if not TryStr2Double(FloatPart, d) then exit;
      Result := q = v;
    except
    end;
  end;
var
  ErrCount: Integer = 0;
  s, TestFile: string;
begin
  TestFile := TestDir + 'ibm-fpgen.txt';
  AssertTrue('Test file not found', FileExists(TestFile));
  Lines.Instance.LoadFromFile(TestFile);
  AssertTrue(Lines.Instance.Count = 102792);
  for s in Lines.Instance do
    Inc(ErrCount, Ord(not Test(s)));
  AssertTrue('Total errors: ' + ErrCount.ToString, ErrCount = 0);
end;

procedure TTestTryStr2Double.IbmFpgen2;
var
  Lines: specialize TGAutoRef<TStringList>;

  function TryVal(const s: string; out v: Double): Boolean;
  var
    c: Integer;
  begin
    Val(s, v, c);
    Result := c = 0;
  end;

  function Test(const s: string): Boolean;
  var
    d: Double;
    q: QWord absolute d;
    v: QWord;
    I: Integer;
    HexPart, FloatPart: string;
  begin
    Result := False;
    try
      HexPart := '$' + Copy(s, 15, 16);
      FloatPart := Copy(s, 32, Length(s) - 31);
      Val(HexPart, v, I);
      if I <> 0 then exit;
      if not TryVal(FloatPart, d) then exit;
      Result := q = v;
    except
    end;
  end;
var
  ErrCount: Integer = 0;
  s, TestFile: string;
begin
  TestFile := TestDir + 'ibm-fpgen.txt';
  AssertTrue('Test file not found', FileExists(TestFile));
  Lines.Instance.LoadFromFile(TestFile);
  AssertTrue(Lines.Instance.Count = 102792);
  for s in Lines.Instance do
    if not Test(s) then
      Inc(ErrCount);
  AssertTrue('Total errors: ' + ErrCount.ToString, ErrCount = 0);
end;

function  NextRandomDouble: Double;
var
  q: QWord;
  d: Double absolute q;
const
  InfExp = QWord($7ff0000000000000);
begin
  repeat
    q := BJNextRandom64;
  until (q and InfExp <> InfExp) and (q and InfExp <> 0);
  Result := d;
end;

procedure TTestTryStr2Double.Roundtrip;
var
  d, d1: Double;
  s: string;
  I: Integer;
const
  TestSize = 1000000;
begin
  BJRandomize64;
  for I := 1 to TestSize do
    begin
      d := NextRandomDouble;
      s := Double2Str(d);
      AssertTrue(TryStr2Double(s, d1));
      AssertTrue(d = d1);
    end;
end;

{ TTestJson }

function TTestJson.CreateArrayOfObj: TJsonNode;
var
  Node, o: TJsonNode;
  I, J, K: Integer;
  n: string;
begin
  Node := TJsonNode.Create;
  for I := 1 to 10 do
    begin
      o := Node.AddNode(jvkObject);
      for J := 1 to 50 do
        begin
          K := J mod 6;
          n := 'name ' + K.ToString;
          case K of
           0: o.Add(n, J);
           1: o.Add(n, Odd(J));
           2: o.Add(n, J.ToString);
           3: o.AddNull(n);
           4: o.Add(n, [K, Odd(J), 'name', JNull]);
           5: o.Add(n, [JPair('number', J), JPair('known', False), JPair('place', 'none')]);
          end;
        end;
    end;
  Result := Node;
end;

function TTestJson.GetNodeCount(aNode: TJsonNode): SizeInt;
var
  Count: SizeInt = 0;
  procedure DoCount(aNode: TJsonNode);
  var
    I: SizeInt;
  begin
    Inc(Count);
    if aNode.IsStruct then
      for I := 0 to Pred(aNode.Count) do
        DoCount(aNode.Items[I]);
  end;
begin
  DoCount(aNode);
  Result := Count;
end;

procedure TTestJson.Parser;
var
  Node: specialize TGAutoRef<TJsonNode>;
  Stream: specialize TGAutoRef<TStringStream>;
  CurrFile, fn: string;
  Result: Boolean;
  c: AnsiChar;
  Total: Integer = 0;
begin
  AssertTrue('File list not loaded', Assigned(TestFileList));
  for CurrFile in TestFileList do
    begin
      Stream.Instance.LoadFromFile(CurrFile);
      Stream.Instance.Position := 0;
      Result := Node.Instance.TryParse(Stream.Instance.DataString);
      fn := ExtractFileName(CurrFile);
      c := fn[1];
      Inc(Total);
      if c = 'y' then
        AssertTrue(fn + ': expected True, but got False', Result)
      else
        if c = 'n' then
          AssertFalse(fn + ': expected False, but got True', Result);
    end;
  AssertTrue(Total = 295);
end;

procedure TTestJson.Validator;
var
  Stream: specialize TGAutoRef<TStringStream>;
  CurrFile, fn: string;
  Result: Boolean;
  c: AnsiChar;
  Total: Integer = 0;
begin
  AssertTrue('File list not loaded', Assigned(TestFileList));
  for CurrFile in TestFileList do
    begin
      Stream.Instance.LoadFromFile(CurrFile);
      Stream.Instance.Position := 0;
      Result := TJsonNode.ValidJson(Stream.Instance.DataString);
      fn := ExtractFileName(CurrFile);
      c := fn[1];
      Inc(Total);
      if c = 'y' then
        AssertTrue(fn + ': expected True, but got False', Result)
      else
        if c = 'n' then
          AssertFalse(fn + ': expected False, but got True', Result);
    end;
  AssertTrue(Total = 295);
end;

procedure TTestJson.Parse;
var
  o: specialize TGAutoRef<TJsonNode>;
begin
  AssertTrue(o.Instance.TryParse(TestJson));
  AssertTrue(o.Instance.AsJson = TestJson);
  AssertTrue(o.Instance.IsArray);
  AssertTrue(o.Instance.Count = 2);
  AssertTrue(o.Instance.Items[0].IsObject);
  AssertTrue(o.Instance.Items[0].Count = 6);
  AssertTrue(o.Instance.Items[0].Contains('online'));
  AssertFalse(o.Instance.Items[0]['online'].Value);
  AssertTrue(o.Instance.Items[0]['age'].Value = 30);
  AssertTrue(o.Instance.Items[0].Items[4].Count = 3);
  AssertTrue(o.Instance.Items[1].IsObject);
  AssertTrue(o.Instance.Items[1].Contains('online'));
  AssertTrue(o.Instance.Items[1]['online'].Value);
  AssertTrue(o.Instance.Items[1]['age'].Value = 42);
  AssertTrue(o.Instance.Items[1].Count = 6);
  AssertTrue(o.Instance.Items[1].Items[4].Count = 4);
end;

procedure TTestJson.GetEnumerator;
var
  o: specialize TGAutoRef<TJsonNode>;
  Node, Nest: TJsonNode;
  I, J: Integer;
begin
  {%H-}o.Instance := CreateArrayOfObj;
  I := 0;
  for Node in o.Instance do
    begin
      AssertTrue(Node.IsObject);
      Inc(I);
    end;
  AssertTrue(I = 10);
  for I := 0 to Pred(o.Instance.Count) do
    begin
      Node := o.Instance.Items[I];
      J := 0;
      for Nest in Node do
        Inc(J);
      AssertTrue(J = 50);
    end;
end;

procedure TTestJson.SubTree;
var
  o: specialize TGAutoRef<TJsonNode>;
  Node: TJsonNode;
  I: Integer;
begin
  {%H-}o.Instance := CreateArrayOfObj;
  I := 1;
  for Node in o.Instance.SubTree do
    Inc(I);
  AssertTrue(I = GetNodeCount(o.Instance));
end;

procedure TTestJson.Enrties;
var
  o: specialize TGAutoRef<TJsonNode>;
  Node: TJsonNode;
  e: TJsonNode.TPair;
  I, J: Integer;
begin
  {%H-}o.Instance := CreateArrayOfObj;
  for I := 0 to Pred(o.Instance.Count) do
    begin
      Node := o.Instance.Items[I];
      J := 0;
      for e in Node.Entries do
        begin
          AssertTrue(Node.Contains(e.Key));
          Inc(J);
        end;
      AssertTrue(J = 50);
    end;
end;

procedure TTestJson.Names;
var
  o: specialize TGAutoRef<TJsonNode>;
  Node: TJsonNode;
  Name: string;
  I, J: Integer;
begin
  {%H-}o.Instance := CreateArrayOfObj;
  for I := 0 to Pred(o.Instance.Count) do
    begin
      Node := o.Instance.Items[I];
      J := 0;
      for Name in Node.Names do
        begin
          AssertTrue(Node.Contains(Name));
          Inc(J);
        end;
      AssertTrue(J = 50);
    end;
end;

procedure TTestJson.IdenticNames;
var
  o: specialize TGAutoRef<TJsonNode>;
  Node: TJsonNode;
  e, ie: TJsonNode.TPair;
  I, J: Integer;
begin
  {%H-}o.Instance := CreateArrayOfObj;
  for I := 0 to Pred(o.Instance.Count) do
    begin
      Node := o.Instance.Items[I];
      for e in Node.Entries do
        begin
          J := 0;
          for ie in Node.EqualNames(e.Key) do
            Inc(J);
          AssertTrue(Node.CountOfName(e.Key) = J);
        end;
    end;
end;

procedure TTestJson.JsonPointer;
var
  o: specialize TGAutoRef<TJsonNode>;
  Node: TJsonNode;
begin
  AssertTrue(o.Instance.TryParse(PathJson));
  AssertTrue(o.Instance.FindPath(TJsonPtr.From(''), Node));
  AssertTrue(Node = o.Instance);
  AssertTrue(o.Instance.FindPath(TJsonPtr.From('/foo'), Node));
  AssertTrue(Node.IsArray);
  AssertTrue(Node.Count = 2);
  AssertTrue(Node.Items[0].AsString = 'bar');
  AssertTrue(Node.Items[1].AsString = 'baz');
  AssertTrue(o.Instance.FindPath(TJsonPtr.From('/foo/0'), Node));
  AssertTrue(Node.IsString);
  AssertTrue(Node.AsString = 'bar');
  AssertTrue(o.Instance.FindPath(TJsonPtr.From('/'), Node));
  AssertTrue(Node.IsNumber);
  AssertTrue(Node.AsNumber = 0);
  AssertTrue(o.Instance.FindPath(TJsonPtr.From(['a/b']), Node));
  AssertTrue(Node.IsNumber);
  AssertTrue(Node.AsNumber = 1);
  AssertTrue(o.Instance.FindPath(TJsonPtr.From('/c%d'), Node));
  AssertTrue(Node.AsNumber = 2);
  AssertTrue(o.Instance.FindPath(TJsonPtr.From('/e^f'), Node));
  AssertTrue(Node.AsNumber = 3);
  AssertTrue(o.Instance.FindPath(TJsonPtr.From('/g|h'), Node));
  AssertTrue(Node.AsNumber = 4);
  AssertTrue(o.Instance.FindPath(TJsonPtr.From('/i\j'), Node));
  AssertTrue(Node.AsNumber = 5);
  AssertTrue(o.Instance.FindPath(TJsonPtr.From('/k"l'), Node));
  AssertTrue(Node.AsNumber = 6);
  AssertTrue(o.Instance.FindPath(TJsonPtr.From('/ '), Node));
  AssertTrue(Node.AsNumber = 7);
  AssertTrue(o.Instance.FindPath(TJsonPtr.From(['m~n']), Node));
  AssertTrue(Node.AsNumber = 8);
end;

procedure TTestJson.TestTryAdd;
var
  o: specialize TGAutoRef<TJsonNode>;
  Node: TJsonNode;
begin
  AssertTrue(o.Instance.TryAdd('key', False));
  AssertTrue(o.Instance.Find('key', Node));
  AssertTrue(o.Instance.Count = 1);
  AssertFalse(o.Instance.TryAdd('key', 'data'));
  AssertTrue(o.Instance.Count = 1);
  AssertTrue(o.Instance.TryAdd('new key', 42));
  AssertTrue(o.Instance.Count = 2);
  AssertTrue(o.Instance.Find('new key', Node));
end;

procedure TTestJson.TestAddAllVar;
var
  o: specialize TGAutoRef<TJsonNode>;
begin
  TJsonNode(o).Add(1);
  TJsonNode(o).Add(2);
  TJsonNode(o).AddAll([3,4,5]);
  AssertTrue(TJsonNode(o).IsArray);
  AssertTrue(TJsonNode(o).Count = 5);
  AssertTrue(TJsonNode(o).Items[0].Value = 1);
  AssertTrue(TJsonNode(o).Items[1].Value = 2);
  AssertTrue(TJsonNode(o).Items[2].Value = 3);
  AssertTrue(TJsonNode(o).Items[3].Value = 4);
  AssertTrue(TJsonNode(o).Items[4].Value = 5);
end;

procedure TTestJson.TestAddAll;
var
  o1, o2: specialize TGAutoRef<TJsonNode>;
begin
  TJsonNode(o1).AsBoolean := True;
  TJsonNode(o1).AddAll(TJsonNode(o2));
  AssertTrue(TJsonNode(o1).IsBoolean);

  TJsonNode(o1).AddAll([1,2,3]);
  TJsonNode(o2).AddAll([4,5,6]);
  TJsonNode(o1).AddAll(o2.Instance);
  AssertTrue(TJsonNode(o1).IsArray);
  AssertTrue(TJsonNode(o1).Count = 6);
  AssertTrue(TJsonNode(o2).Count = 3);

  TJsonNode(o1).AddAll(o2.Instance, False);
  AssertTrue(TJsonNode(o1).Count = 9);
  AssertTrue(TJsonNode(o2).Count = 0);
end;

procedure TTestJson.Values;
var
  o: specialize TGAutoRef<TJsonNode>;
  User: TJsonNode;
  v: TJVariant;
begin
  User := o.Instance.AddNode(jvkObject);
  User['firstName'].Value := 'John';
  User['lastName'].Value := 'Smith';
  User['age'].Value := 42;
  User['married'].Value := False;
  User['spouse'].Value := JNull;
  AssertTrue(o.Instance.Count = 1);
  User := o.Instance.Items[0];
  AssertTrue(User.IsObject);
  AssertTrue(User['firstName'].Value = 'John');
  AssertTrue(User['lastName'].Value = 'Smith');
  AssertTrue(User['age'].Value = 42);
  AssertFalse(User['married'].Value);
  AssertTrue(User['spouse'].Value.Kind = vkNull);

  TJsonNode(o).Value := [
    42,
    [
      JPair('a',False)
    ],
    [
      JPair('b',True),
      JPair('c',JNull)
    ],
    'bla'
  ];
  AssertTrue(TJsonNode(o).IsArray);
  v := [42,[JPair('a',False)],[JPair('b',True),JPair('c',JNull)],'bla'];
  AssertTrue(TJsonNode(o).Value = v);
  AssertTrue(TJsonNode(o).AsJson = '[42,{"a":false},{"b":true,"c":null},"bla"]');
end;

procedure TTestJson.SkipBom;
var
  o: TJsonNode;
  s: string;
  Stream: specialize TGAutoRef<TStringStream>;
begin
  s := #$EF#$BB#$BF + TestJson;
  AssertTrue(TJsonNode.ValidJson(s, True));
  {%H-}Stream.Instance := TStringStream.Create(s);
  Stream.Instance.Position := 0;
  AssertTrue(TJsonNode.ValidJson(Stream.Instance, True));
  AssertTrue(TJsonNode.TryParse(s, o, True));
  o.Free;
  Stream.Instance.Position := 0;
  AssertTrue(TJsonNode.TryParse(Stream.Instance, o, True));
  o.Free;
end;

procedure TTestJson.Equal;
var
  o1, o2: specialize TGAutoRef<TJsonNode>;
  User: TJsonNode;
  n1: TJsonNode = nil;
  n2: TJsonNode = nil;
begin
  AssertTrue(n1.EqualTo(n2));
  AssertTrue(n2.EqualTo(n1));
  AssertTrue(o1.Instance.EqualTo(o1.Instance));
  AssertTrue(o1.Instance.EqualTo(o2.Instance));
  AssertTrue(o1.Instance.TryParse(TestJson));
  AssertTrue(o2.Instance.TryParse(TestJson));
  AssertTrue(o1.Instance.EqualTo(o2.Instance));
  User := o2.Instance.Items[1];
  User['spouse'].Value := 'Kate';
  AssertFalse(o1.Instance.EqualTo(o2.Instance));
  User['spouse'].Value := 'Mary';
  AssertTrue(o1.Instance.EqualTo(o2.Instance));
end;

procedure TTestJson.Clone;
var
  o1, o2: specialize TGAutoRef<TJsonNode>;
begin
  AssertTrue(o1.Instance.TryParse(TestJson));
  {%H-}o2.Instance := o1.Instance.Clone;
  AssertTrue(o1.Instance <> o2.Instance);
  AssertTrue(o1.Instance.EqualTo(o2.Instance));
end;

procedure TTestJson.TestSave2Stream;
var
  o1, o2: specialize TGAutoRef<TJsonNode>;
  Stream: specialize TGAutoRef<TStringStream>;
begin
  o1.Instance.TryParse(TestJson);
  AssertTrue(o1.Instance.SaveToStream(Stream.Instance) = Length(TestJson));
  AssertTrue(o2.Instance.TryParse(Stream.Instance.DataString));
  AssertTrue(o2.Instance.EqualTo(o1.Instance));
end;

{ TTestJsonWriter }

procedure TTestJsonWriter.AddNull;
  procedure WriteNull(aStream: TStream);
  var
    Writer: TWriter;
  begin
    {%H-}Writer.Instance := TJsonWriter.Create(aStream);
    Writer.Instance.AddNull;
  end;
var
  Stream: TStrStream;
  s: string;
begin
  WriteNull(Stream.Instance);
  s := Stream.Instance.DataString;
  AssertTrue(TJsonNode.ValidJson(s));
  AssertTrue(TJsonNode.LikelyKind(Pointer(s), Length(s)) = jvkNull);
  AssertTrue(s = 'null');
end;

procedure TTestJsonWriter.AddFalse;
  procedure WriteFalse(aStream: TStream);
  var
    Writer: TWriter;
  begin
    {%H-}Writer.Instance := TJsonWriter.Create(aStream);
    Writer.Instance.AddFalse;
  end;
var
  Stream: TStrStream;
  s: string;
begin
  WriteFalse(Stream.Instance);
  s := Stream.Instance.DataString;
  AssertTrue(TJsonNode.ValidJson(s));
  AssertTrue(TJsonNode.LikelyKind(Pointer(s), Length(s)) = jvkFalse);
  AssertTrue(s = 'false');
end;

procedure TTestJsonWriter.AddTrue;
  procedure WriteTrue(aStream: TStream);
  var
    Writer: TWriter;
  begin
    {%H-}Writer.Instance := TJsonWriter.Create(aStream);
    Writer.Instance.AddTrue;
  end;
var
  Stream: TStrStream;
  s: string;
begin
  WriteTrue(Stream.Instance);
  s := Stream.Instance.DataString;
  AssertTrue(TJsonNode.ValidJson(s));
  AssertTrue(TJsonNode.LikelyKind(Pointer(s), Length(s)) = jvkTrue);
  AssertTrue(s = 'true');
end;

procedure TTestJsonWriter.AddNumber;
const
  Num = -42;
  procedure WriteNum(aStream: TStream);
  var
    Writer: TWriter;
  begin
    {%H-}Writer.Instance := TJsonWriter.Create(aStream);
    Writer.Instance.Add(Num);
  end;
var
  Stream: TStrStream;
  s: string;
begin
  WriteNum(Stream.Instance);
  s := Stream.Instance.DataString;
  AssertTrue(TJsonNode.ValidJson(s));
  AssertTrue(TJsonNode.LikelyKind(Pointer(s), Length(s)) = jvkNumber);
  AssertTrue(s = '-42');
end;

procedure TTestJsonWriter.AddString;
const
  Str = 'string "value"';
  procedure WriteStr(aStream: TStream);
  var
    Writer: TWriter;
  begin
    {%H-}Writer.Instance := TJsonWriter.Create(aStream);
    Writer.Instance.Add(Str);
  end;
var
  Stream: TStrStream;
  s: string;
begin
  WriteStr(Stream.Instance);
  s := Stream.Instance.DataString;
  AssertTrue(TJsonNode.ValidJson(s));
  AssertTrue(TJsonNode.LikelyKind(Pointer(s), Length(s)) = jvkString);
  AssertTrue(s = '"string \"value\""');
end;

procedure TTestJsonWriter.AddJson;
const
  Json = '{"items":["one",42,false],"value":false}';
  procedure WriteJson(aStream: TStream);
  var
    Writer: TWriter;
  begin
    {%H-}Writer.Instance := TJsonWriter.Create(aStream);
    Writer.Instance.AddJson(Json);
  end;
var
  Stream: TStrStream;
  s: string;
begin
  WriteJson(Stream.Instance);
  s := Stream.Instance.DataString;
  AssertTrue(TJsonNode.ValidJson(s));
  AssertTrue(TJsonNode.LikelyKind(Pointer(s), Length(s)) = jvkObject);
  AssertTrue(s = Json);
end;

procedure TTestJsonWriter.AddNode;
var
  Node: TNode;
  procedure WriteNode(aStream: TStream);
  var
    Writer: TWriter;
  begin
    {%H-}Writer.Instance := TJsonWriter.Create(aStream);
    Writer.Instance.Add(Node.Instance);
  end;
var
  Stream: TStrStream;
  s: string;
const
  Json = '{"items":["one",42,false],"value":false}';
begin
  {%H-}Node.Instance := TJsonNode.NewJson(Json);
  WriteNode(Stream.Instance);
  s := Stream.Instance.DataString;
  AssertTrue(TJsonNode.ValidJson(s));
  AssertTrue(TJsonNode.LikelyKind(Pointer(s), Length(s)) = jvkObject);
  AssertTrue(s = Json);
end;

procedure TTestJsonWriter.WriteArray;
  procedure WriteArr(aStream: TStream);
  var
    Writer: TWriter;
  begin
    {%H-}Writer.Instance := TJsonWriter.Create(aStream);
    Writer.Instance
      .BeginArray
      .EndArray;
  end;
var
  Stream: TStrStream;
  s: string;
begin
  WriteArr(Stream.Instance);
  s := Stream.Instance.DataString;
  AssertTrue(TJsonNode.ValidJson(s));
  AssertTrue(TJsonNode.LikelyKind(Pointer(s), Length(s)) = jvkArray);
  AssertTrue(s = '[]');
end;

procedure TTestJsonWriter.WriteObject;
  procedure WriteObj(aStream: TStream);
  var
    Writer: TWriter;
  begin
    {%H-}Writer.Instance := TJsonWriter.Create(aStream);
    Writer.Instance
      .BeginObject
      .EndObject;
  end;
var
  Stream: TStrStream;
  s: string;
begin
  WriteObj(Stream.Instance);
  s := Stream.Instance.DataString;
  AssertTrue(TJsonNode.ValidJson(s));
  AssertTrue(TJsonNode.LikelyKind(Pointer(s), Length(s)) = jvkObject);
  AssertTrue(s = '{}');
end;

procedure TTestJsonWriter.Write;
  procedure WriteObj(aStream: TStream);
  var
    Writer: TWriter;
  begin
    {%H-}Writer.Instance := TJsonWriter.Create(aStream);
    Writer.Instance
      .BeginObject
        .AddNull('item1')
        .AddTrue('item2')
        .AddName('item3')
          .BeginArray
            .Add('value')
            .Add(42)
            .AddFalse
              .BeginArray
                .Add('value1')
                .Add(42)
                .AddNull
             .EndArray
          .EndArray
        .AddJson('item4', '["value2", 1001, null]')
      .EndObject;
  end;
var
  Stream: TStrStream;
  s: string;
const
  Json = '{"item1":null,"item2":true,"item3":["value",42,false,["value1",42,null]],"item4":["value2", 1001, null]}';
begin
  WriteObj(Stream.Instance);
  s := Stream.Instance.DataString;
  AssertTrue(TJsonNode.ValidJson(s));
  AssertTrue(TJsonNode.LikelyKind(Pointer(s), Length(s)) = jvkObject);
  AssertTrue(s = Json);
end;

{ TTestJsonReader.TIterObj }

function TTestJsonReader.TIterObj.OnStruct(aReader: TJsonReader): Boolean;
begin
  List.Add(TPair.Create(aReader.ParentName, ''));
  Result := True;
end;

function TTestJsonReader.TIterObj.OnValue(aReader: TJsonReader): Boolean;
begin
  Result := True;
end;

function TTestJsonReader.TIterObj.CountIt(aReader: TJsonReader): Boolean;
begin
  List.Add(TPair.Create(aReader.Name, aReader.ValueToString));
  Result := True;
end;

{ TTestJsonReader }

procedure TTestJsonReader.Read;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
begin
  {%H-}Stream.Instance := TStringStream.Create('');
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = rsError);
  Stream.Instance := TStringStream.Create('" "');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkString);
  AssertTrue(Reader.Instance.AsString = ' ');
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
end;

procedure TTestJsonReader.Read1;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  s: string;
begin
  s := '';
  {%H-}Reader.Instance := TJsonReader.Create(Pointer(s), Length(s));
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = rsError);
  s := '" "';
  Reader.Instance := TJsonReader.Create(Pointer(s), Length(s));
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkString);
  AssertTrue(Reader.Instance.AsString = ' ');
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
end;

procedure TTestJsonReader.ReadStr;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
begin
  {%H-}Stream.Instance := TStringStream.Create(#13'""'#9#10);
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkString);
  AssertTrue(Reader.Instance.AsString = '');
  Stream.Instance := TStringStream.Create('"\"\"\"\""');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkString);
  AssertTrue(Reader.Instance.AsString = '""""');
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
end;

procedure TTestJsonReader.ReadStr1;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  s: string;
begin
   s := #13'""'#9#10;
  {%H-}Reader.Instance := TJsonReader.Create(Pointer(s), Length(s));
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkString);
  AssertTrue(Reader.Instance.AsString = '');
  s := '"\"\"\"\""';
  Reader.Instance := TJsonReader.Create(Pointer(s), Length(s));
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkString);
  AssertTrue(Reader.Instance.AsString = '""""');
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
end;

procedure TTestJsonReader.ReadNum;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
begin
  {%H-}Stream.Instance := TStringStream.Create('01');
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertFalse(Reader.Instance.Read);
  Stream.Instance := TStringStream.Create('42');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkNumber);
  AssertTrue(Reader.Instance.AsNumber = 42);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
end;

procedure TTestJsonReader.ReadNum1;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  s: string;
begin
  s := '01';
  {%H-}Reader.Instance := TJsonReader.Create(Pointer(s), Length(s));
  AssertFalse(Reader.Instance.Read);
  s := '42';
  Reader.Instance := TJsonReader.Create(Pointer(s), Length(s));
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkNumber);
  AssertTrue(Reader.Instance.AsNumber = 42);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
end;

procedure TTestJsonReader.ReadNull;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
begin
  {%H-}Stream.Instance := TStringStream.Create('Null');
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertFalse(Reader.Instance.Read);
  Stream.Instance := TStringStream.Create('null');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkNull);
  AssertTrue(Reader.Instance.IsNull);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
end;

procedure TTestJsonReader.ReadNull1;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  s: string;
begin
  s := 'Null';
  {%H-}Reader.Instance := TJsonReader.Create(Pointer(s), Length(s));
  AssertFalse(Reader.Instance.Read);
  s := 'null';
  Reader.Instance := TJsonReader.Create(Pointer(s), Length(s));
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkNull);
  AssertTrue(Reader.Instance.IsNull);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
end;

procedure TTestJsonReader.ReadTrue;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
begin
  {%H-}Stream.Instance := TStringStream.Create('True');
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertFalse(Reader.Instance.Read);
  Stream.Instance := TStringStream.Create('true');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkTrue);
  AssertTrue(Reader.Instance.AsBoolean);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
end;

procedure TTestJsonReader.ReadFalse;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
begin
  {%H-}Stream.Instance := TStringStream.Create('False');
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertFalse(Reader.Instance.Read);
  Stream.Instance := TStringStream.Create('false');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkFalse);
  AssertFalse(Reader.Instance.AsBoolean);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
end;

procedure TTestJsonReader.ReadArray;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
begin
  {%H-}Stream.Instance := TStringStream.Create('[]');
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkArrayBegin);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkArrayEnd);
  Stream.Instance := TStringStream.Create('[null, "data", 42]');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkArrayBegin);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkNull);
  AssertTrue(Reader.Instance.IsNull);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkString);
  AssertTrue(Reader.Instance.AsString = 'data');
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkNumber);
  AssertTrue(Reader.Instance.AsNumber = 42);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkArrayEnd);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
end;

procedure TTestJsonReader.ReadObject;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
begin
  {%H-}Stream.Instance := TStringStream.Create('{}');
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkObjectBegin);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkObjectEnd);
  Stream.Instance := TStringStream.Create('{"what": null, "which": "data", "value": 42}');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkObjectBegin);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkNull);
  AssertTrue(Reader.Instance.Name = 'what');
  AssertTrue(Reader.Instance.IsNull);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkString);
  AssertTrue(Reader.Instance.Name = 'which');
  AssertTrue(Reader.Instance.AsString = 'data');
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkNumber);
  AssertTrue(Reader.Instance.Name = 'value');
  AssertTrue(Reader.Instance.AsNumber = 42);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkObjectEnd);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
end;

procedure TTestJsonReader.Skip;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
begin
  {%H-}Stream.Instance := TStringStream.Create('{"key": [42, "value"]}');
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.Read);
  Reader.Instance.Skip;
  AssertTrue(Reader.Instance.TokenKind = tkObjectEnd);
  AssertFalse(Reader.Instance.Read);
  Stream.Instance := TStringStream.Create('{"key": [42, "value"]}');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.Read);
  Reader.Instance.Skip;
  AssertTrue(Reader.Instance.TokenKind = tkArrayEnd);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkObjectEnd);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = rsEOF);

end;

procedure TTestJsonReader.Iterate;
var
  io: TIterObj;
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
  p: TPair;
begin
  {%H-}Stream.Instance := TStringStream.Create('');
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Iterate(@{%H-}io.CountIt);
  AssertTrue(io.List.IsEmpty);

  Stream.Instance := TStringStream.Create('0');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Iterate(@io.CountIt);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
  AssertTrue(io.List.Count = 1);
  p := io.List[0];
  AssertTrue((p.Key = '') and (p.Value = '0'));
  io.List.Clear;

  Stream.Instance := TStringStream.Create(TestJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Iterate(@io.CountIt);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
  AssertTrue(io.List.Count = 17);
  AssertTrue(io.List.CountOf('userid') = 2);
  for p in io.List.EqualKeys('userid') do
    AssertTrue((p.Value = '42') or (p.Value = '1001'));
  AssertTrue(io.List.CountOf('name') = 2);
  for p in io.List.EqualKeys('name') do
    AssertTrue((p.Value = 'John') or (p.Value = 'Thomas'));
  AssertTrue(io.List.CountOf('age') = 2);
  for p in io.List.EqualKeys('age') do
    AssertTrue((p.Value = '30') or (p.Value = '42'));
  AssertTrue(io.List.CountOf('online') = 2);
  for p in io.List.EqualKeys('online') do
    AssertTrue((p.Value = 'false') or (p.Value = 'true'));
  AssertTrue(io.List.CountOf('0') = 2);
  for p in io.List.EqualKeys('0') do
    AssertTrue(p.Value = 'talk');
  AssertTrue(io.List.CountOf('1') = 2);
  for p in io.List.EqualKeys('1') do
    AssertTrue((p.Value = 'humor') or (p.Value = 'games'));
  AssertTrue(io.List.CountOf('2') = 2);
  for p in io.List.EqualKeys('2') do
    AssertTrue((p.Value = 'cook') or (p.Value = 'math'));
  AssertTrue(io.List.CountOf('3') = 1);
  AssertTrue(io.List.Find('3')^.Value = 'art');
end;

procedure TTestJsonReader.Iterate1;
var
  io: TIterObj;
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
  I: Integer;
begin
  {%H-}Stream.Instance := TStringStream.Create(TestJson);
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Iterate(@{%H-}io.OnStruct, @{%H-}io.OnValue);
  AssertTrue(io.List.Count = 5);
  for I := 0 to Pred(io.List.Count) do
    case I of
      0: AssertTrue(io.List[I].Key = '');
      1: AssertTrue(io.List[I].Key = '0');
      2: AssertTrue(io.List[I].Key = 'groups');
      3: AssertTrue(io.List[I].Key = '1');
      4: AssertTrue(io.List[I].Key = 'groups');
    else
    end;
end;

procedure TTestJsonReader.Iterate2;
var
  io: TIterObj;
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
begin
  {%H-}Stream.Instance := TStringStream.Create(TestJson);
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From('/0/groups')));
  Reader.Instance.Iterate(@{%H-}io.CountIt);
  AssertTrue(io.List.Count = 3);
  AssertTrue(io.List[0].Value = 'talk');
  AssertTrue(io.List[1].Value = 'humor');
  AssertTrue(io.List[2].Value = 'cook');
end;

procedure TTestJsonReader.IterateNest;
var
  List: THashList;
  function CountIt(aReader: TJsonReader): Boolean;
  begin
    List.Add(TPair.Create(aReader.Name, aReader.ValueToString));
    Result := True;
  end;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
  p: TPair;
begin
  {%H-}Stream.Instance := TStringStream.Create('');
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Iterate(@CountIt);
  AssertTrue({%H-}List.IsEmpty);

  Stream.Instance := TStringStream.Create('0');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Iterate(@CountIt);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
  AssertTrue(List.Count = 1);
  p := List[0];
  AssertTrue((p.Key = '') and (p.Value = '0'));
  List.Clear;

  Stream.Instance := TStringStream.Create(TestJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Iterate(@CountIt);
  AssertTrue(Reader.Instance.ReadState = rsEOF);
  AssertTrue(List.Count = 17);
  AssertTrue(List.CountOf('userid') = 2);
  for p in List.EqualKeys('userid') do
    AssertTrue((p.Value = '42') or (p.Value = '1001'));
  AssertTrue(List.CountOf('name') = 2);
  for p in List.EqualKeys('name') do
    AssertTrue((p.Value = 'John') or (p.Value = 'Thomas'));
  AssertTrue(List.CountOf('age') = 2);
  for p in List.EqualKeys('age') do
    AssertTrue((p.Value = '30') or (p.Value = '42'));
  AssertTrue(List.CountOf('online') = 2);
  for p in List.EqualKeys('online') do
    AssertTrue((p.Value = 'false') or (p.Value = 'true'));
  AssertTrue(List.CountOf('0') = 2);
  for p in List.EqualKeys('0') do
    AssertTrue(p.Value = 'talk');
  AssertTrue(List.CountOf('1') = 2);
  for p in List.EqualKeys('1') do
    AssertTrue((p.Value = 'humor') or (p.Value = 'games'));
  AssertTrue(List.CountOf('2') = 2);
  for p in List.EqualKeys('2') do
    AssertTrue((p.Value = 'cook') or (p.Value = 'math'));
  AssertTrue(List.CountOf('3') = 1);
  AssertTrue(List.Find('3')^.Value = 'art');
end;

procedure TTestJsonReader.IterateNest1;
var
  I: Integer = 0;
  function OnStruct(aReader: TJsonReader): Boolean;
  begin
    case I of
      0: AssertTrue(aReader.ParentName = '');
      1: AssertTrue(aReader.ParentName = '0');
      2: AssertTrue(aReader.ParentName = 'groups');
      3: AssertTrue(aReader.ParentName = '1');
      4: AssertTrue(aReader.ParentName = 'groups');
    else
    end;
    Inc(I);
    Result := True;
  end;
  function OnValue({%H-}aReader: TJsonReader): Boolean;
  begin
    Result := True;
  end;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
begin
  {%H-}Stream.Instance := TStringStream.Create(TestJson);
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Iterate(@OnStruct, @OnValue);
  AssertTrue(I = 5);
end;

procedure TTestJsonReader.IterateNest2;
var
  List: THashList;
  function CountIt(aReader: TJsonReader): Boolean;
  begin
    List.Add(TPair.Create(aReader.Name, aReader.ValueToString));
    Result := True;
  end;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
begin
  {%H-}Stream.Instance := TStringStream.Create(TestJson);
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From('/1/groups')));
  Reader.Instance.Iterate(@CountIt);
  AssertTrue({%H-}List.Count = 4);
  AssertTrue(List[0].Value = 'talk');
  AssertTrue(List[1].Value = 'games');
  AssertTrue(List[2].Value = 'math');
  AssertTrue(List[3].Value = 'art');
end;

procedure TTestJsonReader.CopyStruct;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
  s: string = '';
begin
  {%H-}Stream.Instance := TStringStream.Create('0');
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Read;
  AssertFalse(Reader.Instance.CopyStruct(s));

  Stream.Instance := TStringStream.Create('{}');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = tkObjectBegin);
  AssertTrue(Reader.Instance.CopyStruct(s));
  AssertTrue(s = '{}');

  Stream.Instance := TStringStream.Create(TestJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Read;
  AssertTrue(Reader.Instance.CopyStruct(s));
  AssertTrue(s = TestJson);

  Stream.Instance := TStringStream.Create(TestJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Read;
  Reader.Instance.Read;
  AssertTrue(Reader.Instance.TokenKind = tkObjectBegin);
  AssertTrue(Reader.Instance.CopyStruct(s));
  AssertTrue(Reader.Instance.TokenKind = tkObjectEnd);
  AssertTrue(s = TestJson0);
  Stream.Instance := TStringStream.Create(TestJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Read;
  Reader.Instance.Read;
  Reader.Instance.Skip;
  Reader.Instance.Read;
  AssertTrue(Reader.Instance.TokenKind = tkObjectBegin);
  AssertTrue(Reader.Instance.CopyStruct(s));
  AssertTrue(Reader.Instance.TokenKind = tkObjectEnd);
  AssertTrue(s = TestJson1);
end;

procedure TTestJsonReader.MoveNext;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
  I: Integer;
begin
  {%H-}Stream.Instance := TStringStream.Create;
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertFalse(Reader.Instance.MoveNext);

  Stream.Instance := TStringStream.Create('0');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.MoveNext);
  AssertTrue((Reader.Instance.Name = '') and (Reader.Instance.ValueToString = '0'));
  AssertTrue(Reader.Instance.ReadState = rsEOF);

  Stream.Instance := TStringStream.Create('{}');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.MoveNext);
  AssertTrue(Reader.Instance.TokenKind = tkObjectEnd);
  AssertTrue(Reader.Instance.ReadState = rsGo);

  Stream.Instance := TStringStream.Create(TestJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  I := 0;
  while Reader.Instance.MoveNext do
    Inc(I);
  AssertTrue(I = 1);
  AssertTrue(Reader.Instance.TokenKind = tkArrayEnd);
  AssertTrue(Reader.Instance.ReadState = rsEOF);

  Stream.Instance := TStringStream.Create(TestJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Read;
  I := 0;
  while Reader.Instance.MoveNext do
    Inc(I);
  AssertTrue(I = 2);
  AssertTrue(Reader.Instance.TokenKind = tkArrayEnd);
  AssertTrue(Reader.Instance.ReadState = rsGo);

  Stream.Instance := TStringStream.Create(TestJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Read;
  Reader.Instance.Read;
  I := 0;
  while Reader.Instance.MoveNext do
    Inc(I);
  AssertTrue(I = 6);
  AssertTrue(Reader.Instance.TokenKind = tkObjectEnd);
  AssertTrue(Reader.Instance.ReadState = rsGo);
end;

procedure TTestJsonReader.Find;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
  I: Integer;
  s: string;
begin
  {%H-}Stream.Instance := TStringStream.Create('""');
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertFalse(Reader.Instance.Find(''));

  Stream.Instance := TStringStream.Create('[false, "item", 42, null]');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Read;
  AssertTrue(Reader.Instance.Find('2'));
  AssertTrue(Reader.Instance.ValueToString = '42');

  Stream.Instance := TStringStream.Create('[false, "item", 42, null]');
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Read;
  for I in [1..3] do
    Reader.Instance.MoveNext;
  AssertFalse(Reader.Instance.Find('2'));
  AssertTrue(Reader.Instance.Find('3'));
  AssertTrue(Reader.Instance.ValueToString = 'null');

  Stream.Instance := TStringStream.Create(TestJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Read;
  Reader.Instance.Read;
  AssertTrue(Reader.Instance.Find('name'));
  AssertTrue(Reader.Instance.ValueToString = 'John');
  AssertTrue(Reader.Instance.Find('groups'));
  AssertTrue(Reader.Instance.TokenKind = tkArrayBegin);
  AssertTrue(Reader.Instance.CopyStruct(s));
  AssertTrue(s = '["talk","humor","cook"]');

  Stream.Instance := TStringStream.Create(TestJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  Reader.Instance.Read;
  AssertTrue(Reader.Instance.Find('1'));
  AssertTrue(Reader.Instance.CopyStruct(s));
  AssertTrue(s = TestJson1);
end;

procedure TTestJsonReader.FindPath;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
  s: string;
begin
  {%H-}Stream.Instance := TStringStream.Create(TestJson);
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From('/0/groups')));
  AssertTrue(Reader.Instance.Find('1'));
  AssertTrue(Reader.Instance.ValueToString = 'humor');

  Stream.Instance := TStringStream.Create(TestJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From('/1/groups')));
  AssertTrue(Reader.Instance.CopyStruct(s));
  AssertTrue(s = '["talk","games","math","art"]');
end;

procedure TTestJsonReader.FindPath1;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
  s: string;
begin
  {%H-}Stream.Instance := TStringStream.Create(PathJson);
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From('')));
  AssertTrue(Reader.Instance.CopyStruct(s));
  AssertTrue(s = PathJson);

  Stream.Instance := TStringStream.Create(PathJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From('/foo')));
  AssertTrue(Reader.Instance.CopyStruct(s));
  AssertTrue(s = '["bar", "baz"]');

  Stream.Instance := TStringStream.Create(PathJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From('/foo/0')));
  AssertTrue(Reader.Instance.TokenKind = tkString);
  AssertTrue(Reader.Instance.AsString = 'bar');

  Stream.Instance := TStringStream.Create(PathJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From('/')));
  AssertTrue(Reader.Instance.TokenKind = tkNumber);
  AssertTrue(Reader.Instance.AsNumber = 0);

  Stream.Instance := TStringStream.Create(PathJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From(['a/b'])));
  AssertTrue(Reader.Instance.TokenKind = tkNumber);
  AssertTrue(Reader.Instance.AsNumber = 1);

  Stream.Instance := TStringStream.Create(PathJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From('/ ')));
  AssertTrue(Reader.Instance.TokenKind = tkNumber);
  AssertTrue(Reader.Instance.AsNumber = 7);

  Stream.Instance := TStringStream.Create(PathJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From(['m~n'])));
  AssertTrue(Reader.Instance.TokenKind = tkNumber);
  AssertTrue(Reader.Instance.AsNumber = 8);
end;

procedure TTestJsonReader.Path;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGUniqRef<TStringStream>;
  ptr: TJsonPtr;
begin
  {%H-}Stream.Instance := TStringStream.Create(PathJson);
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From('')));
  AssertTrue(Reader.Instance.Path = '');

  Stream.Instance := TStringStream.Create(PathJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From('/foo')));
  AssertTrue(Reader.Instance.Path = '/foo');

  Stream.Instance := TStringStream.Create(PathJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From('/foo/0')));
  AssertTrue(Reader.Instance.Path = '/foo/0');

  Stream.Instance := TStringStream.Create(PathJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From('/')));
  AssertTrue(Reader.Instance.Path = '/');

  Stream.Instance := TStringStream.Create(PathJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  ptr := TJsonPtr.From(['a/b']);
  AssertTrue(Reader.Instance.FindPath(ptr));
  AssertTrue(Reader.Instance.Path = ptr.ToString);

  Stream.Instance := TStringStream.Create(PathJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  AssertTrue(Reader.Instance.FindPath(TJsonPtr.From('/ ')));
  AssertTrue(Reader.Instance.Path = '/ ');

  Stream.Instance := TStringStream.Create(PathJson);
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  ptr := TJsonPtr.From(['m~n']);
  AssertTrue(Reader.Instance.FindPath(ptr));
  AssertTrue(Reader.Instance.Path = ptr.ToString);
end;

procedure TTestJsonReader.TestReader;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGAutoRef<TStringStream>;
  CurrFile, fn: string;
  State: TJsonReader.TReadState;
  c: AnsiChar;
  Total: Integer = 0;
begin
  AssertTrue('File list not loaded', Assigned(TestFileList));
  for CurrFile in TestFileList do
    begin
      Stream.Instance.LoadFromFile(CurrFile);
      Stream.Instance.Position := 0;
      Reader.Instance := TJsonReader.Create(Stream.Instance);
      while Reader.Instance.Read do;
      State := Reader.Instance.ReadState;
      fn := ExtractFileName(CurrFile);
      c := fn[1];
      Inc(Total);
      if c = 'y' then
        AssertTrue(fn + ': expected rsEOF, but got rsError', State = rsEOF)
      else
        if c = 'n' then
          AssertTrue(fn + ': expected rsError, but got rsEOF', State = rsError);
    end;
  AssertTrue(Total = 295);
end;

procedure TTestJsonReader.SkipBom;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  Stream: specialize TGAutoRef<TStringStream>;
begin
  {%H-}Stream.Instance := TStringStream.Create(#$EF#$BB#$BF + TestJson);
  Stream.Instance.Position := 0;
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance, 512, 10, True);
  while Reader.Instance.Read do;
  AssertTrue(Reader.Instance.ReadState = rsEOF);
end;

procedure TTestJsonReader.SkipBom1;
var
  Reader: specialize TGUniqRef<TJsonReader>;
  s: string;
begin
  s := #$EF#$BB#$BF + TestJson;
  {%H-}Reader.Instance := TJsonReader.Create(Pointer(s), Length(s), 10, True);
  while Reader.Instance.Read do;
  AssertTrue(Reader.Instance.ReadState = rsEOF);
end;

{ TTestJsonPatch }

procedure TTestJsonPatch.PatchBasic;
var
  Target, Expect: specialize TGUniqRef<TJsonNode>;
  Patch: specialize TGAutoRef<TJsonPatch>;
  s: string;
begin
  s := '';
  AssertTrue(Patch.Instance.Apply(s) = prPatchMiss);
  AssertTrue(s = '');

  AssertFalse(Patch.Instance.TryLoad('[.]'));

  AssertTrue(Patch.Instance.TryLoad(sBadPatch1));
  AssertFalse(Patch.Instance.Validate);
  AssertTrue('1', Patch.Instance.Apply(s) = prTargetMiss);

  s := sSource1;
  AssertTrue(Patch.Instance.Apply(s) = prMalformPatch);

  AssertTrue(Patch.Instance.TryLoad(sBadPatch2));
  AssertFalse(Patch.Instance.Validate);
  AssertTrue(Patch.Instance.Apply(s) = prMalformPatch);

  AssertTrue(Patch.Instance.TryLoad(sBadPatch3));
  AssertFalse(Patch.Instance.Validate);
  AssertTrue(Patch.Instance.Apply(s) = prMalformPatch);

  AssertTrue(Patch.Instance.TryLoad(sPatch1));
  AssertTrue(Patch.Instance.Apply(s) = prOk);
  AssertTrue(s = sTarget1);

  s := sSource1;
  AssertTrue(Patch.Instance.TryLoad(sPatch3));
  AssertTrue(Patch.Instance.Apply(s) = prOk);
  AssertTrue(s = sTarget1);

  {%H-}Target.Instance := TJsonNode.NewJson(sSource2);
  AssertTrue(Assigned(Target.Instance));
  {%H-}Expect.Instance := TJsonNode.NewJson(sTarget2);
  AssertTrue(Assigned(Expect.Instance));

  AssertTrue(Patch.Instance.TryLoad(sPatch2));
  AssertTrue(Patch.Instance.Apply(Target.Instance) = prOk);
  AssertTrue(Target.Instance.EqualTo(Expect.Instance));
end;

procedure TTestJsonPatch.PatchBasic1;
var
  Target, Expect: specialize TGUniqRef<TJsonNode>;
  Patch: specialize TGAutoRef<TJsonPatch>;
begin
  {%H-}Target.Instance := TJsonNode.NewJson(sSource2);
  AssertTrue(Assigned(Target.Instance));
  {%H-}Expect.Instance := TJsonNode.NewJson(sTarget2);
  AssertTrue(Assigned(Expect.Instance));

  AssertTrue(Patch.Instance.TryLoad(sBadPatch4));
  AssertTrue(Patch.Instance.Apply(Target.Instance) = prFail);

  AssertTrue(Patch.Instance.TryLoad(sBadPatch5));
  AssertTrue(Patch.Instance.Apply(Target.Instance) = prFail);
end;

procedure TTestJsonPatch.PatchTests;
var
  TestSet, CurrDoc: specialize TGUniqRef<TJsonNode>;
  CurrPatch: specialize TGAutoRef<TJsonPatch>;
  CurrTest, Expect, Node: TJsonNode;
  CurrFile, FileName, msg, sRet: string;
  I: Integer;
  Ret: TPatchResult;
const
  Fmt = '%s[%d]: %s';
  DocNotFound = 'Source document not found';
  TstNotObj   = 'Test instance is not an an object';
  PatchNotFound = 'Patch not found';
  ExpectNotFound = 'Expected document not found';
  UnexpectFmt = 'Unexpected Patch return(%s)';
  Unexpect = 'Result of applying the patch does not match the expected';
begin
  //trying to run tests from https://github.com/json-patch/json-patch-tests;
  AssertTrue('File list not loaded', Assigned(TestPatchList));
  AssertTrue('File list is empty', TestPatchList.Count > 0);
  for FileName in TestPatchList do
    begin
      CurrFile := ExtractFileName(FileName);
      TestSet.Instance := TJsonNode.LoadFromFile(FileName);
      AssertTrue('Failed to load file ' + CurrFile, Assigned(TestSet.Instance));
      AssertTrue(CurrFile + ': test set is not an array', TestSet.Instance.IsArray);
      for I := 0 to Pred(TestSet.Instance.Count) do
        begin
          CurrTest := TestSet.Instance.Items[I];
          AssertTrue(Format(Fmt, [CurrFile, I, TstNotObj]), CurrTest.IsObject);
          AssertTrue(Format(Fmt, [CurrFile, I, DocNotFound]), CurrTest.Find('doc', Node));
          CurrDoc.Instance := Node.Clone;
          AssertTrue(Format(Fmt, [CurrFile, I, PatchNotFound]), CurrTest.Find('patch', Node));
          CurrPatch.Instance.Load(Node);
          if CurrTest.Find('error', Node) then
            begin
              Ret := CurrPatch.Instance.Apply(CurrDoc.Instance);
              WriteStr(sRet, Ret);
              msg := Format(Fmt, [CurrFile, I, Format(UnexpectFmt, [sRet])]);
              AssertTrue(msg, Ret <> prOk);
            end
          else
            begin
              AssertTrue(Format(Fmt, [CurrFile, I, ExpectNotFound]), CurrTest.Find('expected', Expect));
              Ret := CurrPatch.Instance.Apply(CurrDoc.Instance);
              WriteStr(sRet, Ret);
              msg := Format(Fmt, [CurrFile, I, Format(UnexpectFmt, [sRet])]);
              AssertTrue(msg, Ret = prOk);
              AssertTrue(Format(Fmt, [CurrFile, I, Unexpect]), CurrDoc.Instance.EqualTo(Expect));
            end;
        end;
    end;
end;

procedure TTestJsonPatch.DiffBasic;
var
  Source, Target, Diff: specialize TGUniqRef<TJsonNode>;
  Node: TJsonNode = nil;
begin
  AssertTrue(TJsonPatch.Diff({%H-}Source.Instance, {%H-}Target.Instance, Node) = drSourceMiss);

  Source.Instance := TJsonNode.NewJson(sSource1);
  AssertTrue(TJsonPatch.Diff(Source.Instance, Target.Instance, Node) = drTargetMiss);

  Target.Instance := TJsonNode.NewJson(sTarget1);
  AssertTrue(TJsonPatch.Diff(Source.Instance, Target.Instance, Node) = drOk);
  {%H-}Diff.Instance := Node;
  AssertTrue(TJsonPatch.Patch(Diff.Instance.AsJson, Source.Instance) = prOk);
  AssertTrue(Source.Instance.EqualTo(Target.Instance));

  Source.Instance := TJsonNode.NewJson(sSource2);
  Target.Instance := TJsonNode.NewJson(sTarget2);
  AssertTrue(TJsonPatch.Diff(Source.Instance, Target.Instance, Node) = drOk);
  Diff.Instance := Node;
  AssertTrue(TJsonPatch.Patch(Diff.Instance.AsJson, Source.Instance) = prOk);
  AssertTrue(Source.Instance.EqualTo(Target.Instance));

  Source.Instance.AsJson := sSource3;
  Target.Instance.AsJson := sTarget3;
  AssertTrue(TJsonPatch.Diff(Source.Instance, Target.Instance, Node) = drOk);
  Diff.Instance := Node;
  AssertTrue(TJsonPatch.Patch(Diff.Instance.AsJson, Source.Instance) = prOk);
  AssertTrue(Source.Instance.EqualTo(Target.Instance));
end;

procedure TTestJsonPatch.DiffTests;
var
  TestSet, CurrDoc: specialize TGUniqRef<TJsonNode>;
  CurrPatch: specialize TGUniqRef<TJsonPatch>;
  Patch: TJsonPatch;
  CurrTest, Expect, Node: TJsonNode;
  CurrFile, FileName, msg, sRet: string;
  I: Integer;
  Ret: TDiffResult;
const
  Fmt = '%s[%d]: %s';
  DocNotFound = 'Source document not found';
  TstNotObj   = 'Test instance is not an an object';
  ExpectNotFound = 'Expected document not found';
  UnexpectFmt = 'Unexpected Diff return(%s)';
  PatchFails  = 'Generated patch not working';
  Unexpect = 'Result of applying the patch does not match the expected';
begin
  //trying use test data from https://github.com/json-patch/json-patch-tests;
  AssertTrue('File list not loaded', Assigned(TestPatchList));
  AssertTrue('File list is empty', TestPatchList.Count > 0);
  for FileName in TestPatchList do
    begin
      CurrFile := ExtractFileName(FileName);
      TestSet.Instance := TJsonNode.LoadFromFile(FileName);
      AssertTrue('Failed to load file ' + CurrFile, Assigned(TestSet.Instance));
      AssertTrue(CurrFile + ': test set is not an array', TestSet.Instance.IsArray);
      for I := 0 to Pred(TestSet.Instance.Count) do
        begin
          CurrTest := TestSet.Instance.Items[I];
          AssertTrue(Format(Fmt, [CurrFile, I, TstNotObj]), CurrTest.IsObject);
          if CurrTest.Find('error', Node) then
            continue;
          AssertTrue(Format(Fmt, [CurrFile, I, DocNotFound]), CurrTest.Find('doc', Node));
          CurrDoc.Instance := Node.Clone;
          AssertTrue(Format(Fmt, [CurrFile, I, ExpectNotFound]), CurrTest.Find('expected', Expect));
          Ret := TJsonPatch.Diff(CurrDoc.Instance, Expect, Patch);
          WriteStr(sRet, Ret);
          msg := Format(Fmt, [CurrFile, I, Format(UnexpectFmt, [sRet])]);
          AssertTrue(msg, Ret = drOk);
          CurrPatch.Instance := Patch;
          AssertTrue(Format(Fmt, [CurrFile, I, PatchFails]),
                     CurrPatch.Instance.Apply(CurrDoc.Instance) = prOk);
          AssertTrue(Format(Fmt, [CurrFile, I, Unexpect]), CurrDoc.Instance.EqualTo(Expect));
        end;
    end;
end;

procedure TTestJsonPatch.DiffEmitTestOnRemove;
var
  Source, Target, Diff, Test: specialize TGUniqRef<TJsonNode>;
  Patch: specialize TGAutoRef<TJsonPatch>;
  Node: TJsonNode;
  Ret: TDiffResult;
const
  sSource = '{"foo": [1, "fizz", 3], "boo": ["bar", "baz"]}';
  sTarget = '{"foo": [1, 3], "boo": ["baz"]}';
begin
  {%H-}Source.Instance := TJsonNode.NewJson(sSource);
  {%H-}Target.Instance := TJsonNode.NewJson(sTarget);
  Ret := TJsonPatch.Diff(Source.Instance, Target.Instance, Node);
  AssertTrue('Diff failed', Ret = drOk);
  {%H-}Diff.Instance := Node;
  AssertTrue('Unexpected diff count', Node.Count = 2);
  Ret := TJsonPatch.Diff(Source.Instance, Target.Instance, Node, [doEmitTestOnRemove]);
  AssertTrue('Diff with option failed', Ret = drOk);
  Diff.Instance := Node;
  AssertTrue('Unexpected diff count with option', Node.Count = 4);
  {%H-}Test.Instance := Source.Instance.Clone;
  Patch.Instance.Load(Diff.Instance);
  AssertTrue('Patching1 failed', Patch.Instance.Apply(Test.Instance) = prOk);
  AssertTrue('Patching1 wrong', Test.Instance.EqualTo(Target.Instance));

  Test.Instance := Source.Instance.Clone;
  Test.Instance.FindPath(TJsonPtr.From('/foo/1')).AsString := 'fuzz';
  AssertTrue('Unexpected patching2 result', Patch.Instance.Apply(Test.Instance) = prFail);

  Test.Instance := Source.Instance.Clone;
  Test.Instance.FindPath(TJsonPtr.From('/boo/0')).AsString := 'ber';
  AssertTrue('Unexpected patching3 result', Patch.Instance.Apply(Test.Instance) = prFail);
end;

procedure TTestJsonPatch.DiffEmitTestOnReplace;
var
  Source, Target, Diff, Test: specialize TGUniqRef<TJsonNode>;
  Patch: specialize TGAutoRef<TJsonPatch>;
  Node: TJsonNode;
  Ret: TDiffResult;
const
  sSource = '{"foo": [1, "fizz", 3], "boo": ["bar", "baz"]}';
  sTarget = '{"foo": [1, "fuzz", 3], "boo": ["ber", "baz"]}';
begin
  {%H-}Source.Instance := TJsonNode.NewJson(sSource);
  {%H-}Target.Instance := TJsonNode.NewJson(sTarget);
  Ret := TJsonPatch.Diff(Source.Instance, Target.Instance, Node);
  AssertTrue('Diff failed', Ret = drOk);
  {%H-}Diff.Instance := Node;
  AssertTrue('Unexpected diff count', Node.Count = 2);
  Ret := TJsonPatch.Diff(Source.Instance, Target.Instance, Node, [doEmitTestOnReplace]);
  AssertTrue('Diff with option failed', Ret = drOk);
  Diff.Instance := Node;
  AssertTrue('Unexpected diff count with option', Node.Count = 4);
  {%H-}Test.Instance := Source.Instance.Clone;
  Patch.Instance.Load(Diff.Instance);
  AssertTrue('Patching1 failed', Patch.Instance.Apply(Test.Instance) = prOk);
  AssertTrue('Patching1 wrong', Test.Instance.EqualTo(Target.Instance));

  Test.Instance := Source.Instance.Clone;
  Test.Instance.FindPath(TJsonPtr.From('/foo/1')).AsString := 'fiz';
  AssertTrue('Unexpected patching2 result', Patch.Instance.Apply(Test.Instance) = prFail);

  Test.Instance := Source.Instance.Clone;
  Test.Instance.FindPath(TJsonPtr.From('/boo/0')).AsString := 'bir';
  AssertTrue('Unexpected patching3 result', Patch.Instance.Apply(Test.Instance) = prFail);
end;

procedure TTestJsonPatch.DiffEnableMove;
var
  Source, Target, Diff, Test: specialize TGUniqRef<TJsonNode>;
  Patch: specialize TGAutoRef<TJsonPatch>;
  Node: TJsonNode;
  Ret: TDiffResult;
const
  sSource = '{"foo": [1, "fizz", 3], "boo": ["bar", "baz", "fizz"]}';
  sTarget = '{"foo": [1, 3, "fizz"], "boo": ["baz", "fizz"], "ozz": "bar"}';
begin
  {%H-}Source.Instance := TJsonNode.NewJson(sSource);
  {%H-}Target.Instance := TJsonNode.NewJson(sTarget);
  Ret := TJsonPatch.Diff(Source.Instance, Target.Instance, Node);
  AssertTrue('Diff failed', Ret = drOk);
  {%H-}Diff.Instance := Node;
  AssertTrue('Unexpected diff count', Node.Count = 4);
  Ret := TJsonPatch.Diff(Source.Instance, Target.Instance, Node, [doEnableMove]);
  AssertTrue('Diff with option failed', Ret = drOk);
  Diff.Instance := Node;
  AssertTrue('Unexpected diff count with option', Node.Count = 2);
  {%H-}Test.Instance := Source.Instance.Clone;
  Patch.Instance.Load(Diff.Instance);
  AssertTrue('Patching failed', Patch.Instance.Apply(Test.Instance) = prOk);
  AssertTrue('Patching wrong', Test.Instance.EqualTo(Target.Instance));
end;

initialization

  LoadFileLists;
  RegisterTest(TTestJVariant);
  RegisterTest(TTestDouble2Str);
  RegisterTest(TTestTryStr2Double);
  RegisterTest(TTestJson);
  RegisterTest(TTestJsonWriter);
  RegisterTest(TTestJsonReader);
  RegisterTest(TTestJsonPatch);

finalization

  TestFileList.Free;
  TestPatchList.Free;

end.

