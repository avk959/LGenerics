unit lgPdoTest;

{$MODE OBJFPC}{$H+}{$OPTIMIZATION NOORDERFIELDS}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, lgPdo, lgArrayHelpers, Variants;

type
  { TTestPdoRegister }

  TTestPdoRegister = class(TTestCase)
  private
  type

    TRec = record
      a, b, c: Integer;
    end;

    TRec2 = record
      a, b, c: Integer;
    end;

    TObj = object
      a: Boolean;
    end;
  published
    procedure TestRegister;
    procedure Registered;
    procedure UnRegister;
  end;

  { TTestPdoToJson }

  TTestPdoToJson = class(TTestCase)
  private
  type

    TMyRec = record
      MyInt: Integer;
      MyBool: WordBool;
      MyBoolean: Boolean;
      MyStr: string;
      MySString: string[20];
      MyQWord: QWord;
    end;

    TSimple = record
      a: Integer;
      b: string;
    end;
    TSimpleArray = array of TSimple;

    TMyObj = class(TCollectionItem)
    private
      FName: string;
      FValue: Variant;
    published
      property name: string read FName write FName;
      property value: Variant read FValue write FValue;
    end;

  published
    procedure Unregistered;
    procedure UnregisteredDynArray;
    procedure UnregisteredArray;
    procedure UnregisteredArray2D;
    procedure UnregisteredArrayOfDynArray;
    procedure Registered;
    procedure Registered1;
    procedure RegisteredDynArray;
    procedure RegisteredArrayOfDynArray;
    procedure AString;
    procedure WString;
    procedure UAString;
    procedure AChar;
    procedure WChar;
    procedure UChar;
    procedure TestVariantShortInt;
    procedure TestVariantNull;
    procedure TestVariantUStr;
    procedure TestVarArray;
    procedure TestVarArray1;
    procedure TestObject;
    procedure TestObject1;
    procedure TestCollection;
  end;

implementation

{ TTestPdoRegister }

procedure TTestPdoRegister.TestRegister;
var
  a: TStringArray = ('one', 'two', 'three','four');
  si: ShortInt;
  b: Byte;
  smi: SmallInt;
  w: Word;
  i: Integer;
  dw: DWord;
  i64: Int64;
  qw: QWord;
  bo: Boolean;
  b8: Boolean8;
  b16: Boolean16;
  b32: Boolean32;
  b64: Boolean64;
  bb: ByteBool;
  wb: WordBool;
  lb: LongBool;
  qwb: QWordBool;
  c: Char;
  s: string;
  ss: shortstring;
  o: TObj;
  lst: TList;
  intf: IInterface;
  r: TRec;
begin
  AssertFalse(RegisterPdo(TypeInfo(si), a));
  AssertFalse(RegisterPdo(TypeInfo(b), a));
  AssertFalse(RegisterPdo(TypeInfo(smi), a));
  AssertFalse(RegisterPdo(TypeInfo(w), a));
  AssertFalse(RegisterPdo(TypeInfo(i), a));
  AssertFalse(RegisterPdo(TypeInfo(dw), a));
  AssertFalse(RegisterPdo(TypeInfo(i64), a));
  AssertFalse(RegisterPdo(TypeInfo(qw), a));
  AssertFalse(RegisterPdo(TypeInfo(bo), a));
  AssertFalse(RegisterPdo(TypeInfo(b8), a));
  AssertFalse(RegisterPdo(TypeInfo(b16), a));
  AssertFalse(RegisterPdo(TypeInfo(b32), a));
  AssertFalse(RegisterPdo(TypeInfo(b64), a));
  AssertFalse(RegisterPdo(TypeInfo(bb), a));
  AssertFalse(RegisterPdo(TypeInfo(wb), a));
  AssertFalse(RegisterPdo(TypeInfo(lb), a));
  AssertFalse(RegisterPdo(TypeInfo(qwb), a));
  AssertFalse(RegisterPdo(TypeInfo(c), a));
  AssertFalse(RegisterPdo(TypeInfo(s), a));
  AssertFalse(RegisterPdo(TypeInfo(ss), a));
  AssertFalse(RegisterPdo(TypeInfo(o), a));
  AssertFalse(RegisterPdo(TypeInfo(lst), a));
  AssertFalse(RegisterPdo(TypeInfo(intf), a));

  AssertTrue(RegisterPdo(TypeInfo(r), a));
end;

procedure TTestPdoRegister.Registered;
var
  r1: TRec;
  r2: TRec2;
  a: TStringArray;
const
  Expect: TStringArray = ('one', 'two', 'three');
begin
  a := ['dummy'];
  AssertFalse(RegisteredPdo(TypeInfo(r2), a));
  AssertTrue(a = nil);
  AssertTrue(RegisteredPdo(TypeInfo(r1), a));
  AssertTrue(specialize TGComparableArrayHelper<string>.Same(a, Expect));
end;

procedure TTestPdoRegister.UnRegister;
var
  r1: TRec;
  r2: TRec2;
begin
  AssertFalse(UnRegisterPdo(TypeInfo(r2)));
  AssertTrue(UnRegisterPdo(TypeInfo(r1)));
  AssertFalse(UnRegisterPdo(TypeInfo(r1)));
end;

procedure TTestPdoToJson.Unregistered;
var
  r: TMyRec;
  s: string;
const
  Expect =
    '{"field1":2147483647,"field2":true,"field3":true,"field4":"just string","field5":"short string","field6":9007199254740991}';
begin
  r.MyInt := MaxInt;
  r.MyBool := WordBool(42);
  r.MyBoolean := True;
  r.MyStr := 'just string';
  r.MySString := 'short string';
  r.MyQWord := 9007199254740991;
  s := PdoToJson(TypeInfo(r), r);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.UnregisteredDynArray;
var
  a: TSimpleArray = ((a: 42; b: 'str1'), (a: 1001; b: 'str2'));
  s: string;
const
  Expect = '[{"field1":42,"field2":"str1"},{"field1":1001,"field2":"str2"}]';
begin
  s := PdoToJson(TypeInfo(a), a);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.UnregisteredArray;
type
  TArr = array[1..2] of TSimple;
var
  a: TArr = ((a: 42; b: 'str1'), (a: 1001; b: 'str2'));
  s: string;
const
  Expect = '[{"field1":42,"field2":"str1"},{"field1":1001,"field2":"str2"}]';
begin
  s := PdoToJson(TypeInfo(a), a);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.UnregisteredArray2D;
type
  TArr = array[1..2, 0..1] of TSimple;
var
  a: TArr = (
    ((a: 42; b: 'str1'), (a: 1001; b: 'str2')),
    ((a: 12; b: 'str3'), (a: 1024; b: 'str4')));
  s: string;
const
  Expect = '[{"field1":42,"field2":"str1"},{"field1":1001,"field2":"str2"},' +
           '{"field1":12,"field2":"str3"},{"field1":1024,"field2":"str4"}]';
begin
  s := PdoToJson(TypeInfo(a), a);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.UnregisteredArrayOfDynArray;
type
  TArr = array[1..2] of TSimpleArray;
var
  a: TArr = (
    ((a: 42; b: 'str1'), (a: 1001; b: 'str2')),
    ((a: 12; b: 'str3'), (a: 1024; b: 'str4')));
  s: string;
const
  Expect = '[[{"field1":42,"field2":"str1"},{"field1":1001,"field2":"str2"}],' +
           '[{"field1":12,"field2":"str3"},{"field1":1024,"field2":"str4"}]]';
begin
  s := PdoToJson(TypeInfo(a), a);
  AssertTrue(s, s = Expect);
end;

procedure TTestPdoToJson.Registered;
var
  r: TMyRec;
  s: string;
  fm: TStringArray = ('myint','mybool','myboolean','mystr','mysstring','myqword');
const
  Expect =
    '{"myint":2147483647,"mybool":true,"myboolean":true,"mystr":"just string","mysstring":"short string","myqword":9007199254740991}';
begin
  r.MyInt := MaxInt;
  r.MyBool := WordBool(42);
  r.MyBoolean := True;
  r.MyStr := 'just string';
  r.MySString := 'short string';
  r.MyQWord := 9007199254740991;
  AssertTrue(RegisterPdo(TypeInfo(r), fm));
  s := PdoToJson(TypeInfo(r), r);
  AssertTrue(s = Expect);
  AssertTrue(UnRegisterPdo(TypeInfo(r)));
end;

procedure TTestPdoToJson.Registered1;
var
  r: TMyRec;
  s: string;
  fm: TStringArray = ('myint','mybool','','mystr','','myqword');
const
  Expect =
    '{"myint":2147483647,"mybool":true,"mystr":"just string","myqword":9007199254740991}';
begin
  r.MyInt := MaxInt;
  r.MyBool := WordBool(42);
  r.MyBoolean := True;
  r.MyStr := 'just string';
  r.MySString := 'short string';
  r.MyQWord := 9007199254740991;
  AssertTrue(RegisterPdo(TypeInfo(r), fm));
  s := PdoToJson(TypeInfo(r), r);
  AssertTrue(s = Expect);
  AssertTrue(UnRegisterPdo(TypeInfo(r)));
end;

procedure TTestPdoToJson.RegisteredDynArray;
var
  a: TSimpleArray = ((a: 42; b: 'str1'), (a: 1001; b: 'str2'));
  b: TStringArray = ('a', 'b');
  s: string;
const
  Expect = '[{"a":42,"b":"str1"},{"a":1001,"b":"str2"}]';
begin
  AssertTrue(RegisterPdo(TypeInfo(TSimple), b));
  s := PdoToJson(TypeInfo(a), a);
  AssertTrue(s = Expect);
  AssertTrue(UnRegisterPdo(TypeInfo(TSimple)));
end;

procedure TTestPdoToJson.RegisteredArrayOfDynArray;
type
  TArr = array[1..2] of TSimpleArray;
var
  a: TArr = (
    ((a: 42; b: 'str1'), (a: 1001; b: 'str2')),
    ((a: 12; b: 'str3'), (a: 1024; b: 'str4')));
  b: TStringArray = ('a', 'b');
  s: string;
const
  Expect = '[[{"a":42,"b":"str1"},{"a":1001,"b":"str2"}],' +
           '[{"a":12,"b":"str3"},{"a":1024,"b":"str4"}]]';
begin
  AssertTrue(RegisterPdo(TypeInfo(TSimple), b));
  s := PdoToJson(TypeInfo(a), a);
  AssertTrue(s, s = Expect);
  AssertTrue(UnRegisterPdo(TypeInfo(TSimple)));
end;

procedure TTestPdoToJson.AString;
var
  s: string;
const
  Expect = '"Мороз и солнце; день чудесный!"';
begin
  s := 'Мороз и солнце; день чудесный!';
  s := PdoToJson(TypeInfo(s), s);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.WString;
var
  ws: widestring;
  s: string;
const
  Expect = '"Мороз и солнце; день чудесный!"';
begin
  s := 'Мороз и солнце; день чудесный!';
  ws := {%H-}s;
  s := PdoToJson(TypeInfo(ws), ws);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.UAString;
var
  us: unicodestring;
  s: string;
const
  Expect = '"Мороз и солнце; день чудесный!"';
begin
  s := 'Мороз и солнце; день чудесный!';
  us := {%H-}s;
  s := PdoToJson(TypeInfo(us), us);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.AChar;
var
  c: Char = 'p';
  s: string;
const
  Expect = '"p"';
begin
  s := PdoToJson(TypeInfo(c), c);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.WChar;
var
  wc: WideChar = WideChar($1A9);
  s: string;
const
  Expect = '"Ʃ"';
begin
  s := PdoToJson(TypeInfo(wc), wc);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.UChar;
var
  uc: UnicodeChar = UnicodeChar($1A9);
  s: string;
const
  Expect = '"Ʃ"';
begin
  s := PdoToJson(TypeInfo(uc), uc);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.TestVariantShortInt;
var
  v: Variant;
  s: string;
const
  Expect = '1001';
begin
  v := 1001;
  s := PdoToJson(TypeInfo(v), v);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.TestVariantNull;
var
  v: Variant;
  s: string;
const
  Expect = 'null';
begin
  s := PdoToJson(TypeInfo(v), v{%H-});
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.TestVariantUStr;
var
  v: Variant;
  s: string;
const
  Expect = '"Мороз и солнце"';
begin
  s := 'Мороз и солнце';
  v := unicodestring(s);
  s := PdoToJson(TypeInfo(v), v);
  AssertTrue(s, s = Expect);
end;

procedure TTestPdoToJson.TestVarArray;
var
  v, a: Variant;
  s: string;
const
  Expect = '[42,"abracadabra",false,null]';
begin
  a := VarArrayOf([42, 'abracadabra', False, {%H-}v]);
  s := PdoToJson(TypeInfo(a), a);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.TestVarArray1;
var
  v: Variant;
  s: string;
const
  Expect = '[42,"abracadabra",false,["foo",null,1001]]';
begin
  v := VarArrayOf([42, 'abracadabra', False, VarArrayOf(['foo', Null, 1001])]);
  s := PdoToJson(TypeInfo(v), v);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.TestObject;
var
  o: TObject = nil;
  s: string;
const
  Expect = 'null';
begin
  s := PdoToJson(TypeInfo(o), o);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.TestObject1;
var
  o: TMyObj = nil;
  s: string;
const
  Expect = '{"name":"just a name","value":42}';
begin
  o := TMyObj.Create(nil);
  o.Name := 'just a name';
  o.Value := 42;
  s := PdoToJson(TypeInfo(o), o);
  o.Free;
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.TestCollection;
var
  c: TCollection;
  s: string;
const
  Expect = '[{"name":"just a name","value":42},{"name":"another name","value":1001}]';
begin
  c := TCollection.Create(TMyObj);
  with TMyObj(c.Add) do
    begin
      Name := 'just a name';
      Value := 42;
    end;
  with TMyObj(c.Add) do
    begin
      Name := 'another name';
      Value := 1001;
    end;
  s := PdoToJson(TypeInfo(c), c);
  c.Free;
  AssertTrue(s = Expect);
end;

initialization
  RegisterTest(TTestPdoRegister);
  RegisterTest(TTestPdoToJson);
end.

