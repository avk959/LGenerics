unit lgPdoTest;

{$MODE OBJFPC}{$H+}{$OPTIMIZATION NOORDERFIELDS}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Variants, fpcunit, testregistry, lgPdo, lgArrayHelpers;

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
      class procedure WriteJson(r: Pointer; aWriter: TJsonStrWriter); static;
    end;
    TSimpleArray = array of TSimple;

    TMyItem = class(TCollectionItem)
    private
      FName: string;
      FValue: Variant;
    published
      property name: string read FName write FName;
      property value: Variant read FValue write FValue;
    end;

    TMyItemClass = class of TMyItem;

    TMyClass = class
    private
      FName: string;
    public
      property name: string read FName write FName;
    end;

    TMyObj = object
    private
      FName: string;
      FValue: Integer;
      FFlag: Boolean;
      class procedure WriteJson(o: Pointer; aWriter: TJsonStrWriter); static;
    public
      property Name: string read FName write FName;
      property Value: Integer read FValue write FValue;
      property Flag: Boolean read FFlag write FFlag;
    end;

    TMyEnum   = (meZero, meOne, meTwo, meThree, meFour, meFive, meSix);
    TMySet    = set of TMyEnum;
    TByteSet  = set of Byte;
    TSmallSet = set of 0..31;

    TMyEnumSet = class(TCollectionItem)
    private
      FSet: TMySet;
    published
      property setValue: TMySet read FSet write FSet;
    end;

    TMySmallSet = class(TCollectionItem)
    private
      FSet: TSmallSet;
    published
      property setValue: TSmallSet read FSet write FSet;
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
    procedure Enum;
    procedure EnumSet;
    procedure CharSet;
    procedure ByteSet;
    procedure PublishedSet;
    procedure PublishedSet1;
    procedure TestVariantInt;
    procedure TestVariantNull;
    procedure TestVariantUStr;
    procedure TestVarArray;
    procedure TestVarArray1;
    procedure UnsupportVariant;
    procedure UnsupportType;
    procedure TestClass;
    procedure TestClass1;
    procedure UnregisteredClass;
    procedure TestCollection;
    procedure CustomRecordProc;
    procedure CustomObjectProc;
    procedure UnregisteredObject;
    procedure Strings;
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
  AssertFalse(RegisterRecordFields(TypeInfo(si), a));
  AssertFalse(RegisterRecordFields(TypeInfo(b), a));
  AssertFalse(RegisterRecordFields(TypeInfo(smi), a));
  AssertFalse(RegisterRecordFields(TypeInfo(w), a));
  AssertFalse(RegisterRecordFields(TypeInfo(i), a));
  AssertFalse(RegisterRecordFields(TypeInfo(dw), a));
  AssertFalse(RegisterRecordFields(TypeInfo(i64), a));
  AssertFalse(RegisterRecordFields(TypeInfo(qw), a));
  AssertFalse(RegisterRecordFields(TypeInfo(bo), a));
  AssertFalse(RegisterRecordFields(TypeInfo(b8), a));
  AssertFalse(RegisterRecordFields(TypeInfo(b16), a));
  AssertFalse(RegisterRecordFields(TypeInfo(b32), a));
  AssertFalse(RegisterRecordFields(TypeInfo(b64), a));
  AssertFalse(RegisterRecordFields(TypeInfo(bb), a));
  AssertFalse(RegisterRecordFields(TypeInfo(wb), a));
  AssertFalse(RegisterRecordFields(TypeInfo(lb), a));
  AssertFalse(RegisterRecordFields(TypeInfo(qwb), a));
  AssertFalse(RegisterRecordFields(TypeInfo(c), a));
  AssertFalse(RegisterRecordFields(TypeInfo(s), a));
  AssertFalse(RegisterRecordFields(TypeInfo(ss), a));
  AssertFalse(RegisterRecordFields(TypeInfo(o), a));
  AssertFalse(RegisterRecordFields(TypeInfo(lst), a));
  AssertFalse(RegisterRecordFields(TypeInfo(intf), a));

  AssertTrue(RegisterRecordFields(TypeInfo(r), a));
  AssertTrue(UnRegisterPdo(TypeInfo(r)));
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
  AssertFalse(RegisteredRecordFields(TypeInfo(r2), a));
  AssertTrue(a = nil);
  a := ['dummy'];
  AssertFalse(RegisteredRecordFields(TypeInfo(r1), a));
  AssertTrue(a = nil);
  a := ['one', 'two', 'three','four'];
  AssertTrue(RegisterRecordFields(TypeInfo(r1), a));
  AssertFalse(RegisteredRecordFields(TypeInfo(r2), a));
  AssertTrue(a = nil);
  AssertTrue(RegisteredRecordFields(TypeInfo(r1), a));
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

{ TTestPdoToJson.TSimple }

class procedure TTestPdoToJson.TSimple.WriteJson(r: Pointer; aWriter: TJsonStrWriter);
type
  PSimple = ^TSimple;
begin
  with PSimple(r)^ do
    aWriter
      .BeginObject
        .Add('field a', a)
        .Add('field b', b)
      .EndObject;
end;

{ TItem }

class procedure TTestPdoToJson.TMyObj.WriteJson(o: Pointer; aWriter: TJsonStrWriter);
type
  PMyObj = ^TMyObj;
const
  cName  = 'name';
  cValue = 'value';
  cFlag  = 'flag';
begin
  with PMyObj(o)^ do begin
    aWriter
      .BeginObject
        .Add(cName, Name)
        .Add(cValue, Value);
    if Flag then
      aWriter.AddTrue(cFlag)
    else
      aWriter.AddTrue(cFlag);
    aWriter .EndObject;
  end;
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
  AssertTrue(RegisterRecordFields(TypeInfo(r), fm));
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
  AssertTrue(RegisterRecordFields(TypeInfo(r), fm));
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
  AssertTrue(RegisterRecordFields(TypeInfo(TSimple), b));
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
  AssertTrue(RegisterRecordFields(TypeInfo(TSimple), b));
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

procedure TTestPdoToJson.Enum;
var
  a: array of TMyEnum = (meOne, meOne, meFour, meTwo, meThree);
  s: string;
const
  Expect = '["meOne","meOne","meFour","meTwo","meThree"]';
begin
  s := PdoToJson(TypeInfo(a), a);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.EnumSet;
var
  ms: TMySet = [meSix, meZero, meTwo, meFive];
  s: string;
const
  Expect = '["meZero","meTwo","meFive","meSix"]';
begin
  s := PdoToJson(TypeInfo(ms), ms);
  AssertTrue(s = Expect);
  ms := [];
  s := PdoToJson(TypeInfo(ms), ms);
  AssertTrue(s = '[]');
end;

procedure TTestPdoToJson.CharSet;
var
  cs: TSysCharset = ['b', 'f', 'k', 'q', 'x', 'D'];
  s: string;
const
  Expect = '["D","b","f","k","q","x"]';
begin
  s := PdoToJson(TypeInfo(cs), cs);
  AssertTrue(s = Expect);
  cs := [];
  s := PdoToJson(TypeInfo(cs), cs);
  AssertTrue(s = '[]');
end;

procedure TTestPdoToJson.ByteSet;
var
  bs: TByteSet = [0, 67, 101, 255, 11, 42];
  s: string;
const
  Expect = '[0,11,42,67,101,255]';
begin
  s := PdoToJson(TypeInfo(bs), bs);
  AssertTrue(s = Expect);
  bs := [];
  s := PdoToJson(TypeInfo(bs), bs);
  AssertTrue(s = '[]');
end;

procedure TTestPdoToJson.PublishedSet;
var
  ms: TMyEnumSet;
  s: string;
const
  Expect = '{"setValue":["meZero","meTwo","meFive","meSix"]}';
begin
  ms := TMyEnumSet.Create(nil);
  ms.setValue := [meSix, meZero, meTwo, meFive];
  s := PdoToJson(TypeInfo(ms), ms);
  AssertTrue(s = Expect);
  ms.setValue := [];
  s := PdoToJson(TypeInfo(ms), ms);
  ms.Free;
  AssertTrue(s = '{"setValue":[]}');
end;

procedure TTestPdoToJson.PublishedSet1;
var
  ss: TMySmallSet;
  s: string;
const
  Expect = '{"setValue":[0,7,13,17,23,29,31]}';
begin
  ss := TMySmallSet.Create(nil);
  ss.setValue := [0, 23, 29, 31, 7, 13, 17];
  s := PdoToJson(TypeInfo(ss), ss);
  AssertTrue(s = Expect);
  ss.setValue := [];
  s := PdoToJson(TypeInfo(ss), ss);
  ss.Free;
  AssertTrue(s = '{"setValue":[]}');
end;

procedure TTestPdoToJson.TestVariantInt;
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

procedure TTestPdoToJson.UnsupportVariant;
var
  v: Variant;
  s: string;
  I: IInterface;
  Raised: Boolean = False;
const
  Expect = '"unknown data"';
begin
  v := I;
  s := PdoToJson(TypeInfo(v), v);
  AssertTrue(s = Expect);
  try
    s := PdoToJson(TypeInfo(v), v, True);
  except
    on e: EJsonExport do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoToJson.UnsupportType;
var
  c: TMyItemClass;
  s: string;
  Raised: Boolean = False;
const
  Expect = '"unknown data"';
begin
  s := PdoToJson(TypeInfo(c), c);
  AssertTrue(s = Expect);
  try
    s := PdoToJson(TypeInfo(c), c, True);
  except
    on e: EJsonExport do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoToJson.TestClass;
var
  o: TObject = nil;
  s: string;
const
  Expect = 'null';
begin
  s := PdoToJson(TypeInfo(o), o);
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.TestClass1;
var
  o: TMyItem = nil;
  s: string;
const
  Expect = '{"name":"just a name","value":42}';
begin
  o := TMyItem.Create(nil);
  o.Name := 'just a name';
  o.Value := 42;
  s := PdoToJson(TypeInfo(o), o);
  o.Free;
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.UnregisteredClass;
var
  o: TMyClass = nil;
  s: string;
const
  Expect = '{}';
begin
  o := TMyClass.Create;
  o.Name := 'just a name';
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
  c := TCollection.Create(TMyItem);
  with TMyItem(c.Add) do
    begin
      Name := 'just a name';
      Value := 42;
    end;
  with TMyItem(c.Add) do
    begin
      Name := 'another name';
      Value := 1001;
    end;
  s := PdoToJson(TypeInfo(c), c);
  c.Free;
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.CustomRecordProc;
var
  r: TSimple;
  s: string;
const
  Expect = '{"field a":42,"field b":"string value"}';
begin
  AssertTrue(RegisterRecordJsonProc(TypeInfo(r), @TSimple.WriteJson));
  r.a := 42;
  r.b := 'string value';
  s := PdoToJson(TypeInfo(r), r);
  AssertTrue(UnRegisterPdo(TypeInfo(r)));
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.CustomObjectProc;
var
  o: TMyObj;
  s: string;
const
  Expect = '{"name":"just name","value":42,"flag":true}';
begin
  AssertTrue(RegisterObjectJsonProc(TypeInfo(o), @TMyObj.WriteJson));
  o.Name := 'just name';
  o.Value := 42;
  o.Flag := True;
  s := PdoToJson(TypeInfo(o), o);
  AssertTrue(UnRegisterPdo(TypeInfo(o)));
  AssertTrue(s = Expect);
end;

procedure TTestPdoToJson.UnregisteredObject;
var
  o: TMyObj;
  s: string;
  Raised: Boolean = False;
const
  Expect = '"unknown data"';
begin
  o.Name := 'just name';
  o.Value := 42;
  o.Flag := True;
  s := PdoToJson(TypeInfo(o), o);
  AssertTrue(s = Expect);
  try
    s := PdoToJson(TypeInfo(o), o, True);
  except
    on e: EJsonExport do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoToJson.Strings;
var
  sl: TStringList;
  s: string;
const
  Expect = '["one","two","three","four"]';
begin
  sl := TStringList.Create;
  sl.AddStrings(['one','two','three','four']);
  s := PdoToJson(TypeInfo(sl), sl);
  sl.Free;
  AssertTrue(s = Expect);
end;

initialization
  RegisterTest(TTestPdoRegister);
  RegisterTest(TTestPdoToJson);
end.

