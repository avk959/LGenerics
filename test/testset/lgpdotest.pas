unit lgPdoTest;

{$MODE OBJFPC}{$H+}{$OPTIMIZATION NOORDERFIELDS}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Variants, fpcunit, testregistry, lgPdo, lgArrayHelpers, lgJson;

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

  TMyEnum = (meZero, meOne, meTwo, meThree, meFour, meFive, meSix);

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

  { TTestPdoLoadJson }

  TTestPdoLoadJson = class(TTestCase)
  private
  type
    TMySet    = set of TMyEnum;
    TByteSet  = set of Byte;
    TSmallSet = set of 0..31;

    TMyItem = class(TCollectionItem)
    private
      FName: string;
      FValue: Integer;
    published
      property name: string read FName write FName;
      property value: Integer read FValue write FValue;
    end;

    TMyClass = class(TPersistent)
    private
      FName: string;
      FValue: TMyClass;
    public
      destructor Destroy; override;
    published
      property name: string read FName write FName;
      property value: TMyClass read FValue write FValue;
    end;

    TMyRec = record
      IntValue: Integer;
      BoolValue: Boolean;
      StrValue: string;
      SStrValue: string[10];
      EnumValue: TMyEnum;
    end;

    TTestRec = record
      Key: string;
      Value: Integer;
      class procedure LoadJson(p: Pointer; aReader: TJsonReader; const aOptions: TJsonReadOptions); static;
    end;

  published
    procedure LoadChar;
    procedure LoadCharError;
    procedure LoadCharError1;
    procedure LoadCharError2;
    procedure LoadWChar;
    procedure LoadWCharError;
    procedure LoadWCharError1;
    procedure LoadInteger;
    procedure LoadIntegerError;
    procedure LoadIntegerError1;
    procedure LoadIntegerError2;
    procedure LoadInt64;
    procedure LoadInt64Error;
    procedure LoadInt64Error1;
    procedure LoadSString;
    procedure LoadSString1;
    procedure LoadSStringError;
    procedure LoadSStringError1;
    procedure LoadString;
    procedure LoadString1;
    procedure LoadString2;
    procedure LoadStringError;
    procedure LoadVariantNull;
    procedure LoadVariantBool;
    procedure LoadVariantInt;
    procedure LoadVariantFloat;
    procedure LoadVariantStr;
    procedure LoadVariantArray;
    procedure LoadVariantError;
    procedure LoadEnum;
    procedure LoadEnumError;
    procedure LoadNullSet;
    procedure LoadEnumSet;
    procedure LoadEnumSetError;
    procedure LoadCharSet;
    procedure LoadCharSetError;
    procedure LoadByteSet;
    procedure LoadByteSetError;
    procedure LoadStringsNull;
    procedure LoadStringsEmpty;
    procedure LoadStrings;
    procedure LoadStrings1;
    procedure LoadStringsError;
    procedure LoadCollectionNull;
    procedure LoadCollection;
    procedure LoadCollectionSkipProp;
    procedure LoadCollectionUnknownPropError;
    procedure LoadCollectionIgnoreCase;
    procedure LoadCollectionCaseError;
    procedure LoadClass;
    procedure LoadClassArray;
    procedure LoadRecordFields;
    procedure LoadRecordFieldsSkipProp;
    procedure LoadRecordFieldsIgnoreCase;
    procedure LoadRecordFieldsError;
    procedure LoadRecordFieldsLenError;
    procedure LoadRecordProc;
    procedure LoadRecordProcIgnoreCase;
    procedure LoadRecordProcSkipProp;
    procedure LoadRecordProcRangeError;
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
    on e: EPdoStoreJson do
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
    on e: EPdoStoreJson do
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
  AssertTrue(RegisterRecordStoreProc(TypeInfo(r), @TSimple.WriteJson));
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
  AssertTrue(RegisterObjectStoreProc(TypeInfo(o), @TMyObj.WriteJson));
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
    on e: EPdoStoreJson do
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

{ TTestPdoLoadJson.TMyClass }

destructor TTestPdoLoadJson.TMyClass.Destroy;
begin
  FValue.Free;
  inherited;
end;

{ TTestPdoLoadJson.TTestRec }

class procedure TTestPdoLoadJson.TTestRec.LoadJson(p: Pointer; aReader: TJsonReader; const aOptions: TJsonReadOptions);
type
  PTestRec = ^TTestRec;
var
  pRec: PTestRec absolute p;
  d: Double = 0;
  I: Int64 = 0;
  KeyFound, ValueFound: Boolean;
begin
  if aReader.TokenKind <> tkObjectBegin then
    raise EPdoLoadJson.Create('Unexpected TokenKind on begin record');
  KeyFound := False;
  ValueFound:= False;
  repeat
    aReader.Read;
    if aReader.TokenKind = tkObjectEnd then break;
    if jroIgnoreNameCase in aOptions then
      case LowerCase(aReader.Name) of
        'key':
          begin
            if aReader.TokenKind = tkString then
              pRec^.Key := aReader.AsString
            else
              if (aReader.TokenKind = tkNull) and not(jroRejectNulls in aOptions)then
                pRec^.Key := ''
              else
                raise EPdoLoadJson.Create('Unexpected TokenKind when read Key');
            KeyFound := True;
          end;
        'value':
          if aReader.TokenKind = tkNumber then
            begin
              d := aReader.AsNumber;
              ValueFound:= True;
            end
          else
            raise EPdoLoadJson.Create('Unexpected TokenKind when read Value');
      else
        if not(jroSkipUnknownProps in aOptions) then
          raise EPdoLoadJson.Create('Unknown field name');
      end
    else
      case aReader.Name of
        'Key':
          begin
            if aReader.TokenKind = tkString then
              pRec^.Key := aReader.AsString
            else
              if (aReader.TokenKind = tkNull) and not(jroRejectNulls in aOptions)then
                pRec^.Key := ''
              else
                raise EPdoLoadJson.Create('Unexpected TokenKind when read Key');
            KeyFound := True;
          end;
        'Value':
          if aReader.TokenKind = tkNumber then
            begin
              d := aReader.AsNumber;
              ValueFound:= True;
            end
          else
            raise EPdoLoadJson.Create('Unexpected TokenKind when read Value');
      else
        if not(jroSkipUnknownProps in aOptions) then
          raise EPdoLoadJson.Create('Unknown field name');
      end;
  until False;
  if not KeyFound then
    raise EPdoLoadJson.Create('Key not found');
  if not ValueFound then
    raise EPdoLoadJson.Create('Value not found');
  if not IsExactInt(d, I) then
    raise EPdoLoadJson.Create('Value is not integer');
  if (jroRangeOverflowCheck in aOptions) and ((I < Low(Integer)) or (I > High(Integer))) then
    raise EPdoLoadJson.Create('Range error');
  pRec^.Value := I;
end;

{ TTestPdoLoadJson }

procedure TTestPdoLoadJson.LoadChar;
var
  c: Char = ' ';
const
  Json = '"z"';
begin
  PdoLoadJson(TypeInfo(c), c, Json);
  AssertTrue(c = 'z');
end;

procedure TTestPdoLoadJson.LoadCharError;
var
  c: Char = ' ';
  Raised: Boolean = False;
const
  Json = '"yz"';
begin
  try
    PdoLoadJson(TypeInfo(c), c, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadCharError1;
var
  c: Char = ' ';
  Raised: Boolean = False;
const
  Json = 'null';
begin
  try
    PdoLoadJson(TypeInfo(c), c, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadCharError2;
var
  c: Char = ' ';
  Raised: Boolean = False;
const
  Json = '7';
begin
  try
    PdoLoadJson(TypeInfo(c), c, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadWChar;
var
  c: WideChar = ' ';
  Json: string;
begin
  Json := '"' + 'Ю' + '"';
  PdoLoadJson(TypeInfo(c), c, Json);
  AssertTrue(string(c) = 'Ю');
end;

procedure TTestPdoLoadJson.LoadWCharError;
var
  c: WideChar = ' ';
  Json: string;
  Raised: Boolean = False;
begin
  Json := '"' + 'ЮЯ' + '"';
  try
    PdoLoadJson(TypeInfo(c), c, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadWCharError1;
var
  c: WideChar = ' ';
  Raised: Boolean = False;
const
  Json = 'null';
begin
  try
    PdoLoadJson(TypeInfo(c), c, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadInteger;
var
  I: Integer = 0;
const
  Json = '-1001';
begin
  PdoLoadJson(TypeInfo(I), I, Json);
  AssertTrue(I = -1001);
end;

procedure TTestPdoLoadJson.LoadIntegerError;
var
  I: Integer = 0;
  Raised: Boolean = False;
const
  Json = '3.1416';
begin
  try
    PdoLoadJson(TypeInfo(I), I, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadIntegerError1;
var
  I: Integer = 0;
  Raised: Boolean = False;
const
  Json = 'null';
begin
  try
    PdoLoadJson(TypeInfo(I), I, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadIntegerError2;
var
  I: Integer = 0;
  Raised: Boolean = False;
const
  Json = '[]';
begin
  try
    PdoLoadJson(TypeInfo(I), I, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadInt64;
var
  I: Int64 = 0;
const
  Json = '-9007199254740991';
begin
  PdoLoadJson(TypeInfo(I), I, Json);
  AssertTrue(I = -9007199254740991);
end;

procedure TTestPdoLoadJson.LoadInt64Error;
var
  I: Int64 = 0;
  Raised: Boolean = False;
const
  Json = '43.1';
begin
  try
    PdoLoadJson(TypeInfo(I), I, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadInt64Error1;
var
  I: Int64 = 0;
  Raised: Boolean = False;
const
  Json = 'null';
begin
  try
    PdoLoadJson(TypeInfo(I), I, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadSString;
var
  s: string[20];
const
  Json = '"abcdefgh"';
  Expect = 'abcdefgh';
begin
  PdoLoadJson(TypeInfo(s), s, Json);
  AssertTrue(s = Expect);
end;

procedure TTestPdoLoadJson.LoadSString1;
var
  s: string[10] = 'abcd';
const
  Json = 'null';
begin
  PdoLoadJson(TypeInfo(s), s, Json);
  AssertTrue(s = '');
end;

procedure TTestPdoLoadJson.LoadSStringError;
var
  s: string[10];
  Raised: Boolean = False;
const
  Json = '"abcdefghijk"';
begin
  try
    PdoLoadJson(TypeInfo(s), s, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadSStringError1;
var
  s: string[10];
  Raised: Boolean = False;
const
  Json = '42';
begin
  try
    PdoLoadJson(TypeInfo(s), s, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadString;
var
  s: string = '';
const
  Json = '"abcdefgh"';
  Expect = 'abcdefgh';
begin
  PdoLoadJson(TypeInfo(s), s, Json);
  AssertTrue(s = Expect);
end;

procedure TTestPdoLoadJson.LoadString1;
var
  s: string = 'abcd';
const
  Json = 'null';
begin
  PdoLoadJson(TypeInfo(s), s, Json);
  AssertTrue(s = '');
end;

procedure TTestPdoLoadJson.LoadString2;
var
  s: string = 'abcd';
const
  Json = '""';
begin
  PdoLoadJson(TypeInfo(s), s, Json);
  AssertTrue(s = '');
end;

procedure TTestPdoLoadJson.LoadStringError;
var
  s: string;
  Raised: Boolean = False;
const
  Json = '{}';
begin
  try
    PdoLoadJson(TypeInfo(s), s, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadVariantNull;
var
  v: Variant;
const
  Json = 'null';
begin
  v := 42;
  PdoLoadJson(TypeInfo(v), v, Json);
  AssertTrue(VarIsNull(v));
end;

procedure TTestPdoLoadJson.LoadVariantBool;
var
  v: Variant;
const
  Json = 'true';
begin
  v := 42;
  PdoLoadJson(TypeInfo(v), v, Json);
  AssertTrue(VarIsBool(v));
  AssertTrue(v);
end;

procedure TTestPdoLoadJson.LoadVariantInt;
var
  v: Variant;
const
  Json = '42';
begin
  PdoLoadJson(TypeInfo(v), v, Json);
  AssertTrue(VarIsOrdinal(v));
  AssertTrue(v = 42);
end;

procedure TTestPdoLoadJson.LoadVariantFloat;
var
  v: Variant;
const
  Json = '3.1416';
begin
  PdoLoadJson(TypeInfo(v), v, Json);
  AssertTrue(VarIsFloat(v));
  AssertTrue(VarSameValue(v, 3.1416));
end;

procedure TTestPdoLoadJson.LoadVariantStr;
var
  v: Variant;
const
  Json = '"Just string"';
begin
  PdoLoadJson(TypeInfo(v), v, Json);
  AssertTrue(VarIsStr(v));
  AssertTrue(v = 'Just string');
end;

procedure TTestPdoLoadJson.LoadVariantArray;
var
  v: Variant;
  I, J: Integer;
const
  Json = '[42, "str", null, [], false]';
begin
  PdoLoadJson(TypeInfo(v), v, Json);
  AssertTrue(VarIsArray(v));
  AssertTrue(VarArrayDimCount(v) = 1);
  J := 0;
  for I := VarArrayLowBound(v, 1) to VarArrayHighBound(v, 1) do
    begin
      case J of
        0:
          begin
            AssertTrue(VarIsOrdinal(v[I]));
            AssertTrue(v[I] = 42);
          end;
        1:
          begin
            AssertTrue(VarIsStr(v[I]));
            AssertTrue(v[I] = 'str');
          end;
        2:
          AssertTrue(VarIsNull(v[I]));
        3:
          AssertTrue(VarIsArray(v[I]));
        4:
          begin
            AssertTrue(VarIsBool(v[I]));
            AssertFalse(v[I]);
          end;
      else
      end;
      Inc(J);
    end;
end;

procedure TTestPdoLoadJson.LoadVariantError;
var
  v: Variant;
  Raised: Boolean = False;
const
  Json = '{}';
begin
  try
    PdoLoadJson(TypeInfo(v), v, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadEnum;
var
  e: TMyEnum;
const
  Json = '"meFive"';
begin
  PdoLoadJson(TypeInfo(e), e, Json);
  AssertTrue(e = meFive);
end;

procedure TTestPdoLoadJson.LoadEnumError;
var
  e: TMyEnum;
  Raised: Boolean = False;
const
  Json = '"meSeven"';
begin
  try
    PdoLoadJson(TypeInfo(e), e, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadNullSet;
var
  s: TMySet;
const
  Json = 'null';
begin
  s := [meOne, meTwo];
  PdoLoadJson(TypeInfo(s), s, Json);
  AssertTrue(s = []);
end;

procedure TTestPdoLoadJson.LoadEnumSet;
var
  s: TMySet;
const
  Json = '["meFive", "meFour", "meZero"]';
begin
  s := [meOne, meTwo];
  PdoLoadJson(TypeInfo(s), s, Json);
  AssertTrue(s = [meZero, meFour, meFive]);
end;

procedure TTestPdoLoadJson.LoadEnumSetError;
var
  s: TMySet;
  Raised: Boolean = False;
const
  Json = '["meFive", "meFour", "meZero", "meNull"]';
begin
  try
    PdoLoadJson(TypeInfo(s), s, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadCharSet;
var
  s: TSysCharset;
const
  Json = '["x", "y", "z"]';
begin
  s := ['a', 'b'];
  PdoLoadJson(TypeInfo(s), s, Json);
  AssertTrue(s = ['x', 'y', 'z']);
end;

procedure TTestPdoLoadJson.LoadCharSetError;
var
  s: TSysCharset;
  Raised: Boolean = False;
const
  Json = '["x", "y", "z", "ab"]';
begin
  try
    PdoLoadJson(TypeInfo(s), s, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadByteSet;
var
  s: TByteSet;
const
  Json = '[1, 42, 101]';
begin
  s := [0, 7];
  PdoLoadJson(TypeInfo(s), s, Json);
  AssertTrue(s = [1, 42, 101]);
end;

procedure TTestPdoLoadJson.LoadByteSetError;
var
  s: TSysCharset;
  Raised: Boolean = False;
const
  Json = '[1, 42, 101, 1001]';
begin
  try
    PdoLoadJson(TypeInfo(s), s, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadStringsNull;
var
  sl: TStringList;
const
  Json = 'null';
begin
  sl := TStringList.Create;
  PdoLoadJson(TypeInfo(sl), sl, Json);
  AssertTrue(sl = nil);
end;

procedure TTestPdoLoadJson.LoadStringsEmpty;
var
  sl: TStringList;
const
  Json = '[]';
begin
  sl := TStringList.Create;
  sl.Add('item');
  PdoLoadJson(TypeInfo(sl), sl, Json);
  AssertTrue(sl.Count = 0);
  sl.Free;
end;

procedure TTestPdoLoadJson.LoadStrings;
var
  sl: TStringList;
const
  Json = '["one", "two", "three"]';
begin
  sl := TStringList.Create;
  sl.Add('item');
  PdoLoadJson(TypeInfo(sl), sl, Json);
  AssertTrue(sl.Count = 3);
  AssertTrue(sl[0] = 'one');
  AssertTrue(sl[1] = 'two');
  AssertTrue(sl[2] = 'three');
  sl.Free;
end;

procedure TTestPdoLoadJson.LoadStrings1;
var
  sl: TStringList = nil;
const
  Json = '["one", "two", "three"]';
begin
  PdoLoadJson(TypeInfo(sl), sl, Json, [jroTryCreateClassInst]);
  AssertTrue(sl.Count = 3);
  AssertTrue(sl[0] = 'one');
  AssertTrue(sl[1] = 'two');
  AssertTrue(sl[2] = 'three');
  sl.Free;
end;

procedure TTestPdoLoadJson.LoadStringsError;
var
  sl: TStringList;
  Raised: Boolean = False;
const
  Json = '42';
begin
  sl := TStringList.Create;
  try
    PdoLoadJson(TypeInfo(sl), sl, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  sl.Free;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadCollectionNull;
var
  c: TCollection;
const
  Json = 'null';
begin
  c := TCollection.Create(TMyItem);
  PdoLoadJson(TypeInfo(c), c, Json);
  AssertTrue(c = nil);
end;

procedure TTestPdoLoadJson.LoadCollection;
var
  c: TCollection;
  Item: TMyItem;
const
  Json = '[{"name":"name1","value":42},{"name":"name2","value":1001}]';
begin
  c := TCollection.Create(TMyItem);
  PdoLoadJson(TypeInfo(c), c, Json);
  AssertTrue(c.Count = 2);
  Item := TMyItem(c.Items[0]);
  AssertTrue((Item.Name = 'name1') and (Item.Value = 42));
  Item := TMyItem(c.Items[1]);
  AssertTrue((Item.Name = 'name2') and (Item.Value = 1001));
  c.Free;
end;

procedure TTestPdoLoadJson.LoadCollectionSkipProp;
var
  c: TCollection;
  Item: TMyItem;
const
  Json = '[{"name":"name1","value":42,"items":[1,2]},{"name":"name2","value":1001,"items":[3,4]}]';
begin
  c := TCollection.Create(TMyItem);
  PdoLoadJson(TypeInfo(c), c, Json, [jroSkipUnknownProps]);
  AssertTrue(c.Count = 2);
  Item := TMyItem(c.Items[0]);
  AssertTrue((Item.Name = 'name1') and (Item.Value = 42));
  Item := TMyItem(c.Items[1]);
  AssertTrue((Item.Name = 'name2') and (Item.Value = 1001));
  c.Free;
end;

procedure TTestPdoLoadJson.LoadCollectionUnknownPropError;
var
  c: TCollection;
  Raised: Boolean = False;
const
  Json = '[{"name":"name1","value":42,"items":[1,2]},{"name":"name2","value":1001,"items":[3,4]}]';
begin
  c := TCollection.Create(TMyItem);
  try
    PdoLoadJson(TypeInfo(c), c, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  c.Free;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadCollectionIgnoreCase;
var
  c: TCollection;
  Item: TMyItem;
const
  Json = '[{"Name":"name1","Value":42},{"Name":"name2","Value":1001}]';
begin
  c := TCollection.Create(TMyItem);
  PdoLoadJson(TypeInfo(c), c, Json, [jroIgnoreNameCase]);
  AssertTrue(c.Count = 2);
  Item := TMyItem(c.Items[0]);
  AssertTrue((Item.Name = 'name1') and (Item.Value = 42));
  Item := TMyItem(c.Items[1]);
  AssertTrue((Item.Name = 'name2') and (Item.Value = 1001));
  c.Free;
end;

procedure TTestPdoLoadJson.LoadCollectionCaseError;
var
  c: TCollection;
  Raised: Boolean = False;
const
  Json = '[{"Name":"name1","Value":42,"items":[1,2]},{"Name":"name2","Value":1001,"items":[3,4]}]';
begin
  c := TCollection.Create(TMyItem);
  try
    PdoLoadJson(TypeInfo(c), c, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  c.Free;
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadClass;
var
  c: TMyClass = nil;
const
  Json = '{"name":"root name","value":{"name":"name1","value":{"name":"name2","value":null}}}';
begin
  PdoLoadJson(TypeInfo(c), c, Json, [jroTryCreateClassInst]);
  AssertTrue(c <> nil);
  AssertTrue(c.Name = 'root name');
  AssertTrue(c.Value <> nil);
  AssertTrue(c.Value.Name = 'name1');
  AssertTrue(c.Value.Value <> nil);
  AssertTrue(c.Value.Value.Name = 'name2');
  AssertTrue(c.Value.Value.Value = nil);
  c.Free;
end;

procedure TTestPdoLoadJson.LoadClassArray;
var
  a: array of TMyClass = nil;
const
  Json = '[{"name":"root name","value":{"name":"name1","value":{"name":"name2","value":null}}},'+
         '{"name":"root name1","value":{"name":"name3","value":{"name":"name4","value":null}}}]';
begin
  PdoLoadJson(TypeInfo(a), a, Json, [jroTryCreateClassInst]);
  AssertTrue(Length(a) = 2);
  AssertTrue(a[0].Name = 'root name');
  AssertTrue(a[0].Value <> nil);
  AssertTrue(a[0].Value.Name = 'name1');
  AssertTrue(a[0].Value.Value <> nil);
  AssertTrue(a[0].Value.Value.Name = 'name2');
  AssertTrue(a[0].Value.Value.Value = nil);

  AssertTrue(a[1].Name = 'root name1');
  AssertTrue(a[1].Value <> nil);
  AssertTrue(a[1].Value.Name = 'name3');
  AssertTrue(a[1].Value.Value <> nil);
  AssertTrue(a[1].Value.Value.Name = 'name4');
  AssertTrue(a[1].Value.Value.Value = nil);
  a[0].Free;
  a[1].Free;
end;

procedure TTestPdoLoadJson.LoadRecordFields;
var
  r: TMyRec;
const
  Json = '{"intValue":42,"enumValue":"meTwo","sstrValue":"ssrting9","strValue":"string1","boolValue":true}';
begin
  r := Default(TMyRec);
  AssertTrue(RegisterRecordFields(TypeInfo(r), ['intValue','boolValue','strValue','sstrValue','enumValue']));
  PdoLoadJson(TypeInfo(r), r, Json);
  AssertTrue(r.IntValue = 42);
  AssertTrue(r.BoolValue);
  AssertTrue(r.StrValue = 'string1');
  AssertTrue(r.SStrValue = 'ssrting9');
  AssertTrue(r.EnumValue = meTwo);
  AssertTrue(UnRegisterPdo(TypeInfo(r)));
end;

procedure TTestPdoLoadJson.LoadRecordFieldsSkipProp;
var
  r: TMyRec;
const
  Json = '{"intValue":42,"enumValue":"meTwo","sstrValue":"ssrting9","strValue":"string1","boolValue":true,"anyValue":false}';
begin
  r := Default(TMyRec);
  AssertTrue(RegisterRecordFields(TypeInfo(r), ['intValue','boolValue','strValue','sstrValue','enumValue']));
  PdoLoadJson(TypeInfo(r), r, Json, [jroSkipUnknownProps]);
  AssertTrue(r.IntValue = 42);
  AssertTrue(r.BoolValue);
  AssertTrue(r.StrValue = 'string1');
  AssertTrue(r.SStrValue = 'ssrting9');
  AssertTrue(r.EnumValue = meTwo);
  AssertTrue(UnRegisterPdo(TypeInfo(r)));
end;

procedure TTestPdoLoadJson.LoadRecordFieldsIgnoreCase;
var
  r: TMyRec;
const
  Json = '{"IntValue":42,"EnUmValue":"meTwo","SstRValue":"ssrting9","sTrValue":"string1","bOOlValue":true}';
begin
  r := Default(TMyRec);
  AssertTrue(RegisterRecordFields(TypeInfo(r), ['intValue','boolValue','strValue','sstrValue','enumValue']));
  PdoLoadJson(TypeInfo(r), r, Json, [jroIgnoreNameCase]);
  AssertTrue(r.IntValue = 42);
  AssertTrue(r.BoolValue);
  AssertTrue(r.StrValue = 'string1');
  AssertTrue(r.SStrValue = 'ssrting9');
  AssertTrue(r.EnumValue = meTwo);
  AssertTrue(UnRegisterPdo(TypeInfo(r)));
end;

procedure TTestPdoLoadJson.LoadRecordFieldsError;
var
  r: TMyRec;
  Raised: Boolean = False;
const
  Json = '{"IntValue":42,"enumValue":"meTwo","sstrValue":"ssrting9","strValue":"string1","boolValue":true}';
begin
  r := Default(TMyRec);
  AssertTrue(RegisterRecordFields(TypeInfo(r), ['intValue','boolValue','strValue','sstrValue','enumValue']));
  try
    PdoLoadJson(TypeInfo(r), r, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(UnRegisterPdo(TypeInfo(r)));
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadRecordFieldsLenError;
var
  r: TMyRec;
  Raised: Boolean = False;
const
  Json = '{"intValue":42,"enumValue":"meTwo","sstrValue":"ssrting9142","strValue":"string1","boolValue":true}';
begin
  r := Default(TMyRec);
  AssertTrue(RegisterRecordFields(TypeInfo(r), ['intValue','boolValue','strValue','sstrValue','enumValue']));
  try
    PdoLoadJson(TypeInfo(r), r, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(UnRegisterPdo(TypeInfo(r)));
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadRecordProc;
var
  r: TTestRec;
const
  Json = '{"Key":"key1","Value":42}';
begin
  r := Default(TTestRec);
  AssertTrue(RegisterRecordLoadProc(TypeInfo(r), @TTestRec.LoadJson));
  PdoLoadJson(TypeInfo(r), r, Json);
  AssertTrue(UnRegisterPdo(TypeInfo(r)));
  AssertTrue(r.Key = 'key1');
  AssertTrue(r.Value = 42);
end;

procedure TTestPdoLoadJson.LoadRecordProcIgnoreCase;
var
  r: TTestRec;
const
  Json = '{"KEY":"key1","VALUE":42}';
begin
  r := Default(TTestRec);
  AssertTrue(RegisterRecordLoadProc(TypeInfo(r), @TTestRec.LoadJson));
  PdoLoadJson(TypeInfo(r), r, Json, [jroIgnoreNameCase]);
  AssertTrue(UnRegisterPdo(TypeInfo(r)));
  AssertTrue(r.Key = 'key1');
  AssertTrue(r.Value = 42);
end;

procedure TTestPdoLoadJson.LoadRecordProcSkipProp;
var
  r: TTestRec;
const
  Json = '{"Key":"key1","Blah":null,"Value":42}';
begin
  r := Default(TTestRec);
  AssertTrue(RegisterRecordLoadProc(TypeInfo(r), @TTestRec.LoadJson));
  PdoLoadJson(TypeInfo(r), r, Json, [jroSkipUnknownProps]);
  AssertTrue(UnRegisterPdo(TypeInfo(r)));
  AssertTrue(r.Key = 'key1');
  AssertTrue(r.Value = 42);
end;

procedure TTestPdoLoadJson.LoadRecordProcRangeError;
var
  r: TTestRec;
  Raised: Boolean = False;
const
  Json = '{"Key":"key1","Value":2147483648}';
begin
  r := Default(TTestRec);
  AssertTrue(RegisterRecordLoadProc(TypeInfo(r), @TTestRec.LoadJson));
  try
    PdoLoadJson(TypeInfo(r), r, Json, [jroRangeOverflowCheck]);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(UnRegisterPdo(TypeInfo(r)));
  AssertTrue(Raised);
end;

initialization
  RegisterTest(TTestPdoRegister);
  RegisterTest(TTestPdoToJson);
  RegisterTest(TTestPdoLoadJson);
end.

