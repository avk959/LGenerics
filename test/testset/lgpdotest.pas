unit lgPdoTest;

{$MODE OBJFPC}{$H+}{$OPTIMIZATION NOORDERFIELDS}
{$MODESWITCH ADVANCEDRECORDS}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 5027 off : Local variable "$1" is assigned but never used}
{$WARN 5089 off : Local variable "$1" of a managed type does not seem to be initialized}
interface

uses
  Classes, SysUtils, Variants, fpcunit, testregistry,
  lgUtils, lgHelpers, lgPdo, lgArrayHelpers, lgJson, lgMsgPack, lgMsgPackUtils,
  lgVector, LgHashMap;

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
      class function LoadJson(p: Pointer; aReader: TJsonReader; const aOptions: TJsonReadOptions): Boolean; static;
    end;

  published
    procedure Unregistered;
    procedure UnregisteredDynArray;
    procedure UnregisteredArray;
    procedure UnregisteredArray2D;
    procedure UnregisteredDynArray2D;
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
    procedure LoadArrayRecordFields;
  end;

  { TTestPdoMsgPack }

  TTestPdoMsgPack = class(TTestCase)
  private
  type
    TMyRec = record
      MyInt: Integer;
      MyBoolean: Boolean;
      MyStr: string;
      MyCur: Currency;
      MyQWord: QWord;
      constructor Make(i: Integer; b: Boolean; s: string; c: Currency; q: QWord);
      class operator = (const L, R: TMyRec): Boolean;
    end;

    TColItem = class(TCollectionItem)
    private
      FName: string;
      FValue: Variant;
    public
      function Equals(o: TObject): Boolean; override;
    published
      property Name: string read FName write FName;
      property Value: Variant read FValue write FValue;
    end;

  published
    procedure TestShortInt;
    procedure TestByte;
    procedure TestSmallInt;
    procedure TestWord;
    procedure TestLongInt;
    procedure TestDWord;
    procedure TestInt64;
    procedure TestQWord;
    procedure TestChar;
    procedure TestEnumeration;
    procedure TestSingle;
    procedure TestDouble;
    procedure TestCurrency;
    procedure TestSet;
    procedure TestCharSet;
    procedure TestByteSet;
    procedure TestRangeSet1;
    procedure TestRangeSet2;
    procedure TestRangeSet4;
    procedure TestRangeSet5;
    procedure TestRangeSet6;
    procedure TestRangeSet7;
    procedure TestRangeSet31;
    procedure TestShortString;
    procedure TestString;
    procedure TestWString;
    procedure TestVariant;
    procedure TestArray;
    procedure TestArray2;
    procedure TestRecord;
    procedure TestStrings;
    procedure TestCollection;
    procedure TestWChar;
    procedure TestDynArray;
    procedure TestDynArray2;
    procedure TestBoolean;
    procedure TestUString;
  end;

  { TTestMsgPackExt }

  TTestMsgPackExt = class(TTestCase)
  private
  type
    TMpExt      = specialize TGAutoRef<TMpUserExt>;
    TOptString  = specialize TGOptional<string>;
    TOptGuid    = specialize TGOptional<TGuid>;
    TIntVector  = specialize TGLiteVector<Integer>;
    TGuidVector = specialize TGLiteVector<TGuid>;

    TStrGuidMapExt = specialize TGLiteStrHashMapExt<TGuid>;
    TStrGuidMap    = TStrGuidMapExt.TExtValue;
    TIntGuidMapExt = specialize TGLiteIntHashMapExt<TGuid>;
    TIntGuidMap    = TIntGuidMapExt.TExtValue;
    TGuidTxtMapExt = specialize TGLiteChainHashMapExt<TGuid, TSimpleText, TGuid>;
    TGuidTxtMap    = TGuidTxtMapExt.TExtValue;
  published
    procedure TestGuidExt;
    procedure TestOptStringExt;
    procedure TestOptGuidExt;
    procedure TestIntVectorExt;
    procedure TestGuidVectorExt;
    procedure TestStrGuidMapExt;
    procedure TestIntGuidMapExt;
    procedure TestTextCompressExt;
    procedure TestGuidTxtMap;
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
  AssertFalse(RecordFieldsRegistered(TypeInfo(r2), a));
  AssertTrue(a = nil);
  a := ['dummy'];
  AssertFalse(RecordFieldsRegistered(TypeInfo(r1), a));
  AssertTrue(a = nil);
  a := ['one', 'two', 'three','four'];
  AssertTrue(RegisterRecordFields(TypeInfo(r1), a));
  AssertFalse(RecordFieldsRegistered(TypeInfo(r2), a));
  AssertTrue(a = nil);
  AssertTrue(RecordFieldsRegistered(TypeInfo(r1), a));
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
  Expect = '[2147483647,true,true,"just string","short string",9007199254740991]';
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
  Expect = '[[42,"str1"],[1001,"str2"]]';
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
  Expect = '[[42,"str1"],[1001,"str2"]]';
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
  Expect = '[[42,"str1"],[1001,"str2"],[12,"str3"],[1024,"str4"]]';
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
  Expect = '[[[42,"str1"],[1001,"str2"]],[[12,"str3"],[1024,"str4"]]]';
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
begin
  v := I;
  try
    s := PdoToJson(TypeInfo(v), v);
  except
    on e: EPdoStoreJson do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoToJson.UnsupportType;
var
  c: TMyItemClass;
  I: IInterface;
  s: string;
  Raised: Boolean = False;
begin
  try
    s := PdoToJson(TypeInfo(c), c);
  except
    on e: EPdoStoreJson do
      Raised := True;
  end;
  AssertTrue(Raised);
  Raised := False;
  try
    s := PdoToJson(TypeInfo(I), I);
  except
    on e: EPdoStoreJson do
      Raised := True;
  end;
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
  AssertTrue(RegisterPdoToJsonProc(TypeInfo(r), @TSimple.WriteJson));
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
  AssertTrue(RegisterPdoToJsonProc(TypeInfo(o), @TMyObj.WriteJson));
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
begin
  o.Name := 'just name';
  o.Value := 42;
  o.Flag := True;
  try
    s := PdoToJson(TypeInfo(o), o);
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

class function TTestPdoLoadJson.TTestRec.LoadJson(p: Pointer; aReader: TJsonReader;
  const aOptions: TJsonReadOptions): Boolean;
type
  PTestRec = ^TTestRec;
var
  pRec: PTestRec absolute p;
  d: Double = 0;
  I: Int64 = 0;
  KeyFound, ValueFound: Boolean;
begin
  if aReader.TokenKind <> rtkObjectBegin then exit(False);
  KeyFound := False;
  ValueFound:= False;
  repeat
    aReader.Read;
    if aReader.TokenKind = rtkObjectEnd then break;
    if jroIgnoreNameCase in aOptions then
      case LowerCase(aReader.Name) of
        'key':
          begin
            if aReader.TokenKind = rtkString then
              pRec^.Key := aReader.AsString
            else
              if (aReader.TokenKind = rtkNull) and not(jroRejectNulls in aOptions)then
                pRec^.Key := ''
              else exit(False);
            KeyFound := True;
          end;
        'value':
          if aReader.TokenKind = rtkNumber then
            begin
              d := aReader.AsNumber;
              ValueFound:= True;
            end
          else exit(False);
      else
        if not(jroSkipUnknownProps in aOptions) then exit(False);
      end
    else
      case aReader.Name of
        'Key':
          begin
            if aReader.TokenKind = rtkString then
              pRec^.Key := aReader.AsString
            else
              if (aReader.TokenKind = rtkNull) and not(jroRejectNulls in aOptions)then
                pRec^.Key := ''
              else exit(False);
            KeyFound := True;
          end;
        'Value':
          if aReader.TokenKind = rtkNumber then
            begin
              d := aReader.AsNumber;
              ValueFound:= True;
            end
          else exit(False);
      else
        if not(jroSkipUnknownProps in aOptions) then exit(False);
      end;
  until False;
  if not(KeyFound and ValueFound) then exit(False);
  if not Double.IsExactInt(d, I) then exit(False);
  if (I < Low(Integer)) or (I > High(Integer)) then
    exit(False);
  pRec^.Value := I;
  Result := True;
end;

{ TTestPdoLoadJson }

procedure TTestPdoLoadJson.Unregistered;
var
  r: TMyRec;
const
  JSON = '[2147483647,true,"just string","short str","meThree"]';
begin
  r := Default(TMyRec);
  PdoLoadJson(TypeInfo(r), r, JSON);
  AssertTrue(r.IntValue = 2147483647);
  AssertTrue(r.BoolValue);
  AssertTrue(r.StrValue = 'just string');
  AssertTrue(r.SStrValue = 'short str');
  AssertTrue(r.EnumValue = meThree);
end;

procedure TTestPdoLoadJson.UnregisteredDynArray;
var
  a: array of TTestRec = nil;
const
  JSON = '[["foo",42],["bar",1001],["baz",777]]';
begin
  PdoLoadJson(TypeInfo(a), a, JSON);
  AssertTrue(Length(a) = 3);
  AssertTrue(a[0].Key = 'foo');
  AssertTrue(a[0].Value = 42);
  AssertTrue(a[1].Key = 'bar');
  AssertTrue(a[1].Value = 1001);
  AssertTrue(a[2].Key = 'baz');
  AssertTrue(a[2].Value = 777);
end;

procedure TTestPdoLoadJson.UnregisteredArray;
type
  TArray = array[1..3]of TTestRec;
var
  a: TArray;
const
  JSON = '[["foo",42],["bar",1001],["baz",777]]';
begin
  a := Default(TArray);
  PdoLoadJson(TypeInfo(a), a, JSON);
  AssertTrue(a[1].Key = 'foo');
  AssertTrue(a[1].Value = 42);
  AssertTrue(a[2].Key = 'bar');
  AssertTrue(a[2].Value = 1001);
  AssertTrue(a[3].Key = 'baz');
  AssertTrue(a[3].Value = 777);
end;

procedure TTestPdoLoadJson.UnregisteredArray2D;
type
  TArray = array[0..1,1..2]of TTestRec;
var
  a: TArray;
const
  JSON = '[["foo",42],["bar",1001],["zoo",777],["baz",513]]';
begin
  a := Default(TArray);
  PdoLoadJson(TypeInfo(a), a, JSON);
  AssertTrue(a[0,1].Key = 'foo');
  AssertTrue(a[0,1].Value = 42);
  AssertTrue(a[0,2].Key = 'bar');
  AssertTrue(a[0,2].Value = 1001);
  AssertTrue(a[1,1].Key = 'zoo');
  AssertTrue(a[1,1].Value = 777);
  AssertTrue(a[1,2].Key = 'baz');
  AssertTrue(a[1,2].Value = 513);
end;

procedure TTestPdoLoadJson.UnregisteredDynArray2D;
var
  a: array of array of TTestRec = nil;
const
  JSON = '[[["foo",42],["bar",1001]],[["zoo",777],["baz",513]]]';
begin
  PdoLoadJson(TypeInfo(a), a, JSON);
  AssertTrue(Length(a) = 2);
  AssertTrue(Length(a[0]) = 2);
  AssertTrue(Length(a[1]) = 2);
  AssertTrue(a[0,0].Key = 'foo');
  AssertTrue(a[0,0].Value = 42);
  AssertTrue(a[0,1].Key = 'bar');
  AssertTrue(a[0,1].Value = 1001);
  AssertTrue(a[1,0].Key = 'zoo');
  AssertTrue(a[1,0].Value = 777);
  AssertTrue(a[1,1].Key = 'baz');
  AssertTrue(a[1,1].Value = 513);
end;

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
  AssertTrue(RegisterJsonToPdoProc(TypeInfo(r), @TTestRec.LoadJson));
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
  AssertTrue(RegisterJsonToPdoProc(TypeInfo(r), @TTestRec.LoadJson));
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
  AssertTrue(RegisterJsonToPdoProc(TypeInfo(r), @TTestRec.LoadJson));
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
  AssertTrue(RegisterJsonToPdoProc(TypeInfo(r), @TTestRec.LoadJson));
  try
    PdoLoadJson(TypeInfo(r), r, Json);
  except
    on e: EPdoLoadJson do
      Raised := True;
  end;
  AssertTrue(UnRegisterPdo(TypeInfo(r)));
  AssertTrue(Raised);
end;

procedure TTestPdoLoadJson.LoadArrayRecordFields;
var
  r: TMyRec;
  a: array of TMyRec = nil;
const
  Json = '[{"intValue":42,"enumValue":"meTwo","sstrValue":"ssrting9","strValue":"string1","boolValue":true},'+
         '{"enumValue":"meFive","intValue":1001,"sstrValue":"string","strValue":"string2","boolValue":false}]';
begin
  r := Default(TMyRec);
  AssertTrue(RegisterRecordFields(TypeInfo(r), ['intValue','boolValue','strValue','sstrValue','enumValue']));
  PdoLoadJson(TypeInfo(a), a, Json);
  AssertTrue(Length(a) = 2);

  AssertTrue(a[0].IntValue = 42);
  AssertTrue(a[0].BoolValue);
  AssertTrue(a[0].StrValue = 'string1');
  AssertTrue(a[0].SStrValue = 'ssrting9');
  AssertTrue(a[0].EnumValue = meTwo);

  AssertTrue(a[1].IntValue = 1001);
  AssertFalse(a[1].BoolValue);
  AssertTrue(a[1].StrValue = 'string2');
  AssertTrue(a[1].SStrValue = 'string');
  AssertTrue(a[1].EnumValue = meFive);

  AssertTrue(UnRegisterPdo(TypeInfo(r)));
end;

{ TTestPdoMsgPack.TMyRec }

constructor TTestPdoMsgPack.TMyRec.Make(i: Integer; b: Boolean; s: string; c: Currency; q: QWord);
begin
  MyInt := i;
  MyBoolean := b;
  MyStr := s;
  MyCur := c;
  MyQWord := q;
end;

class operator TTestPdoMsgPack.TMyRec.=(const L, R: TMyRec): Boolean;
begin
  Result := (L.MyInt = R.MyInt) and (L.MyBoolean = R.MyBoolean) and (L.MyStr = R.MyStr) and
            (L.MyCur = R.MyCur) and (L.MyQWord = R.MyQWord);
end;

{ TTestPdoMsgPack.TColItem }

function TTestPdoMsgPack.TColItem.Equals(o: TObject): Boolean;
begin
  if o = Self then exit(True);
  if (o = nil) or not(o is TColItem) then exit(False);
  Result := (Name = TColItem(o).Name) and (Value = TColItem(o).Value);
end;

{ TTestPdoMsgPack }

procedure TTestPdoMsgPack.TestShortInt;
var
  v1, v2: ShortInt;
  b: TBytes;
begin
  v1 := Low(ShortInt);
  v2 := High(ShortInt);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = Low(ShortInt));

  v1 := High(ShortInt);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = High(ShortInt));
end;

procedure TTestPdoMsgPack.TestByte;
var
  v1, v2: Byte;
  b: TBytes;
begin
  v1 := 0;
  v2 := High(Byte);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = 0);

  v1 := High(Byte);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = High(Byte));
end;

procedure TTestPdoMsgPack.TestSmallInt;
var
  v1, v2: SmallInt;
  b: TBytes;
begin
  v1 := Low(SmallInt);
  v2 := High(SmallInt);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = Low(SmallInt));

  v1 := High(SmallInt);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = High(SmallInt));
end;

procedure TTestPdoMsgPack.TestWord;
var
  v1, v2: Word;
  b: TBytes;
begin
  v1 := 0;
  v2 := High(Word);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = 0);

  v1 := High(Word);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = High(Word));
end;

procedure TTestPdoMsgPack.TestLongInt;
var
  v1, v2: LongInt;
  b: TBytes;
begin
  v1 := Low(LongInt);
  v2 := High(LongInt);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = Low(LongInt));

  v1 := High(LongInt);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = High(LongInt));
end;

procedure TTestPdoMsgPack.TestDWord;
var
  v1, v2: DWord;
  b: TBytes;
begin
  v1 := 0;
  v2 := High(DWord);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = 0);

  v1 := High(DWord);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = High(DWord));
end;

procedure TTestPdoMsgPack.TestInt64;
var
  v1, v2: Int64;
  b: TBytes;
begin
  v1 := Low(Int64);
  v2 := High(Int64);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = Low(Int64));

  v1 := High(Int64);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = High(Int64));
end;

procedure TTestPdoMsgPack.TestQWord;
var
  v1, v2: QWord;
  b: TBytes;
begin
  v1 := 0;
  v2 := High(QWord);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = 0);

  v1 := High(QWord);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = High(QWord));
end;

procedure TTestPdoMsgPack.TestChar;
var
  v1, v2: AnsiChar;
  b: TBytes;
begin
  v1 := 'A';
  v2 := '0';
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = 'A');

  v1 := '0';
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = '0');
end;

procedure TTestPdoMsgPack.TestEnumeration;
var
  v1, v2: TMyEnum;
  b: TBytes;
begin
  v1 := meZero;
  v2 := meFive;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = meZero);

  v1 := meSix;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = meSix);
end;

procedure TTestPdoMsgPack.TestSingle;
var
  v1, v2: Single;
  b: TBytes;
begin
  v1 := Single.MinValue;
  v2 := Single.MaxValue;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = Single.MinValue);

  v1 := Single.MaxValue;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = Single.MaxValue);
end;

procedure TTestPdoMsgPack.TestDouble;
var
  v1, v2: Double;
  b: TBytes;
begin
  v1 := Double.MinValue;
  v2 := Double.MaxValue;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = Double.MinValue);

  v1 := Double.MaxValue;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = Double.MaxValue);
end;

procedure TTestPdoMsgPack.TestCurrency;
var
  v1, v2: Currency;
  b: TBytes;
begin
  v1 := Currency.MinValue;
  v2 := Currency.MaxValue;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = Currency.MinValue);

  v1 := Currency.MaxValue;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = Currency.MaxValue);
end;

procedure TTestPdoMsgPack.TestSet;
type
  TMySet = set of TMyEnum;
var
  v1, v2: TMySet;
  b: TBytes;
begin
  v1 := [];
  v2 := [meOne, meTwo, meFour, meFive];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = []);

  v1 := [meOne, meTwo, meFour, meFive];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = [meOne, meTwo, meFour, meFive]);
end;

procedure TTestPdoMsgPack.TestCharSet;
var
  v1, v2: set of AnsiChar;
  b: TBytes;
begin
  v1 := [];
  v2 := [#0, 'a', #255];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = []);

  v1 := [#0, ' ', '7', 'E', 'a', #255];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = [#0, ' ', '7', 'E', 'a', #255]);
end;

procedure TTestPdoMsgPack.TestByteSet;
var
  v1, v2: set of Byte;
  b: TBytes;
begin
  v1 := [];
  v2 := [1,5,42,77,101,202,255];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = []);

  v1 := [1,5,42,77,101,202,255];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = [1,5,42,77,101,202,255]);
end;

{$PUSH}{$PACKSET 1}
procedure TTestPdoMsgPack.TestRangeSet1;
type
  TSet = set of 0..7;
var
  v1, v2: TSet;
  b: TBytes;
begin
  AssertTrue(SizeOf(v1) = 1);
  v1 := [];
  v2 := [1,7];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = []);

  v1 := [1,5,7];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = [1,5,7]);
end;

procedure TTestPdoMsgPack.TestRangeSet2;
type
  TSet = set of 0..15;
var
  v1, v2: TSet;
  b: TBytes;
begin
  AssertTrue(SizeOf(v1) = 2);
  v1 := [];
  v2 := [1,11];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = []);

  v1 := [0,1,5,7,11,15];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = [0,1,5,7,11,15]);
end;

procedure TTestPdoMsgPack.TestRangeSet4;
type
  TSet = set of 0..25;
var
  v1, v2: TSet;
  b: TBytes;
begin
  AssertTrue(SizeOf(v1) = 4);
  v1 := [];
  v2 := [1,21];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = []);

  v1 := [0,1,5,7,11,25];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = [0,1,5,7,11,25]);
end;

procedure TTestPdoMsgPack.TestRangeSet5;
type
  TSet = set of 0..39;
var
  v1, v2: TSet;
  b: TBytes;
begin
  AssertTrue(SizeOf(v1) = 5);
  v1 := [];
  v2 := [1,31];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = []);

  v1 := [0,1,5,7,11,25,37];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = [0,1,5,7,11,25,37]);
end;

procedure TTestPdoMsgPack.TestRangeSet6;
type
  TSet = set of 0..47;
var
  v1, v2: TSet;
  b: TBytes;
begin
  AssertTrue(SizeOf(v1) = 6);
  v1 := [];
  v2 := [1,31];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = []);

  v1 := [0,1,5,7,11,25,37,45];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = [0,1,5,7,11,25,37,45]);
end;

procedure TTestPdoMsgPack.TestRangeSet7;
type
  TSet = set of 0..55;
var
  v1, v2: TSet;
  b: TBytes;
begin
  AssertTrue(SizeOf(v1) = 7);
  v1 := [];
  v2 := [1,51];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = []);

  v1 := [0,1,5,7,11,25,37,45,53];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = [0,1,5,7,11,25,37,45,53]);
end;

procedure TTestPdoMsgPack.TestRangeSet31;
type
  TSet = set of 0..247;
var
  v1, v2: TSet;
  b: TBytes;
begin
  AssertTrue(SizeOf(v1) = 31);
  v1 := [];
  v2 := [1,241];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = []);

  v1 := [0,1,5,7,11,25,37,45,53,245];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = [0,1,5,7,11,25,37,45,53,245]);
end;
{$POP}

procedure TTestPdoMsgPack.TestShortString;
var
  v1, v2: string[11];
  s: string;
  b: TBytes;
  Raised: Boolean = False;
begin
  v1 := '';
  v2 := 'abcd';
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = '');

  v1 := 'abcdefghijk';
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = 'abcdefghijk');

  s := v1 + ' ';
  b := PdoToMsgPack(TypeInfo(s), s);
  try
    PdoLoadMsgPack(TypeInfo(v2), v2, b);
  except
    on EPdoLoadMsgPack do Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TTestPdoMsgPack.TestString;
var
  v1, v2: string;
  b: TBytes;
begin
  v1 := '';
  v2 := 'abcd';
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = '');

  v1 := 'abcdefghijk只是一组词语';
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = 'abcdefghijk只是一组词语');
end;

procedure TTestPdoMsgPack.TestWString;
var
  v1, v2: widestring;
  b: TBytes;
  s: string;
begin
  s := 'abcdefghijk只是一组词语';
  v1 := '';
  v2 := widestring(s);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = '');

  v1 := widestring(s);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = v1);
end;

procedure TTestPdoMsgPack.TestVariant;
var
  v1, v2: Variant;
  s: Single;
  d: Double;
  c: Currency;
  dt: TDateTime;
  b: TBytes;
  st: string;
  I, J: Integer;
begin
  v1 := Null;
  v2 := 42;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsNull(v2));

  v1 := SmallInt(42);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varSmallInt) and (v2 = 42));

  v1 := Integer(42);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varInteger) and (v2 = 42));

  s := 111.175;
  TVarData(v1).vType := varSingle;
  TVarData(v1).vSingle := s;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varSingle) and (v2 = s));

  d := Pi;
  v1 := d;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varDouble) and (v2 = d));

  c := 421001.777;
  v1 := c;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varCurrency) and (v2 = c));

  dt := Now;
  v1 := dt;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varDate) and (v2 = dt));

  st := 'abcdefghijk只是一组词语';

  v1 := widestring(st);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varOleStr) and (v2 = widestring(st)));

  v1 := True;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varBoolean) and Boolean(v2));

  v1 := ShortInt(42);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varShortInt) and (v2 = 42));

  v1 := Byte(42);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varByte) and (v2 = 42));

  v1 := Word(42);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varWord) and (v2 = 42));

  v1 := LongWord(42);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varLongWord) and (v2 = 42));

  v1 := High(Int64);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varInt64) and (v2 = High(Int64)));

  v1 := High(QWord);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varQWord) and (v2 = High(QWord)));

  v1 := unicodestring(st);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varOleStr) and (v2 = unicodestring(st)));

  v1 := st;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsType(v2, varString) and (v2 = st));

  v1 := VarArrayOf(['foo', False, Null, Now]);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(VarIsArray(v2));
  AssertTrue(VarArrayDimCount(v2) = 1);
  AssertTrue(VarArrayHighBound(v1, 1) - VarArrayLowBound(v1, 1) =
             VarArrayHighBound(v2, 1) - VarArrayLowBound(v2, 1));
  J := VarArrayLowBound(v1, 1);
  for I := VarArrayLowBound(v1, 1) to VarArrayHighBound(v1, 1) do begin
    AssertTrue(v1[I] = v2[J]);
    Inc(J);
  end;
end;

procedure TTestPdoMsgPack.TestArray;
type
  TArray = array[2..7] of Integer;
const
  Arr: TArray = (1,2,3,4,5,6);
var
  v1, v2: TArray;
  b: TBytes;
begin
  v1 := Default(TArray);
  v2 := Arr;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(CompareByte(v1[2], v2[2], SizeOf(v2)) = 0);

  v1 := Arr;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(CompareByte(v1[2], Arr[2], SizeOf(Arr)) = 0);
end;

procedure TTestPdoMsgPack.TestArray2;
type
  TArray = array[0..2,2..4] of Integer;
const
  Arr: TArray = ((1,2,3),(4,5,6),(7,8,9));
var
  v1, v2: TArray;
  b: TBytes;
begin
  v1 := Default(TArray);
  v2 := Arr;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(CompareByte(v1[0,2], v2[0,2], SizeOf(v2)) = 0);

  v1 := Arr;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(CompareByte(v1[0,2], Arr[0,2], SizeOf(Arr)) = 0);
end;

procedure TTestPdoMsgPack.TestRecord;
var
  v1, v2: TMyRec;
  b: TBytes;
begin
  v1 := Default(TMyRec);
  v2 := TMyRec.Make(42,False,'qwerty',1001.42,High(QWord));
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = Default(TMyRec));

  v1 := TMyRec.Make(42,False,'qwerty',1001.42,High(QWord));
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = v1);
end;

procedure TTestPdoMsgPack.TestStrings;
var
  v1, v2: specialize TGAutoRef<TStringList>;
  sl: TStringList;
  b: TBytes;
  I: Integer;
begin
  sl := nil;
  b := PdoToMsgPack(TypeInfo(sl), sl);
  sl := TStringList.Create;
  PdoLoadMsgPack(TypeInfo(sl), sl, b);
  AssertTrue(sl = nil);

  v1.Instance.AddStrings(['111','222','333','444']);
  b := PdoToMsgPack(TypeInfo(v1.Instance), v1.Instance);
  PdoLoadMsgPack(TypeInfo(sl), sl, b);
  v2.Instance := sl;
  sl := nil;
  AssertTrue(v1.Instance.Count = v2.Instance.Count);
  for I := 0 to Pred(v1.Instance.Count) do
    AssertTrue(v1.Instance[I] = v2.Instance[I]);
end;

procedure TTestPdoMsgPack.TestCollection;
var
  v1, v2: TCollection;
  b: TBytes;
  I: Integer;
begin
  v1 := nil;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  v2 := TCollection.Create(TColItem);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = nil);

  v1 := TCollection.Create(TColItem);
  with TColItem(v1.Add) do begin
    Name := 'Name1';
    Value := 1001;
  end;
  with TColItem(v1.Add) do begin
    Name := 'Just name';
    Value := 'value';
  end;
  with TColItem(v1.Add) do begin
    Name := 'Unnamed';
    Value := Low(Int64);
  end;

  b := PdoToMsgPack(TypeInfo(v1), v1);
  v2 := TCollection.Create(TColItem);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v1.Count = v2.Count);
  for I := 0 to Pred(v1.Count) do
    AssertTrue(v1.Items[I].Equals(v2.Items[I]));
  v1.Free;
  v2.Free;
end;

procedure TTestPdoMsgPack.TestWChar;
var
  v1, v2: WideChar;
  b: TBytes;
begin
  v1 := #$0410;
  v2 := #$00;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = #$0410);

  v1 := #$00;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = #$00);
end;

procedure TTestPdoMsgPack.TestDynArray;
var
  v1, v2: array of Integer;
  sa1, sa2: TStringArray;
  b: TBytes;
  I: Integer;
begin
  v1 := nil;
  v2 := [42];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = nil);

  v1 := [1,2,3,4,5];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(Length(v1) = Length(v2));
  for I := 0 to High(v1) do
    AssertTrue(v1[I] = v2[I]);

  sa1 := nil;
  sa2 := ['111','222'];
  b := PdoToMsgPack(TypeInfo(sa1), sa1);
  PdoLoadMsgPack(TypeInfo(sa2), sa2, b);
  AssertTrue(sa2 = nil);

  sa1 := ['aaa','bbb','ccc','ddd'];
  b := PdoToMsgPack(TypeInfo(sa1), sa1);
  PdoLoadMsgPack(TypeInfo(sa2), sa2, b);
  AssertTrue(Length(sa1) = Length(sa2));
  for I := 0 to High(sa1) do
    AssertTrue(sa1[I] = sa2[I]);
end;

procedure TTestPdoMsgPack.TestDynArray2;
var
  v1, v2: array of array of Integer;
  b: TBytes;
  I, J: Integer;
begin
  v1 := nil;
  v2 := [[42],[1001]];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = nil);

  v1 := [[1,2,3,4],[5,6,7,8,9],[0,1,3],[7,6,5,4,3]];
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(Length(v1) = Length(v2));
  for I := 0 to High(v1) do begin
    AssertTrue(Length(v1[I]) = Length(v2[I]));
    for J := 0 to High(v1[I]) do
      AssertTrue(v1[I,J] = v2[I,J]);
  end;
end;

procedure TTestPdoMsgPack.TestBoolean;
var
  v1, v2: Boolean;
  b: TBytes;
begin
  v1 := False;
  v2 := True;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertFalse(v2);

  v1 := True;
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2);
end;

procedure TTestPdoMsgPack.TestUString;
var
  v1, v2: unicodestring;
  b: TBytes;
  s: string;
begin
  s := 'abcdefghijk只是一组词语';
  v1 := '';
  v2 := unicodestring(s);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = '');

  v1 := unicodestring(s);
  b := PdoToMsgPack(TypeInfo(v1), v1);
  PdoLoadMsgPack(TypeInfo(v2), v2, b);
  AssertTrue(v2 = v1);
end;

{ TTestMsgPackExt }

procedure TTestMsgPackExt.TestGuidExt;
var
  v1, v2: TGuid;
  ext: TMpExt;
  b: TBytes;
begin
  v1 := TGuid.NewGuid;
  v2 := TGuid.NewGuid;
  AssertFalse(TGuid.Equal(v1, v2));
  AssertTrue(ext.Instance.TryAddHook(TGuidExt.Create(0)));
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertTrue(TGuid.Equal(v1, v2));
end;

procedure TTestMsgPackExt.TestOptStringExt;
var
  v1, v2: TOptString;
  ext: TMpExt;
  b: TBytes;
begin
  AssertTrue(ext.Instance.TryAddHook(specialize TGOptionalExt<string>.Create(0)));
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  v2 := 'abcd';
  AssertTrue(v2.Assigned);
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertFalse(v2.Assigned);

  v1 := 'string';
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertTrue(v2.Assigned);
  AssertTrue(v1.Value = v2.Value);
end;

procedure TTestMsgPackExt.TestOptGuidExt;
var
  v1, v2: TOptGuid;
  ext: TMpExt;
  b: TBytes;
begin
  ext.Instance := TMpUserExt.Create([TGuidExt.Create(0), specialize TGOptionalExt<string>.Create(1)]);
  v2 := TGuid.NewGuid;
  AssertTrue(v2.Assigned);
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertFalse(v2.Assigned);

  v1 := TGuid.NewGuid;
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertTrue(v2.Assigned);
  AssertTrue(TGuid.Equal(v1.Value, v2.Value));
end;

procedure TTestMsgPackExt.TestIntVectorExt;
var
  v1, v2: TIntVector;
  ext: TMpExt;
  b: TBytes;
  I: Integer;
begin
  AssertTrue(ext.Instance.TryAddHook(specialize TGLiteVectorExt<Integer>.Create(0)));
  v2.AddAll([1,2,3]);
  AssertTrue(v2.Count = 3);
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertTrue(v2.IsEmpty);

  v2.AddAll([1,2,3,4,5,6]);
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertTrue(v1.Count = v2.Count);
  for I := 0 to Pred(v1.Count) do
    AssertTrue(v1[I] = v2[I]);
end;

procedure TTestMsgPackExt.TestGuidVectorExt;
var
  v1, v2: TGuidVector;
  ext: TMpExt;
  b: TBytes;
  I: Integer;
begin
  ext.Instance := TMpUserExt.Create([TGuidExt.Create(0), specialize TGLiteVectorExt<TGuid>.Create(1)]);
  v2.AddAll([TGuid.NewGuid, TGuid.NewGuid]);
  AssertTrue(v2.Count = 2);
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertTrue(v2.IsEmpty);

  v1.AddAll([TGuid.NewGuid, TGuid.NewGuid, TGuid.NewGuid, TGuid.NewGuid]);
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertTrue(v1.Count = v2.Count);
  for I := 0 to Pred(v1.Count) do
    AssertTrue(TGuid.Equal(v1[I], v2[I]));
end;

procedure TTestMsgPackExt.TestStrGuidMapExt;
type
  TEntry = TStrGuidMap.TEntry;
var
  v1, v2: TStrGuidMap;
  ext: TMpExt;
  b: TBytes;
  e: TEntry;
  g: TGuid;
begin
  ext.Instance := TMpUserExt.Create([TGuidExt.Create(0), TStrGuidMapExt.Create(1)]);
  v2.Add('aaa', TGuid.NewGuid);
  AssertTrue(v2.Count = 1);
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertTrue(v2.IsEmpty);

  v1.AddAll(
    [TEntry.Create('aa',TGuid.NewGuid), TEntry.Create('bb',TGuid.NewGuid),
     TEntry.Create('cc',TGuid.NewGuid), TEntry.Create('dd',TGuid.NewGuid)
  ]);
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertTrue(v1.Count = v2.Count);
  for e in v1 do begin
    AssertTrue(v2.TryGetValue(e.Key, g));
    AssertTrue(TGuid.Equal(e.Value, g));
  end;
end;

procedure TTestMsgPackExt.TestIntGuidMapExt;
type
  TEntry = TIntGuidMap.TEntry;
var
  v1, v2: TIntGuidMap;
  ext: TMpExt;
  b: TBytes;
  e: TEntry;
  g: TGuid;
begin
  ext.Instance := TMpUserExt.Create([TGuidExt.Create(0), TIntGuidMapExt.Create(1)]);
  v2.Add(111, TGuid.NewGuid);
  AssertTrue(v2.Count = 1);
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertTrue(v2.IsEmpty);

  v1.AddAll(
    [TEntry.Create(111,TGuid.NewGuid), TEntry.Create(222,TGuid.NewGuid),
     TEntry.Create(333,TGuid.NewGuid), TEntry.Create(444,TGuid.NewGuid)
  ]);
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertTrue(v1.Count = v2.Count);
  for e in v1 do begin
    AssertTrue(v2.TryGetValue(e.Key, g));
    AssertTrue(TGuid.Equal(e.Value, g));
  end;
end;

procedure TTestMsgPackExt.TestTextCompressExt;
var
  v1, v2: TSimpleText;
  ext: TMpExt;
  b: TBytes;
const
  TXT = 'MessagePack is an object serialization specification like JSON.' +
        'MessagePack has two concepts: type system and formats.' +
        'Serialization is conversion from application objects into MessagePack formats via MessagePack type system.' +
        'Deserialization is conversion from MessagePack formats into application objects via MessagePack type system.' +
        'This document describes the MessagePack type system, MessagePack formats and conversion of them.' +
        'MessagePack allows applications to define application-specific types using the Extension type. Extension type consists of an integer and a byte' +
        'array where the integer represents a kind of types and the byte array represents data.' +
        'Applications can assign 0 to 127 to store application-specific type information. An example usage is that application defines type = 0 as' +
        'the application''s unique type system, and stores name of a type and values of the type at the payload.';
begin
  ext.Instance := TMpUserExt.Create([TTextCompressExt.Create(0)]);
  v2 := 'abcdef';
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertTrue(string(v2) = '');

  v1 := TXT;
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  AssertTrue(Length(b) < Length(TXT));
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertTrue(string(v2) = TXT);
end;

procedure TTestMsgPackExt.TestGuidTxtMap;
type
  TEntry = TGuidTxtMap.TEntry;
var
  v1, v2: TGuidTxtMap;
  ext: TMpExt;
  b: TBytes;
  e: TEntry;
  txt: TSimpleText;
begin
  ext.Instance := TMpUserExt.Create(
    [TGuidExt.Create(0), TTextCompressExt.Create(1), TGuidTxtMapExt.Create(2)]);
  v2.Add(TGuid.NewGuid, 'aaa');
  AssertTrue(v2.Count = 1);
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertTrue(v2.IsEmpty);

  v1.AddAll(
    [TEntry.Create(TGuid.NewGuid, 'aaa'), TEntry.Create(TGuid.NewGuid, 'bbb'),
     TEntry.Create(TGuid.NewGuid, 'ccc'), TEntry.Create(TGuid.NewGuid, 'ddd')
  ]);
  b := PdoToMsgPack(TypeInfo(v1), v1, ext.Instance);
  PdoLoadMsgPack(TypeInfo(v2), v2, b, ext.Instance);
  AssertTrue(v1.Count = v2.Count);
  for e in v1 do begin
    AssertTrue(v2.TryGetValue(e.Key, txt));
    AssertTrue(string(e.Value) = string(txt));
  end;
end;

initialization
  RegisterTest(TTestPdoRegister);
  RegisterTest(TTestPdoToJson);
  RegisterTest(TTestPdoLoadJson);
  RegisterTest(TTestPdoMsgPack);
  RegisterTest(TTestMsgPackExt);
end.

