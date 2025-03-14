unit LGTreeMapTest;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
{$modeswitch advancedrecords}

interface
uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGHelpers,
  LGAbstractContainer,
  LGTreeMap,
  LGHashSet,
  LGArrayHelpers;

type

  TObjProc = procedure of object;

  TTestObj = class
  private
    FValue: Integer;
    FProc: TObjProc;
  public
    constructor Create(aProc: TObjProc);
    constructor Create(aValue: Integer; aProc: TObjProc);
    destructor Destroy; override;
    property Value: Integer read FValue;
  end;

  TCounter = object
  private
    FCount: Integer;
    procedure IncCount;
    function  GetIncrement: TObjProc;
  public
    property  Increment: TObjProc read GetIncrement;
    property  Count: Integer read FCount;
  end;

  TIntSet        = specialize TGHashSetLP<Integer>;
  TAutoIntSet    = specialize TGAutoRef<TIntSet>;
  TStrSet        = specialize TGHashSetLP<string>;
  TAutoStrSet    = specialize TGAutoRef<TStrSet>;
  TObjSet        = specialize TGObjHashSetLP<TTestObj>;
  TAutoObjSet    = specialize TGAutoRef<TObjSet>;
  TIntEntry      = specialize TGMapEntry<Integer, Integer>;
  TIntHelper     = specialize TGArrayHelpUtil<TIntEntry>;
  TStrEntry      = specialize TGMapEntry<string, Integer>;
  TObjEntry      = specialize TGMapEntry<TTestObj, TTestObj>;
  TIntEntryArray = array of TIntEntry;
  TStrEntryArray = array of TStrEntry;
  TObjEntryArray = array of TObjEntry;
  TIntKeyArray   = array of Integer;
  TStrKeyArray   = array of string;

  TBaseTreeMapTest = class(TTestCase)
  private
  type
    TIntMap     = specialize TGTreeMap<Integer, Integer>;
    TAutoIntMap = specialize TGAutoRef<TIntMap>;
    TStrMap     = specialize TGTreeMap<string, Integer>;
    TAutoStrMap = specialize TGAutoRef<TStrMap>;
    TObjMap     = specialize TGObjTreeMap<TTestObj, TTestObj>;
    TAutoObjMap = specialize TGAutoRef<TObjMap>;

  published
    procedure Int_1;
    procedure Int_2;
    procedure IntRetain;
    procedure Str_1;
    procedure Str_2;
    procedure StrRetain;
    procedure ObjectMap;
    procedure Head;
    procedure Tail;
    procedure Range;
    procedure FindFloor;
    procedure FindCeil;
  end;

  TRec = record
    a,
    b: Integer;
    class operator < (const L, R: TRec): Boolean;
    constructor Create(ai, bi: Integer);
  end;

  { TComparableTreeMapTest }

  TComparableTreeMapTest = class(TTestCase)
  private
  type
    TIntMap     = specialize TGComparableTreeMap<Integer, Integer>;
    TAutoIntMap = specialize TGAutoRef<TIntMap>;
    TStrMap     = specialize TGComparableTreeMap<string, Integer>;
    TAutoStrMap = specialize TGAutoRef<TStrMap>;
    TRecMap     = specialize TGComparableTreeMap<TRec, Integer>;

  published
    procedure Int_1;
    procedure Int_2;
    procedure Rec_1;
    procedure IntRetain;
    procedure Str_1;
    procedure Str_2;
    procedure StrRetain;
    procedure Head;
    procedure Tail;
    procedure Range;
    procedure FindFloor;
    procedure FindCeil;
  end;

  TRegularTreeMapTest = class(TTestCase)
  private
  type
    TIntMap     = specialize TGRegularTreeMap<Integer, Integer>;
    TAutoIntMap = specialize TGAutoRef<TIntMap>;
    TStrMap     = specialize TGRegularTreeMap<string, Integer>;
    TAutoStrMap = specialize TGAutoRef<TStrMap>;
    TObjMap     = specialize TGObjectRegularTreeMap<TTestObj, TTestObj>;
    TAutoObjMap = specialize TGAutoRef<TObjMap>;

  published
    procedure Int_1;
    procedure Int_2;
    procedure IntRetain;
    procedure Str_1;
    procedure Str_2;
    procedure StrRetain;
    procedure ObjectMap;
    procedure Head;
    procedure Tail;
    procedure Range;
    procedure FindFloor;
    procedure FindCeil;
  end;

  TDelegatedTreeMapTest = class(TTestCase)
  private
  type
    TIntMap     = specialize TGDelegatedTreeMap<Integer, Integer>;
    TAutoIntMap = specialize TGAutoRef<TIntMap>;
    TStrMap     = specialize TGDelegatedTreeMap<string, Integer>;
    TAutoStrMap = specialize TGAutoRef<TStrMap>;
    TObjMap     = specialize TGObjectDelegatedTreeMap<TTestObj, TTestObj>;
    TAutoObjMap = specialize TGAutoRef<TObjMap>;

  published
    procedure Int_1;
    procedure Int_2;
    procedure IntRetain;
    procedure Str_1;
    procedure Str_2;
    procedure StrRetain;
    procedure ObjectMap;
    procedure Head;
    procedure Tail;
    procedure Range;
    procedure FindFloor;
    procedure FindCeil;
  end;

  { TLiteTreeMapTest }

  TLiteTreeMapTest = class(TTestCase)
  private
  type
    TIntMap = specialize TGLiteTreeMap<Integer, Integer, Integer>;
    TStrMap = specialize TGLiteTreeMap<string, Integer, string>;

  published
    procedure Int_1;
    procedure Int_2;
    procedure IntRetain;
    procedure Str_1;
    procedure Str_2;
    procedure StrRetain;
    procedure Head;
    procedure Tail;
    procedure Range;
    procedure FindFloor;
    procedure FindCeil;
    procedure MakeEmptyTest;
    procedure PassByValue;
  end;

  function CreateIntArray(aSize: Integer): TIntEntryArray;
  function CreateStrArray(aSize: Integer): TStrEntryArray;
  function CreateObjArray(var aCounter: TCounter; aSize: Integer): TObjEntryArray;

implementation
{$B-}{$COPERATORS ON}{$WARNINGS OFF}

function IntOdd(const aValue: Integer): Boolean;
begin
  Result := Odd(aValue);
end;

function IntOddValue(const e: TIntEntry): Boolean;
begin
  Result := Odd(e.Value);
end;

function StrOdd(const aValue: string): Boolean;
var
  I: Integer;
begin
  if TryStrToInt(Copy(aValue, 5, Length(aValue)), I) then
    exit(Odd(I));
  Result := False;
end;

function StrOddKey(const e: TStrEntry): Boolean;
var
  I: Integer;
begin
  if TryStrToInt(Copy(e.Key, 5, Length(e.Key)), I) then
    exit(Odd(I));
  Result := False;
end;

{ TTestObj }

constructor TTestObj.Create(aProc: TObjProc);
begin
  FProc := aProc;
end;

constructor TTestObj.Create(aValue: Integer; aProc: TObjProc);
begin
  FValue := aValue;
  FProc := aProc;
end;

destructor TTestObj.Destroy;
begin
  if FProc <> nil then
    FProc;
  inherited;
end;

{ TCounter }

procedure TCounter.IncCount;
begin
  Inc(FCount);
end;

function TCounter.GetIncrement: TObjProc;
begin
  Result := @IncCount;
end;

function CreateIntArray(aSize: Integer): TIntEntryArray;
var
  I: Integer;
begin
  System.SetLength(Result, aSize);
  for I := 0 to Pred(aSize) do
    begin
      Result[I].Key := I;
      Result[I].Value := I;
    end;
end;

function CreateStrArray(aSize: Integer): TStrEntryArray;
const
  STR_PREFIX = 'str_';
var
  I: Integer;
begin
  System.SetLength(Result, aSize);
  for I := 0 to Pred(aSize) do
    begin
      Result[I].Key := STR_PREFIX + I.ToString;
      Result[I].Value := I;
    end;
end;

function CreateObjArray(var aCounter: TCounter; aSize: Integer): TObjEntryArray;
var
  I: Integer;
begin
  System.SetLength(Result, aSize);
  for I := 0 to Pred(aSize) do
    begin
      Result[I].Key := TTestObj.Create(aCounter.Increment);
      Result[I].Value := TTestObj.Create(aCounter.Increment);
    end;
end;

procedure TBaseTreeMapTest.Int_1;
var
  m: TAutoIntMap;
  I: Integer;
  Raised: Boolean = False;
begin
  AssertTrue(m.Instance.IsEmpty);
  m.Instance.Add(1,11);
  AssertTrue(m.Instance.Count = 1);
  AssertTrue(m.Instance.Contains(1));
  AssertTrue(m.Instance[1] = 11);
  AssertFalse(m.Instance.Contains(2));
  m.Instance[2] := 22;
  AssertTrue(m.Instance.Contains(2));
  AssertTrue(m.Instance[2] = 22);
  AssertTrue(m.Instance.Count = 2);
  m.Instance.Clear;
  AssertTrue(m.Instance.Count = 0);
  for I := 1 to 100 do
    m.Instance[I * 23] := I * 47;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[I * 23] = I * 47);
  for I := 1 to 100 do
    m.Instance[I * 23] := I * 53;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[I * 23] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m.Instance[I * 23] = I * 53);
  try
    I := m.Instance[22];
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(m.Instance.Remove(23));
  AssertFalse(m.Instance.Contains(23));
  AssertFalse(m.Instance.TryGetValue(12, I));
  AssertTrue(m.Instance.TryGetValue(46, I));
  AssertTrue(I = 106);
  AssertTrue(m.Instance.GetValueDef(17, 0) = 0);
  AssertTrue(m.Instance.GetValueDef(17, 27) = 27);
  for I := 1 to 100 do
    m.Instance.Remove(I * 23);
  AssertTrue(m.Instance.Count = 0);
end;

procedure TBaseTreeMapTest.Int_2;
var
  m: TAutoIntMap;
  I, TestSize: Integer;
  ea: TIntEntryArray;
  k: TIntKeyArray;
  e: TIntEntry;
begin
  TestSize := 100;
  ea := CreateIntArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(ea) do
    k[I] := ea[I].Key;
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.RemoveIf(@IntOddValue) = 50);
  AssertTrue(m.Instance.Count = 50);
  for e in m.Instance.Entries do
    AssertFalse(IntOddValue(e));
  AssertTrue(m.Instance.AddAll(ea) = 50);
  ea := m.Instance.ExtractIf(@IntOddValue);
  AssertTrue(Length(ea) = 50);
  for e in ea do
    AssertTrue(IntOddValue(e));
end;

procedure TBaseTreeMapTest.IntRetain;
var
  m: TAutoIntMap;
  s: TAutoIntSet;
  I, TestSize: Integer;
  e: TIntEntryArray;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  I := 1;
  while I <= System.High(e) do
    begin
      s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(s.Instance));
  for I in m.Instance.Keys do
    AssertTrue(IntOdd(I));
  AssertTrue(m.Instance.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Instance.Count = 0);
end;

procedure TBaseTreeMapTest.Str_1;
var
  m: TAutoStrMap;
  I: Integer;
  Raised: Boolean = False;
begin
  AssertTrue(m.Instance.IsEmpty);
  m.Instance.Add('1',11);
  AssertTrue(m.Instance.Count = 1);
  AssertTrue(m.Instance.Contains('1'));
  AssertTrue(m.Instance['1'] = 11);
  AssertFalse(m.Instance.Contains('2'));
  m.Instance['2'] := 22;
  AssertTrue(m.Instance.Contains('2'));
  AssertTrue(m.Instance['2'] = 22);
  AssertTrue(m.Instance.Count = 2);
  m.Instance.Clear;
  AssertTrue(m.Instance.Count = 0);
  for I := 1 to 100 do
    m.Instance[IntToStr(I * 23)] := I * 47;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[IntToStr(I * 23)] = I * 47);
  for I := 1 to 100 do
    m.Instance[IntToStr(I * 23)] := I * 53;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[IntToStr(I * 23)] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m.Instance[IntToStr(I * 23)] = I * 53);
  try
    I := m.Instance['22'];
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(m.Instance.Remove('23'));
  AssertFalse(m.Instance.Contains('23'));
  AssertFalse(m.Instance.TryGetValue('12', I));
  AssertTrue(m.Instance.TryGetValue('46', I));
  AssertTrue(I = 106);
  AssertTrue(m.Instance.GetValueDef('17', 0) = 0);
  AssertTrue(m.Instance.GetValueDef('17', 27) = 27);
  for I := 1 to 100 do
    m.Instance.Remove(IntToStr(I * 23));
  AssertTrue(m.Instance.Count = 0);
end;

procedure TBaseTreeMapTest.Str_2;
var
  m: TAutoStrMap;
  I, TestSize: Integer;
  ea: TStrEntryArray;
  k: TStrKeyArray;
  e: TStrEntry;
begin
  TestSize := 100;
  ea := CreateStrArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(ea) do
    k[I] := ea[I].Key;
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.RemoveIf(@StrOddKey) = 50);
  AssertTrue(m.Instance.Count = 50);
  for e in m.Instance.Entries do
    AssertFalse(StrOddKey(e));
  AssertTrue(m.Instance.AddAll(ea) = 50);
  ea := m.Instance.ExtractIf(@StrOddKey);
  AssertTrue(Length(ea) = 50);
  for e in ea do
    AssertTrue(StrOddKey(e));
end;

procedure TBaseTreeMapTest.StrRetain;
var
  m: TAutoStrMap;
  s: TAutoStrSet;
  I, TestSize: Integer;
  e: TStrEntryArray;
  v: string;
begin
  TestSize := 100;
  e := CreateStrArray(TestSize);
  I := 1;
  while I <= System.High(e) do
    begin
      s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(s.Instance));
  for v in m.Instance.Keys do
    AssertTrue(StrOdd(v));
  AssertTrue(m.Instance.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Instance.Count = 0);
end;

procedure TBaseTreeMapTest.ObjectMap;
var
  m: TAutoObjMap;
  s: TAutoObjSet;
  I, TestSize: Integer;
  e: TObjEntryArray;
  Counter: TCounter;
begin
  Counter := Default(TCounter);
  TestSize := 100;
  e := CreateObjArray(Counter, TestSize);
  AssertTrue(Counter.Count = 0);
  {%H-}m.Instance := TObjMap.Create(OWNS_BOTH);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = 100);
  for I := 0 to System.High(e) do
    AssertTrue(m.Instance.Contains(e[I].Key));
  for I := 50 to 99 do
    s.Instance.Add(e[I].Key);
  AssertTrue(s.Instance.Count = 50);
  AssertTrue(Counter.Count = 0);
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(Counter.Count = 100);
  m.Instance.OwnsKeys := False;
  m.Instance.Clear;
  AssertTrue(Counter.Count = 150);
  s.Instance.Clear;
  AssertTrue(Counter.Count = 200);
end;

procedure TBaseTreeMapTest.Head;
var
  m: TAutoIntMap;
  e: TIntEntryArray;
  I, J, TestSize: Integer;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  TIntHelper.RandomShuffle(e);
  m.Instance := TIntMap.Create(e);
  AssertTrue(m.Instance.Head(0).None);
  AssertTrue(m.Instance.Head(0, True).Total = 1);
  I := 0;
  for J in m.Instance.Head(80) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 80);
  AssertTrue(m.Instance.Head(80, True).Total = 81);
end;

procedure TBaseTreeMapTest.Tail;
var
  m: TAutoIntMap;
  e: TIntEntryArray;
  I, J, TestSize: Integer;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  TIntHelper.RandomShuffle(e);
  m.Instance := TIntMap.Create(e);
  AssertTrue(m.Instance.Tail(99).Total = 1);
  AssertTrue(m.Instance.Tail(99, False).None);
  I := 30;
  for J in m.Instance.Tail(30) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 100);
end;

procedure TBaseTreeMapTest.Range;
var
  m: TAutoIntMap;
  e: TIntEntryArray;
  I, J, TestSize: Integer;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  TIntHelper.RandomShuffle(e);
  m.Instance := TIntMap.Create(e);
  AssertTrue(m.Instance.Range(10, 10).None);
  AssertTrue(m.Instance.Range(100, 100).None);
  AssertTrue(m.Instance.Range(0, 0).None);
  AssertTrue(m.Instance.Range(0, 0, BOUNDS_BOTH).Total = 1);
  I := 10;
  for J in m.Instance.Range(10, 90) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 90);
end;

procedure TBaseTreeMapTest.FindFloor;
var
  m: TAutoIntMap;
  I: Integer;
begin
  for I := 1 to 16 do
    m.Instance[I * 3] := I;
  AssertFalse(m.Instance.FindFloor(2, I));
  AssertFalse(m.Instance.FindFloor(3, I));
  AssertTrue(m.Instance.FindFloor(3, I, True));
  AssertTrue(I = 3);
  AssertTrue(m.Instance.FindFloor(9, I));
  AssertTrue(I = 6);
  AssertTrue(m.Instance.FindFloor(9, I, True));
  AssertTrue(I = 9);
  AssertTrue(m.Instance.FindFloor(13, I));
  AssertTrue(I = 12);
  AssertTrue(m.Instance.FindFloor(22, I));
  AssertTrue(I = 21);
  AssertTrue(m.Instance.FindFloor(27, I));
  AssertTrue(I = 24);
  AssertTrue(m.Instance.FindFloor(27, I, True));
  AssertTrue(I = 27);
end;

procedure TBaseTreeMapTest.FindCeil;
var
  m: TAutoIntMap;
  I: Integer;
begin
  for I := 1 to 16 do
    m.Instance[I * 3] := I;
  AssertTrue(m.Instance.FindCeil(48, I));
  AssertTrue(I = 48);
  AssertFalse(m.Instance.FindCeil(48, I, False));
  AssertTrue(m.Instance.FindCeil(9, I));
  AssertTrue(I = 9);
  AssertTrue(m.Instance.FindCeil(9, I, False));
  AssertTrue(I = 12);
  AssertTrue(m.Instance.FindCeil(13, I));
  AssertTrue(I = 15);
  AssertTrue(m.Instance.FindCeil(22, I));
  AssertTrue(I = 24);
  AssertTrue(m.Instance.FindCeil(27, I));
  AssertTrue(I = 27);
  AssertTrue(m.Instance.FindCeil(27, I, False));
  AssertTrue(I = 30);
end;

{ TRec }

class operator TRec.<(const L, R: TRec): Boolean;
begin
  Result := L.a + L.b < R.a + R.b;
end;

constructor TRec.Create(ai, bi: Integer);
begin
  a := ai;
  b := bi;
end;

{ TComparableTreeMapTest}

procedure TComparableTreeMapTest.Int_1;
var
  m: TAutoIntMap;
  I: Integer;
  Raised: Boolean = False;
begin
  AssertTrue(m.Instance.IsEmpty);
  m.Instance.Add(1,11);
  AssertTrue(m.Instance.Count = 1);
  AssertTrue(m.Instance.Contains(1));
  AssertTrue(m.Instance[1] = 11);
  AssertFalse(m.Instance.Contains(2));
  m.Instance[2] := 22;
  AssertTrue(m.Instance.Contains(2));
  AssertTrue(m.Instance[2] = 22);
  AssertTrue(m.Instance.Count = 2);
  m.Instance.Clear;
  AssertTrue(m.Instance.Count = 0);
  for I := 1 to 100 do
    m.Instance[I * 23] := I * 47;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[I * 23] = I * 47);
  for I := 1 to 100 do
    m.Instance[I * 23] := I * 53;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[I * 23] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m.Instance[I * 23] = I * 53);
  try
    I := m.Instance[22];
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(m.Instance.Remove(23));
  AssertFalse(m.Instance.Contains(23));
  AssertFalse(m.Instance.TryGetValue(12, I));
  AssertTrue(m.Instance.TryGetValue(46, I));
  AssertTrue(I = 106);
  AssertTrue(m.Instance.GetValueDef(17, 0) = 0);
  AssertTrue(m.Instance.GetValueDef(17, 27) = 27);
  for I := 1 to 100 do
    m.Instance.Remove(I * 23);
  AssertTrue(m.Instance.Count = 0);
end;

procedure TComparableTreeMapTest.Int_2;
var
  m: TAutoIntMap;
  I, TestSize: Integer;
  ea: TIntEntryArray;
  k: TIntKeyArray;
  e: TIntEntry;
begin
  TestSize := 100;
  ea := CreateIntArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(ea) do
    k[I] := ea[I].Key;
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.RemoveIf(@IntOddValue) = 50);
  AssertTrue(m.Instance.Count = 50);
  for e in m.Instance.Entries do
    AssertFalse(IntOddValue(e));
  AssertTrue(m.Instance.AddAll(ea) = 50);
  ea := m.Instance.ExtractIf(@IntOddValue);
  AssertTrue(Length(ea) = 50);
  for e in ea do
    AssertTrue(IntOddValue(e));
end;

procedure TComparableTreeMapTest.Rec_1;
var
  m: specialize TGAutoRef<TRecMap>;
  I: Integer;
  r: TRec;
const
  TestSize = 100;
begin
  for I := 1 to TestSize do
    AssertTrue(m.Instance.Add(TRec.Create(I, 0), I));
  AssertTrue(m.Instance.Count = TestSize);
  AssertFalse(m.Instance.Add(TRec.Create(1, 0), 2));
  I := 0;
  for r in m.Instance.Range(TRec.Create(10, 0), TRec.Create(30, 0)) do
    Inc(I);
  AssertTrue(I = 20);
end;

procedure TComparableTreeMapTest.IntRetain;
var
  m: TAutoIntMap;
  s: TAutoIntSet;
  I, TestSize: Integer;
  e: TIntEntryArray;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  I := 1;
  while I <= System.High(e) do
    begin
      s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(s.Instance));
  for I in m.Instance.Keys do
    AssertTrue(IntOdd(I));
  AssertTrue(m.Instance.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Instance.Count = 0);
end;

procedure TComparableTreeMapTest.Str_1;
var
  m: TAutoStrMap;
  I: Integer;
  Raised: Boolean = False;
begin
  AssertTrue(m.Instance.IsEmpty);
  m.Instance.Add('1',11);
  AssertTrue(m.Instance.Count = 1);
  AssertTrue(m.Instance.Contains('1'));
  AssertTrue(m.Instance['1'] = 11);
  AssertFalse(m.Instance.Contains('2'));
  m.Instance['2'] := 22;
  AssertTrue(m.Instance.Contains('2'));
  AssertTrue(m.Instance['2'] = 22);
  AssertTrue(m.Instance.Count = 2);
  m.Instance.Clear;
  AssertTrue(m.Instance.Count = 0);
  for I := 1 to 100 do
    m.Instance[IntToStr(I * 23)] := I * 47;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[IntToStr(I * 23)] = I * 47);
  for I := 1 to 100 do
    m.Instance[IntToStr(I * 23)] := I * 53;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[IntToStr(I * 23)] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m.Instance[IntToStr(I * 23)] = I * 53);
  try
    I := m.Instance['22'];
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(m.Instance.Remove('23'));
  AssertFalse(m.Instance.Contains('23'));
  AssertFalse(m.Instance.TryGetValue('12', I));
  AssertTrue(m.Instance.TryGetValue('46', I));
  AssertTrue(I = 106);
  AssertTrue(m.Instance.GetValueDef('17', 0) = 0);
  AssertTrue(m.Instance.GetValueDef('17', 27) = 27);
  for I := 1 to 100 do
    m.Instance.Remove(IntToStr(I * 23));
  AssertTrue(m.Instance.Count = 0);
end;

procedure TComparableTreeMapTest.Str_2;
var
  m: TAutoStrMap;
  I, TestSize: Integer;
  ea: TStrEntryArray;
  k: TStrKeyArray;
  e: TStrEntry;
begin
  TestSize := 100;
  ea := CreateStrArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(ea) do
    k[I] := ea[I].Key;
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.RemoveIf(@StrOddKey) = 50);
  AssertTrue(m.Instance.Count = 50);
  for e in m.Instance.Entries do
    AssertFalse(StrOddKey(e));
  AssertTrue(m.Instance.AddAll(ea) = 50);
  ea := m.Instance.ExtractIf(@StrOddKey);
  AssertTrue(Length(ea) = 50);
  for e in ea do
    AssertTrue(StrOddKey(e));
end;

procedure TComparableTreeMapTest.StrRetain;
var
  m: TAutoStrMap;
  s: TAutoStrSet;
  I, TestSize: Integer;
  e: TStrEntryArray;
  v: string;
begin
  TestSize := 100;
  e := CreateStrArray(TestSize);
  I := 1;
  while I <= System.High(e) do
    begin
      s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(s.Instance));
  for v in m.Instance.Keys do
    AssertTrue(StrOdd(v));
  AssertTrue(m.Instance.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Instance.Count = 0);
end;

procedure TComparableTreeMapTest.Head;
var
  m: TAutoIntMap;
  e: TIntEntryArray;
  I, J, TestSize: Integer;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  TIntHelper.RandomShuffle(e);
  m.Instance := TIntMap.Create(e);
  AssertTrue(m.Instance.Head(0).None);
  AssertTrue(m.Instance.Head(0, True).Total = 1);
  I := 0;
  for J in m.Instance.Head(80) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 80);
  AssertTrue(m.Instance.Head(80, True).Total = 81);
end;

procedure TComparableTreeMapTest.Tail;
var
  m: TAutoIntMap;
  e: TIntEntryArray;
  I, J, TestSize: Integer;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  TIntHelper.RandomShuffle(e);
  m.Instance := TIntMap.Create(e);
  AssertTrue(m.Instance.Tail(99).Total = 1);
  AssertTrue(m.Instance.Tail(99, False).None);
  I := 30;
  for J in m.Instance.Tail(30) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 100);
end;

procedure TComparableTreeMapTest.Range;
var
  m: TAutoIntMap;
  e: TIntEntryArray;
  I, J, TestSize: Integer;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  TIntHelper.RandomShuffle(e);
  m.Instance := TIntMap.Create(e);
  AssertTrue(m.Instance.Range(10, 10).None);
  AssertTrue(m.Instance.Range(100, 100).None);
  AssertTrue(m.Instance.Range(0, 0).None);
  AssertTrue(m.Instance.Range(0, 0, BOUNDS_BOTH).Total = 1);
  I := 10;
  for J in m.Instance.Range(10, 90) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 90);
end;

procedure TComparableTreeMapTest.FindFloor;
var
  m: TAutoIntMap;
  I: Integer;
begin
  for I := 1 to 16 do
    m.Instance[I * 3] := I;
  AssertFalse(m.Instance.FindFloor(2, I));
  AssertFalse(m.Instance.FindFloor(3, I));
  AssertTrue(m.Instance.FindFloor(3, I, True));
  AssertTrue(I = 3);
  AssertTrue(m.Instance.FindFloor(9, I));
  AssertTrue(I = 6);
  AssertTrue(m.Instance.FindFloor(9, I, True));
  AssertTrue(I = 9);
  AssertTrue(m.Instance.FindFloor(13, I));
  AssertTrue(I = 12);
  AssertTrue(m.Instance.FindFloor(22, I));
  AssertTrue(I = 21);
  AssertTrue(m.Instance.FindFloor(27, I));
  AssertTrue(I = 24);
  AssertTrue(m.Instance.FindFloor(27, I, True));
  AssertTrue(I = 27);
end;

procedure TComparableTreeMapTest.FindCeil;
var
  m: TAutoIntMap;
  I: Integer;
begin
  for I := 1 to 16 do
    m.Instance[I * 3] := I;
  AssertTrue(m.Instance.FindCeil(48, I));
  AssertTrue(I = 48);
  AssertFalse(m.Instance.FindCeil(48, I, False));
  AssertTrue(m.Instance.FindCeil(9, I));
  AssertTrue(I = 9);
  AssertTrue(m.Instance.FindCeil(9, I, False));
  AssertTrue(I = 12);
  AssertTrue(m.Instance.FindCeil(13, I));
  AssertTrue(I = 15);
  AssertTrue(m.Instance.FindCeil(22, I));
  AssertTrue(I = 24);
  AssertTrue(m.Instance.FindCeil(27, I));
  AssertTrue(I = 27);
  AssertTrue(m.Instance.FindCeil(27, I, False));
  AssertTrue(I = 30);
end;

{ TRegularTreeMapTest}

procedure TRegularTreeMapTest.Int_1;
var
  m: TAutoIntMap;
  I: Integer;
  Raised: Boolean = False;
begin
  AssertTrue(m.Instance.IsEmpty);
  m.Instance.Add(1,11);
  AssertTrue(m.Instance.Count = 1);
  AssertTrue(m.Instance.Contains(1));
  AssertTrue(m.Instance[1] = 11);
  AssertFalse(m.Instance.Contains(2));
  m.Instance[2] := 22;
  AssertTrue(m.Instance.Contains(2));
  AssertTrue(m.Instance[2] = 22);
  AssertTrue(m.Instance.Count = 2);
  m.Instance.Clear;
  AssertTrue(m.Instance.Count = 0);
  for I := 1 to 100 do
    m.Instance[I * 23] := I * 47;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[I * 23] = I * 47);
  for I := 1 to 100 do
    m.Instance[I * 23] := I * 53;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[I * 23] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m.Instance[I * 23] = I * 53);
  try
    I := m.Instance[22];
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(m.Instance.Remove(23));
  AssertFalse(m.Instance.Contains(23));
  AssertFalse(m.Instance.TryGetValue(12, I));
  AssertTrue(m.Instance.TryGetValue(46, I));
  AssertTrue(I = 106);
  AssertTrue(m.Instance.GetValueDef(17, 0) = 0);
  AssertTrue(m.Instance.GetValueDef(17, 27) = 27);
  for I := 1 to 100 do
    m.Instance.Remove(I * 23);
  AssertTrue(m.Instance.Count = 0);
end;

procedure TRegularTreeMapTest.Int_2;
var
  m: TAutoIntMap;
  I, TestSize: Integer;
  ea: TIntEntryArray;
  k: TIntKeyArray;
  e: TIntEntry;
begin
  TestSize := 100;
  ea := CreateIntArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(ea) do
    k[I] := ea[I].Key;
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.RemoveIf(@IntOddValue) = 50);
  AssertTrue(m.Instance.Count = 50);
  for e in m.Instance.Entries do
    AssertFalse(IntOddValue(e));
  AssertTrue(m.Instance.AddAll(ea) = 50);
  ea := m.Instance.ExtractIf(@IntOddValue);
  AssertTrue(Length(ea) = 50);
  for e in ea do
    AssertTrue(IntOddValue(e));
end;

procedure TRegularTreeMapTest.IntRetain;
var
  m: TAutoIntMap;
  s: TAutoIntSet;
  I, TestSize: Integer;
  e: TIntEntryArray;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  I := 1;
  while I <= System.High(e) do
    begin
      s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(s.Instance));
  for I in m.Instance.Keys do
    AssertTrue(IntOdd(I));
  AssertTrue(m.Instance.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Instance.Count = 0);
end;

procedure TRegularTreeMapTest.Str_1;
var
  m: TAutoStrMap;
  I: Integer;
  Raised: Boolean = False;
begin
  AssertTrue(m.Instance.IsEmpty);
  m.Instance.Add('1',11);
  AssertTrue(m.Instance.Count = 1);
  AssertTrue(m.Instance.Contains('1'));
  AssertTrue(m.Instance['1'] = 11);
  AssertFalse(m.Instance.Contains('2'));
  m.Instance['2'] := 22;
  AssertTrue(m.Instance.Contains('2'));
  AssertTrue(m.Instance['2'] = 22);
  AssertTrue(m.Instance.Count = 2);
  m.Instance.Clear;
  AssertTrue(m.Instance.Count = 0);
  for I := 1 to 100 do
    m.Instance[IntToStr(I * 23)] := I * 47;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[IntToStr(I * 23)] = I * 47);
  for I := 1 to 100 do
    m.Instance[IntToStr(I * 23)] := I * 53;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[IntToStr(I * 23)] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m.Instance[IntToStr(I * 23)] = I * 53);
  try
    I := m.Instance['22'];
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(m.Instance.Remove('23'));
  AssertFalse(m.Instance.Contains('23'));
  AssertFalse(m.Instance.TryGetValue('12', I));
  AssertTrue(m.Instance.TryGetValue('46', I));
  AssertTrue(I = 106);
  AssertTrue(m.Instance.GetValueDef('17', 0) = 0);
  AssertTrue(m.Instance.GetValueDef('17', 27) = 27);
  for I := 1 to 100 do
    m.Instance.Remove(IntToStr(I * 23));
  AssertTrue(m.Instance.Count = 0);
end;

procedure TRegularTreeMapTest.Str_2;
var
  m: TAutoStrMap;
  I, TestSize: Integer;
  ea: TStrEntryArray;
  k: TStrKeyArray;
  e: TStrEntry;
begin
  TestSize := 100;
  ea := CreateStrArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(ea) do
    k[I] := ea[I].Key;
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.RemoveIf(@StrOddKey) = 50);
  AssertTrue(m.Instance.Count = 50);
  for e in m.Instance.Entries do
    AssertFalse(StrOddKey(e));
  AssertTrue(m.Instance.AddAll(ea) = 50);
  ea := m.Instance.ExtractIf(@StrOddKey);
  AssertTrue(Length(ea) = 50);
  for e in ea do
    AssertTrue(StrOddKey(e));
end;

procedure TRegularTreeMapTest.StrRetain;
var
  m: TAutoStrMap;
  s: TAutoStrSet;
  I, TestSize: Integer;
  e: TStrEntryArray;
  v: string;
begin
  TestSize := 100;
  e := CreateStrArray(TestSize);
  I := 1;
  while I <= System.High(e) do
    begin
      s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(s.Instance));
  for v in m.Instance.Keys do
    AssertTrue(StrOdd(v));
  AssertTrue(m.Instance.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Instance.Count = 0);
end;

procedure TRegularTreeMapTest.ObjectMap;
var
  m: TAutoObjMap;
  s: TAutoObjSet;
  I, TestSize: Integer;
  e: TObjEntryArray;
  Counter: TCounter;
begin
  Counter := Default(TCounter);
  TestSize := 100;
  e := CreateObjArray(Counter, TestSize);
  AssertTrue(Counter.Count = 0);
  {%H-}m.Instance := TObjMap.Create(OWNS_BOTH);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = 100);
  for I := 0 to System.High(e) do
    AssertTrue(m.Instance.Contains(e[I].Key));
  for I := 50 to 99 do
    s.Instance.Add(e[I].Key);
  AssertTrue(s.Instance.Count = 50);
  AssertTrue(Counter.Count = 0);
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(Counter.Count = 100);
  m.Instance.OwnsKeys := False;
  m.Instance.Clear;
  AssertTrue(Counter.Count = 150);
  s.Instance.Clear;
  AssertTrue(Counter.Count = 200);
end;

procedure TRegularTreeMapTest.Head;
var
  m: TAutoIntMap;
  e: TIntEntryArray;
  I, J, TestSize: Integer;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  TIntHelper.RandomShuffle(e);
  m.Instance := TIntMap.Create(e, specialize TGDefaults<Integer>.Less);
  AssertTrue(m.Instance.Head(0).None);
  AssertTrue(m.Instance.Head(0, True).Total = 1);
  I := 0;
  for J in m.Instance.Head(80) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 80);
  AssertTrue(m.Instance.Head(80, True).Total = 81);
end;

procedure TRegularTreeMapTest.Tail;
var
  m: TAutoIntMap;
  e: TIntEntryArray;
  I, J, TestSize: Integer;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  TIntHelper.RandomShuffle(e);
  m.Instance := TIntMap.Create(e, specialize TGDefaults<Integer>.Less);
  AssertTrue(m.Instance.Tail(99).Total = 1);
  AssertTrue(m.Instance.Tail(99, False).None);
  I := 30;
  for J in m.Instance.Tail(30) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 100);
end;

procedure TRegularTreeMapTest.Range;
var
  m: TAutoIntMap;
  e: TIntEntryArray;
  I, J, TestSize: Integer;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  TIntHelper.RandomShuffle(e);
  m.Instance := TIntMap.Create(e, specialize TGDefaults<Integer>.Less);
  AssertTrue(m.Instance.Range(10, 10).None);
  AssertTrue(m.Instance.Range(100, 100).None);
  AssertTrue(m.Instance.Range(0, 0).None);
  AssertTrue(m.Instance.Range(0, 0, BOUNDS_BOTH).Total = 1);
  I := 10;
  for J in m.Instance.Range(10, 90) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 90);
end;

procedure TRegularTreeMapTest.FindFloor;
var
  m: TAutoIntMap;
  I: Integer;
begin
  for I := 1 to 16 do
    m.Instance[I * 3] := I;
  AssertFalse(m.Instance.FindFloor(2, I));
  AssertFalse(m.Instance.FindFloor(3, I));
  AssertTrue(m.Instance.FindFloor(3, I, True));
  AssertTrue(I = 3);
  AssertTrue(m.Instance.FindFloor(9, I));
  AssertTrue(I = 6);
  AssertTrue(m.Instance.FindFloor(9, I, True));
  AssertTrue(I = 9);
  AssertTrue(m.Instance.FindFloor(13, I));
  AssertTrue(I = 12);
  AssertTrue(m.Instance.FindFloor(22, I));
  AssertTrue(I = 21);
  AssertTrue(m.Instance.FindFloor(27, I));
  AssertTrue(I = 24);
  AssertTrue(m.Instance.FindFloor(27, I, True));
  AssertTrue(I = 27);
end;

procedure TRegularTreeMapTest.FindCeil;
var
  m: TAutoIntMap;
  I: Integer;
begin
  for I := 1 to 16 do
    m.Instance[I * 3] := I;
  AssertTrue(m.Instance.FindCeil(48, I));
  AssertTrue(I = 48);
  AssertFalse(m.Instance.FindCeil(48, I, False));
  AssertTrue(m.Instance.FindCeil(9, I));
  AssertTrue(I = 9);
  AssertTrue(m.Instance.FindCeil(9, I, False));
  AssertTrue(I = 12);
  AssertTrue(m.Instance.FindCeil(13, I));
  AssertTrue(I = 15);
  AssertTrue(m.Instance.FindCeil(22, I));
  AssertTrue(I = 24);
  AssertTrue(m.Instance.FindCeil(27, I));
  AssertTrue(I = 27);
  AssertTrue(m.Instance.FindCeil(27, I, False));
  AssertTrue(I = 30);
end;

{ TDelegatedTreeMapTest}

procedure TDelegatedTreeMapTest.Int_1;
var
  m: TAutoIntMap;
  I: Integer;
  Raised: Boolean = False;
begin
  AssertTrue(m.Instance.IsEmpty);
  m.Instance.Add(1,11);
  AssertTrue(m.Instance.Count = 1);
  AssertTrue(m.Instance.Contains(1));
  AssertTrue(m.Instance[1] = 11);
  AssertFalse(m.Instance.Contains(2));
  m.Instance[2] := 22;
  AssertTrue(m.Instance.Contains(2));
  AssertTrue(m.Instance[2] = 22);
  AssertTrue(m.Instance.Count = 2);
  m.Instance.Clear;
  AssertTrue(m.Instance.Count = 0);
  for I := 1 to 100 do
    m.Instance[I * 23] := I * 47;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[I * 23] = I * 47);
  for I := 1 to 100 do
    m.Instance[I * 23] := I * 53;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[I * 23] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m.Instance[I * 23] = I * 53);
  try
    I := m.Instance[22];
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(m.Instance.Remove(23));
  AssertFalse(m.Instance.Contains(23));
  AssertFalse(m.Instance.TryGetValue(12, I));
  AssertTrue(m.Instance.TryGetValue(46, I));
  AssertTrue(I = 106);
  AssertTrue(m.Instance.GetValueDef(17, 0) = 0);
  AssertTrue(m.Instance.GetValueDef(17, 27) = 27);
  for I := 1 to 100 do
    m.Instance.Remove(I * 23);
  AssertTrue(m.Instance.Count = 0);
end;

procedure TDelegatedTreeMapTest.Int_2;
var
  m: TAutoIntMap;
  I, TestSize: Integer;
  ea: TIntEntryArray;
  k: TIntKeyArray;
  e: TIntEntry;
begin
  TestSize := 100;
  ea := CreateIntArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(ea) do
    k[I] := ea[I].Key;
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.RemoveIf(@IntOddValue) = 50);
  AssertTrue(m.Instance.Count = 50);
  for e in m.Instance.Entries do
    AssertFalse(IntOddValue(e));
  AssertTrue(m.Instance.AddAll(ea) = 50);
  ea := m.Instance.ExtractIf(@IntOddValue);
  AssertTrue(Length(ea) = 50);
  for e in ea do
    AssertTrue(IntOddValue(e));
end;

procedure TDelegatedTreeMapTest.IntRetain;
var
  m: TAutoIntMap;
  s: TAutoIntSet;
  I, TestSize: Integer;
  e: TIntEntryArray;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  I := 1;
  while I <= System.High(e) do
    begin
      s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(s.Instance));
  for I in m.Instance.Keys do
    AssertTrue(IntOdd(I));
  AssertTrue(m.Instance.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Instance.Count = 0);
end;

procedure TDelegatedTreeMapTest.Str_1;
var
  m: TAutoStrMap;
  I: Integer;
  Raised: Boolean = False;
begin
  AssertTrue(m.Instance.IsEmpty);
  m.Instance.Add('1',11);
  AssertTrue(m.Instance.Count = 1);
  AssertTrue(m.Instance.Contains('1'));
  AssertTrue(m.Instance['1'] = 11);
  AssertFalse(m.Instance.Contains('2'));
  m.Instance['2'] := 22;
  AssertTrue(m.Instance.Contains('2'));
  AssertTrue(m.Instance['2'] = 22);
  AssertTrue(m.Instance.Count = 2);
  m.Instance.Clear;
  AssertTrue(m.Instance.Count = 0);
  for I := 1 to 100 do
    m.Instance[IntToStr(I * 23)] := I * 47;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[IntToStr(I * 23)] = I * 47);
  for I := 1 to 100 do
    m.Instance[IntToStr(I * 23)] := I * 53;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[IntToStr(I * 23)] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m.Instance[IntToStr(I * 23)] = I * 53);
  try
    I := m.Instance['22'];
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(m.Instance.Remove('23'));
  AssertFalse(m.Instance.Contains('23'));
  AssertFalse(m.Instance.TryGetValue('12', I));
  AssertTrue(m.Instance.TryGetValue('46', I));
  AssertTrue(I = 106);
  AssertTrue(m.Instance.GetValueDef('17', 0) = 0);
  AssertTrue(m.Instance.GetValueDef('17', 27) = 27);
  for I := 1 to 100 do
    m.Instance.Remove(IntToStr(I * 23));
  AssertTrue(m.Instance.Count = 0);
end;

procedure TDelegatedTreeMapTest.Str_2;
var
  m: TAutoStrMap;
  I, TestSize: Integer;
  ea: TStrEntryArray;
  k: TStrKeyArray;
  e: TStrEntry;
begin
  TestSize := 100;
  ea := CreateStrArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(ea) do
    k[I] := ea[I].Key;
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(ea);
  AssertTrue(m.Instance.RemoveIf(@StrOddKey) = 50);
  AssertTrue(m.Instance.Count = 50);
  for e in m.Instance.Entries do
    AssertFalse(StrOddKey(e));
  AssertTrue(m.Instance.AddAll(ea) = 50);
  ea := m.Instance.ExtractIf(@StrOddKey);
  AssertTrue(Length(ea) = 50);
  for e in ea do
    AssertTrue(StrOddKey(e));
end;

procedure TDelegatedTreeMapTest.StrRetain;
var
  m: TAutoStrMap;
  s: TAutoStrSet;
  I, TestSize: Integer;
  e: TStrEntryArray;
  v: string;
begin
  TestSize := 100;
  e := CreateStrArray(TestSize);
  I := 1;
  while I <= System.High(e) do
    begin
      s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(s.Instance));
  for v in m.Instance.Keys do
    AssertTrue(StrOdd(v));
  AssertTrue(m.Instance.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Instance.Count = 0);
end;

procedure TDelegatedTreeMapTest.ObjectMap;
var
  m: TAutoObjMap;
  s: TAutoObjSet;
  I, TestSize: Integer;
  e: TObjEntryArray;
  Counter: TCounter;
begin
  Counter := Default(TCounter);
  TestSize := 100;
  e := CreateObjArray(Counter, TestSize);
  AssertTrue(Counter.Count = 0);
  {%H-}m.Instance := TObjMap.Create(OWNS_BOTH);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = 100);
  for I := 0 to System.High(e) do
    AssertTrue(m.Instance.Contains(e[I].Key));
  for I := 50 to 99 do
    s.Instance.Add(e[I].Key);
  AssertTrue(s.Instance.Count = 50);
  AssertTrue(Counter.Count = 0);
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(Counter.Count = 100);
  m.Instance.OwnsKeys := False;
  m.Instance.Clear;
  AssertTrue(Counter.Count = 150);
  s.Instance.Clear;
  AssertTrue(Counter.Count = 200);
end;

procedure TDelegatedTreeMapTest.Head;
var
  m: TAutoIntMap;
  e: TIntEntryArray;
  I, J, TestSize: Integer;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  TIntHelper.RandomShuffle(e);
  m.Instance := TIntMap.Create(e, specialize TGDefaults<Integer>.OnLess);
  AssertTrue(m.Instance.Head(0).None);
  AssertTrue(m.Instance.Head(0, True).Total = 1);
  I := 0;
  for J in m.Instance.Head(80) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 80);
  AssertTrue(m.Instance.Head(80, True).Total = 81);
end;

procedure TDelegatedTreeMapTest.Tail;
var
  m: TAutoIntMap;
  e: TIntEntryArray;
  I, J, TestSize: Integer;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  TIntHelper.RandomShuffle(e);
  m.Instance := TIntMap.Create(e, specialize TGDefaults<Integer>.OnLess);
  AssertTrue(m.Instance.Tail(99).Total = 1);
  AssertTrue(m.Instance.Tail(99, False).None);
  I := 30;
  for J in m.Instance.Tail(30) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 100);
end;

procedure TDelegatedTreeMapTest.Range;
var
  m: TAutoIntMap;
  e: TIntEntryArray;
  I, J, TestSize: Integer;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  TIntHelper.RandomShuffle(e);
  m.Instance := TIntMap.Create(e, specialize TGDefaults<Integer>.OnLess);
  AssertTrue(m.Instance.Range(10, 10).None);
  AssertTrue(m.Instance.Range(100, 100).None);
  AssertTrue(m.Instance.Range(0, 0).None);
  AssertTrue(m.Instance.Range(0, 0, BOUNDS_BOTH).Total = 1);
  I := 10;
  for J in m.Instance.Range(10, 90) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 90);
end;

procedure TDelegatedTreeMapTest.FindFloor;
var
  m: TAutoIntMap;
  I: Integer;
begin
  for I := 1 to 16 do
    m.Instance[I * 3] := I;
  AssertFalse(m.Instance.FindFloor(2, I));
  AssertFalse(m.Instance.FindFloor(3, I));
  AssertTrue(m.Instance.FindFloor(3, I, True));
  AssertTrue(I = 3);
  AssertTrue(m.Instance.FindFloor(9, I));
  AssertTrue(I = 6);
  AssertTrue(m.Instance.FindFloor(9, I, True));
  AssertTrue(I = 9);
  AssertTrue(m.Instance.FindFloor(13, I));
  AssertTrue(I = 12);
  AssertTrue(m.Instance.FindFloor(22, I));
  AssertTrue(I = 21);
  AssertTrue(m.Instance.FindFloor(27, I));
  AssertTrue(I = 24);
  AssertTrue(m.Instance.FindFloor(27, I, True));
  AssertTrue(I = 27);
end;

procedure TDelegatedTreeMapTest.FindCeil;
var
  m: TAutoIntMap;
  I: Integer;
begin
  for I := 1 to 16 do
    m.Instance[I * 3] := I;
  AssertTrue(m.Instance.FindCeil(48, I));
  AssertTrue(I = 48);
  AssertFalse(m.Instance.FindCeil(48, I, False));
  AssertTrue(m.Instance.FindCeil(9, I));
  AssertTrue(I = 9);
  AssertTrue(m.Instance.FindCeil(9, I, False));
  AssertTrue(I = 12);
  AssertTrue(m.Instance.FindCeil(13, I));
  AssertTrue(I = 15);
  AssertTrue(m.Instance.FindCeil(22, I));
  AssertTrue(I = 24);
  AssertTrue(m.Instance.FindCeil(27, I));
  AssertTrue(I = 27);
  AssertTrue(m.Instance.FindCeil(27, I, False));
  AssertTrue(I = 30);
end;

{ TLiteTreeMapTest }

procedure TLiteTreeMapTest.Int_1;
var
  m: TIntMap;
  I: Integer;
  Raised: Boolean = False;
begin
  AssertTrue(m.IsEmpty);
  m.Add(1,11);
  AssertTrue(m.Count = 1);
  AssertTrue(m.Contains(1));
  AssertTrue(m[1] = 11);
  AssertFalse(m.Contains(2));
  m[2] := 22;
  AssertTrue(m.Contains(2));
  AssertTrue(m[2] = 22);
  AssertTrue(m.Count = 2);
  m.Clear;
  AssertTrue(m.Count = 0);
  for I := 1 to 100 do
    m[I * 23] := I * 47;
  AssertTrue(m.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m[I * 23] = I * 47);
  for I := 1 to 100 do
    m[I * 23] := I * 53;
  AssertTrue(m.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m[I * 23] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m[I * 23] = I * 53);
  try
    I := m[22];
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(m.Remove(23));
  AssertFalse(m.Contains(23));
  AssertFalse(m.TryGetValue(12, I));
  AssertTrue(m.TryGetValue(46, I));
  AssertTrue(I = 106);
  AssertTrue(m.GetValueDef(17, 0) = 0);
  AssertTrue(m.GetValueDef(17, 27) = 27);
  for I := 1 to 100 do
    m.Remove(I * 23);
  AssertTrue(m.Count = 0);
end;

procedure TLiteTreeMapTest.Int_2;
var
  m: TIntMap;
  I, TestSize: Integer;
  ea: TIntEntryArray;
  k: TIntKeyArray;
  e: TIntEntry;
begin
  TestSize := 100;
  ea := CreateIntArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(ea) do
    k[I] := ea[I].Key;
  m.AddAll(ea);
  AssertTrue(m.Count = TestSize);
  AssertTrue(m.ContainsAll(k));
  AssertTrue(m.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(k[50..99]));
  AssertTrue(m.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Count = 0);
  m.AddAll(ea);
  AssertTrue(m.RemoveIf(@IntOddValue) = 50);
  AssertTrue(m.Count = 50);
  for e in m do
    AssertFalse(IntOddValue(e));
  AssertTrue(m.AddAll(ea) = 50);
  ea := m.ExtractIf(@IntOddValue);
  AssertTrue(Length(ea) = 50);
  for e in ea do
    AssertTrue(IntOddValue(e));
end;

procedure TLiteTreeMapTest.IntRetain;
var
  m: TIntMap;
  s: TAutoIntSet;
  I, TestSize: Integer;
  e: TIntEntryArray;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  I := 1;
  while I <= System.High(e) do
    begin
      s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.AddAll(e);
  AssertTrue(m.Count = TestSize);
  m.RetainAll(s.Instance);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(s.Instance));
  for I in m.Keys do
    AssertTrue(IntOdd(I));
  AssertTrue(m.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Count = 0);
end;

procedure TLiteTreeMapTest.Str_1;
var
  m: TStrMap;
  I: Integer;
  Raised: Boolean = False;
begin
  AssertTrue(m.IsEmpty);
  m.Add('1',11);
  AssertTrue(m.Count = 1);
  AssertTrue(m.Contains('1'));
  AssertTrue(m['1'] = 11);
  AssertFalse(m.Contains('2'));
  m['2'] := 22;
  AssertTrue(m.Contains('2'));
  AssertTrue(m['2'] = 22);
  AssertTrue(m.Count = 2);
  m.Clear;
  AssertTrue(m.Count = 0);
  for I := 1 to 100 do
    m[IntToStr(I * 23)] := I * 47;
  AssertTrue(m.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m[IntToStr(I * 23)] = I * 47);
  for I := 1 to 100 do
    m[IntToStr(I * 23)] := I * 53;
  AssertTrue(m.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m[IntToStr(I * 23)] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m[IntToStr(I * 23)] = I * 53);
  try
    I := m['22'];
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(m.Remove('23'));
  AssertFalse(m.Contains('23'));
  AssertFalse(m.TryGetValue('12', I));
  AssertTrue(m.TryGetValue('46', I));
  AssertTrue(I = 106);
  AssertTrue(m.GetValueDef('17', 0) = 0);
  AssertTrue(m.GetValueDef('17', 27) = 27);
  for I := 1 to 100 do
    m.Remove(IntToStr(I * 23));
  AssertTrue(m.Count = 0);
end;

procedure TLiteTreeMapTest.Str_2;
var
  m: TStrMap;
  I, TestSize: Integer;
  ea: TStrEntryArray;
  k: TStrKeyArray;
  e: TStrEntry;
begin
  TestSize := 100;
  ea := CreateStrArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(ea) do
    k[I] := ea[I].Key;
  m.AddAll(ea);
  AssertTrue(m.Count = TestSize);
  AssertTrue(m.ContainsAll(k));
  AssertTrue(m.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(k[50..99]));
  AssertTrue(m.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Count = 0);
  m.AddAll(ea);
  AssertTrue(m.RemoveIf(@StrOddKey) = 50);
  AssertTrue(m.Count = 50);
  for e in m do
    AssertFalse(StrOddKey(e));
  AssertTrue(m.AddAll(ea) = 50);
  ea := m.ExtractIf(@StrOddKey);
  AssertTrue(Length(ea) = 50);
  for e in ea do
    AssertTrue(StrOddKey(e));
end;

procedure TLiteTreeMapTest.StrRetain;
var
  m: TStrMap;
  s: TAutoStrSet;
  I, TestSize: Integer;
  e: TStrEntryArray;
  v: string;
begin
  TestSize := 100;
  e := CreateStrArray(TestSize);
  I := 1;
  while I <= System.High(e) do
    begin
      s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.AddAll(e);
  AssertTrue(m.Count = TestSize);
  m.RetainAll(s.Instance);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(s.Instance));
  for v in m.Keys do
    AssertTrue(StrOdd(v));
  AssertTrue(m.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Count = 0);
end;

procedure TLiteTreeMapTest.Head;
var
  m: TIntMap;
  ea: TIntEntryArray;
  e: TIntEntry;
  I, TestSize: Integer;
begin
  TestSize := 100;
  ea := CreateIntArray(TestSize);
  TIntHelper.RandomShuffle(ea);

  I := 0;
  for e in m.Head(TestSize) do
    Inc(I);
  AssertTrue(I = 0);

  m.AddAll(ea);
  for e in m.Head(0) do
    Inc(I);
  AssertTrue(I = 0);

  for e in m.Head(0, True) do
    Inc(I);
  AssertTrue(I = 1);

  I := 0;
  for e in m.Head(80) do
    begin
      AssertTrue(I = e.Key);
      Inc(I);
    end;
  AssertTrue(I = 80);

  I := 0;
  for e in m.Head(80, True) do
    begin
      AssertTrue(I = e.Key);
      Inc(I);
    end;
  AssertTrue(I = 81);
end;

procedure TLiteTreeMapTest.Tail;
var
  m: TIntMap;
  ea: TIntEntryArray;
  e: TIntEntry;
  I, TestSize: Integer;
begin
  TestSize := 100;
  ea := CreateIntArray(TestSize);
  TIntHelper.RandomShuffle(ea);

  I := 0;
  for e in m.Tail(0) do
    Inc(I);
  AssertTrue(I = 0);

  m.AddAll(ea);
  for e in m.Tail(99) do
    Inc(I);
  AssertTrue(I = 1);

  I := 0;
  for e in m.Tail(99, False) do
    Inc(I);
  AssertTrue(I = 0);

  I := 30;
  for e in m.Tail(I) do
    begin
      AssertTrue(I = e.Key);
      Inc(I);
    end;
  AssertTrue(I = 100);
end;

procedure TLiteTreeMapTest.Range;
var
  m: TIntMap;
  ea: TIntEntryArray;
  e: TIntEntry;
  I, TestSize: Integer;
begin
  TestSize := 100;
  ea := CreateIntArray(TestSize);
  TIntHelper.RandomShuffle(ea);

  I := 0;
  for e in m.Range(0, 100) do
    Inc(I);
  AssertTrue(I = 0);

  m.AddAll(ea);

  for e in m.Range(10, 10) do
    Inc(I);
  AssertTrue(I = 0);

  for e in m.Range(0, 0) do
    Inc(I);
  AssertTrue(I = 0);

  for e in m.Range(0, 0, BOUNDS_BOTH) do
    Inc(I);
  AssertTrue(I = 1);


  I := 10;
  for e in m.Range(I, 90) do
    begin
      AssertTrue(I = e.Key);
      Inc(I);
    end;
  AssertTrue(I = 90);

  I := 10;
  for e in m.Range(I, 90, BOUNDS_BOTH) do
    begin
      AssertTrue(I = e.Key);
      Inc(I);
    end;
  AssertTrue(I = 91);
end;

procedure TLiteTreeMapTest.FindFloor;
var
  m: TIntMap;
  I: Integer;
begin
  for I := 1 to 16 do
    m[I * 3] := I;
  AssertFalse(m.FindFloorKey(2, I));
  AssertFalse(m.FindFloorKey(3, I));
  AssertTrue(m.FindFloorKey(3, I, True));
  AssertTrue(I = 3);
  AssertTrue(m.FindFloorKey(9, I));
  AssertTrue(I = 6);
  AssertTrue(m.FindFloorKey(9, I, True));
  AssertTrue(I = 9);
  AssertTrue(m.FindFloorKey(13, I));
  AssertTrue(I = 12);
  AssertTrue(m.FindFloorKey(22, I));
  AssertTrue(I = 21);
  AssertTrue(m.FindFloorKey(27, I));
  AssertTrue(I = 24);
  AssertTrue(m.FindFloorKey(27, I, True));
  AssertTrue(I = 27);
end;

procedure TLiteTreeMapTest.FindCeil;
var
  m: TIntMap;
  I: Integer;
begin
  for I := 1 to 16 do
    m[I * 3] := I;
  AssertTrue(m.FindCeilKey(48, I));
  AssertTrue(I = 48);
  AssertFalse(m.FindCeilKey(48, I, False));
  AssertTrue(m.FindCeilKey(9, I));
  AssertTrue(I = 9);
  AssertTrue(m.FindCeilKey(9, I, False));
  AssertTrue(I = 12);
  AssertTrue(m.FindCeilKey(13, I));
  AssertTrue(I = 15);
  AssertTrue(m.FindCeilKey(22, I));
  AssertTrue(I = 24);
  AssertTrue(m.FindCeilKey(27, I));
  AssertTrue(I = 27);
  AssertTrue(m.FindCeilKey(27, I, False));
  AssertTrue(I = 30);
end;

procedure TLiteTreeMapTest.MakeEmptyTest;
var
  m: TIntMap;
begin
  m.AddAll([TIntEntry.Create(15,15), TIntEntry.Create(5,5), TIntEntry.Create(3,3)]);
  AssertTrue(m.Count = 3);

  m.MakeEmpty;
  AssertTrue(m.Count = 0);
  AssertTrue(m.Consistent);

  m.AddAll([TIntEntry.Create(215,15), TIntEntry.Create(125,5), TIntEntry.Create(103,3)]);
  AssertTrue(m.Count = 3);
  AssertTrue(m.Consistent);
end;

procedure TLiteTreeMapTest.PassByValue;
  procedure Test(aMap: TStrMap);
  begin
    aMap.Add('key0', 0);
    aMap.Add('key1', 1);
    aMap.Add('key2', 2);
    AssertTrue(aMap.NonEmpty);
    AssertTrue(aMap.Contains('key0'));
    AssertTrue(aMap.Contains('key1'));
    AssertTrue(aMap.Contains('key2'));
  end;
var
  m: TStrMap;
begin
  m.EnsureCapacity(8);
  AssertTrue(m.IsEmpty);
  Test(m);
  AssertTrue(m.IsEmpty);
  AssertTrue(m.NonContains('key0'));
  AssertTrue(m.NonContains('key1'));
  AssertTrue(m.NonContains('key2'));
end;

initialization
  RegisterTest(TBaseTreeMapTest);
  RegisterTest(TComparableTreeMapTest);
  RegisterTest(TRegularTreeMapTest);
  RegisterTest(TDelegatedTreeMapTest);
  RegisterTest(TLiteTreeMapTest);
end.

