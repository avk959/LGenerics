unit LGTreeMapTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

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

  TComparableTreeMapTest = class(TTestCase)
  private
  type
    TIntMap     = specialize TGComparableTreeMap<Integer, Integer>;
    TAutoIntMap = specialize TGAutoRef<TIntMap>;
    TStrMap     = specialize TGComparableTreeMap<string, Integer>;
    TAutoStrMap = specialize TGAutoRef<TStrMap>;

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

  function CreateIntArray(aSize: Integer): TIntEntryArray;
  function CreateStrArray(aSize: Integer): TStrEntryArray;
  function CreateObjArray(var aCounter: TCounter; aSize: Integer): TObjEntryArray;

implementation
{$B-}{$COPERATORS ON}{$WARNINGS OFF}

function IntOdd(constref aValue: Integer): Boolean;
begin
  Result := Odd(aValue);
end;

function StrOdd(constref aValue: string): Boolean;
begin
  Result := Odd(StrToInt(System.Copy(aValue, System.Length(aValue), 1)));
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
  STR_PREFIX     = 'str_';
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
  e: TIntEntryArray;
  k: TIntKeyArray;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(e) do
    k[I] := e[I].Key;
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.RemoveIf(@IntOdd) = 50);
  AssertTrue(m.Instance.Count = 50);
  for I in m.Instance.Keys do
    AssertFalse(IntOdd(I));
  AssertTrue(m.Instance.AddAll(e) = 50);
  e := m.Instance.ExtractIf(@IntOdd);
  for I := 0 to System.High(e) do
    AssertTrue(IntOdd(e[I].Key));
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
  e: TStrEntryArray;
  k: TStrKeyArray;
  s: string;
begin
  TestSize := 100;
  e := CreateStrArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(e) do
    k[I] := e[I].Key;
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.RemoveIf(@StrOdd) = 50);
  AssertTrue(m.Instance.Count = 50);
  for s in m.Instance.Keys do
    AssertFalse(StrOdd(s));
  AssertTrue(m.Instance.AddAll(e) = 50);
  e := m.Instance.ExtractIf(@StrOdd);
  for I := 0 to System.High(e) do
    AssertTrue(StrOdd(e[I].Key));
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
  e: TIntEntryArray;
  k: TIntKeyArray;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(e) do
    k[I] := e[I].Key;
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.RemoveIf(@IntOdd) = 50);
  AssertTrue(m.Instance.Count = 50);
  for I in m.Instance.Keys do
    AssertFalse(IntOdd(I));
  AssertTrue(m.Instance.AddAll(e) = 50);
  e := m.Instance.ExtractIf(@IntOdd);
  for I := 0 to System.High(e) do
    AssertTrue(IntOdd(e[I].Key));
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
  e: TStrEntryArray;
  k: TStrKeyArray;
  s: string;
begin
  TestSize := 100;
  e := CreateStrArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(e) do
    k[I] := e[I].Key;
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.RemoveIf(@StrOdd) = 50);
  AssertTrue(m.Instance.Count = 50);
  for s in m.Instance.Keys do
    AssertFalse(StrOdd(s));
  AssertTrue(m.Instance.AddAll(e) = 50);
  e := m.Instance.ExtractIf(@StrOdd);
  for I := 0 to System.High(e) do
    AssertTrue(StrOdd(e[I].Key));
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
  e: TIntEntryArray;
  k: TIntKeyArray;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(e) do
    k[I] := e[I].Key;
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.RemoveIf(@IntOdd) = 50);
  AssertTrue(m.Instance.Count = 50);
  for I in m.Instance.Keys do
    AssertFalse(IntOdd(I));
  AssertTrue(m.Instance.AddAll(e) = 50);
  e := m.Instance.ExtractIf(@IntOdd);
  for I := 0 to System.High(e) do
    AssertTrue(IntOdd(e[I].Key));
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
  e: TStrEntryArray;
  k: TStrKeyArray;
  s: string;
begin
  TestSize := 100;
  e := CreateStrArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(e) do
    k[I] := e[I].Key;
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.RemoveIf(@StrOdd) = 50);
  AssertTrue(m.Instance.Count = 50);
  for s in m.Instance.Keys do
    AssertFalse(StrOdd(s));
  AssertTrue(m.Instance.AddAll(e) = 50);
  e := m.Instance.ExtractIf(@StrOdd);
  for I := 0 to System.High(e) do
    AssertTrue(StrOdd(e[I].Key));
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
  e: TIntEntryArray;
  k: TIntKeyArray;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(e) do
    k[I] := e[I].Key;
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.RemoveIf(@IntOdd) = 50);
  AssertTrue(m.Instance.Count = 50);
  for I in m.Instance.Keys do
    AssertFalse(IntOdd(I));
  AssertTrue(m.Instance.AddAll(e) = 50);
  e := m.Instance.ExtractIf(@IntOdd);
  for I := 0 to System.High(e) do
    AssertTrue(IntOdd(e[I].Key));
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
  e: TStrEntryArray;
  k: TStrKeyArray;
  s: string;
begin
  TestSize := 100;
  e := CreateStrArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(e) do
    k[I] := e[I].Key;
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(m.Instance.ContainsAll(k));
  AssertTrue(m.Instance.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Instance.Count = 50);
  AssertTrue(m.Instance.ContainsAll(k[50..99]));
  AssertTrue(m.Instance.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Instance.Count = 0);
  m.Instance.AddAll(e);
  AssertTrue(m.Instance.RemoveIf(@StrOdd) = 50);
  AssertTrue(m.Instance.Count = 50);
  for s in m.Instance.Keys do
    AssertFalse(StrOdd(s));
  AssertTrue(m.Instance.AddAll(e) = 50);
  e := m.Instance.ExtractIf(@StrOdd);
  for I := 0 to System.High(e) do
    AssertTrue(StrOdd(e[I].Key));
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

initialization
  RegisterTest(TBaseTreeMapTest);
  RegisterTest(TComparableTreeMapTest);
  RegisterTest(TRegularTreeMapTest);
  RegisterTest(TDelegatedTreeMapTest);
end.

