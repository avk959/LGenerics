unit LGHashMapTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface
uses
  Classes, SysUtils, fpcunit, testregistry,
  LGUtils,
  LGArrayHelpers,
  LGAbstractContainer,
  LGHashMap,
  LGHashSet;

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
  TStrEntry      = specialize TGMapEntry<string, Integer>;
  TObjEntry      = specialize TGMapEntry<TTestObj, TTestObj>;
  TIntEntryArray = array of TIntEntry;
  TStrEntryArray = array of TStrEntry;
  TObjEntryArray = array of TObjEntry;
  TIntKeyArray   = array of Integer;
  TStrKeyArray   = array of string;

  { THashMapLPTest }

  THashMapLPTest = class(TTestCase)
  private
  type
    TIntMap     = specialize TGHashMapLP<Integer, Integer>;
    TAutoIntMap = specialize TGAutoRef<TIntMap>;
    TStrMap     = specialize TGHashMapLP<string, Integer>;
    TAutoStrMap = specialize TGAutoRef<TStrMap>;
    TObjMap     = specialize TGObjHashMapLP<TTestObj, TTestObj>;
    TAutoObjMap = specialize TGAutoRef<TObjMap>;

  published
    procedure Int_1;
    procedure Int_2;
    procedure IntRetain;
    procedure Str_1;
    procedure Str_2;
    procedure StrRetain;
    procedure ObjectMap;
    procedure GetOrAddMutValue;
    procedure GetOrAddMutValue2;
  end;

  THashMapLPTTest = class(TTestCase)
  private
  type
    TIntMap     = specialize TGHashMapLPT<Integer, Integer>;
    TAutoIntMap = specialize TGAutoRef<TIntMap>;
    TStrMap     = specialize TGHashMapLPT<string, Integer>;
    TAutoStrMap = specialize TGAutoRef<TStrMap>;
    TObjMap     = specialize TGObjHashMapLPT<TTestObj, TTestObj>;
    TAutoObjMap = specialize TGAutoRef<TObjMap>;

  published
    procedure Int_1;
    procedure Int_2;
    procedure IntRetain;
    procedure Str_1;
    procedure Str_2;
    procedure StrRetain;
    procedure ObjectMap;
  end;

  THashMapQPTest = class(TTestCase)
  private
  type
    TIntMap     = specialize TGHashMapQP<Integer, Integer>;
    TAutoIntMap = specialize TGAutoRef<TIntMap>;
    TStrMap     = specialize TGHashMapQP<string, Integer>;
    TAutoStrMap = specialize TGAutoRef<TStrMap>;
    TObjMap     = specialize TGObjHashMapQP<TTestObj, TTestObj>;
    TAutoObjMap = specialize TGAutoRef<TObjMap>;

  published
    procedure Int_1;
    procedure Int_2;
    procedure IntRetain;
    procedure Str_1;
    procedure Str_2;
    procedure StrRetain;
    procedure ObjectMap;
  end;

  TChainHashMapTest = class(TTestCase)
  private
  type
    TIntMap     = specialize TGChainHashMap<Integer, Integer>;
    TAutoIntMap = specialize TGAutoRef<TIntMap>;
    TStrMap     = specialize TGChainHashMap<string, Integer>;
    TAutoStrMap = specialize TGAutoRef<TStrMap>;
    TObjMap     = specialize TGObjChainHashMap<TTestObj, TTestObj>;
    TAutoObjMap = specialize TGAutoRef<TObjMap>;

  published
    procedure Int_1;
    procedure Int_2;
    procedure IntRetain;
    procedure Str_1;
    procedure Str_2;
    procedure StrRetain;
    procedure ObjectMap;
  end;

  { TOrderHashMapTest }
  TOrderHashMapTest = class(TTestCase)
  private
  type
    TIntMap     = specialize TGOrderedHashMap<Integer, Integer>;
    TAutoIntMap = specialize TGAutoRef<TIntMap>;
    TStrMap     = specialize TGOrderedHashMap<string, Integer>;
    TAutoStrMap = specialize TGAutoRef<TStrMap>;
    TObjMap     = specialize TGObjOrdHashMap<TTestObj, TTestObj>;
    TAutoObjMap = specialize TGAutoRef<TObjMap>;

  published
    procedure Int_1;
    procedure Int_2;
    procedure IntRetain;
    procedure Str_1;
    procedure Str_2;
    procedure StrRetain;

    procedure InOrder;
    procedure UpdateOnHit;
    procedure FindFirst;
    procedure FindLast;
    procedure ExtractFirst;
    procedure ExtractLast;
    procedure RemoveFirst;
    procedure RemoveLast;

    procedure ObjectMap;
    procedure ObjectOrdMapFirst;
    procedure ObjectOrdMapLast;
  end;

  { TLiteHashMapLPTest }

  TLiteHashMapLPTest = class(TTestCase)
  private
  type
    TIntMapSpec = specialize TGLiteHashMapLP<Integer, Integer, Integer>;
    TIntMap     = TIntMapSpec.TMap;
    TStrMapSpec = specialize TGLiteHashMapLP<string, Integer, string>;
    TStrMap     = TStrMapSpec.TMap;

  published
    procedure Int_1;
    procedure Int_2;
    procedure IntRetain;
    procedure Str_1;
    procedure Str_2;
    procedure StrRetain;
    procedure PassByValue;
    procedure GetOrAddMutValue;
    procedure GetOrAddMutValue2;
  end;

  TLiteChainHashMapTest = class(TTestCase)
  private
  type
    TIntMapSpec = specialize TGLiteChainHashMap<Integer, Integer, Integer>;
    TIntMap     = TIntMapSpec.TMap;
    TStrMapSpec = specialize TGLiteChainHashMap<string, Integer, string>;
    TStrMap     = TStrMapSpec.TMap;

  published
    procedure Int_1;
    procedure Int_2;
    procedure IntRetain;
    procedure Str_1;
    procedure Str_2;
    procedure StrRetain;
    procedure PassByValue;
  end;

  TLiteEquatableHashMapTest = class(TTestCase)
  private
  type
    TIntMapSpec = specialize TGLiteEquatableHashMap<Integer, Integer, Integer>;
    TIntMap     = TIntMapSpec.TMap;
    TStrMapSpec = specialize TGLiteEquatableHashMap<string, Integer, string>;
    TStrMap     = TStrMapSpec.TMap;

  published
    procedure Int_1;
    procedure Int_2;
    procedure IntRetain;
    procedure Str_1;
    procedure Str_2;
    procedure StrRetain;
    procedure PassByValue;
  end;

  { THashMapFGTest }

  THashMapFGTest = class(TTestCase)
  private
  type
    TMap    = specialize TGThreadHashMapFG<Integer, Integer>;
    TMapRef = specialize TGAutoRef<TMap>;
    TEntry  = TMap.TEntry;
    TArray  = array of Integer;
    THelper = specialize TGOrdinalArrayHelper<Integer>;

    TWorker = class(TThread)
    private
      FMap: TMap;
      FData: TArray;
      FIndex: Integer;
      FFlag,
      FResult: PInteger;
    public
      constructor Create(aMap: TMap; const aData: TArray; aIndex: Integer; aFlagValue, aResult: PInteger);
    end;

    TAdder = class(TWorker)
    protected
      procedure Execute; override;
    end;

    TRemover = class(TWorker)
    protected
      procedure Execute; override;
    end;

  published
    procedure SimpleAdd;
    procedure AddOrSetValue;
    procedure GetValueDef;
    procedure Replace;
    procedure SimpleRemove;
    procedure Add;
    procedure Remove;
  end;

  function CreateIntArray(aSize: Integer): TIntEntryArray;
  function CreateStrArray(aSize: Integer): TStrEntryArray;
  function CreateObjArray(var aCounter: TCounter; aSize: Integer): TObjEntryArray;

implementation
{$B-}{$COPERATORS ON}

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

{ THashMapLPTest }

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

procedure THashMapLPTest.Int_1;
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

procedure THashMapLPTest.Int_2;
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

procedure THashMapLPTest.IntRetain;
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

procedure THashMapLPTest.Str_1;
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
    m.Instance[(I * 23).ToString] := I * 47;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[(I * 23).ToString] = I * 47);
  for I := 1 to 100 do
    m.Instance[(I * 23).ToString] := I * 53;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[(I * 23).ToString] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m.Instance[(I * 23).ToString] = I * 53);
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
    m.Instance.Remove((I * 23).ToString);
  AssertTrue(m.Instance.Count = 0);
end;

procedure THashMapLPTest.Str_2;
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

procedure THashMapLPTest.StrRetain;
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

procedure THashMapLPTest.ObjectMap;
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
  {%H-}m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = 100);
  for I := 0 to System.High(e) do
    AssertTrue(m.Instance.Contains(e[I].Key));
  for I := 50 to 99 do
    {%H-}s.Instance.Add(e[I].Key);
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

procedure THashMapLPTest.GetOrAddMutValue;
var
  im: TAutoIntMap;
  sm: TAutoStrMap;
  I: Integer;
  p: PInteger;
begin
  AssertFalse(im.Instance.FindOrAddMutValue(1, p));
  p^ := 1;
  AssertTrue(im.Instance.TryGetValue(1, I));
  AssertTrue(I = 1);
  AssertTrue(im.Instance.FindOrAddMutValue(1, p));
  p^ := 2;
  AssertTrue(im.Instance.TryGetValue(1, I));
  AssertTrue(I = 2);
  AssertFalse(im.Instance.FindOrAddMutValue(2, p));

  AssertFalse(sm.Instance.FindOrAddMutValue('key 1', p));
  p^ := 1;
  AssertTrue(sm.Instance.TryGetValue('key 1', I));
  AssertTrue(I = 1);
  AssertTrue(sm.Instance.FindOrAddMutValue('key 1', p));
  p^ := 2;
  AssertTrue(sm.Instance.TryGetValue('key 1', I));
  AssertTrue(I = 2);
  AssertFalse(sm.Instance.FindOrAddMutValue('key 2', p));
end;

procedure THashMapLPTest.GetOrAddMutValue2;
type
  TMap = specialize TGHashMapLP<Integer, string>;
  TRef = specialize TGAutoRef<TMap>;
var
  Map: TRef;
  p: ^string;
  s: string;
begin
  AssertFalse(Map.Instance.FindOrAddMutValue(1, p));
  AssertTrue(p^ = '');
  p^ := 'value 1';
  AssertTrue(Map.Instance.TryGetValue(1, s));
  AssertTrue(s = 'value 1');
  AssertTrue(Map.Instance.FindOrAddMutValue(1, p));
  p^ := 'value 2';
  AssertTrue(Map.Instance.TryGetValue(1, s));
  AssertTrue(s = 'value 2');
end;


{ THashMapLPTTest }

procedure THashMapLPTTest.Int_1;
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

procedure THashMapLPTTest.Int_2;
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

procedure THashMapLPTTest.IntRetain;
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

procedure THashMapLPTTest.Str_1;
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
    m.Instance[(I * 23).ToString] := I * 47;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[(I * 23).ToString] = I * 47);
  for I := 1 to 100 do
    m.Instance[(I * 23).ToString] := I * 53;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[(I * 23).ToString] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m.Instance[(I * 23).ToString] = I * 53);
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
    m.Instance.Remove((I * 23).ToString);
  AssertTrue(m.Instance.Count = 0);
end;

procedure THashMapLPTTest.Str_2;
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

procedure THashMapLPTTest.StrRetain;
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

procedure THashMapLPTTest.ObjectMap;
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
  {%H-}m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = 100);
  for I := 0 to System.High(e) do
    AssertTrue(m.Instance.Contains(e[I].Key));
  for I := 50 to 99 do
    {%H-}s.Instance.Add(e[I].Key);
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

{ THashMapQPTest }

procedure THashMapQPTest.Int_1;
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

procedure THashMapQPTest.Int_2;
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

procedure THashMapQPTest.IntRetain;
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

procedure THashMapQPTest.Str_1;
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
    m.Instance[(I * 23).ToString] := I * 47;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[(I * 23).ToString] = I * 47);
  for I := 1 to 100 do
    m.Instance[(I * 23).ToString] := I * 53;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[(I * 23).ToString] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m.Instance[(I * 23).ToString] = I * 53);
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
    m.Instance.Remove((I * 23).ToString);
  AssertTrue(m.Instance.Count = 0);
end;

procedure THashMapQPTest.Str_2;
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

procedure THashMapQPTest.StrRetain;
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

procedure THashMapQPTest.ObjectMap;
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
  {%H-}m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = 100);
  for I := 0 to System.High(e) do
    AssertTrue(m.Instance.Contains(e[I].Key));
  for I := 50 to 99 do
    {%H-}s.Instance.Add(e[I].Key);
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

{ TChainHashMapTest }

procedure TChainHashMapTest.Int_1;
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

procedure TChainHashMapTest.Int_2;
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

procedure TChainHashMapTest.IntRetain;
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

procedure TChainHashMapTest.Str_1;
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
    m.Instance[(I * 23).ToString] := I * 47;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[(I * 23).ToString] = I * 47);
  for I := 1 to 100 do
    m.Instance[(I * 23).ToString] := I * 53;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[(I * 23).ToString] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m.Instance[(I * 23).ToString] = I * 53);
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
    m.Instance.Remove((I * 23).ToString);
  AssertTrue(m.Instance.Count = 0);
end;

procedure TChainHashMapTest.Str_2;
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

procedure TChainHashMapTest.StrRetain;
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

procedure TChainHashMapTest.ObjectMap;
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
  {%H-}m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = 100);
  for I := 0 to System.High(e) do
    AssertTrue(m.Instance.Contains(e[I].Key));
  for I := 50 to 99 do
    {%H-}s.Instance.Add(e[I].Key);
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

{ TOrderHashMapTest }

procedure TOrderHashMapTest.Int_1;
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

procedure TOrderHashMapTest.Int_2;
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

procedure TOrderHashMapTest.IntRetain;
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

procedure TOrderHashMapTest.Str_1;
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
    m.Instance[(I * 23).ToString] := I * 47;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[(I * 23).ToString] = I * 47);
  for I := 1 to 100 do
    m.Instance[(I * 23).ToString] := I * 53;
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m.Instance[(I * 23).ToString] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m.Instance[(I * 23).ToString] = I * 53);
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
    m.Instance.Remove((I * 23).ToString);
  AssertTrue(m.Instance.Count = 0);
end;

procedure TOrderHashMapTest.Str_2;
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

procedure TOrderHashMapTest.StrRetain;
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

procedure TOrderHashMapTest.InOrder;
var
  m: TAutoIntMap;
  e: TIntMap.TEntry;
  I, k: Integer;
begin
  for I := 1 to 100 do
    m.Instance.Add(I, I);
  AssertTrue(m.Instance.Count = 100);
  I := 1;
  for e in m.Instance.Entries do
    begin
      AssertTrue(e.Key = I);
      Inc(I);
    end;
  I := 1;
  for k in m.Instance.Keys do
    begin
      AssertTrue(k = I);
      Inc(I);
    end;
  m.Instance.Clear;
  for I := 100 downto 1 do
    m.Instance.Add(I, I);
  I := 100;
  for e in m.Instance.Entries do
    begin
      AssertTrue(e.Key = I);
      Dec(I);
    end;
  I := 100;
  for k in m.Instance.Keys do
    begin
      AssertTrue(k = I);
      Dec(I);
    end;
end;

procedure TOrderHashMapTest.UpdateOnHit;
var
  m: TAutoStrMap;
  I: Integer;
  Key, k: string;
begin
  for I := 1 to 100 do
    m.Instance.Add('key ' + I.ToString, I);
  AssertTrue(m.Instance.Count = 100);
  m.Instance.UpdateOnHit := True;
  for I := 1 to 100 do
    begin
      Key := 'key ' + I.ToString;
      AssertTrue(m.Instance.Contains(Key));
      AssertTrue(m.Instance.FindLastKey(k));
      AssertTrue(k = Key);
    end;
end;

procedure TOrderHashMapTest.FindFirst;
var
  m: TAutoIntMap;
  e: TIntMap.TEntry;
  I: Integer;
begin
  AssertFalse(m.Instance.FindFirst(e));
  for I := 1 to 100 do
    m.Instance.Add(I, I);
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    begin
      AssertTrue(m.Instance.FindFirst(e));
      AssertTrue(e.Key = I);
      m.Instance.Remove(I);
    end;
end;

procedure TOrderHashMapTest.FindLast;
var
  m: TAutoIntMap;
  e: TIntMap.TEntry;
  I: Integer;
begin
  AssertFalse(m.Instance.FindLast(e));
  for I := 1 to 100 do
    m.Instance.Add(I, I);
  AssertTrue(m.Instance.Count = 100);
  for I := 100 downto 1 do
    begin
      AssertTrue(m.Instance.FindLast(e));
      AssertTrue(e.Key = I);
      m.Instance.Remove(I);
    end;
end;

procedure TOrderHashMapTest.ExtractFirst;
var
  m: TAutoIntMap;
  e: TIntMap.TEntry;
  I: Integer;
begin
  AssertFalse(m.Instance.ExtractFirst(e));
  for I := 1 to 100 do
    m.Instance.Add(I, I);
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 100 do
    begin
      AssertTrue(m.Instance.ExtractFirst(e));
      AssertTrue(e.Key = I);
    end;
end;

procedure TOrderHashMapTest.ExtractLast;
var
  m: TAutoIntMap;
  e: TIntMap.TEntry;
  I: Integer;
begin
  AssertFalse(m.Instance.ExtractLast(e));
  for I := 1 to 100 do
    m.Instance.Add(I, I);
  AssertTrue(m.Instance.Count = 100);
  for I := 100 downto 1 do
    begin
      AssertTrue(m.Instance.ExtractLast(e));
      AssertTrue(e.Key = I);
    end;
end;

procedure TOrderHashMapTest.RemoveFirst;
var
  m: TAutoIntMap;
  e: TIntMap.TEntry;
  I: Integer;
begin
  AssertFalse(m.Instance.RemoveFirst);
  for I := 1 to 100 do
    m.Instance.Add(I, I);
  AssertTrue(m.Instance.Count = 100);
  for I := 1 to 99 do
    begin
      AssertTrue(m.Instance.RemoveFirst);
      AssertTrue(m.Instance.FindFirst(e));
      AssertTrue(e.Key = I + 1);
    end;
end;

procedure TOrderHashMapTest.RemoveLast;
var
  m: TAutoIntMap;
  e: TIntMap.TEntry;
  I: Integer;
begin
  AssertFalse(m.Instance.RemoveLast);
  for I := 1 to 100 do
    m.Instance.Add(I, I);
  AssertTrue(m.Instance.Count = 100);
  for I := 100 downto 2 do
    begin
      AssertTrue(m.Instance.RemoveLast);
      AssertTrue(m.Instance.FindLast(e));
      AssertTrue(e.Key = I - 1);
    end;
end;

procedure TOrderHashMapTest.ObjectMap;
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
  {%H-}m.Instance.AddAll(e);
  AssertTrue(m.Instance.Count = TestSize);
  for I := 0 to System.High(e) do
    AssertTrue(m.Instance.Contains(e[I].Key));
  for I := 50 to 99 do
    {%H-}s.Instance.Add(e[I].Key);
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

procedure TOrderHashMapTest.ObjectOrdMapFirst;
var
  m: TAutoObjMap;
  I, TestSize: Integer;
  eArray: TObjEntryArray;
  e: TObjMap.TEntry;
  Counter: TCounter;
begin
  Counter := Default(TCounter);
  TestSize := 100;//must be even
  eArray := CreateObjArray(Counter, TestSize);
  AssertTrue(Counter.Count = 0);
  {%H-}m.Instance.AddAll(eArray);
  AssertTrue(m.Instance.Count = TestSize);
  for I := 0 to Pred(TestSize div 2) do
    begin
      AssertTrue(m.Instance.ExtractFirst(e));
      AssertTrue(e.Key = eArray[I].Key);
      AssertTrue(e.Value = eArray[I].Value);
    end;
  AssertTrue(Counter.Count = 0);
  AssertTrue(m.Instance.Count = TestSize div 2);
  for I := 0 to Pred(TestSize div 2) do
    begin
      eArray[I].Key.Free;
      eArray[I].Value.Free;
    end;
  AssertTrue(Counter.Count = TestSize);
  for I := 1 to TestSize div 2 do
    AssertTrue(m.Instance.RemoveFirst);
  AssertTrue(m.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize * 2);
end;

procedure TOrderHashMapTest.ObjectOrdMapLast;
var
  m: TAutoObjMap;
  I, J, TestSize: Integer;
  eArray: TObjEntryArray;
  e: TObjMap.TEntry;
  Counter: TCounter;
begin
  Counter := Default(TCounter);
  TestSize := 100;//must be even
  eArray := CreateObjArray(Counter, TestSize);
  AssertTrue(Counter.Count = 0);
  {%H-}m.Instance.AddAll(eArray);
  AssertTrue(m.Instance.Count = TestSize);
  J := TestSize;
  for I := 1 to TestSize div 2  do
    begin
      Dec(J);
      AssertTrue(m.Instance.ExtractLast(e));
      AssertTrue(e.Key = eArray[J].Key);
     AssertTrue(e.Value = eArray[J].Value);
    end;
  AssertTrue(Counter.Count = 0);
  AssertTrue(m.Instance.Count = TestSize div 2);
  for I := Pred(TestSize) downto TestSize div 2  do
    begin
      eArray[I].Key.Free;
      eArray[I].Value.Free;
    end;
  AssertTrue(Counter.Count = TestSize);
  for I := 1 to TestSize div 2 do
    AssertTrue(m.Instance.RemoveLast);
  AssertTrue(m.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize * 2);
end;

procedure TLiteHashMapLPTest.Int_1;
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

procedure TLiteHashMapLPTest.Int_2;
var
  m: TIntMap;
  I, TestSize: Integer;
  e: TIntEntryArray;
  k: TIntKeyArray;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(e) do
    k[I] := e[I].Key;
  m.AddAll(e);
  AssertTrue(m.Count = TestSize);
  AssertTrue(m.ContainsAll(k));
  AssertTrue(m.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(k[50..99]));
  AssertTrue(m.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Count = 0);
  m.AddAll(e);
  AssertTrue(m.RemoveIf(@IntOdd) = 50);
  AssertTrue(m.Count = 50);
  for {%H-}I in m.Keys do
    AssertFalse(IntOdd(I));
  AssertTrue(m.AddAll(e) = 50);
  e := m.ExtractIf(@IntOdd);
  for I := 0 to System.High(e) do
    AssertTrue(IntOdd(e[I].Key));
end;

procedure TLiteHashMapLPTest.IntRetain;
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
      {%H-}s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.AddAll(e);
  AssertTrue(m.Count = TestSize);
  m.RetainAll(s.Instance);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(s.Instance));
  for {%H-}I in m.Keys do
    AssertTrue(IntOdd(I));
  AssertTrue(m.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Count = 0);
end;

procedure TLiteHashMapLPTest.Str_1;
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
    m[(I * 23).ToString] := I * 47;
  AssertTrue(m.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m[(I * 23).ToString] = I * 47);
  for I := 1 to 100 do
    m[(I * 23).ToString] := I * 53;
  AssertTrue(m.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m[(I * 23).ToString] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m[(I * 23).ToString] = I * 53);
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
    m.Remove((I * 23).ToString);
  AssertTrue(m.Count = 0);
end;

procedure TLiteHashMapLPTest.Str_2;
var
  m: TStrMap;
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
  m.AddAll(e);
  AssertTrue(m.Count = TestSize);
  AssertTrue(m.ContainsAll(k));
  AssertTrue(m.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(k[50..99]));
  AssertTrue(m.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Count = 0);
  m.AddAll(e);
  AssertTrue(m.RemoveIf(@StrOdd) = 50);
  AssertTrue(m.Count = 50);
  for {%H-}s in m.Keys do
    AssertFalse(StrOdd(s));
  AssertTrue(m.AddAll(e) = 50);
  e := m.ExtractIf(@StrOdd);
  for I := 0 to System.High(e) do
    AssertTrue(StrOdd(e[I].Key));
end;

procedure TLiteHashMapLPTest.StrRetain;
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
      {%H-}s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.AddAll(e);
  AssertTrue(m.Count = TestSize);
  m.RetainAll(s.Instance);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(s.Instance));
  for {%H-}v in m.Keys do
    AssertTrue(StrOdd(v));
  AssertTrue(m.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Count = 0);
end;

procedure TLiteHashMapLPTest.PassByValue;
  procedure Test(aMap: TStrMap);
  begin
    aMap.Add('key0', 0);
    aMap.Add('key1', 1);
    aMap.Add('key2', 2);
    AssertTrue(aMap.NonEmpty);
  end;
var
  m: TStrMap;
begin
  m.EnsureCapacity(10);
  AssertTrue(m.IsEmpty);
  Test(m);
  AssertTrue(m.IsEmpty);
  AssertTrue(m.NonContains('key0'));
  AssertTrue(m.NonContains('key1'));
  AssertTrue(m.NonContains('key2'));
end;

procedure TLiteHashMapLPTest.GetOrAddMutValue;
var
  iMap: TIntMap;
  sMap: TStrMap;
  I: Integer;
  p: PInteger;
begin
  AssertFalse(iMap.FindOrAddMutValue(1, p));
  p^ := 1;
  AssertTrue(iMap.TryGetValue(1, I));
  AssertTrue(I = 1);
  AssertTrue(iMap.FindOrAddMutValue(1, p));
  p^ := 2;
  AssertTrue(iMap.TryGetValue(1, I));
  AssertTrue(I = 2);
  AssertFalse(iMap.FindOrAddMutValue(2, p));

  AssertFalse(sMap.FindOrAddMutValue('key 1', p));
  p^ := 1;
  AssertTrue(sMap.TryGetValue('key 1', I));
  AssertTrue(I = 1);
  AssertTrue(sMap.FindOrAddMutValue('key 1', p));
  p^ := 2;
  AssertTrue(sMap.TryGetValue('key 1', I));
  AssertTrue(I = 2);
  AssertFalse(sMap.FindOrAddMutValue('key 2', p));
end;

procedure TLiteHashMapLPTest.GetOrAddMutValue2;
type
  TMap = specialize TGLiteHashMapLP<Integer, string, Integer>.TMap;
var
  Map: TMap;
  p: ^string;
  s: string;
begin
  AssertFalse(Map.FindOrAddMutValue(1, p));
  AssertTrue(p^ = '');
  p^ := 'value 1';
  AssertTrue(Map.TryGetValue(1, s));
  AssertTrue(s = 'value 1');
  AssertTrue(Map.FindOrAddMutValue(1, p));
  p^ := 'value 2';
  AssertTrue(Map.TryGetValue(1, s));
  AssertTrue(s = 'value 2');
end;

//////////////////////////////

procedure TLiteChainHashMapTest.Int_1;
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

procedure TLiteChainHashMapTest.Int_2;
var
  m: TIntMap;
  I, TestSize: Integer;
  e: TIntEntryArray;
  k: TIntKeyArray;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(e) do
    k[I] := e[I].Key;
  m.AddAll(e);
  AssertTrue(m.Count = TestSize);
  AssertTrue(m.ContainsAll(k));
  AssertTrue(m.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(k[50..99]));
  AssertTrue(m.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Count = 0);
  m.AddAll(e);
  AssertTrue(m.RemoveIf(@IntOdd) = 50);
  AssertTrue(m.Count = 50);
  for {%H-}I in m.Keys do
    AssertFalse(IntOdd(I));
  AssertTrue(m.AddAll(e) = 50);
  e := m.ExtractIf(@IntOdd);
  for I := 0 to System.High(e) do
    AssertTrue(IntOdd(e[I].Key));
end;

procedure TLiteChainHashMapTest.IntRetain;
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
      {%H-}s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.AddAll(e);
  AssertTrue(m.Count = TestSize);
  m.RetainAll(s.Instance);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(s.Instance));
  for {%H-}I in m.Keys do
    AssertTrue(IntOdd(I));
  AssertTrue(m.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Count = 0);
end;

procedure TLiteChainHashMapTest.Str_1;
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
    m[(I * 23).ToString] := I * 47;
  AssertTrue(m.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m[(I * 23).ToString] = I * 47);
  for I := 1 to 100 do
    m[(I * 23).ToString] := I * 53;
  AssertTrue(m.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m[(I * 23).ToString] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m[(I * 23).ToString] = I * 53);
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
    m.Remove((I * 23).ToString);
  AssertTrue(m.Count = 0);
end;

procedure TLiteChainHashMapTest.Str_2;
var
  m: TStrMap;
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
  m.AddAll(e);
  AssertTrue(m.Count = TestSize);
  AssertTrue(m.ContainsAll(k));
  AssertTrue(m.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(k[50..99]));
  AssertTrue(m.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Count = 0);
  m.AddAll(e);
  AssertTrue(m.RemoveIf(@StrOdd) = 50);
  AssertTrue(m.Count = 50);
  for {%H-}s in m.Keys do
    AssertFalse(StrOdd(s));
  AssertTrue(m.AddAll(e) = 50);
  e := m.ExtractIf(@StrOdd);
  for I := 0 to System.High(e) do
    AssertTrue(StrOdd(e[I].Key));
end;

procedure TLiteChainHashMapTest.StrRetain;
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
      {%H-}s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.AddAll(e);
  AssertTrue(m.Count = TestSize);
  m.RetainAll(s.Instance);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(s.Instance));
  for {%H-}v in m.Keys do
    AssertTrue(StrOdd(v));
  AssertTrue(m.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Count = 0);
end;

procedure TLiteChainHashMapTest.PassByValue;
  procedure Test(aMap: TStrMap);
  begin
    aMap.Add('key0', 0);
    aMap.Add('key1', 1);
    aMap.Add('key2', 2);
    AssertTrue(aMap.NonEmpty);
  end;
var
  m: TStrMap;
begin
  m.EnsureCapacity(10);
  AssertTrue(m.IsEmpty);
  Test(m);
  AssertTrue(m.IsEmpty);
  AssertTrue(m.NonContains('key0'));
  AssertTrue(m.NonContains('key1'));
  AssertTrue(m.NonContains('key2'));
end;

procedure TLiteEquatableHashMapTest.Int_1;
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

procedure TLiteEquatableHashMapTest.Int_2;
var
  m: TIntMap;
  I, TestSize: Integer;
  e: TIntEntryArray;
  k: TIntKeyArray;
begin
  TestSize := 100;
  e := CreateIntArray(TestSize);
  System.SetLength(k, TestSize);
  for I := 0 to System.High(e) do
    k[I] := e[I].Key;
  m.AddAll(e);
  AssertTrue(m.Count = TestSize);
  AssertTrue(m.ContainsAll(k));
  AssertTrue(m.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(k[50..99]));
  AssertTrue(m.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Count = 0);
  m.AddAll(e);
  AssertTrue(m.RemoveIf(@IntOdd) = 50);
  AssertTrue(m.Count = 50);
  for {%H-}I in m.Keys do
    AssertFalse(IntOdd(I));
  AssertTrue(m.AddAll(e) = 50);
  e := m.ExtractIf(@IntOdd);
  for I := 0 to System.High(e) do
    AssertTrue(IntOdd(e[I].Key));
end;

procedure TLiteEquatableHashMapTest.IntRetain;
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
      {%H-}s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.AddAll(e);
  AssertTrue(m.Count = TestSize);
  m.RetainAll(s.Instance);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(s.Instance));
  for {%H-}I in m.Keys do
    AssertTrue(IntOdd(I));
  AssertTrue(m.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Count = 0);
end;

procedure TLiteEquatableHashMapTest.Str_1;
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
    m[(I * 23).ToString] := I * 47;
  AssertTrue(m.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m[(I * 23).ToString] = I * 47);
  for I := 1 to 100 do
    m[(I * 23).ToString] := I * 53;
  AssertTrue(m.Count = 100);
  for I := 1 to 100 do
    AssertTrue(m[(I * 23).ToString] = I * 53);
  for I := 1 to 100 do
    AssertTrue(m[(I * 23).ToString] = I * 53);
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
    m.Remove((I * 23).ToString);
  AssertTrue(m.Count = 0);
end;

procedure TLiteEquatableHashMapTest.Str_2;
var
  m: TStrMap;
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
  m.AddAll(e);
  AssertTrue(m.Count = TestSize);
  AssertTrue(m.ContainsAll(k));
  AssertTrue(m.RemoveAll(k[0..49]) = 50);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(k[50..99]));
  AssertTrue(m.RemoveAll(k[50..99]) = 50);
  AssertTrue(m.Count = 0);
  m.AddAll(e);
  AssertTrue(m.RemoveIf(@StrOdd) = 50);
  AssertTrue(m.Count = 50);
  for {%H-}s in m.Keys do
    AssertFalse(StrOdd(s));
  AssertTrue(m.AddAll(e) = 50);
  e := m.ExtractIf(@StrOdd);
  for I := 0 to System.High(e) do
    AssertTrue(StrOdd(e[I].Key));
end;

procedure TLiteEquatableHashMapTest.StrRetain;
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
      {%H-}s.Instance.Add(e[I].Key);
      I += 2;
    end;
  AssertTrue(s.Instance.Count = 50);
  m.AddAll(e);
  AssertTrue(m.Count = TestSize);
  m.RetainAll(s.Instance);
  AssertTrue(m.Count = 50);
  AssertTrue(m.ContainsAll(s.Instance));
  for {%H-}v in m.Keys do
    AssertTrue(StrOdd(v));
  AssertTrue(m.RemoveAll(s.Instance) = 50);
  AssertTrue(m.Count = 0);
end;

procedure TLiteEquatableHashMapTest.PassByValue;
  procedure Test(aMap: TStrMap);
  begin
    aMap.Add('key0', 0);
    aMap.Add('key1', 1);
    aMap.Add('key2', 2);
    AssertTrue(aMap.NonEmpty);
  end;
var
  m: TStrMap;
begin
  m.EnsureCapacity(10);
  AssertTrue(m.IsEmpty);
  Test(m);
  AssertTrue(m.IsEmpty);
  AssertTrue(m.NonContains('key0'));
  AssertTrue(m.NonContains('key1'));
  AssertTrue(m.NonContains('key2'));
end;

{ THashMapFGTest.TWorker }

constructor THashMapFGTest.TWorker.Create(aMap: TMap; const aData: TArray; aIndex: Integer; aFlagValue,
  aResult: PInteger);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FMap := aMap;
  FData := aData;
  FIndex := aIndex;
  FFlag := aFlagValue;
  FResult := aResult;
end;

{ THashMapFGTest.TAdder }

procedure THashMapFGTest.TAdder.Execute;
var
  I: Integer;
  Added: Integer = 0;
begin
  for I in FData do
    Added += Ord(FMap.Add(I, I));
  FResult[FIndex] := Added;
  InterlockedIncrement(FFlag^);
  Terminate;
end;

{ THashMapFGTest.TRemover }

procedure THashMapFGTest.TRemover.Execute;
var
  I: Integer;
  Removed: Integer = 0;
begin
  for I in FData do
    Removed += Ord(FMap.Remove(I));
  FResult[FIndex] := Removed;
  InterlockedIncrement(FFlag^);
  Terminate;
end;

{ THashMapFGTest }

procedure THashMapFGTest.SimpleAdd;
var
  map: TMap;
  ref: TMapRef;
begin
  map := {%H-}ref;
  AssertTrue(map.Count = 0);

  AssertTrue(map.Add(-1, -2));
  AssertTrue(map.Count = 1);
  AssertTrue(map.Contains(-1));
  AssertTrue(map[-1] = -2);

  AssertTrue(map.Add(TEntry.Create(0, -1)));
  AssertTrue(map.Count = 2);
  AssertTrue(map.Contains(0));
  AssertTrue(map[0] = -1);

  AssertTrue(map.Add(1, 1));
  AssertTrue(map.Count = 3);
  AssertTrue(map.Contains(1));
  AssertTrue(map[1] = 1);

  AssertFalse(map.Add(0, 2));
  AssertTrue(map.Count = 3);
  AssertTrue(map[0] = -1);

  AssertFalse(map.Add(TEntry.Create(-1, 5)));
  AssertTrue(map.Count = 3);
  AssertTrue(map[-1] = -2);
end;

procedure THashMapFGTest.AddOrSetValue;
var
  map: TMap;
  ref: TMapRef;
begin
  map := {%H-}ref;
  AssertTrue(map.Count = 0);

  map.AddOrSetValue(0, -2);
  AssertTrue(map.Contains(0));
  AssertTrue(map.Count = 1);
  AssertTrue(map[0] = -2);

  map.AddOrSetValue(5, 5);
  AssertTrue(map.Contains(5));
  AssertTrue(map.Count = 2);
  AssertTrue(map[5] = 5);

  map.AddOrSetValue(0, 0);
  AssertTrue(map.Count = 2);
  AssertTrue(map[0] = 0);

  map.AddOrSetValue(5, 10);
  AssertTrue(map.Count = 2);
  AssertTrue(map[5] = 10);
end;

procedure THashMapFGTest.GetValueDef;
var
  map: TMap;
  ref: TMapRef;
begin
  map := {%H-}ref;
  AssertTrue(map.GetValueDef(0, -2) = -2);
  AssertTrue(map.GetValueDef(1, 5) = 5);

  map.AddOrSetValue(4, -4);
  AssertTrue(map.GetValueDef(4, -1) = -4);
end;

procedure THashMapFGTest.Replace;
var
  map: TMap;
  ref: TMapRef;
begin
  map := {%H-}ref;
  AssertTrue(map.Count = 0);

  map.AddOrSetValue(-1, -5);
  map.AddOrSetValue(0, -1);
  map.AddOrSetValue(1, 5);
  map.AddOrSetValue(5, 5);
  AssertTrue(map.Count = 4);

  AssertFalse(map.Replace(-2, 2));
  AssertFalse(map.Replace(2, 2));

  AssertTrue(map.Replace(-1, -7));
  AssertTrue(map[-1] = -7);

  AssertTrue(map.Replace(0, 2));
  AssertTrue(map[0] = 2);

  AssertTrue(map.Replace(1, 3));
  AssertTrue(map[1] = 3);

  AssertTrue(map.Replace(5, 7));
  AssertTrue(map[5] = 7);
end;

procedure THashMapFGTest.SimpleRemove;
var
  map: TMap;
  ref: TMapRef;
  I: Integer;
begin
  map := {%H-}ref;
  AssertTrue(map.Count = 0);

  for I := 0 to 99 do
    AssertTrue(map.Add(I, I));
  AssertTrue(map.Count = 100);

  for I := 100 to 150 do
    AssertFalse(map.Remove(I));
  AssertTrue(map.Count = 100);

  for I := 0 to 99 do
    AssertTrue(map.Remove(I));
  AssertTrue(map.Count = 0);
end;

procedure THashMapFGTest.Add;
const
  TestSize    = 100000;
  ThreadCount = 4;
  function CreateTestArray: TArray;
  begin
    Result := THelper.CreateRange(1, TestSize);
    THelper.RandomShuffle(Result);
  end;
var
  map: TMap;
  TestArray: array[0..Pred(ThreadCount)] of TArray;
  ThreadArray: array[0..Pred(ThreadCount)] of TAdder;
  Results: array[0..Pred(ThreadCount)] of Integer;
  I, Total: Integer;
  Finished: Integer = 0;
begin
  for I := 0 to Pred(ThreadCount) do
    TestArray[I] := CreateTestArray;
  map := TMap.Create(0);
  for I := 0 to Pred(ThreadCount) do
    ThreadArray[I] := TAdder.Create(map, TestArray[I], I, @Finished, @Results[0]);
  for I := 0 to Pred(ThreadCount) do
    ThreadArray[I].Start;
  while Finished < ThreadCount do
    Sleep(50);
  AssertTrue(map.Count = TestSize);
  map.Free;
  Total := 0;
  for I in Results do
    Total += I;
  AssertTrue(Total = TestSize);
end;

procedure THashMapFGTest.Remove;
const
  TestSize    = 100000;
  ThreadCount = 4;
  function CreateTestArray: TArray;
  begin
    Result := THelper.CreateRange(1, TestSize);
    THelper.RandomShuffle(Result);
  end;
var
  map: TMap;
  TestArray: array[0..Pred(ThreadCount)] of TArray;
  ThreadArray: array[0..Pred(ThreadCount)] of TRemover;
  Results: array[0..Pred(ThreadCount)] of Integer;
  I, Total: Integer;
  Finished: Integer = 0;
begin
  map := TMap.Create(TestSize);
  for I := 1 to TestSize do
    map.Add(I, I);
  for I := 0 to Pred(ThreadCount) do
    TestArray[I] := CreateTestArray;
  for I := 0 to Pred(ThreadCount) do
    ThreadArray[I] := TRemover.Create(map, TestArray[I], I, @Finished, @Results[0]);
  for I := 0 to Pred(ThreadCount) do
    ThreadArray[I].Start;
  while Finished < ThreadCount do
    Sleep(50);
  AssertTrue(map.Count = 0);
  map.Free;
  Total := 0;
  for I in Results do
    Total += I;
  AssertTrue(Total = TestSize);
end;

initialization
  RegisterTest(THashMapLPTest);
  RegisterTest(THashMapLPTTest);
  RegisterTest(THashMapQPTest);
  RegisterTest(TChainHashMapTest);
  RegisterTest(TOrderHashMapTest);
  RegisterTest(TLiteHashMapLPTest);
  RegisterTest(TLiteChainHashMapTest);
  RegisterTest(TLiteEquatableHashMapTest);
  RegisterTest(THashMapFGTest);
end.

