unit LGHashMapTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface
uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
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

  TLiteHashMapLPTest = class(TTestCase)
  private
  type
    TIntMap = specialize TGLiteHashMapLP<Integer, Integer, Integer>;
    TStrMap = specialize TGLiteHashMapLP<string, Integer, string>;

  published
    procedure Int_1;
    procedure Int_2;
    procedure IntRetain;
    procedure Str_1;
    procedure Str_2;
    procedure StrRetain;
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
  AssertTrue(m.Instance.GetValueDef(17) = 0);
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
  AssertTrue(m.Instance.GetValueDef('17') = 0);
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
  AssertTrue(m.Instance.GetValueDef(17) = 0);
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
  AssertTrue(m.Instance.GetValueDef('17') = 0);
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
  AssertTrue(m.Instance.GetValueDef(17) = 0);
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
  AssertTrue(m.Instance.GetValueDef('17') = 0);
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
  AssertTrue(m.Instance.GetValueDef(17) = 0);
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
  AssertTrue(m.Instance.GetValueDef('17') = 0);
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
  AssertTrue(m.GetValueDef(17) = 0);
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
  AssertTrue(m.GetValueDef('17') = 0);
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

initialization
  RegisterTest(THashMapLPTest);
  RegisterTest(THashMapLPTTest);
  RegisterTest(THashMapQPTest);
  RegisterTest(TChainHashMapTest);
  RegisterTest(TLiteHashMapLPTest);
end.

